#include "core.h"
#include "rzstring.h"
#include "heap.h"

/* these must be defined inside of a single translation unit - oof, that's here */
rz_signal_t* rz_heap_base = NULL;
rz_signal_t* rz_heap_cursor = NULL;
size_t rz_heap_size = 0;
rz_signal_list_t *rz_global_output_signals;

rz_object_t *rz_ctor(int16_t tag, int16_t num_fields, rz_box_t* args) {
    rz_object_fields_t* obj = (rz_object_fields_t*) rz_malloc(sizeof(rz_object_fields_t) + (num_fields - 1) * sizeof(rz_box_t));
    obj->_base.header.tag = tag;
    obj->_base.header.num_fields = num_fields;
    obj->_base.header.refcount = 1;
    obj->_base.header.obj_type = RZ_OBJECT;
    for (int i = 0; i < num_fields; i++) {
        rz_box_t field = args[i];
        obj->fields[i] = field;
    }
    return (rz_object_t*)obj;
}

void rz_refcount_inc(rz_object_t* obj) {
    if (obj) obj->header.refcount++;
}

/* forward declare from heap.h */
static void rz_free_pap(rz_object_t* obj);
void rz_refcount_dec(rz_object_t* obj) {
    if (!obj) return;
    rz_refcount_t new_count = --obj->header.refcount;
    
    if (0 == new_count) {
        switch (obj->header.obj_type) {
            case RZ_SIGNAL: {
                rz_signal_free(obj);
            } break;
            case RZ_PARTIAL_APP: {
                rz_free_pap(obj);
            } break;
            case RZ_OBJECT: {
                rz_object_fields_t *objf = (rz_object_fields_t*)obj;
                uint16_t end = obj->header.num_fields;
                for (uint16_t i = 0; i < end; i++) {
                    rz_box_t field = objf->fields[i];
                    rz_refcount_dec_box(field);
                }
                rz_free(obj);
            } break;
            case RZ_STRING: {
                rz_free(obj);
            }
        }
    }

}

rz_object_t* rz_reset_object(rz_object_t* obj) {
    /* reset-shared */
    if(obj->header.refcount != 1) {
        rz_refcount_dec(obj);
        return NULL;
    }
    /* reset-unique - assuming refcount == 1*/
    rz_object_fields_t* objf = (rz_object_fields_t*)obj;
    size_t end = obj->header.num_fields;
    for (uint16_t i = 0; i < end; i++) {
        rz_refcount_dec_box(objf->fields[i]);
    }
    return obj;
}

rz_object_t* rz_reuse_object(rz_object_t* obj, int16_t tag, int16_t num_fields, rz_box_t* args) {
    /* reuse-shared */
    if(obj == NULL) return rz_ctor(tag, num_fields, args);
    /* reuse-unique */
    obj->header.obj_type = RZ_OBJECT;
    obj->header.tag = tag;
    obj->header.num_fields = num_fields;
    rz_object_fields_t* objf = (rz_object_fields_t*)obj;
    size_t end = obj->header.num_fields;
    for (size_t i = 0; i < end; i++) {
        objf->fields[i] = args[i];
    }
    return obj;
}

void rz_refcount_dec_box(rz_box_t box) {
    if (!rz_is_boxed(box) && rz_unbox_ptr(box)) {
        rz_refcount_dec(rz_unbox_ptr(box));
    }
}

void rz_refcount_inc_box(rz_box_t box) {
    if (!rz_is_boxed(box) && rz_unbox_ptr(box)) {
        rz_refcount_inc(rz_unbox_ptr(box));
    }
}

/* const-app-full */
rz_box_t rz_call(rz_fun* f, size_t num_args, rz_box_t* args) {
    return f(num_args, args);
}

static inline rz_function_t *rz_malloc_func(rz_fun *f, int32_t arity, size_t num_free_vars) {
    rz_function_t* fun = (rz_function_t*) rz_malloc(sizeof_function_with(num_free_vars));
    fun->_base.header.num_fields = num_free_vars;
    fun->_base.header.refcount = 1;
    fun->_base.header.obj_type = RZ_PARTIAL_APP;
    fun->fun = f;
    fun->_base.header.tag = arity; // fun->arity = arity;
    return fun;
}

static void rz_free_pap(rz_object_t* obj) {
    rz_function_t* fun = (rz_function_t*) obj;
    rz_function_args_t* fun_args = ARGS_OF(fun);
    int16_t n = fun->_base.header.num_fields;
    for (size_t i = 0; i < n; i++) {
        rz_refcount_dec_box(fun_args->args[i]);
    }
    rz_free(fun);
}

/* const-app-part - partial app of C function */
rz_box_t rz_lift_c_fun(rz_fun* f, int32_t arity, rz_box_t* free_vars, size_t num_free_vars) {
    rz_function_t* fun = rz_malloc_func(f, arity, num_free_vars);
    rz_function_args_t* fun_args = ARGS_OF(fun);
    for (int i = 0; i < num_free_vars; i++) {
        fun_args->args[i] = free_vars[i];
    }
    return rz_make_ptr_fun(fun);
}

rz_box_t rz_apply1(rz_object_t* fun_obj, rz_box_t arg) {
    rz_function_t* fun = (rz_function_t*)fun_obj;
    size_t fun_arity = rz_function_get_arity(fun);
    rz_fun* function_ptr = fun->fun;
    if(fun_arity == fun->_base.header.num_fields + 1) {
        /* var-app-full */
        size_t n = fun->_base.header.num_fields;
        /* Alternatively 'rz_box_t args[n + 1]' but that doesn work for all C-compilers */
        rz_box_t* args = (rz_box_t*) alloca((n + 1) * sizeof(rz_box_t));
        rz_function_args_t* fun_free_args = ARGS_OF(fun);
        for(int i = 0; i < n; i++) {
            rz_refcount_inc_box(args[i] = fun_free_args->args[i]);
        }
        args[n] = arg;
        rz_refcount_dec(fun_obj);
        return function_ptr(n + 1, args);
    } else { 
        /* var-app-part */
        /* TODO: reuse fun if unique - or can we entirely skip copying step? */
        rz_function_t* copy = rz_malloc_func(function_ptr, fun_arity, fun->_base.header.num_fields + 1);
        rz_function_args_t* fun_free_args = ARGS_OF(fun);
        rz_function_args_t* copy_free_args = ARGS_OF(copy);
        size_t n = fun->_base.header.num_fields;
        copy_free_args->args[n] = arg;
        for(int i = 0; i < n; i++) {
            rz_box_t arg = copy_free_args->args[i] = fun_free_args->args[i];
            rz_refcount_inc_box(arg);
        }
        rz_refcount_dec(fun_obj);
        return rz_make_ptr_fun(copy);
    }
}

rz_box_t rz_eq(rz_box_t a, rz_box_t b) {
    if (rz_box_is_string(a) || rz_box_is_string(b)) {
        bool both_strings = rz_box_is_string(a) && rz_box_is_string(b);
        return rz_make_ptr(rz_bool_ctor(both_strings && rz_string_eq_content(a, b)));
    }
    rz_box_kind_t a_kind = rz_box_get_kind(a);
    rz_box_kind_t b_kind = rz_box_get_kind(b);
    if (a_kind != b_kind) return rz_make_ptr( rz_bool_ctor(false) );
    if (a_kind == RZ_BOX_INT) {
        return rz_make_ptr( rz_bool_ctor(rz_unbox_int(a) == rz_unbox_int(b)) );
    } else {
        rz_object_t* a_obj = rz_unbox_ptr(a);
        rz_object_t* b_obj = rz_unbox_ptr(b);
        if (a_obj->header.tag != b_obj->header.tag 
            || a_obj->header.num_fields != b_obj->header.num_fields) {
            return rz_make_ptr( rz_bool_ctor(false) );
        }
        rz_object_fields_t* a_fields = (rz_object_fields_t*)a_obj;
        rz_object_fields_t* b_fields = (rz_object_fields_t*)b_obj;
        for (size_t i = 0; i < a_obj->header.num_fields; i++) {
            rz_object_t* equality = rz_unbox_ptr(rz_eq(a_fields->fields[i], b_fields->fields[i]));
            switch (rz_object_tag(equality)) {
                case 0: return rz_make_ptr( rz_bool_ctor(false) );
                default: continue;
            }
        }
        return rz_make_ptr( rz_bool_ctor(true) );
    }
}

void rz_debug_print_box(rz_box_t box) {
    switch (rz_box_get_kind(box)) {
        case RZ_BOX_INT: {
            printf("%d", rz_unbox_int(box));
        } break;
        case RZ_BOX_STRING_LITERAL: {
            printf("%s", rz_unbox_str_lit(box));
        } break;
        case RZ_BOX_PTR: {
            switch (rz_object_get_type(rz_unbox_ptr(box))) {
                case RZ_STRING: {
                    printf("%s", ((rz_string_t*)rz_unbox_ptr(box))->bytes);
                } break;
                case RZ_SIGNAL: { rz_debug_print_signal(box); } break;
                case RZ_OBJECT: {
                    rz_object_fields_t* fields = (rz_object_fields_t*)rz_unbox_ptr(box);
                    printf("ctor(%d, ref: %d)", fields->_base.header.tag, fields->_base.header.refcount);
                    if(fields->_base.header.num_fields > 0) {
                        printf("{ ");
                        for (size_t i = 0; i < fields->_base.header.num_fields; i++)
                        {
                            rz_debug_print_box(fields->fields[i]); printf(", ");
                        }
                        printf("}");
                    }
                }break;
                case RZ_PARTIAL_APP: {
                    rz_function_t* fun = rz_unbox_fun(box);
                    printf("pap(ref: %d, arity: %d, applied_vars: %d)", fun->_base.header.refcount, fun->_base.header.tag, fun->_base.header.num_fields);
                } break;
                default: {
                    printf("Unknown object type: '%d'", rz_object_get_type(rz_unbox_ptr(box)));
                    exit(1);
                }
            }
        } break;
        default: {
            printf("Unknown box tag: '%d'", rz_box_get_kind(box));
            exit(1);
        }
    }
}

/*  |------------------------------|
    |         OUTPUT HELPERS       |
    |------------------------------| */

void rz_print_registered_output_head(rz_signal_t *sig, bool force) {
    /*TODO: this assume strings for now - we will want to make it strings */
    if (rz_unbox_int(sig->updated) || force) {
        rz_debug_print_box(sig->head);
        printf("\n");
    }
}

void rz_print_registered_outputs() {
    for (size_t i = 0; i < rz_global_output_signals->count; i++) {
        rz_print_registered_output_head(rz_global_output_signals->signals[i], false);
    }
}

rz_signal_list_t *rz_signal_list_create() {
    size_t initial_capacity = 10;
    rz_signal_list_t *list = malloc(sizeof(rz_signal_list_t) + initial_capacity * sizeof(rz_signal_t *));
    list->count = 0;
    list->capacity = initial_capacity;
    return list;
}

void rz_signal_list_add(rz_signal_list_t **list_ref, rz_object_t *signal) {
    rz_signal_list_t* list = *list_ref;
    if (list->count == list->capacity) {
        size_t new_capacity = list->capacity * 2;
        list = realloc(list, sizeof(rz_signal_list_t) + new_capacity * sizeof(rz_signal_t *));
        list->capacity = new_capacity;
        *list_ref = list;
    }
    list->signals[list->count++] = (rz_signal_t *)signal;
}

/* registers a boxed signal for output */
rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args) {
    (void)num_args;
    rz_box_t sig = args[0];
    rz_box_kind_t sig_kind = rz_box_get_kind(sig);
    if (sig_kind != RZ_BOX_PTR || rz_object_get_type(rz_unbox_ptr(sig)) != RZ_SIGNAL) {
        rz_debug_print_box(sig);
        fprintf(stderr, "Runtime error: rz_register_output_signal got a non-signal value (%d)\n", sig_kind);
        exit(1);
    }
    /* we've just read a signal, which has a head value in the current time tick - output that */
    rz_print_registered_output_head((rz_signal_t *)rz_unbox_ptr(sig), true);
    rz_signal_list_add(&rz_global_output_signals, rz_unbox_ptr(sig));
    return rz_make_int(0); /* return unit */
}
