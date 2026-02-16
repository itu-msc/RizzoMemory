#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdbool.h>
#include <alloca.h>

typedef int32_t rz_refcount_t;

typedef struct rz_header {
    int16_t num_fields;         /* number of fields in constructor, recursively free */
    int16_t tag;                /* tag of constructor for case matching */
    int16_t  offset;            /* offset of first field after header - i.e. function should have 1 because of C function pointer */
    rz_refcount_t refcount;     /* ref count */
} rz_header_t;

typedef struct rz_object {
    rz_header_t header;
    /* fields follow */
} rz_object_t;

typedef enum {
    RZ_BOX_INT,
    RZ_BOX_PTR
} rz_box_kind_t;

typedef struct rz_box {
    rz_box_kind_t kind;
    union {
        int32_t i32;
        void* fun;
        rz_object_t* obj;
    } as; 
} rz_box_t;

bool rz_is_boxed(rz_box_t box) {
    return box.kind != RZ_BOX_PTR;
}

static inline rz_box_t rz_make_int(int32_t v) {
    return (rz_box_t){ .kind = RZ_BOX_INT, .as.i32 = v };
}

static inline int32_t rz_unbox_int(rz_box_t box) {
    return (int32_t)box.as.i32;
}

static inline rz_box_t rz_make_ptr(rz_object_t* obj) {
    return (rz_box_t){ .kind = RZ_BOX_PTR, .as.obj = obj };
}

static inline rz_object_t* rz_unbox_ptr(rz_box_t box) {
    return box.as.obj;
}

/*TODO: proper handling of refcount for these? negative refcount so they are likely never freed */
static rz_object_t RZ_BOOL_TRUE = { .header = { .num_fields = 0, .tag = 0, .refcount = -1 } };
static rz_object_t RZ_BOOL_FALSE = { .header = { .num_fields = 0, .tag = 1, .refcount = -1 } };
static inline rz_object_t* rz_bool_ctor(bool b) {
    return b ? &RZ_BOOL_TRUE : &RZ_BOOL_FALSE;
}

typedef struct rz_object_fields {
    rz_object_t _base;
    rz_box_t fields[1];
} rz_object_fields_t;

rz_object_t *rz_ctor(int16_t tag, int16_t num_fields, rz_box_t* args) {
    rz_object_fields_t* obj = (rz_object_fields_t*) malloc(sizeof(rz_object_fields_t) + (num_fields - 1) * sizeof(rz_box_t));
    obj->_base.header.tag = tag;
    obj->_base.header.num_fields = num_fields;
    obj->_base.header.refcount = 1;
    obj->_base.header.offset = 0;
    for (int i = 0; i < num_fields; i++) {
        rz_box_t field = args[i];
        obj->fields[i] = field;
    }
    return (rz_object_t*)obj;
}

#define rz_ctor_var(tag, num_fields, ...) rz_ctor(tag, num_fields, (rz_box_t[]){__VA_ARGS__})

static inline void rz_refcount_inc(rz_object_t* obj) {
    if (obj) obj->header.refcount++;
}

static inline void rz_refcount_dec(rz_object_t* obj) {
    if (!obj) return;
    rz_refcount_t new_count = --obj->header.refcount;
    if (0 == new_count) {
        rz_object_fields_t *objf = (rz_object_fields_t*)obj;
        for (int i = obj->header.offset; i < obj->header.num_fields; i++) {
            rz_box_t field = objf->fields[i];
            if (!rz_is_boxed(field) && field.as.obj) { 
                rz_refcount_dec(field.as.obj); 
            }
        }
        free(obj);
    }
}

static inline void rz_refcount_dec_box(rz_box_t box) {
    if (!rz_is_boxed(box) && box.as.obj) {
        rz_refcount_dec(box.as.obj);
    }
}

static inline void rz_refcount_inc_box(rz_box_t box) {
    if (!rz_is_boxed(box) && box.as.obj) {
        rz_refcount_inc(box.as.obj);
    }
}

typedef struct rz_function rz_function_t;
typedef rz_box_t(rz_fun)(rz_function_t *fun);

typedef struct rz_function {
    rz_object_t _base;
    rz_box_t fun;       /* boxed ptr to a C-function - count this a num_fields */
    /* free variables, closure */
} rz_function_t;

typedef struct rz_function_args {
    rz_function_t _base;
    rz_box_t args[1];
} rz_function_args_t;

#define ARGS_OF(f) ((rz_function_args_t*)f)
#define ARGS_OF_BOXED(f) (((rz_box_t*)(f + 1)))
#define sizeof_function_with(num_args) (sizeof(rz_function_t) + (num_args) * sizeof(rz_box_t))

static inline size_t rz_function_get_arity(rz_function_t* fun) {
    return fun->_base.header.tag;
}

/* const-app-full */
rz_box_t rz_call(rz_fun* f, rz_box_t* args, size_t num_args) {
    /* stack allocate a function + arguments (which should all be boxed values) */
    rz_function_t* fun = (rz_function_t*)alloca(sizeof_function_with(num_args));
    fun->_base.header.num_fields = num_args;
    fun->_base.header.offset = 1;
    fun->_base.header.tag = num_args; /* as opposed to fun->arity = num_args */
    fun->fun = (rz_box_t){ .kind = RZ_BOX_INT, .as.fun = f};
    rz_box_t* dst = ARGS_OF_BOXED(fun);
    for (int i = 0; i < num_args; i++) {
        dst[i] = args[i];
    }
    return f(fun);
}

static inline rz_function_t *rz_malloc_func(rz_fun *f, int32_t arity, size_t num_free_vars) {
    rz_function_t* fun = (rz_function_t*) malloc(sizeof_function_with(num_free_vars));
    fun->_base.header.num_fields = num_free_vars;
    fun->_base.header.refcount = 1;
    fun->_base.header.offset = 1;
    fun->fun = (rz_box_t){ .kind = RZ_BOX_INT, .as.fun = f };
    fun->_base.header.tag = arity; // fun->arity = arity;
    return fun;
}

/* const-app-part - partial app of C function */
rz_box_t rz_lift_c_fun(rz_fun* f, int32_t arity, rz_box_t* free_vars, size_t num_free_vars) {
    rz_function_t* fun = rz_malloc_func(f, arity, num_free_vars);
    rz_function_args_t* fun_args = ARGS_OF(fun);
    for (int i = 0; i < num_free_vars; i++) {
        fun_args->args[i] = free_vars[i];
    }
    return rz_make_ptr((rz_object_t*)fun);
}

static inline rz_box_t rz_apply1(rz_object_t* fun_obj, rz_box_t arg) {
    rz_function_t* fun = (rz_function_t*)fun_obj;
    /* TODO: reuse fun if unique - or can we entirely skip copying step? */
    rz_function_t* copy = rz_malloc_func((rz_fun*) fun->fun.as.fun, rz_function_get_arity(fun), fun->_base.header.num_fields + 1);
    rz_function_args_t* fun_free_args = ARGS_OF(fun);
    rz_function_args_t* copy_free_args = ARGS_OF(copy);
    copy_free_args->args[fun->_base.header.num_fields] = arg;
    size_t n = fun->_base.header.num_fields;
    /* TODO: add to header an field_offset - then we don't have to this funky off by 1 stuff*/
    for(int i = 0; i < n; i++) {
        
        rz_box_t arg = copy_free_args->args[i] = fun_free_args->args[i];
        if (!rz_is_boxed(arg) && arg.as.obj) {
            rz_refcount_inc(arg.as.obj);
        }
    }
    
    rz_refcount_dec(fun_obj);
    /* var-app-full */
    if (copy->_base.header.num_fields == rz_function_get_arity(copy)) {
        rz_fun* tocall = copy->fun.as.fun;
        return tocall(copy);
    }
    /* var-app-part */
    return rz_make_ptr((rz_object_t*)copy);
}

static inline rz_box_t rz_eq(rz_box_t a, rz_box_t b) {
    if (a.kind != b.kind) return rz_make_ptr( rz_bool_ctor(false) );
    if (a.kind == RZ_BOX_INT) {
        return rz_make_ptr( rz_bool_ctor(a.as.i32 == b.as.i32) );
    } else {
        rz_object_t* a_obj = a.as.obj;
        rz_object_t* b_obj = b.as.obj;
        if (a_obj->header.tag != b_obj->header.tag 
            || a_obj->header.num_fields != b_obj->header.num_fields) {
            return rz_make_ptr( rz_bool_ctor(false) );
        }
        rz_object_fields_t* a_fields = (rz_object_fields_t*)a_obj;
        rz_object_fields_t* b_fields = (rz_object_fields_t*)b_obj;
        for (size_t i = 0; i < a_obj->header.num_fields; i++) {
            switch (rz_eq(a_fields->fields[i], b_fields->fields[i]).as.obj->header.tag) {
                case 0: return rz_make_ptr( rz_bool_ctor(false) );
                default: continue;
            }
        }
        return rz_make_ptr( rz_bool_ctor(true) );
    }
}
