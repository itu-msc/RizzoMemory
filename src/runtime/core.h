#pragma once

#include "allocation.h"
#include "box.h"
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdbool.h>
#include <string.h>

#ifdef _WIN32
#include <malloc.h>
#define alloca _alloca
#else
#include <alloca.h>
#endif
typedef int32_t rz_refcount_t;

typedef enum rz_object_type {
    RZ_OBJECT = 0,
    RZ_STRING = 1,
    RZ_SIGNAL = 2,
    RZ_PARTIAL_APP = 3,
} rz_object_type_t;

/*TODO: how big should these fields be? */
typedef struct rz_header {
    uint16_t num_fields;         /* number of fields in constructor, recursively free */
    uint8_t tag;                 /* tag of constructor for case matching */
    uint8_t obj_type;            /* is this a signal? A regular memory block? A string? for deallocation logic! */
    rz_refcount_t refcount;      /* ref count */
} rz_header_t;

typedef struct rz_object {
    rz_header_t header;
    /* fields follow */
} rz_object_t;

typedef struct rz_object_fields {
    rz_object_t _base;
    rz_box_t fields[1];
} rz_object_fields_t;

typedef struct rz_function {
    rz_object_t _base;
    rz_fun* fun;       /* pointer to the C-function to call */
    /* free variables, closure */
} rz_function_t;

typedef struct rz_function_args {
    rz_function_t _base;
    rz_box_t args[1];
} rz_function_args_t;

#define ARGS_OF(f) ((rz_function_args_t*)f)
#define ARGS_OF_BOXED(f) (((rz_box_t*)(f + 1)))
#define sizeof_function_with(num_args) (sizeof(rz_function_t) + (num_args) * sizeof(rz_box_t))

/* 
    OBJET HELPERS !
*/

static inline uint16_t rz_object_tag(rz_object_t* obj) {
    return obj->header.tag;
}

static inline rz_object_type_t rz_object_get_type(rz_object_t* obj) {
    return (rz_object_type_t)obj->header.obj_type;
}

/** Gets the i-th field of a constructor. Instead of casting to [rz_object_fields_t*] */
static inline rz_box_t rz_object_get_field(rz_object_t* obj, int16_t idx) {
    if (idx >= obj->header.num_fields) {
        printf("Tried to access field '%d' out of '%d'", idx, obj->header.num_fields);
        exit(1);
    }
    rz_object_fields_t* objf = (rz_object_fields_t*) obj;
    return objf->fields[idx];
}

/** variadic constructor */
#define rz_ctor_var(tag, num_fields, ...) rz_ctor(tag, num_fields, (rz_box_t[]){__VA_ARGS__})

rz_object_t* rz_ctor(int16_t tag, int16_t num_fields, rz_box_t* args);

/** Reuses [obj] if it is not null, otherwise behaves like [rz_ctor] */
rz_object_t* rz_reuse_object(rz_object_t* obj, int16_t tag, int16_t num_fields, rz_box_t* args);

/* 
    RZ FUNCTIONS
*/

static inline size_t rz_function_get_arity(rz_function_t* fun) {
    return fun->_base.header.tag;
}

/** const-app-full */
rz_box_t rz_call(rz_fun* f, size_t num_args, rz_box_t* args);

/** const-app-part - partial app of C function */
rz_box_t rz_lift_c_fun(rz_fun* f, int32_t arity, rz_box_t* free_vars, size_t num_free_vars);

/** Variable application - both partial and full. Will not allocate if full */
rz_box_t rz_apply1(rz_object_t* fun_obj, rz_box_t arg);

/* 
    REFERENCE COUNTING 
*/

void rz_refcount_inc(rz_object_t* obj);
void rz_refcount_dec(rz_object_t* obj);

rz_object_t* rz_reset_object(rz_object_t* obj);

void rz_refcount_dec_box(rz_box_t box);
void rz_refcount_inc_box(rz_box_t box);

/* 
    BOX HELPERS
*/

rz_box_t rz_eq(rz_box_t a, rz_box_t b);
void rz_debug_print_box(rz_box_t box);

static rz_object_t RZ_BOOL_TRUE = { .header = { .num_fields = 0, .tag = 0, .refcount = -1 } };
static rz_object_t RZ_BOOL_FALSE = { .header = { .num_fields = 0, .tag = 1, .refcount = -1 } };
static inline rz_object_t* rz_bool_ctor(bool b) {
    return b ? &RZ_BOOL_TRUE : &RZ_BOOL_FALSE;
}

static inline bool rz_box_is_string(rz_box_t box) {
    rz_box_kind_t kind = rz_box_get_kind(box);
    return kind == RZ_BOX_STRING_LITERAL
        || (kind == RZ_BOX_PTR && rz_object_get_type(rz_unbox_ptr(box)) == RZ_STRING);
}

/* 
    RIZZO OUTPUTS
*/
typedef struct rz_signal rz_signal_t;
typedef struct rz_signal_list {
    size_t count, capacity;
    rz_signal_t *signals[];
} rz_signal_list_t;

extern rz_signal_list_t *rz_global_output_signals;

rz_signal_list_t *rz_signal_list_create();
void rz_signal_list_add(rz_signal_list_t** list, rz_object_t* signal);
void rz_print_registered_outputs();
void rz_print_registered_output_head(rz_signal_t* sig, bool force);
rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args);
