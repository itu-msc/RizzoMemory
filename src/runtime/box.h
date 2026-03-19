#pragma once

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

typedef struct rz_object rz_object_t;
typedef struct rz_function rz_function_t;

// #define RZ_NAN_BOX
#ifdef RZ_NAN_BOX 

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define QNAN ((uint64_t)0x7ffc000000000000)
#define SIGN_BIT ((uint64_t)0x8000000000000000)

typedef uint64_t rz_box_t;
typedef uint8_t rz_box_kind_t;

typedef rz_box_t(rz_fun)(size_t num_args, rz_box_t* args);

#define RZ_BOX_INT              1
#define RZ_BOX_STRING_LITERAL   2
#define RZ_BOX_PTR              3

#define rz_make_int(v) (rz_box_of_num(v))
#define rz_unbox_int(box) ((int32_t)rz_unbox_num(box))
#define rz_box_is_int(box) (QNAN != (box & QNAN))

#define rz_make_ptr(obj) ((rz_box_t) (SIGN_BIT | QNAN | (uint64_t)(uintptr_t)(obj)))
#define rz_unbox_ptr(box) ((rz_object_t*)(uintptr_t)((box) & ~(SIGN_BIT | QNAN)))
#define rz_box_is_ptr(box) (((box) & (QNAN | SIGN_BIT)) == (QNAN | SIGN_BIT))

#define rz_make_ptr_fun(fun) rz_make_ptr(fun)
#define rz_unbox_fun(box) ((rz_function_t*)rz_unbox_ptr(box))

//String literals have their point in the lower bits, special tag in the upper bits
#define QNAN_STR_LIT_TAG (QNAN | ((uint64_t)RZ_BOX_STRING_LITERAL << 48))
#define rz_make_str_lit(str) ((rz_box_t) (QNAN_STR_LIT_TAG | (uint64_t)(uintptr_t)(str)))
#define rz_unbox_str_lit(box) ((char*)(uintptr_t)((box) & ~(QNAN_STR_LIT_TAG)))
#define rz_box_is_str_lit(box) (((box) & (QNAN_STR_LIT_TAG)) == (QNAN_STR_LIT_TAG))

static inline rz_box_kind_t rz_box_get_kind(rz_box_t box) {
    if (rz_box_is_int(box)) return RZ_BOX_INT;
    if (rz_box_is_str_lit(box)) return RZ_BOX_STRING_LITERAL;
    if (rz_box_is_ptr(box)) return RZ_BOX_PTR;
    // Should never happen since all bit patterns are valid boxes, but just in case:
    fprintf(stderr, "Runtime error: invalid box with bits 0x%lx\n", box);
    exit(1);
}

static inline bool rz_is_boxed(rz_box_t box) {
    rz_box_kind_t kind = rz_box_get_kind(box);
    return kind == RZ_BOX_INT || kind == RZ_BOX_STRING_LITERAL;
}

static inline rz_box_t rz_box_of_num(double num) {
    rz_box_t box;
    memcpy(&box, &num, sizeof(double));
    return box;
}

static inline double rz_unbox_num(rz_box_t box) {
    double num;
    memcpy(&num, &box, sizeof(rz_box_t));
    return num;
}

#else

typedef struct rz_box rz_box_t;
typedef rz_box_t(rz_fun)(size_t num_args, rz_box_t* args);

typedef enum {
    RZ_BOX_INT,
    RZ_BOX_STRING_LITERAL,
    RZ_BOX_PTR,
} rz_box_kind_t;

typedef struct rz_box {
    rz_box_kind_t kind;
    union {
        int32_t i32;
        rz_fun* c_fun_ptr;
        rz_object_t* obj;
        char* str; /*pointer so its keeps the size of the struct */
    } as; 
} rz_box_t;

static inline rz_box_kind_t rz_box_get_kind(rz_box_t box) {
    return box.kind;
}

static inline bool rz_is_boxed(rz_box_t box) {
    rz_box_kind_t kind = rz_box_get_kind(box);
    return kind == RZ_BOX_INT || kind == RZ_BOX_STRING_LITERAL;
}

#define rz_make_int(v) ((rz_box_t){ .kind = RZ_BOX_INT, .as.i32 = (v) })

static inline int32_t rz_unbox_int(rz_box_t box) {
    return (int32_t)box.as.i32;
}

#define rz_make_ptr(target) ((rz_box_t){ .kind = RZ_BOX_PTR, .as.obj = (target) })

static inline rz_object_t* rz_unbox_ptr(rz_box_t box) {
    return box.as.obj;
}

#define rz_make_str_lit(target) ((rz_box_t){ .kind = RZ_BOX_STRING_LITERAL, .as.str = (target) })

static inline char* rz_unbox_str_lit(rz_box_t box) {
    return box.as.str;
}

static inline rz_box_t rz_make_ptr_fun(rz_function_t* fun) {
    return (rz_box_t){ .kind = RZ_BOX_PTR, .as.obj = (rz_object_t*) fun };
}

static inline rz_function_t* rz_unbox_fun(rz_box_t box) {
    return (rz_function_t*) box.as.obj;
}

void rz_debug_print_box(rz_box_t box);

#endif
