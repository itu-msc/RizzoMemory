#pragma once

#include "core.h"

typedef struct rz_string {
    rz_object_t _base;
    size_t byte_length;
    char bytes[];
} rz_string_t;

static inline const char* rz_string_data(rz_box_t box) {
    rz_box_kind_t kind = rz_box_get_kind(box);
    if (kind == RZ_BOX_STRING_LITERAL) {
        return rz_unbox_str_lit(box);
    }
    if (kind == RZ_BOX_PTR && rz_object_get_type(rz_unbox_ptr(box)) == RZ_STRING) {
        return ((rz_string_t*)rz_unbox_ptr(box))->bytes;
    }
    fprintf(stderr, "Runtime error: expected string box, got kind %d\n", kind);
    exit(1);
}

static inline size_t rz_string_byte_length(rz_box_t box) {
    rz_box_kind_t kind = rz_box_get_kind(box);
    if (kind == RZ_BOX_STRING_LITERAL) {
        return strlen(rz_unbox_str_lit(box));
    }
    if (kind == RZ_BOX_PTR && rz_object_get_type(rz_unbox_ptr(box)) == RZ_STRING) {
        return ((rz_string_t*)rz_unbox_ptr(box))->byte_length;
    }
    fprintf(stderr, "Runtime error: expected string box, got kind %d\n", kind);
    exit(1);
}

static inline rz_string_t* rz_alloc_string(size_t len) {
    rz_string_t* str = (rz_string_t*) rz_malloc(sizeof(rz_string_t) + len + 1);
    str->_base.header.tag = 0;
    str->_base.header.num_fields = 0;
    str->_base.header.obj_type = RZ_STRING;
    str->_base.header.refcount = 1;
    str->byte_length = len;
    str->bytes[len] = '\0';
    return str;
}

static inline rz_box_t rz_make_string_len(const char* bytes, size_t len) {
    rz_string_t* str = rz_alloc_string(len);
    memcpy(str->bytes, bytes, len);
    return rz_make_ptr((rz_object_t*)str);
}

static inline size_t rz_utf8_codepoint_width(unsigned char lead) {
    if ((lead & 0x80) == 0x00) return 1;
    if ((lead & 0xE0) == 0xC0) return 2;
    if ((lead & 0xF0) == 0xE0) return 3;
    if ((lead & 0xF8) == 0xF0) return 4;
    fprintf(stderr, "Runtime error: invalid UTF-8 leading byte 0x%02X\n", lead);
    exit(1);
}

static inline bool rz_string_eq_content(rz_box_t a, rz_box_t b) {
    size_t a_len = rz_string_byte_length(a);
    size_t b_len = rz_string_byte_length(b);
    return a_len == b_len && memcmp(rz_string_data(a), rz_string_data(b), a_len) == 0;
}
