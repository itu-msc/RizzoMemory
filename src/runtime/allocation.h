#pragma once

#include <stdlib.h>
#include <stdio.h>

#ifdef __RZ_DEBUG_MALLOC
static void* rz_debug_malloc(size_t size) {
    void* ptr = malloc(size);
    printf("malloc: %zu bytes at %p\n", size, ptr);
    return ptr;
}

static void rz_debug_free(void* ptr) {
    printf("free: at %p\n", ptr);
    free(ptr);
}

#define rz_malloc(size) rz_debug_malloc(size)
#define rz_free rz_debug_free
#else
#define rz_malloc(size) malloc(size)
#define rz_free free
#endif
