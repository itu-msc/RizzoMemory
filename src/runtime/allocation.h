#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef __RZ_DEBUG_MALLOC
static void* rz_debug_malloc(size_t size, char* file, int line) {
    void* ptr = malloc(size);
    printf("(%s, %d) malloc: %zu bytes at %p\n", file, line, size, ptr);
    return ptr;
}

static void rz_debug_free(void* ptr) {
    printf("free: at %p\n", ptr);
    free(ptr);
}

#define rz_malloc(size) rz_debug_malloc(size, __FILE__, __LINE__)
#define rz_free rz_debug_free
#else
#define rz_malloc(size) malloc(size)
#define rz_free free
#endif
