#pragma once

#include "core.h"
#include "later.h"

static void rz_debug_print_heap();

typedef struct rz_signal {
    rz_header_t _base;
    /* fields */
    rz_box_t head, tail, prev, next;
} rz_signal_t;

rz_signal_t* rz_heap_base = NULL;
rz_signal_t* rz_heap_cursor = NULL;
size_t rz_heap_size = 0;

/* constructs a new signal AND inserts it into the global heap. 
STATEFUL: updates the global heap, mutates the heap cursor, and prev/next pointers.
    s1
    s2 <- cursor
    s3

    after rz_signal_ctor
    s1, s2, new_sig, s3
*/
static rz_object_t* rz_signal_ctor(rz_box_t head, rz_box_t tail) {
    rz_heap_size++;
    if (rz_heap_base == NULL) {
        rz_box_t args[4] = {head, tail, rz_make_ptr(NULL), rz_make_ptr(NULL)};
        rz_heap_base = (rz_signal_t*) rz_ctor(0, 4, args);
        rz_heap_base->_base.num_fields = 2; /* only count head and tail as fields for ref counting purposes */
        rz_heap_cursor = rz_heap_base;
        return (rz_object_t*) rz_heap_base;
    }

    if (rz_heap_cursor == NULL) {
        fprintf(stderr, "Heap cursor is NULL\n");
        exit(1);
    }

    rz_signal_t* prev = (rz_signal_t*)rz_heap_cursor;
    rz_signal_t* next = (rz_signal_t*)rz_heap_cursor->next.as.obj;
    /* TODO: to ref count or not to ref count these? */
    // rz_refcount_inc((rz_object_t*)prev); 
    // rz_refcount_inc((rz_object_t*)next);

    rz_box_t args[4] = {head, tail, rz_make_ptr((rz_object_t*)prev), rz_make_ptr((rz_object_t*)next)};
    rz_signal_t* new_sig = (rz_signal_t*) rz_ctor(0, 4, args);
    new_sig->_base.num_fields = 2; /* only count head and tail as fields for ref counting purposes */
    
    if (next) next->prev.as.obj = (rz_object_t*) new_sig;
    if (prev) prev->next.as.obj = (rz_object_t*) new_sig;
    return (rz_object_t*) new_sig;
}

static inline rz_box_t rz_make_ptr_sig(rz_object_t* sig) {
    return (rz_box_t){.kind = RZ_BOX_SIGNAL, .as.obj = sig};
}

static inline void rz_signal_free(rz_object_t* obj) {
    rz_signal_t* sig = (rz_signal_t*) obj;
    rz_signal_t* prev = (rz_signal_t*)sig->prev.as.obj;
    rz_signal_t* next = (rz_signal_t*)sig->next.as.obj;

    if (prev) prev->next.as.obj = (rz_object_t*) next;
    else rz_heap_base = next;
    if (next) next->prev.as.obj = (rz_object_t*) prev;

    if(rz_heap_cursor == sig) rz_heap_cursor = next ? next : prev;
    free(sig);
    rz_heap_size--;
}

static void rz_debug_print_heap() {
    if (rz_heap_base == NULL) {
        printf("Heap is empty\n");
        return;
    }
    rz_signal_t* cursor = rz_heap_base;
    printf("Heap size: %zu\n", rz_heap_size);
    for(int i = 0; cursor; cursor = (rz_signal_t*)cursor->next.as.obj, i++) {
        printf("%d:(ref:%d) (", i, cursor->_base.refcount);
        rz_debug_print_box(cursor->head);
        printf(", ");
        rz_debug_print_later(cursor->tail);
        printf(", prev: %p, next: %p);\n", cursor->prev.as.obj, cursor->next.as.obj);
    }
}
