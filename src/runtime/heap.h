#pragma once

#include "allocation.h"
#include "core.h"
#include "later.h"
#include "channel.h"
#include "os.h"

/* TODO: assume some has tag 1 */
#define RZ_TAG_SOME 1

typedef struct rz_signal {
    rz_header_t _base;
    /* fields */
    rz_box_t head, tail;    /* could be any (boxed) value   */
    rz_box_t updated;       /* an integer, either 0 or 1    */
    rz_box_t prev, next;    /* holders a pointer (of rz_signal_t) to the prev/next signal in the heap */
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
        rz_box_t args[5] = {head, tail, rz_make_int(0), rz_make_ptr(NULL), rz_make_ptr(NULL)};
        rz_heap_base = (rz_signal_t*) rz_ctor(0, 5, args);
        rz_heap_base->_base.num_fields = 2; /* only count head and tail as fields for ref counting purposes */
        rz_heap_cursor = rz_heap_base;
        return (rz_object_t*) rz_heap_base;
    }

    if (rz_heap_cursor == NULL) {
        fprintf(stderr, "Heap cursor is NULL\n");
        exit(1);
    }

    //TODO: swap around, prev should be cursor.prev, next is cursor
    rz_signal_t* prev = (rz_signal_t*) rz_unbox_ptr(rz_heap_cursor->prev);
    rz_signal_t* next = rz_heap_cursor;

    rz_box_t args[5] = {head, tail, rz_make_int(0), rz_make_ptr((rz_object_t*)prev), rz_make_ptr((rz_object_t*)next)};
    rz_signal_t* new_sig = (rz_signal_t*) rz_ctor(0, 5, args);
    new_sig->_base.num_fields = 2; /* only count head and tail as fields for ref counting purposes */
    
    if (next) next->prev.as.obj = (rz_object_t*) new_sig;
    if (prev) prev->next.as.obj = (rz_object_t*) new_sig;
    if (rz_heap_cursor == rz_heap_base) rz_heap_base = new_sig;
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
    rz_free(sig);
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
        char* const updated_str = rz_unbox_int(cursor->updated) ? "true" : "false";
        printf(", U: %s, prev: %p, next: %p);\n", updated_str, rz_unbox_ptr(cursor->prev), rz_unbox_ptr(cursor->next));
    }
}

/*   |------------------------------|
     |      REACTIVE SEMANTICS      |
     |      ...                     |
     |------------------------------|
*/

static bool rz_ticked(rz_object_t* later, rz_channel_t chan, rz_box_t v);
static void rz_heap_update(rz_channel_t chan, rz_box_t v);
static rz_box_t rz_advance(rz_object_t* later, rz_channel_t chan, rz_box_t v);

static void rz_heap_update(rz_channel_t chan, rz_box_t v) {
    if(!rz_heap_base) return;
    rz_signal_t* cur = rz_heap_cursor = rz_heap_base;
    while(cur) {
        rz_heap_cursor = cur;
        rz_object_t* tl = rz_unbox_ptr(rz_heap_cursor->tail);
        if(!rz_ticked(tl, chan, v)) {
            rz_heap_cursor->updated.as.i32 = false;
        } else { /* chan produced a tick on tl of cursor! */
            rz_box_t l_boxed = rz_advance(tl, chan, v);
            rz_signal_t* l = (rz_signal_t*)rz_unbox_ptr(l_boxed);
            rz_heap_cursor->updated.as.i32 = true;
            rz_refcount_inc_box(l->head);
            rz_refcount_inc_box(l->tail);
            rz_refcount_dec_box(rz_heap_cursor->head);
            rz_refcount_dec_box(rz_heap_cursor->tail);
            rz_heap_cursor->head = l->head;
            rz_heap_cursor->tail = l->tail;
            rz_refcount_dec_box(l_boxed);
        }
        cur = (rz_signal_t*)rz_unbox_ptr(rz_heap_cursor->next);
    }
}

static bool rz_ticked(rz_object_t* later, rz_channel_t chan, rz_box_t v) {
    switch (rz_object_tag(later)) {
        case RZ_TAG_LATER_NEVER: return false;
        case RZ_TAG_LATER_APP: {
            rz_box_t w = rz_object_get_field(later, 1);
            return rz_ticked(w.as.obj, chan, v);
        }
        case RZ_TAG_LATER_WAIT: {
            rz_box_t k = rz_object_get_field(later, 0);
            return k.as.i32 == chan;
        }
        case RZ_TAG_LATER_WATCH: {
            rz_signal_t* l = (rz_signal_t*) rz_unbox_ptr(rz_object_get_field(later, 0));
            return (RZ_TAG_SOME == rz_object_tag(rz_unbox_ptr(l->head))) && rz_unbox_int(l->updated);
        }
        case RZ_TAG_LATER_TAIL: {
            rz_signal_t* l = (rz_signal_t*) rz_unbox_ptr(rz_object_get_field(later, 0));
            return rz_unbox_int(l->updated);
        }
        case RZ_TAG_LATER_SYNC: {
            rz_box_t later1 = rz_object_get_field(later, 0);
            rz_box_t later2 = rz_object_get_field(later, 1);
            return rz_ticked(rz_unbox_ptr(later1), chan, v) 
                || rz_ticked(rz_unbox_ptr(later2), chan, v);
        }
        default: 
            printf("rz_ticked(%s) - unknown later tag '%d'", __FILE__, rz_object_tag(later));
            exit(1);
    }
}

/** No inc/dec are performed on the argument [later since it doesn't do reuse (yet)
 *  Values returned by [rz_advanced] have been incremented */
static rz_box_t rz_advance(rz_object_t* later, rz_channel_t chan, rz_box_t v) {
    switch (rz_object_tag(later)) {
        case RZ_TAG_LATER_WAIT: {
            /* Technically we have already checked (wait k) -> k = chan */
            rz_refcount_inc_box(v);
            return v;
        }
        case RZ_TAG_LATER_APP: {
            /* let arg_later = proj1 later in
               inc(arg_later)                       <- if later is owned, then so must its projections
               let advanced_arg = rz_advance(arg_later, chan, v) in
               let delayed_fun = proj0 later in
               inc(delayed_fun)                     <- if later is owned, then so must its projections
               dec(later)                           <- don't need the later value beyond this point
               let fun = proj0 delayed_fun in 
               inc(fun)                             <- again, a projection of an owned variable (delayed_fun)
               dec(delayed_fun) <- don't need delayed_fun anymore
               let res = varapp(fun, advanced_arg)
               ret res */
            rz_box_t arg_later = rz_object_get_field(later, 1);
            rz_box_t arg = rz_advance(rz_unbox_ptr(arg_later), chan, v);
            rz_refcount_inc_box(arg);
            rz_box_t delayed_fun = rz_object_get_field(later, 0);
            rz_box_t fun = rz_object_get_field(rz_unbox_ptr(delayed_fun), 0);
            rz_refcount_inc_box(fun);
            return rz_apply1(rz_unbox_ptr(fun), arg);
        } 
        case RZ_TAG_LATER_TAIL: {
            rz_box_t l = rz_object_get_field(later,0);
            rz_refcount_inc_box(l);
            return l;
        }
        case RZ_TAG_LATER_WATCH: {
            rz_signal_t* l = (rz_signal_t*)rz_unbox_ptr(rz_object_get_field(later, 0));
            rz_object_t* some = rz_unbox_ptr(l->head);
            if (RZ_TAG_SOME != rz_object_tag(some)) {
                fprintf(stderr, "RUNTIME ERROR: tried to advance signal where head wasn't a SOME");
                exit(1);
            }
            /* assume some -> ctor1(v) */
            rz_box_t v = rz_object_get_field(some, 0);
            rz_refcount_inc_box(v);
            return v;
        }
        case RZ_TAG_LATER_SYNC: {
            rz_object_t* v1 = rz_unbox_ptr(rz_object_get_field(later, 0));
            rz_object_t* v2 = rz_unbox_ptr(rz_object_get_field(later, 1));
            bool ticked1 = rz_ticked(v1, chan, v);
            bool ticked2 = rz_ticked(v2, chan, v);
            if (ticked1 && ticked2) {
                rz_box_t u1 = rz_advance(v1, chan, v);
                rz_box_t u2 = rz_advance(v2, chan, v);
                rz_object_t* both = rz_ctor_var(RZ_TAG_SYNC_BOTH, 2, u1, u2);
                return rz_make_ptr(both);
            } 
            else if (ticked1) { 
                rz_box_t u1 = rz_advance(v1, chan, v);
                rz_object_t* left = rz_ctor_var(RZ_TAG_SYNC_LEFT, 1, u1);
                return rz_make_ptr(left);
            }
            else /* if (ticked2) */ { 
                rz_box_t u2 = rz_advance(v2, chan, v);
                rz_object_t* right = rz_ctor_var(RZ_TAG_SYNC_RIGHT, 1, u2);
                return rz_make_ptr(right);
            }
        }
        case RZ_TAG_LATER_NEVER: /* never happens*/
        default: 
            printf("rz_advance(%s) - unknown later tag '%d'", __FILE__, rz_object_tag(later));
            exit(1);
    }
}
