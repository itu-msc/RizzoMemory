#pragma once

#include "allocation.h"
#include "core.h"
#include "later.h"
#include "channel.h"
#include "os.h"

/* TODO: assume some has tag 1 */
#define RZ_TAG_SOME 1

typedef struct rz_signal
{
    rz_header_t _base;
    /* fields */
    rz_box_t head, tail; /* could be any (boxed) value   */
    rz_box_t updated;    /* an integer, either 0 or 1    */
    rz_box_t prev, next; /* holders a pointer (of rz_signal_t) to the prev/next signal in the heap */
#ifdef __RZ_DEBUG_INFO
    rz_box_t debug_index;
#endif
} rz_signal_t;

#define RZ_HEAP_SENTINEL_TAG 255

static rz_signal_t rz_heap_head = {
    ._base = {.num_fields = 0, .tag = RZ_HEAP_SENTINEL_TAG, .obj_type = RZ_SIGNAL, .refcount = -1},
    .head = {.kind = RZ_BOX_INT, .as.i64 = 0},
    .tail = {.kind = RZ_BOX_PTR, .as.obj = &RZ_NEVER_OBJ},
    .updated = {.kind = RZ_BOX_INT, .as.i64 = 0},
    .prev = {.kind = RZ_BOX_PTR, .as.obj = NULL},
    .next = {.kind = RZ_BOX_PTR, .as.obj = NULL},
};

static rz_signal_t rz_heap_tail = {
    ._base = {.num_fields = 0, .tag = RZ_HEAP_SENTINEL_TAG, .obj_type = RZ_SIGNAL, .refcount = -1},
    .head = {.kind = RZ_BOX_INT, .as.i64 = 0},
    .tail = {.kind = RZ_BOX_PTR, .as.obj = &RZ_NEVER_OBJ},
    .updated = {.kind = RZ_BOX_INT, .as.i64 = 0},
    .prev = {.kind = RZ_BOX_PTR, .as.obj = NULL},
    .next = {.kind = RZ_BOX_PTR, .as.obj = NULL},
};

rz_signal_t *rz_heap_base = &rz_heap_head;
rz_signal_t *rz_heap_cursor = &rz_heap_tail;
size_t rz_heap_size = 0;
#ifdef __RZ_DEBUG_INFO
uint64_t rz_debug_signal_next_index = 0;
#endif

static inline void rz_heap_init(void)
{
    rz_heap_head.next = rz_make_ptr((rz_object_t *)&rz_heap_tail);
    rz_heap_tail.prev = rz_make_ptr((rz_object_t *)&rz_heap_head);

    rz_heap_base = &rz_heap_head;
    rz_heap_cursor = &rz_heap_tail;
    rz_heap_size = 0;
}

static inline void rz_remove_signal_node(rz_signal_t *sig)
{
    rz_signal_t *prev = (rz_signal_t *)sig->prev.as.obj;
    rz_signal_t *next = (rz_signal_t *)sig->next.as.obj;

    prev->next.as.obj = (rz_object_t *)next;
    next->prev.as.obj = (rz_object_t *)prev;

    if (rz_heap_cursor == sig)
        rz_heap_cursor = next;
}

static inline void rz_insert_signal_node(rz_signal_t *sig)
{
    rz_signal_t *prev = (rz_signal_t *)rz_unbox_ptr(rz_heap_cursor->prev);
    rz_signal_t *next = rz_heap_cursor;

    sig->prev.as.obj = (rz_object_t *)prev;
    sig->next.as.obj = (rz_object_t *)next;

    next->prev.as.obj = (rz_object_t *)sig;
    prev->next.as.obj = (rz_object_t *)sig;
}

/* constructs a new signal AND inserts it into the global heap.
STATEFUL: updates the global heap, mutates the heap cursor, and prev/next pointers.
    s1
    s2 <- cursor
    s3

    after rz_signal_ctor
    s1, s2, new_sig, s3
*/
static rz_object_t *rz_signal_ctor(rz_box_t head, rz_box_t tail)
{
    rz_heap_size++;

#ifndef __RZ_DEBUG_INFO
    int16_t args_cnt = 5;
    rz_box_t args[5] = {head, tail, rz_make_int(0), rz_make_ptr(NULL), rz_make_ptr(NULL)};
#else
    int16_t args_cnt = 6;
    rz_box_t args[6] = {head, tail, rz_make_int(1), rz_make_ptr(NULL), rz_make_ptr(NULL), rz_make_int(rz_debug_signal_next_index++)};
#endif
    rz_signal_t *new_sig = (rz_signal_t *)rz_ctor(0, args_cnt, args);
    new_sig->_base.obj_type = RZ_SIGNAL;

    rz_insert_signal_node(new_sig);

    return (rz_object_t *)new_sig;
}

static inline rz_box_t rz_make_ptr_sig(rz_object_t *sig)
{
    return (rz_box_t){.kind = RZ_BOX_PTR, .as.obj = sig};
}

static inline void rz_signal_free(rz_object_t *obj)
{
    rz_signal_t *sig = (rz_signal_t *)obj;
    rz_refcount_dec_box(sig->head);
    rz_refcount_dec_box(sig->tail);
    rz_remove_signal_node(sig);
    rz_free(sig);
    rz_heap_size--;
}

static rz_object_t* rz_reset_signal(rz_object_t* obj)
{
    /* reset-shared*/
    if (obj->header.refcount != 1) {
        rz_refcount_dec(obj);
        return NULL;
    }
    /* reset-unique */
    rz_signal_t* sig = (rz_signal_t*)obj;
    rz_refcount_dec_box(sig->head);
    rz_refcount_dec_box(sig->tail);
    sig->updated = rz_make_int(0);
    rz_remove_signal_node(sig);
    return obj;
}

static inline rz_object_t *rz_reuse_signal(rz_object_t *obj, rz_box_t head, rz_box_t tail)
{
    if (obj == NULL)
    {
        return rz_signal_ctor(head, tail);
    }

    rz_signal_t *sig = (rz_signal_t *)obj;
    rz_refcount_dec_box(sig->head);
    rz_refcount_dec_box(sig->tail);

    /* Keep the currently processed source signal in place.
       Removing it would advance rz_heap_cursor to the next node, and reinserting
       relative to that cursor would move the source behind its dependents. */
    if (rz_heap_cursor == sig)
    {
        sig->updated.as.i64 = 0;
        sig->_base.num_fields = 5;
        sig->_base.tag = 0;
        sig->_base.obj_type = RZ_SIGNAL;
        sig->head = head;
        sig->tail = tail;
#ifdef __RZ_DEBUG_INFO
        sig->debug_index = rz_make_int(rz_debug_signal_next_index++);
#endif
        return obj;
    }

    rz_remove_signal_node(sig);

    sig->updated.as.i64 = 0;
    sig->_base.num_fields = 5;
    sig->_base.tag = 0;
    sig->_base.obj_type = RZ_SIGNAL;
    sig->head = head;
    sig->tail = tail;

     /* rz_insert_signal_node will update the prev/next ptr,
         so the signal is correctly placed just before the heap cursor */
    rz_insert_signal_node(sig);

#ifdef __RZ_DEBUG_MALLOC
    printf("reuse: %p - now stores a signal\n", (void*)sig);
#endif
    return obj;
}

#ifdef __RZ_DEBUG_INFO
static inline void rz_debug_print_heap()
{
    printf("(size: %zu) ", rz_heap_size);
    for (rz_signal_t *sig = &rz_heap_head; sig != NULL; sig = (rz_signal_t *)rz_unbox_ptr(sig->next))
    {
        if (sig == &rz_heap_head || sig == &rz_heap_tail)
        {
            if (sig == rz_heap_cursor)
                printf("(|) ");
            else
                printf("| ");
        }
        else if (rz_unbox_int(sig->updated))
        {
            printf("[%zu] ", rz_unbox_int(sig->debug_index));
        }
        else
        {
            if (sig == rz_heap_cursor)
                printf("(%zu) ", rz_unbox_int(sig->debug_index));
            else
                printf("%zu ", rz_unbox_int(sig->debug_index));
        }
    }
    printf("\n");
}
#endif

static void rz_debug_print_signal(rz_box_t box)
{
    rz_signal_t *signal = (rz_signal_t *)rz_unbox_ptr(box);
    printf("signal(ref: %d, head: ", signal->_base.refcount);
    rz_debug_print_box(signal->head);
    printf(", tail: ");
    rz_debug_print_box(signal->tail);
    printf(", updated: %"PRId64")", rz_unbox_int(signal->updated));
}

static inline rz_box_t rz_signal_eq(rz_object_t *a, rz_object_t *b) 
{
    if (rz_object_get_type(a) != RZ_SIGNAL || rz_object_get_type(b) != RZ_SIGNAL) 
        return rz_make_ptr(rz_bool_ctor(false));
    
    rz_signal_t *sig_a = (rz_signal_t *)a;
    rz_signal_t *sig_b = (rz_signal_t *)b;

    rz_object_t *heads_equal = rz_unbox_ptr(rz_eq(sig_a->head, sig_b->head));
    rz_object_t *tails_equal = rz_unbox_ptr(rz_eq(sig_a->tail, sig_b->tail));
    bool res = rz_object_tag(heads_equal) == RZ_TAG_BOOL_TRUE 
               && rz_object_tag(tails_equal) == RZ_TAG_BOOL_TRUE;
    return rz_make_ptr(rz_bool_ctor(res));
}

/*   |------------------------------|
     |      REACTIVE SEMANTICS      |
     |      ...                     |
     |------------------------------|
*/

static bool rz_ticked(rz_object_t *later, rz_channel_t chan, rz_box_t v);
static void rz_heap_update(rz_channel_t chan, rz_box_t v);
static rz_box_t rz_advance(rz_object_t *later, rz_channel_t chan, rz_box_t v);
static inline rz_box_t rz_advance_delayed(rz_box_t delayed);

static void rz_heap_update(rz_channel_t chan, rz_box_t v)
{
    if (rz_unbox_ptr(rz_heap_head.next) == (rz_object_t *)&rz_heap_tail)
        return;
    rz_heap_cursor = (rz_signal_t *)rz_unbox_ptr(rz_heap_head.next);
    while (rz_heap_cursor != &rz_heap_tail)
    {
        rz_object_t *tl = rz_unbox_ptr(rz_heap_cursor->tail);
        if (!rz_ticked(tl, chan, v))
        {
            rz_heap_cursor->updated.as.i64 = false;
        }
        else
        { /* chan produced a tick on tl of cursor! */
            rz_refcount_dec_box(rz_heap_cursor->head);
            rz_box_t l_boxed = rz_advance(tl, chan, v);
            rz_signal_t *l = (rz_signal_t *)rz_unbox_ptr(l_boxed);
            rz_heap_cursor->updated.as.i64 = true;
            rz_refcount_inc_box(l->head);
            rz_refcount_inc_box(l->tail);
            rz_heap_cursor->head = l->head;
            rz_heap_cursor->tail = l->tail;

            rz_refcount_dec_box(l_boxed);
        }
        rz_heap_cursor = (rz_signal_t *)rz_unbox_ptr(rz_heap_cursor->next);
    }
}

/** Checks whether a later value produced a tick on the current time step for the channel [chan]
 *  Takes borrowed references to [later], [chan], and [v]. */
static bool rz_ticked(rz_object_t *later, rz_channel_t chan, rz_box_t v)
{
    switch (rz_object_tag(later))
    {
    case RZ_TAG_LATER_NEVER:
        return false;
    case RZ_TAG_LATER_APP:
    {
        rz_box_t w = rz_object_get_field(later, 1);
        return rz_ticked(w.as.obj, chan, v);
    }
    case RZ_TAG_LATER_WAIT:
    {
        rz_box_t k = rz_object_get_field(later, 0);
        return k.as.i64 == chan;
    }
    case RZ_TAG_LATER_WATCH:
    {
        rz_signal_t *l = (rz_signal_t *)rz_unbox_ptr(rz_object_get_field(later, 0));
        return (RZ_TAG_SOME == rz_object_tag(rz_unbox_ptr(l->head))) && rz_unbox_int(l->updated);
    }
    case RZ_TAG_LATER_TAIL:
    {
        rz_signal_t *l = (rz_signal_t *)rz_unbox_ptr(rz_object_get_field(later, 0));
        return rz_unbox_int(l->updated);
    }
    case RZ_TAG_LATER_SYNC:
    {
        rz_box_t later1 = rz_object_get_field(later, 0);
        rz_box_t later2 = rz_object_get_field(later, 1);
        return rz_ticked(rz_unbox_ptr(later1), chan, v) || rz_ticked(rz_unbox_ptr(later2), chan, v);
    }
    default:
        printf("rz_ticked(%s) - unknown later tag '%d'", __FILE__, rz_object_tag(later));
        exit(1);
    }
}

/** Advances a value of the 'Later' type (watch, wait, tail, sync, laterapp)
 *  which produces a value for that later for the current time step.
 *  Takes an owned reference to [later] and borrowed references to [chan] and [v].
 *  Returns: owned reference to resulting value 
 *  Note: There is no caching of the resulting value. This is a limitation
 */
static rz_box_t rz_advance(rz_object_t *later, rz_channel_t chan, rz_box_t v)
{
    switch (rz_object_tag(later))
    {
    case RZ_TAG_LATER_WAIT:
    {
        /* Technically we have already checked (wait k) -> k = chan */
        rz_refcount_inc_box(v);
        rz_refcount_dec(later);
        return v;
    }
    case RZ_TAG_LATER_APP:
    {
        rz_box_t delayed_fun = rz_object_get_field(later, 0);
        rz_refcount_inc_box(delayed_fun);
        rz_box_t fun = rz_advance_delayed(delayed_fun);
        rz_box_t arg_later = rz_object_get_field(later, 1);
        rz_refcount_inc_box(arg_later);
        rz_refcount_dec(later);
        rz_box_t arg = rz_advance(rz_unbox_ptr(arg_later), chan, v);
        return rz_apply1(rz_unbox_ptr(fun), arg);
    }
    case RZ_TAG_LATER_TAIL:
    {
        rz_box_t l = rz_object_get_field(later, 0);
        rz_refcount_inc_box(l);
        rz_refcount_dec(later);
        return l;
    }
    case RZ_TAG_LATER_WATCH:
    {
        rz_signal_t *l = (rz_signal_t *)rz_unbox_ptr(rz_object_get_field(later, 0));
        rz_object_t *some = rz_unbox_ptr(l->head);
        if (RZ_TAG_SOME != rz_object_tag(some))
        {
            fprintf(stderr, "RUNTIME ERROR: tried to advance signal where head wasn't a SOME");
            exit(1);
        }
        /* assume some -> ctor1(v) */
        rz_box_t v = rz_object_get_field(some, 0);
        rz_refcount_inc_box(v);
        rz_refcount_dec(later);
        return v;
    }
    case RZ_TAG_LATER_SYNC:
    {
        rz_object_t *v1 = rz_unbox_ptr(rz_object_get_field(later, 0));
        rz_object_t *v2 = rz_unbox_ptr(rz_object_get_field(later, 1));
        rz_refcount_inc(v1);
        rz_refcount_inc(v2);
        rz_object_t *ll = rz_reset_object(later);
        bool ticked1 = rz_ticked(v1, chan, v);
        bool ticked2 = rz_ticked(v2, chan, v);
        if (ticked1 && ticked2)
        {
            rz_box_t u1 = rz_advance(v1, chan, v);
            rz_box_t u2 = rz_advance(v2, chan, v);
            rz_object_t *both = rz_reuse_object(ll, RZ_TAG_SYNC_BOTH, 2, (rz_box_t[]){u1, u2});
            return rz_make_ptr(both);
        }
        else if (ticked1)
        {
            rz_refcount_dec(v2);
            rz_box_t u1 = rz_advance(v1, chan, v);
            rz_object_t *left = rz_ctor_var(RZ_TAG_SYNC_LEFT, 1, u1);
            return rz_make_ptr(left);
        }
        else /* if (ticked2) */
        {
            rz_refcount_dec(v1);
            rz_box_t u2 = rz_advance(v2, chan, v);
            rz_object_t *right = rz_ctor_var(RZ_TAG_SYNC_RIGHT, 1, u2);
            return rz_make_ptr(right);
        }
    }
    case RZ_TAG_LATER_NEVER: /* never happens*/
    default:
        printf("rz_advance(%s) - unknown later tag '%d'", __FILE__, rz_object_tag(later));
        exit(1);
    }
}

/** Forces a Delayed value (either 'delay' or 'ostar') - NO caching of the resulting values.
 * Takes an owned reference to [delayed]
 */
static inline rz_box_t rz_advance_delayed(rz_box_t delayed) {
    rz_object_t* ptr_delayed = rz_unbox_ptr(delayed);
    switch (rz_object_tag(ptr_delayed)) {
        case RZ_TAG_DELAY: {
            rz_object_t* thunk = rz_unbox_ptr(rz_object_get_field(ptr_delayed, 0));
            rz_refcount_inc(thunk);
            rz_refcount_dec(ptr_delayed);
            return rz_apply1(thunk, RZ_UNIT);
        }
        case RZ_TAG_OSTAR: {
            // typeof OSTAR: Delayed ('a -> 'b) -> Delayed 'a -> Delayed 'b
            rz_box_t operand_left_delay = rz_object_get_field(ptr_delayed, 0);
            rz_box_t operand_right_delay = rz_object_get_field(ptr_delayed, 1);
            rz_refcount_inc_box(operand_left_delay);
            rz_refcount_inc_box(operand_right_delay);
            rz_refcount_dec(ptr_delayed);
            rz_object_t* f =  rz_unbox_ptr(rz_advance_delayed(operand_left_delay));
            rz_box_t operand_right = rz_advance_delayed(operand_right_delay);
            return rz_apply1(f, operand_right);
        }
        default: {
            printf("Unknown delayed tag in 'rz_advance_delayed': %d", rz_object_tag(ptr_delayed));
            exit(1);
        }
    }
    
}
