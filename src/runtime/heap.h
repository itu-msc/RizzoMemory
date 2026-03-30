#pragma once

#include "allocation.h"
#include "core.h"
#include "later.h"
#include "channel.h"
#include "os.h"

// #define RZ_DEBUG // TODO remove when done with debugging

/* TODO: assume some has tag 1 */
#define RZ_TAG_SOME 1

typedef struct rz_signal
{
    rz_header_t _base;
    /* fields */
    rz_box_t head, tail; /* could be any (boxed) value   */
    rz_box_t updated;    /* an integer, either 0 or 1    */
    rz_box_t prev, next; /* holders a pointer (of rz_signal_t) to the prev/next signal in the heap */
#ifdef RZ_DEBUG
    size_t debug_index;
#endif
} rz_signal_t;

#ifdef RZ_DEBUG
static size_t rz_signal_debug_index = 0;
#endif

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
#ifdef RZ_DEBUG
    int signal_arguments = 6;
    rz_box_t args[6] = {head, tail, rz_make_int(1), rz_make_ptr(NULL), rz_make_ptr(NULL), rz_signal_debug_index++};
#else
    int signal_arguments = 5;
    rz_box_t args[5] = {head, tail, rz_make_int(0), rz_make_ptr(NULL), rz_make_ptr(NULL)};
#endif

    rz_signal_t *new_sig = (rz_signal_t *)rz_ctor(0, signal_arguments, args);
    new_sig->_base.obj_type = RZ_SIGNAL;

    rz_insert_signal_node(new_sig);

#ifdef RZ_DEBUG
    // printf("Created signal %zu\n\thead: ", new_sig->debug_index);
    // rz_debug_print_box(head);
    // printf("\n\ttail: ");
    // rz_debug_print_box(tail);
    // printf("\n\n");
#endif

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
    return obj;
}

static void rz_debug_print_heap()
{
#ifdef RZ_DEBUG
    rz_signal_t *sig = &rz_heap_head;

    while (sig)
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
            printf("[%zu] ", sig->debug_index);
        }
        else
        {
            if (sig == rz_heap_cursor)
                printf("(%zu) ", sig->debug_index);
            else
                printf("%zu ", sig->debug_index);
        }

        sig = (rz_signal_t *)rz_unbox_ptr(sig->next);
    }

    printf("\n");
#endif
}

static void rz_debug_print_signal(rz_box_t box)
{
    rz_signal_t *signal = (rz_signal_t *)box.as.obj;
    if (!signal) {
        printf("signal(null)");
        return;
    }
    if (rz_debug_print_object_is_active((rz_object_t *)signal)) {
        printf("<cycle@%p>", (void *)signal);
        return;
    }
    if (!rz_debug_print_object_push((rz_object_t *)signal)) {
        printf("<depth-limit@%p>", (void *)signal);
        return;
    }

    printf("signal(ref=%d, updated=%s)", signal->_base.refcount, rz_unbox_int(signal->updated) ? "true" : "false");
    printf(" {\n");

    rz_debug_print_indent(rz_debug_print_depth());
    printf("head = ");
    rz_debug_print_box(signal->head);
    printf("\n");

    rz_debug_print_indent(rz_debug_print_depth());
    printf("tail = ");
    rz_debug_print_box(signal->tail);
    printf("\n");

    rz_debug_print_indent(rz_debug_print_depth() - 1);
    printf("}");
    rz_debug_print_object_pop();
}

/*   |------------------------------|
     |      REACTIVE SEMANTICS      |
     |      ...                     |
     |------------------------------|
*/

static bool rz_ticked(rz_object_t *later, rz_channel_t chan, rz_box_t v);
static void rz_heap_update(rz_channel_t chan, rz_box_t v);
static rz_box_t rz_advance(rz_object_t *later, rz_channel_t chan, rz_box_t v);

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
            rz_box_t l_boxed = rz_advance(tl, chan, v);
            rz_signal_t *l = (rz_signal_t *)rz_unbox_ptr(l_boxed);
            rz_heap_cursor->updated.as.i64 = true;
            rz_refcount_inc_box(l->head);
            rz_refcount_inc_box(l->tail);
            rz_refcount_dec_box(rz_heap_cursor->head);
            rz_refcount_dec_box(rz_heap_cursor->tail);
            rz_heap_cursor->head = l->head;
            rz_heap_cursor->tail = l->tail;

            rz_refcount_dec_box(l_boxed);
        }
        rz_heap_cursor = (rz_signal_t *)rz_unbox_ptr(rz_heap_cursor->next);
    }
}

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

/** No inc/dec are performed on the argument [later since it doesn't do reuse (yet)
 *  Values returned by [rz_advanced] have been incremented */
static rz_box_t rz_advance(rz_object_t *later, rz_channel_t chan, rz_box_t v)
{
    switch (rz_object_tag(later))
    {
    case RZ_TAG_LATER_WAIT:
    {
        /* Technically we have already checked (wait k) -> k = chan */
        rz_refcount_inc_box(v);
        return v;
    }
    case RZ_TAG_LATER_APP:
    {
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
        rz_box_t delayed_fun = rz_object_get_field(later, 0);
        rz_box_t fun = rz_object_get_field(rz_unbox_ptr(delayed_fun), 0);
        rz_refcount_inc_box(fun);
        // although arg is used in apply1 (varapp), it doesn't need inc because advance returns +1
        return rz_apply1(rz_unbox_ptr(fun), arg);
    }
    case RZ_TAG_LATER_TAIL:
    {
        rz_box_t l = rz_object_get_field(later, 0);
        rz_refcount_inc_box(l);
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
        return v;
    }
    case RZ_TAG_LATER_SYNC:
    {
        rz_object_t *v1 = rz_unbox_ptr(rz_object_get_field(later, 0));
        rz_object_t *v2 = rz_unbox_ptr(rz_object_get_field(later, 1));
        bool ticked1 = rz_ticked(v1, chan, v);
        bool ticked2 = rz_ticked(v2, chan, v);
        if (ticked1 && ticked2)
        {
            rz_box_t u1 = rz_advance(v1, chan, v);
            rz_box_t u2 = rz_advance(v2, chan, v);
            rz_object_t *both = rz_ctor_var(RZ_TAG_SYNC_BOTH, 2, u1, u2);
            return rz_make_ptr(both);
        }
        else if (ticked1)
        {
            rz_box_t u1 = rz_advance(v1, chan, v);
            rz_object_t *left = rz_ctor_var(RZ_TAG_SYNC_LEFT, 1, u1);
            return rz_make_ptr(left);
        }
        else /* if (ticked2) */
        {
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
