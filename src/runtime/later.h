#pragma once

#include "core.h"

typedef enum {
    RZ_TAG_LATER_NEVER = 0,
    RZ_TAG_LATER_WAIT = 1,
    RZ_TAG_LATER_TAIL = 2,
    RZ_TAG_LATER_SYNC = 4,
    RZ_TAG_LATER_WATCH = 5,
    RZ_TAG_LATER_APP = 6,
} rz_later_tag_t;

typedef enum {
    RZ_TAG_SYNC_LEFT = 0,
    RZ_TAG_SYNC_RIGHT = 1,
    RZ_TAG_SYNC_BOTH = 2
} rz_sync_tag_t;

typedef enum {
    RZ_TAG_DELAY = 0,
    RZ_TAG_OSTAR = 1
} rz_delay_tag_t;

static rz_object_t RZ_NEVER_OBJ = { .header = { .num_fields = 0, .tag = RZ_TAG_LATER_NEVER, .refcount = -1 } };
static rz_box_t RZ_NEVER = { .kind = RZ_BOX_INT, .as.obj = &RZ_NEVER_OBJ }; /* should it have kind int? */

/** Forces a Delayed value (either 'delay' or 'ostar') - NO caching of the resulting values.
 * Returns a boxed value with refcount +1, so the caller doesn't need to inc.
 */
static inline rz_box_t rz_advance_delayed(rz_box_t delayed) {
    rz_object_t* ptr_delayed = rz_unbox_ptr(delayed);
    switch (rz_object_tag(ptr_delayed)) {
        case RZ_TAG_DELAY: {
            rz_object_t* thunk = rz_unbox_ptr(rz_object_get_field(ptr_delayed, 0));
            rz_refcount_inc(thunk);
            return rz_apply1(thunk, RZ_UNIT);
        }
        case RZ_TAG_OSTAR: {
            // typeof OSTAR: Delayed ('a -> 'b) -> Delayed 'a -> Delayed 'b
            rz_box_t operand_left =  rz_advance_delayed(rz_object_get_field(ptr_delayed, 0));
            rz_box_t operand_right = rz_advance_delayed(rz_object_get_field(ptr_delayed, 1));
            rz_object_t* f = rz_unbox_ptr(operand_left);
            return rz_apply1(f, operand_right);
        }
        default: {
            printf("Unknown delayed tag in 'rz_advance_delayed': %d", rz_object_tag(ptr_delayed));
            exit(1);
        }
    }
    
}

static inline void rz_debug_print_delayed(rz_box_t delay);

static inline void rz_debug_print_later(rz_box_t later) {
    switch (rz_object_tag(rz_unbox_ptr(later))) {
        case RZ_TAG_LATER_NEVER: { printf("never"); } break;
        case RZ_TAG_LATER_WAIT: { 
            printf("wait(%"PRId64")", rz_unbox_int(rz_object_get_field(rz_unbox_ptr(later), 0))); 
        } break;
        case RZ_TAG_LATER_TAIL: { 
            printf("tail(%p)", rz_unbox_ptr(rz_object_get_field(rz_unbox_ptr(later), 0))); 
        } break;
        case RZ_TAG_LATER_SYNC: { 
            printf("sync ("); 
            rz_debug_print_later(rz_object_get_field(rz_unbox_ptr(later), 0));
            printf(", ");
            rz_debug_print_later(rz_object_get_field(rz_unbox_ptr(later), 0));
            printf(")"); 
        } break;
        case RZ_TAG_LATER_WATCH: {
            printf("watch(%p)", rz_unbox_ptr(rz_object_get_field(rz_unbox_ptr(later), 0))); 
        } break;
        case RZ_TAG_LATER_APP: {
            printf("laterapp("); 
            rz_debug_print_delayed(rz_object_get_field(rz_unbox_ptr(later), 0));
            printf(", ");
            rz_debug_print_later(rz_object_get_field(rz_unbox_ptr(later), 1));
            printf(")");
        } break;
        default: printf("Unknown later tag: %d", rz_object_tag(rz_unbox_ptr(later))); break;
    }
}

static inline void rz_debug_print_delayed(rz_box_t delay) {
    switch (rz_object_tag(rz_unbox_ptr(delay))) {
        case RZ_TAG_DELAY: { 
            printf("delay("); rz_debug_print_box(rz_object_get_field(rz_unbox_ptr(delay), 0)); printf(")"); 
        } break;
        case RZ_TAG_OSTAR: { 
            printf("delayedapp(");
            rz_debug_print_delayed(rz_object_get_field(rz_unbox_ptr(delay), 0));
            printf(", ");
            rz_debug_print_delayed(rz_object_get_field(rz_unbox_ptr(delay), 1));
            printf(")"); 
        } break;
        default: printf("Unknown delay tag: %d", rz_object_tag(rz_unbox_ptr(delay))); break;
    }
}
