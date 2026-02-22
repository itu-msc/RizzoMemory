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

static inline void rz_debug_print_delayed(rz_box_t delay);

static inline void rz_debug_print_later(rz_box_t later) {
    switch (rz_object_tag(rz_unbox_ptr(later))) {
        case RZ_TAG_LATER_NEVER: { printf("never"); } break;
        case RZ_TAG_LATER_WAIT: { 
            printf("wait(%d)", rz_unbox_int(rz_object_get_field(rz_unbox_ptr(later), 0))); 
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
