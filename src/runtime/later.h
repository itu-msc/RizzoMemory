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
    rz_object_t *obj = rz_unbox_ptr(later);
    if (!obj) {
        printf("later(null)");
        return;
    }
    if (rz_debug_print_object_is_active(obj)) {
        printf("<cycle@%p>", (void *)obj);
        return;
    }
    if (!rz_debug_print_object_push(obj)) {
        printf("<depth-limit@%p>", (void *)obj);
        return;
    }

    switch (rz_object_tag(obj)) {
        case RZ_TAG_LATER_NEVER: { printf("never"); } break;
        case RZ_TAG_LATER_WAIT: { 
            printf("wait(%"PRId64")", rz_unbox_int(rz_object_get_field(obj, 0))); 
        } break;
        case RZ_TAG_LATER_TAIL: { 
            printf("tail(%p)", rz_unbox_ptr(rz_object_get_field(obj, 0))); 
        } break;
        case RZ_TAG_LATER_SYNC: { 
            printf("sync ("); 
            rz_debug_print_later(rz_object_get_field(obj, 0));
            printf(", ");
            rz_debug_print_later(rz_object_get_field(obj, 1));
            printf(")"); 
        } break;
        case RZ_TAG_LATER_WATCH: {
            printf("watch(%p)", rz_unbox_ptr(rz_object_get_field(obj, 0))); 
        } break;
        case RZ_TAG_LATER_APP: {
            printf("laterapp("); 
            rz_debug_print_delayed(rz_object_get_field(obj, 0));
            printf(", ");
            rz_debug_print_later(rz_object_get_field(obj, 1));
            printf(")");
        } break;
        default: printf("Unknown later tag: %d", rz_object_tag(obj)); break;
    }

    rz_debug_print_object_pop();
}

static inline void rz_debug_print_delayed(rz_box_t delay) {
    rz_object_t *obj = rz_unbox_ptr(delay);
    if (!obj) {
        printf("delay(null)");
        return;
    }
    if (rz_debug_print_object_is_active(obj)) {
        printf("<cycle@%p>", (void *)obj);
        return;
    }
    if (!rz_debug_print_object_push(obj)) {
        printf("<depth-limit@%p>", (void *)obj);
        return;
    }

    switch (rz_object_tag(obj)) {
        case RZ_TAG_DELAY: { 
            printf("delay("); rz_debug_print_box(rz_object_get_field(obj, 0)); printf(")"); 
        } break;
        case RZ_TAG_OSTAR: { 
            printf("delayedapp(");
            rz_debug_print_delayed(rz_object_get_field(obj, 0));
            printf(", ");
            rz_debug_print_delayed(rz_object_get_field(obj, 1));
            printf(")"); 
        } break;
        default: printf("Unknown delay tag: %d", rz_object_tag(obj)); break;
    }

    rz_debug_print_object_pop();
}
