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

static rz_object_t RZ_NEVER_OBJ = { .header = { .num_fields = 0, .tag = RZ_TAG_LATER_NEVER, .refcount = -1 } };
static rz_box_t RZ_NEVER = { .kind = RZ_BOX_INT, .as.obj = &RZ_NEVER_OBJ }; /* should it have kind int? */

static inline void rz_debug_print_later(rz_box_t later) {
    switch (rz_object_tag(rz_unbox_ptr(later))) {
        case RZ_TAG_LATER_NEVER: printf("NEVER"); break;
        case RZ_TAG_LATER_WAIT: printf("WAIT"); break;
        case RZ_TAG_LATER_TAIL: printf("TAIL"); break;
        case RZ_TAG_LATER_SYNC: printf("SYNC"); break;
        case RZ_TAG_LATER_WATCH: printf("WATCH"); break;
        case RZ_TAG_LATER_APP: printf("APP"); break;
        default: printf("Unknown later tag: %d", rz_object_tag(rz_unbox_ptr(later))); break;
    }
}

