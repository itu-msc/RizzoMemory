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
