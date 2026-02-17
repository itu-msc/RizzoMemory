#pragma once

#include "stdint.h"
#include "core.h"

typedef int32_t rz_channel_t;

#define RZ_CHANNEL_CONSOLE_IN 0


static inline rz_box_t rz_make_channel(rz_channel_t chan) {
    return rz_make_int(chan);
}

static inline rz_box_t rz_channel_console_get() {
    return rz_make_channel(RZ_CHANNEL_CONSOLE_IN);
}

