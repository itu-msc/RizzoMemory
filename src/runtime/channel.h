#pragma once

#include "stdint.h"
#include "core.h"

typedef int64_t rz_channel_t;

static const rz_channel_t RZ_CHANNEL_CONSOLE_IN = 0;
static const rz_channel_t RZ_CHANNEL_TIMER_BASE = 1;

static inline rz_box_t rz_make_channel(rz_channel_t chan) {
    return rz_make_int(chan);
}

static inline rz_box_t rz_channel_console_get() {
    return rz_make_channel(RZ_CHANNEL_CONSOLE_IN);
}
