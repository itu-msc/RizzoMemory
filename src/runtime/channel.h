#pragma once

#include "stdint.h"
#include "core.h"

typedef int64_t rz_channel_t;

#define RZ_CHANNEL_CONSOLE_IN ((rz_channel_t)0)
#define RZ_CHANNEL_KEYBOARD_IN ((rz_channel_t)1)
#define RZ_CHANNEL_DYNAMIC_BASE ((rz_channel_t)2)

static rz_channel_t rz_next_dynamic_channel = RZ_CHANNEL_DYNAMIC_BASE;

static inline void rz_channel_reset_dynamic(void) {
    rz_next_dynamic_channel = RZ_CHANNEL_DYNAMIC_BASE;
}

static inline rz_channel_t rz_channel_alloc(void) {
    return rz_next_dynamic_channel++;
}

static inline rz_box_t rz_make_channel(rz_channel_t chan) {
    return rz_make_int(chan);
}

static inline rz_box_t rz_channel_console_get() {
    return rz_make_channel(RZ_CHANNEL_CONSOLE_IN);
}

static inline rz_box_t rz_channel_keyboard_get() {
    return rz_make_channel(RZ_CHANNEL_KEYBOARD_IN);
}
