#pragma once

// use the -D flag to enable debug info, printing at runtime `gcc ... -D__RZ_DEBUG_INFO`
// #define __RZ_DEBUG_INFO

#ifndef __RZ_INPUT_BUFFER_SIZE
#define __RZ_INPUT_BUFFER_SIZE 512
#endif

#include "core.h"
#include "heap.h"
#include "later.h"
#include "channel.h"
#include "builtins.h"
#include "os.h"
#include "stdlib.h"

/* initializes the Rizzo runtime. */
static void rz_init_rizzo() {
    rz_global_output_signals = rz_signal_list_create();
}

/** Steps the Rizzo program one tick forward.
   Additionally, outputs the head of any registered outputs that was updated during heap update. */
static inline void rz_step(rz_channel_t chan, rz_box_t v) {
    rz_heap_update(chan, v);
    rz_print_registered_outputs();
}

/* Starts the Rizzo event loop:
   - Listens to input on channels (currently only console input)
   - Then produces a time step by calling `rz_step` (which updates the heap) */
static rz_box_t rz_start_event_loop() {
    char buffer[__RZ_INPUT_BUFFER_SIZE];
    while (true) {
        rz_os_result_t status = rz_readline(buffer, sizeof(buffer));
        if (status == RZ_OK) {
            rz_step(RZ_CHANNEL_CONSOLE_IN, rz_make_string_len(buffer, strlen(buffer)));
#ifdef __RZ_DEBUG_INFO
            printf("AFTER STEP\n");
            rz_debug_print_heap();
#endif
        }
        else if (status == RZ_INPUT_TOO_LONG)
        {
            printf("Input too long, try again.\n");
        }
    }
    return rz_make_int(0);
}
