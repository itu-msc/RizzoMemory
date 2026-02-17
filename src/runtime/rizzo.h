#pragma once

#include "core.h"
#include "heap.h"
#include "later.h"
#include "channel.h"
#include "os.h"
#include "stdlib.h"

static void rz_start_event_loop() {
    char buffer[256];
    while (true) {
        rz_os_result_t status = rz_readline(buffer, sizeof(buffer));
        if (status == RZ_OK) {
            char* p;
            long converted = strtol(buffer, &p, 10);
            if (!*p) {
                rz_step(RZ_CHANNEL_CONSOLE_IN, rz_make_int((int32_t)converted));
                printf("\n AFTER STEP \n");
                rz_debug_print_heap();
            } else {
                printf("TODO: only accept ints\n");
            }
        }
        else if (status == RZ_INPUT_TOO_LONG) {
            printf("Input too long, try again.\n");
        }
    }
}