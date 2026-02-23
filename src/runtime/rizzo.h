#pragma once

//use the -D flag to enable debug info, printing at runtime `gcc ... -D__RZ_DEBUG_INFO`
//#define __RZ_DEBUG_INFO 

#include "core.h"
#include "heap.h"
#include "later.h"
#include "channel.h"
#include "os.h"
#include "stdlib.h"

typedef struct rz_signal_list {
    size_t count, capacity;
    rz_signal_t* signals[];
} rz_signal_list_t;
static rz_signal_list_t* rz_signal_list_create();
static void rz_signal_list_add(rz_signal_list_t* list, rz_object_t* signal);
static void rz_print_registered_outputs();
static void rz_print_registered_output_head(rz_signal_t* sig, bool force);

rz_signal_list_t* rz_global_output_signals = NULL;

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
    char buffer[256];
    while (true) {
        rz_os_result_t status = rz_readline(buffer, sizeof(buffer));
        if (status == RZ_OK) {
            char* p;
            long converted = strtol(buffer, &p, 10);
            if(p == buffer) continue; 
            if (*p != '\0' || converted > INT32_MAX || converted < INT32_MIN) {
                printf("Invalid input, only 32-bit integers\n");
                continue;
            }
            rz_step(RZ_CHANNEL_CONSOLE_IN, rz_make_int((int32_t)converted));
#ifdef __RZ_DEBUG_INFO
            printf("AFTER STEP\n");
            rz_debug_print_heap();
#endif
        }
        else if (status == RZ_INPUT_TOO_LONG) {
            printf("Input too long, try again.\n");
        }
    }
    return rz_make_int(0);
}

/* registers a boxed signal for output */
static inline rz_box_t rz_register_output_signal(size_t num_args, rz_box_t* args) {
    (void) num_args;
    rz_box_t sig = args[0];
    if(sig.kind != RZ_BOX_SIGNAL) {
        fprintf(stderr, "Runtime error: rz_register_output_signal got a non-signal value\n");
        exit(1);
    }
    /* we've just read a signal, which has a head value in the current time tick - output that */
    rz_print_registered_output_head((rz_signal_t*)rz_unbox_ptr(sig), true);
    rz_signal_list_add(rz_global_output_signals, rz_unbox_ptr(sig));
    return rz_make_int(0); /* return unit */
}

/*  |------------------------------|
    |         OUTPUT HELPERS       |
    |------------------------------| */

static inline void rz_print_registered_output_head(rz_signal_t* sig, bool force) {
    /*TODO: this assume strings for now - we will want to make it strings */
    if (rz_unbox_int(sig->updated) || force) { 
        // printf("%d\n", sig->head.as.i32);
        rz_debug_print_box(sig->head); 
        printf("\n");
    }
}

static inline void rz_print_registered_outputs() {
    for (size_t i = 0; i < rz_global_output_signals->count; i++) {
        rz_print_registered_output_head(rz_global_output_signals->signals[i], false);
    }
}
    
static rz_signal_list_t* rz_signal_list_create() {
    size_t initial_capacity = 10;
    rz_signal_list_t* list = malloc(sizeof(rz_signal_list_t) + initial_capacity * sizeof(rz_signal_t*));
    list->count = 0;
    list->capacity = initial_capacity;
    return list;
}

static void rz_signal_list_add(rz_signal_list_t* list, rz_object_t* signal) {
    if (list->count == list->capacity) {
        size_t new_capacity = list->capacity * 2;
        list = realloc(list, sizeof(rz_signal_list_t) + new_capacity * sizeof(rz_signal_t*));
        list->capacity = new_capacity;
    }
    list->signals[list->count++] = (rz_signal_t*)signal;
}
