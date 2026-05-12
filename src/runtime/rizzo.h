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
#include "tcp.h"
#include "timer.h"

static bool rz_should_quit = false;

#include "builtins.h"
#include "os.h"
#include "stdlib.h"

typedef struct rz_signal_list
{
    size_t count, capacity;
    rz_signal_t *signals[];
} rz_signal_list_t;

typedef struct rz_port_output
{
    rz_signal_t *signal;
    rz_socket_t socket;
} rz_port_output_t;

typedef struct rz_port_output_list
{
    size_t count, capacity;
    rz_port_output_t outputs[];
} rz_port_output_list_t;

static rz_signal_list_t *rz_signal_list_create();
static void rz_signal_list_add(rz_signal_list_t **list, rz_object_t *signal);
static rz_port_output_list_t *rz_port_output_list_create();
static void rz_port_output_list_add(rz_port_output_list_t **list, rz_object_t *signal, rz_socket_t socket);
static void rz_print_registered_outputs();
static void rz_print_registered_output_head(rz_signal_t *sig, bool force);
static void rz_send_registered_port_output_head(rz_port_output_t *output, bool force);
static void rz_drain_keyboard_events();

rz_signal_list_t *rz_global_output_signals = NULL;
rz_port_output_list_t *rz_global_port_output_signals = NULL;

/* initializes the Rizzo runtime. */
static void rz_init_rizzo()
{
    rz_channel_reset_dynamic();
    rz_heap_init();
    rz_global_output_signals = rz_signal_list_create();
    rz_global_port_output_signals = rz_port_output_list_create();
    rz_should_quit = false;
    rz_timer_reset();
    rz_tcp_reset();
    rz_keyboard_queue_reset();
    rz_random_state = 0;
}

#if defined(__RZ_DEBUG_INFO) || defined(__RZ_HEAP_INFO)
static uint64_t rz_debug_heap_step_count = 0;
#endif

#ifdef __RZ_HEAP_INFO
static uint64_t rz_heap_info_average_time = 0;
#endif

/** Steps the Rizzo program one tick forward.
   Additionally, outputs the head of any registered outputs that was updated during heap update. */
static inline void rz_step(rz_channel_t chan, rz_box_t v)
{
#ifdef __RZ_HEAP_INFO
    double start_seconds = rz_timer_now_seconds();
#endif

    rz_heap_update(chan, v);
    rz_refcount_dec_box(v);
    rz_print_registered_outputs();

#ifdef __RZ_HEAP_INFO
    double end_seconds = rz_timer_now_seconds();
    uint64_t delta_us = (uint64_t)((end_seconds - start_seconds) * 1000000.0);
    rz_heap_info_average_time = (rz_debug_heap_step_count * rz_heap_info_average_time + delta_us) / (rz_debug_heap_step_count + 1);
#endif

#if defined(__RZ_DEBUG_INFO) || defined(__RZ_HEAP_INFO)
    rz_debug_heap_step_count++;
#endif

#ifdef __RZ_DEBUG_INFO
    printf("step %.4" PRIu64 ", channel %" PRIu64 ", Sig index %" PRIu64 ", ", rz_debug_heap_step_count, chan, rz_debug_signal_next_index);
    rz_debug_print_heap();
#endif
}

static void rz_drain_keyboard_events()
{
    char key_buffer[RZ_KEYBOARD_EVENT_SIZE];
    while (!rz_should_quit && rz_keyboard_take_event(key_buffer, sizeof(key_buffer)))
    {
        rz_step(RZ_CHANNEL_KEYBOARD_IN, rz_make_string_len(key_buffer, strlen(key_buffer)));
    }
}

/* Starts the Rizzo event loop:
   - Listens to input on channels (currently only console input)
   - Then produces a time step by calling `rz_step` (which updates the heap) */
static rz_box_t rz_start_event_loop()
{
    char buffer[__RZ_INPUT_BUFFER_SIZE];
    rz_channel_t timer_channel;
    rz_box_t timer_value;
    rz_channel_t tcp_channel;
    rz_box_t tcp_value;

#ifdef __RZ_DEBUG_INFO
    printf("After initialization, Sig index %" PRIu64 ", ", rz_debug_signal_next_index);
    rz_debug_print_heap();
#endif

    while (!rz_should_quit)
    {
        rz_drain_keyboard_events();
        double now = rz_timer_now_seconds();
        uint32_t timeout_ms = UINT32_MAX;
        bool has_timers = rz_timer_next_timeout_ms(now, &timeout_ms);
        bool has_tcp_inputs = rz_tcp_has_registered_inputs();
        if (has_tcp_inputs && timeout_ms > 10)
        {
            timeout_ms = 10;
        }
        rz_os_result_t status = has_timers
                                    ? rz_readline_timeout(buffer, sizeof(buffer), timeout_ms)
                                    : (has_tcp_inputs
                                        ? rz_readline_timeout(buffer, sizeof(buffer), timeout_ms)
                                        : rz_readline(buffer, sizeof(buffer)));
        now = rz_timer_now_seconds();
        while (rz_timer_take_due(now, &timer_channel, &timer_value))
        {
            rz_step(timer_channel, timer_value);
        }
        while (rz_tcp_take_input(&tcp_channel, &tcp_value))
        {
            rz_step(tcp_channel, tcp_value);
        }
        rz_drain_keyboard_events();
        if (status == RZ_OK)
        {
            rz_step(RZ_CHANNEL_CONSOLE_IN, rz_make_string_len(buffer, strlen(buffer)));
        }
        else if (status == RZ_NO_INPUT)
        {
            if (!rz_timer_has_registered_channels() && !rz_tcp_has_registered_inputs())
            {
                break;
            }
            if (!rz_should_quit && timeout_ms > 0 && timeout_ms != UINT32_MAX)
            {
                rz_sleep_ms(timeout_ms);
            }
        }
        else if (status == RZ_INPUT_TOO_LONG)
        {
            printf("Input too long, try again.\n");
        }
        else if (status == RZ_TIMEOUT)
        {
        }
    }

#ifdef __RZ_HEAP_INFO
    printf("Steps taken: %" PRIu64 ", Average step time: %" PRIu64 " us, Signals left in heap: %" PRIu64 "\n", rz_debug_heap_step_count, rz_heap_info_average_time, rz_heap_size);
#endif
    return rz_make_int(0);
}

/* registers a boxed signal for output */
static inline rz_box_t rz_register_output_signal(size_t num_args, rz_box_t *args)
{
    (void)num_args;
    rz_box_t sig = args[0];
    if (sig.kind != RZ_BOX_PTR || rz_object_get_type(rz_unbox_ptr(sig)) != RZ_SIGNAL)
    {
        rz_debug_print_box(sig);
        fprintf(stderr, "Runtime error: rz_register_output_signal got a non-signal value (%d)\n", sig.kind);
        exit(1);
    }
    /* we've just read a signal, which has a head value in the current time tick - output that */
    rz_print_registered_output_head((rz_signal_t *)rz_unbox_ptr(sig), true);
    rz_signal_list_add(&rz_global_output_signals, rz_unbox_ptr(sig));
    return RZ_UNIT;
}

static inline rz_box_t rz_register_port_output_signal(size_t num_args, rz_box_t *args)
{
    (void)num_args;
    rz_box_t port = args[0];
    rz_box_t sig = args[1];
    if (sig.kind != RZ_BOX_PTR || rz_object_get_type(rz_unbox_ptr(sig)) != RZ_SIGNAL)
    {
        rz_debug_print_box(sig);
        fprintf(stderr, "Runtime error: rz_register_port_output_signal got a non-signal value (%d)\n", sig.kind);
        exit(1);
    }
    rz_socket_t socket = rz_tcp_connect_localhost(rz_unbox_int(port));
    rz_port_output_t output = {
        .signal = (rz_signal_t *)rz_unbox_ptr(sig),
        .socket = socket,
    };
    rz_send_registered_port_output_head(&output, true);
    rz_port_output_list_add(&rz_global_port_output_signals, rz_unbox_ptr(sig), socket);
    return RZ_UNIT;
}

/* Registers a signal after heap update has already started for the current tick.
   Signals that already updated this tick are printed by the normal output pass. */
static inline rz_box_t rz_register_output_signal_deferred(size_t num_args, rz_box_t *args)
{
    (void)num_args;
    rz_box_t sig = args[0];
    if (sig.kind != RZ_BOX_PTR || rz_object_get_type(rz_unbox_ptr(sig)) != RZ_SIGNAL)
    {
        rz_debug_print_box(sig);
        fprintf(stderr, "Runtime error: rz_register_output_signal_deferred got a non-signal value (%d)\n", sig.kind);
        exit(1);
    }

    rz_signal_t *signal = (rz_signal_t *)rz_unbox_ptr(sig);
    if (!rz_unbox_int(signal->updated))
    {
        rz_print_registered_output_head(signal, true);
    }
    rz_signal_list_add(&rz_global_output_signals, (rz_object_t *)signal);
    return RZ_UNIT;
}

/*  |------------------------------|
    |         OUTPUT HELPERS       |
    |------------------------------| */

static inline void rz_print_registered_output_head(rz_signal_t *sig, bool force)
{
    if (rz_unbox_int(sig->updated) || force)
    {
        rz_debug_print_box(sig->head);
        printf("\n");
        fflush(stdout);
    }
}

static inline void rz_print_registered_outputs()
{
    for (size_t i = 0; i < rz_global_output_signals->count; i++)
    {
        rz_print_registered_output_head(rz_global_output_signals->signals[i], false);
    }
    for (size_t i = 0; i < rz_global_port_output_signals->count; i++)
    {
        rz_send_registered_port_output_head(&rz_global_port_output_signals->outputs[i], false);
    }
}

static inline void rz_send_registered_port_output_head(rz_port_output_t *output, bool force)
{
    if (rz_unbox_int(output->signal->updated) || force)
    {
        if (!rz_box_is_string(output->signal->head))
        {
            fprintf(stderr, "Runtime error: port_out_signal expected Signal String\n");
            exit(1);
        }
        rz_tcp_send_line(output->socket, output->signal->head);
    }
}

static rz_signal_list_t *rz_signal_list_create()
{
    size_t initial_capacity = 10;
    rz_signal_list_t *list = (rz_signal_list_t *)malloc(sizeof(rz_signal_list_t) + initial_capacity * sizeof(rz_signal_t *));
    list->count = 0;
    list->capacity = initial_capacity;
    return list;
}

static void rz_signal_list_add(rz_signal_list_t **list_ref, rz_object_t *signal)
{
    rz_signal_list_t *list = *list_ref;
    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity * 2;
        list = (rz_signal_list_t *)realloc(list, sizeof(rz_signal_list_t) + new_capacity * sizeof(rz_signal_t *));
        list->capacity = new_capacity;
        *list_ref = list;
    }
    list->signals[list->count++] = (rz_signal_t *)signal;
}

static rz_port_output_list_t *rz_port_output_list_create()
{
    size_t initial_capacity = 10;
    rz_port_output_list_t *list = (rz_port_output_list_t *)malloc(sizeof(rz_port_output_list_t) + initial_capacity * sizeof(rz_port_output_t));
    list->count = 0;
    list->capacity = initial_capacity;
    return list;
}

static void rz_port_output_list_add(rz_port_output_list_t **list_ref, rz_object_t *signal, rz_socket_t socket)
{
    rz_port_output_list_t *list = *list_ref;
    if (list->count == list->capacity)
    {
        size_t new_capacity = list->capacity * 2;
        list = (rz_port_output_list_t *)realloc(list, sizeof(rz_port_output_list_t) + new_capacity * sizeof(rz_port_output_t));
        list->capacity = new_capacity;
        *list_ref = list;
    }
    list->outputs[list->count].signal = (rz_signal_t *)signal;
    list->outputs[list->count].socket = socket;
    list->count++;
}
