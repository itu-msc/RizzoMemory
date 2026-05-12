#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <math.h>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <time.h>
#endif

#include "allocation.h"
#include "channel.h"
#include "core.h"

typedef struct rz_timer
{
    rz_channel_t channel;
    double interval_seconds;
    double start_seconds;
    double next_fire_seconds;
    struct rz_timer *next;
} rz_timer_t;

static rz_timer_t *rz_timer_list = NULL;

static inline double rz_timer_now_seconds(void)
{
#ifdef _WIN32
    LARGE_INTEGER frequency;
    LARGE_INTEGER counter;
    QueryPerformanceFrequency(&frequency);
    QueryPerformanceCounter(&counter);
    return (double)counter.QuadPart / (double)frequency.QuadPart;
#else
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (double)ts.tv_sec + ((double)ts.tv_nsec / 1000000000.0);
#endif
}

static inline void rz_timer_reset(void)
{
    rz_timer_t *timer = rz_timer_list;
    while (timer)
    {
        rz_timer_t *next = timer->next;
        rz_free(timer);
        timer = next;
    }
    rz_timer_list = NULL;
}

static inline rz_channel_t rz_timer_register(int64_t interval_ms)
{
    rz_timer_t *timer = (rz_timer_t *)rz_malloc(sizeof(rz_timer_t));
    double now_seconds = rz_timer_now_seconds();
    timer->channel = rz_channel_alloc();
    timer->interval_seconds = (double)interval_ms / 1000.0;
    timer->start_seconds = now_seconds;
    timer->next_fire_seconds = now_seconds + timer->interval_seconds;
    timer->next = rz_timer_list;
    rz_timer_list = timer;
    return timer->channel;
}

static inline bool rz_timer_has_registered_channels(void)
{
    return rz_timer_list != NULL;
}

static inline bool rz_timer_next_timeout_ms(double now_seconds, uint32_t *timeout_ms_out)
{
    rz_timer_t *timer = rz_timer_list;
    double min_remaining_seconds;
    bool have_timer = false;
    while (timer)
    {
        double remaining_seconds = timer->next_fire_seconds - now_seconds;
        if (!have_timer || remaining_seconds < min_remaining_seconds)
        {
            min_remaining_seconds = remaining_seconds;
            have_timer = true;
        }
        timer = timer->next;
    }
    if (!have_timer)
    {
        return false;
    }
    if (min_remaining_seconds <= 0.0)
    {
        *timeout_ms_out = 0;
        return true;
    }
    else
    {
        double timeout_ms = min_remaining_seconds * 1000.0;
        *timeout_ms_out = timeout_ms >= (double)UINT32_MAX
            ? UINT32_MAX
            : (uint32_t)timeout_ms;
        return true;
    }
}

static inline bool rz_timer_take_due(double now_seconds, rz_channel_t *channel_out, rz_box_t *value_out)
{
    rz_timer_t *timer = rz_timer_list;
    while (timer)
    {
        if (timer->next_fire_seconds <= now_seconds)
        {
            // added 0.5 to acount for double -> int rounding
            int64_t elapsed_ms = llround((timer->next_fire_seconds - timer->start_seconds) * 1000.0);
            *channel_out = timer->channel;
            *value_out = rz_make_int(elapsed_ms);
            timer->next_fire_seconds += timer->interval_seconds;
            return true;
        }
        timer = timer->next;
    }
    return false;
}
