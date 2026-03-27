#pragma once

#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/select.h>
#include <time.h>
#include <unistd.h>
#endif

typedef enum {
    RZ_OK = 0,
    RZ_NO_INPUT = 1,
    RZ_INPUT_TOO_LONG = 2,
    RZ_TIMEOUT = 3,
} rz_os_result_t;

/* https://stackoverflow.com/a/4023921 */
static rz_os_result_t rz_readline(char* buffer, size_t size) {
    if (fgets(buffer, size, stdin) == NULL) {
        return RZ_NO_INPUT;
    }

    size_t len = strlen(buffer);
    bool had_line_ending = false;
    if(len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
        len--;
        had_line_ending = true;
    }
    if (len > 0 && buffer[len - 1] == '\r') {
        buffer[len - 1] = '\0';
        had_line_ending = true;
    }
    if (had_line_ending) {
        return RZ_OK;
    }

    /* empty the line */
    int ch;
    while ((ch = getchar()) != '\n' && (ch != EOF));
    return RZ_INPUT_TOO_LONG;
}

static inline void rz_sleep_ms(uint32_t duration_ms) {
#ifdef _WIN32
    Sleep(duration_ms);
#else
    struct timespec req = {
        .tv_sec = duration_ms / 1000,
        .tv_nsec = (long)(duration_ms % 1000) * 1000000L
    };
    nanosleep(&req, NULL);
#endif
}

static rz_os_result_t rz_readline_timeout(char* buffer, size_t size, uint32_t timeout_ms) {
#ifdef _WIN32
    HANDLE stdin_handle = GetStdHandle(STD_INPUT_HANDLE);
    DWORD console_mode = 0;
    if (stdin_handle == INVALID_HANDLE_VALUE || stdin_handle == NULL) {
        return RZ_NO_INPUT;
    }

    if (GetConsoleMode(stdin_handle, &console_mode)) {
        DWORD wait_result = WaitForSingleObject(stdin_handle, timeout_ms == UINT32_MAX ? INFINITE : timeout_ms);
        if (wait_result == WAIT_TIMEOUT) {
            return RZ_TIMEOUT;
        }
        if (wait_result != WAIT_OBJECT_0) {
            return RZ_NO_INPUT;
        }
        return rz_readline(buffer, size);
    }

    ULONGLONG deadline = GetTickCount64() + (ULONGLONG)timeout_ms;
    while (true) {
        DWORD bytes_available = 0;
        if (!PeekNamedPipe(stdin_handle, NULL, 0, NULL, &bytes_available, NULL)) {
            DWORD last_error = GetLastError();
            if (last_error == ERROR_BROKEN_PIPE || last_error == ERROR_HANDLE_EOF) {
                return RZ_NO_INPUT;
            }
            return RZ_NO_INPUT;
        }
        if (bytes_available > 0) {
            return rz_readline(buffer, size);
        }
        if (timeout_ms == UINT32_MAX) {
            rz_sleep_ms(10);
            continue;
        }
        ULONGLONG now = GetTickCount64();
        if (now >= deadline) {
            return RZ_TIMEOUT;
        }
        {
            DWORD remaining_ms = (DWORD)(deadline - now);
            rz_sleep_ms(remaining_ms < 10 ? remaining_ms : 10);
        }
    }
#else
    int stdin_fd = fileno(stdin);
    fd_set read_fds;
    int ready;
    FD_ZERO(&read_fds);
    FD_SET(stdin_fd, &read_fds);
    if (timeout_ms == UINT32_MAX) {
        ready = select(stdin_fd + 1, &read_fds, NULL, NULL, NULL);
    } else {
        struct timeval timeout = {
            .tv_sec = (time_t)(timeout_ms / 1000),
            .tv_usec = (suseconds_t)((timeout_ms % 1000) * 1000)
        };
        ready = select(stdin_fd + 1, &read_fds, NULL, NULL, &timeout);
    }
    if (ready == 0) {
        return RZ_TIMEOUT;
    }
    if (ready < 0) {
        return RZ_NO_INPUT;
    }
    return rz_readline(buffer, size);
#endif
}
