#pragma once

#include <stdbool.h>
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

#ifdef _WIN32
typedef struct {
    char buffer[4096];
    size_t length;
    bool truncated;
} rz_console_line_state_t;

static rz_console_line_state_t rz_console_line_state = {{0}, 0, false};

static inline void rz_console_echo_bytes(const char* text, DWORD length) {
    HANDLE stdout_handle = GetStdHandle(STD_OUTPUT_HANDLE);
    DWORD written = 0;
    DWORD console_mode = 0;

    if (stdout_handle == INVALID_HANDLE_VALUE || stdout_handle == NULL || length == 0) {
        return;
    }
    if (GetConsoleMode(stdout_handle, &console_mode)) {
        WriteConsoleA(stdout_handle, text, length, &written, NULL);
    } else {
        WriteFile(stdout_handle, text, length, &written, NULL);
    }
}

static inline void rz_console_echo_char(char c) {
    rz_console_echo_bytes(&c, 1);
}

static inline void rz_console_line_reset(void) {
    rz_console_line_state.length = 0;
    rz_console_line_state.truncated = false;
}

static inline void rz_console_line_push_char(char c) {
    if (rz_console_line_state.length + 1 < sizeof(rz_console_line_state.buffer)) {
        rz_console_line_state.buffer[rz_console_line_state.length++] = c;
        rz_console_line_state.buffer[rz_console_line_state.length] = '\0';
    } else {
        rz_console_line_state.truncated = true;
    }
}

static inline rz_os_result_t rz_console_line_finish(char* buffer, size_t size) {
    size_t copy_len = 0;
    if (size > 0) {
        copy_len = rz_console_line_state.length;
        if (copy_len >= size) {
            copy_len = size - 1;
        }
        memcpy(buffer, rz_console_line_state.buffer, copy_len);
        buffer[copy_len] = '\0';
    }

    rz_os_result_t result =
        (rz_console_line_state.truncated || rz_console_line_state.length >= size)
        ? RZ_INPUT_TOO_LONG
        : RZ_OK;
    rz_console_line_reset();
    return result;
}

static inline bool rz_console_handle_key_event(const KEY_EVENT_RECORD* key_event, char* buffer, size_t size, rz_os_result_t* result_out) {
    char c;
    if (!key_event->bKeyDown) {
        return false;
    }

    c = key_event->uChar.AsciiChar;
    if (c == '\0') {
        return false;
    }
    if (c == '\r' || c == '\n') {
        rz_console_echo_bytes("\r\n", 2);
        *result_out = rz_console_line_finish(buffer, size);
        return true;
    }
    if (c == '\b') {
        if (rz_console_line_state.length > 0) {
            rz_console_line_state.length--;
            rz_console_line_state.buffer[rz_console_line_state.length] = '\0';
            rz_console_echo_bytes("\b \b", 3);
        }
        return false;
    }
    if ((unsigned char)c >= ' ' || c == '\t') {
        rz_console_line_push_char(c);
        rz_console_echo_char(c);
    }
    return false;
}

static rz_os_result_t rz_readline_timeout_console(char* buffer, size_t size, uint32_t timeout_ms) {
    HANDLE stdin_handle = GetStdHandle(STD_INPUT_HANDLE);
    ULONGLONG deadline = 0;

    if (timeout_ms != UINT32_MAX) {
        deadline = GetTickCount64() + (ULONGLONG)timeout_ms;
    }

    while (true) {
        DWORD wait_ms;
        DWORD wait_result;

        if (timeout_ms == UINT32_MAX) {
            wait_ms = INFINITE;
        } else {
            ULONGLONG now = GetTickCount64();
            if (now >= deadline) {
                return RZ_TIMEOUT;
            }
            wait_ms = (DWORD)(deadline - now);
        }

        wait_result = WaitForSingleObject(stdin_handle, wait_ms);
        if (wait_result == WAIT_TIMEOUT) {
            return RZ_TIMEOUT;
        }
        if (wait_result != WAIT_OBJECT_0) {
            return RZ_NO_INPUT;
        }

        while (true) {
            DWORD available = 0;
            INPUT_RECORD record;
            DWORD records_read = 0;
            rz_os_result_t result;

            if (!GetNumberOfConsoleInputEvents(stdin_handle, &available)) {
                return RZ_NO_INPUT;
            }
            if (available == 0) {
                break;
            }
            if (!ReadConsoleInputA(stdin_handle, &record, 1, &records_read) || records_read == 0) {
                return RZ_NO_INPUT;
            }
            if (record.EventType != KEY_EVENT) {
                continue;
            }
            if (rz_console_handle_key_event(&record.Event.KeyEvent, buffer, size, &result)) {
                return result;
            }
        }
    }
}
#endif

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
        return rz_readline_timeout_console(buffer, size, timeout_ms);
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
