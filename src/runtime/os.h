#pragma once

#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#else
#include <errno.h>
#include <fcntl.h>
#include <sys/select.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#endif

typedef enum {
    RZ_OK = 0,
    RZ_NO_INPUT = 1,
    RZ_INPUT_TOO_LONG = 2,
    RZ_TIMEOUT = 3,
} rz_os_result_t;

#define RZ_KEYBOARD_QUEUE_CAPACITY 128
#define RZ_KEYBOARD_EVENT_SIZE 32

typedef struct {
    char events[RZ_KEYBOARD_QUEUE_CAPACITY][RZ_KEYBOARD_EVENT_SIZE];
    size_t head;
    size_t count;
} rz_keyboard_queue_t;

static rz_keyboard_queue_t rz_keyboard_queue = {{{0}}, 0, 0};

static inline void rz_keyboard_queue_reset(void) {
    rz_keyboard_queue.head = 0;
    rz_keyboard_queue.count = 0;
}

static inline void rz_keyboard_queue_push_name(const char* name) {
    size_t index;
    if (name == NULL || name[0] == '\0') {
        return;
    }
    if (rz_keyboard_queue.count == RZ_KEYBOARD_QUEUE_CAPACITY) {
        rz_keyboard_queue.head = (rz_keyboard_queue.head + 1) % RZ_KEYBOARD_QUEUE_CAPACITY;
        rz_keyboard_queue.count--;
    }
    index = (rz_keyboard_queue.head + rz_keyboard_queue.count) % RZ_KEYBOARD_QUEUE_CAPACITY;
    snprintf(rz_keyboard_queue.events[index], RZ_KEYBOARD_EVENT_SIZE, "%s", name);
    rz_keyboard_queue.count++;
}

static inline void rz_keyboard_queue_push_bytes(const char* bytes, size_t length) {
    char event[RZ_KEYBOARD_EVENT_SIZE];
    if (bytes == NULL || length == 0 || length >= RZ_KEYBOARD_EVENT_SIZE) {
        return;
    }
    memcpy(event, bytes, length);
    event[length] = '\0';
    rz_keyboard_queue_push_name(event);
}

static inline void rz_keyboard_queue_push_char(char c) {
    switch (c) {
        case '\r':
        case '\n':
            rz_keyboard_queue_push_name("Enter");
            break;
        case '\b':
        case 127:
            rz_keyboard_queue_push_name("Backspace");
            break;
        case '\t':
            rz_keyboard_queue_push_name("Tab");
            break;
        case 27:
            rz_keyboard_queue_push_name("Escape");
            break;
        default:
            if ((unsigned char)c >= ' ') {
                rz_keyboard_queue_push_bytes(&c, 1);
            }
            break;
    }
}

static inline void rz_keyboard_queue_push_input_bytes(const char* bytes, size_t length) {
    for (size_t i = 0; i < length; i++) {
        if (bytes[i] == '\r' && i + 1 < length && bytes[i + 1] == '\n') {
            rz_keyboard_queue_push_name("Enter");
            i++;
            continue;
        }
        if ((unsigned char)bytes[i] == 27 && i + 2 < length && bytes[i + 1] == '[') {
            switch (bytes[i + 2]) {
                case 'A':
                    rz_keyboard_queue_push_name("ArrowUp");
                    i += 2;
                    continue;
                case 'B':
                    rz_keyboard_queue_push_name("ArrowDown");
                    i += 2;
                    continue;
                case 'C':
                    rz_keyboard_queue_push_name("ArrowRight");
                    i += 2;
                    continue;
                case 'D':
                    rz_keyboard_queue_push_name("ArrowLeft");
                    i += 2;
                    continue;
                default:
                    break;
            }
        }
        rz_keyboard_queue_push_char(bytes[i]);
    }
}

static inline bool rz_keyboard_take_event(char* buffer, size_t size) {
    if (rz_keyboard_queue.count == 0 || size == 0) {
        return false;
    }
    snprintf(buffer, size, "%s", rz_keyboard_queue.events[rz_keyboard_queue.head]);
    rz_keyboard_queue.head = (rz_keyboard_queue.head + 1) % RZ_KEYBOARD_QUEUE_CAPACITY;
    rz_keyboard_queue.count--;
    return true;
}

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
        switch (key_event->wVirtualKeyCode) {
            case VK_UP:
                rz_keyboard_queue_push_name("ArrowUp");
                break;
            case VK_DOWN:
                rz_keyboard_queue_push_name("ArrowDown");
                break;
            case VK_LEFT:
                rz_keyboard_queue_push_name("ArrowLeft");
                break;
            case VK_RIGHT:
                rz_keyboard_queue_push_name("ArrowRight");
                break;
            case VK_ESCAPE:
                rz_keyboard_queue_push_name("Escape");
                break;
            default:
                break;
        }
        return false;
    }
    rz_keyboard_queue_push_char(c);
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
#else
typedef struct {
    char buffer[4096];
    size_t length;
    bool truncated;
} rz_posix_console_line_state_t;

static rz_posix_console_line_state_t rz_posix_console_line_state = {{0}, 0, false};
static struct termios rz_posix_original_termios;
static bool rz_posix_raw_console_enabled = false;

static inline void rz_posix_console_restore(void) {
    if (rz_posix_raw_console_enabled) {
        tcsetattr(STDIN_FILENO, TCSAFLUSH, &rz_posix_original_termios);
        rz_posix_raw_console_enabled = false;
    }
}

static inline bool rz_posix_console_enable_raw(void) {
    struct termios raw;
    if (rz_posix_raw_console_enabled) {
        return true;
    }
    if (!isatty(STDIN_FILENO)) {
        return false;
    }
    if (tcgetattr(STDIN_FILENO, &rz_posix_original_termios) != 0) {
        return false;
    }
    raw = rz_posix_original_termios;
    raw.c_lflag &= (tcflag_t)~(ECHO | ICANON);
    raw.c_iflag &= (tcflag_t)~(IXON | ICRNL);
    raw.c_cc[VMIN] = 0;
    raw.c_cc[VTIME] = 0;
    if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) != 0) {
        return false;
    }
    rz_posix_raw_console_enabled = true;
    atexit(rz_posix_console_restore);
    return true;
}

static inline void rz_posix_console_echo_bytes(const char* text, size_t length) {
    if (length > 0) {
        (void)write(STDOUT_FILENO, text, length);
    }
}

static inline void rz_posix_console_line_reset(void) {
    rz_posix_console_line_state.length = 0;
    rz_posix_console_line_state.truncated = false;
}

static inline void rz_posix_console_line_push_char(char c) {
    if (rz_posix_console_line_state.length + 1 < sizeof(rz_posix_console_line_state.buffer)) {
        rz_posix_console_line_state.buffer[rz_posix_console_line_state.length++] = c;
        rz_posix_console_line_state.buffer[rz_posix_console_line_state.length] = '\0';
    } else {
        rz_posix_console_line_state.truncated = true;
    }
}

static inline rz_os_result_t rz_posix_console_line_finish(char* buffer, size_t size) {
    size_t copy_len = 0;
    if (size > 0) {
        copy_len = rz_posix_console_line_state.length;
        if (copy_len >= size) {
            copy_len = size - 1;
        }
        memcpy(buffer, rz_posix_console_line_state.buffer, copy_len);
        buffer[copy_len] = '\0';
    }

    rz_os_result_t result =
        (rz_posix_console_line_state.truncated || rz_posix_console_line_state.length >= size)
        ? RZ_INPUT_TOO_LONG
        : RZ_OK;
    rz_posix_console_line_reset();
    return result;
}

static inline bool rz_posix_console_handle_input_bytes(const char* bytes, size_t length, char* buffer, size_t size, rz_os_result_t* result_out) {
    rz_keyboard_queue_push_input_bytes(bytes, length);
    for (size_t i = 0; i < length; i++) {
        char c = bytes[i];
        if (c == 27 && i + 2 < length && bytes[i + 1] == '[') {
            i += 2;
            continue;
        }
        if (c == '\r' || c == '\n') {
            rz_posix_console_echo_bytes("\r\n", 2);
            *result_out = rz_posix_console_line_finish(buffer, size);
            return true;
        }
        if (c == '\b' || c == 127) {
            if (rz_posix_console_line_state.length > 0) {
                rz_posix_console_line_state.length--;
                rz_posix_console_line_state.buffer[rz_posix_console_line_state.length] = '\0';
                rz_posix_console_echo_bytes("\b \b", 3);
            }
            continue;
        }
        if ((unsigned char)c >= ' ' || c == '\t') {
            rz_posix_console_line_push_char(c);
            rz_posix_console_echo_bytes(&c, 1);
        }
    }
    return false;
}

static rz_os_result_t rz_readline_timeout_terminal(char* buffer, size_t size, uint32_t timeout_ms) {
    int stdin_fd = fileno(stdin);
    fd_set read_fds;
    int ready;
    char input_buffer[16];
    ssize_t bytes_read;
    rz_os_result_t line_result = RZ_TIMEOUT;

    if (!rz_posix_console_enable_raw()) {
        return RZ_NO_INPUT;
    }

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

    bytes_read = read(stdin_fd, input_buffer, sizeof(input_buffer));
    if (bytes_read <= 0) {
        if (bytes_read < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
            return RZ_TIMEOUT;
        }
        return RZ_NO_INPUT;
    }
    if (rz_posix_console_handle_input_bytes(input_buffer, (size_t)bytes_read, buffer, size, &line_result)) {
        return line_result;
    }
    return RZ_TIMEOUT;
}
#endif

/* https://stackoverflow.com/a/4023921 */
static rz_os_result_t rz_readline(char* buffer, size_t size) {
    if (fgets(buffer, size, stdin) == NULL) {
        return RZ_NO_INPUT;
    }

    size_t len = strlen(buffer);
    bool had_line_ending = false;
    rz_keyboard_queue_push_input_bytes(buffer, len);
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
    if (isatty(stdin_fd)) {
        return rz_readline_timeout_terminal(buffer, size, timeout_ms);
    }
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
