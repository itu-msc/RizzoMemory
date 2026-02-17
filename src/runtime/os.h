#pragma once

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef enum {
    RZ_OK = 0,
    RZ_NO_INPUT = 1,
    RZ_INPUT_TOO_LONG = 2,
} rz_os_result_t;

/* https://stackoverflow.com/a/4023921 */
static rz_os_result_t rz_readline(char* buffer, size_t size) {
    if (fgets(buffer, size, stdin) == NULL) {
        return RZ_NO_INPUT;
    }

    size_t len = strlen(buffer);
    if(len > 0 && buffer[len - 1] == '\n') {
        buffer[len - 1] = '\0';
        return RZ_OK;
    }

    /* empty the line */
    int ch;
    while ((ch = getchar()) != '\n' && (ch != EOF));
    return RZ_INPUT_TOO_LONG;
}
