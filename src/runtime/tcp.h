#pragma once

#include <errno.h>
#include <stdbool.h>
#include <stdint.h>
#include <inttypes.h>
#include <stdio.h>
#include <string.h>

#ifndef __RZ_INPUT_BUFFER_SIZE
#define __RZ_INPUT_BUFFER_SIZE 512
#endif

#include "allocation.h"
#include "channel.h"
#include "core.h"

#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <winsock2.h>
#include <ws2tcpip.h>
typedef SOCKET rz_socket_t;
#define RZ_INVALID_SOCKET INVALID_SOCKET
#define rz_socket_close closesocket
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <sys/socket.h>
#include <unistd.h>
typedef int rz_socket_t;
#define RZ_INVALID_SOCKET (-1)
#define rz_socket_close close
#endif

typedef struct rz_tcp_client
{
    rz_socket_t socket;
    char buffer[__RZ_INPUT_BUFFER_SIZE];
    size_t length;
    struct rz_tcp_client *next;
} rz_tcp_client_t;

typedef struct rz_tcp_input
{
    int64_t port;
    rz_channel_t channel;
    rz_socket_t listen_socket;
    rz_tcp_client_t *clients;
    struct rz_tcp_input *next;
} rz_tcp_input_t;

static rz_tcp_input_t *rz_tcp_inputs = NULL;
static bool rz_tcp_initialized = false;

static inline void rz_tcp_ensure_initialized(void)
{
    if (rz_tcp_initialized)
    {
        return;
    }
#ifdef _WIN32
    WSADATA data;
    if (WSAStartup(MAKEWORD(2, 2), &data) != 0)
    {
        fprintf(stderr, "Runtime error: failed to initialize Winsock\n");
        exit(1);
    }
#endif
    rz_tcp_initialized = true;
}

static inline bool rz_tcp_would_block(void)
{
#ifdef _WIN32
    int err = WSAGetLastError();
    return err == WSAEWOULDBLOCK;
#else
    return errno == EAGAIN || errno == EWOULDBLOCK;
#endif
}

static inline void rz_tcp_set_nonblocking(rz_socket_t socket)
{
#ifdef _WIN32
    u_long mode = 1;
    if (ioctlsocket(socket, FIONBIO, &mode) != 0)
    {
        fprintf(stderr, "Runtime error: failed to make TCP socket nonblocking\n");
        exit(1);
    }
#else
    int flags = fcntl(socket, F_GETFL, 0);
    if (flags < 0 || fcntl(socket, F_SETFL, flags | O_NONBLOCK) < 0)
    {
        fprintf(stderr, "Runtime error: failed to make TCP socket nonblocking\n");
        exit(1);
    }
#endif
}

static inline void rz_tcp_validate_port(const char *name, int64_t port)
{
    if (port <= 0 || port > 65535)
    {
        fprintf(stderr, "Runtime error: builtin '%s' expected port in range 1..65535, got %" PRId64 "\n", name, port);
        exit(1);
    }
}

static inline struct sockaddr_in rz_tcp_loopback_addr(int64_t port)
{
    struct sockaddr_in addr;
    memset(&addr, 0, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons((uint16_t)port);
    addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    return addr;
}

static inline rz_socket_t rz_tcp_create_socket(void)
{
    rz_socket_t sock;
    rz_tcp_ensure_initialized();
    sock = (rz_socket_t)socket(AF_INET, SOCK_STREAM, 0);
    if (sock == RZ_INVALID_SOCKET)
    {
        fprintf(stderr, "Runtime error: failed to create TCP socket\n");
        exit(1);
    }
    return sock;
}

static inline void rz_tcp_client_free(rz_tcp_client_t *client)
{
    if (client)
    {
        rz_socket_close(client->socket);
        rz_free(client);
    }
}

static inline void rz_tcp_reset(void)
{
    rz_tcp_input_t *input = rz_tcp_inputs;
    while (input)
    {
        rz_tcp_input_t *next_input = input->next;
        rz_tcp_client_t *client = input->clients;
        while (client)
        {
            rz_tcp_client_t *next_client = client->next;
            rz_tcp_client_free(client);
            client = next_client;
        }
        rz_socket_close(input->listen_socket);
        rz_free(input);
        input = next_input;
    }
    rz_tcp_inputs = NULL;
}

static inline bool rz_tcp_has_registered_inputs(void)
{
    return rz_tcp_inputs != NULL;
}

static inline rz_channel_t rz_tcp_input_register(int64_t port)
{
    rz_tcp_input_t *existing = rz_tcp_inputs;
    while (existing)
    {
        if (existing->port == port)
        {
            return existing->channel;
        }
        existing = existing->next;
    }

    rz_tcp_validate_port("port_input", port);
    rz_socket_t listen_socket = rz_tcp_create_socket();
    struct sockaddr_in addr = rz_tcp_loopback_addr(port);
    int reuse = 1;
    if (setsockopt(listen_socket, SOL_SOCKET, SO_REUSEADDR, (const char *)&reuse, sizeof(reuse)) != 0)
    {
        rz_socket_close(listen_socket);
        fprintf(stderr, "Runtime error: failed to configure TCP port %" PRId64 "\n", port);
        exit(1);
    }
    if (bind(listen_socket, (struct sockaddr *)&addr, sizeof(addr)) != 0)
    {
        rz_socket_close(listen_socket);
        fprintf(stderr, "Runtime error: failed to bind TCP input port %" PRId64 "\n", port);
        exit(1);
    }
    if (listen(listen_socket, 5) != 0)
    {
        rz_socket_close(listen_socket);
        fprintf(stderr, "Runtime error: failed to listen on TCP input port %" PRId64 "\n", port);
        exit(1);
    }
    rz_tcp_set_nonblocking(listen_socket);

    rz_tcp_input_t *input = (rz_tcp_input_t *)rz_malloc(sizeof(rz_tcp_input_t));
    input->port = port;
    input->channel = rz_channel_alloc();
    input->listen_socket = listen_socket;
    input->clients = NULL;
    input->next = rz_tcp_inputs;
    rz_tcp_inputs = input;
    return input->channel;
}

static inline void rz_tcp_accept_pending(rz_tcp_input_t *input)
{
    while (true)
    {
        rz_socket_t client_socket = accept(input->listen_socket, NULL, NULL);
        if (client_socket == RZ_INVALID_SOCKET)
        {
            if (rz_tcp_would_block())
            {
                return;
            }
            return;
        }
        rz_tcp_set_nonblocking(client_socket);
        rz_tcp_client_t *client = (rz_tcp_client_t *)rz_malloc(sizeof(rz_tcp_client_t));
        client->socket = client_socket;
        client->length = 0;
        client->next = input->clients;
        input->clients = client;
    }
}

static inline bool rz_tcp_client_take_line(rz_tcp_client_t *client, rz_box_t *value_out, bool *closed_out)
{
    char c;
    *closed_out = false;
    while (true)
    {
        int received = recv(client->socket, &c, 1, 0);
        if (received == 0)
        {
            *closed_out = true;
            return false;
        }
        if (received < 0)
        {
            if (rz_tcp_would_block())
            {
                return false;
            }
            *closed_out = true;
            return false;
        }
        if (c == '\n')
        {
            *value_out = rz_make_string_len(client->buffer, client->length);
            client->length = 0;
            return true;
        }
        if (c == '\r')
        {
            continue;
        }
        if (client->length + 1 >= sizeof(client->buffer))
        {
            fprintf(stderr, "Runtime error: TCP input line exceeded %d bytes\n", __RZ_INPUT_BUFFER_SIZE);
            client->length = 0;
            continue;
        }
        client->buffer[client->length++] = c;
    }
}

static inline bool rz_tcp_take_input(rz_channel_t *channel_out, rz_box_t *value_out)
{
    rz_tcp_input_t *input = rz_tcp_inputs;
    while (input)
    {
        rz_tcp_accept_pending(input);
        rz_tcp_client_t **client_ref = &input->clients;
        while (*client_ref)
        {
            rz_tcp_client_t *client = *client_ref;
            bool closed = false;
            if (rz_tcp_client_take_line(client, value_out, &closed))
            {
                *channel_out = input->channel;
                return true;
            }
            if (closed)
            {
                *client_ref = client->next;
                rz_tcp_client_free(client);
            }
            else
            {
                client_ref = &client->next;
            }
        }
        input = input->next;
    }
    return false;
}

static inline rz_socket_t rz_tcp_connect_localhost(int64_t port)
{
    rz_tcp_validate_port("port_out_signal", port);
    rz_socket_t socket = rz_tcp_create_socket();
    struct sockaddr_in addr = rz_tcp_loopback_addr(port);
    if (connect(socket, (struct sockaddr *)&addr, sizeof(addr)) != 0)
    {
        rz_socket_close(socket);
        fprintf(stderr, "Runtime error: failed to connect to TCP output port %" PRId64 "\n", port);
        exit(1);
    }
    return socket;
}

static inline void rz_tcp_send_all(rz_socket_t socket, const char *bytes, size_t len)
{
    size_t sent_total = 0;
    while (sent_total < len)
    {
        int sent = send(socket, bytes + sent_total, (int)(len - sent_total), 0);
        if (sent <= 0)
        {
            fprintf(stderr, "Runtime error: failed to send TCP output\n");
            exit(1);
        }
        sent_total += (size_t)sent;
    }
}

static inline void rz_tcp_send_line(rz_socket_t socket, rz_box_t value)
{
    const char *bytes = rz_string_data(value);
    size_t len = rz_string_byte_length(value);
    rz_tcp_send_all(socket, bytes, len);
    rz_tcp_send_all(socket, "\n", 1);
}
