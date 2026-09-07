/*
 *
 * Copyright (C) 2026 Nvtop contributors
 *
 * This file is part of Nvtop.
 *
 * Nvtop is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Nvtop is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with nvtop.  If not, see <http://www.gnu.org/licenses/>.
 */

#include "nvtop/export_server.h"
#include "nvtop/remote_proto.h"

#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netinet/in.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <unistd.h>

static struct list_head *monitored_gpus;
static int listen_sock = -1;
static int client_sock = -1;
static uint8_t *frame_buf;
static size_t frame_buf_size = 0;

void export_server_init(struct list_head *monitored_gpus_ptr) {
  monitored_gpus = monitored_gpus_ptr;
  client_sock = -1;
}

int export_server_start(unsigned short port, const char *bind_addr) {
  struct sockaddr_in addr;
  int opt = 1;

  listen_sock = socket(AF_INET, SOCK_STREAM, 0);
  if (listen_sock < 0) return -1;
  setsockopt(listen_sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

  memset(&addr, 0, sizeof(addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons(port);
  if (bind_addr && strlen(bind_addr) > 0) {
    if (inet_pton(AF_INET, bind_addr, &addr.sin_addr) != 1) {
      close(listen_sock);
      listen_sock = -1;
      return -1;
    }
  } else {
    addr.sin_addr.s_addr = INADDR_ANY;
  }
  if (bind(listen_sock, (struct sockaddr *)&addr, sizeof(addr)) != 0) {
    close(listen_sock);
    listen_sock = -1;
    return -1;
  }
  if (listen(listen_sock, 8) != 0) {
    close(listen_sock);
    listen_sock = -1;
    return -1;
  }
  fcntl(listen_sock, F_SETFL, fcntl(listen_sock, F_GETFL, 0) | O_NONBLOCK);
  return 0;
}

/* Serve exactly one POLL: read 1 byte, encode the current device list,
 * and send the length-prefixed frame. Returns true if a frame was sent. */
bool export_server_poll(void) {
  if (listen_sock < 0) return false;

  /* Accept a new client if one is waiting (non-blocking). */
  if (client_sock < 0) {
    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);
    client_sock = accept(listen_sock, (struct sockaddr *)&client_addr, &addr_len);
    if (client_sock >= 0) {
      /* Use a blocking socket with SO_RCVTIMEO/SO_SNDTIMEO for timeouts,
       * and SO_LINGER so close() flushes the send buffer. */
      struct timeval tv = {1, 0};
      setsockopt(client_sock, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv));
      setsockopt(client_sock, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv));
      /* Force close() to flush the send buffer before tearing down, so a
       * consumer reading the frame receives every byte. */
      struct linger lg = {1, 30};
      setsockopt(client_sock, SOL_SOCKET, SO_LINGER, &lg, sizeof(lg));
    }
  }
  if (client_sock < 0) return false;

  uint8_t poll_byte;
  ssize_t n = recv(client_sock, &poll_byte, 1, 0);
  while (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
    struct timespec ts = {0, 50 * 1000 * 1000};
    nanosleep(&ts, NULL);
    n = recv(client_sock, &poll_byte, 1, 0);
  }
  /* Client closed the connection (recv returned 0) or errored: tear down
   * the socket so the next client can be accepted. Without this, a stale
   * connection holds client_sock and blocks all subsequent POLLs. */
  if (n == 0) {
    close(client_sock);
    client_sock = -1;
    return false;
  }
  if (n != 1) return false;

  /* Encode the current snapshot into the frame buffer. */
  if (!frame_buf) {
    frame_buf_size = 1 << 16;
    frame_buf = (uint8_t *)malloc(frame_buf_size);
  }
  size_t frame_len = remote_wire_encode(monitored_gpus, frame_buf, frame_buf_size);
  if (frame_len == 0) return false;
  if (frame_len + 4 > frame_buf_size) return false;

  const uint8_t *p = frame_buf;
  size_t total = frame_len + 4;
  while (total > 0) {
    ssize_t sent = send(client_sock, p, total, MSG_NOSIGNAL);
    if (sent > 0) {
      p += sent;
      total -= (size_t)sent;
    } else if (sent == 0) {
      close(client_sock);
      client_sock = -1;
      return false;
    } else {
      if (errno != EAGAIN && errno != EWOULDBLOCK) {
        close(client_sock);
        client_sock = -1;
        return false;
      }
      struct timespec ts = {0, 50 * 1000 * 1000};
      nanosleep(&ts, NULL);
    }
  }
  /* close() with SO_LINGER flushes the send buffer before tearing down,
   * so the consumer receives every byte of the frame. Then reset
   * client_sock so the next poller's connection is accepted. */
  close(client_sock);
  client_sock = -1;
  return true;
}

void export_server_stop(void) {
  if (client_sock >= 0) {
    close(client_sock);
    client_sock = -1;
  }
  if (listen_sock >= 0) {
    close(listen_sock);
    listen_sock = -1;
  }
}
