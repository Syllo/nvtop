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

#ifndef NVTOP_EXPORT_SERVER_H__
#define NVTOP_EXPORT_SERVER_H__

#include <stdbool.h>

#include "list.h"

/*
 * nvtop --export server: a single-client TCP listener that serves one
 * length-prefixed NVT1 frame per POLL byte written by the consumer.
 *
 * All socket I/O happens on the main thread (nvtop is single-threaded),
 * so no locks are needed. The consumer sends exactly one POLL byte per
 * refresh; the server serializes the current monitored GPU list into a
 * frame and ships it back.
 */

/* Register the device list to serialize; call before start. */
void export_server_init(struct list_head *monitored_gpus);

/* Open the TCP listener. Returns 0 on success, -1 on error. */
int export_server_start(unsigned short port, const char *bind_addr);

/* Non-blocking: if a POLL byte is waiting, serve one frame and return 1. */
bool export_server_poll(void);

/* Close the listener + client, reset state. */
void export_server_stop(void);

#endif // NVTOP_EXPORT_SERVER_H__
