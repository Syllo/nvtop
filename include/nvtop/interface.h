/*
 *
 * Copyright (C) 2017-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
 *
 */

#ifndef INTERFACE_H_
#define INTERFACE_H_

#include "nvtop/extract_gpuinfo.h"
#include "nvtop/interface_common.h"
#include "nvtop/interface_options.h"

struct nvtop_interface;

struct nvtop_interface *initialize_curses(unsigned num_devices,
                                          unsigned largest_device_name,
                                          nvtop_interface_option options);

void clean_ncurses(struct nvtop_interface *interface);

void draw_gpu_info_ncurses(unsigned devices_count, gpu_info *devices,
                           struct nvtop_interface *interface);

void update_interface_retained_data(unsigned devices_count, gpu_info *devices,
                                    struct nvtop_interface *interface);

void update_window_size_to_terminal_size(struct nvtop_interface *inter);

void interface_key(int keyId, struct nvtop_interface *inter);

bool is_escape_for_quit(struct nvtop_interface *inter);

bool interface_freeze_processes(struct nvtop_interface *interface);

#endif // INTERFACE_H_
