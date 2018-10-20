/*
 *
 * Copyright (C) 2017-2018 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

enum process_field {
  process_pid = 0,
  process_user,
  process_gpu_id,
  process_type,
  process_memory,
  process_command,
  process_end,
};

enum nvtop_option_window_state {
  nvtop_option_state_hidden,
  nvtop_option_state_kill,
  nvtop_option_state_sort_by,
};

struct nvtop_interface;

void show_gpu_infos_ascii(
    unsigned int num_devices,
    struct device_info *dev_info);

struct nvtop_interface* initialize_curses(unsigned int num_devices,
    unsigned int biggest_device_name, bool use_color, bool use_fahrenheit,
    double encode_decode_hide_time);

void clean_ncurses(struct nvtop_interface *interface);

void draw_gpu_info_ncurses(
    struct device_info *dev_info,
    struct nvtop_interface *interface);

void update_window_size_to_terminal_size(struct nvtop_interface *inter);

void interface_key(int keyId, struct nvtop_interface *inter);

bool is_escape_for_quit(struct nvtop_interface *inter);

#endif // INTERFACE_H_
