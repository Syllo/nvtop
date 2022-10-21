/*
 *
 * Copyright (C) 2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef INTERFACE_SETUP_WIN_H__
#define INTERFACE_SETUP_WIN_H__

#include "nvtop/extract_gpuinfo.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/interface_layout_selection.h"

void alloc_setup_window(struct window_position *position, struct setup_window *setup_win);

void free_setup_window(struct setup_window *setup_win);

void show_setup_window(struct nvtop_interface *interface);

void hide_setup_window(struct nvtop_interface *interface);

void draw_setup_window(unsigned monitored_dev_count, struct list_head *devices, struct nvtop_interface *interface);

void draw_setup_window_shortcuts(struct nvtop_interface *interface);

void handle_setup_win_keypress(int keyId, struct nvtop_interface *interface);

#endif // INTERFACE_SETUP_WIN_H__
