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

#ifndef INTERFACE_INTERNAL_COMMON_H__
#define INTERFACE_INTERNAL_COMMON_H__

#include "nvtop/common.h"
#include "nvtop/interface_layout_selection.h"
#include "nvtop/interface_options.h"
#include "nvtop/interface_ring_buffer.h"
#include "nvtop/time.h"

#include <ncurses.h>

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))
#define ARRAY_SIZE(x) (sizeof(x) / sizeof((x)[0]))

enum nvtop_option_window_state {
  nvtop_option_state_hidden,
  nvtop_option_state_kill,
  nvtop_option_state_sort_by,
};

enum interface_color {
  cyan_color = 1,
  yellow_color,
  magenta_color,
  green_color,
  red_color,
  blue_color,
};

struct device_window {
  WINDOW *name_win; // Name of the GPU
  WINDOW *gpu_util_enc_dec;
  WINDOW *gpu_util_no_enc_or_dec;
  WINDOW *gpu_util_no_enc_and_dec;
  WINDOW *mem_util_enc_dec;
  WINDOW *mem_util_no_enc_or_dec;
  WINDOW *mem_util_no_enc_and_dec;
  WINDOW *encode_util;
  WINDOW *decode_util;
  WINDOW *fan_speed;
  WINDOW *temperature;
  WINDOW *power_info;
  WINDOW *gpu_clock_info;
  WINDOW *mem_clock_info;
  WINDOW *pcie_info;
  WINDOW *shader_cores;
  WINDOW *l2_cache_size;
  WINDOW *exec_engines;
  bool enc_was_visible;
  bool dec_was_visible;
  nvtop_time last_decode_seen;
  nvtop_time last_encode_seen;
};

static const unsigned int option_window_size = 13;
struct option_window {
  enum nvtop_option_window_state state;
  enum nvtop_option_window_state previous_state;
  unsigned int selected_row;
  unsigned int offset;
  WINDOW *option_win;
};

struct process_window {
  unsigned offset;
  unsigned offset_column;
  WINDOW *process_win;
  WINDOW *process_with_option_win;
  unsigned selected_row;
  pid_t selected_pid;
  struct option_window option_window;
};

struct plot_window {
  size_t num_data;
  double *data;
  WINDOW *win;
  WINDOW *plot_window;
  unsigned num_devices_to_plot;
  unsigned devices_ids[MAX_LINES_PER_PLOT];
};

enum setup_window_section {
  setup_general_selected,
  setup_header_selected,
  setup_chart_selected,
  setup_process_list_selected,
  setup_monitored_gpu_list_selected,
  setup_window_selection_count
};

struct setup_window {
  unsigned indentation_level;
  enum setup_window_section selected_section;
  bool visible;
  WINDOW *clean_space;
  WINDOW *setup;
  WINDOW *single;
  WINDOW *split[2];
  unsigned options_selected[2];
};

// Keep gpu information every 1 second for 10 minutes
struct nvtop_interface {
  nvtop_interface_option options;
  unsigned total_dev_count;
  unsigned monitored_dev_count;
  struct device_window *devices_win;
  struct process_window process;
  WINDOW *shortcut_window;
  unsigned num_plots;
  struct plot_window *plots;
  interface_ring_buffer saved_data_ring;
  struct setup_window setup_win;
};

enum device_field {
  device_name = 0,
  device_fan_speed,
  device_temperature,
  device_power,
  device_pcie,
  device_clock,
  device_shadercores,
  device_l2features,
  device_execengines,
  device_field_count,
};

inline void set_attribute_between(WINDOW *win, int startY, int startX, int endX, attr_t attr, short pair) {
  int rows, cols;
  getmaxyx(win, rows, cols);
  (void)rows;
  if (startX >= cols || endX < 0)
    return;
  startX = startX < 0 ? 0 : startX;
  endX = endX > cols ? cols : endX;
  int size = endX - startX;
  mvwchgat(win, startY, startX, size, attr, pair, NULL);
}

#endif // INTERFACE_INTERNAL_COMMON_H__
