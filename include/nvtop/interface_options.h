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

#ifndef INTERFACE_OPTIONS_H__
#define INTERFACE_OPTIONS_H__

#include "nvtop/interface_common.h"

#include <stdbool.h>

typedef struct nvtop_interface_option_struct {
  bool plot_left_to_right;        // true to reverse the plot refresh direction
                                  // defines inactivity (0 use rate) before
                                  // hiding it
  bool temperature_in_fahrenheit; // Switch from celsius to fahrenheit
  // temperature scale
  bool use_color;                    // Name self explanatory
  double encode_decode_hiding_timer; // Negative to always display, positive
  plot_info_to_draw
      *device_information_drawn; // Stores the information to drawn for each
                                 // GPU (see enum plot_draw_information)
  char *config_file_location;    // Location of the config file
  enum process_field
      sort_processes_by;      // Specify the field used to order the processes
  bool sort_descending_order; // Sort in descenging order
  int update_interval;        // Interval between interface update in milliseconds
} nvtop_interface_option;

inline plot_info_to_draw plot_add_draw_info(enum plot_information set_info,
                                            plot_info_to_draw to_draw) {
  return to_draw | (1 << set_info);
}

inline plot_info_to_draw plot_remove_draw_info(enum plot_information reset_info,
                                               plot_info_to_draw to_draw) {
  return to_draw & (~(1 << reset_info));
}

inline plot_info_to_draw plot_default_draw_info(void) {
  return (1 << plot_gpu_rate) | (1 << plot_gpu_mem_rate);
}

inline bool plot_isset_draw_info(enum plot_information check_info,
                                 plot_info_to_draw to_draw) {
  return to_draw & (1 << check_info);
}

inline unsigned plot_count_draw_info(plot_info_to_draw to_draw) {
  unsigned count = 0;
  for (enum plot_information i = plot_gpu_rate; i < plot_information_count; ++i) {
    count += plot_isset_draw_info(i, to_draw) ? 1 : 0;
  }
  return count;
}

void alloc_interface_options_internals(char *config_file_location,
                                       unsigned num_devices,
                                       nvtop_interface_option *options);

bool load_interface_options_from_config_file(unsigned num_devices,
                                             nvtop_interface_option *options);

bool save_interface_options_to_config_file(
    unsigned num_devices, const nvtop_interface_option *options);

#endif // INTERFACE_OPTIONS_H__
