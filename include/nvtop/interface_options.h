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

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/interface_common.h"

#include <stdbool.h>

typedef struct {
  plot_info_to_draw to_draw;  // The set of metrics to draw for this gpu
  bool doNotMonitor;          // True if this GPU should not be monitored
  struct gpu_info *linkedGpu; // The gpu to which this option apply
} nvtop_interface_gpu_opts;

typedef struct nvtop_interface_option_struct {
  bool
      plot_left_to_right; // true to reverse the plot refresh direction defines inactivity (0 use rate) before hiding it
  bool temperature_in_fahrenheit;                   // Switch from celsius to fahrenheit temperature scale
  bool use_color;                                   // Name self explanatory
  double encode_decode_hiding_timer;                // Negative to always display, positive
  nvtop_interface_gpu_opts *gpu_specific_opts;      // GPU specific options
  char *config_file_location;                       // Location of the config file
  enum process_field sort_processes_by;             // Specify the field used to order the processes
  bool sort_descending_order;                       // Sort in descending order
  int update_interval;                              // Interval between interface update in milliseconds
  process_field_displayed process_fields_displayed; // Which columns of the
                                                    // process list are displayed
  bool show_startup_messages;                       // True to show the startup messages
  bool filter_nvtop_pid;                            // Do not show nvtop pid in the processes list
  bool has_monitored_set_changed;                   // True if the set of monitored gpu was modified through the interface
  bool has_gpu_info_bar;                            // Show info bar with additional GPU parametres
  bool hide_processes_list;                         // Hide processes list
} nvtop_interface_option;

inline bool plot_isset_draw_info(enum plot_information check_info, plot_info_to_draw to_draw) {
  return (to_draw & (1 << check_info)) > 0;
}

inline unsigned plot_count_draw_info(plot_info_to_draw to_draw) {
  unsigned count = 0;
  for (int i = plot_gpu_rate; i < plot_information_count; ++i) {
    count += plot_isset_draw_info((enum plot_information)i, to_draw);
  }
  return count;
}

inline plot_info_to_draw plot_add_draw_info(enum plot_information set_info, plot_info_to_draw to_draw) {
  if (plot_count_draw_info(to_draw) < MAX_LINES_PER_PLOT)
    return to_draw | (1 << set_info);
  else
    return to_draw;
}

inline plot_info_to_draw plot_remove_draw_info(enum plot_information reset_info, plot_info_to_draw to_draw) {
  return to_draw & (~(1 << reset_info));
}

inline plot_info_to_draw plot_default_draw_info(void) { return (1 << plot_gpu_rate) | (1 << plot_gpu_mem_rate); }

void alloc_interface_options_internals(char *config_file_location, unsigned num_devices, struct list_head *devices,
                                       nvtop_interface_option *options);

unsigned interface_check_and_fix_monitored_gpus(unsigned num_devices, struct list_head *monitoredGpus,
                                                struct list_head *nonMonitoredGpus, nvtop_interface_option *options);

bool load_interface_options_from_config_file(unsigned num_devices, nvtop_interface_option *options);

bool save_interface_options_to_config_file(unsigned total_dev_count, const nvtop_interface_option *options);

inline bool process_is_field_displayed(enum process_field field, process_field_displayed fields_displayed) {
  return (fields_displayed & (1 << field)) > 0;
}

inline process_field_displayed process_remove_field_to_display(enum process_field field,
                                                               process_field_displayed fields_displayed) {
  return fields_displayed & (~(1 << field));
}

inline process_field_displayed process_add_field_to_display(enum process_field field,
                                                            process_field_displayed fields_displayed) {
  return fields_displayed | (1 << field);
}

inline process_field_displayed process_default_displayed_field(void) {
  process_field_displayed to_display = 0;
  for (int field = process_pid; field < process_field_count; ++field) {
    to_display = process_add_field_to_display((enum process_field)field, to_display);
  }
  to_display = process_remove_field_to_display(process_enc_rate, to_display);
  to_display = process_remove_field_to_display(process_dec_rate, to_display);
  return to_display;
}

inline unsigned process_field_displayed_count(process_field_displayed fields_displayed) {
  unsigned displayed_count = 0;
  for (int field = process_pid; field < process_field_count; ++field) {
    if (process_is_field_displayed((enum process_field)field, fields_displayed))
      displayed_count++;
  }
  return displayed_count;
}

enum process_field process_default_sort_by_from(process_field_displayed fields_displayed);

#endif // INTERFACE_OPTIONS_H__
