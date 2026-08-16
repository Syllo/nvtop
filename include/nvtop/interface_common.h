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

#ifndef INTERFACE_COMMON_H__
#define INTERFACE_COMMON_H__

enum plot_information {
  plot_gpu_rate = 0,
  plot_gpu_mem_rate,
  plot_encoder_rate,
  plot_decoder_rate,
  plot_gpu_temperature,
  plot_gpu_power_draw_rate,
  plot_fan_speed,
  plot_gpu_clock_rate,
  plot_gpu_mem_clock_rate,
  plot_effective_load_rate,
  plot_information_count
};

typedef int plot_info_to_draw;

enum process_field {
  process_pid = 0,
  process_user,
  process_gpu_id,
  process_type,
  process_gpu_rate,
  process_enc_rate,
  process_dec_rate,
  process_memory,
  process_cpu_usage,
  process_cpu_mem_usage,
  process_command,
  process_field_count,
  // Sentinel used to represent "no sorting field" (e.g. no secondary sort key)
  process_none = process_field_count,
};

// Filter applied to the displayed process list based on their workload type
enum process_type_filter {
  process_type_filter_all = 0,       // Show every process
  process_type_filter_compute_only,  // Show compute (and compute+graphical) processes only
  process_type_filter_graphical_only,// Show graphical (and compute+graphical) processes only
  process_type_filter_count,
};

typedef int process_field_displayed;

#endif // INTERFACE_COMMON_H__
