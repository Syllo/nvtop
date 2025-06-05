/*
 *
 * Copyright (C) 2017-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/ffwd_interface.h"
#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/interface_common.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/interface_layout_selection.h"
#include "nvtop/interface_options.h"
#include "nvtop/interface_ring_buffer.h"
#include "nvtop/interface_setup_win.h"
#include "nvtop/plot.h"
#include "nvtop/time.h"

#include <assert.h>
#include <inttypes.h>
#include <ncurses.h>
#include <signal.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <unistd.h>

void print_ffwd_json(struct list_head *devices, bool use_fahrenheit_option) {
  gpuinfo_populate_static_infos(devices);
  gpuinfo_refresh_dynamic_info(devices);
  struct gpu_info *device;

  printf("[\n");
  list_for_each_entry(device, devices, list) {
    const char *indent_level_two = "  ";
    const char *indent_level_four = "   ";

    const char *device_name_field = "device_name";
    const char *gpu_clock_field = "gpu_clock";
    const char *mem_clock_field = "mem_clock";
    const char *temp_field = "temp";
    const char *fan_field = "fan_speed"; // RPM
    const char *fan_field_pct = "fan_speed_percentage";
    const char *cpu_fan_field = "cpu_fan";
    const char *power_field = "power_draw"; // mW
    const char *gpu_util_field = "gpu_util";
    const char *mem_util_field = "mem_util";
    const char *mem_used_field = "mem_used";
    const char *mem_free_field = "mem_free";
    const char *pcie_ingress_field = "pcie_ingress_rate"; // KB/s, traffic into the GPU
    const char *pcie_egress_field = "pcie_egress_rate"; // KB/s, traffic out of the GPU
    const char *encoder_util_field = "encoder_util";
    const char *decoder_util_field = "decoder_util";

    printf("%s{\n", indent_level_two);

    // Device Name
    if (GPUINFO_STATIC_FIELD_VALID(&device->static_info, device_name)) {
      printf("%s\"%s\": \"%s\",\n", indent_level_four, device_name_field, device->static_info.device_name);
    }
    // GPU Clock Speed
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_clock_speed)) {
      printf("%s\"%s\": %u,\n", indent_level_four, gpu_clock_field, device->dynamic_info.gpu_clock_speed);
    }

    // MEM Clock Speed
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_clock_speed)) {
      printf("%s\"%s\": %u,\n", indent_level_four, mem_clock_field, device->dynamic_info.mem_clock_speed);
    }

    // GPU Temperature, Celsius only for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_temp)) {
      printf("%s\"%s\": %u,\n", indent_level_four, temp_field, device->dynamic_info.gpu_temp);
    }

    // Fan speed, modified for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, fan_speed)) {
      printf("%s\"%s\": %u,\n", indent_level_four, fan_field_pct,
             device->dynamic_info.fan_speed > 100 ? 100 : device->dynamic_info.fan_speed);
    }
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, fan_rpm)) {
      printf("%s\"%s\": %u,\n", indent_level_four, fan_field,
             device->dynamic_info.fan_rpm > 9999 ? 9999 : device->dynamic_info.fan_rpm);
    }
    if (device->static_info.integrated_graphics) {
      printf("%s\"%s\": true,\n", indent_level_four, cpu_fan_field);
    }

    // Memory used, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, used_memory)) {
      printf("%s\"%s\": %u,\n", indent_level_four, mem_used_field, device->dynamic_info.used_memory);
    }
    // Memory free, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, free_memory)) {
      printf("%s\"%s\": %u,\n", indent_level_four, mem_free_field, device->dynamic_info.used_memory);
    }
    // Memory PCIe ingress, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_rx)) {
      printf("%s\"%s\": %u,\n", indent_level_four, pcie_ingress_field, device->dynamic_info.pcie_rx);
    }
    // Memory PCIe egress, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_tx)) {
      printf("%s\"%s\": %u,\n", indent_level_four, pcie_egress_field, device->dynamic_info.pcie_tx);
    }
    // Encoder util, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, encoder_rate)) {
      printf("%s\"%s\": %u,\n", indent_level_four, encoder_util_field, device->dynamic_info.encoder_rate);
    }
    // Decoder util, new for FFWD
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, decoder_rate)) {
      printf("%s\"%s\": %u,\n", indent_level_four, decoder_util_field, device->dynamic_info.decoder_rate);
    }

    // Power draw
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw)) {
      printf("%s\"%s\": %u,\n", indent_level_four, power_field, device->dynamic_info.power_draw / 1000);
    }

    // GPU Utilization
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_util_rate)) {
      printf("%s\"%s\": %u,\n", indent_level_four, gpu_util_field, device->dynamic_info.gpu_util_rate);
    }
    // Memory Utilization
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_util_rate)) {
      printf("%s\"%s\": %u\n", indent_level_four, mem_util_field, device->dynamic_info.mem_util_rate);
    } else {
      printf("%s\"%s\": null\n", indent_level_four, mem_util_field); //avoid trailing comma
    }

    if (device->list.next == devices)
      printf("%s}\n", indent_level_two);
    else
      printf("%s},\n", indent_level_two);
  }
  printf("]\n");
}
