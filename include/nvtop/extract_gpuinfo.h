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

#ifndef EXTRACT_GPUINFO_H_
#define EXTRACT_GPUINFO_H_

#include "nvtop/extract_gpuinfo_common.h"

#include <stdbool.h>

bool gpuinfo_init_info_extraction(unsigned *total_dev_count, struct list_head *devices);

bool gpuinfo_shutdown_info_extraction(struct list_head *devices);

bool gpuinfo_populate_static_infos(struct list_head *devices);

bool gpuinfo_refresh_dynamic_info(struct list_head *devices);

bool gpuinfo_fix_dynamic_info_from_process_info(struct list_head *devices);

bool gpuinfo_refresh_processes(struct list_head *devices);

bool gpuinfo_utilisation_rate(struct list_head *devices);

void gpuinfo_clean(struct list_head *devices);

void gpuinfo_clear_cache(void);

#endif // EXTRACT_GPUINFO_H_
