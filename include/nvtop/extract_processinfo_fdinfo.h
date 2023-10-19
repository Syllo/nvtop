/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef NVTOP_EXTRACT_PROCESSINFO_FDINFO__
#define NVTOP_EXTRACT_PROCESSINFO_FDINFO__

#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo_common.h"

#include <stdio.h>

/**
 * @brief A callback function that populates the \p process_info structure from
 * the information gathered while parsing the fdinfo file \p fdinfo_file. Return true if the data in process_info is
 * valid, false otherwise.
 */
typedef bool (*processinfo_fdinfo_callback)(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);

/**
 * @brief Register a callback function to parse a fdinfo file opened to a DRM driver
 *
 * @param callback The callback function
 * @param info The struct gpu_info that will be updated when the callback succeeds.
 */
void processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info);

/**
 * @brief Remove a previously registered callback.
 *
 * @param info The callback to this gpu_info will be removed
 */
void processinfo_drop_callback(const struct gpu_info *info);

/**
 * @brief Enables or disables a fdinfo processing callback
 *
 * @param info Enabling/Disabling the callback to this gpu_info
 * @param enable True to enable the callback, false to disable
 */
void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable);

/**
 * @brief Scann all the processes in /proc. Call the registered callbacks on
 * each file descriptor to the DRM driver that can successfully be oppened. If a
 * callback succeeds, the gpu_info structure processes array will be updated
 * with the retrieved data.
 */
void processinfo_sweep_fdinfos(void);

#endif // NVTOP_EXTRACT_PROCESSINFO_FDINFO__
