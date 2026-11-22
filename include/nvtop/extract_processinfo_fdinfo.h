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

#include "nvtop/extract_gpuinfo_common.h"

#include <stdio.h>

/**
 * @brief A callback function that populates the \p process_info structure from
 * the information gathered while parsing the fdinfo file \p fdinfo_file. Return true if the data in process_info is
 * valid, false otherwise.
 */
typedef bool (*processinfo_fdinfo_callback)(struct gpu_info *info, FILE *fdinfo_file, struct gpu_process *process_info);

/**
 * @brief Register a callback function to parse a fdinfo file opened to a device driver
 *
 * The device node lets the sweep decide which device owns each fd without
 * running every backend's parser on fdinfo files from other drivers. It is
 * resolved to a device number; for DRM devices it is also expanded to the
 * sibling card/render nodes through sysfs. This keeps driver-specific parsing
 * from being applied to a file descriptor of another device that happens to
 * expose similar keys.
 *
 * A usable device node is mandatory: if \p device_node is NULL or cannot be
 * resolved to a character device, no callback is registered.
 *
 * @param callback The callback function
 * @param info The struct gpu_info that will be updated when the callback succeeds.
 * @param device_node Path to a device node of the device (e.g.,
 * /dev/dri/renderD128 or /dev/accel/accel0). The device's other DRM nodes are
 * found through sysfs, and only the fds opened to the registered nodes are
 * passed to \p callback.
 * @return true when the node was resolved and the callback registered.
 */
bool processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info,
                                          const char *device_node);

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
 * @brief Scan all the processes in /proc. Call the callback of the device owning
 * each device file descriptor that can successfully be opened. If the callback
 * succeeds, the gpu_info structure processes array will be updated with the
 * retrieved data. Active devices also receive processes_query_succeeded unless
 * the scan of /proc failed, or one of their own fds could not be read or parsed.
 * Processes or descriptors that disappear during the scan, processes that cannot
 * be inspected without privileges, and fds of other devices are skipped.
 */
void processinfo_sweep_fdinfos(void);

#endif // NVTOP_EXTRACT_PROCESSINFO_FDINFO__
