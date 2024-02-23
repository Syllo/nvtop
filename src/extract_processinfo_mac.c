/*
 * Copyright (C) 2023 Robin Voetter <robin@voetter.nl>
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

#include "nvtop/extract_processinfo_fdinfo.h"

void processinfo_drop_callback(const struct gpu_info *info) {
  (void) info;
}

void processinfo_register_fdinfo_callback(processinfo_fdinfo_callback callback, struct gpu_info *info) {
  (void) callback;
  (void) info;
}

void processinfo_sweep_fdinfos(void) {
}

void processinfo_enable_disable_callback_for(const struct gpu_info *info, bool enable) {
  (void)info;
  (void)enable;
}
