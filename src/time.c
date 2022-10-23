/*
 *
 * Copyright (C) 2018-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/time.h"

extern inline void nvtop_get_current_time(nvtop_time *time);
extern inline double nvtop_difftime(nvtop_time t0, nvtop_time t1);
extern inline uint64_t nvtop_difftime_u64(nvtop_time t0, nvtop_time t1);
extern inline uint64_t nvtop_time_u64(nvtop_time t0);
extern inline nvtop_time nvtop_add_time(nvtop_time t0, nvtop_time t1);
extern inline nvtop_time nvtop_substract_time(nvtop_time t0, nvtop_time t1);
extern inline nvtop_time nvtop_hmns_to_time(unsigned hour, unsigned minutes, unsigned long nanosec);
