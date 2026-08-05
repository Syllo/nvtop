/*
 *
 * Copyright (C) 2026 NVTOP contributors
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

#ifndef EXTRACT_GPUINFO_APPLE_IOREPORT_H_
#define EXTRACT_GPUINFO_APPLE_IOREPORT_H_

#include <stdbool.h>

struct gpuinfo_apple_ioreport;

bool gpuinfo_apple_ioreport_init(struct gpuinfo_apple_ioreport **ioreport);
void gpuinfo_apple_ioreport_shutdown(struct gpuinfo_apple_ioreport *ioreport);
bool gpuinfo_apple_ioreport_get_power_draw(struct gpuinfo_apple_ioreport *ioreport, unsigned *power_draw);
bool gpuinfo_apple_ioreport_get_gpu_clock_speed(struct gpuinfo_apple_ioreport *ioreport, unsigned *clock_speed,
                                               unsigned *max_clock_speed);

#endif // EXTRACT_GPUINFO_APPLE_IOREPORT_H_
