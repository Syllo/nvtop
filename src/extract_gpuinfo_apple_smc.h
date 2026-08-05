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

#ifndef EXTRACT_GPUINFO_APPLE_SMC_H_
#define EXTRACT_GPUINFO_APPLE_SMC_H_

#include <stdbool.h>

struct gpuinfo_apple_smc;

bool gpuinfo_apple_smc_init(struct gpuinfo_apple_smc **smc);
void gpuinfo_apple_smc_shutdown(struct gpuinfo_apple_smc *smc);
bool gpuinfo_apple_smc_get_gpu_temperature(struct gpuinfo_apple_smc *smc, unsigned *temperature);
bool gpuinfo_apple_smc_get_fan_rpm(struct gpuinfo_apple_smc *smc, unsigned *fan_rpm);

#endif // EXTRACT_GPUINFO_APPLE_SMC_H_
