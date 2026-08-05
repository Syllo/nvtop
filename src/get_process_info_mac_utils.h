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

#ifndef GET_PROCESS_INFO_MAC_UTILS_H_
#define GET_PROCESS_INFO_MAC_UTILS_H_

#include <stdbool.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

double processinfo_mac_time_to_seconds(uint64_t mach_ticks, bool translated, uint32_t timebase_numerator,
                                       uint32_t timebase_denominator);

#ifdef __cplusplus
}
#endif

#endif // GET_PROCESS_INFO_MAC_UTILS_H_
