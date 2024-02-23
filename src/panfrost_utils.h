/*
 *
 * Copyright (C) 2023 Adrian Larumbe <adrian.larumbe@collabora.com>
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

#include <stdint.h>

const char * panfrost_parse_marketing_name(uint64_t gpu_id);
unsigned int util_last_bit(unsigned int u);
unsigned int get_number_engines(uint32_t gpu_id, int core_count, uint32_t core_features, uint32_t thread_features);
