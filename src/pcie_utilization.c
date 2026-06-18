/*
 *
 * Copyright (C) 2024 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/pcie_utilization.h"

unsigned pcie_max_bandwidth_kbs(const struct gpuinfo_static_info *static_info) {
  if (!GPUINFO_STATIC_FIELD_VALID(static_info, max_pcie_gen) ||
      !GPUINFO_STATIC_FIELD_VALID(static_info, max_pcie_link_width))
    return 0;
  unsigned per_lane_kbs;
  switch (static_info->max_pcie_gen) {
  case 1:
    per_lane_kbs = 250000u;
    break;
  case 2:
    per_lane_kbs = 500000u;
    break;
  case 3:
    per_lane_kbs = 984600u;
    break;
  case 4:
    per_lane_kbs = 1969000u;
    break;
  case 5:
    per_lane_kbs = 3938000u;
    break;
  case 6:
    per_lane_kbs = 7563000u;
    break;
  default:
    return 0;
  }
  return per_lane_kbs * static_info->max_pcie_link_width;
}

unsigned pcie_load_percent(unsigned value_kbs, const struct gpuinfo_static_info *static_info) {
  unsigned max_bw = pcie_max_bandwidth_kbs(static_info);
  if (max_bw == 0)
    return 0;
  unsigned percent = (unsigned)((unsigned long long)value_kbs * 100u / max_bw);
  return percent > 100u ? 100u : percent;
}
