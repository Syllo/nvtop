/*
 *
 * Copyright (C) 2026 Basavaraja Mattihalli <basavaraja.ms7@gmail.com>
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

#ifndef PCIE_UTILIZATION_H__
#define PCIE_UTILIZATION_H__

#include "nvtop/extract_gpuinfo_common.h"

// Returns the maximum unidirectional PCIe bandwidth in KB/s derived from the link
// generation and width, or 0 if it cannot be determined. The per-lane figures use the
// effective data rate after PCIe encoding overhead (8b/10b for gen 1/2, 128b/130b for
// gen 3+, FLIT for gen 6).
unsigned pcie_max_bandwidth_kbs(const struct gpuinfo_static_info *static_info);

// Converts an instantaneous PCIe throughput (KB/s) to a 0-100% load relative to the
// maximum link bandwidth. Returns 0 when the maximum bandwidth cannot be determined.
unsigned pcie_load_percent(unsigned value_kbs, const struct gpuinfo_static_info *static_info);

#endif // PCIE_UTILIZATION_H__
