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

#include <unistd.h>
#include "nvtop/interface_internal_common.h"
#include "panfrost_utils.h"

struct panfrost_model {
	const char *name;
	unsigned int id;
};

#define GPU_MODEL(_name, _id) \
{\
	.name = #_name,                                          \
	.id = _id,						\
}
static const struct panfrost_model gpu_models[] = {
	GPU_MODEL(t600, 0x600),
	GPU_MODEL(t620, 0x620),
	GPU_MODEL(t720, 0x720),
	GPU_MODEL(t760, 0x750),
	GPU_MODEL(t820, 0x820),
	GPU_MODEL(t830, 0x830),
	GPU_MODEL(t860, 0x860),
	GPU_MODEL(t880, 0x880),
	GPU_MODEL(g71, 0x6000),
	GPU_MODEL(g72, 0x6001),
	GPU_MODEL(g51, 0x7000),
	GPU_MODEL(g76, 0x7001),
	GPU_MODEL(g52, 0x7002),
	GPU_MODEL(g31, 0x7003),
	GPU_MODEL(g57, 0x9001),
	GPU_MODEL(g57, 0x9003),
};

static inline int panfrost_model_cmp(unsigned int match, unsigned int id)
{
	if (match & 0xf000)
		match &= 0xf00f;
	return match - id;
}

const char * panfrost_parse_marketing_name(uint64_t gpu_id) {
  for (unsigned i = 0; i < ARRAY_SIZE(gpu_models); i++) {
    if (!panfrost_model_cmp(gpu_id, gpu_models[i].id)) {
      return gpu_models[i].name;
    }
  }
  return NULL;
}
