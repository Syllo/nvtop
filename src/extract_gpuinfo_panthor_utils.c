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
#include "panthor_utils.h"

/**
 * struct panthor_model - GPU model description
 */
struct panthor_model {
  /** @name: Model name. */
  const char *name;

  /** @arch_major: Major version number of architecture */
  uint8_t arch_major;

  /* @product_major: Major version number of product */
  uint8_t product_major;
};

/**
 * GPU_MODEL() - Define a GPU model. A GPU product can be uniquely identified
 * by a combination of the major architecture version and the major product
 * version.
 */
#define GPU_MODEL(_name, _arch_major, _product_major)	\
  {							\
    .name = #_name,					\
      .arch_major = _arch_major,			\
      .product_major = _product_major,			\
      }

static const struct panthor_model gpu_models[] = {
  GPU_MODEL(g610, 10, 7),
  GPU_MODEL(g310, 10, 4),
  {0},
};

const char * panthor_device_name(uint32_t gpu_id)
{
  uint32_t arch_major, product_major;
  const struct panthor_model *model;

  arch_major = (gpu_id >> 28) & 0xf;
  product_major = (gpu_id >> 16) & 0xf;

  for (model = gpu_models; model->name; model++) {
    if (model->arch_major == arch_major &&
	model->product_major == product_major)
      return model->name;
  }

  return NULL;
}
