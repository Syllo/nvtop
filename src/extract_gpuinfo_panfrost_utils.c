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
	unsigned int (*nengines) (int32_t core_count, uint32_t core_features, uint32_t thread_features);
};

static uint32_t get_num_eng_g52(int core_count,
				uint32_t core_features,
				uint32_t thread_features)
{
  (void) core_count;
  (void) thread_features;
  return core_features & 0xF;
}

#define GPU_MODEL(_name, _id, _nengines)	\
{						\
	.name = #_name,				\
	.id = _id,				\
	.nengines = _nengines,			\
}

static const struct panfrost_model gpu_models[] = {
  GPU_MODEL(t600, 0x600, NULL),
  GPU_MODEL(t620, 0x620, NULL),
  GPU_MODEL(t720, 0x720, NULL),
  GPU_MODEL(t760, 0x750, NULL),
  GPU_MODEL(t820, 0x820, NULL),
  GPU_MODEL(t830, 0x830, NULL),
  GPU_MODEL(t860, 0x860, NULL),
  GPU_MODEL(t880, 0x880, NULL),
  GPU_MODEL(g71, 0x6000, NULL),
  GPU_MODEL(g72, 0x6001, NULL),
  GPU_MODEL(g51, 0x7000, NULL),
  GPU_MODEL(g76, 0x7001, NULL),
  GPU_MODEL(g52, 0x7002, get_num_eng_g52),
  GPU_MODEL(g31, 0x7003, NULL),
  GPU_MODEL(g57, 0x9001, NULL),
  GPU_MODEL(g57, 0x9003, NULL),
};

static inline int panfrost_model_cmp(unsigned int match, unsigned int id)
{
  if (match & 0xf000)
    match &= 0xf00f;
  return match - id;
}

const char * panfrost_parse_marketing_name(uint64_t gpu_id)
{
  for (unsigned i = 0; i < ARRAY_SIZE(gpu_models); i++) {
    if (!panfrost_model_cmp(gpu_id, gpu_models[i].id)) {
      return gpu_models[i].name;
    }
  }
  return NULL;
}

unsigned int get_number_engines(uint32_t gpu_id,
				int core_count,
				uint32_t core_features,
				uint32_t thread_features)
{
  for (unsigned i = 0; i < ARRAY_SIZE(gpu_models); i++) {
    if (!panfrost_model_cmp(gpu_id, gpu_models[i].id)) {
      if (gpu_models[i].nengines)
	return gpu_models[i].nengines(core_count, core_features, thread_features);
      else
	return 0;
    }
  }
  return 0;
}

unsigned int util_last_bit(unsigned int u)
{
#if defined(HAVE___BUILTIN_CLZ)
   return u == 0 ? 0 : 32 - __builtin_clz(u);
#elif defined(_MSC_VER) && (_M_IX86 || _M_ARM || _M_AMD64 || _M_IA64)
   unsigned long index;
   if (_BitScanReverse(&index, u))
      return index + 1;
   else
      return 0;
#else
   unsigned r = 0;
   while (u) {
      r++;
      u >>= 1;
   }
   return r;
#endif
}
