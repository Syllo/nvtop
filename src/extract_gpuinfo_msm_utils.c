/*
 *
 * Copyright (C) 2023 Ryan Houdek <Sonicadvance1@gmail.com>
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

#include "nvtop/interface_internal_common.h"
#include <stdint.h>

struct msm_id_struct {
  uint64_t id;
  const char *name;
};

#define GetHundredDigit(coreid) (coreid / 100)
#define GetHundred(coreid) (GetHundredDigit(coreid) * 100)
#define GetTenDigit(coreid) ((coreid - GetHundred(coreid)) / 10)
#define GetTen(coreid) (GetTenDigit(coreid) * 10)
#define GetOneDigit(coreid) (coreid - (GetHundred(coreid) + GetTen(coreid)))

#define CHIPID(coreid) \
  (GetHundredDigit(coreid) << 24) | \
  (GetTenDigit(coreid) << 16) | \
  (GetOneDigit(coreid) << 8)

static const struct msm_id_struct msm_ids[] = {
  // Adreno 2xx
  {CHIPID(200),   "Adreno 200"},
  {CHIPID(201),   "Adreno 201"},
  {CHIPID(205),   "Adreno 205"},
  {CHIPID(220),   "Adreno 220"},

  // Adreno 3xx
  {CHIPID(305),   "Adreno 305"},
  {CHIPID(307),   "Adreno 307"},
  {CHIPID(320),   "Adreno 320"},
  {CHIPID(330),   "Adreno 330"},

  // Adreno 4xx
  {CHIPID(405),   "Adreno 405"},
  {CHIPID(420),   "Adreno 420"},
  {CHIPID(430),   "Adreno 430"},

  // Adreno 5xx
  {CHIPID(508),   "Adreno 508"},
  {CHIPID(509),   "Adreno 509"},
  {CHIPID(510),   "Adreno 510"},
  {CHIPID(512),   "Adreno 512"},
  {CHIPID(530),   "Adreno 530"},
  {CHIPID(540),   "Adreno 540"},

  // Adreno 6xx
  {CHIPID(615),    "Adreno 615"},
  {CHIPID(616),    "Adreno 616"},
  {CHIPID(618),    "Adreno 618"},
  {CHIPID(619),    "Adreno 619"},
  {CHIPID(620),    "Adreno 620"},
  {CHIPID(630),    "Adreno 630"},
  {CHIPID(640),    "Adreno 640"},
  // QCM6490
  {0x00ac06030500, "Adreno 643"},
  {CHIPID(650),    "Adreno 650"},
  {CHIPID(660),    "Adreno 660"},
  {CHIPID(680),    "Adreno 680"},
  {CHIPID(690),    "Adreno 690"},
  // no-speedbin Adreno 690
  {0xffff06090000, "Adreno 690"},

  // Adreno 7xx
  {CHIPID(730),   "Adreno 730"},
  {CHIPID(740),   "Adreno 740"},
  {CHIPID(750),   "Adreno 750"},
  {CHIPID(790),   "Adreno 750"},

  // Misc
  {0x00be06030500, "Adreno 8c Gen 3"},
  {0x007506030500, "Adreno 7c+ Gen 3"},
  {0x006006030500, "Adreno 7c+ Gen 3 Lite"},
  {0x000043051401, "Adreno 750"},
};

const char * msm_parse_marketing_name(uint64_t gpu_id);

const char * msm_parse_marketing_name(uint64_t gpu_id) {
  for (unsigned i = 0; i < ARRAY_SIZE(msm_ids); i++) {
    if (gpu_id == msm_ids[i].id) {
      return msm_ids[i].name;
    }
  }

  return NULL;
}
