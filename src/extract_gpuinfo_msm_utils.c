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
  uint64_t chip_id;
  const char *name;
};

static const struct msm_id_struct msm_ids[] = {
   { 0x20000ff, "FD200" },
   { 0x20001ff, "FD201" },
   { 0x20005ff, "FD205" },
   { 0x20200ff, "FD220" },
   { 0x30005ff, "FD305" },
   { 0x30007ff, "FD307" },
   { 0x30200ff, "FD320" },
   { 0x30300ff, "FD330" },
   { 0x3000512, "FD305B" },
   { 0x3000620, "FD306A" },
   { 0x40005ff, "FD405" },
   { 0x40200ff, "FD420" },
   { 0x40300ff, "FD430" },
   { 0x50005ff, "FD505" },
   { 0x50006ff, "FD506" },
   { 0x50008ff, "FD508" },
   { 0x50009ff, "FD509" },
   { 0x50100ff, "FD510" },
   { 0x50102ff, "FD512" },
   { 0x50300ff, "FD530" },
   { 0x50400ff, "FD540" },
   { 0x60005ff, "FD605" },
   { 0x60008ff, "FD608" },
   { 0x60100ff, "FD610" },
   { 0x60102ff, "FD612" },
   { 0x60105ff, "FD615" },
   { 0x60106ff, "FD616" },
   { 0x60108ff, "FD618" },
   { 0x60109ff, "FD619" },
   { 0x60200ff, "FD620" },
   { 0xffff06020100, "FD621" },
   { 0xffff06020300, "Adreno623" },
   { 0x60300ff, "FD630" },
   { 0x60400ff, "FD640" },
   { 0x60800ff, "FD680" },
   { 0x60500ff, "FD650" },
   { 0xbe06030500, "Adreno 8c Gen 3" },
   { 0x7506030500, "Adreno 7c+ Gen 3" },
   { 0x6006030500, "Adreno 7c+ Gen 3 Lite" },
   { 0xac06030500, "FD643" },
   { 0xffff06030500, "Adreno 7c+ Gen 3" },
   { 0x60600ff, "FD660" },
   { 0x6060201, "FD644" },
   { 0xffff06060300, "FD663" },
   { 0x60900ff, "FD690" },
   { 0xffff06090000, "FD690" },
   { 0x70002ff, "FD702" },
   { 0xb207002000, "FD702" },
   { 0xffff07002000, "FD702" },
   { 0x7030002, "FD725" },
   { 0xffff07030002, "FD725" },
   { 0x7030001, "FD730" },
   { 0xffff07030001, "FD730" },
   { 0xffff43030c00, "Adreno X1-45" },
   { 0x43030b00, "FD735" },
   { 0x70400ff, "FD740" },
   { 0x43050a01, "FD740" },
   { 0xffff43050a01, "FD740" },
   { 0xffff43050c01, "Adreno X1-85" },
   { 0x43050a00, "FDA32" },
   { 0xffff43050a00, "FDA32" },
   { 0x43050b00, "FD740v3" },
   { 0xffff43050b00, "FD740v3" },
   { 0x43051401, "FD750" },
   { 0xffff43051401, "FD750" },
   { 0x44050000, "Adreno (TM) 830" },
   { 0x44050001, "Adreno (TM) 830" },
   { 0xffff44050a31, "Adreno (TM) 840" },
   { 0xffff44070041, "Adreno (TM) X2-85" },
};

const char * msm_parse_marketing_name(uint64_t chip_id);

const char * msm_parse_marketing_name(uint64_t chip_id) {
  for (unsigned i = 0; i < ARRAY_SIZE(msm_ids); i++) {
    /* Reference entry from device table: */
    const struct msm_id_struct *ref = &msm_ids[i];

    /* Match on either:
     * (a) exact match:
     */
    if (ref->chip_id == chip_id)
        return ref->name;

    /* (b) device table entry has 0xff wildcard patch_id and core/
     *     major/minor match:
     */
    if (((ref->chip_id & 0xff) == 0xff) &&
         ((ref->chip_id & UINT64_C(0xffffff00)) ==
          (chip_id & UINT64_C(0xffffff00))))
      return ref->name;

  #define WILDCARD_FUSE_ID UINT64_C(0x0000ffff00000000)
    /* If the reference id has wildcard fuse-id value (ie. bits 47..32
     * are all ones, then try matching ignoring the device fuse-id:
     */
    if ((ref->chip_id & WILDCARD_FUSE_ID) == WILDCARD_FUSE_ID) {
      uint64_t new_chip_id = chip_id | WILDCARD_FUSE_ID;

      /* (c) exact match (ignoring the fuse-id from kernel):
       */
      if (ref->chip_id == new_chip_id)
        return ref->name;

      /* (d) device table entry has 0xff wildcard patch_id and core/
       *     major/minor match (ignoring fuse-id from kernel):
       */
      if (((ref->chip_id & 0xff) == 0xff) &&
           ((ref->chip_id & UINT64_C(0xffffff00)) ==
            (new_chip_id & UINT64_C(0xffffff00))))
        return ref->name;
    }
  }

  return NULL;
}
