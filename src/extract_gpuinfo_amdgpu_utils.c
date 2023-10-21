/*
 * Copyright (C) 2023 Advanced Micro Devices, Inc. All rights reserved.
 *
 * MIT License.
 */


#include <assert.h>
#include <ctype.h>
#include <dirent.h>
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <inttypes.h>

#include <libdrm/amdgpu.h>
#include <libdrm/amdgpu_drm.h>

#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>

#include "amdgpu_ids.h"

const char *amdgpu_parse_marketing_name(struct amdgpu_gpu_info *info);

const char * amdgpu_parse_marketing_name(struct amdgpu_gpu_info *info)
{
    int i;
    int ntypes = sizeof(amdgpu_ids) / sizeof(amdgpu_ids[0]);

    if (!info)
        return NULL;

    for (i = 0; i < ntypes; i++) {
        if (info->asic_id == amdgpu_ids[i].asic_id && info->pci_rev_id == amdgpu_ids[i].pci_rev_id) {
            return amdgpu_ids[i].name;
        }
    }

    return NULL;
}
