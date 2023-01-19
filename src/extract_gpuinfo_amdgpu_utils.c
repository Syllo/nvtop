/*
 * Copyright © [2023] Advanced Micro Devices, Inc. All rights reserved.
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

#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>

#include "amdgpu_ids.h"

static const char * parse_one_line(uint32_t asic_id, uint32_t pci_rev_id, const char *line)
{
    char *buf, *saveptr;
    char *s_did;
    uint32_t did;
    char *s_rid;
    uint32_t rid;
    char *s_name;
    char *endptr;
    const char *rtn = NULL;

    /* ignore empty line and commented line */
    if (strlen(line) == 0 || line[0] == '#')
        return rtn;

    buf = strdup(line);
    if (!buf)
        return rtn;

    /* device id */
    s_did = strtok_r(buf, ",", &saveptr);
    if (!s_did)
        goto out;

    did = strtol(s_did, &endptr, 16);
    if (*endptr)
        goto out;

    if (did != asic_id) {
        goto out;
    }

    /* revision id */
    s_rid = strtok_r(NULL, ",", &saveptr);
    if (!s_rid)
        goto out;

    rid = strtol(s_rid, &endptr, 16);
    if (*endptr)
        goto out;

    if (rid != pci_rev_id) {
        goto out;
    }

    /* marketing name */
    s_name = strtok_r(NULL, ",", &saveptr);
    if (!s_name)
        goto out;

    /* trim leading whitespaces or tabs */
    while (isblank(*s_name)) s_name++;

    if (strlen(s_name) == 0) goto out;

    rtn = line + (s_name - buf);

out:
    free(buf);

    return rtn;
}

const char * amdgpu_parse_marketing_name(struct amdgpu_gpu_info *info)
{
    size_t len = 0;
    int line_num = 1;
    int i;
    int ntypes = sizeof(amdgpu_ids) / sizeof(amdgpu_ids[0]);
    const char *name = NULL;

    if (!info)
        return name;

    for (i = 0; i < ntypes; i++) {
        name = parse_one_line(info->asic_id, info->pci_rev_id, amdgpu_ids[i]);

        if (name)
            break;
    }

    return name;
}
