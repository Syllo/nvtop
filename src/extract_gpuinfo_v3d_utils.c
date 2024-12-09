/*
 *
 * Copyright (C) 2022 Hoream Xiao <horeamx@gmail.com>
 *
 * This file is part of Nvtop and adapted from the vcgencmd implementation.
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

#include "nvtop/extract_gpuinfo_common.h"
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <unistd.h>

/*
 * use ioctl to send mbox property message
 */
#define DEVICE_FILE_NAME "/dev/vcio"
#define MAJOR_NUM 100
#define IOCTL_MBOX_PROPERTY _IOWR(MAJOR_NUM, 0, char *)
#define MAX_STRING 1024
#define GET_GENCMD_RESULT 0x00030080
#define MAX_DECODER_FREQUENCE 550006336

int mbox_open(void);
void mbox_close(int mb);
void set_debug_files(int card_id);
void set_gpuinfo_from_vcio(struct gpuinfo_dynamic_info *dynamic_info, int mb);
void set_memory_gpuinfo(struct gpuinfo_dynamic_info *dynamic_info);
void set_init_max_memory(int mb);

static uint64_t max_gpu_memory_bytes = 128 << 20;

static const char measure_temp[] = "measure_temp";
static const char measure_clock_v3d[] = "measure_clock v3d";
static const char measure_clock_h264[] = "measure_clock h264";
static const char get_mem_gpu[] = "get_mem gpu";

static char bo_stats_file[50];

void set_debug_files(int card_id) {
  snprintf(bo_stats_file, sizeof(bo_stats_file), "/sys/kernel/debug/dri/%d/bo_stats", card_id);
  if (access(bo_stats_file, F_OK))
    printf("%s is not available.\n", bo_stats_file);
}

static int mbox_property(int mb, void *buf) {
  int ret_val = ioctl(mb, IOCTL_MBOX_PROPERTY, buf);

  if (ret_val < 0) {
    printf("ioctl_set_msg failed:%d\n", ret_val);
  }
  return ret_val;
}

int mbox_open(void) {
  int mb;

  // open a char device file used for communicating with kernel mbox driver
  mb = open(DEVICE_FILE_NAME, 0);
  if (mb < 0) {
    printf("Can't open device file: %s\n", DEVICE_FILE_NAME);
    printf("Try creating a device file with: sudo mknod %s c %d 0\n", DEVICE_FILE_NAME, MAJOR_NUM);
  }
  return mb;
}

void mbox_close(int mb) { close(mb); }

static unsigned gencmd(int mb, const char *command, char *result, int result_len) {
  int i = 0;
  unsigned p[(MAX_STRING >> 2) + 7];
  int len = strlen(command);
  // maximum length for command or response
  if (len + 1 >= MAX_STRING) {
    fprintf(stderr, "gencmd length too long : %d\n", len);
    return -1;
  }
  p[i++] = 0;          // size
  p[i++] = 0x00000000; // process request

  p[i++] = GET_GENCMD_RESULT; // (the tag id)
  p[i++] = MAX_STRING;        // buffer_len
  p[i++] = 0;                 // request_len (set to response length)
  p[i++] = 0;                 // error response

  memcpy(p + i, command, len + 1);
  i += MAX_STRING >> 2;

  p[i++] = 0x00000000;  // end tag
  p[0] = i * sizeof *p; // actual size

  mbox_property(mb, p);
  result[0] = 0;

  size_t available_space = result_len - strlen(result) - 1;
  strncat(result, (const char *)(p + 6), available_space);

  return p[5];
}

void set_init_max_memory(int mb) {
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, get_mem_gpu, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "gpu=%luM", &max_gpu_memory_bytes) == 1) {
      max_gpu_memory_bytes <<= 20;
    }
  }
}

static unsigned cal_percentage_usage(unsigned usage, unsigned all) { return (unsigned)(100.0 * usage / all + 0.5); }

static void set_gpuinfo_decode(struct gpuinfo_dynamic_info *dynamic_info, int mb) {
  unsigned int decode_usage = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, measure_clock_h264, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "frequency(28)=%u", &decode_usage) == 1)
      // divide current frequency by max frequency; usage rate might not be accurate.
      SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, cal_percentage_usage(decode_usage, MAX_DECODER_FREQUENCE));
  }
}

static void set_gpuinfo_temp(struct gpuinfo_dynamic_info *dynamic_info, int mb) {
  float temperature = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, measure_temp, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "temp=%f'C", &temperature) == 1) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, (unsigned)temperature);
    }
  }
}

static void set_gpuinfo_clock(struct gpuinfo_dynamic_info *dynamic_info, int mb) {
  unsigned int clock = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, measure_clock_v3d, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "frequency(46)=%u", &clock) == 1) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, clock >> 20);
    }
  }
}

void set_gpuinfo_from_vcio(struct gpuinfo_dynamic_info *dynamic_info, int mb) {
  set_gpuinfo_temp(dynamic_info, mb);
  set_gpuinfo_clock(dynamic_info, mb);
  set_gpuinfo_decode(dynamic_info, mb);
}

void set_memory_gpuinfo(struct gpuinfo_dynamic_info *dynamic_info) {
  FILE *fp = fopen(bo_stats_file, "rb");
  if (fp == NULL) {
    return;
  }

  char line[256];
  uint64_t allocated_bo_size_kb = 0;

  while (fgets(line, sizeof(line), fp)) {
    if (sscanf(line, "allocated bo size (kb): %lu", &allocated_bo_size_kb) == 1) {
      break;
    }
  }

  fclose(fp);

  uint64_t allocated_bo_size_bytes = allocated_bo_size_kb << 10;

  SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, allocated_bo_size_bytes);
  if (allocated_bo_size_bytes >= max_gpu_memory_bytes)
    max_gpu_memory_bytes = allocated_bo_size_bytes;
  SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, max_gpu_memory_bytes);
  SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, cal_percentage_usage(allocated_bo_size_bytes, max_gpu_memory_bytes));
}
