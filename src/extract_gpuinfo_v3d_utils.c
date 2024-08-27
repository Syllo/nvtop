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
void set_gpuinfo_from_vcio(struct gpuinfo_dynamic_info *dynamic_info);
void set_mem_info(struct gpuinfo_dynamic_info *dynamic_info);
void set_sum_usage(struct gpuinfo_dynamic_info *dynamic_info);
void get_pid_usage(struct gpu_process *process_info);

static uint64_t last_timestamp = 0;
static uint64_t last_runtime = 0;
static uint64_t max_gpu_mem = 0;

static int mbox_property(int file_desc, void *buf) {
  int ret_val = ioctl(file_desc, IOCTL_MBOX_PROPERTY, buf);

  if (ret_val < 0) {
    printf("ioctl_set_msg failed:%d\n", ret_val);
  }
  return ret_val;
}

static int mbox_open(void) {
  int file_desc;

  // open a char device file used for communicating with kernel mbox driver
  file_desc = open(DEVICE_FILE_NAME, 0);
  if (file_desc < 0) {
    printf("Can't open device file: %s\n", DEVICE_FILE_NAME);
    printf("Try creating a device file with: sudo mknod %s c %d 0\n", DEVICE_FILE_NAME, MAJOR_NUM);
  }
  return file_desc;
}

static void mbox_close(int file_desc) { close(file_desc); }

static unsigned gencmd(int file_desc, const char *command, char *result, int result_len) {
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
  p[i++] = 0;                 // error repsonse

  memcpy(p + i, command, len + 1);
  i += MAX_STRING >> 2;

  p[i++] = 0x00000000;  // end tag
  p[0] = i * sizeof *p; // actual size

  mbox_property(file_desc, p);
  result[0] = 0;
  strncat(result, (const char *)(p + 6), result_len);

  return p[5];
}

static void set_gpuinfo_max_mem(int mb, const char *command) {
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, command, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "gpu=%luM", &max_gpu_mem) == 1) {
      max_gpu_mem <<= 20;
    }
  }
}

static void set_gpuinfo_decode(struct gpuinfo_dynamic_info *dynamic_info, int mb, const char *command) {
  unsigned int decode_usage = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, command, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "frequency(28)=%u", &decode_usage) == 1) {
      SET_GPUINFO_DYNAMIC(dynamic_info, decoder_rate, (unsigned)(100 * (decode_usage / 550006336.0)));
    }
  }
}

static void set_gpuinfo_temp(struct gpuinfo_dynamic_info *dynamic_info, int mb, const char *command) {
  float temperature = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, command, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "temp=%f'C", &temperature) == 1) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_temp, (unsigned)temperature);
    }
  }
}

static void set_gpuinfo_clock(struct gpuinfo_dynamic_info *dynamic_info, int mb, const char *command) {
  unsigned int clock = 0;
  char result[MAX_STRING] = {};

  int ret = gencmd(mb, command, result, sizeof result);
  if (!ret) {
    if (sscanf(result, "frequency(46)=%u", &clock) == 1) {
      SET_GPUINFO_DYNAMIC(dynamic_info, gpu_clock_speed, clock >> 20);
    }
  }
}

void set_gpuinfo_from_vcio(struct gpuinfo_dynamic_info *dynamic_info) {
  int mb = mbox_open();
  set_gpuinfo_temp(dynamic_info, mb, "measure_temp");
  set_gpuinfo_clock(dynamic_info, mb, "measure_clock v3d");
  set_gpuinfo_decode(dynamic_info, mb, "measure_clock h264");
  set_gpuinfo_max_mem(mb, "get_mem gpu");
  mbox_close(mb);
}

void set_mem_info(struct gpuinfo_dynamic_info *dynamic_info) {
  FILE *file = fopen("/sys/kernel/debug/dri/0/bo_stats", "r");
  if (file == NULL) {
    return;
  }

  char line[256];
  unsigned long allocated_bo_size_kb = 0;

  while (fgets(line, sizeof(line), file)) {
    if (sscanf(line, "allocated bo size (kb): %lu", &allocated_bo_size_kb) == 1) {
      break;
    }
  }

  fclose(file);

  unsigned long allocated_bo_size_bytes = allocated_bo_size_kb << 10;

  SET_GPUINFO_DYNAMIC(dynamic_info, used_memory, allocated_bo_size_bytes);
  if (allocated_bo_size_bytes >= max_gpu_mem) {
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, allocated_bo_size_bytes);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, 100);
  } else {
    SET_GPUINFO_DYNAMIC(dynamic_info, total_memory, max_gpu_mem);
    SET_GPUINFO_DYNAMIC(dynamic_info, mem_util_rate, (uint)(100.0 * allocated_bo_size_bytes / max_gpu_mem));
  }
}

void set_sum_usage(struct gpuinfo_dynamic_info *dynamic_info) {
  FILE *fp = fopen("/sys/kernel/debug/dri/0/gpu_usage", "rb");

  char *buf = NULL;
  size_t res = 0;
  unsigned long jobs, active;
  uint64_t timestamp, elapsed, runtime;

  while (getline(&buf, &res, fp) > 0) {
    if (sscanf(buf, "timestamp;%ld;", &timestamp) == 1) {
      elapsed = timestamp - last_timestamp;
      last_timestamp = timestamp;
    } else if (sscanf(strchr(buf, ';'), ";%ld;%ld;%ld;", &jobs, &runtime, &active) == 3) {
      if (!strncmp(buf, "v3d_ren", 7)) {
        int usage = busy_usage_from_time_usage_round(runtime, last_runtime, elapsed);
        last_runtime = runtime;
        SET_GPUINFO_DYNAMIC(dynamic_info, gpu_util_rate, usage);
        free(buf);
        fclose(fp);
        return;
      }
    }
  }

  free(buf);
  fclose(fp);
}

void get_pid_usage(struct gpu_process *process_info) {
  FILE *fp = fopen("/sys/kernel/debug/dri/0/gpu_pid_usage", "rb");

  char *buf = NULL;
  size_t res = 0;
  unsigned long jobs, active;
  pid_t pid;
  uint64_t runtime;
  uint64_t timestamp;

  while (getline(&buf, &res, fp) > 0) {
    if (sscanf(buf, "timestamp;%ld;", &timestamp) == 1) {
    } else if (sscanf(strchr(buf, ';'), ";%d;%ld;%ld;%ld;", &pid, &jobs, &runtime, &active) == 4) {
      if (!strncmp(buf, "v3d_ren", 7) && (pid == process_info->pid || pid == process_info->pid + 10)) {
        SET_GPUINFO_PROCESS(process_info, gfx_engine_used, runtime);
        free(buf);
        fclose(fp);
        return;
      }
    }
  }

  SET_GPUINFO_PROCESS(process_info, gfx_engine_used, 0);
  free(buf);
  fclose(fp);
  return;
}
