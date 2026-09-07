/*
 *
 * Copyright (C) 2026 Nvtop contributors
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
 */

#include <gtest/gtest.h>

extern "C" {
#include "list.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/remote_proto.h"
}

namespace {

LIST_HEAD(devices);

struct gpu_info *make_device(const char *pdev, struct gpu_process *procs, unsigned proc_count) {
  struct gpu_info *d = (struct gpu_info *)calloc(1, sizeof(*d));
  snprintf(d->pdev, PDEV_LEN, "%s", pdev);
  d->processes = procs;
  d->processes_count = proc_count;
  d->processes_array_size = proc_count;
  list_add_tail(&d->list, &devices);
  return d;
}

TEST(RemoteWire, RoundTrip) {
  // Build a fake device list with known values.
  INIT_LIST_HEAD(&devices);

  struct gpu_process procs[2];
  memset(procs, 0, sizeof(procs));
  procs[0].type = gpu_process_compute;
  procs[0].pid = 1234;
  procs[0].gpu_usage = 42;
  procs[0].gpu_memory_usage = 1024;
  procs[0].cmdline = (char *)"vllm serve";
  procs[0].user_name = (char *)"gpu_user";
  SET_VALID(gpuinfo_process_cmdline_valid, procs[0].valid);
  SET_VALID(gpuinfo_process_user_name_valid, procs[0].valid);
  SET_VALID(gpuinfo_process_gpu_usage_valid, procs[0].valid);
  SET_VALID(gpuinfo_process_gpu_memory_usage_valid, procs[0].valid);

  struct gpu_info *d0 = make_device("card0", procs, 2);
  struct gpu_info *d1 = make_device("card1", NULL, 0);

  struct gpuinfo_dynamic_info *dyn0 = &d0->dynamic_info;
  RESET_ALL(dyn0->valid);
  SET_GPUINFO_DYNAMIC(dyn0, gpu_clock_speed, 1500);
  SET_GPUINFO_DYNAMIC(dyn0, gpu_clock_speed_max, 2100);
  SET_GPUINFO_DYNAMIC(dyn0, gpu_util_rate, 87);
  SET_GPUINFO_DYNAMIC(dyn0, total_memory, 24ULL * 1024 * 1024 * 1024);
  SET_GPUINFO_DYNAMIC(dyn0, used_memory, 8ULL * 1024 * 1024 * 1024);
  SET_GPUINFO_DYNAMIC(dyn0, gpu_temp, 65);
  SET_GPUINFO_DYNAMIC(dyn0, power_draw, 25000);

  struct gpuinfo_static_info *st0 = &d0->static_info;
  RESET_ALL(st0->valid);
  snprintf(st0->device_name, sizeof(st0->device_name), "RTX 3090");
  SET_VALID(gpuinfo_device_name_valid, st0->valid);
  SET_GPUINFO_STATIC(st0, max_pcie_gen, 4);
  SET_GPUINFO_STATIC(st0, max_pcie_link_width, 16);

  struct gpuinfo_dynamic_info *dyn1 = &d1->dynamic_info;
  RESET_ALL(dyn1->valid);
  SET_GPUINFO_DYNAMIC(dyn1, gpu_util_rate, 12);
  SET_GPUINFO_DYNAMIC(dyn1, total_memory, 122ULL * 1024 * 1024 * 1024);

  // Encode
  uint8_t buf[1 << 16];
  size_t total = remote_wire_encode(&devices, buf, sizeof(buf));
  ASSERT_GT(total, 0u);
  ASSERT_LE(total, sizeof(buf));

  // Decode
  struct wire_static statics[1] = {{0}};
  struct wire_gpu gpus[2] = {{0}};
  struct wire_process procs_out[64] = {{0}};
  unsigned n = remote_wire_decode(buf, total, statics, gpus, procs_out);

  EXPECT_EQ(n, 2u);

  // Header checks
  EXPECT_EQ(gpus[0].gpu_clock_speed, 1500u);
  EXPECT_EQ(gpus[0].gpu_clock_speed_max, 2100u);
  EXPECT_EQ(gpus[0].gpu_util_rate, 87u);
  EXPECT_EQ(gpus[0].total_memory, 24ULL * 1024 * 1024 * 1024);
  EXPECT_EQ(gpus[0].used_memory, 8ULL * 1024 * 1024 * 1024);
  EXPECT_EQ(gpus[0].gpu_temp, 65u);
  EXPECT_EQ(gpus[0].power_draw, 25000u);
  EXPECT_EQ(gpus[0].process_count, 2u);
  EXPECT_STREQ(procs_out[0].cmdline, "vllm serve");
  EXPECT_STREQ(procs_out[0].user_name, "gpu_user");
  EXPECT_EQ(procs_out[0].gpu_usage, 42u);
  EXPECT_EQ(gpus[1].gpu_util_rate, 12u);
  EXPECT_EQ(gpus[1].total_memory, 122ULL * 1024 * 1024 * 1024);

  // valid[] bits forwarded verbatim
  EXPECT_TRUE(IS_VALID(gpuinfo_gpu_util_rate_valid, gpus[0].valid));
  EXPECT_TRUE(IS_VALID(gpuinfo_gpu_clock_speed_valid, gpus[0].valid));
  EXPECT_TRUE(IS_VALID(gpuinfo_gpu_temp_valid, gpus[0].valid));
  EXPECT_TRUE(IS_VALID(gpuinfo_process_cmdline_valid, procs_out[0].valid));

  // Static block round-trips the device name.
  EXPECT_STREQ(statics[0].device_name, "RTX 3090");
  EXPECT_EQ(statics[0].max_pcie_gen, 4u);
  EXPECT_EQ(statics[0].max_pcie_link_width, 16u);
  EXPECT_TRUE(IS_VALID(gpuinfo_max_pcie_gen_valid, statics[0].valid));
}

TEST(RemoteWire, TruncatedBufferReturnsError) {
  INIT_LIST_HEAD(&devices);
  struct gpu_info *d0 = make_device("card0", NULL, 0);
  struct gpuinfo_dynamic_info *dyn = &d0->dynamic_info;
  RESET_ALL(dyn->valid);
  SET_GPUINFO_DYNAMIC(dyn, gpu_util_rate, 55);

  uint8_t buf[1 << 16];
  size_t total = remote_wire_encode(&devices, buf, sizeof(buf));
  ASSERT_GT(total, 0u);

  // Feeding a frame smaller than the declared length must fail.
  struct wire_static s[1] = {{0}};
  struct wire_gpu g[2] = {{0}};
  struct wire_process p[64] = {{0}};
  EXPECT_EQ(remote_wire_decode(buf, total / 2, s, g, p), 0u);

  // A frame with the wrong magic is rejected.
  uint8_t bad[1 << 16];
  memcpy(bad, buf, total);
  bad[4] = 'X'; // clobber the magic
  EXPECT_EQ(remote_wire_decode(bad, total, s, g, p), 0u);
}

} // namespace
