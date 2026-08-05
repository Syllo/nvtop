/*
 *
 * Copyright (C) 2026 NVTOP contributors
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
 *
 */

#include "extract_gpuinfo_apple_utils.h"

#include <Foundation/Foundation.h>
#include <gtest/gtest.h>
#include <stdlib.h>

TEST(AppleDynamicInfo, ParsesPerformanceStatistics) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"PerformanceStatistics" : @{
        @"Device Utilization %" : @42,
        @"Alloc system memory" : @123456,
      },
    };
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_TRUE(sample.gpu_util_rate_valid);
    EXPECT_EQ(sample.gpu_util_rate, 42u);
    EXPECT_TRUE(sample.allocated_system_memory_valid);
    EXPECT_EQ(sample.allocated_system_memory, 123456u);
  }
}

TEST(AppleDynamicInfo, RejectsMissingOrMalformedStatistics) {
  @autoreleasepool {
    NSArray *invalid_properties = @[
      @{},
      @{@"PerformanceStatistics" : @42},
    ];
    struct gpuinfo_apple_performance_sample sample;

    for (NSDictionary *properties in invalid_properties)
      EXPECT_FALSE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)properties, &sample));
  }
}

TEST(AppleDynamicInfo, IgnoresMalformedValuesAndCapsUtilization) {
  @autoreleasepool {
    NSDictionary *malformed_values = @{
      @"PerformanceStatistics" : @{
        @"Device Utilization %" : @"not a number",
        @"Alloc system memory" : @(-1),
      },
    };
    NSDictionary *excessive_utilization = @{
      @"PerformanceStatistics" : @{@"Device Utilization %" : @125},
    };
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)malformed_values, &sample));
    EXPECT_FALSE(sample.gpu_util_rate_valid);
    EXPECT_FALSE(sample.allocated_system_memory_valid);

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)excessive_utilization, &sample));
    EXPECT_TRUE(sample.gpu_util_rate_valid);
    EXPECT_EQ(sample.gpu_util_rate, 100u);
  }
}

TEST(AppleProcessInfo, ParsesAndSumsAppUsage) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"IOUserClientCreator" : @"pid 42, test",
      @"AppUsage" : @[
        @{@"API" : @"Metal", @"accumulatedGPUTime" : @125},
        @{@"API" : @"GL/CL", @"accumulatedGPUTime" : @75},
        @{@"API" : @"Metal", @"accumulatedGPUTime" : @0},
      ],
    };
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_EQ(sample.pid, 42);
    EXPECT_TRUE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time, 200u);
  }
}

TEST(AppleProcessInfo, PreservesPidWhenAppUsageIsUnavailable) {
  @autoreleasepool {
    NSDictionary *missing_usage = @{@"IOUserClientCreator" : @"pid 7, test"};
    NSDictionary *empty_usage = @{@"IOUserClientCreator" : @"pid 8, test", @"AppUsage" : @[]};
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)missing_usage, &sample));
    EXPECT_EQ(sample.pid, 7);
    EXPECT_FALSE(sample.gpu_time_valid);

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)empty_usage, &sample));
    EXPECT_EQ(sample.pid, 8);
    EXPECT_FALSE(sample.gpu_time_valid);
  }
}

TEST(AppleProcessInfo, IgnoresMalformedAppUsageEntries) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"IOUserClientCreator" : @"pid 42, test",
      @"AppUsage" : @[
        @"not a dictionary",
        @{@"accumulatedGPUTime" : @"not a number"},
        @{@"accumulatedGPUTime" : @(-1)},
        @{@"accumulatedGPUTime" : @25},
      ],
    };
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_TRUE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time, 25u);
  }
}

TEST(AppleProcessInfo, RejectsMalformedCreator) {
  @autoreleasepool {
    NSArray *invalid_properties = @[
      @{},
      @{@"IOUserClientCreator" : @42},
      @{@"IOUserClientCreator" : @"test"},
      @{@"IOUserClientCreator" : @"pid -1, test"},
    ];
    struct gpuinfo_apple_process_sample sample;

    for (NSDictionary *properties in invalid_properties)
      EXPECT_FALSE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
  }
}

TEST(AppleProcessInfo, CalculatesGpuUsageFromCounterDelta) {
  unsigned gpu_usage;

  ASSERT_TRUE(gpuinfo_apple_calculate_gpu_usage(100, 350, 1000, &gpu_usage));
  EXPECT_EQ(gpu_usage, 25u);

  ASSERT_TRUE(gpuinfo_apple_calculate_gpu_usage(350, 350, 1000, &gpu_usage));
  EXPECT_EQ(gpu_usage, 0u);

  ASSERT_TRUE(gpuinfo_apple_calculate_gpu_usage(100, 1200, 1000, &gpu_usage));
  EXPECT_EQ(gpu_usage, 100u);
}

TEST(AppleProcessInfo, RejectsInvalidCounterDeltas) {
  unsigned gpu_usage;

  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_usage(350, 100, 1000, &gpu_usage));
  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_usage(100, 350, 0, &gpu_usage));
}

TEST(AppleProcessInfo, AggregatesClientsByPid) {
  struct gpu_info gpu_info = {};

  gpuinfo_apple_add_process(&gpu_info, 42, true, 25);
  gpuinfo_apple_add_process(&gpu_info, 42, true, 35);
  gpuinfo_apple_add_process(&gpu_info, 7, false, 0);

  ASSERT_EQ(gpu_info.processes_count, 2u);
  EXPECT_EQ(gpu_info.processes[0].pid, 42);
  EXPECT_EQ(gpu_info.processes[0].type, gpu_process_graphical_compute);
  EXPECT_TRUE(GPUINFO_PROCESS_FIELD_VALID(&gpu_info.processes[0], gpu_usage));
  EXPECT_EQ(gpu_info.processes[0].gpu_usage, 60u);
  EXPECT_EQ(gpu_info.processes[1].pid, 7);
  EXPECT_FALSE(GPUINFO_PROCESS_FIELD_VALID(&gpu_info.processes[1], gpu_usage));

  free(gpu_info.processes);
}

TEST(AppleProcessInfo, CapsAggregatedUsageAtOneHundredPercent) {
  struct gpu_info gpu_info = {};

  gpuinfo_apple_add_process(&gpu_info, 42, true, 75);
  gpuinfo_apple_add_process(&gpu_info, 42, true, 50);

  ASSERT_EQ(gpu_info.processes_count, 1u);
  EXPECT_EQ(gpu_info.processes[0].gpu_usage, 100u);

  free(gpu_info.processes);
}
