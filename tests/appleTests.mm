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
#include "get_process_info_mac_utils.h"
#include "nvtop/common.h"

#include <Foundation/Foundation.h>
#include <gtest/gtest.h>
#include <limits.h>
#include <math.h>
#include <stdlib.h>

TEST(AppleProcessCpuInfo, ConvertsNativeMachTicksToSeconds) {
  EXPECT_DOUBLE_EQ(processinfo_mac_time_to_seconds(1000000000, false, 1, 1), 1.0);
  EXPECT_DOUBLE_EQ(processinfo_mac_time_to_seconds(24000000, false, 125, 3), 1.0);
  // A zero denominator cannot be interpreted; report no duration instead of inf/NaN.
  EXPECT_DOUBLE_EQ(processinfo_mac_time_to_seconds(1000, false, 1, 0), 0.0);
}

TEST(AppleProcessCpuInfo, UsesNativeTimebaseUnderRosetta) {
  EXPECT_DOUBLE_EQ(processinfo_mac_time_to_seconds(24000000, true, 1, 1), 1.0);
}

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

TEST(AppleDynamicInfo, DoesNotSubstituteInUseMemoryCounters) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"PerformanceStatistics" : @{
        @"In use system memory" : @654321,
        @"In use system memory (driver)" : @111111,
      },
    };
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_FALSE(sample.allocated_system_memory_valid);
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

TEST(AppleDynamicInfo, ParsesDiscreteAmdStatistics) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"PerformanceStatistics" : @{
        @"Device Utilization %" : @0,
        @"GPU Activity(%)" : @37,
        @"inUseVidMemoryBytes" : @8589934592ULL,
        @"Temperature(C)" : @61.4,
        @"Total Power(W)" : @43.6,
        @"Core Clock(MHz)" : @2291,
        @"Memory Clock(MHz)" : @1984,
        @"Fan Speed(%)" : @42,
      },
    };
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_TRUE(sample.gpu_util_rate_valid);
    EXPECT_EQ(sample.gpu_util_rate, 0u);
    EXPECT_TRUE(sample.gpu_activity_rate_valid);
    EXPECT_EQ(sample.gpu_activity_rate, 37u);
    EXPECT_TRUE(sample.used_memory_valid);
    EXPECT_EQ(sample.used_memory, UINT64_C(8589934592));
    EXPECT_TRUE(sample.gpu_temp_valid);
    EXPECT_EQ(sample.gpu_temp, 61u);
    EXPECT_TRUE(sample.power_draw_valid);
    EXPECT_EQ(sample.power_draw_mw, 43600u);
    EXPECT_TRUE(sample.gpu_clock_speed_valid);
    EXPECT_EQ(sample.gpu_clock_speed, 2291u);
    EXPECT_TRUE(sample.mem_clock_speed_valid);
    EXPECT_EQ(sample.mem_clock_speed, 1984u);
    EXPECT_TRUE(sample.fan_speed_valid);
    EXPECT_EQ(sample.fan_speed, 42u);
  }
}

TEST(AppleDynamicInfo, FallsBackToVramUsedBytes) {
  @autoreleasepool {
    NSDictionary *fallback = @{@"PerformanceStatistics" : @{@"vramUsedBytes" : @123456789ULL}};
    NSDictionary *preferred =
        @{@"PerformanceStatistics" : @{@"inUseVidMemoryBytes" : @111ULL, @"vramUsedBytes" : @222ULL}};
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)fallback, &sample));
    EXPECT_TRUE(sample.used_memory_valid);
    EXPECT_EQ(sample.used_memory, UINT64_C(123456789));

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)preferred, &sample));
    EXPECT_TRUE(sample.used_memory_valid);
    EXPECT_EQ(sample.used_memory, UINT64_C(111));
  }
}

TEST(AppleDynamicInfo, RejectsImplausibleDiscreteAmdValues) {
  @autoreleasepool {
    NSDictionary *negative_values = @{
      @"PerformanceStatistics" : @{
        @"Temperature(C)" : @(-12.5),
        @"Total Power(W)" : @(-1.0),
        @"Fan Speed(%)" : @250,
      },
    };
    NSDictionary *out_of_range_values = @{
      @"PerformanceStatistics" : @{
        @"Temperature(C)" : @260.0,
        @"Total Power(W)" : @1200.0,
      },
    };
    NSDictionary *zero_temperature = @{
      @"PerformanceStatistics" : @{
        @"Temperature(C)" : @0.0,
      },
    };
    struct gpuinfo_apple_performance_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)negative_values, &sample));
    EXPECT_FALSE(sample.gpu_temp_valid);
    EXPECT_FALSE(sample.power_draw_valid);
    EXPECT_TRUE(sample.fan_speed_valid);
    EXPECT_EQ(sample.fan_speed, 100u);

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)out_of_range_values, &sample));
    EXPECT_FALSE(sample.gpu_temp_valid);
    EXPECT_FALSE(sample.power_draw_valid);

    ASSERT_TRUE(gpuinfo_apple_parse_performance_sample((__bridge CFDictionaryRef)zero_temperature, &sample));
    EXPECT_FALSE(sample.gpu_temp_valid);
  }
}

TEST(ApplePowerInfo, ConvertsEnergyUnitsToNanojoules) {
  uint64_t energy;

  ASSERT_TRUE(gpuinfo_apple_energy_to_nanojoules(42, "nJ", &energy));
  EXPECT_EQ(energy, 42u);
  ASSERT_TRUE(gpuinfo_apple_energy_to_nanojoules(42, "uJ", &energy));
  EXPECT_EQ(energy, 42000u);
  ASSERT_TRUE(gpuinfo_apple_energy_to_nanojoules(42, "mJ", &energy));
  EXPECT_EQ(energy, 42000000u);
  ASSERT_TRUE(gpuinfo_apple_energy_to_nanojoules(42, "J", &energy));
  EXPECT_EQ(energy, 42000000000u);
}

TEST(ApplePowerInfo, RejectsInvalidEnergyValues) {
  uint64_t energy;

  EXPECT_FALSE(gpuinfo_apple_energy_to_nanojoules(-1, "nJ", &energy));
  EXPECT_FALSE(gpuinfo_apple_energy_to_nanojoules(1, "watts", &energy));
  EXPECT_FALSE(gpuinfo_apple_energy_to_nanojoules(INT64_MAX, "J", &energy));
  EXPECT_FALSE(gpuinfo_apple_energy_to_nanojoules(1, NULL, &energy));
  EXPECT_FALSE(gpuinfo_apple_energy_to_nanojoules(1, "nJ", NULL));
}

TEST(ApplePowerInfo, CalculatesPowerDrawFromEnergyDelta) {
  unsigned power_draw;

  ASSERT_TRUE(gpuinfo_apple_calculate_power_draw(5000000, 1000000000, &power_draw));
  EXPECT_EQ(power_draw, 5u);
  ASSERT_TRUE(gpuinfo_apple_calculate_power_draw(1500000, 1000000000, &power_draw));
  EXPECT_EQ(power_draw, 2u);
  ASSERT_TRUE(gpuinfo_apple_calculate_power_draw(0, 1000000000, &power_draw));
  EXPECT_EQ(power_draw, 0u);
}

TEST(ApplePowerInfo, RejectsInvalidPowerSamples) {
  unsigned power_draw;

  EXPECT_FALSE(gpuinfo_apple_calculate_power_draw(1, 0, &power_draw));
  EXPECT_FALSE(gpuinfo_apple_calculate_power_draw(UINT64_MAX, 1, &power_draw));
  EXPECT_FALSE(gpuinfo_apple_calculate_power_draw(1, 1, NULL));
}

TEST(AppleClockInfo, ParsesGpuFrequencyStates) {
  const uint8_t voltage_states[] = {
      0x00, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00,
      0x00, 0x65, 0xcd, 0x1d, 0xbc, 0x02, 0x00, 0x00,
      0x00, 0xca, 0x9a, 0x3b, 0x84, 0x03, 0x00, 0x00,
      0x00, 0x2f, 0x68, 0x59, 0x4c, 0x04, 0x00, 0x00,
  };
  unsigned frequencies[4];
  size_t frequency_count;

  ASSERT_TRUE(gpuinfo_apple_parse_gpu_frequency_states(
      voltage_states, sizeof(voltage_states), frequencies, 4, &frequency_count));
  ASSERT_EQ(frequency_count, 4u);
  EXPECT_EQ(frequencies[0], 0u);
  EXPECT_EQ(frequencies[1], 500u);
  EXPECT_EQ(frequencies[2], 1000u);
  EXPECT_EQ(frequencies[3], 1500u);
}

TEST(AppleClockInfo, ParsesKilohertzGpuFrequencyStates) {
  const uint8_t voltage_states[] = {
      0x00, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00,
      0x20, 0xa1, 0x07, 0x00, 0xbc, 0x02, 0x00, 0x00,
      0x40, 0x42, 0x0f, 0x00, 0x84, 0x03, 0x00, 0x00,
      0x60, 0xe3, 0x16, 0x00, 0x4c, 0x04, 0x00, 0x00,
  };
  unsigned frequencies[4];
  size_t frequency_count;

  ASSERT_TRUE(gpuinfo_apple_parse_gpu_frequency_states(
      voltage_states, sizeof(voltage_states), frequencies, 4, &frequency_count));
  ASSERT_EQ(frequency_count, 4u);
  EXPECT_EQ(frequencies[0], 0u);
  EXPECT_EQ(frequencies[1], 500u);
  EXPECT_EQ(frequencies[2], 1000u);
  EXPECT_EQ(frequencies[3], 1500u);
}

TEST(AppleClockInfo, RejectsGpuFrequencyStatesWithoutPlausibleUnits) {
  // Values in the low thousands fit neither a Hz nor a kHz table, so report no
  // clock instead of one that is off by orders of magnitude.
  const uint8_t megahertz_states[] = {
      0x00, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00,
      0xdc, 0x05, 0x00, 0x00, 0xbc, 0x02, 0x00, 0x00,
  };
  const uint8_t zero_active_state[] = {
      0x00, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x00, 0xbc, 0x02, 0x00, 0x00,
  };
  unsigned frequencies[2];
  size_t frequency_count;

  EXPECT_FALSE(gpuinfo_apple_parse_gpu_frequency_states(
      megahertz_states, sizeof(megahertz_states), frequencies, 2, &frequency_count));
  EXPECT_FALSE(gpuinfo_apple_parse_gpu_frequency_states(
      zero_active_state, sizeof(zero_active_state), frequencies, 2, &frequency_count));
}

TEST(AppleClockInfo, RejectsMalformedGpuFrequencyStates) {
  const uint8_t voltage_states[] = {
      0x00, 0x00, 0x00, 0x00, 0x7d, 0x00, 0x00, 0x00,
      0x00, 0x65, 0xcd, 0x1d, 0xbc, 0x02, 0x00, 0x00,
  };
  unsigned frequencies[2];
  size_t frequency_count;

  EXPECT_FALSE(gpuinfo_apple_parse_gpu_frequency_states(
      voltage_states, sizeof(voltage_states) - 1, frequencies, 2, &frequency_count));
  EXPECT_FALSE(gpuinfo_apple_parse_gpu_frequency_states(
      voltage_states, sizeof(voltage_states), frequencies, 1, &frequency_count));
  EXPECT_FALSE(gpuinfo_apple_parse_gpu_frequency_states(
      voltage_states, sizeof(voltage_states), frequencies, 2, NULL));
}

TEST(AppleClockInfo, CalculatesActiveResidencyWeightedClockSpeed) {
  const unsigned frequencies[] = {0, 500, 1000, 1500};
  const uint64_t residencies[] = {1000, 100, 300, 600};
  unsigned clock_speed;

  ASSERT_TRUE(gpuinfo_apple_calculate_gpu_clock_speed(
      residencies, frequencies, 4, &clock_speed));
  EXPECT_EQ(clock_speed, 1250u);
}

TEST(AppleClockInfo, ReportsZeroWhenGpuRemainsOff) {
  const unsigned frequencies[] = {0, 500, 1000};
  const uint64_t residencies[] = {1000, 0, 0};
  unsigned clock_speed;

  ASSERT_TRUE(gpuinfo_apple_calculate_gpu_clock_speed(
      residencies, frequencies, 3, &clock_speed));
  EXPECT_EQ(clock_speed, 0u);
}

TEST(AppleClockInfo, RejectsInvalidResidencyTables) {
  const unsigned frequencies[] = {0, 500};
  const uint64_t residencies[] = {1000, 100};
  unsigned clock_speed;

  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_clock_speed(NULL, frequencies, 2, &clock_speed));
  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_clock_speed(residencies, NULL, 2, &clock_speed));
  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_clock_speed(residencies, frequencies, 1, &clock_speed));
  EXPECT_FALSE(gpuinfo_apple_calculate_gpu_clock_speed(residencies, frequencies, 2, NULL));
}

TEST(AppleClockInfo, AccumulatesActiveChannelResidencies) {
  uint64_t residencies[] = {0, 10, 20, 30};
  const int64_t channel[] = {12345, 1, 2, 3};

  ASSERT_TRUE(gpuinfo_apple_accumulate_gpu_residencies(residencies, channel, 4));
  // The zero-frequency OFF state is never folded into the aggregate.
  EXPECT_EQ(residencies[0], 0u);
  EXPECT_EQ(residencies[1], 11u);
  EXPECT_EQ(residencies[2], 22u);
  EXPECT_EQ(residencies[3], 33u);
}

TEST(AppleClockInfo, RejectsInvalidChannelResidenciesAtomically) {
  uint64_t residencies[] = {0, 10, 20};
  const int64_t negative[] = {0, 1, -1};
  const int64_t overflowing[] = {0, 0, 1};

  EXPECT_FALSE(gpuinfo_apple_accumulate_gpu_residencies(residencies, negative, 3));
  EXPECT_EQ(residencies[1], 10u);
  EXPECT_EQ(residencies[2], 20u);

  uint64_t saturated[] = {0, 0, UINT64_MAX};
  EXPECT_FALSE(gpuinfo_apple_accumulate_gpu_residencies(saturated, overflowing, 3));
  EXPECT_EQ(saturated[1], 0u);
  EXPECT_EQ(saturated[2], UINT64_MAX);
}

TEST(AppleClockInfo, RejectsInvalidResidencyInputs) {
  uint64_t residencies[2] = {0, 0};
  const int64_t channel[2] = {0, 1};

  EXPECT_FALSE(gpuinfo_apple_accumulate_gpu_residencies(NULL, channel, 2));
  EXPECT_FALSE(gpuinfo_apple_accumulate_gpu_residencies(residencies, NULL, 2));
  EXPECT_FALSE(gpuinfo_apple_accumulate_gpu_residencies(residencies, channel, 1));
}

TEST(AppleSamplingInfo, SamplesImmediatelyAndThenRespectsTheFloor) {
  const uint64_t floor = 1000000000;

  EXPECT_TRUE(gpuinfo_apple_sample_is_due(false, 0, floor));
  EXPECT_FALSE(gpuinfo_apple_sample_is_due(true, 0, floor));
  EXPECT_FALSE(gpuinfo_apple_sample_is_due(true, floor - 1, floor));
  // A refresh landing exactly on the interval must sample, otherwise a 1 s update
  // interval could round down and halve the effective sampling rate.
  EXPECT_TRUE(gpuinfo_apple_sample_is_due(true, floor, floor));
  EXPECT_TRUE(gpuinfo_apple_sample_is_due(true, floor + 1, floor));
}

TEST(AppleTemperatureInfo, DecodesSmcFloat) {
  const uint8_t encoded_temperature[] = {0x00, 0x00, 0x43, 0x42};
  float temperature;

  ASSERT_TRUE(gpuinfo_apple_decode_smc_float(encoded_temperature, sizeof(encoded_temperature), &temperature));
  EXPECT_FLOAT_EQ(temperature, 48.75f);
}

TEST(AppleTemperatureInfo, RejectsMalformedSmcFloat) {
  const uint8_t encoded_temperature[] = {0x00, 0x00, 0x43, 0x42};
  float temperature;

  EXPECT_FALSE(gpuinfo_apple_decode_smc_float(NULL, sizeof(encoded_temperature), &temperature));
  EXPECT_FALSE(gpuinfo_apple_decode_smc_float(encoded_temperature, sizeof(encoded_temperature) - 1, &temperature));
  EXPECT_FALSE(gpuinfo_apple_decode_smc_float(encoded_temperature, sizeof(encoded_temperature), NULL));
}

TEST(AppleTemperatureInfo, AveragesValidGpuSensors) {
  const float temperatures[] = {46.25f, 47.25f, 0.0f, -1.0f, 151.0f, NAN, INFINITY};
  unsigned average_temperature;

  ASSERT_TRUE(gpuinfo_apple_average_temperatures(
      temperatures, sizeof(temperatures) / sizeof(temperatures[0]), &average_temperature));
  EXPECT_EQ(average_temperature, 47u);
}

TEST(AppleTemperatureInfo, RejectsMissingValidGpuSensors) {
  const float invalid_temperatures[] = {0.0f, -1.0f, 151.0f, NAN, INFINITY};
  unsigned average_temperature;

  EXPECT_FALSE(gpuinfo_apple_average_temperatures(
      invalid_temperatures, sizeof(invalid_temperatures) / sizeof(invalid_temperatures[0]), &average_temperature));
  EXPECT_FALSE(gpuinfo_apple_average_temperatures(NULL, 1, &average_temperature));
  EXPECT_FALSE(gpuinfo_apple_average_temperatures(invalid_temperatures, 0, &average_temperature));
  EXPECT_FALSE(gpuinfo_apple_average_temperatures(invalid_temperatures, 1, NULL));
}

TEST(AppleFanInfo, SelectsHighestValidFanSpeed) {
  const float fan_speeds[] = {1200.25f, 1350.75f, 0.0f, -1.0f, NAN, INFINITY};
  unsigned fan_rpm;

  ASSERT_TRUE(gpuinfo_apple_max_fan_rpm(fan_speeds, sizeof(fan_speeds) / sizeof(fan_speeds[0]), &fan_rpm));
  EXPECT_EQ(fan_rpm, 1351u);
}

TEST(AppleFanInfo, AcceptsStoppedFans) {
  const float fan_speeds[] = {0.0f, -1.0f, NAN};
  unsigned fan_rpm;

  ASSERT_TRUE(gpuinfo_apple_max_fan_rpm(fan_speeds, sizeof(fan_speeds) / sizeof(fan_speeds[0]), &fan_rpm));
  EXPECT_EQ(fan_rpm, 0u);
}

TEST(AppleFanInfo, RejectsMissingValidFanSpeeds) {
  const float invalid_fan_speeds[] = {-1.0f, 100001.0f, NAN, INFINITY};
  unsigned fan_rpm;

  EXPECT_FALSE(gpuinfo_apple_max_fan_rpm(
      invalid_fan_speeds, sizeof(invalid_fan_speeds) / sizeof(invalid_fan_speeds[0]), &fan_rpm));
  EXPECT_FALSE(gpuinfo_apple_max_fan_rpm(NULL, 1, &fan_rpm));
  EXPECT_FALSE(gpuinfo_apple_max_fan_rpm(invalid_fan_speeds, 0, &fan_rpm));
  EXPECT_FALSE(gpuinfo_apple_max_fan_rpm(invalid_fan_speeds, 1, NULL));
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
    EXPECT_EQ(sample.gpu_time_queue_count, 3u);
  }
}

TEST(AppleProcessInfo, ParsesDirectAccumulatedGpuTime) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"IOUserClientCreator" : @"pid 42, test",
      @"accumulatedGPUTime" : @125,
    };
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_EQ(sample.pid, 42);
    EXPECT_TRUE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time, 125u);
    // A direct total has no per-queue breakdown.
    EXPECT_EQ(sample.gpu_time_queue_count, 0u);
  }
}

TEST(AppleProcessInfo, PrefersDirectAccumulatedGpuTimeOverAppUsage) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"IOUserClientCreator" : @"pid 42, test",
      @"accumulatedGPUTime" : @500,
      @"AppUsage" : @[
        @{@"accumulatedGPUTime" : @125},
        @{@"accumulatedGPUTime" : @75},
      ],
    };
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
    // Counting both shapes would report the same GPU work twice.
    EXPECT_TRUE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time, 500u);
    EXPECT_EQ(sample.gpu_time_queue_count, 0u);
  }
}

TEST(AppleProcessInfo, FallsBackToAppUsageWhenDirectCounterIsMalformed) {
  @autoreleasepool {
    NSArray *direct_values = @[ @"not a number", @(-1) ];
    struct gpuinfo_apple_process_sample sample;

    for (id direct_value in direct_values) {
      NSDictionary *properties = @{
        @"IOUserClientCreator" : @"pid 42, test",
        @"accumulatedGPUTime" : direct_value,
        @"AppUsage" : @[
          @{@"accumulatedGPUTime" : @125},
          @{@"accumulatedGPUTime" : @75},
        ],
      };

      ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
      EXPECT_TRUE(sample.gpu_time_valid);
      EXPECT_EQ(sample.gpu_time, 200u);
      EXPECT_EQ(sample.gpu_time_queue_count, 2u);
    }
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
    EXPECT_EQ(sample.gpu_time_queue_count, 0u);

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)empty_usage, &sample));
    EXPECT_EQ(sample.pid, 8);
    EXPECT_FALSE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time_queue_count, 0u);
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
    EXPECT_EQ(sample.gpu_time_queue_count, 1u);
  }
}

TEST(AppleProcessInfo, ReportsNoQueueWhenAppUsageIsEntirelyMalformed) {
  @autoreleasepool {
    NSDictionary *properties = @{
      @"IOUserClientCreator" : @"pid 42, test",
      @"AppUsage" : @[
        @"not a dictionary",
        @{@"API" : @"Metal"},
        @{@"accumulatedGPUTime" : @(-1)},
      ],
    };
    struct gpuinfo_apple_process_sample sample;

    ASSERT_TRUE(gpuinfo_apple_parse_process_sample((__bridge CFDictionaryRef)properties, &sample));
    EXPECT_EQ(sample.pid, 42);
    EXPECT_FALSE(sample.gpu_time_valid);
    EXPECT_EQ(sample.gpu_time, 0u);
    EXPECT_EQ(sample.gpu_time_queue_count, 0u);
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

TEST(AppleProcessInfo, GrowsProcessArrayAcrossReallocIncrements) {
  struct gpu_info gpu_info = {};

  // One more than a single linear increment forces at least two reallocations.
  const unsigned process_count = COMMON_PROCESS_LINEAR_REALLOC_INC + 1;
  for (unsigned i = 1; i <= process_count; ++i)
    gpuinfo_apple_add_process(&gpu_info, (pid_t)i, true, 1);

  ASSERT_EQ(gpu_info.processes_count, process_count);
  EXPECT_GE(gpu_info.processes_array_size, process_count);
  EXPECT_EQ(gpu_info.processes[process_count - 1].pid, (pid_t)process_count);
  EXPECT_TRUE(GPUINFO_PROCESS_FIELD_VALID(&gpu_info.processes[process_count - 1], gpu_usage));
  EXPECT_EQ(gpu_info.processes[process_count - 1].gpu_usage, 1u);

  free(gpu_info.processes);
}

TEST(ApplePciInfo, ParsesPciDebugAddress) {
  unsigned bus = 0;
  unsigned device = 0;
  unsigned function = 0;

  EXPECT_TRUE(gpuinfo_apple_parse_pci_bdf("194:0:0", &bus, &device, &function));
  EXPECT_EQ(bus, 194u);
  EXPECT_EQ(device, 0u);
  EXPECT_EQ(function, 0u);

  EXPECT_TRUE(gpuinfo_apple_parse_pci_bdf("0:31:2", &bus, &device, &function));
  EXPECT_EQ(bus, 0u);
  EXPECT_EQ(device, 31u);
  EXPECT_EQ(function, 2u);

  // Bridges append the secondary/subordinate bus range after the BDF.
  EXPECT_TRUE(gpuinfo_apple_parse_pci_bdf("0:0:0(1:128)", &bus, &device, &function));

  EXPECT_FALSE(gpuinfo_apple_parse_pci_bdf("not-a-bdf", &bus, &device, &function));
  EXPECT_FALSE(gpuinfo_apple_parse_pci_bdf("1:2", &bus, &device, &function));
  EXPECT_FALSE(gpuinfo_apple_parse_pci_bdf("1:40:0", &bus, &device, &function));
  EXPECT_FALSE(gpuinfo_apple_parse_pci_bdf("1:2:8", &bus, &device, &function));
  EXPECT_FALSE(gpuinfo_apple_parse_pci_bdf(NULL, &bus, &device, &function));
}

TEST(ApplePciInfo, DecodesPcieLinkStatus) {
  unsigned gen = 0;
  unsigned width = 0;

  // 0x103: speed code 3 (8 GT/s), width 16.
  EXPECT_TRUE(gpuinfo_apple_decode_pcie_link_status(0x103, &gen, &width));
  EXPECT_EQ(gen, 3u);
  EXPECT_EQ(width, 16u);

  // 0x43: speed code 3 (8 GT/s), width 4 (a Thunderbolt eGPU).
  EXPECT_TRUE(gpuinfo_apple_decode_pcie_link_status(0x43, &gen, &width));
  EXPECT_EQ(gen, 3u);
  EXPECT_EQ(width, 4u);

  EXPECT_FALSE(gpuinfo_apple_decode_pcie_link_status(0xFFFF, &gen, &width));
  EXPECT_FALSE(gpuinfo_apple_decode_pcie_link_status(0, &gen, &width));

  // A speed code without a width (and vice versa) is not a valid link.
  EXPECT_FALSE(gpuinfo_apple_decode_pcie_link_status(0x03, &gen, &width));
  EXPECT_FALSE(gpuinfo_apple_decode_pcie_link_status(0x30, &gen, &width));
}

TEST(ApplePciInfo, DecodesSlotName) {
  const uint8_t mac_pro_slot[] = {'S', 'l', 'o', 't', '-', '1', 0, 0};
  const uint8_t padded_slot[] = {'P', 'C', 'I', ' ', 'S', 'l', 'o', 't', ' ', '2', 0xAA, 0xAA};
  const uint8_t empty_slot[] = {0, 0};
  const uint8_t embedded_nul[] = {'S', 'l', 'o', 't', 0, '1', '1'};
  const uint8_t exact_fit[] = {'S', 'l', 'o', 't', '-', '1', '2', '3', '4', '5', '6', '7'};
  const uint8_t too_long[] = "a slot label that does not fit";
  char slot[32];
  char small[8];
  char exact[13];

  EXPECT_TRUE(gpuinfo_apple_decode_slot_name(mac_pro_slot, sizeof(mac_pro_slot), slot, sizeof(slot)));
  EXPECT_STREQ(slot, "Slot-1");

  EXPECT_TRUE(gpuinfo_apple_decode_slot_name(padded_slot, sizeof(padded_slot), slot, sizeof(slot)));
  EXPECT_STREQ(slot, "PCI Slot 2");

  // The property must be one NUL-terminated (or exactly-sized) C string; an
  // embedded NUL means the data is not a usable label.
  EXPECT_FALSE(gpuinfo_apple_decode_slot_name(embedded_nul, sizeof(embedded_nul), slot, sizeof(slot)));
  EXPECT_FALSE(gpuinfo_apple_decode_slot_name(empty_slot, sizeof(empty_slot), slot, sizeof(slot)));
  EXPECT_FALSE(gpuinfo_apple_decode_slot_name(too_long, sizeof(too_long) - 1, small, sizeof(small)));

  // Exactly slot_size - 1 printable bytes fit without truncation.
  EXPECT_TRUE(gpuinfo_apple_decode_slot_name(exact_fit, sizeof(exact_fit), exact, sizeof(exact)));
  EXPECT_STREQ(exact, "Slot-1234567");
}

TEST(ApplePciInfo, ParsesDieIndex) {
  unsigned die = 0;

  EXPECT_TRUE(gpuinfo_apple_parse_die_index("IOACPIPlane:/_SB/PCI0@0/BR3A@0/GU00@0/GD01@1/EGP0@0", &die));
  EXPECT_EQ(die, 1u);

  EXPECT_TRUE(gpuinfo_apple_parse_die_index("IOACPIPlane:/_SB/PCI0@0/BR1A@0/GU00@0/GD00@0/EGP0@0", &die));
  EXPECT_EQ(die, 0u);

  // attached-gpu-control-path uses the IOService namespace but carries the
  // same GD<nn> segment.
  EXPECT_TRUE(
      gpuinfo_apple_parse_die_index("IOService:/AppleACPIPlatformExpert/PCI0@0/BR3A@0/GU00@0/GD01@1/EGP0@0", &die));
  EXPECT_EQ(die, 1u);

  EXPECT_FALSE(gpuinfo_apple_parse_die_index("IOACPIPlane:/_SB/PCI0@0/BR1A@0/UPSB@0", &die));
  EXPECT_FALSE(gpuinfo_apple_parse_die_index(NULL, &die));
}
