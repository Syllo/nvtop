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

#include "extract_gpuinfo_apple_ioreport.h"

#include "extract_gpuinfo_apple_utils.h"
#include "nvtop/time.h"

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/IOKitLib.h>
#include <dlfcn.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

// IOReport has no public user-space header. Keep its declarations in this file and load the
// framework dynamically so an API or channel change only makes the extra metrics unavailable.
typedef struct __IOReportSubscriptionRef *IOReportSubscriptionRef;

struct gpuinfo_apple_ioreport_api {
  CFDictionaryRef (*copy_channels_in_group)(CFStringRef, CFStringRef, uint64_t, uint64_t, uint64_t);
  IOReportSubscriptionRef (*create_subscription)(const void *, CFMutableDictionaryRef, CFMutableDictionaryRef *,
                                                 uint64_t, CFTypeRef);
  CFDictionaryRef (*create_samples)(IOReportSubscriptionRef, CFMutableDictionaryRef, CFTypeRef);
  CFDictionaryRef (*create_samples_delta)(CFDictionaryRef, CFDictionaryRef, CFTypeRef);
  CFStringRef (*channel_get_name)(CFDictionaryRef);
  CFStringRef (*channel_get_unit_label)(CFDictionaryRef);
  int64_t (*simple_get_integer_value)(CFDictionaryRef, int);
  int (*state_get_count)(CFDictionaryRef);
  CFStringRef (*state_get_name_for_index)(CFDictionaryRef, int);
  int64_t (*state_get_residency)(CFDictionaryRef, int);
};

struct gpuinfo_apple_ioreport {
  void *library;
  struct gpuinfo_apple_ioreport_api api;
  bool energy_api_available;
  bool performance_state_api_available;
  IOReportSubscriptionRef energy_subscription;
  CFMutableDictionaryRef energy_channels;
  CFDictionaryRef previous_energy_sample;
  nvtop_time previous_sample_time;
  IOReportSubscriptionRef performance_state_subscription;
  CFMutableDictionaryRef performance_state_channels;
  CFDictionaryRef previous_performance_state_sample;
  unsigned *gpu_frequencies;
  size_t gpu_frequency_count;
  unsigned max_gpu_frequency;
};

static bool gpuinfo_apple_ioreport_load_symbol(void *library, const char *name, void *destination,
                                               size_t destination_size) {
  void *symbol = dlsym(library, name);
  if (!symbol || destination_size != sizeof(symbol))
    return false;

  memcpy(destination, &symbol, sizeof(symbol));
  return true;
}

static bool gpuinfo_apple_ioreport_load_api(struct gpuinfo_apple_ioreport *ioreport) {
#define LOAD_REQUIRED_IOREPORT_SYMBOL(field, name)                                                                  \
  if (!gpuinfo_apple_ioreport_load_symbol(ioreport->library, name, &ioreport->api.field,                            \
                                          sizeof(ioreport->api.field)))                                              \
    return false

  LOAD_REQUIRED_IOREPORT_SYMBOL(copy_channels_in_group, "IOReportCopyChannelsInGroup");
  LOAD_REQUIRED_IOREPORT_SYMBOL(create_subscription, "IOReportCreateSubscription");
  LOAD_REQUIRED_IOREPORT_SYMBOL(create_samples, "IOReportCreateSamples");
  LOAD_REQUIRED_IOREPORT_SYMBOL(create_samples_delta, "IOReportCreateSamplesDelta");

#undef LOAD_REQUIRED_IOREPORT_SYMBOL

  ioreport->energy_api_available =
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportChannelGetChannelName",
                                         &ioreport->api.channel_get_name,
                                         sizeof(ioreport->api.channel_get_name)) &&
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportChannelGetUnitLabel",
                                         &ioreport->api.channel_get_unit_label,
                                         sizeof(ioreport->api.channel_get_unit_label)) &&
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportSimpleGetIntegerValue",
                                         &ioreport->api.simple_get_integer_value,
                                         sizeof(ioreport->api.simple_get_integer_value));
  ioreport->performance_state_api_available =
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportStateGetCount",
                                         &ioreport->api.state_get_count,
                                         sizeof(ioreport->api.state_get_count)) &&
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportStateGetNameForIndex",
                                         &ioreport->api.state_get_name_for_index,
                                         sizeof(ioreport->api.state_get_name_for_index)) &&
      gpuinfo_apple_ioreport_load_symbol(ioreport->library, "IOReportStateGetResidency",
                                         &ioreport->api.state_get_residency,
                                         sizeof(ioreport->api.state_get_residency));
  return true;
}

static void gpuinfo_apple_ioreport_release_subscription(IOReportSubscriptionRef *subscription,
                                                        CFMutableDictionaryRef *channels) {
  if (*subscription)
    CFRelease(*subscription);
  if (*channels)
    CFRelease(*channels);
  *subscription = NULL;
  *channels = NULL;
}

static bool gpuinfo_apple_ioreport_create_subscription(struct gpuinfo_apple_ioreport *ioreport,
                                                       CFMutableDictionaryRef requested_channels,
                                                       IOReportSubscriptionRef *subscription,
                                                       CFMutableDictionaryRef *subscribed_channels) {
  *subscription =
      ioreport->api.create_subscription(NULL, requested_channels, subscribed_channels, 0, NULL);
  if (!*subscription || !*subscribed_channels ||
      CFGetTypeID(*subscribed_channels) != CFDictionaryGetTypeID()) {
    gpuinfo_apple_ioreport_release_subscription(subscription, subscribed_channels);
    return false;
  }
  return true;
}

static bool gpuinfo_apple_ioreport_is_gpu_energy_channel(struct gpuinfo_apple_ioreport *ioreport,
                                                         CFDictionaryRef channel) {
  CFStringRef channel_name = ioreport->api.channel_get_name(channel);
  return channel_name && CFGetTypeID(channel_name) == CFStringGetTypeID() &&
         CFStringHasSuffix(channel_name, CFSTR("GPU Energy"));
}

static CFMutableDictionaryRef gpuinfo_apple_ioreport_copy_energy_channels(
    struct gpuinfo_apple_ioreport *ioreport) {
  CFDictionaryRef all_channels =
      ioreport->api.copy_channels_in_group(CFSTR("Energy Model"), NULL, 0, 0, 0);
  if (!all_channels || CFGetTypeID(all_channels) != CFDictionaryGetTypeID()) {
    if (all_channels)
      CFRelease(all_channels);
    return NULL;
  }

  CFTypeRef channels_value = CFDictionaryGetValue(all_channels, CFSTR("IOReportChannels"));
  if (!channels_value || CFGetTypeID(channels_value) != CFArrayGetTypeID()) {
    CFRelease(all_channels);
    return NULL;
  }

  CFArrayRef channels = channels_value;
  CFMutableArrayRef energy_channels =
      CFArrayCreateMutable(kCFAllocatorDefault, 0, &kCFTypeArrayCallBacks);
  if (!energy_channels) {
    CFRelease(all_channels);
    return NULL;
  }

  for (CFIndex i = 0; i < CFArrayGetCount(channels); ++i) {
    CFTypeRef channel_value = CFArrayGetValueAtIndex(channels, i);
    if (channel_value && CFGetTypeID(channel_value) == CFDictionaryGetTypeID() &&
        gpuinfo_apple_ioreport_is_gpu_energy_channel(ioreport, channel_value))
      CFArrayAppendValue(energy_channels, channel_value);
  }

  CFMutableDictionaryRef selected_channels = NULL;
  if (CFArrayGetCount(energy_channels)) {
    selected_channels = CFDictionaryCreateMutableCopy(kCFAllocatorDefault, 0, all_channels);
    if (selected_channels)
      CFDictionarySetValue(selected_channels, CFSTR("IOReportChannels"), energy_channels);
  }

  CFRelease(energy_channels);
  CFRelease(all_channels);
  return selected_channels;
}

static CFMutableDictionaryRef gpuinfo_apple_ioreport_copy_performance_state_channels(
    struct gpuinfo_apple_ioreport *ioreport) {
  CFDictionaryRef channels = ioreport->api.copy_channels_in_group(
      CFSTR("GPU Stats"), CFSTR("GPU Performance States"), 0, 0, 0);
  if (!channels || CFGetTypeID(channels) != CFDictionaryGetTypeID()) {
    if (channels)
      CFRelease(channels);
    return NULL;
  }

  CFTypeRef channel_array = CFDictionaryGetValue(channels, CFSTR("IOReportChannels"));
  if (!channel_array || CFGetTypeID(channel_array) != CFArrayGetTypeID() ||
      !CFArrayGetCount(channel_array)) {
    CFRelease(channels);
    return NULL;
  }

  CFMutableDictionaryRef mutable_channels =
      CFDictionaryCreateMutableCopy(kCFAllocatorDefault, 0, channels);
  CFRelease(channels);
  return mutable_channels;
}

static bool gpuinfo_apple_ioreport_load_gpu_frequencies(struct gpuinfo_apple_ioreport *ioreport) {
  io_service_t power_manager =
      IOServiceGetMatchingService(kIOMainPortDefault, IOServiceNameMatching("pmgr"));
  if (!MACH_PORT_VALID(power_manager))
    return false;

  CFTypeRef voltage_states = IORegistryEntryCreateCFProperty(
      power_manager, CFSTR("voltage-states9"), kCFAllocatorDefault, kNilOptions);
  IOObjectRelease(power_manager);
  if (!voltage_states || CFGetTypeID(voltage_states) != CFDataGetTypeID()) {
    if (voltage_states)
      CFRelease(voltage_states);
    return false;
  }

  const CFIndex data_size = CFDataGetLength(voltage_states);
  if (data_size <= 0 || data_size % 8) {
    CFRelease(voltage_states);
    return false;
  }

  const size_t state_count = (size_t)data_size / 8;
  unsigned *frequencies = calloc(state_count, sizeof(*frequencies));
  size_t frequency_count;
  const bool frequencies_valid = frequencies && gpuinfo_apple_parse_gpu_frequency_states(
                                                    CFDataGetBytePtr(voltage_states), (size_t)data_size,
                                                    frequencies, state_count, &frequency_count);
  CFRelease(voltage_states);
  if (!frequencies_valid) {
    free(frequencies);
    return false;
  }

  unsigned max_frequency = 0;
  for (size_t i = 1; i < frequency_count; ++i) {
    if (frequencies[i] > max_frequency)
      max_frequency = frequencies[i];
  }
  if (!max_frequency) {
    free(frequencies);
    return false;
  }

  ioreport->gpu_frequencies = frequencies;
  ioreport->gpu_frequency_count = frequency_count;
  ioreport->max_gpu_frequency = max_frequency;
  return true;
}

bool gpuinfo_apple_ioreport_init(struct gpuinfo_apple_ioreport **ioreport) {
  if (!ioreport)
    return false;
  *ioreport = NULL;

  struct gpuinfo_apple_ioreport *new_ioreport = calloc(1, sizeof(*new_ioreport));
  if (!new_ioreport)
    return false;

  new_ioreport->library = dlopen("/usr/lib/libIOReport.dylib", RTLD_LAZY | RTLD_LOCAL);
  if (!new_ioreport->library || !gpuinfo_apple_ioreport_load_api(new_ioreport)) {
    gpuinfo_apple_ioreport_shutdown(new_ioreport);
    return false;
  }

  bool source_available = false;
  CFMutableDictionaryRef requested_channels = NULL;
  if (new_ioreport->energy_api_available)
    requested_channels = gpuinfo_apple_ioreport_copy_energy_channels(new_ioreport);
  if (requested_channels) {
    source_available = gpuinfo_apple_ioreport_create_subscription(
        new_ioreport, requested_channels, &new_ioreport->energy_subscription,
        &new_ioreport->energy_channels);
    CFRelease(requested_channels);
  }

  if (new_ioreport->performance_state_api_available &&
      gpuinfo_apple_ioreport_load_gpu_frequencies(new_ioreport)) {
    requested_channels = gpuinfo_apple_ioreport_copy_performance_state_channels(new_ioreport);
    if (requested_channels) {
      const bool performance_states_available = gpuinfo_apple_ioreport_create_subscription(
          new_ioreport, requested_channels, &new_ioreport->performance_state_subscription,
          &new_ioreport->performance_state_channels);
      source_available = source_available || performance_states_available;
      CFRelease(requested_channels);
    }
    if (!new_ioreport->performance_state_subscription) {
      free(new_ioreport->gpu_frequencies);
      new_ioreport->gpu_frequencies = NULL;
      new_ioreport->gpu_frequency_count = 0;
      new_ioreport->max_gpu_frequency = 0;
    }
  }

  if (!source_available) {
    gpuinfo_apple_ioreport_shutdown(new_ioreport);
    return false;
  }

  *ioreport = new_ioreport;
  return true;
}

void gpuinfo_apple_ioreport_shutdown(struct gpuinfo_apple_ioreport *ioreport) {
  if (!ioreport)
    return;

  if (ioreport->previous_energy_sample)
    CFRelease(ioreport->previous_energy_sample);
  if (ioreport->previous_performance_state_sample)
    CFRelease(ioreport->previous_performance_state_sample);
  gpuinfo_apple_ioreport_release_subscription(&ioreport->energy_subscription,
                                              &ioreport->energy_channels);
  gpuinfo_apple_ioreport_release_subscription(&ioreport->performance_state_subscription,
                                              &ioreport->performance_state_channels);
  free(ioreport->gpu_frequencies);
  if (ioreport->library)
    dlclose(ioreport->library);
  free(ioreport);
}

static bool gpuinfo_apple_ioreport_parse_power_draw(struct gpuinfo_apple_ioreport *ioreport,
                                                    CFDictionaryRef energy_delta, uint64_t elapsed,
                                                    unsigned *power_draw) {
  if (!energy_delta || CFGetTypeID(energy_delta) != CFDictionaryGetTypeID())
    return false;

  CFTypeRef channels_value = CFDictionaryGetValue(energy_delta, CFSTR("IOReportChannels"));
  if (!channels_value || CFGetTypeID(channels_value) != CFArrayGetTypeID())
    return false;

  uint64_t total_energy = 0;
  bool energy_valid = false;
  CFArrayRef channels = channels_value;
  for (CFIndex i = 0; i < CFArrayGetCount(channels); ++i) {
    CFTypeRef channel_value = CFArrayGetValueAtIndex(channels, i);
    if (!channel_value || CFGetTypeID(channel_value) != CFDictionaryGetTypeID() ||
        !gpuinfo_apple_ioreport_is_gpu_energy_channel(ioreport, channel_value))
      continue;

    CFStringRef unit_label = ioreport->api.channel_get_unit_label(channel_value);
    char unit[8];
    if (!unit_label || CFGetTypeID(unit_label) != CFStringGetTypeID() ||
        !CFStringGetCString(unit_label, unit, sizeof(unit), kCFStringEncodingUTF8))
      continue;

    const int64_t raw_energy = ioreport->api.simple_get_integer_value(channel_value, 0);
    uint64_t energy;
    if (!gpuinfo_apple_energy_to_nanojoules(raw_energy, unit, &energy) || UINT64_MAX - total_energy < energy)
      continue;

    total_energy += energy;
    energy_valid = true;
  }

  return energy_valid && gpuinfo_apple_calculate_power_draw(total_energy, elapsed, power_draw);
}

bool gpuinfo_apple_ioreport_get_power_draw(struct gpuinfo_apple_ioreport *ioreport, unsigned *power_draw) {
  if (!ioreport || !ioreport->energy_api_available || !ioreport->energy_subscription ||
      !power_draw)
    return false;

  CFDictionaryRef current_sample =
      ioreport->api.create_samples(ioreport->energy_subscription, ioreport->energy_channels, NULL);
  if (!current_sample)
    return false;

  nvtop_time current_time;
  nvtop_get_current_time(&current_time);
  if (!ioreport->previous_energy_sample) {
    ioreport->previous_energy_sample = current_sample;
    ioreport->previous_sample_time = current_time;
    return false;
  }

  CFDictionaryRef previous_sample = ioreport->previous_energy_sample;
  const nvtop_time previous_time = ioreport->previous_sample_time;
  ioreport->previous_energy_sample = current_sample;
  ioreport->previous_sample_time = current_time;

  CFDictionaryRef energy_delta = ioreport->api.create_samples_delta(previous_sample, current_sample, NULL);
  CFRelease(previous_sample);
  if (!energy_delta)
    return false;

  const uint64_t elapsed = nvtop_difftime_u64(previous_time, current_time);
  const bool power_draw_valid =
      gpuinfo_apple_ioreport_parse_power_draw(ioreport, energy_delta, elapsed, power_draw);
  CFRelease(energy_delta);
  return power_draw_valid;
}

static bool gpuinfo_apple_ioreport_parse_gpu_clock_speed(struct gpuinfo_apple_ioreport *ioreport,
                                                         CFDictionaryRef performance_state_delta,
                                                         unsigned *clock_speed) {
  if (!performance_state_delta ||
      CFGetTypeID(performance_state_delta) != CFDictionaryGetTypeID())
    return false;

  CFTypeRef channels_value =
      CFDictionaryGetValue(performance_state_delta, CFSTR("IOReportChannels"));
  if (!channels_value || CFGetTypeID(channels_value) != CFArrayGetTypeID())
    return false;

  uint64_t *residencies = calloc(ioreport->gpu_frequency_count, sizeof(*residencies));
  if (!residencies)
    return false;

  bool residency_valid = false;
  CFArrayRef channels = channels_value;
  for (CFIndex i = 0; i < CFArrayGetCount(channels); ++i) {
    CFTypeRef channel_value = CFArrayGetValueAtIndex(channels, i);
    if (!channel_value || CFGetTypeID(channel_value) != CFDictionaryGetTypeID())
      continue;

    const int state_count = ioreport->api.state_get_count(channel_value);
    if (state_count < 2 || (size_t)state_count < ioreport->gpu_frequency_count)
      continue;

    CFStringRef first_state = ioreport->api.state_get_name_for_index(channel_value, 0);
    if (!first_state || CFGetTypeID(first_state) != CFStringGetTypeID() ||
        CFStringCompare(first_state, CFSTR("OFF"), 0) != kCFCompareEqualTo)
      continue;

    // State zero maps to voltage-states9's zero-frequency entry. Aggregate the subsequent
    // active-state residencies across channels before calculating a multi-die average.
    bool channel_valid = true;
    for (size_t state = 1; state < ioreport->gpu_frequency_count; ++state) {
      const int64_t residency = ioreport->api.state_get_residency(channel_value, (int)state);
      if (residency < 0 || UINT64_MAX - residencies[state] < (uint64_t)residency) {
        channel_valid = false;
        break;
      }
    }
    if (!channel_valid)
      continue;

    for (size_t state = 1; state < ioreport->gpu_frequency_count; ++state)
      residencies[state] +=
          (uint64_t)ioreport->api.state_get_residency(channel_value, (int)state);
    residency_valid = true;
  }

  const bool clock_speed_valid =
      residency_valid && gpuinfo_apple_calculate_gpu_clock_speed(
                             residencies, ioreport->gpu_frequencies,
                             ioreport->gpu_frequency_count, clock_speed);
  free(residencies);
  return clock_speed_valid;
}

bool gpuinfo_apple_ioreport_get_gpu_clock_speed(struct gpuinfo_apple_ioreport *ioreport,
                                               unsigned *clock_speed, unsigned *max_clock_speed) {
  if (!ioreport || !ioreport->performance_state_api_available ||
      !ioreport->performance_state_subscription || !clock_speed || !max_clock_speed)
    return false;

  CFDictionaryRef current_sample = ioreport->api.create_samples(
      ioreport->performance_state_subscription, ioreport->performance_state_channels, NULL);
  if (!current_sample)
    return false;

  if (!ioreport->previous_performance_state_sample) {
    ioreport->previous_performance_state_sample = current_sample;
    return false;
  }

  CFDictionaryRef previous_sample = ioreport->previous_performance_state_sample;
  ioreport->previous_performance_state_sample = current_sample;
  CFDictionaryRef state_delta =
      ioreport->api.create_samples_delta(previous_sample, current_sample, NULL);
  CFRelease(previous_sample);
  if (!state_delta)
    return false;

  const bool clock_speed_valid =
      gpuinfo_apple_ioreport_parse_gpu_clock_speed(ioreport, state_delta, clock_speed);
  CFRelease(state_delta);
  if (clock_speed_valid)
    *max_clock_speed = ioreport->max_gpu_frequency;
  return clock_speed_valid;
}
