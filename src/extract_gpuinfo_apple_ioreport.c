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
};

struct gpuinfo_apple_ioreport {
  void *library;
  struct gpuinfo_apple_ioreport_api api;
  IOReportSubscriptionRef energy_subscription;
  CFMutableDictionaryRef energy_channels;
  CFDictionaryRef previous_energy_sample;
  nvtop_time previous_sample_time;
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
#define LOAD_IOREPORT_SYMBOL(field, name)                                                                           \
  if (!gpuinfo_apple_ioreport_load_symbol(ioreport->library, name, &ioreport->api.field,                            \
                                          sizeof(ioreport->api.field)))                                              \
    return false

  LOAD_IOREPORT_SYMBOL(copy_channels_in_group, "IOReportCopyChannelsInGroup");
  LOAD_IOREPORT_SYMBOL(create_subscription, "IOReportCreateSubscription");
  LOAD_IOREPORT_SYMBOL(create_samples, "IOReportCreateSamples");
  LOAD_IOREPORT_SYMBOL(create_samples_delta, "IOReportCreateSamplesDelta");
  LOAD_IOREPORT_SYMBOL(channel_get_name, "IOReportChannelGetChannelName");
  LOAD_IOREPORT_SYMBOL(channel_get_unit_label, "IOReportChannelGetUnitLabel");
  LOAD_IOREPORT_SYMBOL(simple_get_integer_value, "IOReportSimpleGetIntegerValue");

#undef LOAD_IOREPORT_SYMBOL
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

  CFMutableDictionaryRef requested_channels = gpuinfo_apple_ioreport_copy_energy_channels(new_ioreport);
  if (!requested_channels) {
    gpuinfo_apple_ioreport_shutdown(new_ioreport);
    return false;
  }

  new_ioreport->energy_subscription = new_ioreport->api.create_subscription(
      NULL, requested_channels, &new_ioreport->energy_channels, 0, NULL);
  CFRelease(requested_channels);
  if (!new_ioreport->energy_subscription || !new_ioreport->energy_channels ||
      CFGetTypeID(new_ioreport->energy_channels) != CFDictionaryGetTypeID()) {
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
  if (ioreport->energy_subscription)
    CFRelease(ioreport->energy_subscription);
  if (ioreport->energy_channels)
    CFRelease(ioreport->energy_channels);
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
  if (!ioreport || !power_draw)
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
