/*
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
 *
 * This file is part of Nvtop and adapted from radeontop.
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

#include "nvtop/device_discovery.h"

#if defined(USING_LIBUDEV) && defined(USING_LIBSYSTEMD)
#error Cannot use libudev and libsystemd at the same time
#endif

#if defined(USING_LIBUDEV)

#include <assert.h>
#include <errno.h>
#include <libudev.h>
#include <stdlib.h>

typedef struct nvtop_device_enumerator {
  unsigned refCount;
  struct udev *udev;
  struct udev_enumerate *enumerate;
  struct udev_list_entry *list_entry, *current;
} nvtop_device_enumerator;

nvtop_device *nvtop_device_ref(nvtop_device *device) {
  udev_device_ref((struct udev_device *)device);
  return device;
}

nvtop_device *nvtop_device_unref(nvtop_device *device) {
  udev_device_unref((struct udev_device *)device);
  return NULL;
}

int nvtop_device_get_parent(nvtop_device *child, nvtop_device **parent) {
  *parent = (nvtop_device *)udev_device_get_parent((struct udev_device *)child);
  return *parent ? 0 : -ENOENT;
}

int nvtop_device_get_driver(nvtop_device *device, const char **driver) {
  *driver = udev_device_get_driver((struct udev_device *)device);
  return *driver ? 0 : -ENOENT;
}

int nvtop_device_get_devname(nvtop_device *device, const char **devname) {
  *devname = udev_device_get_devnode((struct udev_device *)device);
  return *devname ? 0 : -ENOENT;
}

int nvtop_device_get_property_value(nvtop_device *device, const char *key, const char **value) {
  *value = udev_device_get_property_value((struct udev_device *)device, key);
  return *value ? 0 : -ENOENT;
}

int nvtop_device_get_sysattr_value(nvtop_device *device, const char *sysattr, const char **value) {
  *value = udev_device_get_sysattr_value((struct udev_device *)device, sysattr);
  return *value ? 0 : -ENOENT;
}

int nvtop_enumerator_new(nvtop_device_enumerator **enumerator) {
  *enumerator = calloc(1, sizeof(**enumerator));
  if (!enumerator)
    return -1;
  (*enumerator)->refCount = 1;
  (*enumerator)->udev = udev_new();
  if (!(*enumerator)->udev)
    goto cleanAllocErr;
  (*enumerator)->enumerate = udev_enumerate_new((*enumerator)->udev);
  if (!(*enumerator)->enumerate)
    goto cleanAllErr;

  return 0;

cleanAllErr:
  udev_unref((*enumerator)->udev);
cleanAllocErr:
  free(*enumerator);
  return -1;
}

nvtop_device_enumerator *nvtop_enumerator_ref(nvtop_device_enumerator *enumerator) {
  udev_ref(enumerator->udev);
  udev_enumerate_ref(enumerator->enumerate);
  return enumerator;
}

nvtop_device_enumerator *nvtop_enumerator_unref(nvtop_device_enumerator *enumerator) {
  udev_unref(enumerator->udev);
  udev_enumerate_unref(enumerator->enumerate);
  if (!enumerator->refCount) {
    free(enumerator);
    return NULL;
  } else {
    return enumerator;
  }
}

int nvtop_device_enumerator_add_match_subsystem(nvtop_device_enumerator *enumerator, const char *subsystem, int match) {
  assert(enumerator->refCount && "Reference at zero");
  enumerator->list_entry = NULL;
  int ret = 1;
  if (match) {
    ret = udev_enumerate_add_match_subsystem(enumerator->enumerate, subsystem);
  } else {
    ret = udev_enumerate_add_nomatch_subsystem(enumerator->enumerate, subsystem);
  }
  return ret;
}

int nvtop_device_enumerator_add_match_property(nvtop_device_enumerator *enumerator, const char *property,
                                               const char *value) {
  assert(enumerator->refCount && "Reference at zero");
  enumerator->list_entry = NULL;
  return udev_enumerate_add_match_property(enumerator->enumerate, property, value);
}

static nvtop_device *nvtop_enumerator_get_current(nvtop_device_enumerator *enumerator) {
  if (!enumerator->current)
    return NULL;
  const char *syspath = udev_list_entry_get_name(enumerator->list_entry);
  return (nvtop_device *)udev_device_new_from_syspath(enumerator->udev, syspath);
}

nvtop_device *nvtop_enumerator_get_device_first(nvtop_device_enumerator *enumerator) {
  if (!enumerator->list_entry) {
    int err = udev_enumerate_scan_devices(enumerator->enumerate);
    if (err < 0)
      return NULL;
    enumerator->list_entry = udev_enumerate_get_list_entry(enumerator->enumerate);
    enumerator->current = enumerator->list_entry;
  }
  return nvtop_enumerator_get_current(enumerator);
}

nvtop_device *nvtop_enumerator_get_device_next(nvtop_device_enumerator *enumerator) {
  enumerator->current = udev_list_entry_get_next(enumerator->current);
  return nvtop_enumerator_get_current(enumerator);
}

#elif defined(USING_LIBSYSTEMD)

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <systemd/sd-device.h>

nvtop_device *nvtop_device_ref(nvtop_device *device) { return (nvtop_device *)sd_device_ref((sd_device *)device); }

nvtop_device *nvtop_device_unref(nvtop_device *device) { return (nvtop_device *)sd_device_unref((sd_device *)device); }

int nvtop_device_get_parent(nvtop_device *child, nvtop_device **parent) {
  return sd_device_get_parent((sd_device *)child, (sd_device **)parent);
}

int nvtop_device_get_driver(nvtop_device *device, const char **driver) {
  return sd_device_get_driver((sd_device *)device, driver);
}

int nvtop_device_get_devname(nvtop_device *device, const char **devname) {
  return sd_device_get_devname((sd_device *)device, devname);
}

int nvtop_device_get_property_value(nvtop_device *device, const char *key, const char **value) {
  return sd_device_get_property_value((sd_device *)device, key, value);
}

int nvtop_device_get_sysattr_value(nvtop_device *device, const char *sysattr, const char **value) {
  return sd_device_get_sysattr_value((sd_device *)device, sysattr, value);
}

int nvtop_enumerator_new(nvtop_device_enumerator **enumerator) {
  return sd_device_enumerator_new((sd_device_enumerator **)enumerator);
}

nvtop_device_enumerator *nvtop_enumerator_ref(nvtop_device_enumerator *enumerator) {
  return (nvtop_device_enumerator *)sd_device_enumerator_ref((sd_device_enumerator *)enumerator);
}

nvtop_device_enumerator *nvtop_enumerator_unref(nvtop_device_enumerator *enumerator) {
  return (nvtop_device_enumerator *)sd_device_enumerator_unref((sd_device_enumerator *)enumerator);
}

int nvtop_device_enumerator_add_match_subsystem(nvtop_device_enumerator *enumerator, const char *subsystem, int match) {
  return sd_device_enumerator_add_match_subsystem((sd_device_enumerator *)enumerator, subsystem, match);
}

int nvtop_device_enumerator_add_match_property(nvtop_device_enumerator *enumerator, const char *property,
                                               const char *value) {
  return sd_device_enumerator_add_match_property((sd_device_enumerator *)enumerator, property, value);
}

nvtop_device *nvtop_enumerator_get_device_first(nvtop_device_enumerator *enumerator) {
  return (nvtop_device *)sd_device_enumerator_get_device_first((sd_device_enumerator *)enumerator);
}

nvtop_device *nvtop_enumerator_get_device_next(nvtop_device_enumerator *enumerator) {
  return (nvtop_device *)sd_device_enumerator_get_device_next((sd_device_enumerator *)enumerator);
}

#endif // elif USE_LIBSYSTEMD