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

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

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
  unsigned num_devices;
  unsigned current_device;
  struct udev_device **devices;
} nvtop_device_enumerator;

nvtop_device *nvtop_device_ref(nvtop_device *device) {
  udev_device_ref((struct udev_device *)device);
  return device;
}

nvtop_device *nvtop_device_unref(nvtop_device *device) {
  udev_device_unref((struct udev_device *)device);
  return NULL;
}

int nvtop_device_new_from_syspath(nvtop_device **dev, const char *syspath) {
  struct udev *udev = udev_new();
  if (!udev)
    return -ENOMEM;
  *dev = (nvtop_device *)udev_device_new_from_syspath(udev, syspath);
  udev_unref(udev);
  return *dev ? 1 : -ENOENT;
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

int nvtop_device_get_syspath(nvtop_device *device, const char **sysPath) {
  *sysPath = udev_device_get_syspath((struct udev_device *)device);
  return *sysPath ? 0 : -ENOENT;
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

static void nvtop_free_enumerator_devices(nvtop_device_enumerator *enumerator) {
  if (enumerator->devices) {
    for (unsigned i = 0; enumerator->devices && i < enumerator->num_devices; ++i) {
      udev_device_unref(enumerator->devices[i]);
    }
    free(enumerator->devices);
    enumerator->devices = NULL;
  }
}

nvtop_device_enumerator *nvtop_enumerator_ref(nvtop_device_enumerator *enumerator) {
  enumerator->refCount++;
  udev_ref(enumerator->udev);
  udev_enumerate_ref(enumerator->enumerate);
  return enumerator;
}

nvtop_device_enumerator *nvtop_enumerator_unref(nvtop_device_enumerator *enumerator) {
  enumerator->refCount--;
  udev_enumerate_unref(enumerator->enumerate);
  udev_unref(enumerator->udev);
  if (!enumerator->refCount) {
    nvtop_free_enumerator_devices(enumerator);
    free(enumerator);
    return NULL;
  } else {
    return enumerator;
  }
}

int nvtop_device_enumerator_add_match_subsystem(nvtop_device_enumerator *enumerator, const char *subsystem, int match) {
  assert(enumerator->refCount && "Reference at zero");
  nvtop_free_enumerator_devices(enumerator);
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
  nvtop_free_enumerator_devices(enumerator);
  return udev_enumerate_add_match_property(enumerator->enumerate, property, value);
}

int nvtop_device_enumerator_add_match_parent(nvtop_device_enumerator *enumerator, nvtop_device *parent) {
  assert(enumerator->refCount && "Reference at zero");
  nvtop_free_enumerator_devices(enumerator);
  return udev_enumerate_add_match_parent(enumerator->enumerate, (struct udev_device *)parent);
}

static nvtop_device *nvtop_enumerator_get_current(nvtop_device_enumerator *enumerator) {
  if (!enumerator->devices || enumerator->current_device >= enumerator->num_devices)
    return NULL;
  return (nvtop_device *)enumerator->devices[enumerator->current_device];
}

nvtop_device *nvtop_enumerator_get_device_first(nvtop_device_enumerator *enumerator) {
  if (!enumerator->devices) {
    int err = udev_enumerate_scan_devices(enumerator->enumerate);
    if (err < 0)
      return NULL;
    struct udev_list_entry *list = udev_enumerate_get_list_entry(enumerator->enumerate);
    struct udev_list_entry *curr;
    enumerator->num_devices = 0;
    udev_list_entry_foreach(curr, list) { enumerator->num_devices++; }
    enumerator->devices = calloc(enumerator->num_devices, sizeof(*enumerator->devices));
    if (!enumerator->devices)
      return NULL;
    unsigned idx = 0;
    udev_list_entry_foreach(curr, list) {
      const char *path = udev_list_entry_get_name(curr);
      enumerator->devices[idx++] = udev_device_new_from_syspath(enumerator->udev, path);
    }
  }
  enumerator->current_device = 0;
  return nvtop_enumerator_get_current(enumerator);
}

nvtop_device *nvtop_enumerator_get_device_next(nvtop_device_enumerator *enumerator) {
  enumerator->current_device++;
  return nvtop_enumerator_get_current(enumerator);
}

#elif defined(USING_LIBSYSTEMD)

#include <assert.h>
#include <errno.h>
#include <stdlib.h>
#include <systemd/sd-device.h>

nvtop_device *nvtop_device_ref(nvtop_device *device) { return (nvtop_device *)sd_device_ref((sd_device *)device); }

nvtop_device *nvtop_device_unref(nvtop_device *device) { return (nvtop_device *)sd_device_unref((sd_device *)device); }

int nvtop_device_new_from_syspath(nvtop_device **dev, const char *syspath) {
  return sd_device_new_from_syspath((sd_device **)dev, syspath);
}

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

int nvtop_device_get_syspath(nvtop_device *device, const char **sysPath) {
  return sd_device_get_syspath((sd_device *)device, sysPath);
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

int nvtop_device_enumerator_add_match_parent(nvtop_device_enumerator *enumerator, nvtop_device *parent) {
  return sd_device_enumerator_add_match_parent((sd_device_enumerator *)enumerator, (sd_device *)parent);
}

nvtop_device *nvtop_enumerator_get_device_first(nvtop_device_enumerator *enumerator) {
  return (nvtop_device *)sd_device_enumerator_get_device_first((sd_device_enumerator *)enumerator);
}

nvtop_device *nvtop_enumerator_get_device_next(nvtop_device_enumerator *enumerator) {
  return (nvtop_device *)sd_device_enumerator_get_device_next((sd_device_enumerator *)enumerator);
}

#endif // elif USE_LIBSYSTEMD

static int pcie_walker_helper(nvtop_device *dev, nvtop_pcie_link *pcie_info, const char *link_speed_attr,
                              const char *link_width_attr) {
  bool valid = false;
  const char *driver;
  int ret = nvtop_device_get_driver(dev, &driver);
  if (ret < 0)
    return ret;
  if (!strcmp(driver, "pcieport")) {
    const char *speed_str, *width_str;
    unsigned speed, width;
    ret = nvtop_device_get_sysattr_value(dev, link_speed_attr, &speed_str);
    if (ret < 0)
      return ret;
    ret = nvtop_device_get_sysattr_value(dev, link_width_attr, &width_str);
    if (ret < 0)
      return ret;
    ret = sscanf(speed_str, "%u", &speed);
    if (ret != 1)
      return -1;
    ret = sscanf(width_str, "%u", &width);
    if (ret != 1)
      return -1;
    pcie_info->speed = pcie_info->speed > speed ? speed : pcie_info->speed;
    pcie_info->width = pcie_info->width > width ? width : pcie_info->width;
    valid = true;
  }
  nvtop_device *parent;
  ret = nvtop_device_get_parent(dev, &parent);
  if (ret < 0)
    return valid;
  ret = pcie_walker_helper(parent, pcie_info, link_speed_attr, link_width_attr);
  return valid || ret >= 0;
}

int nvtop_device_maximum_pcie_link(nvtop_device *dev, nvtop_pcie_link *pcie_info) {
  pcie_info->speed = UINT_MAX;
  pcie_info->width = UINT_MAX;
  return pcie_walker_helper(dev, pcie_info, "max_link_speed", "max_link_width");
}

int nvtop_device_current_pcie_link(nvtop_device *dev, nvtop_pcie_link *pcie_info) {
  // We open a new device to avoid cached values
  const char *path;
  nvtop_device_get_syspath(dev, &path);
  nvtop_device *dev_samepath;
  int ret = nvtop_device_new_from_syspath(&dev_samepath, path);
  if (ret < 0)
    return ret;
  pcie_info->speed = UINT_MAX;
  pcie_info->width = UINT_MAX;
  ret = pcie_walker_helper(dev_samepath, pcie_info, "current_link_speed", "current_link_width");
  nvtop_device_unref(dev_samepath);
  return ret;
}

nvtop_device *nvtop_device_get_hwmon(nvtop_device *dev) {
  nvtop_device_enumerator *enumerator;
  int ret = nvtop_enumerator_new(&enumerator);
  if (ret < 0)
    return NULL;
  ret = nvtop_device_enumerator_add_match_subsystem(enumerator, "hwmon", true);
  if (ret < 0)
    return NULL;
  ret = nvtop_device_enumerator_add_match_parent(enumerator, dev);
  if (ret < 0)
    return NULL;
  nvtop_device *hwmon = nvtop_enumerator_get_device_first(enumerator);
  if (!hwmon)
    return NULL;
  nvtop_device_ref(hwmon);
  nvtop_enumerator_unref(enumerator);
  return hwmon;
}
