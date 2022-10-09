/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef NVTOP_DEVICE_DISCOVERY_H__
#define NVTOP_DEVICE_DISCOVERY_H__

// Devices
typedef struct nvtop_device nvtop_device;

nvtop_device *nvtop_device_ref(nvtop_device *device);
nvtop_device *nvtop_device_unref(nvtop_device *device);

int nvtop_device_new_from_syspath(nvtop_device **ret, const char *syspath);
int nvtop_device_get_parent(nvtop_device *child, nvtop_device **parent);
int nvtop_device_get_driver(nvtop_device *device, const char **driver);
int nvtop_device_get_devname(nvtop_device *device, const char **devname);
int nvtop_device_get_property_value(nvtop_device *device, const char *key, const char **value);
int nvtop_device_get_sysattr_value(nvtop_device *device, const char *sysattr, const char **value);
int nvtop_device_get_syspath(nvtop_device *device, const char **sysPath);

// Devices enumerator
typedef struct nvtop_device_enumerator nvtop_device_enumerator;

int nvtop_enumerator_new(nvtop_device_enumerator **enumerator);
nvtop_device_enumerator *nvtop_enumerator_ref(nvtop_device_enumerator *enumerator);
nvtop_device_enumerator *nvtop_enumerator_unref(nvtop_device_enumerator *enumerator);

int nvtop_device_enumerator_add_match_subsystem(nvtop_device_enumerator *enumerator, const char *subsystem, int match);
int nvtop_device_enumerator_add_match_property(nvtop_device_enumerator *enumerator, const char *property,
                                               const char *value);
int nvtop_device_enumerator_add_match_parent(nvtop_device_enumerator *enumerator, nvtop_device *parent);

nvtop_device *nvtop_enumerator_get_device_first(nvtop_device_enumerator *enumerator);
nvtop_device *nvtop_enumerator_get_device_next(nvtop_device_enumerator *enumerator);

typedef struct {
  unsigned width;
  unsigned speed;
} nvtop_pcie_link;

int nvtop_device_maximum_pcie_link(nvtop_device *dev, nvtop_pcie_link *pcie_info);
int nvtop_device_current_pcie_link(nvtop_device *dev, nvtop_pcie_link *pcie_info);

nvtop_device *nvtop_device_get_hwmon(nvtop_device *dev);

#endif // NVTOP_DEVICE_DISCOVERY_H__
