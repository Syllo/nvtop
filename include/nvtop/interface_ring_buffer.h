/*
 *
 * Copyright (C) 2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef INTERFACE_RING_BUFFER_H__
#define INTERFACE_RING_BUFFER_H__

#include <assert.h>

typedef struct interface_ring_buffer_st {
  unsigned monitored_dev_count;
  unsigned per_device_data_saved;
  unsigned buffer_size;
  void *ring_buffer[2];
} interface_ring_buffer;

#define INTERFACE_RING_BUFFER_DATA(ring_buffer_ptr, name)                                                              \
  unsigned(*name)[ring_buffer_ptr->per_device_data_saved][ring_buffer_ptr->buffer_size] =                              \
      (unsigned(*)[ring_buffer_ptr->per_device_data_saved][ring_buffer_ptr->buffer_size])                              \
          ring_buffer_ptr->ring_buffer[1];

#define INTERFACE_RING_BUFFER_INDICES(ring_buffer_ptr, name)                                                           \
  unsigned(*name)[ring_buffer_ptr->per_device_data_saved][2] =                                                         \
      (unsigned(*)[ring_buffer_ptr->per_device_data_saved][2])ring_buffer_ptr->ring_buffer[0];

void interface_alloc_ring_buffer(unsigned monitored_dev_count, unsigned per_device_data, unsigned buffer_size,
                                 interface_ring_buffer *ring_buffer);

void interface_free_ring_buffer(interface_ring_buffer *buffer);

inline unsigned interface_ring_buffer_data_stored(const interface_ring_buffer *buff, unsigned device,
                                                  unsigned which_data) {
  INTERFACE_RING_BUFFER_INDICES(buff, indices);
  unsigned start = indices[device][which_data][0];
  unsigned end = indices[device][which_data][1];
  unsigned length = end - start;
  if (end < start) { // Has wrapped around the buffer
    length += buff->buffer_size;
  }
  return length;
}

inline unsigned interface_index_in_ring(const interface_ring_buffer *buff, unsigned device, unsigned which_data,
                                        unsigned index) {
  assert(interface_ring_buffer_data_stored(buff, device, which_data) > index);
  INTERFACE_RING_BUFFER_INDICES(buff, indices);
  unsigned start = indices[device][which_data][0];
  unsigned location = start + index;
  if (location >= buff->buffer_size)
    location -= buff->buffer_size;
  return location;
}

inline unsigned interface_ring_buffer_get(const interface_ring_buffer *buff, unsigned device, unsigned which_data,
                                          unsigned index) {
  INTERFACE_RING_BUFFER_DATA(buff, data);
  unsigned index_in_ring = interface_index_in_ring(buff, device, which_data, index);
  return data[device][which_data][index_in_ring];
}

inline void interface_ring_buffer_push(interface_ring_buffer *buff, unsigned device, unsigned which_data,
                                       unsigned value) {
  INTERFACE_RING_BUFFER_INDICES(buff, indices);
  INTERFACE_RING_BUFFER_DATA(buff, data);
  unsigned start = indices[device][which_data][0];
  unsigned end = indices[device][which_data][1];
  // If ring full, move start index
  data[device][which_data][end] = value;
  end++;
  if (end == buff->buffer_size)
    end -= buff->buffer_size;
  if (end == start) {
    start++;
    if (start == buff->buffer_size)
      start -= buff->buffer_size;
  }
  indices[device][which_data][0] = start;
  indices[device][which_data][1] = end;
}

inline void interface_ring_buffer_pop(interface_ring_buffer *buff, unsigned device, unsigned which_data) {
  INTERFACE_RING_BUFFER_INDICES(buff, indices);
  unsigned start = indices[device][which_data][0];
  unsigned end = indices[device][which_data][1];
  if (start != end) {
    start++;
    if (start == buff->buffer_size)
      start -= buff->buffer_size;
    indices[device][which_data][0] = start;
  }
}

inline void interface_ring_buffer_empty_select(interface_ring_buffer *buff, unsigned device, unsigned which_data) {
  INTERFACE_RING_BUFFER_INDICES(buff, indices);
  indices[device][which_data][0] = 0;
  indices[device][which_data][1] = 0;
}

inline void interface_ring_buffer_empty(interface_ring_buffer *buff, unsigned device) {
  for (unsigned i = 0; i < buff->per_device_data_saved; ++i) {
    interface_ring_buffer_empty_select(buff, device, i);
  }
}

#endif // INTERFACE_RING_BUFFER_H__
