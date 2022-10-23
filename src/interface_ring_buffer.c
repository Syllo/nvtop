
#include "nvtop/interface_ring_buffer.h"

#include "stdio.h"
#include "stdlib.h"

void interface_alloc_ring_buffer(unsigned devices_count, unsigned per_device_data_saved, unsigned buffer_size,
                                 interface_ring_buffer *ring_buffer) {
  ring_buffer->ring_buffer[0] = calloc(1, sizeof(unsigned[devices_count][per_device_data_saved][2]));
  if (!ring_buffer->ring_buffer[0]) {
    perror("Cannot allocate memory: ");
    exit(EXIT_FAILURE);
  }
  ring_buffer->ring_buffer[1] = malloc(sizeof(unsigned[devices_count][per_device_data_saved][buffer_size]));
  if (!ring_buffer->ring_buffer[1]) {
    perror("Cannot allocate memory: ");
    exit(EXIT_FAILURE);
  }
  ring_buffer->buffer_size = buffer_size;
  ring_buffer->per_device_data_saved = per_device_data_saved;
  ring_buffer->monitored_dev_count = devices_count;
}

void interface_free_ring_buffer(interface_ring_buffer *buffer) {
  free(buffer->ring_buffer[0]);
  free(buffer->ring_buffer[1]);
}

extern inline unsigned interface_ring_buffer_data_stored(const interface_ring_buffer *buff, unsigned device,
                                                         unsigned which_data);

extern inline unsigned interface_index_in_ring(const interface_ring_buffer *buff, unsigned device, unsigned which_data,
                                               unsigned index);

extern inline unsigned interface_ring_buffer_get(const interface_ring_buffer *buff, unsigned device,
                                                 unsigned which_data, unsigned index);

extern inline void interface_ring_buffer_push(interface_ring_buffer *buff, unsigned device, unsigned which_data,
                                              unsigned value);

extern inline void interface_ring_buffer_pop(interface_ring_buffer *buff, unsigned device, unsigned which_data);

extern inline void interface_ring_buffer_empty_select(interface_ring_buffer *buff, unsigned device,
                                                      unsigned which_data);

extern inline void interface_ring_buffer_empty(interface_ring_buffer *buff, unsigned device);
