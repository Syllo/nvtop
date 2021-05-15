#ifndef INTERFACE_LAYOUT_SELECTION_H__
#define INTERFACE_LAYOUT_SELECTION_H__

#include <stdbool.h>

struct layout_selection;

enum plot_type {
  plot_gpu_max,
  plot_gpu_solo,
  plot_gpu_duo,
};

struct window_position {
  unsigned posX, posY, sizeX, sizeY;
};

char *layout_as_string(struct layout_selection *layout);

void compute_sizes_from_layout(
    bool show_graphs, bool show_header, bool show_process, unsigned num_devices,
    unsigned num_info_per_device, unsigned device_header_rows,
    unsigned device_header_cols, unsigned rows, unsigned cols,
    struct window_position *device_position,
    struct window_position *process_position, unsigned *num_plots,
    struct window_position **plot_positions, enum plot_type *plot_types,
    struct window_position *setup_position);

#endif // INTERFACE_LAYOUT_SELECTION_H__
