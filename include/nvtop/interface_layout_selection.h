#ifndef INTERFACE_LAYOUT_SELECTION_H__
#define INTERFACE_LAYOUT_SELECTION_H__

#include "nvtop/interface_common.h"
#include "nvtop/interface_options.h"

#include <stdbool.h>

struct window_position {
  unsigned posX, posY, sizeX, sizeY;
};

// Should be fine
#define MAX_CHARTS 64

void compute_sizes_from_layout(unsigned monitored_dev_count, unsigned device_header_rows, unsigned device_header_cols,
                               unsigned rows, unsigned cols, const nvtop_interface_gpu_opts *gpu_opts,
                               process_field_displayed process_field_displayed,
                               struct window_position *device_positions, unsigned *num_plots,
                               struct window_position plot_positions[MAX_CHARTS], unsigned *map_device_to_plot,
                               struct window_position *process_position, struct window_position *setup_position,
                               bool process_win_hide);

#endif // INTERFACE_LAYOUT_SELECTION_H__
