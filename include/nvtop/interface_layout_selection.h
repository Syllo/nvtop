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

// Clamp the selected row and the offset of a vertical list to the size of the
// window that draws it, scrolling as little as possible around the selection.
void update_selected_offset_with_window_size(unsigned int *selected_row, unsigned int *offset,
                                             unsigned int row_available_to_draw, unsigned int num_to_draw);

// When a process list is followed by a TOTAL row, minimal scrolling can leave
// the selected process on the last line of the window and push the TOTAL row
// out of view. If the last process is visible, return the offset that shifts
// the window up by one line while keeping the selected process on screen;
// otherwise return the offset unchanged.
unsigned int keep_total_row_visible(unsigned int selected_row, unsigned int offset,
                                    unsigned int row_available_to_draw, unsigned int num_processes);

// Sum the numeric fields of all processes into a pseudo-process that has the
// same shape as a process and is used to render the TOTAL row.
void sum_processes(all_processes all_procs, struct gpu_process *total);

#endif // INTERFACE_LAYOUT_SELECTION_H__
