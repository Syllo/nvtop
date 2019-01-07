#include "nvtop/interface.h"
#include "nvtop/interface_layout_selection.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

static void min_size_taken_by_process(unsigned rows, unsigned num_devices,
                                      unsigned *rows_needed) {
  *rows_needed = 1 + max(5, min(rows / 4, num_devices * 3));
}

static const unsigned cols_needed_box_drawing = 5;
static void min_size_taken_by_plot(unsigned num_data_info_to_plot,
                                   unsigned *rows_needed,
                                   unsigned *cols_needed) {
  *rows_needed = 7;
  *cols_needed = cols_needed_box_drawing + 10 * num_data_info_to_plot;
}

void compute_sizes_from_layout(
    bool show_graphs, bool show_header, bool show_process, size_t num_devices,
    unsigned num_info_per_device, unsigned device_header_rows,
    unsigned device_header_cols, unsigned rows, unsigned cols,
    struct window_position *device_positions,
    struct window_position *process_position, unsigned *num_plots,
    struct window_position **plot_positions, enum plot_type *plot_types) {

  unsigned min_rows_for_header = 0, header_stacks = 0, num_device_per_row = 0;
  if (show_header) {
    num_device_per_row = max(1, cols / device_header_cols);
    header_stacks = num_devices / num_device_per_row +
                    ((num_devices % num_device_per_row) > 0);
    if (num_devices % header_stacks == 0)
      num_device_per_row = num_devices / header_stacks;
    min_rows_for_header = header_stacks * device_header_rows;
  }
  unsigned min_rows_for_process = 0;
  if (show_process) {
    min_size_taken_by_process(rows, num_devices, &min_rows_for_process);
  }
  // Not enough room for the header and process
  if (rows < min_rows_for_header + min_rows_for_process) {
    if (rows >= min_rows_for_header + 2) { // Shrink process
      min_rows_for_process = rows - min_rows_for_header;
    } else { // Only header if possible
      min_rows_for_header = rows;
      min_rows_for_process = 0;
    }
  }
  unsigned rows_for_header = min_rows_for_header;
  unsigned rows_for_process = min_rows_for_process;
  unsigned rows_left = rows - min_rows_for_header - min_rows_for_process;

  unsigned min_plot_rows, min_plot_cols_solo, min_plot_cols_duo;
  min_size_taken_by_plot(num_info_per_device, &min_plot_rows,
                         &min_plot_cols_solo);
  min_size_taken_by_plot(num_info_per_device * 2, &min_plot_rows,
                         &min_plot_cols_duo);

  enum plot_type preferred_plot_type = plot_gpu_duo;
  unsigned max_plot_per_row = cols / min_plot_cols_duo;
  if (max_plot_per_row == 0) {
    if (cols >= min_plot_cols_solo) {
      max_plot_per_row = 1;
      preferred_plot_type = plot_gpu_solo;
    } else {
      max_plot_per_row = 0;
    }
  }
  unsigned num_borrow_line = 0;
  unsigned num_plot_stacks = 0;
  if (max_plot_per_row > 0 && show_graphs) {
    if (preferred_plot_type == plot_gpu_duo) {
      num_plot_stacks =
          (num_devices + (num_devices % 2)) / 2 / max_plot_per_row;
    } else {
      num_plot_stacks = num_devices / max_plot_per_row;
    }
    num_plot_stacks = max(num_plot_stacks, 1);
    if (num_plot_stacks * min_plot_rows > rows_left) {
      if (rows_left >= min_plot_rows) {
        preferred_plot_type = plot_gpu_max;
        num_plot_stacks = 1;
      } else {
        num_plot_stacks = 0;
      }
    }
    if (num_plot_stacks > 0) {
      switch (preferred_plot_type) {
      case plot_gpu_duo:
        *num_plots = (num_devices + (num_devices % 2)) / 2;
        break;
      case plot_gpu_solo:
        *num_plots = num_devices;
        break;
      case plot_gpu_max:
        *num_plots = 1;
        break;
      }
      num_borrow_line = rows_left - num_plot_stacks * min_plot_rows;
    } else {
      goto no_plot;
    }
  } else {
  no_plot:
    num_borrow_line = rows_left;
    *num_plots = 0;
  }

  unsigned space_for_header = header_stacks == 0 ? 0 : header_stacks - 1;
  bool space_between_header_stack = false;
  if (num_borrow_line >= space_for_header && show_header) {
    rows_for_header += space_for_header;
    rows_left -= space_for_header;
    space_between_header_stack = true;
  }
  if (*num_plots == 0 && show_process && rows_left > 0) {
    rows_for_process += rows_left - 1;
  }
  if (num_plot_stacks > 0) {
    // Allocate new plot stack is enough vertical room
    while (num_plot_stacks < *num_plots &&
           rows_left / (num_plot_stacks + 1) >= 11 &&
           (num_plot_stacks + 1) * min_plot_rows <= rows_left)
      num_plot_stacks++;
  }

  // Now compute the interface window positions

  // Device Information Header
  unsigned cols_header_left = cols - num_device_per_row * device_header_cols;
  bool space_between_header_col = false;
  bool space_before_header = false;
  if (cols_header_left > num_device_per_row) {
    space_between_header_col = true;
    cols_header_left -= num_device_per_row - 1;
  }
  if (cols_header_left > 0)
    space_before_header = true;

  unsigned num_this_row = 0;
  unsigned headerPosX = space_before_header;
  unsigned headerPosY = 0;
  for (unsigned i = 0; i < num_devices; ++i) {
    device_positions[i].posX = headerPosX;
    device_positions[i].posY = headerPosY;
    device_positions[i].sizeX = device_header_cols;
    device_positions[i].sizeY = device_header_rows;
    num_this_row++;
    if (num_this_row == num_device_per_row) {
      headerPosX = space_before_header;
      headerPosY += device_header_rows + space_between_header_stack;
      num_this_row = 0;
    } else {
      headerPosX += device_header_cols + space_between_header_col;
    }
  }

  unsigned rows_left_for_process = 0;
  if (*num_plots == 0) {
    *plot_positions = NULL;
  } else {
    *plot_positions = malloc(*num_plots * sizeof(**plot_positions));
    *plot_types = preferred_plot_type;
    unsigned rows_per_stack = rows_left / num_plot_stacks;
    if (rows_per_stack > 23)
      rows_per_stack = 23;
    unsigned plot_per_row = *num_plots / num_plot_stacks;
    unsigned num_plot_done = 0;
    unsigned currentPosX = 0, currentPosY = rows_for_header;
    for (unsigned i = 0; i < num_plot_stacks; ++i) {
      unsigned plot_in_this_row = min(*num_plots - num_plot_done, plot_per_row);
      unsigned cols_per_plot = cols / plot_in_this_row;
      if (*plot_types == plot_gpu_duo)
        cols_per_plot -= (cols_per_plot - cols_needed_box_drawing) %
                         (2 * num_info_per_device);
      else
        cols_per_plot -=
            (cols_per_plot - cols_needed_box_drawing) % num_info_per_device;
      unsigned extra_cols = cols - cols_per_plot * plot_per_row;
      unsigned cols_between_plots =
          extra_cols / (plot_in_this_row <= 1 ? 1 : plot_in_this_row - 1);
      for (unsigned j = 0; j < plot_in_this_row; ++j) {
        (*plot_positions)[num_plot_done].posX = currentPosX;
        (*plot_positions)[num_plot_done].posY = currentPosY;
        (*plot_positions)[num_plot_done].sizeX = cols_per_plot;
        (*plot_positions)[num_plot_done].sizeY = rows_per_stack;
        currentPosX += cols_per_plot + cols_between_plots;
        num_plot_done++;
      }
      currentPosY += rows_per_stack;
      currentPosX = 0;
    }
    rows_left_for_process = rows_left - rows_per_stack * num_plot_stacks;
  }

  process_position->posX = 0;
  process_position->posY = rows - rows_for_process - rows_left_for_process;
  process_position->sizeY = rows_for_process + rows_left_for_process;
  process_position->sizeX = cols;
}
