#include "nvtop/interface_layout_selection.h"
#include "nvtop/interface.h"
#include "nvtop/interface_options.h"

#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define max(a, b) ((a) > (b) ? (a) : (b))
#define min(a, b) ((a) < (b) ? (a) : (b))

static unsigned min_rows_taken_by_process(unsigned rows, unsigned num_devices) {
  return 1 + max(5, min(rows / 4, num_devices * 3));
}

static const unsigned cols_needed_box_drawing = 5;
static const unsigned min_plot_rows = 7;
static unsigned min_plot_cols(unsigned num_data_info_to_plot) {
  return cols_needed_box_drawing + 10 * num_data_info_to_plot;
}

// The merge works as follows:
// Try to merge consecutive devices pairs, i.e. (0,1),(1,2), ..., (n-1,n)
// And then every 2 separated devices pairs, i.e. (0,2),(1,3), ... ,(n-2,n)
// And then every p separated devices pairs, i.e. (0,p),(1,p), ... ,(n-p,n)
static bool who_to_merge(unsigned n_th_merge, unsigned devices_count,
                         const plot_info_to_draw to_draw[devices_count],
                         unsigned merge_ids[2]) {
  unsigned valid_merge_encountered = 0;
  unsigned merge1 = 0, merge2;
  unsigned separation = 1;

  while (true) {
    merge2 = merge1 + separation;
    if (merge2 < devices_count) {
      unsigned dev1_info_count = plot_count_draw_info(to_draw[merge1]);
      unsigned dev2_info_count = plot_count_draw_info(to_draw[merge2]);
      if (dev1_info_count > 0 && dev2_info_count > 0 &&
          dev1_info_count + dev2_info_count <= 4) {
        if (valid_merge_encountered == n_th_merge) {
          merge_ids[0] = merge1;
          merge_ids[1] = merge2;
          return true;
        }
        valid_merge_encountered++;
      }
      merge1++;
    } else {
      merge1 = 0;
      separation++;
      // Has exhausted all possibilities
      if (separation >= devices_count)
        return false;
    }
  }
}

static bool move_plot_to_stack(unsigned stack_max_cols, unsigned plot_id,
                               unsigned destination_stack, unsigned plot_count,
                               unsigned stack_count,
                               const unsigned num_info_per_plot[plot_count],
                               unsigned cols_allocated_in_stacks[stack_count],
                               unsigned plot_in_stack[plot_count]) {
  if (plot_in_stack[plot_id] == destination_stack)
    return false;
  unsigned cols_used_by_plot_id = min_plot_cols(num_info_per_plot[plot_id]);
  unsigned cols_after_merge =
      cols_allocated_in_stacks[destination_stack] + cols_used_by_plot_id;
  if (cols_after_merge > stack_max_cols) {
    return false;
  } else {
    cols_allocated_in_stacks[plot_in_stack[plot_id]] -= cols_used_by_plot_id;
    cols_allocated_in_stacks[destination_stack] += cols_used_by_plot_id;
    plot_in_stack[plot_id] = destination_stack;
    return true;
  }
}

static unsigned info_in_plot(unsigned plot_id, unsigned devices_count,
                             const unsigned map_device_to_plot[devices_count],
                             const plot_info_to_draw to_draw[devices_count]) {
  unsigned sum = 0;
  for (unsigned dev_id = 0; dev_id < devices_count; ++dev_id) {
    if (map_device_to_plot[dev_id] == plot_id)
      sum += plot_count_draw_info(to_draw[dev_id]);
  }
  return sum;
}

static unsigned cols_used_by_stack(unsigned stack_id, unsigned plot_count,
                                   const unsigned num_info_per_plot[plot_count],
                                   const unsigned plot_in_stack[plot_count]) {
  unsigned sum = 0;
  for (unsigned plot_id = 0; plot_id < plot_count; ++plot_id) {
    if (plot_in_stack[plot_id] == stack_id)
      sum += min_plot_cols(num_info_per_plot[plot_id]);
  }
  return sum;
}

static unsigned
size_differences_between_stacks(unsigned plot_count, unsigned stack_count,
                                unsigned cols_allocated_in_stacks[plot_count]) {
  unsigned sum = 0;
  for (unsigned i = 0; i < stack_count; ++i) {
    for (unsigned j = i + 1; j < stack_count; ++j) {
      if (cols_allocated_in_stacks[i] > cols_allocated_in_stacks[j]) {
        sum += cols_allocated_in_stacks[i] - cols_allocated_in_stacks[j];
      } else {
        sum += cols_allocated_in_stacks[j] - cols_allocated_in_stacks[i];
      }
    }
  }
  return sum;
}

static void
preliminary_plot_positioning(unsigned rows_for_plots, unsigned plot_total_cols,
                             unsigned devices_count,
                             const plot_info_to_draw to_draw[devices_count],
                             unsigned map_device_to_plot[devices_count],
                             unsigned plot_in_stack[devices_count],
                             unsigned *num_plots, unsigned *plot_stack_count) {

  // Used to handle the merging process
  unsigned num_info_per_devices[MAX_CHARTS];
  unsigned how_many_to_merge = 0;

  bool plot_anything = false;
  for (unsigned i = 0; i < devices_count; ++i) {
    num_info_per_devices[i] = plot_count_draw_info(to_draw[i]);
    if (num_info_per_devices[i])
      plot_anything = true;
  }

  // Get the most packed configuration possible with one chart per device if
  // possible.
  // If there is not enough place, merge the charts and retry.
  unsigned num_plot_stacks = 0;
  bool search_a_window_configuration =
      plot_anything && rows_for_plots >= min_plot_rows;
  while (search_a_window_configuration) {
    search_a_window_configuration = false;
    unsigned plot_id = 0;
    num_plot_stacks = 1;
    unsigned cols_used_in_stack = 0;
    unsigned rows_left_to_allocate = rows_for_plots - min_plot_rows;

    for (unsigned i = 0; i < devices_count; ++i) {
      unsigned num_info_for_this_plot = num_info_per_devices[i];
      if (num_info_for_this_plot == 0)
        continue;

      unsigned cols_this_plot = min_plot_cols(num_info_for_this_plot);
      // If there is enough horizontal space left, allocate side by side
      if (plot_total_cols >= cols_this_plot + cols_used_in_stack) {
        cols_used_in_stack += cols_this_plot;
        plot_in_stack[plot_id] = num_plot_stacks - 1;
        map_device_to_plot[i] = plot_id;
        plot_id++;
      } else {
        // This plot is too wide for an empty stack, abort
        if (cols_used_in_stack == 0) {
          num_plot_stacks = 0;
          break;
        }
        // Else allocate a new stack and retry
        if (rows_left_to_allocate >= min_plot_rows) {
          rows_left_to_allocate -= min_plot_rows;
          num_plot_stacks++;
          cols_used_in_stack = 0;
          i--;
        } else { // Not enough space for a stack: retry and merge one more
          unsigned to_merge[2];
          if (who_to_merge(how_many_to_merge, devices_count, to_draw,
                           to_merge)) {
            num_info_per_devices[to_merge[0]] +=
                num_info_per_devices[to_merge[1]];
            num_info_per_devices[to_merge[1]] = 0;
            how_many_to_merge++;
            search_a_window_configuration = true;
          } else { // No merge left
            num_plot_stacks = 0;
          }
          break;
        }
      }
    }
  }

  // Compute the number of plots, the mapping and the size
  *num_plots = 0;
  *plot_stack_count = num_plot_stacks;
  if (num_plot_stacks > 0) {
    for (unsigned i = 0; i < devices_count; ++i) {
      if (num_info_per_devices[i]) {
        map_device_to_plot[i] = *num_plots;
        *num_plots += 1;
      }
    }
    // For the devices that were merged
    for (unsigned i = 0; i < how_many_to_merge; ++i) {
      unsigned to_merge[2];
      who_to_merge(i, devices_count, to_draw, to_merge);
      map_device_to_plot[to_merge[1]] = map_device_to_plot[to_merge[0]];
    }
  }
}

static void balance_info_on_stacks_preserving_plot_order(
    unsigned stack_max_cols, unsigned stack_count, unsigned plot_count,
    unsigned num_info_per_plot[plot_count],
    unsigned cols_allocated_in_stacks[stack_count],
    unsigned plot_in_stack[plot_count]) {
  if (stack_count > plot_count) {
    stack_count = plot_count;
  }
  unsigned moving_plot_id = plot_count - 1;
  while (moving_plot_id < plot_count) {
    unsigned to_stack = plot_in_stack[moving_plot_id] + 1;
    if (to_stack < stack_count) {
      unsigned diff_sum_before = size_differences_between_stacks(
          plot_count, stack_count, cols_allocated_in_stacks);
      unsigned stack_before = plot_in_stack[moving_plot_id];
      if (move_plot_to_stack(stack_max_cols, moving_plot_id, to_stack,
                             plot_count, stack_count, num_info_per_plot,
                             cols_allocated_in_stacks, plot_in_stack)) {
        unsigned diff_sum_after = size_differences_between_stacks(
            plot_count, stack_count, cols_allocated_in_stacks);
        if (diff_sum_after <= diff_sum_before) {
          moving_plot_id = plot_count;
        } else {
          // Move back
          move_plot_to_stack(stack_max_cols, moving_plot_id, stack_before,
                             plot_count, stack_count, num_info_per_plot,
                             cols_allocated_in_stacks, plot_in_stack);
        }
      }
    }
    moving_plot_id--;
  }
}

void compute_sizes_from_layout(
    unsigned devices_count, unsigned device_header_rows,
    unsigned device_header_cols, unsigned rows, unsigned cols,
    const plot_info_to_draw to_draw[devices_count],
    process_field_displayed process_displayed,
    struct window_position device_positions[devices_count], unsigned *num_plots,
    struct window_position plot_positions[MAX_CHARTS],
    unsigned map_device_to_plot[devices_count],
    struct window_position *process_position,
    struct window_position *setup_position) {

  unsigned min_rows_for_header = 0, header_stacks = 0, num_device_per_row = 0;
  num_device_per_row = max(1, cols / device_header_cols);
  header_stacks = devices_count / num_device_per_row +
                  ((devices_count % num_device_per_row) > 0);
  if (devices_count % header_stacks == 0)
    num_device_per_row = devices_count / header_stacks;
  min_rows_for_header = header_stacks * device_header_rows;

  unsigned min_rows_for_process =
      process_field_displayed_count(process_displayed)
          ? min_rows_taken_by_process(rows, devices_count)
          : 0;

  // Not enough room for the header and process
  if (rows < min_rows_for_header + min_rows_for_process) {
    if (rows >= min_rows_for_header + 2 &&
        process_field_displayed_count(process_displayed)) { // Shrink process
      min_rows_for_process = rows - min_rows_for_header;
    } else { // Only header if possible
      min_rows_for_header = rows;
      min_rows_for_process = 0;
    }
  }
  unsigned rows_for_header = min_rows_for_header;
  unsigned rows_for_process = min_rows_for_process;
  unsigned rows_for_plots = rows - min_rows_for_header - min_rows_for_process;

  unsigned num_plot_stacks = 0;
  unsigned plot_in_stack[MAX_CHARTS];
  preliminary_plot_positioning(rows_for_plots, cols, devices_count, to_draw,
                               map_device_to_plot, plot_in_stack, num_plots,
                               &num_plot_stacks);

  // Transfer some lines to the header to separate the devices
  unsigned transferable_lines =
      rows_for_plots - num_plot_stacks * min_plot_rows;
  unsigned space_for_header = header_stacks == 0 ? 0 : header_stacks - 1;
  bool space_between_header_stack = false;
  if (transferable_lines >= space_for_header) {
    rows_for_header += space_for_header;
    rows_for_plots -= space_for_header;
    space_between_header_stack = true;
  }

  // Allocate additional plot stacks if there is enough vertical room
  if (num_plot_stacks > 0) {
    while (num_plot_stacks < *num_plots &&
           rows_for_plots / (num_plot_stacks + 1) >= 11 &&
           (num_plot_stacks + 1) * min_plot_rows <= rows_for_plots)
      num_plot_stacks++;
  }

  // Compute the cols used in each stacks to prepare balancing
  unsigned num_info_per_plot[MAX_CHARTS];
  for (unsigned i = 0; i < *num_plots; ++i) {
    num_info_per_plot[i] =
        info_in_plot(i, devices_count, map_device_to_plot, to_draw);
  }
  unsigned cols_allocated_in_stacks[MAX_CHARTS];
  for (unsigned i = 0; i < num_plot_stacks; ++i) {
    cols_allocated_in_stacks[i] =
        cols_used_by_stack(i, *num_plots, num_info_per_plot, plot_in_stack);
  }

  // Keep the plot order of apparition, but spread the plot on different stacks
  balance_info_on_stacks_preserving_plot_order(
      cols, num_plot_stacks, *num_plots, num_info_per_plot,
      cols_allocated_in_stacks, plot_in_stack);

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
  for (unsigned i = 0; i < devices_count; ++i) {
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
  if (*num_plots > 0) {
    unsigned rows_per_stack = rows_for_plots / num_plot_stacks;
    if (rows_per_stack > 23)
      rows_per_stack = 23;
    unsigned num_plot_done = 0;
    unsigned currentPosX = 0, currentPosY = rows_for_header;
    for (unsigned stack_id = 0; stack_id < num_plot_stacks; ++stack_id) {
      unsigned plot_in_this_stack = 0;
      unsigned lines_to_draw = 0;
      for (unsigned j = 0; j < *num_plots; ++j) {
        if (plot_in_stack[j] == stack_id) {
          plot_in_this_stack++;
          lines_to_draw += num_info_per_plot[j];
        }
      }
      unsigned cols_for_line_drawing =
          cols - plot_in_this_stack * cols_needed_box_drawing;
      for (unsigned j = 0; j < *num_plots; ++j) {
        if (plot_in_stack[j] == stack_id) {
          unsigned max_plot_cols =
              cols_needed_box_drawing +
              cols_for_line_drawing * num_info_per_plot[j] / lines_to_draw;
          unsigned plot_cols =
              max_plot_cols -
              (max_plot_cols - cols_needed_box_drawing) % num_info_per_plot[j];
          plot_positions[num_plot_done].posX = currentPosX;
          plot_positions[num_plot_done].posY = currentPosY;
          plot_positions[num_plot_done].sizeX = plot_cols;
          plot_positions[num_plot_done].sizeY = rows_per_stack;
          currentPosX += max_plot_cols;
          num_plot_done++;
        }
      }
      currentPosY += rows_per_stack;
      currentPosX = 0;
    }
    if (process_field_displayed_count(process_displayed) > 0)
      rows_left_for_process = rows_for_plots - rows_per_stack * num_plot_stacks;
  } else {
    // No plot displayed, allocate the leftover space to the processes
    if (process_field_displayed_count(process_displayed) > 0)
      rows_for_process += rows_for_plots - 1;
  }

  process_position->posX = 0;
  process_position->posY = rows - rows_for_process - rows_left_for_process;
  process_position->sizeY = rows_for_process + rows_left_for_process;
  process_position->sizeX = cols;

  setup_position->posX = 0;
  setup_position->posY = rows_for_header;
  setup_position->sizeY = rows - rows_for_header;
  setup_position->sizeX = cols;
}
