/*
 *
 * Copyright (C) 2017-2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/interface.h"
#include "nvtop/common.h"
#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/interface_common.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/interface_layout_selection.h"
#include "nvtop/interface_options.h"
#include "nvtop/interface_ring_buffer.h"
#include "nvtop/interface_setup_win.h"
#include "nvtop/plot.h"
#include "nvtop/time.h"

#include <assert.h>
#include <inttypes.h>
#include <ncurses.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <unistd.h>

static unsigned int sizeof_device_field[device_field_count] = {
    [device_name] = 11,  [device_fan_speed] = 8, [device_temperature] = 10,
    [device_power] = 15, [device_clock] = 11,    [device_pcie] = 46,
    [device_shadercores] = 7, [device_l2features] = 11, [device_execengines] = 11,
};

static unsigned int sizeof_process_field[process_field_count] = {
    [process_pid] = 7,       [process_user] = 4,          [process_gpu_id] = 3,   [process_type] = 8,
    [process_gpu_rate] = 4,  [process_enc_rate] = 4,      [process_dec_rate] = 4,
    [process_memory] = 14, // 9 for mem 5 for %
    [process_cpu_usage] = 6, [process_cpu_mem_usage] = 9, [process_command] = 0,
};

static void alloc_device_window(unsigned int start_row, unsigned int start_col, unsigned int totalcol,
                                struct device_window *dwin) {

  const unsigned int spacer = 1;

  // Line 1 = Name | PCIe info

  dwin->name_win = newwin(1, sizeof_device_field[device_name], start_row, start_col);
  if (dwin->name_win == NULL)
    goto alloc_error;
  dwin->pcie_info =
      newwin(1, sizeof_device_field[device_pcie], start_row, start_col + spacer + sizeof_device_field[device_name]);
  if (dwin->pcie_info == NULL)
    goto alloc_error;

  // Line 2 = GPU clk | MEM clk | Temp | Fan | Power
  dwin->gpu_clock_info = newwin(1, sizeof_device_field[device_clock], start_row + 1, start_col);
  if (dwin->gpu_clock_info == NULL)
    goto alloc_error;
  dwin->mem_clock_info = newwin(1, sizeof_device_field[device_clock], start_row + 1,
                                start_col + spacer + sizeof_device_field[device_clock]);
  if (dwin->mem_clock_info == NULL)
    goto alloc_error;
  dwin->temperature = newwin(1, sizeof_device_field[device_temperature], start_row + 1,
                             start_col + spacer * 2 + sizeof_device_field[device_clock] * 2);
  if (dwin->temperature == NULL)
    goto alloc_error;
  dwin->fan_speed =
      newwin(1, sizeof_device_field[device_fan_speed], start_row + 1,
             start_col + spacer * 3 + sizeof_device_field[device_clock] * 2 + sizeof_device_field[device_temperature]);
  if (dwin->fan_speed == NULL)
    goto alloc_error;
  dwin->power_info = newwin(1, sizeof_device_field[device_power], start_row + 1,
                            start_col + spacer * 4 + sizeof_device_field[device_clock] * 2 +
                                sizeof_device_field[device_temperature] + sizeof_device_field[device_fan_speed]);
  if (dwin->power_info == NULL)
    goto alloc_error;

  // Line 3 = GPU used | MEM used | Encoder | Decoder

  int remaining_cols = totalcol - 3 * spacer;
  int size_gpu, size_mem, size_encode, size_decode;
  int quot, rem;
  quot = remaining_cols / 3;
  rem = remaining_cols % 3;
  size_gpu = size_mem = size_encode = quot;
  switch (rem) {
  case 2:
    if (size_encode % 2 == 1)
      size_encode += 1;
    else
      size_mem += 1;
    /* Falls through */
  case 1:
    size_gpu += 1;
    break;
  }

  if (size_encode % 2 == 1) {
    size_mem += 1;
    size_encode -= 1;
  }

  if (size_encode / 2 < 14) {
    size_encode += 2;
    size_gpu -= 1;
    size_mem -= 1;
  }

  size_decode = size_encode / 2;
  if (size_encode % 2 == 1)
    size_encode += 1;
  size_encode /= 2;

  dwin->gpu_util_enc_dec = newwin(1, size_gpu, start_row + 2, start_col);
  if (dwin->gpu_util_enc_dec == NULL)
    goto alloc_error;
  dwin->mem_util_enc_dec = newwin(1, size_mem, start_row + 2, start_col + spacer + size_gpu);
  if (dwin->mem_util_enc_dec == NULL)
    goto alloc_error;
  dwin->encode_util = newwin(1, size_encode, start_row + 2, start_col + spacer * 2 + size_gpu + size_mem);
  if (dwin->encode_util == NULL)
    goto alloc_error;
  dwin->decode_util = newwin(1, size_decode, start_row + 2, start_col + spacer * 3 + size_gpu + size_mem + size_encode);
  if (dwin->decode_util == NULL)
    goto alloc_error;
  // For auto-hide encode / decode window
  dwin->gpu_util_no_enc_or_dec = newwin(1, size_gpu + size_encode / 2 + 1, start_row + 2, start_col);
  if (dwin->gpu_util_no_enc_or_dec == NULL)
    goto alloc_error;
  dwin->mem_util_no_enc_or_dec =
      newwin(1, size_mem + size_encode / 2, start_row + 2, start_col + spacer + size_gpu + size_encode / 2 + 1);
  if (dwin->mem_util_no_enc_or_dec == NULL)
    goto alloc_error;
  dwin->gpu_util_no_enc_and_dec = newwin(1, size_gpu + size_encode + 1, start_row + 2, start_col);
  if (dwin->gpu_util_no_enc_and_dec == NULL)
    goto alloc_error;
  dwin->mem_util_no_enc_and_dec =
      newwin(1, size_mem + size_encode + 1, start_row + 2, start_col + spacer + size_gpu + size_encode + 1);
  if (dwin->mem_util_no_enc_and_dec == NULL)
    goto alloc_error;
  dwin->enc_was_visible = false;
  dwin->dec_was_visible = false;

  // Line 4 = Number of shading cores | L2 Features
  dwin->shader_cores =
    newwin(1, sizeof_device_field[device_shadercores], start_row + 3, start_col);
  if (dwin->shader_cores == NULL)
    goto alloc_error;
  dwin->l2_cache_size =
    newwin(1, sizeof_device_field[device_l2features], start_row + 3, start_col + spacer +
           sizeof_device_field[device_shadercores]);
  if (dwin->l2_cache_size == NULL)
    goto alloc_error;
  dwin->exec_engines =
    newwin(1, sizeof_device_field[device_execengines], start_row + 3, start_col + spacer * 2 +
           sizeof_device_field[device_shadercores] + sizeof_device_field[device_l2features]);
  if (dwin->exec_engines == NULL)
    goto alloc_error;

  return;
alloc_error:
  endwin();
  fprintf(stderr, "Error: Not enough columns to draw device information\n");
  exit(EXIT_FAILURE);
}

static void free_device_windows(struct device_window *dwin) {
  delwin(dwin->name_win);
  delwin(dwin->gpu_util_enc_dec);
  delwin(dwin->mem_util_enc_dec);
  delwin(dwin->gpu_util_no_enc_or_dec);
  delwin(dwin->mem_util_no_enc_or_dec);
  delwin(dwin->gpu_util_no_enc_and_dec);
  delwin(dwin->mem_util_no_enc_and_dec);
  delwin(dwin->encode_util);
  delwin(dwin->decode_util);
  delwin(dwin->gpu_clock_info);
  delwin(dwin->mem_clock_info);
  delwin(dwin->power_info);
  delwin(dwin->temperature);
  delwin(dwin->fan_speed);
  delwin(dwin->pcie_info);
}

static void alloc_process_with_option(struct nvtop_interface *interface, unsigned posX, unsigned posY, unsigned sizeX,
                                      unsigned sizeY) {
  if (sizeY > 0) {
    interface->process.process_win = newwin(sizeY, sizeX, posY, posX);
    interface->process.process_with_option_win =
        newwin(sizeY, sizeX - option_window_size, posY, posX + option_window_size);
  } else {
    interface->process.process_win = NULL;
    interface->process.process_with_option_win = NULL;
  }
  interface->process.selected_row = 0;
  interface->process.selected_pid = -1;
  interface->process.offset_column = 0;
  interface->process.offset = 0;

  interface->process.option_window.option_win = newwin(sizeY, option_window_size, posY, posX);

  interface->process.option_window.state = nvtop_option_state_hidden;
  interface->process.option_window.previous_state = nvtop_option_state_sort_by;
  interface->process.option_window.offset = 0;
  interface->process.option_window.selected_row = 0;
}

static void initialize_gpu_mem_plot(struct plot_window *plot, struct window_position *position,
                                    nvtop_interface_option *options) {
  unsigned rows = position->sizeY;
  unsigned cols = position->sizeX;
  cols -= 5;
  rows -= 2;
  plot->plot_window = newwin(rows, cols, position->posY + 1, position->posX + 4);
  draw_rectangle(plot->win, 3, 0, cols + 2, rows + 2);
  mvwprintw(plot->win, 1 + rows * 3 / 4, 0, " 25");
  mvwprintw(plot->win, 1 + rows / 4, 0, " 75");
  mvwprintw(plot->win, 1 + rows / 2, 0, " 50");
  mvwprintw(plot->win, 1, 0, "100");
  mvwprintw(plot->win, rows, 0, "  0");
  plot->data = calloc(cols, sizeof(*plot->data));
  plot->num_data = cols;

  unsigned column_divisor = 0;
  for (unsigned i = 0; i < plot->num_devices_to_plot; ++i) {
    unsigned dev_id = plot->devices_ids[i];
    plot_info_to_draw to_draw = options->gpu_specific_opts[dev_id].to_draw;
    column_divisor += plot_count_draw_info(to_draw);
  }
  assert(column_divisor > 0);
  char elapsedSeconds[5];
  char *err = "err";
  char *zeroSec = "0s";
  if (options->plot_left_to_right) {
    char *toPrint = zeroSec;
    mvwprintw(plot->win, position->sizeY - 1, 4, "%s", toPrint);

    int retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / 4 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols / 4 - strlen(toPrint) / 2, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / 2 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols / 2 - strlen(toPrint) / 2, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols * 3 / 4 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols * 3 / 4 - strlen(toPrint) / 2, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols - strlen(toPrint), "%s", toPrint);
  } else {
    char *toPrint;
    int retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols * 3 / 4 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols / 4 - strlen(toPrint) / 2, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / 2 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols / 2 - strlen(toPrint) / 2, "%s", toPrint);

    retval = snprintf(elapsedSeconds, 5, "%ds", options->update_interval * cols / 4 / column_divisor / 1000);
    if (retval > 4)
      toPrint = err;
    else
      toPrint = elapsedSeconds;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols * 3 / 4 - strlen(toPrint) / 2, "%s", toPrint);

    toPrint = zeroSec;
    mvwprintw(plot->win, position->sizeY - 1, 4 + cols - strlen(toPrint), "%s", toPrint);
  }
  wnoutrefresh(plot->win);
}

static void alloc_plot_window(unsigned devices_count, struct window_position *plot_positions,
                              unsigned map_device_to_plot[devices_count], struct nvtop_interface *interface) {
  if (!interface->num_plots) {
    interface->plots = NULL;
    return;
  }
  interface->plots = malloc(interface->num_plots * sizeof(*interface->plots));
  for (size_t i = 0; i < interface->num_plots; ++i) {
    interface->plots[i].num_devices_to_plot = 0;
    for (unsigned dev_id = 0; dev_id < devices_count; ++dev_id) {
      if (map_device_to_plot[dev_id] == i) {
        interface->plots[i].devices_ids[interface->plots[i].num_devices_to_plot] = dev_id;
        interface->plots[i].num_devices_to_plot++;
      }
    }
    interface->plots[i].win =
        newwin(plot_positions[i].sizeY, plot_positions[i].sizeX, plot_positions[i].posY, plot_positions[i].posX);
    initialize_gpu_mem_plot(&interface->plots[i], &plot_positions[i], &interface->options);
  }
}

static unsigned device_length(void) {
  return max(sizeof_device_field[device_name] + sizeof_device_field[device_pcie] + 1,

             2 * sizeof_device_field[device_clock] + sizeof_device_field[device_temperature] +
                 sizeof_device_field[device_fan_speed] + sizeof_device_field[device_power] + 4);
}

static pid_t nvtop_pid;

static void initialize_all_windows(struct nvtop_interface *dwin) {
  int rows, cols;
  getmaxyx(stdscr, rows, cols);

  unsigned int devices_count = dwin->monitored_dev_count;

  struct window_position device_positions[devices_count];
  unsigned map_device_to_plot[devices_count];
  struct window_position process_position;
  struct window_position plot_positions[MAX_CHARTS];
  struct window_position setup_position;

  compute_sizes_from_layout(devices_count, dwin->options.has_gpu_info_bar ? 4 : 3, device_length(), rows - 1, cols,
                            dwin->options.gpu_specific_opts, dwin->options.process_fields_displayed,
                            device_positions, &dwin->num_plots, plot_positions,
                            map_device_to_plot, &process_position, &setup_position, dwin->options.hide_processes_list);

  alloc_plot_window(devices_count, plot_positions, map_device_to_plot, dwin);

  for (unsigned int i = 0; i < devices_count; ++i) {
    alloc_device_window(device_positions[i].posY, device_positions[i].posX, device_positions[i].sizeX,
                        &dwin->devices_win[i]);
  }

  alloc_process_with_option(dwin, process_position.posX, process_position.posY, process_position.sizeX,
                            process_position.sizeY);

  dwin->shortcut_window = newwin(1, cols, rows - 1, 0);

  alloc_setup_window(&setup_position, &dwin->setup_win);
  nvtop_pid = getpid();
}

static void delete_all_windows(struct nvtop_interface *dwin) {
  for (unsigned int i = 0; i < dwin->monitored_dev_count; ++i) {
    free_device_windows(&dwin->devices_win[i]);
  }
  delwin(dwin->process.process_win);
  delwin(dwin->process.process_with_option_win);
  dwin->process.process_win = NULL;
  dwin->process.process_with_option_win = NULL;
  delwin(dwin->shortcut_window);
  delwin(dwin->process.option_window.option_win);
  for (size_t i = 0; i < dwin->num_plots; ++i) {
    delwin(dwin->plots[i].win);
    free(dwin->plots[i].data);
  }
  free_setup_window(&dwin->setup_win);
  free(dwin->plots);
}

static void initialize_colors(void) {
  start_color();
  short background_color;
#ifdef NCURSES_VERSION
  if (use_default_colors() == OK)
    background_color = -1;
  else
    background_color = COLOR_BLACK;
#else
  background_color = COLOR_BLACK;
#endif
  init_pair(cyan_color, COLOR_CYAN, background_color);
  init_pair(red_color, COLOR_RED, background_color);
  init_pair(green_color, COLOR_GREEN, background_color);
  init_pair(yellow_color, COLOR_YELLOW, background_color);
  init_pair(blue_color, COLOR_BLUE, background_color);
  init_pair(magenta_color, COLOR_MAGENTA, background_color);
}

struct nvtop_interface *initialize_curses(unsigned total_devices, unsigned devices_count, unsigned largest_device_name,
                                          nvtop_interface_option options) {
  struct nvtop_interface *interface = calloc(1, sizeof(*interface));
  interface->options = options;
  interface->devices_win = calloc(devices_count, sizeof(*interface->devices_win));
  interface->total_dev_count = total_devices;
  interface->monitored_dev_count = devices_count;
  sizeof_device_field[device_name] = largest_device_name + 11;
  initscr();
  refresh();
  if (interface->options.use_color && has_colors() == TRUE) {
    initialize_colors();
  }
  cbreak();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);

  // Hide decode and encode if not active for some time
  if (interface->options.encode_decode_hiding_timer > 0.) {
    nvtop_time time_now, some_time_in_past;
    some_time_in_past =
        nvtop_hmns_to_time(0, (unsigned int)(interface->options.encode_decode_hiding_timer / 60.) + 1, 0);

    nvtop_get_current_time(&time_now);
    some_time_in_past = nvtop_substract_time(time_now, some_time_in_past);
    for (size_t i = 0; i < devices_count; ++i) {
      interface->devices_win[i].last_encode_seen = some_time_in_past;
      interface->devices_win[i].last_decode_seen = some_time_in_past;
    }
  }

  interface_alloc_ring_buffer(devices_count, 4, 10 * 60 * 1000, &interface->saved_data_ring);
  initialize_all_windows(interface);
  return interface;
}

void clean_ncurses(struct nvtop_interface *interface) {
  endwin();
  delete_all_windows(interface);
  free(interface->options.gpu_specific_opts);
  free(interface->options.config_file_location);
  free(interface->devices_win);
  interface_free_ring_buffer(&interface->saved_data_ring);
  free(interface);
}

static void draw_percentage_meter(WINDOW *win, const char *prelude, unsigned int new_percentage,
                                  const char inside_braces_right[1024]) {
  int rows, cols;
  getmaxyx(win, rows, cols);
  (void)rows;
  size_t size_prelude = strlen(prelude);
  wcolor_set(win, cyan_color, NULL);
  mvwprintw(win, 0, 0, "%s", prelude);
  wstandend(win);
  waddch(win, '[');
  int curx, cury;
  curx = getcurx(win);
  cury = getcury(win);
  int between_sbraces = cols - size_prelude - 2;
  float usage = round((float)between_sbraces * new_percentage / 100.f);
  int represent_usage = (int)usage;
  whline(win, '|', (int)represent_usage);
  mvwhline(win, cury, curx + represent_usage, ' ', between_sbraces - represent_usage);
  mvwaddch(win, cury, curx + between_sbraces, ']');
  unsigned int right_side_braces_space_required = strlen(inside_braces_right);
  wmove(win, cury, curx + between_sbraces - right_side_braces_space_required);
  wprintw(win, "%s", inside_braces_right);
  mvwchgat(win, cury, curx, represent_usage, 0, green_color, NULL);
  wnoutrefresh(win);
}

static const char *memory_prefix[] = {" B", "Ki", "Mi", "Gi", "Ti", "Pi"};

static void draw_temp_color(WINDOW *win, unsigned int temp, unsigned int temp_slowdown, bool celsius) {
  unsigned int temp_convert;
  if (celsius)
    temp_convert = temp;
  else
    temp_convert = (unsigned)(32 + nearbyint(temp * 1.8));
  wcolor_set(win, cyan_color, NULL);
  mvwprintw(win, 0, 0, "TEMP");

  if (temp >= temp_slowdown - 5) {
    if (temp >= temp_slowdown)
      wcolor_set(win, red_color, NULL);
    else
      wcolor_set(win, yellow_color, NULL);
  } else {
    wcolor_set(win, green_color, NULL);
  }
  wprintw(win, " %3u", temp_convert);
  wstandend(win);

  waddch(win, ACS_DEGREE);
  if (celsius)
    waddch(win, 'C');
  else
    waddch(win, 'F');
  wnoutrefresh(win);
}

static void print_pcie_at_scale(WINDOW *win, unsigned int value) {
  int prefix_off;
  double val_d = value;
  for (prefix_off = 1; prefix_off < 5 && val_d >= 1000.; ++prefix_off) {
    val_d = val_d / 1024.;
  }
  if (val_d >= 100.) {
    wprintw(win, "%.1f", val_d);
  } else {
    if (val_d >= 10.) {
      wprintw(win, "%.2f", val_d);
    } else {
      wprintw(win, "%.3f", val_d);
    }
  }
  wprintw(win, " %sB/s", memory_prefix[prefix_off]);
}

static inline void werase_and_wnoutrefresh(WINDOW *w) {
  werase(w);
  wnoutrefresh(w);
}

static bool cleaned_enc_window(struct device_window *dev, double encode_decode_hiding_timer, nvtop_time tnow) {
  if (encode_decode_hiding_timer > 0. && nvtop_difftime(dev->last_encode_seen, tnow) > encode_decode_hiding_timer) {
    if (dev->enc_was_visible) {
      dev->enc_was_visible = false;
      if (dev->dec_was_visible) {
        werase_and_wnoutrefresh(dev->gpu_util_enc_dec);
        werase_and_wnoutrefresh(dev->mem_util_enc_dec);
      } else {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_or_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_or_dec);
      }
    }
    return true;
  } else {
    return false;
  }
}

static bool cleaned_dec_window(struct device_window *dev, double encode_decode_hiding_timer, nvtop_time tnow) {
  if (encode_decode_hiding_timer > 0. && nvtop_difftime(dev->last_decode_seen, tnow) > encode_decode_hiding_timer) {
    if (dev->dec_was_visible) {
      dev->dec_was_visible = false;
      if (dev->enc_was_visible) {
        werase_and_wnoutrefresh(dev->gpu_util_enc_dec);
        werase_and_wnoutrefresh(dev->mem_util_enc_dec);
      } else {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_or_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_or_dec);
      }
    }
    return true;
  } else {
    return false;
  }
}

static void encode_decode_show_select(struct device_window *dev, bool encode_valid, bool decode_valid,
                                      unsigned encode_rate, unsigned decode_rate, double encode_decode_hiding_timer,
                                      bool encode_decode_shared, bool *display_encode, bool *display_decode) {
  nvtop_time tnow;
  nvtop_get_current_time(&tnow);
  if (encode_valid && encode_rate > 0) {
    *display_encode = true;
    dev->last_encode_seen = tnow;
    if (!dev->enc_was_visible) {
      dev->enc_was_visible = true;
      if (!dev->dec_was_visible) {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_and_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_and_dec);
      } else {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_or_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_or_dec);
      }
    }
  } else {
    *display_encode = !cleaned_enc_window(dev, encode_decode_hiding_timer, tnow);
  }
  // If shared, rely on decode
  *display_encode = *display_encode && !encode_decode_shared;
  if (decode_valid && decode_rate > 0) {
    *display_decode = true;
    dev->last_decode_seen = tnow;
    if (!dev->dec_was_visible) {
      dev->dec_was_visible = true;
      if (!dev->enc_was_visible) {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_and_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_and_dec);
      } else {
        werase_and_wnoutrefresh(dev->gpu_util_no_enc_or_dec);
        werase_and_wnoutrefresh(dev->mem_util_no_enc_or_dec);
      }
    }
  } else {
    *display_decode = !cleaned_dec_window(dev, encode_decode_hiding_timer, tnow);
  }
}

static void draw_devices(struct list_head *devices, struct nvtop_interface *interface) {
  struct gpu_info *device;
  unsigned dev_id = 0;

  list_for_each_entry(device, devices, list) {
    struct device_window *dev = &interface->devices_win[dev_id];

    wcolor_set(dev->name_win, cyan_color, NULL);
    mvwprintw(dev->name_win, 0, 0, "Device %-2u", dev_id);
    wstandend(dev->name_win);
    if (GPUINFO_STATIC_FIELD_VALID(&device->static_info, device_name)) {
      wprintw(dev->name_win, "[%s]", device->static_info.device_name);
      wnoutrefresh(dev->name_win);
    } else {
      wprintw(dev->name_win, "[N/A]");
      wnoutrefresh(dev->name_win);
    }
    bool display_encode = false;
    bool display_decode = false;
    encode_decode_show_select(dev, GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, encoder_rate),
                              GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, decoder_rate),
                              device->dynamic_info.encoder_rate, device->dynamic_info.decoder_rate,
                              interface->options.encode_decode_hiding_timer, device->static_info.encode_decode_shared,
                              &display_encode, &display_decode);

    WINDOW *gpu_util_win;
    WINDOW *mem_util_win;
    WINDOW *encode_win = dev->encode_util;
    WINDOW *decode_win = dev->decode_util;
    if (display_encode && display_decode) {
      gpu_util_win = dev->gpu_util_enc_dec;
      mem_util_win = dev->mem_util_enc_dec;
    } else {
      if (display_encode || display_decode) {
        // If encode only, place at decode location
        encode_win = dev->decode_util;
        gpu_util_win = dev->gpu_util_no_enc_or_dec;
        mem_util_win = dev->mem_util_no_enc_or_dec;
      } else {
        gpu_util_win = dev->gpu_util_no_enc_and_dec;
        mem_util_win = dev->mem_util_no_enc_and_dec;
      }
    }
    char buff[1024];
    if (display_encode) {
      unsigned rate =
          GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, encoder_rate) ? device->dynamic_info.encoder_rate : 0;
      snprintf(buff, 1024, "%u%%", rate);
      draw_percentage_meter(encode_win, "ENC", rate, buff);
    }
    if (display_decode) {
      unsigned rate =
          GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, decoder_rate) ? device->dynamic_info.decoder_rate : 0;
      snprintf(buff, 1024, "%u%%", rate);
      if (device->static_info.encode_decode_shared)
        draw_percentage_meter(decode_win, "ENC/DEC", rate, buff);
      else
        draw_percentage_meter(decode_win, "DEC", rate, buff);
    }
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_util_rate)) {
      snprintf(buff, 1024, "%u%%", device->dynamic_info.gpu_util_rate);
      draw_percentage_meter(gpu_util_win, "GPU", device->dynamic_info.gpu_util_rate, buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_percentage_meter(gpu_util_win, "GPU", 0, buff);
    }

    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, total_memory) &&
        GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, used_memory)) {
      double total_mem = device->dynamic_info.total_memory;
      double used_mem = device->dynamic_info.used_memory;
      double total_prefixed = total_mem, used_prefixed = used_mem;
      size_t prefix_off;
      for (prefix_off = 0; prefix_off < 5 && total_prefixed >= 1000.; ++prefix_off) {
        total_prefixed /= 1024.;
        used_prefixed /= 1024.;
      }
      snprintf(buff, 1024, "%.3f%s/%.3f%s", used_prefixed, memory_prefix[prefix_off], total_prefixed,
               memory_prefix[prefix_off]);
      draw_percentage_meter(mem_util_win, "MEM", (unsigned int)(100. * used_mem / total_mem), buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_percentage_meter(mem_util_win, "MEM", 0, buff);
    }
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_temp)) {
      if (!GPUINFO_STATIC_FIELD_VALID(&device->static_info, temperature_slowdown_threshold))
        device->static_info.temperature_slowdown_threshold = 0;
      draw_temp_color(dev->temperature, device->dynamic_info.gpu_temp,
                      device->static_info.temperature_slowdown_threshold,
                      !interface->options.temperature_in_fahrenheit);
    } else {
      mvwprintw(dev->temperature, 0, 0, "TEMP N/A");
      waddch(dev->temperature, ACS_DEGREE);
      if (interface->options.temperature_in_fahrenheit)
        waddch(dev->temperature, 'F');
      else
        waddch(dev->temperature, 'C');
      mvwchgat(dev->temperature, 0, 0, 4, 0, cyan_color, NULL);
      wnoutrefresh(dev->temperature);
    }

    // FAN
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, fan_speed))
      mvwprintw(dev->fan_speed, 0, 0, "FAN %3u%%", device->dynamic_info.fan_speed);
    else if (device->static_info.integrated_graphics) {
      mvwprintw(dev->fan_speed, 0, 0, "CPU-FAN ");
      mvwchgat(dev->fan_speed, 0, 3, 5, 0, cyan_color, NULL);
    } else
      mvwprintw(dev->fan_speed, 0, 0, "FAN N/A ");
    mvwchgat(dev->fan_speed, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->fan_speed);

    // GPU CLOCK
    werase(dev->gpu_clock_info);
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_clock_speed))
      mvwprintw(dev->gpu_clock_info, 0, 0, "GPU %uMHz", device->dynamic_info.gpu_clock_speed);
    else
      mvwprintw(dev->gpu_clock_info, 0, 0, "GPU N/A MHz");

    mvwchgat(dev->gpu_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->gpu_clock_info);

    // MEM CLOCK
    werase(dev->mem_clock_info);
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_clock_speed))
      mvwprintw(dev->mem_clock_info, 0, 0, "MEM %uMHz", device->dynamic_info.mem_clock_speed);
    else
      mvwprintw(dev->mem_clock_info, 0, 0, "MEM N/A MHz");
    mvwchgat(dev->mem_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->mem_clock_info);

    // POWER
    werase(dev->power_info);
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw) &&
        GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw_max))
      mvwprintw(dev->power_info, 0, 0, "POW %3u / %3u W", device->dynamic_info.power_draw / 1000,
                device->dynamic_info.power_draw_max / 1000);
    else if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw) &&
             !GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw_max))
      mvwprintw(dev->power_info, 0, 0, "POW %3u W", device->dynamic_info.power_draw / 1000);
    else if (!GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw) &&
             GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw_max))
      mvwprintw(dev->power_info, 0, 0, "POW N/A / %3u W", device->dynamic_info.power_draw_max / 1000);
    else
      mvwprintw(dev->power_info, 0, 0, "POW N/A W");
    mvwchgat(dev->power_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->power_info);

    // PICe throughput
    werase(dev->pcie_info);
    if (device->static_info.integrated_graphics) {
      wcolor_set(dev->pcie_info, cyan_color, NULL);
      mvwprintw(dev->pcie_info, 0, 0, "Integrated GPU");
    } else {
      wcolor_set(dev->pcie_info, cyan_color, NULL);
      mvwprintw(dev->pcie_info, 0, 0, "PCIe ");
      wcolor_set(dev->pcie_info, magenta_color, NULL);
      wprintw(dev->pcie_info, "GEN ");
      wstandend(dev->pcie_info);
      if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_link_gen) &&
          GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_link_width))
        wprintw(dev->pcie_info, "%u@%2ux", device->dynamic_info.pcie_link_gen, device->dynamic_info.pcie_link_width);
      else
        wprintw(dev->pcie_info, "N/A");
    }

    wcolor_set(dev->pcie_info, magenta_color, NULL);
    wprintw(dev->pcie_info, " RX: ");
    wstandend(dev->pcie_info);
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_rx))
      print_pcie_at_scale(dev->pcie_info, device->dynamic_info.pcie_rx);
    else
      wprintw(dev->pcie_info, "N/A");
    wcolor_set(dev->pcie_info, magenta_color, NULL);
    wprintw(dev->pcie_info, " TX: ");
    wstandend(dev->pcie_info);
    if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, pcie_tx))
      print_pcie_at_scale(dev->pcie_info, device->dynamic_info.pcie_tx);
    else
      wprintw(dev->pcie_info, "N/A");

    wnoutrefresh(dev->pcie_info);

    if (interface->options.has_gpu_info_bar) {
      // Number of shader cores
      werase(dev->shader_cores);
      wcolor_set(dev->shader_cores, cyan_color, NULL);
      mvwprintw(dev->shader_cores, 0, 0, "NSHC ");
      wstandend(dev->shader_cores);
      if (GPUINFO_STATIC_FIELD_VALID(&device->static_info, n_shared_cores))
        wprintw(dev->shader_cores, "%u", device->static_info.n_shared_cores);
      else
        wprintw(dev->shader_cores, "N/A");

      wnoutrefresh(dev->shader_cores);

      // L2 cache information
      werase(dev->l2_cache_size);
      wcolor_set(dev->l2_cache_size, cyan_color, NULL);
      mvwprintw(dev->l2_cache_size, 0, 0, "L2CF ");
      wstandend(dev->l2_cache_size);
      if (GPUINFO_STATIC_FIELD_VALID(&device->static_info, l2cache_size))
        wprintw(dev->l2_cache_size, "%u", device->static_info.l2cache_size);
      else
        wprintw(dev->l2_cache_size, "N/A");

      wnoutrefresh(dev->l2_cache_size);

      // Number of execution engines
      werase(dev->exec_engines);
      wcolor_set(dev->exec_engines, cyan_color, NULL);
      mvwprintw(dev->exec_engines, 0, 0, "NEXC ");
      wstandend(dev->exec_engines);
      if (GPUINFO_STATIC_FIELD_VALID(&device->static_info, n_exec_engines))
        wprintw(dev->exec_engines, "%u", device->static_info.n_exec_engines);
      else
        wprintw(dev->exec_engines, "N/A");

      wnoutrefresh(dev->exec_engines);
    }

    dev_id++;
  }
}

typedef struct {
  unsigned processes_count;
  struct gpuid_and_process {
    unsigned gpu_id;
    struct gpu_process *process;
  } * processes;
} all_processes;

static all_processes all_processes_array(struct list_head *devices) {
  unsigned total_processes_count = 0;
  struct gpu_info *device;
  unsigned dev_id = 0;

  list_for_each_entry(device, devices, list) { total_processes_count += device->processes_count; }

  all_processes merged_devices_processes;
  merged_devices_processes.processes_count = total_processes_count;
  if (total_processes_count) {
    merged_devices_processes.processes = malloc(total_processes_count * sizeof(*merged_devices_processes.processes));
    if (!merged_devices_processes.processes) {
      perror("Cannot allocate memory: ");
      exit(EXIT_FAILURE);
    }
  } else {
    merged_devices_processes.processes = NULL;
  }

  size_t offset = 0;
  list_for_each_entry(device, devices, list) {
    for (unsigned int j = 0; j < device->processes_count; ++j) {
      merged_devices_processes.processes[offset].gpu_id = dev_id;
      merged_devices_processes.processes[offset++].process = &device->processes[j];
    }

    dev_id++;
  }

  return merged_devices_processes;
}

static int compare_pid_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return p1->process->pid >= p2->process->pid ? -1 : 1;
}

static int compare_pid_asc(const void *pp1, const void *pp2) { return compare_pid_desc(pp2, pp1); }

static int compare_username_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, user_name) && GPUINFO_PROCESS_FIELD_VALID(p2->process, user_name))
    return -strcmp(p1->process->user_name, p2->process->user_name);
  else
    return 0;
}

static int compare_username_asc(const void *pp1, const void *pp2) { return compare_username_desc(pp2, pp1); }

static int compare_process_name_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, cmdline) && GPUINFO_PROCESS_FIELD_VALID(p2->process, cmdline))
    return -strcmp(p1->process->cmdline, p2->process->cmdline);
  else
    return 0;
}

static int compare_process_name_asc(const void *pp1, const void *pp2) { return compare_process_name_desc(pp2, pp1); }

static int compare_mem_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, gpu_memory_usage) &&
      GPUINFO_PROCESS_FIELD_VALID(p2->process, gpu_memory_usage))
    return p1->process->gpu_memory_usage >= p2->process->gpu_memory_usage ? -1 : 1;
  else
    return 0;
}

static int compare_mem_usage_asc(const void *pp1, const void *pp2) { return compare_mem_usage_desc(pp2, pp1); }

static int compare_cpu_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, cpu_usage) && GPUINFO_PROCESS_FIELD_VALID(p2->process, cpu_usage))
    return p1->process->cpu_usage >= p2->process->cpu_usage ? -1 : 1;
  else
    return 0;
}

static int compare_cpu_usage_asc(const void *pp1, const void *pp2) { return compare_cpu_usage_desc(pp2, pp1); }

static int compare_cpu_mem_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, cpu_memory_res) &&
      GPUINFO_PROCESS_FIELD_VALID(p2->process, cpu_memory_res))
    return p1->process->cpu_memory_res >= p2->process->cpu_memory_res ? -1 : 1;
  else
    return 0;
}

static int compare_cpu_mem_usage_asc(const void *pp1, const void *pp2) { return compare_cpu_mem_usage_desc(pp2, pp1); }

static int compare_gpu_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return p1->gpu_id >= p2->gpu_id ? -1 : 1;
}

static int compare_gpu_asc(const void *pp1, const void *pp2) { return -compare_gpu_desc(pp1, pp2); }

static int compare_process_type_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return (p1->process->type == gpu_process_graphical) != (p2->process->type == gpu_process_graphical);
}

static int compare_process_type_asc(const void *pp1, const void *pp2) { return -compare_process_name_desc(pp1, pp2); }

static int compare_process_gpu_rate_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, gpu_usage) && GPUINFO_PROCESS_FIELD_VALID(p2->process, gpu_usage)) {
    return p1->process->gpu_usage > p2->process->gpu_usage ? -1 : 1;
  } else {
    if (GPUINFO_PROCESS_FIELD_VALID(p1->process, gpu_usage)) {
      return p1->process->gpu_usage > 0 ? -1 : 0;
    } else if (GPUINFO_PROCESS_FIELD_VALID(p2->process, gpu_usage)) {
      return p2->process->gpu_usage > 0 ? 1 : 0;
    } else {
      return 0;
    }
  }
}

static int compare_process_gpu_rate_asc(const void *pp1, const void *pp2) {
  return -compare_process_gpu_rate_desc(pp1, pp2);
}

static int compare_process_enc_rate_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, encode_usage) &&
      GPUINFO_PROCESS_FIELD_VALID(p2->process, encode_usage)) {
    return p1->process->encode_usage >= p2->process->encode_usage ? -1 : 1;
  } else {
    if (GPUINFO_PROCESS_FIELD_VALID(p1->process, encode_usage)) {
      return p1->process->encode_usage > 0 ? -1 : 0;
    } else if (GPUINFO_PROCESS_FIELD_VALID(p2->process, encode_usage)) {
      return p2->process->encode_usage > 0 ? 1 : 0;
    } else {
      return 0;
    }
  }
}

static int compare_process_enc_rate_asc(const void *pp1, const void *pp2) {
  return -compare_process_enc_rate_desc(pp1, pp2);
}

static int compare_process_dec_rate_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (GPUINFO_PROCESS_FIELD_VALID(p1->process, decode_usage) &&
      GPUINFO_PROCESS_FIELD_VALID(p2->process, decode_usage)) {
    return p1->process->decode_usage >= p2->process->decode_usage ? -1 : 1;
  } else {
    if (GPUINFO_PROCESS_FIELD_VALID(p1->process, decode_usage)) {
      return p1->process->decode_usage > 0 ? -1 : 0;
    } else if (GPUINFO_PROCESS_FIELD_VALID(p2->process, decode_usage)) {
      return p2->process->decode_usage > 0 ? 1 : 0;
    } else {
      return 0;
    }
  }
}
static int compare_process_dec_rate_asc(const void *pp1, const void *pp2) {
  return -compare_process_dec_rate_desc(pp1, pp2);
}

static void sort_process(all_processes all_procs, enum process_field criterion, bool asc_sort) {
  if (all_procs.processes_count == 0 || !all_procs.processes)
    return;
  int (*sort_fun)(const void *, const void *);
  switch (criterion) {
  case process_pid:
    if (asc_sort)
      sort_fun = compare_pid_asc;
    else
      sort_fun = compare_pid_desc;
    break;
  case process_user:
    if (asc_sort)
      sort_fun = compare_username_asc;
    else
      sort_fun = compare_username_desc;
    break;
  case process_gpu_id:
    if (asc_sort)
      sort_fun = compare_gpu_asc;
    else
      sort_fun = compare_gpu_desc;
    break;
  case process_type:
    if (asc_sort)
      sort_fun = compare_process_type_asc;
    else
      sort_fun = compare_process_type_desc;
    break;
  case process_memory:
    if (asc_sort)
      sort_fun = compare_mem_usage_asc;
    else
      sort_fun = compare_mem_usage_desc;
    break;
  case process_command:
    if (asc_sort)
      sort_fun = compare_process_name_asc;
    else
      sort_fun = compare_process_name_desc;
    break;
  case process_cpu_usage:
    if (asc_sort)
      sort_fun = compare_cpu_usage_asc;
    else
      sort_fun = compare_cpu_usage_desc;
    break;
  case process_cpu_mem_usage:
    if (asc_sort)
      sort_fun = compare_cpu_mem_usage_asc;
    else
      sort_fun = compare_cpu_mem_usage_desc;
    break;
  case process_gpu_rate:
    if (asc_sort)
      sort_fun = compare_process_gpu_rate_asc;
    else
      sort_fun = compare_process_gpu_rate_desc;
    break;
  case process_enc_rate:
    if (asc_sort)
      sort_fun = compare_process_enc_rate_asc;
    else
      sort_fun = compare_process_enc_rate_desc;
    break;
  case process_dec_rate:
    if (asc_sort)
      sort_fun = compare_process_dec_rate_asc;
    else
      sort_fun = compare_process_dec_rate_desc;
    break;
  case process_field_count:
    return;
  }
  qsort(all_procs.processes, all_procs.processes_count, sizeof(*all_procs.processes), sort_fun);
}

static void filter_out_nvtop_pid(all_processes *all_procs, struct nvtop_interface *interface) {
  if (interface->options.filter_nvtop_pid) {
    for (unsigned procId = 0; procId < all_procs->processes_count; ++procId) {
      if (all_procs->processes[procId].process->pid == nvtop_pid) {
        memmove(&all_procs->processes[procId], &all_procs->processes[procId + 1],
                (all_procs->processes_count - procId - 1) * sizeof(*all_procs->processes));
        all_procs->processes_count = all_procs->processes_count - 1;
        break;
      }
    }
  }
}

static const char *columnName[process_field_count] = {
    "PID", "USER", "DEV", "TYPE", "GPU", "ENC", "DEC", "GPU MEM", "CPU", "HOST MEM", "Command",
};

static void update_selected_offset_with_window_size(unsigned int *selected_row, unsigned int *offset,
                                                    unsigned int row_available_to_draw, unsigned int num_to_draw) {

  if (!num_to_draw)
    return;

  if (*selected_row > num_to_draw - 1)
    *selected_row = num_to_draw - 1;

  if (*offset > *selected_row)
    *offset = *selected_row;

  if (*offset + row_available_to_draw - 1 < *selected_row)
    *offset = *selected_row - row_available_to_draw + 1;

  while (row_available_to_draw > num_to_draw - *offset && *offset != 0)
    *offset -= 1;
}

#define process_buffer_line_size 8192
static char process_print_buffer[process_buffer_line_size];

static void print_processes_on_screen(all_processes all_procs, struct process_window *process,
                                      enum process_field sort_criterion, process_field_displayed fields_to_display) {
  WINDOW *win = process->option_window.state == nvtop_option_state_hidden ? process->process_win
                                                                          : process->process_with_option_win;
  struct gpuid_and_process *processes = all_procs.processes;

  unsigned int rows, cols;
  getmaxyx(win, rows, cols);
  rows -= 1;

  update_selected_offset_with_window_size(&process->selected_row, &process->offset, rows, all_procs.processes_count);
  if (process->offset_column + cols >= process_buffer_line_size)
    process->offset_column = process_buffer_line_size - cols - 1;

  size_t special_row = process->selected_row;

  char pid_str[sizeof_process_field[process_pid] + 1];
  char guid_str[sizeof_process_field[process_gpu_id] + 1];
  char memory[sizeof_process_field[process_memory] + 1];
  char cpu_percent[sizeof_process_field[process_cpu_usage] + 1];
  char cpu_mem[sizeof_process_field[process_cpu_mem_usage] + 1];

  unsigned int start_at_process = process->offset;
  unsigned int end_at_process = start_at_process + rows;

  int printed = 0;
  int column_sort_start = 0, column_sort_end = sizeof_process_field[0];
  memset(process_print_buffer, 0, sizeof(process_print_buffer));
  for (enum process_field i = process_pid; i < process_field_count; ++i) {
    if (i == sort_criterion) {
      column_sort_start = printed;
      column_sort_end =
          i == process_command ? process_buffer_line_size - 4 : column_sort_start + sizeof_process_field[i];
    }
    if (process_is_field_displayed(i, fields_to_display))
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[i], columnName[i]);
  }

  mvwprintw(win, 0, 0, "%.*s", cols, &process_print_buffer[process->offset_column]);
  wclrtoeol(win);
  mvwchgat(win, 0, 0, -1, A_STANDOUT, green_color, NULL);
  set_attribute_between(win, 0, column_sort_start - (int)process->offset_column,
                        column_sort_end - (int)process->offset_column, A_STANDOUT, cyan_color);

  int start_col_process_type = 0;
  for (enum process_field i = process_pid; i < process_type; ++i) {
    if (process_is_field_displayed(i, fields_to_display))
      start_col_process_type += sizeof_process_field[i] + 1;
  }
  int end_col_process_type = start_col_process_type + sizeof_process_field[process_type];

  static unsigned printed_last_call = 0;
  unsigned last_line_printed = 0;
  for (unsigned int i = start_at_process; i < end_at_process && i < all_procs.processes_count; ++i) {
    memset(process_print_buffer, 0, sizeof(process_print_buffer));

    printed = 0;
    if (process_is_field_displayed(process_pid, fields_to_display)) {
      size_t size =
          snprintf(pid_str, sizeof_process_field[process_pid] + 1, "%" PRIdMAX, (intmax_t)processes[i].process->pid);
      if (size == sizeof_process_field[process_pid] + 1)
        pid_str[sizeof_process_field[process_pid]] = '\0';
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size, "%*s ",
                          sizeof_process_field[process_pid], pid_str);
    }

    if (process_is_field_displayed(process_user, fields_to_display)) {
      const char *username;
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, user_name)) {
        username = processes[i].process->user_name;
      } else {
        username = "N/A";
      }

      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[process_user], username);
    }

    if (process_is_field_displayed(process_gpu_id, fields_to_display)) {
      size_t size = snprintf(guid_str, sizeof_process_field[process_gpu_id] + 1, "%u", processes[i].gpu_id);
      if (size >= sizeof_process_field[process_gpu_id] + 1)
        pid_str[sizeof_process_field[process_gpu_id]] = '\0';
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[process_gpu_id], guid_str);
    }

    if (process_is_field_displayed(process_type, fields_to_display)) {
      if (processes[i].process->type == gpu_process_graphical_compute) {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                            sizeof_process_field[process_type], "Both G+C");
      } else if (processes[i].process->type == gpu_process_graphical) {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                            sizeof_process_field[process_type], "Graphic");
      } else {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                            sizeof_process_field[process_type], "Compute");
      }
    }

    if (process_is_field_displayed(process_gpu_rate, fields_to_display)) {
      unsigned gpu_usage = 0;
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, gpu_usage)) {
        gpu_usage = processes[i].process->gpu_usage;
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%3u%% ", gpu_usage);
      } else {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "N/A  ");
      }
    }

    if (process_is_field_displayed(process_enc_rate, fields_to_display)) {
      unsigned encoder_rate = 0;
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, encode_usage)) {
        encoder_rate = processes[i].process->encode_usage;
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%3u%% ", encoder_rate);
      } else {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "N/A  ");
      }
    }

    if (process_is_field_displayed(process_dec_rate, fields_to_display)) {
      unsigned decode_rate = 0;
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, decode_usage)) {
        decode_rate = processes[i].process->decode_usage;
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%3u%% ", decode_rate);
      } else {
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "N/A  ");
      }
    }

    if (process_is_field_displayed(process_memory, fields_to_display)) {
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, gpu_memory_usage)) {
        if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, gpu_memory_percentage)) {
          snprintf(memory, 9 + 1, "%6uMiB", (unsigned)(processes[i].process->gpu_memory_usage / 1048576));
          snprintf(memory + 9, sizeof_process_field[process_memory] - 9 + 1, " %3u%%",
                   processes[i].process->gpu_memory_percentage);
        } else {
          snprintf(memory, sizeof_process_field[process_memory], "%6uMiB",
                   (unsigned)(processes[i].process->gpu_memory_usage / 1048576));
        }
      } else {
        snprintf(memory, sizeof_process_field[process_memory], "%s", "N/A");
      }
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[process_memory], memory);
    }

    if (process_is_field_displayed(process_cpu_usage, fields_to_display)) {
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, cpu_usage))
        snprintf(cpu_percent, sizeof_process_field[process_cpu_usage] + 1, "%u%%", processes[i].process->cpu_usage);
      else
        snprintf(cpu_percent, sizeof_process_field[process_cpu_usage] + 1, "   N/A");
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[process_cpu_usage], cpu_percent);
    }

    if (process_is_field_displayed(process_cpu_mem_usage, fields_to_display)) {
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, cpu_memory_res))
        snprintf(cpu_mem, sizeof_process_field[process_cpu_mem_usage] + 1, "%zuMiB",
                 processes[i].process->cpu_memory_res / 1048576);
      else
        snprintf(cpu_mem, sizeof_process_field[process_cpu_mem_usage] + 1, "N/A");
      printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%*s ",
                          sizeof_process_field[process_cpu_mem_usage], cpu_mem);
    }

    if (process_is_field_displayed(process_command, fields_to_display)) {
      if (GPUINFO_PROCESS_FIELD_VALID(processes[i].process, cmdline))
        printed += snprintf(&process_print_buffer[printed], process_buffer_line_size - printed, "%.*s",
                            process_buffer_line_size - printed, processes[i].process->cmdline);
    }

    unsigned int write_at = i - start_at_process + 1;
    mvwprintw(win, write_at, 0, "%.*s", cols, &process_print_buffer[process->offset_column]);
    unsigned row, col;
    getyx(win, row, col);
    (void)col;
    if (row == write_at)
      wclrtoeol(win);
    last_line_printed = write_at;
    if (i == special_row) {
      mvwchgat(win, write_at, 0, -1, A_STANDOUT, cyan_color, NULL);
    } else {
      if (process_is_field_displayed(process_type, fields_to_display)) {
        if (processes[i].process->type == gpu_process_graphical_compute) {
          set_attribute_between(win, write_at, start_col_process_type - (int)process->offset_column,
                                start_col_process_type - (int)process->offset_column + 4, 0, cyan_color);
          set_attribute_between(win, write_at, end_col_process_type - (int)process->offset_column - 3,
                                end_col_process_type - (int)process->offset_column - 2, 0, yellow_color);
          set_attribute_between(win, write_at, end_col_process_type - (int)process->offset_column - 1,
                                end_col_process_type - (int)process->offset_column, 0, magenta_color);
        } else if (processes[i].process->type == gpu_process_graphical) {
          set_attribute_between(win, write_at, start_col_process_type - (int)process->offset_column,
                                end_col_process_type - (int)process->offset_column, 0, yellow_color);
        } else {
          set_attribute_between(win, write_at, start_col_process_type - (int)process->offset_column,
                                end_col_process_type - (int)process->offset_column, 0, magenta_color);
        }
      }
    }
  }
  if (printed_last_call > last_line_printed) {
    for (unsigned i = last_line_printed + 1; i <= rows && i <= printed_last_call; ++i) {
      wmove(win, i, 0);
      wclrtoeol(win);
    }
  }
  printed_last_call = last_line_printed;
  wnoutrefresh(win);
}

static void update_process_option_win(struct nvtop_interface *interface);

static void draw_processes(struct list_head *devices, struct nvtop_interface *interface) {
  if (interface->options.hide_processes_list)
    return;

  if (interface->process.process_win == NULL)
    return;

  if (interface->process.option_window.state != interface->process.option_window.previous_state) {
    werase(interface->process.option_window.option_win);
    wclear(interface->process.process_win);
    wclear(interface->process.process_with_option_win);
    wnoutrefresh(interface->process.option_window.option_win);
  }
  if (interface->process.option_window.state != nvtop_option_state_hidden)
    update_process_option_win(interface);

  all_processes all_procs = all_processes_array(devices);
  filter_out_nvtop_pid(&all_procs, interface);
  sort_process(all_procs, interface->options.sort_processes_by, !interface->options.sort_descending_order);

  if (all_procs.processes_count > 0) {
    if (interface->process.selected_row >= all_procs.processes_count)
      interface->process.selected_row = all_procs.processes_count - 1;
    interface->process.selected_pid = all_procs.processes[interface->process.selected_row].process->pid;
  } else {
    interface->process.selected_row = 0;
    interface->process.selected_pid = -1;
  }

  unsigned largest_username = 4;
  for (unsigned i = 0; i < all_procs.processes_count; ++i) {
    if (GPUINFO_PROCESS_FIELD_VALID(all_procs.processes[i].process, user_name)) {
      unsigned length = strlen(all_procs.processes[i].process->user_name);
      if (length > largest_username)
        largest_username = length;
    }
  }
  sizeof_process_field[process_user] = largest_username;

  print_processes_on_screen(all_procs, &interface->process, interface->options.sort_processes_by,
                            interface->options.process_fields_displayed);
  free(all_procs.processes);
}

static const char *signalNames[] = {
    "Cancel",  "SIGHUP",    "SIGINT",  "SIGQUIT",  "SIGILL",  "SIGTRAP", "SIGABRT", "SIGBUS",
    "SIGFPE",  "SIGKILL",   "SIGUSR1", "SIGSEGV",  "SIGUSR2", "SIGPIPE", "SIGALRM", "SIGTERM",
    "SIGCHLD", "SIGCONT",   "SIGSTOP", "SIGTSTP",  "SIGTTIN", "SIGTTOU", "SIGURG",  "SIGXCPU",
    "SIGXFSZ", "SIGVTALRM", "SIGPROF", "SIGWINCH", "SIGIO",   "SIGPWR",  "SIGSYS",
};

// SIGPWR does not exist on FreeBSD or Apple, while it is a synonym for SIGINFO on Linux
#if defined(__FreeBSD__) || defined(__APPLE__)
#define SIGPWR SIGINFO
#endif

static const int signalValues[ARRAY_SIZE(signalNames)] = {
    -1,      SIGHUP,  SIGINT,  SIGQUIT,   SIGILL,  SIGTRAP,  SIGABRT, SIGBUS,  SIGFPE,  SIGKILL, SIGUSR1,
    SIGSEGV, SIGUSR2, SIGPIPE, SIGALRM,   SIGTERM, SIGCHLD,  SIGCONT, SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU,
    SIGURG,  SIGXCPU, SIGXFSZ, SIGVTALRM, SIGPROF, SIGWINCH, SIGIO,   SIGPWR,  SIGSYS,
};

static const size_t nvtop_num_signals = ARRAY_SIZE(signalNames) - 1;

static void draw_kill_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->process.option_window.option_win;
  wattr_set(win, A_REVERSE, green_color, NULL);
  mvwprintw(win, 0, 0, "Send signal:");
  wstandend(win);
  wprintw(win, " ");
  int rows, cols;
  getmaxyx(win, rows, cols);

  size_t start_at_option = interface->process.option_window.offset;
  size_t end_at_option = start_at_option + rows - 1;

  for (size_t i = start_at_option; i < end_at_option && i <= nvtop_num_signals; ++i) {
    if (i == interface->process.option_window.selected_row) {
      wattr_set(win, A_STANDOUT, cyan_color, NULL);
    }
    wprintw(win, "%*zu %s", 2, i, signalNames[i]);
    getyx(win, rows, cols);

    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (i == interface->process.option_window.selected_row) {
      wstandend(win);
      mvwprintw(win, rows, option_window_size - 1, " ");
    }
  }
  wnoutrefresh(win);
}

static void draw_sort_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->process.option_window.option_win;
  wattr_set(win, A_REVERSE, green_color, NULL);
  mvwprintw(win, 0, 0, "Sort by     ");
  wstandend(win);
  wprintw(win, " ");
  int rows, cols;
  if (interface->process.option_window.offset == 0) {
    if (interface->process.option_window.selected_row == 0) {
      wattr_set(win, A_STANDOUT, cyan_color, NULL);
    }
    wprintw(win, "Cancel");
    getyx(win, rows, cols);
    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (interface->process.option_window.selected_row == 0) {
      wstandend(win);
      mvwprintw(win, rows, option_window_size - 1, " ");
    }
  }
  getmaxyx(win, rows, cols);

  size_t start_at_option = interface->process.option_window.offset == 0 ? interface->process.option_window.offset
                                                                        : interface->process.option_window.offset - 1;
  size_t end_at_option =
      interface->process.option_window.offset == 0 ? start_at_option + rows - 2 : start_at_option + rows - 1;

  unsigned option_index = 0;
  for (enum process_field field = process_pid; field < process_field_count; ++field) {
    if (process_is_field_displayed(field, interface->options.process_fields_displayed)) {
      if (option_index >= start_at_option && option_index < end_at_option) {
        if (option_index + 1 == interface->process.option_window.selected_row) {
          wattr_set(win, A_STANDOUT, cyan_color, NULL);
        }
        wprintw(win, "%s", columnName[field]);
        getyx(win, rows, cols);
        for (unsigned int j = cols; j < option_window_size; ++j)
          wprintw(win, " ");

        if (option_index + 1 == interface->process.option_window.selected_row) {
          wstandend(win);
          mvwprintw(win, rows, option_window_size - 1, " ");
        }
      }
      option_index++;
    }
  }
  wnoutrefresh(win);
}

static void update_process_option_win(struct nvtop_interface *interface) {
  unsigned int rows, cols;
  getmaxyx(interface->process.option_window.option_win, rows, cols);
  rows -= 1;
  (void)cols;
  unsigned int num_options = 0;
  switch (interface->process.option_window.state) {
  case nvtop_option_state_kill:
    num_options = nvtop_num_signals + 1; // Option + Cancel
    break;
  case nvtop_option_state_sort_by:
    num_options = process_field_displayed_count(interface->options.process_fields_displayed) + 1; // Option + Cancel
    break;
  case nvtop_option_state_hidden:
  default:
    break;
  }

  update_selected_offset_with_window_size(&interface->process.option_window.selected_row,
                                          &interface->process.option_window.offset, rows, num_options);

  switch (interface->process.option_window.state) {
  case nvtop_option_state_kill:
    draw_kill_option(interface);
    break;
  case nvtop_option_state_sort_by:
    draw_sort_option(interface);
    break;
  case nvtop_option_state_hidden:
  default:
    break;
  }
}

static const char *option_selection_hidden[] = {
    "Setup", "Sort", "Kill", "Quit", "Save Config",
};
static const char *option_selection_hidden_num[] = {
    "2", "6", "9", "10", "12",
};

static const char *option_selection_sort[][2] = {
    {"Enter", "Sort"},
    {"ESC", "Cancel"},
    {"+", "Ascending"},
    {"-", "Descending"},
};

static const char *option_selection_kill[][2] = {
    {"Enter", "Send"},
    {"ESC", "Cancel"},
};

static const unsigned int option_selection_width = 8;

static void draw_process_shortcuts(struct nvtop_interface *interface) {
  if (interface->process.option_window.state == interface->process.option_window.previous_state)
    return;
  WINDOW *win = interface->shortcut_window;
  enum nvtop_option_window_state current_state = interface->process.option_window.state;
  wmove(win, 0, 0);
  switch (current_state) {
  case nvtop_option_state_hidden:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_hidden); ++i) {
      if (interface->options.hide_processes_list && (strcmp(option_selection_hidden_num[i], "6") == 0 || strcmp(option_selection_hidden_num[i], "9") == 0))
        continue;

      if (process_field_displayed_count(interface->options.process_fields_displayed) > 0 || (i != 1 && i != 2)) {
        wprintw(win, "F%s", option_selection_hidden_num[i]);
        wattr_set(win, A_STANDOUT, cyan_color, NULL);
        wprintw(win, "%-*s", option_selection_width, option_selection_hidden[i]);
        wstandend(win);
      }
    }
    break;
  case nvtop_option_state_kill:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_kill); ++i) {
      wprintw(win, "%s", option_selection_kill[i][0]);
      wattr_set(win, A_STANDOUT, cyan_color, NULL);
      wprintw(win, "%-*s", option_selection_width, option_selection_kill[i][1]);
      wstandend(win);
    }
    break;
  case nvtop_option_state_sort_by:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_sort); ++i) {
      wprintw(win, "%s", option_selection_sort[i][0]);
      wattr_set(win, A_STANDOUT, cyan_color, NULL);
      wprintw(win, "%-*s", option_selection_width, option_selection_sort[i][1]);
      wstandend(win);
    }
    break;
  default:
    break;
  }
  wclrtoeol(win);
  unsigned int cur_col, tmp;
  (void)tmp;
  getyx(win, tmp, cur_col);
  mvwchgat(win, 0, cur_col, -1, A_STANDOUT, cyan_color, NULL);
  wnoutrefresh(win);
  interface->process.option_window.previous_state = current_state;
}

static void draw_shortcuts(struct nvtop_interface *interface) {
  if (interface->setup_win.visible) {
    draw_setup_window_shortcuts(interface);
  } else {
    draw_process_shortcuts(interface);
  }
}

void save_current_data_to_ring(struct list_head *devices, struct nvtop_interface *interface) {
  struct gpu_info *device;
  unsigned dev_id = 0;

  list_for_each_entry(device, devices, list) {
    unsigned data_index = 0;
    for (enum plot_information info = plot_gpu_rate; info < plot_information_count; ++info) {
      if (plot_isset_draw_info(info, interface->options.gpu_specific_opts[dev_id].to_draw)) {
        unsigned data_val = 0;
        switch (info) {
        case plot_gpu_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_util_rate))
            data_val = device->dynamic_info.gpu_util_rate;
          break;
        case plot_gpu_mem_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_util_rate))
            data_val = device->dynamic_info.mem_util_rate;
          break;
        case plot_encoder_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, encoder_rate))
            data_val = device->dynamic_info.encoder_rate;
          break;
        case plot_decoder_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, decoder_rate))
            data_val = device->dynamic_info.decoder_rate;
          break;
        case plot_gpu_temperature:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_temp)) {
            data_val = device->dynamic_info.gpu_temp;
            if (data_val > 100)
              data_val = 100u;
          }
          break;
        case plot_gpu_power_draw_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw) &&
              GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, power_draw_max)) {
            data_val = device->dynamic_info.power_draw * 100 / device->dynamic_info.power_draw_max;
            if (data_val > 100)
              data_val = 100u;
          }
          break;
        case plot_fan_speed:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, fan_speed)) {
            data_val = device->dynamic_info.fan_speed;
          }
          break;
        case plot_gpu_clock_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_clock_speed) &&
              GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, gpu_clock_speed_max)) {
            data_val = device->dynamic_info.gpu_clock_speed * 100 / device->dynamic_info.gpu_clock_speed_max;
          }
          break;
        case plot_gpu_mem_clock_rate:
          if (GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_clock_speed) &&
              GPUINFO_DYNAMIC_FIELD_VALID(&device->dynamic_info, mem_clock_speed_max)) {
            data_val = device->dynamic_info.mem_clock_speed * 100 / device->dynamic_info.mem_clock_speed_max;
          }
          break;
        case plot_information_count:
          break;
        }
        interface_ring_buffer_push(&interface->saved_data_ring, dev_id, data_index, data_val);
        data_index++;
      }
    }

    dev_id++;
  }
}

static unsigned populate_plot_data_from_ring_buffer(const struct nvtop_interface *interface,
                                                    struct plot_window *plot_win, unsigned size_data_buff,
                                                    double data[size_data_buff],
                                                    char plot_legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {

  memset(data, 0, size_data_buff * sizeof(*data));
  unsigned total_to_draw = 0;
  for (unsigned i = 0; i < plot_win->num_devices_to_plot; ++i) {
    unsigned dev_id = plot_win->devices_ids[i];
    plot_info_to_draw to_draw = interface->options.gpu_specific_opts[dev_id].to_draw;
    total_to_draw += plot_count_draw_info(to_draw);
  }

  assert(total_to_draw > 0);
  assert(size_data_buff % total_to_draw == 0);
  unsigned max_data_to_copy = size_data_buff / total_to_draw;
  double(*data_split)[total_to_draw] = (double(*)[total_to_draw])data;

  unsigned in_processing = 0;
  for (unsigned i = 0; i < plot_win->num_devices_to_plot; ++i) {
    unsigned dev_id = plot_win->devices_ids[i];
    plot_info_to_draw to_draw = interface->options.gpu_specific_opts[dev_id].to_draw;
    unsigned data_ring_index = 0;
    for (enum plot_information info = plot_gpu_rate; info < plot_information_count; ++info) {
      if (plot_isset_draw_info(info, to_draw)) {
        // Populate the legend
        switch (info) {
        case plot_gpu_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u %%", dev_id);
          break;
        case plot_gpu_mem_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u mem%%", dev_id);
          break;
        case plot_encoder_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u encode%%", dev_id);
          break;
        case plot_decoder_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u decode%%", dev_id);
          break;
        case plot_gpu_temperature:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u temp(c)", dev_id);
          break;
        case plot_gpu_power_draw_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u power%%", dev_id);
          break;
        case plot_fan_speed:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u fan%%", dev_id);
          break;
        case plot_gpu_clock_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u clock%%", dev_id);
          break;
        case plot_gpu_mem_clock_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u mem clock%%", dev_id);
          break;
        case plot_information_count:
          break;
        }
        // Copy the data
        unsigned data_in_ring = interface_ring_buffer_data_stored(&interface->saved_data_ring, dev_id, data_ring_index);
        if (interface->options.plot_left_to_right) {
          for (unsigned j = 0; j < data_in_ring && j < max_data_to_copy; ++j) {
            data_split[j][in_processing] =
                interface_ring_buffer_get(&interface->saved_data_ring, dev_id, data_ring_index, data_in_ring - j - 1);
          }
        } else {
          for (unsigned j = 0; j < data_in_ring && j < max_data_to_copy; ++j) {
            data_split[max_data_to_copy - j - 1][in_processing] =
                interface_ring_buffer_get(&interface->saved_data_ring, dev_id, data_ring_index, data_in_ring - j - 1);
          }
        }
        data_ring_index++;
        in_processing++;
      }
    }
  }
  return total_to_draw;
}

static void draw_plots(struct nvtop_interface *interface) {
  for (unsigned plot_id = 0; plot_id < interface->num_plots; ++plot_id) {
    werase(interface->plots[plot_id].plot_window);

    char plot_legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE];

    unsigned num_lines =
        populate_plot_data_from_ring_buffer(interface, &interface->plots[plot_id], interface->plots[plot_id].num_data,
                                            interface->plots[plot_id].data, plot_legend);

    nvtop_line_plot(interface->plots[plot_id].plot_window, interface->plots[plot_id].num_data,
                    interface->plots[plot_id].data, num_lines, !interface->options.plot_left_to_right, plot_legend);

    wnoutrefresh(interface->plots[plot_id].plot_window);
  }
}

void draw_gpu_info_ncurses(unsigned devices_count, struct list_head *devices, struct nvtop_interface *interface) {

  draw_devices(devices, interface);
  if (!interface->setup_win.visible) {
    draw_plots(interface);
    draw_processes(devices, interface);
  } else {
    draw_setup_window(devices_count, devices, interface);
  }
  draw_shortcuts(interface);
  doupdate();
}

void update_window_size_to_terminal_size(struct nvtop_interface *inter) {
  endwin();
  erase();
  refresh();
  refresh();
  delete_all_windows(inter);
  initialize_all_windows(inter);
}

bool is_escape_for_quit(struct nvtop_interface *interface) {
  if (interface->process.option_window.state == nvtop_option_state_hidden && !interface->setup_win.visible)
    return true;
  else
    return false;
}

static void option_do_kill(struct nvtop_interface *interface) {
  if (interface->process.option_window.selected_row == 0)
    return;
  pid_t pid = interface->process.selected_pid;
  int sig = signalValues[interface->process.option_window.selected_row];
  if (pid > 0) {
    kill(pid, sig);
  }
}

static void option_change_sort(struct nvtop_interface *interface) {
  if (interface->process.option_window.selected_row == 0)
    return;
  unsigned index = 0;
  for (enum process_field i = process_pid; i < process_field_count; ++i) {
    if (process_is_field_displayed(i, interface->options.process_fields_displayed)) {
      if (index == interface->process.option_window.selected_row - 1) {
        interface->options.sort_processes_by = i;
        return;
      }
      index++;
    }
  }
}

void interface_key(int keyId, struct nvtop_interface *interface) {
  if (interface->setup_win.visible) {
    handle_setup_win_keypress(keyId, interface);
    return;
  }
  switch (keyId) {
  case KEY_F(2):
    if (interface->process.option_window.state == nvtop_option_state_hidden && !interface->setup_win.visible) {
      show_setup_window(interface);
    }
    break;
  case KEY_F(12):
    save_interface_options_to_config_file(interface->total_dev_count, &interface->options);
    break;
  case KEY_F(9):
    if (process_field_displayed_count(interface->options.process_fields_displayed) > 0 &&
        interface->process.option_window.state == nvtop_option_state_hidden) {
      interface->process.option_window.state = nvtop_option_state_kill;
      interface->process.option_window.selected_row = 0;
    }
    break;
  case KEY_F(6):
    if (process_field_displayed_count(interface->options.process_fields_displayed) > 0 &&
        interface->process.option_window.state == nvtop_option_state_hidden) {
      interface->process.option_window.state = nvtop_option_state_sort_by;
      interface->process.option_window.selected_row = 0;
    }
    break;
  case 'l':
  case KEY_RIGHT:
    if (interface->process.option_window.state == nvtop_option_state_hidden)
      interface->process.offset_column += 4;
    break;
  case 'h':
  case KEY_LEFT:
    if (interface->process.option_window.state == nvtop_option_state_hidden && interface->process.offset_column >= 4)
      interface->process.offset_column -= 4;
    break;
  case 'k':
  case KEY_UP:
    switch (interface->process.option_window.state) {
    case nvtop_option_state_kill:
    case nvtop_option_state_sort_by:
      if (interface->process.option_window.selected_row != 0)
        interface->process.option_window.selected_row--;
      break;
    case nvtop_option_state_hidden:
      if (interface->process.selected_row != 0)
        interface->process.selected_row--;
      break;
    default:
      break;
    }
    break;
  case 'j':
  case KEY_DOWN:
    switch (interface->process.option_window.state) {
    case nvtop_option_state_kill:
    case nvtop_option_state_sort_by:
      interface->process.option_window.selected_row++;
      break;
    case nvtop_option_state_hidden:
      interface->process.selected_row++;
      break;
    default:
      break;
    }
    break;
  case '+':
    interface->options.sort_descending_order = false;
    break;
  case '-':
    interface->options.sort_descending_order = true;
    break;
  case '\n':
  case KEY_ENTER:
    switch (interface->process.option_window.state) {
    case nvtop_option_state_kill:
      option_do_kill(interface);
      interface->process.option_window.state = nvtop_option_state_hidden;
      break;
    case nvtop_option_state_sort_by:
      option_change_sort(interface);
      interface->process.option_window.state = nvtop_option_state_hidden;
      break;
    case nvtop_option_state_hidden:
    default:
      break;
    }
    break;
  case 27:
    interface->process.option_window.state = nvtop_option_state_hidden;
    break;
  default:
    break;
  }
}

bool interface_freeze_processes(struct nvtop_interface *interface) {
  return interface->process.option_window.state == nvtop_option_state_kill;
}

extern inline void set_attribute_between(WINDOW *win, int startY, int startX, int endX, attr_t attr, short pair);

int interface_update_interval(const struct nvtop_interface *interface) { return interface->options.update_interval; }

unsigned interface_largest_gpu_name(struct list_head *devices) {
  struct gpu_info *gpuinfo;
  unsigned max_size = 4;
  list_for_each_entry(gpuinfo, devices, list) {
    if (GPUINFO_STATIC_FIELD_VALID(&gpuinfo->static_info, device_name)) {
      unsigned name_len = strlen(gpuinfo->static_info.device_name);
      max_size = name_len > max_size ? name_len : max_size;
    }
  }
  return max_size;
}

void interface_check_monitored_gpu_change(struct nvtop_interface **interface, unsigned allDevCount,
                                          unsigned *num_monitored_gpus, struct list_head *monitoredGpus,
                                          struct list_head *nonMonitoredGpus) {
  if (!(*interface)->setup_win.visible && (*interface)->options.has_monitored_set_changed) {
    nvtop_interface_option options_copy = (*interface)->options;
    options_copy.has_monitored_set_changed = false;
    memset(&(*interface)->options, 0, sizeof(options_copy));
    *num_monitored_gpus =
        interface_check_and_fix_monitored_gpus(allDevCount, monitoredGpus, nonMonitoredGpus, &options_copy);
    clean_ncurses(*interface);
    *interface =
        initialize_curses(allDevCount, *num_monitored_gpus, interface_largest_gpu_name(monitoredGpus), options_copy);
    timeout(interface_update_interval(*interface));
  }
}

static char dontShowAgain[] = "<Don't Show Again>";
static char okay[] = "<Ok>";
static char interactKeys[] = "Press Enter to select, arrows \">\" and \"<\" to switch options";

static unsigned message_lines(unsigned message_size, unsigned cols) { return (message_size + cols - 1) / cols; }

bool show_information_messages(unsigned num_messages, const char **messages) {
  if (!num_messages)
    return false;
  bool exit = false;
  bool dontShowAgainOption = false;

  while (!exit) {
    initscr();
    clear();
    refresh();
    initialize_colors();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    int rows, cols;
    getmaxyx(stdscr, rows, cols);
    unsigned messages_lines = num_messages / 2;
    for (unsigned i = 0; i < num_messages; ++i) {
      messages_lines += message_lines(strlen(messages[i]), cols) + 1;
    }
    int row = (rows - messages_lines + 1) / 2;
    for (unsigned i = 0; i < num_messages; ++i) {
      int col = (cols - strlen(messages[i]) - 1) / 2;
      col = col < 0 ? 0 : col;
      mvprintw(row, col, "%s", messages[i]);
      row += message_lines(strlen(messages[i]), cols) + 1;
    }
    size_t sizeQuitOptions = sizeof(dontShowAgain) + sizeof(okay);
    int quitOptionsRow = row;
    int quitOptionsCol = (cols - sizeQuitOptions) / 2;
    quitOptionsCol = quitOptionsCol < 0 ? 0 : quitOptionsCol;
    mvprintw(quitOptionsRow, quitOptionsCol, "%s %s", dontShowAgain, okay);
    refresh();
    if (dontShowAgainOption) {
      mvchgat(quitOptionsRow, quitOptionsCol, sizeof(dontShowAgain) - 1, 0, green_color, NULL);
      mvchgat(quitOptionsRow, quitOptionsCol + sizeof(dontShowAgain), sizeof(okay) - 1, 0, 0, NULL);
    } else {
      mvchgat(quitOptionsRow, quitOptionsCol, sizeof(dontShowAgain) - 1, 0, 0, NULL);
      mvchgat(quitOptionsRow, quitOptionsCol + sizeof(dontShowAgain), sizeof(okay) - 1, 0, green_color, NULL);
    }
    int interactKeyCol = (cols - sizeof(interactKeys) - 1) / 2;
    interactKeyCol = interactKeyCol < 0 ? 0 : interactKeyCol;
    mvprintw(quitOptionsRow + 1, interactKeyCol, "%s", interactKeys);

    int input_char = getch();
    switch (input_char) {
    case 27: // ESC
    case 'q':
    case KEY_ENTER:
    case '\n':
      exit = true;
      break;
    case KEY_RIGHT:
      dontShowAgainOption = false;
      break;
    case KEY_LEFT:
      dontShowAgainOption = true;
      break;
    default:
      break;
    }
    endwin();
  }
  return dontShowAgainOption;
}
