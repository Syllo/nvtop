/*
 *
 * Copyright (C) 2017-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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
#include "nvtop/extract_gpuinfo.h"
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/interface_common.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/interface_layout_selection.h"
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

static unsigned int sizeof_device_field[device_field_count] = {
    [device_name] = 11,  [device_fan_speed] = 8, [device_temperature] = 10,
    [device_power] = 15, [device_clock] = 11,    [device_pcie] = 46,
};

static unsigned int sizeof_process_field[process_field_count] = {
    [process_pid] = 7,       [process_user] = 4,          [process_gpu_id] = 3,
    [process_type] = 7,
    [process_memory] = 14, // 9 for mem 5 for %
    [process_cpu_usage] = 6, [process_cpu_mem_usage] = 9, [process_command] = 0,
};

static void alloc_device_window(unsigned int start_row, unsigned int start_col,
                                unsigned int totalcol,
                                struct device_window *dwin) {

  const unsigned int spacer = 1;

  // Line 1 = Name | PCIe info

  dwin->name_win =
      newwin(1, sizeof_device_field[device_name], start_row, start_col);
  if (dwin->name_win == NULL)
    goto alloc_error;
  dwin->pcie_info =
      newwin(1, sizeof_device_field[device_pcie], start_row,
             start_col + spacer + sizeof_device_field[device_name]);
  if (dwin->pcie_info == NULL)
    goto alloc_error;

  // Line 2 = GPU clk | MEM clk | Temp | Fan | Power
  dwin->gpu_clock_info =
      newwin(1, sizeof_device_field[device_clock], start_row + 1, start_col);
  if (dwin->gpu_clock_info == NULL)
    goto alloc_error;
  dwin->mem_clock_info =
      newwin(1, sizeof_device_field[device_clock], start_row + 1,
             start_col + spacer + sizeof_device_field[device_clock]);
  if (dwin->mem_clock_info == NULL)
    goto alloc_error;
  dwin->temperature =
      newwin(1, sizeof_device_field[device_temperature], start_row + 1,
             start_col + spacer * 2 + sizeof_device_field[device_clock] * 2);
  if (dwin->temperature == NULL)
    goto alloc_error;
  dwin->fan_speed =
      newwin(1, sizeof_device_field[device_fan_speed], start_row + 1,
             start_col + spacer * 3 + sizeof_device_field[device_clock] * 2 +
                 sizeof_device_field[device_temperature]);
  if (dwin->fan_speed == NULL)
    goto alloc_error;
  dwin->power_info =
      newwin(1, sizeof_device_field[device_power], start_row + 1,
             start_col + spacer * 4 + sizeof_device_field[device_clock] * 2 +
                 sizeof_device_field[device_temperature] +
                 sizeof_device_field[device_fan_speed]);
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
  dwin->mem_util_enc_dec =
      newwin(1, size_mem, start_row + 2, start_col + spacer + size_gpu);
  if (dwin->mem_util_enc_dec == NULL)
    goto alloc_error;
  dwin->encode_util = newwin(1, size_encode, start_row + 2,
                             start_col + spacer * 2 + size_gpu + size_mem);
  if (dwin->encode_util == NULL)
    goto alloc_error;
  dwin->decode_util =
      newwin(1, size_decode, start_row + 2,
             start_col + spacer * 3 + size_gpu + size_mem + size_encode);
  if (dwin->decode_util == NULL)
    goto alloc_error;
  // For auto-hide encode / decode window
  dwin->gpu_util_no_enc_or_dec =
      newwin(1, size_gpu + size_encode / 2 + 1, start_row + 2, start_col);
  if (dwin->gpu_util_no_enc_or_dec == NULL)
    goto alloc_error;
  dwin->mem_util_no_enc_or_dec =
      newwin(1, size_mem + size_encode / 2, start_row + 2,
             start_col + spacer + size_gpu + size_encode / 2 + 1);
  if (dwin->mem_util_no_enc_or_dec == NULL)
    goto alloc_error;
  dwin->gpu_util_no_enc_and_dec =
      newwin(1, size_gpu + size_encode + 1, start_row + 2, start_col);
  if (dwin->gpu_util_no_enc_and_dec == NULL)
    goto alloc_error;
  dwin->mem_util_no_enc_and_dec =
      newwin(1, size_mem + size_encode + 1, start_row + 2,
             start_col + spacer + size_gpu + size_encode + 1);
  if (dwin->mem_util_no_enc_and_dec == NULL)
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

static void alloc_process_with_option(struct nvtop_interface *interface,
                                      unsigned posX, unsigned posY,
                                      unsigned sizeX, unsigned sizeY) {

  if (sizeY > 0) {
    interface->process.process_win = newwin(sizeY, sizeX, posY, posX);
    interface->process.process_with_option_win = newwin(
        sizeY, sizeX - option_window_size, posY, posX + option_window_size);

  } else {
    interface->process.option_window.state = nvtop_option_state_hidden;
    interface->process.process_win = NULL;
    interface->process.process_with_option_win = NULL;
  }
}

static void initialize_gpu_mem_plot(struct plot_window *plot,
                                    struct window_position *position) {
  unsigned rows = position->sizeY;
  unsigned cols = position->sizeX;
  cols -= 5;
  rows -= 2;
  plot->plot_window =
      newwin(rows, cols, position->posY + 1, position->posX + 4);
  draw_rectangle(plot->win, 3, 0, cols + 2, rows + 2);
  mvwprintw(plot->win, 1 + rows * 3 / 4, 0, " 25");
  mvwprintw(plot->win, 1 + rows / 4, 0, " 75");
  mvwprintw(plot->win, 1 + rows / 2, 0, " 50");
  mvwprintw(plot->win, 1, 0, "100");
  mvwprintw(plot->win, rows, 0, "  0");
  plot->data = calloc(cols, sizeof(*plot->data));
  plot->num_data = cols;
}

static void alloc_plot_window(unsigned devices_count,
                              struct window_position *plot_positions,
                              unsigned map_device_to_plot[devices_count],
                              struct nvtop_interface *interface) {
  if (!interface->num_plots) {
    interface->plots = NULL;
    return;
  }
  interface->plots = malloc(interface->num_plots * sizeof(*interface->plots));
  for (size_t i = 0; i < interface->num_plots; ++i) {
    interface->plots[i].num_devices_to_plot = 0;
    for (unsigned dev_id = 0; dev_id < devices_count; ++dev_id) {
      if (map_device_to_plot[dev_id] == i) {
        interface->plots[i]
            .devices_ids[interface->plots[i].num_devices_to_plot] = dev_id;
        interface->plots[i].num_devices_to_plot++;
      }
    }
    interface->plots[i].win =
        newwin(plot_positions[i].sizeY, plot_positions[i].sizeX,
               plot_positions[i].posY, plot_positions[i].posX);
    initialize_gpu_mem_plot(&interface->plots[i], &plot_positions[i]);
  }
}

static unsigned device_length(void) {
  return max(sizeof_device_field[device_name] +
                 sizeof_device_field[device_pcie] + 1,

             2 * sizeof_device_field[device_clock] +
                 sizeof_device_field[device_temperature] +
                 sizeof_device_field[device_fan_speed] +
                 sizeof_device_field[device_power] + 4);
}

static void initialize_all_windows(struct nvtop_interface *dwin) {
  int rows, cols;
  getmaxyx(stdscr, rows, cols);

  unsigned int devices_count = dwin->devices_count;

  struct window_position device_positions[devices_count];
  unsigned map_device_to_plot[devices_count];
  struct window_position process_position;
  struct window_position plot_positions[MAX_CHARTS];
  struct window_position setup_position;

  compute_sizes_from_layout(devices_count, 3, device_length(), rows - 1, cols,
                            dwin->options.device_information_drawn,
                            device_positions, &dwin->num_plots, plot_positions,
                            map_device_to_plot, &process_position,
                            &setup_position);

  alloc_plot_window(devices_count, plot_positions, map_device_to_plot, dwin);

  for (unsigned int i = 0; i < devices_count; ++i) {
    alloc_device_window(device_positions[i].posY, device_positions[i].posX,
                        device_positions[i].sizeX, &dwin->devices_win[i]);
  }

  alloc_process_with_option(dwin, process_position.posX, process_position.posY,
                            process_position.sizeX, process_position.sizeY);
  dwin->process.option_window.option_win =
      newwin(process_position.sizeY, option_window_size, process_position.posY,
             process_position.posX);

  dwin->process.option_window.option_selection_window =
      newwin(1, cols, rows - 1, 0);

  alloc_setup_window(&setup_position, &dwin->setup_win);
}

static void delete_all_windows(struct nvtop_interface *dwin) {
  for (unsigned int i = 0; i < dwin->devices_count; ++i) {
    free_device_windows(&dwin->devices_win[i]);
  }
  delwin(dwin->process.process_win);
  delwin(dwin->process.process_with_option_win);
  dwin->process.process_win = NULL;
  dwin->process.process_with_option_win = NULL;
  delwin(dwin->process.option_window.option_selection_window);
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

struct nvtop_interface *initialize_curses(unsigned devices_count,
                                          unsigned largest_device_name,
                                          nvtop_interface_option options) {
  struct nvtop_interface *interface = calloc(1, sizeof(*interface));
  interface->options = options;
  interface->devices_win =
      calloc(devices_count, sizeof(*interface->devices_win));
  interface->devices_count = devices_count;
  sizeof_device_field[device_name] = largest_device_name + 11;
  initscr();
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
    some_time_in_past = nvtop_hmns_to_time(
        0,
        (unsigned int)(interface->options.encode_decode_hiding_timer / 60.) + 1,
        0);

    nvtop_get_current_time(&time_now);
    some_time_in_past = nvtop_substract_time(time_now, some_time_in_past);
    for (size_t i = 0; i < devices_count; ++i) {
      interface->devices_win[i].last_encode_seen = some_time_in_past;
      interface->devices_win[i].last_decode_seen = some_time_in_past;
    }
  }

  interface->process.offset = 0;
  interface->process.offset_column = 0;
  interface->process.option_window.offset = 0;
  interface->process.option_window.state = nvtop_option_state_hidden;
  interface->process.selected_row = 0;
  interface_alloc_ring_buffer(devices_count, 4, 10 * 60 * 1000,
                              &interface->saved_data_ring);
  initialize_all_windows(interface);
  refresh();
  return interface;
}

void clean_ncurses(struct nvtop_interface *interface) {
  endwin();
  delete_all_windows(interface);
  free(interface->options.device_information_drawn);
  free(interface->options.config_file_location);
  free(interface->devices_win);
  interface_free_ring_buffer(&interface->saved_data_ring);
  free(interface);
}

static void draw_bare_percentage(WINDOW *win, const char *prelude,
                                 unsigned int new_percentage,
                                 const char inside_braces_right[1024]) {
  int rows, cols;
  getmaxyx(win, rows, cols);
  (void)rows;
  size_t size_prelude = strlen(prelude);
  wattron(win, COLOR_PAIR(cyan_color));
  mvwprintw(win, 0, 0, "%s", prelude);
  wattroff(win, COLOR_PAIR(cyan_color));
  waddch(win, '[');
  int curx, cury;
  curx = getcurx(win);
  cury = getcury(win);
  int between_sbraces = cols - size_prelude - 2;
  float usage = round((float)between_sbraces * new_percentage / 100.f);
  int represent_usage = (int)usage;
  whline(win, '|', (int)represent_usage);
  mvwhline(win, cury, curx + represent_usage, ' ',
           between_sbraces - represent_usage);
  mvwaddch(win, cury, curx + between_sbraces, ']');
  unsigned int right_side_braces_space_required = strlen(inside_braces_right);
  wmove(win, cury, curx + between_sbraces - right_side_braces_space_required);
  wprintw(win, "%s", inside_braces_right);
  mvwchgat(win, cury, curx, represent_usage, 0, green_color, NULL);
  wnoutrefresh(win);
}

static const char *memory_prefix[] = {" B", "Ki", "Mi", "Gi", "Ti", "Pi"};

static void draw_temp_color(WINDOW *win, unsigned int temp,
                            unsigned int temp_slowdown, bool celsius) {
  unsigned int temp_convert;
  if (celsius)
    temp_convert = temp;
  else
    temp_convert = (unsigned)(32 + nearbyint(temp * 1.8));
  mvwprintw(win, 0, 0, "TEMP %3u", temp_convert);
  waddch(win, ACS_DEGREE);
  if (celsius)
    waddch(win, 'C');
  else
    waddch(win, 'F');
  if (temp >= temp_slowdown - 5) {
    if (temp >= temp_slowdown)
      mvwchgat(win, 0, 5, 3, 0, red_color, NULL);
    else
      mvwchgat(win, 0, 5, 3, 0, yellow_color, NULL);
  } else {
    mvwchgat(win, 0, 5, 3, 0, green_color, NULL);
  }
  mvwchgat(win, 0, 0, 4, 0, cyan_color, NULL);
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

static void draw_devices(unsigned devices_count, gpu_info *devices,
                         struct nvtop_interface *interface) {

  for (unsigned i = 0; i < devices_count; ++i) {
    struct device_window *dev = &interface->devices_win[i];

    wattron(dev->name_win, COLOR_PAIR(cyan_color));
    mvwprintw(dev->name_win, 0, 0, "Device %-2u", i);
    wattroff(dev->name_win, COLOR_PAIR(cyan_color));
    if (IS_VALID(gpuinfo_device_name_valid, devices[i].static_info.valid)) {
      wprintw(dev->name_win, "[%s]", devices[i].static_info.device_name);
      wnoutrefresh(dev->name_win);
    } else {
      wprintw(dev->name_win, "[N/A]");
      wnoutrefresh(dev->name_win);
    }

    werase_and_wnoutrefresh(dev->gpu_util_enc_dec);
    werase_and_wnoutrefresh(dev->gpu_util_no_enc_or_dec);
    werase_and_wnoutrefresh(dev->gpu_util_no_enc_and_dec);
    werase_and_wnoutrefresh(dev->mem_util_enc_dec);
    werase_and_wnoutrefresh(dev->mem_util_no_enc_or_dec);
    werase_and_wnoutrefresh(dev->mem_util_no_enc_and_dec);
    char buff[1024];
    nvtop_time tnow;
    nvtop_get_current_time(&tnow);
    bool is_encode_displayed = true;
    if (IS_VALID(gpuinfo_encoder_rate_valid, devices[i].dynamic_info.valid)) {
      if (devices[i].dynamic_info.encoder_rate > 0) {
        is_encode_displayed = true;
        dev->last_encode_seen = tnow;
      } else {
        if (interface->options.encode_decode_hiding_timer > 0. &&
            nvtop_difftime(dev->last_encode_seen, tnow) >
                interface->options.encode_decode_hiding_timer) {
          is_encode_displayed = false;
        }
      }
    } else {
      is_encode_displayed = false;
    }
    bool is_decode_displayed = true;
    if (IS_VALID(gpuinfo_decoder_rate_valid, devices[i].dynamic_info.valid)) {
      if (devices[i].dynamic_info.decoder_rate > 0) {
        is_decode_displayed = true;
        dev->last_decode_seen = tnow;
      } else {
        if (interface->options.encode_decode_hiding_timer > 0. &&
            nvtop_difftime(dev->last_decode_seen, tnow) >
                interface->options.encode_decode_hiding_timer) {
          is_decode_displayed = false;
        }
      }
    } else {
      is_decode_displayed = false;
    }
    WINDOW *gpu_util_win;
    WINDOW *mem_util_win;
    WINDOW *encode_win = dev->encode_util;
    WINDOW *decode_win = dev->decode_util;
    if (is_encode_displayed && is_decode_displayed) {
      gpu_util_win = dev->gpu_util_enc_dec;
      mem_util_win = dev->mem_util_enc_dec;
    } else {
      if (is_encode_displayed || is_decode_displayed) {
        if (is_encode_displayed)
          encode_win = dev->decode_util;
        gpu_util_win = dev->gpu_util_no_enc_or_dec;
        mem_util_win = dev->mem_util_no_enc_or_dec;
      } else {
        gpu_util_win = dev->gpu_util_no_enc_and_dec;
        mem_util_win = dev->mem_util_no_enc_and_dec;
      }
    }
    if (is_encode_displayed) {
      if (IS_VALID(gpuinfo_encoder_rate_valid, devices[i].dynamic_info.valid)) {
        snprintf(buff, 1024, "%u%%", devices[i].dynamic_info.encoder_rate);
        draw_bare_percentage(encode_win, "ENC",
                             devices[i].dynamic_info.encoder_rate, buff);
      }
    }
    if (is_decode_displayed) {
      if (IS_VALID(gpuinfo_decoder_rate_valid, devices[i].dynamic_info.valid)) {
        snprintf(buff, 1024, "%u%%", devices[i].dynamic_info.decoder_rate);
        draw_bare_percentage(decode_win, "DEC",
                             devices[i].dynamic_info.decoder_rate, buff);
      }
    }
    if (IS_VALID(gpuinfo_gpu_util_rate_valid, devices[i].dynamic_info.valid)) {
      snprintf(buff, 1024, "%u%%", devices[i].dynamic_info.gpu_util_rate);
      draw_bare_percentage(gpu_util_win, "GPU",
                           devices[i].dynamic_info.gpu_util_rate, buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(gpu_util_win, "GPU", 0, buff);
    }

    if (IS_VALID(gpuinfo_total_memory_valid, devices[i].dynamic_info.valid) &&
        IS_VALID(gpuinfo_used_memory_valid, devices[i].dynamic_info.valid)) {
      double total_mem = devices[i].dynamic_info.total_memory;
      double used_mem = devices[i].dynamic_info.used_memory;
      double total_prefixed = total_mem, used_prefixed = used_mem;
      size_t prefix_off;
      for (prefix_off = 0; prefix_off < 5 && total_prefixed >= 1000.;
           ++prefix_off) {
        total_prefixed /= 1024.;
        used_prefixed /= 1024.;
      }
      snprintf(buff, 1024, "%.3f%s/%.3f%s", used_prefixed,
               memory_prefix[prefix_off], total_prefixed,
               memory_prefix[prefix_off]);
      draw_bare_percentage(mem_util_win, "MEM",
                           (unsigned int)(100. * used_mem / total_mem), buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(mem_util_win, "MEM", 0, buff);
    }
    if (IS_VALID(gpuinfo_gpu_temp_valid, devices[i].dynamic_info.valid)) {
      if (!IS_VALID(gpuinfo_temperature_slowdown_valid,
                    devices[i].static_info.valid))
        devices[i].static_info.temperature_slowdown_threshold = 0;
      draw_temp_color(dev->temperature, devices[i].dynamic_info.gpu_temp,
                      devices[i].static_info.temperature_slowdown_threshold,
                      !interface->options.temperature_in_fahrenheit);
    } else {
      mvwprintw(dev->temperature, 0, 0, "TEMP N/A");
      waddch(dev->temperature, ACS_DEGREE);
      if (interface->options.temperature_in_fahrenheit)
        waddch(dev->temperature, 'F');
      else
        waddch(dev->temperature, 'C');
      wnoutrefresh(dev->temperature);
    }

    // FAN
    if (IS_VALID(gpuinfo_fan_speed_valid, devices[i].dynamic_info.valid))
      mvwprintw(dev->fan_speed, 0, 0, "FAN %3u%%",
                devices[i].dynamic_info.fan_speed);
    else
      mvwprintw(dev->fan_speed, 0, 0, "FAN N/A%%");
    mvwchgat(dev->fan_speed, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->fan_speed);

    // GPU CLOCK
    werase(dev->gpu_clock_info);
    if (IS_VALID(gpuinfo_curr_gpu_clock_speed_valid,
                 devices[i].dynamic_info.valid))
      mvwprintw(dev->gpu_clock_info, 0, 0, "GPU %uMHz",
                devices[i].dynamic_info.gpu_clock_speed);
    else
      mvwprintw(dev->gpu_clock_info, 0, 0, "GPU N/A MHz");

    mvwchgat(dev->gpu_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->gpu_clock_info);

    // MEM CLOCK
    werase(dev->mem_clock_info);
    if (IS_VALID(gpuinfo_curr_mem_clock_speed_valid,
                 devices[i].dynamic_info.valid))
      mvwprintw(dev->mem_clock_info, 0, 0, "MEM %uMHz",
                devices[i].dynamic_info.mem_clock_speed);
    else
      mvwprintw(dev->mem_clock_info, 0, 0, "MEM N/A MHz");
    mvwchgat(dev->mem_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->mem_clock_info);

    // POWER
    werase(dev->power_info);
    if (IS_VALID(gpuinfo_power_draw_valid, devices[i].dynamic_info.valid) &&
        IS_VALID(gpuinfo_power_draw_max_valid, devices[i].dynamic_info.valid))
      mvwprintw(dev->power_info, 0, 0, "POW %3u / %3u W",
                devices[i].dynamic_info.power_draw / 1000,
                devices[i].dynamic_info.power_draw_max / 1000);
    else if (IS_VALID(gpuinfo_power_draw_valid,
                      devices[i].dynamic_info.valid) &&
             !IS_VALID(gpuinfo_power_draw_max_valid,
                       devices[i].dynamic_info.valid))
      mvwprintw(dev->power_info, 0, 0, "POW %3u W",
                devices[i].dynamic_info.power_draw / 1000);
    else if (!IS_VALID(gpuinfo_power_draw_valid,
                       devices[i].dynamic_info.valid) &&
             IS_VALID(gpuinfo_power_draw_max_valid,
                      devices[i].dynamic_info.valid))
      mvwprintw(dev->power_info, 0, 0, "POW N/A / %3u W",
                devices[i].dynamic_info.power_draw_max / 1000);
    else
      mvwprintw(dev->power_info, 0, 0, "POW N/A W");
    mvwchgat(dev->power_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->power_info);

    // PICe throughput
    werase(dev->pcie_info);
    wattron(dev->pcie_info, COLOR_PAIR(cyan_color));
    mvwprintw(dev->pcie_info, 0, 0, "PCIe ");
    wattroff(dev->pcie_info, COLOR_PAIR(cyan_color));
    wattron(dev->pcie_info, COLOR_PAIR(magenta_color));
    wprintw(dev->pcie_info, "GEN ");
    wattroff(dev->pcie_info, COLOR_PAIR(magenta_color));
    if (IS_VALID(gpuinfo_pcie_link_gen_valid, devices[i].dynamic_info.valid) &&
        IS_VALID(gpuinfo_pcie_link_width_valid, devices[i].dynamic_info.valid))
      wprintw(dev->pcie_info, "%u@%2ux",
              devices[i].dynamic_info.curr_pcie_link_gen,
              devices[i].dynamic_info.curr_pcie_link_width);
    else
      wprintw(dev->pcie_info, "N/A");

    wattron(dev->pcie_info, COLOR_PAIR(magenta_color));
    wprintw(dev->pcie_info, " RX: ");
    wattroff(dev->pcie_info, COLOR_PAIR(magenta_color));
    if (IS_VALID(gpuinfo_pcie_rx_valid, devices[i].dynamic_info.valid))
      print_pcie_at_scale(dev->pcie_info, devices[i].dynamic_info.pcie_rx);
    else
      wprintw(dev->pcie_info, "N/A");
    wattron(dev->pcie_info, COLOR_PAIR(magenta_color));
    wprintw(dev->pcie_info, " TX: ");
    wattroff(dev->pcie_info, COLOR_PAIR(magenta_color));
    if (IS_VALID(gpuinfo_pcie_tx_valid, devices[i].dynamic_info.valid))
      print_pcie_at_scale(dev->pcie_info, devices[i].dynamic_info.pcie_tx);
    else
      wprintw(dev->pcie_info, "N/A");

    wnoutrefresh(dev->pcie_info);
  }
}

typedef struct {
  unsigned processes_count;
  struct gpuid_and_process {
    unsigned gpu_id;
    gpu_process *process;
  } * processes;
} all_processes;

static all_processes all_processes_array(unsigned devices_count,
                                         gpu_info *devices) {

  unsigned total_processes_count = 0;
  for (unsigned i = 0; i < devices_count; ++i) {
    total_processes_count += devices[i].processes_count;
  }
  all_processes merged_devices_processes = {0, NULL};
  merged_devices_processes.processes_count = total_processes_count;
  merged_devices_processes.processes = malloc(
      total_processes_count * sizeof(*merged_devices_processes.processes));
  if (!merged_devices_processes.processes) {
    perror("Cannot allocate memory: ");
    exit(EXIT_FAILURE);
  }

  size_t offset = 0;
  for (unsigned int i = 0; i < devices_count; ++i) {
    for (unsigned int j = 0; j < devices[i].processes_count; ++j) {
      merged_devices_processes.processes[offset].gpu_id = i;
      merged_devices_processes.processes[offset++].process =
          &devices[i].processes[j];
    }
  }

  return merged_devices_processes;
}

static int compare_pid_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return p1->process->pid >= p2->process->pid ? -1 : 1;
}

static int compare_pid_asc(const void *pp1, const void *pp2) {
  return compare_pid_desc(pp2, pp1);
}

static int compare_username_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (IS_VALID(gpuinfo_process_user_name_valid, p1->process->valid) &&
      IS_VALID(gpuinfo_process_user_name_valid, p2->process->valid))
    return -strcmp(p1->process->user_name, p2->process->user_name);
  else
    return 0;
}

static int compare_username_asc(const void *pp1, const void *pp2) {
  return compare_username_desc(pp2, pp1);
}

static int compare_process_name_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (IS_VALID(gpuinfo_process_cmdline_valid, p1->process->valid) &&
      IS_VALID(gpuinfo_process_cmdline_valid, p2->process->valid))
    return -strcmp(p1->process->cmdline, p2->process->cmdline);
  else
    return 0;
}

static int compare_process_name_asc(const void *pp1, const void *pp2) {
  return compare_process_name_desc(pp2, pp1);
}

static int compare_mem_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (IS_VALID(gpuinfo_process_gpu_memory_usage_valid, p1->process->valid) &&
      IS_VALID(gpuinfo_process_gpu_memory_usage_valid, p2->process->valid))
    return p1->process->gpu_memory_usage >= p2->process->gpu_memory_usage ? -1
                                                                          : 1;
  else
    return 0;
}

static int compare_mem_usage_asc(const void *pp1, const void *pp2) {
  return compare_mem_usage_desc(pp2, pp1);
}

static int compare_cpu_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (IS_VALID(gpuinfo_process_cpu_usage_valid, p1->process->valid) &&
      IS_VALID(gpuinfo_process_cpu_usage_valid, p2->process->valid))
    return p1->process->cpu_usage >= p2->process->cpu_usage ? -1 : 1;
  else
    return 0;
}

static int compare_cpu_usage_asc(const void *pp1, const void *pp2) {
  return compare_cpu_usage_desc(pp2, pp1);
}

static int compare_cpu_mem_usage_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  if (IS_VALID(gpuinfo_process_cpu_memory_res_valid, p1->process->valid) &&
      IS_VALID(gpuinfo_process_cpu_memory_res_valid, p2->process->valid))
    return p1->process->cpu_memory_res >= p2->process->cpu_memory_res ? -1 : 1;
  else
    return 0;
}

static int compare_cpu_mem_usage_asc(const void *pp1, const void *pp2) {
  return compare_cpu_mem_usage_desc(pp2, pp1);
}

static int compare_gpu_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return p1->gpu_id >= p2->gpu_id ? -1 : 1;
}

static int compare_gpu_asc(const void *pp1, const void *pp2) {
  return -compare_gpu_desc(pp1, pp2);
}

static int compare_process_type_desc(const void *pp1, const void *pp2) {
  const struct gpuid_and_process *p1 = (const struct gpuid_and_process *)pp1;
  const struct gpuid_and_process *p2 = (const struct gpuid_and_process *)pp2;
  return (p1->process->type == gpu_process_graphical) !=
         (p2->process->type == gpu_process_graphical);
}

static int compare_process_type_asc(const void *pp1, const void *pp2) {
  return -compare_process_name_desc(pp1, pp2);
}

static void sort_process(all_processes all_procs, enum process_field criterion,
                         bool asc_sort) {
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
  case process_field_count:
  default:
    return;
  }
  qsort(all_procs.processes, all_procs.processes_count,
        sizeof(*all_procs.processes), sort_fun);
}

static const char *columnName[] = {
    "PID", "USER", "GPU", "TYPE", "GPU MEM", "CPU", "HOST MEM", "Command",
};

static void update_selected_offset_with_window_size(
    unsigned int *selected_row, unsigned int *offset,
    unsigned int row_available_to_draw, unsigned int num_to_draw) {

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

static void print_processes_on_screen(all_processes all_procs,
                                      struct process_window *process,
                                      enum process_field sort_criterion) {
  WINDOW *win = process->option_window.state == nvtop_option_state_hidden
                    ? process->process_win
                    : process->process_with_option_win;
  struct gpuid_and_process *processes = all_procs.processes;

  werase(win);

  unsigned int rows, cols;
  getmaxyx(win, rows, cols);
  rows -= 1;

  update_selected_offset_with_window_size(&process->selected_row,
                                          &process->offset, rows,
                                          all_procs.processes_count);
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
      column_sort_end = i == process_command
                            ? process_buffer_line_size - 4
                            : column_sort_start + sizeof_process_field[i];
    }
    printed += snprintf(&process_print_buffer[printed],
                        process_buffer_line_size - printed, "%*s ",
                        sizeof_process_field[i], columnName[i]);
  }

  mvwprintw(win, 0, 0, "%.*s", cols,
            &process_print_buffer[process->offset_column]);
  mvwchgat(win, 0, 0, -1, A_STANDOUT, green_color, NULL);
  set_attribute_between(win, 0, column_sort_start - (int)process->offset_column,
                        column_sort_end - (int)process->offset_column,
                        A_STANDOUT, cyan_color);

  int start_type = 0;
  for (unsigned int i = 0; i < process_type; ++i) {
    start_type += sizeof_process_field[i] + 1;
  }
  int end_type = start_type + sizeof_process_field[process_type];

  for (unsigned int i = start_at_process;
       i < end_at_process && i < all_procs.processes_count; ++i) {
    memset(process_print_buffer, 0, sizeof(process_print_buffer));
    size_t size = snprintf(pid_str, sizeof_process_field[process_pid] + 1,
                           "%" PRIdMAX, (intmax_t)processes[i].process->pid);
    if (size == sizeof_process_field[process_pid] + 1)
      pid_str[sizeof_process_field[process_pid]] = '\0';
    size = snprintf(guid_str, sizeof_process_field[process_gpu_id] + 1, "%u",
                    processes[i].gpu_id);
    if (size == sizeof_process_field[process_gpu_id] + 1)
      pid_str[sizeof_process_field[process_gpu_id]] = '\0';

    const char *username;
    if (IS_VALID(gpuinfo_process_user_name_valid,
                 processes[i].process->valid)) {
      username = processes[i].process->user_name;
    } else {
      username = "N/A";
    }
    printed = snprintf(process_print_buffer, process_buffer_line_size,
                       "%*s %*s %*s", sizeof_process_field[process_pid],
                       pid_str, sizeof_process_field[process_user], username,
                       sizeof_process_field[process_gpu_id], guid_str);

    if (processes[i].process->type == gpu_process_graphical) {
      printed += snprintf(&process_print_buffer[printed],
                          process_buffer_line_size - printed, " %*s ",
                          sizeof_process_field[process_type], "Graphic");
    } else {
      printed += snprintf(&process_print_buffer[printed],
                          process_buffer_line_size - printed, " %*s ",
                          sizeof_process_field[process_type], "Compute");
    }

    if (IS_VALID(gpuinfo_process_gpu_memory_usage_valid,
                 processes[i].process->valid)) {
      if (IS_VALID(gpuinfo_process_gpu_memory_percentage_valid,
                   processes[i].process->valid)) {
        snprintf(memory, 10, "%6uMiB",
                 (unsigned)(processes[i].process->gpu_memory_usage / 1048576));
        snprintf(memory + 9, sizeof_process_field[process_memory] - 7, " %3u%%",
                 processes[i].process->gpu_memory_percentage);
      } else {
        snprintf(memory, sizeof_process_field[process_memory], "%6uMiB",
                 (unsigned)(processes[i].process->gpu_memory_usage / 1048576));
      }
    } else {
      memory[0] = '\0';
    }
    printed += snprintf(&process_print_buffer[printed],
                        process_buffer_line_size - printed, "%*s ",
                        sizeof_process_field[process_memory], memory);

    if (IS_VALID(gpuinfo_process_cpu_usage_valid, processes[i].process->valid))
      snprintf(cpu_percent, sizeof_process_field[process_cpu_usage] + 1, "%u%%",
               processes[i].process->cpu_usage);
    else
      snprintf(cpu_percent, sizeof_process_field[process_cpu_usage] + 1,
               "   N/A");
    printed += snprintf(&process_print_buffer[printed],
                        process_buffer_line_size - printed, "%*s ",
                        sizeof_process_field[process_cpu_usage], cpu_percent);

    if (IS_VALID(gpuinfo_process_cpu_memory_res_valid,
                 processes[i].process->valid))
      snprintf(cpu_mem, sizeof_process_field[process_cpu_mem_usage] + 1,
               "%zuMiB", processes[i].process->cpu_memory_res / 1048576);
    else
      snprintf(cpu_mem, sizeof_process_field[process_cpu_mem_usage] + 1, "N/A");
    printed += snprintf(&process_print_buffer[printed],
                        process_buffer_line_size - printed, "%*s ",
                        sizeof_process_field[process_cpu_mem_usage], cpu_mem);

    if (IS_VALID(gpuinfo_process_cmdline_valid, processes[i].process->valid))
      snprintf(&process_print_buffer[printed],
               process_buffer_line_size - printed, "%.*s",
               process_buffer_line_size - printed,
               processes[i].process->cmdline);

    unsigned int write_at = i - start_at_process + 1;
    mvwprintw(win, write_at, 0, "%.*s", cols,
              &process_print_buffer[process->offset_column]);
    if (i == special_row) {
      mvwchgat(win, write_at, 0, -1, A_STANDOUT, cyan_color, NULL);
    } else {
      if (processes[i].process->type == gpu_process_graphical) {
        set_attribute_between(
            win, write_at, start_type - (int)process->offset_column,
            end_type - (int)process->offset_column, 0, yellow_color);
      } else {
        set_attribute_between(
            win, write_at, start_type - (int)process->offset_column,
            end_type - (int)process->offset_column, 0, magenta_color);
      }
    }
  }
  wnoutrefresh(win);
}

static void draw_processes(unsigned devices_count, gpu_info *devices,
                           struct nvtop_interface *interface) {

  if (interface->process.process_win == NULL)
    return;
  all_processes all_procs = all_processes_array(devices_count, devices);
  sort_process(all_procs, interface->options.sort_processes_by,
               !interface->options.sort_descending_order);

  if (interface->process.selected_row >= all_procs.processes_count)
    interface->process.selected_row = all_procs.processes_count - 1;
  interface->process.selected_pid =
      all_procs.processes[interface->process.selected_row].process->pid;

  unsigned largest_username = 4;
  for (unsigned i = 0; i < all_procs.processes_count; ++i) {
    if (IS_VALID(gpuinfo_process_user_name_valid,
                 all_procs.processes[i].process->valid)) {
      unsigned length = strlen(all_procs.processes[i].process->user_name);
      if (length > largest_username)
        largest_username = length;
    }
  }
  sizeof_process_field[process_user] = largest_username;

  print_processes_on_screen(all_procs, &interface->process,
                            interface->options.sort_processes_by);
  free(all_procs.processes);
}

static const char *signalNames[] = {
    "Cancel",  "SIGHUP",    "SIGINT",  "SIGQUIT",  "SIGILL",  "SIGTRAP",
    "SIGABRT", "SIGBUS",    "SIGFPE",  "SIGKILL",  "SIGUSR1", "SIGSEGV",
    "SIGUSR2", "SIGPIPE",   "SIGALRM", "SIGTERM",  "SIGCHLD", "SIGCONT",
    "SIGSTOP", "SIGTSTP",   "SIGTTIN", "SIGTTOU",  "SIGURG",  "SIGXCPU",
    "SIGXFSZ", "SIGVTALRM", "SIGPROF", "SIGWINCH", "SIGIO",   "SIGPWR",
    "SIGSYS",
};

static const int signalValues[ARRAY_SIZE(signalNames)] = {
    -1,      SIGHUP,    SIGINT,  SIGQUIT,  SIGILL,  SIGTRAP, SIGABRT, SIGBUS,
    SIGFPE,  SIGKILL,   SIGUSR1, SIGSEGV,  SIGUSR2, SIGPIPE, SIGALRM, SIGTERM,
    SIGCHLD, SIGCONT,   SIGSTOP, SIGTSTP,  SIGTTIN, SIGTTOU, SIGURG,  SIGXCPU,
    SIGXFSZ, SIGVTALRM, SIGPROF, SIGWINCH, SIGIO,   SIGPWR,  SIGSYS,
};

static const size_t nvtop_num_signals = ARRAY_SIZE(signalNames) - 1;

static void draw_kill_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->process.option_window.option_win;
  wattron(win, COLOR_PAIR(green_color) | A_REVERSE);
  mvwprintw(win, 0, 0, "Send signal:");
  wattroff(win, COLOR_PAIR(green_color) | A_REVERSE);
  wprintw(win, " ");
  int rows, cols;
  getmaxyx(win, rows, cols);

  size_t start_at_option = interface->process.option_window.offset;
  size_t end_at_option = start_at_option + rows - 1;

  for (size_t i = start_at_option; i < end_at_option && i <= nvtop_num_signals;
       ++i) {
    if (i == interface->process.option_window.selected_row) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "%*d %s", 2, i, signalNames[i]);
    getyx(win, rows, cols);

    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (i == interface->process.option_window.selected_row) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size - 1, " ");
    }
  }
  wnoutrefresh(win);
}

static void draw_sort_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->process.option_window.option_win;
  wattron(win, COLOR_PAIR(green_color) | A_REVERSE);
  mvwprintw(win, 0, 0, "Sort by     ");
  wattroff(win, COLOR_PAIR(green_color) | A_REVERSE);
  wprintw(win, " ");
  int rows, cols;
  if (interface->process.option_window.offset == 0) {
    if (interface->process.option_window.selected_row == 0) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "Cancel");
    getyx(win, rows, cols);
    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (interface->process.option_window.selected_row == 0) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size - 1, " ");
    }
  }
  getmaxyx(win, rows, cols);

  size_t start_at_option = interface->process.option_window.offset == 0
                               ? interface->process.option_window.offset
                               : interface->process.option_window.offset - 1;
  size_t end_at_option = interface->process.option_window.offset == 0
                             ? start_at_option + rows - 2
                             : start_at_option + rows - 1;

  for (size_t i = start_at_option; i < end_at_option && i < process_field_count;
       ++i) {
    if (i + 1 == interface->process.option_window.selected_row) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "%s", columnName[i]);
    getyx(win, rows, cols);
    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (i + 1 == interface->process.option_window.selected_row) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size - 1, " ");
    }
  }
  wnoutrefresh(win);
}

static void draw_options(struct nvtop_interface *interface) {
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
    num_options = process_field_count + 1; // Option + Cancel
    break;
  case nvtop_option_state_hidden:
  default:
    break;
  }

  update_selected_offset_with_window_size(
      &interface->process.option_window.selected_row,
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

static void draw_option_selection(struct nvtop_interface *interface) {
  WINDOW *win = interface->process.option_window.option_selection_window;
  wmove(win, 0, 0);
  switch (interface->process.option_window.state) {
  case nvtop_option_state_hidden:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_hidden); ++i) {
      wprintw(win, "F%s", option_selection_hidden_num[i]);
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      wprintw(win, "%s", option_selection_hidden[i]);
      for (size_t j = strlen(option_selection_hidden[i]);
           j < option_selection_width; ++j)
        wprintw(win, " ");
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    break;
  case nvtop_option_state_kill:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_kill); ++i) {
      wprintw(win, "%s", option_selection_kill[i][0]);
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      wprintw(win, "%s", option_selection_kill[i][1]);
      for (size_t j = strlen(option_selection_kill[i][1]);
           j < option_selection_width; ++j)
        wprintw(win, " ");
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    break;
  case nvtop_option_state_sort_by:
    for (size_t i = 0; i < ARRAY_SIZE(option_selection_sort); ++i) {
      wprintw(win, "%s", option_selection_sort[i][0]);
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      wprintw(win, "%s", option_selection_sort[i][1]);
      for (size_t j = strlen(option_selection_sort[i][1]);
           j < option_selection_width; ++j)
        wprintw(win, " ");
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    break;
  default:
    break;
  }
  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(win, tmp, maxcols);
  getyx(win, tmp, cur_col);
  mvwchgat(win, 0, cur_col, maxcols - cur_col, A_STANDOUT, cyan_color, NULL);
  wnoutrefresh(win);
}

void save_current_data_to_ring(unsigned devices_count, gpu_info *devices,
                               struct nvtop_interface *interface) {
  for (unsigned dev_id = 0; dev_id < devices_count; ++dev_id) {
    unsigned data_index = 0;
    for (enum plot_information info = plot_gpu_rate;
         info < plot_information_count; ++info) {
      if (plot_isset_draw_info(
              info, interface->options.device_information_drawn[dev_id])) {
        unsigned data_val = 0;
        switch (info) {
        case plot_gpu_rate:
          if (IS_VALID(gpuinfo_gpu_util_rate_valid,
                       devices[dev_id].dynamic_info.valid))
            data_val = devices[dev_id].dynamic_info.gpu_util_rate;
          break;
        case plot_gpu_mem_rate:
          if (IS_VALID(gpuinfo_mem_util_rate_valid,
                       devices[dev_id].dynamic_info.valid))
            data_val = devices[dev_id].dynamic_info.mem_util_rate;
          break;
        case plot_encoder_rate:
          if (IS_VALID(gpuinfo_encoder_rate_valid,
                       devices[dev_id].dynamic_info.valid))
            data_val = devices[dev_id].dynamic_info.encoder_rate;
          break;
        case plot_decoder_rate:
          if (IS_VALID(gpuinfo_decoder_rate_valid,
                       devices[dev_id].dynamic_info.valid))
            data_val = devices[dev_id].dynamic_info.decoder_rate;
          break;
        case plot_gpu_temperature:
          if (IS_VALID(gpuinfo_gpu_temp_valid,
                       devices[dev_id].dynamic_info.valid)) {
            data_val = devices[dev_id].dynamic_info.gpu_temp;
            if (data_val > 100)
              data_val = 100u;
          }
          break;
        case plot_gpu_power_draw_rate:
          if (IS_VALID(gpuinfo_power_draw_valid,
                       devices[dev_id].dynamic_info.valid) &&
              IS_VALID(gpuinfo_power_draw_max_valid,
                       devices[dev_id].dynamic_info.valid)) {
            data_val = devices[dev_id].dynamic_info.power_draw * 100 /
                       devices[dev_id].dynamic_info.power_draw_max;
            if (data_val > 100)
              data_val = 100u;
          }
          break;
        case plot_fan_speed:
          if (IS_VALID(gpuinfo_fan_speed_valid,
                       devices[dev_id].dynamic_info.valid)) {
            data_val = devices[dev_id].dynamic_info.fan_speed;
          }
          break;
        case plot_information_count:
          break;
        }
        interface_ring_buffer_push(&interface->saved_data_ring, dev_id,
                                   data_index, data_val);
        data_index++;
      }
    }
  }
}

static unsigned populate_plot_data_from_ring_buffer(
    const struct nvtop_interface *interface, struct plot_window *plot_win,
    unsigned size_data_buff, double data[size_data_buff],
    char plot_legend[4][PLOT_MAX_LEGEND_SIZE]) {

  memset(data, 0, size_data_buff * sizeof(double));
  unsigned total_to_draw = 0;
  for (unsigned i = 0; i < plot_win->num_devices_to_plot; ++i) {
    unsigned dev_id = plot_win->devices_ids[i];
    plot_info_to_draw to_draw =
        interface->options.device_information_drawn[dev_id];
    total_to_draw += plot_count_draw_info(to_draw);
  }

  assert(size_data_buff % total_to_draw == 0);
  unsigned max_data_to_copy = size_data_buff / total_to_draw;
  double(*data_split)[total_to_draw] = (double(*)[total_to_draw])data;

  unsigned in_processing = 0;
  for (unsigned i = 0; i < plot_win->num_devices_to_plot; ++i) {
    unsigned dev_id = plot_win->devices_ids[i];
    plot_info_to_draw to_draw =
        interface->options.device_information_drawn[dev_id];
    unsigned data_ring_index = 0;
    for (enum plot_information info = plot_gpu_rate;
         info < plot_information_count; ++info) {
      if (plot_isset_draw_info(info, to_draw)) {
        // Populate the legend
        switch (info) {
        case plot_gpu_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE, "GPU%u %%",
                   dev_id);
          break;
        case plot_gpu_mem_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u mem%%", dev_id);
          break;
        case plot_encoder_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u encode%%", dev_id);
          break;
        case plot_decoder_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u decode%%", dev_id);
          break;
        case plot_gpu_temperature:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u temp(c)", dev_id);
          break;
        case plot_gpu_power_draw_rate:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u power%%", dev_id);
          break;
        case plot_fan_speed:
          snprintf(plot_legend[in_processing], PLOT_MAX_LEGEND_SIZE,
                   "GPU%u fan%%", dev_id);
          break;
        case plot_information_count:
          break;
        }
        // Copy the data
        unsigned data_in_ring = interface_ring_buffer_data_stored(
            &interface->saved_data_ring, dev_id, data_ring_index);
        if (interface->options.plot_left_to_right) {
          for (unsigned j = 0; j < data_in_ring && j < max_data_to_copy; ++j) {
            data_split[j][in_processing] = interface_ring_buffer_get(
                &interface->saved_data_ring, dev_id, data_ring_index,
                data_in_ring - j - 1);
          }
        } else {
          for (unsigned j = 0; j < data_in_ring && j < max_data_to_copy; ++j) {
            data_split[max_data_to_copy - j - 1][in_processing] =
                interface_ring_buffer_get(&interface->saved_data_ring, dev_id,
                                          data_ring_index,
                                          data_in_ring - j - 1);
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
    wnoutrefresh(interface->plots[plot_id].win);
    werase(interface->plots[plot_id].plot_window);

    char plot_legend[4][PLOT_MAX_LEGEND_SIZE];

    unsigned num_lines = populate_plot_data_from_ring_buffer(
        interface, &interface->plots[plot_id],
        interface->plots[plot_id].num_data, interface->plots[plot_id].data,
        plot_legend);

    nvtop_line_plot(interface->plots[plot_id].plot_window,
                    interface->plots[plot_id].num_data,
                    interface->plots[plot_id].data, 0., 100., num_lines,
                    !interface->options.plot_left_to_right, plot_legend);

    wnoutrefresh(interface->plots[plot_id].plot_window);
  }
}

void draw_gpu_info_ncurses(unsigned devices_count, gpu_info *devices,
                           struct nvtop_interface *interface) {

  draw_devices(devices_count, devices, interface);
  if (!interface->setup_win.visible) {
    draw_processes(devices_count, devices, interface);
    if (interface->process.option_window.state != nvtop_option_state_hidden)
      draw_options(interface);
    draw_option_selection(interface);
    draw_plots(interface);
  } else {
    draw_setup_window(devices_count, devices, interface);
  }
  doupdate();
  refresh();
}

void update_window_size_to_terminal_size(struct nvtop_interface *inter) {
  endwin();
  erase();
  refresh();
  refresh();
  delete_all_windows(inter);
  struct option_window options = inter->process.option_window;
  initialize_all_windows(inter);
  inter->process.option_window.selected_row = options.selected_row;
  inter->process.option_window.state = options.state;
}

bool is_escape_for_quit(struct nvtop_interface *interface) {
  if (interface->process.option_window.state == nvtop_option_state_hidden &&
      !interface->setup_win.visible)
    return true;
  else
    return false;
}

static void option_do_kill(struct nvtop_interface *interface) {
  if (interface->process.option_window.selected_row == 0)
    return;
  pid_t pid = interface->process.selected_pid;
  int sig = signalValues[interface->process.option_window.selected_row];
  kill(pid, sig);
}

static void option_change_sort(struct nvtop_interface *interface) {
  if (interface->process.option_window.selected_row == 0)
    return;
  interface->options.sort_processes_by =
      process_pid + interface->process.option_window.selected_row - 1;
}

void interface_key(int keyId, struct nvtop_interface *interface) {
  if (interface->setup_win.visible) {
    handle_setup_win_keypress(keyId, interface);
    return;
  }
  switch (keyId) {
  case KEY_F(2):
    if (interface->process.option_window.state == nvtop_option_state_hidden &&
        !interface->setup_win.visible) {
      show_setup_window(interface);
    }
    break;
  case KEY_F(12):
    save_interface_options_to_config_file(interface->devices_count,
                                          &interface->options);
    break;
  case KEY_F(9):
    if (interface->process.option_window.state == nvtop_option_state_hidden) {
      werase(interface->process.option_window.option_selection_window);
      interface->process.option_window.state = nvtop_option_state_kill;
      interface->process.option_window.selected_row = 0;
    }
    break;
  case KEY_F(6):
    if (interface->process.option_window.state == nvtop_option_state_hidden) {
      werase(interface->process.option_window.option_selection_window);
      interface->process.option_window.state = nvtop_option_state_sort_by;
      interface->process.option_window.selected_row = 0;
    }
    break;
  case KEY_RIGHT:
    if (interface->process.option_window.state == nvtop_option_state_hidden)
      interface->process.offset_column += 4;
    break;
  case KEY_LEFT:
    if (interface->process.option_window.state == nvtop_option_state_hidden &&
        interface->process.offset_column >= 4)
      interface->process.offset_column -= 4;
    break;
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
      werase(interface->process.option_window.option_selection_window);
      break;
    case nvtop_option_state_sort_by:
      option_change_sort(interface);
      interface->process.option_window.state = nvtop_option_state_hidden;
      werase(interface->process.option_window.option_selection_window);
      break;
    case nvtop_option_state_hidden:
    default:
      break;
    }
    break;
  case 27:
    interface->process.option_window.state = nvtop_option_state_hidden;
    werase(interface->process.option_window.option_selection_window);
    break;
  default:
    break;
  }
}

bool interface_freeze_processes(struct nvtop_interface *interface) {
  return interface->process.option_window.state == nvtop_option_state_kill;
}

extern inline void set_attribute_between(WINDOW *win, int startY, int startX,
                                         int endX, attr_t attr, short pair);

int interface_update_interval(const struct nvtop_interface *interface) {
  return interface->options.update_interval;
}
