/*
 *
 * Copyright (C) 2017 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <ncurses.h>
#include <signal.h>
#include <inttypes.h>

#define DEVICE_ID_SIZE 16

enum interface_color {
  cyan_color = 1,
  red_color,
  green_color,
  yellow_color,
  blue_color,
  magenta_color,
};

struct device_window {
  char *device_name;
  WINDOW *name_win;   // Name of the GPU
  WINDOW* gpu_util;
  WINDOW* mem_util;
  WINDOW* encode_util;
  WINDOW* decode_util;
  WINDOW* fan_speed;
  WINDOW* temperature;
  WINDOW* power_info;
  WINDOW* gpu_clock_info;
  WINDOW* mem_clock_info;
  WINDOW* pcie_info;
};

struct all_gpu_processes {
  intmax_t pid;
  char *process_name;
  char *user_name;
  unsigned long long used_memory;
  double mem_percentage;
  unsigned int gpu_id;
  bool is_graphical;
};

struct process_window {
  unsigned int size_biggest_name;
  unsigned int size_processes_buffer;
  unsigned int num_processes;
  unsigned int offset;
  struct all_gpu_processes *all_process;
  WINDOW *process_win;
  unsigned int selected_row;
  enum process_field sort_criterion;
  bool sort_asc;
};

struct option_window {
  enum nvtop_option_window_state state;
  unsigned int selected_row;
  unsigned int offset;
  WINDOW *option_win;
  WINDOW *option_selection_window;
};

static const unsigned int option_window_size = 13;

struct nvtop_interface {
  size_t num_devices;
  struct device_window *devices_win;
  struct process_window process;
  struct option_window option_window;
};

enum device_field {
  device_name = 0,
  device_fan_speed,
  device_temperature,
  device_power,
  device_pcie,
  device_clock,
  device_count,
};

static unsigned int sizeof_device_field[] = {
  [device_name] = 11,
  [device_fan_speed] = 8,
  [device_temperature] = 10,
  [device_power] = 15,
  [device_clock] = 11,
  [device_pcie] = 44,
};

static unsigned int sizeof_process_field[] = {
  [process_pid] = 5,
  [process_user] = 4,
  [process_gpu_id] = 3,
  [process_type] = 7,
  [process_memory] = 14,
  [process_command] = 0,
};

enum ui_window_size_id {
  ui_window_gpu_info,
  ui_window_process_info,
  ui_window_option_selection,
};

static unsigned int sizeof_ui_windows[] = {
  [ui_window_gpu_info] = 4,
  [ui_window_process_info] = 0,
  [ui_window_option_selection] = 1,
};

static void alloc_device_window(
    unsigned int device_id,
    unsigned int row,
    unsigned int totalcol,
    struct device_window *dwin) {

  const unsigned int spacer = 1;

  // Assume at least 80 columns

  // Line 1 = Name | PCIe info

  dwin->name_win = newwin(1, sizeof_device_field[device_name],
      row,
      spacer);
  if (dwin->device_name != NULL) {
      wattron(dwin->name_win, COLOR_PAIR(cyan_color));
      mvwprintw(dwin->name_win, 0, 0, "Device %-2u", device_id);
      wattroff(dwin->name_win, COLOR_PAIR(cyan_color));
      wprintw(dwin->name_win, "[%s]", dwin->device_name);
      wnoutrefresh(dwin->name_win);
  }
  dwin->pcie_info = newwin(1, sizeof_device_field[device_pcie],
      row,
      spacer*2 + sizeof_device_field[device_name]);

  // Line 2 = GPU clk | MEM clk | Temp | Fan | Power
  dwin->gpu_clock_info  = newwin(1, sizeof_device_field[device_clock],
      row+1,
      spacer);
  dwin->mem_clock_info  = newwin(1, sizeof_device_field[device_clock],
      row+1,
      spacer * 2 + sizeof_device_field[device_clock]);
  dwin->temperature  = newwin(1, sizeof_device_field[device_temperature],
      row+1,
      spacer * 3 + sizeof_device_field[device_clock] * 2);
  dwin->fan_speed  = newwin(1, sizeof_device_field[device_fan_speed],
      row+1,
      spacer * 4 +
      sizeof_device_field[device_clock] * 2 +
      sizeof_device_field[device_temperature]);
  dwin->power_info  = newwin(1, sizeof_device_field[device_power],
      row+1,
      spacer * 5 +
      sizeof_device_field[device_clock] * 2 +
      sizeof_device_field[device_temperature] +
      sizeof_device_field[device_fan_speed]);

  // Line 3 = GPU used | MEM used | Encoder | Decoder

  int remaining_cols = totalcol - 5 * spacer;
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

  dwin->gpu_util = newwin(1, size_gpu, row+2, spacer);
  dwin->mem_util = newwin(1, size_mem,
      row+2,
      spacer * 2 + size_gpu);
  dwin->encode_util = newwin(1, size_encode,
      row+2,
      spacer * 3 + size_gpu + size_mem);
  dwin->decode_util = newwin(1, size_decode,
      row+2,
      spacer * 4 + size_gpu + size_mem + size_encode);
}

static void free_device_windows(struct device_window *dwin) {
  delwin(dwin->name_win);
  delwin(dwin->gpu_util);
  delwin(dwin->mem_util);
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
    bool alloc_place_for_option) {
  int rows, cols;
  getmaxyx(stdscr, rows, cols);

  sizeof_ui_windows[ui_window_process_info] =
    rows -
    interface->num_devices * sizeof_ui_windows[ui_window_gpu_info] -
    sizeof_ui_windows[ui_window_option_selection];

  if (sizeof_ui_windows[ui_window_process_info] > 0) {
    if (interface->process.process_win != NULL)
      delwin(interface->process.process_win);

    interface->process.size_processes_buffer =
      sizeof_ui_windows[ui_window_process_info] > 50 ?
      sizeof_ui_windows[ui_window_process_info] : 50;

    unsigned int cols_alloc = cols;
    unsigned int cols_offset = 0;
    if (alloc_place_for_option) {
      cols_alloc -= option_window_size;
      cols_offset += option_window_size;
    }

    interface->process.process_win =
      newwin(sizeof_ui_windows[ui_window_process_info], cols_alloc,
          interface->num_devices * sizeof_ui_windows[ui_window_gpu_info],
          cols_offset);
  } else {
    interface->option_window.state = nvtop_option_state_hidden;
  }
}

static void initialize_all_windows(struct nvtop_interface *dwin) {
  int rows, cols;
  getmaxyx(stdscr, rows, cols);

  unsigned int num_devices = dwin->num_devices;
  for (unsigned int i = 0; i < num_devices; ++i) {
    alloc_device_window(i, i*sizeof_ui_windows[ui_window_gpu_info],
        cols, &dwin->devices_win[i]);
  }

  alloc_process_with_option(dwin,
      dwin->option_window.state != nvtop_option_state_hidden);
  dwin->option_window.option_win =
    newwin(sizeof_ui_windows[ui_window_process_info], option_window_size,
        num_devices * sizeof_ui_windows[ui_window_gpu_info], 0);

  dwin->option_window.option_selection_window =
    newwin(sizeof_ui_windows[ui_window_option_selection], cols, rows-1, 0);
}

static void delete_all_windows(
    struct nvtop_interface *dwin) {
  for (unsigned int i = 0; i < dwin->num_devices; ++i) {
    free_device_windows(&dwin->devices_win[i]);
  }
  delwin(dwin->process.process_win);
  dwin->process.process_win = NULL;
  delwin(dwin->option_window.option_selection_window);
  delwin(dwin->option_window.option_win);
}

void show_gpu_infos_ascii(
    unsigned int num_devices,
    struct device_info *dev_info) {

  for (unsigned int i = 0; i < num_devices; ++i) {
    printf(
        "GPU %u: %s @ (%uMHz,%uMHz),"
        " Util. (%u%% , %u%%),"
        " FAN %u%%,"
        " TEMP %u°c,"
        " POWER %uW / %uW\n",
        i,
        dev_info[i].device_name,
        dev_info[i].gpu_clock_speed,
        dev_info[i].mem_clock_speed,
        dev_info[i].gpu_util_rate,
        dev_info[i].mem_util_rate,
        dev_info[i].fan_speed,
        dev_info[i].gpu_temp,
        dev_info[i].power_draw/1000,
        dev_info[i].power_draw_max/1000);
  }
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
  init_pair(cyan_color,     COLOR_CYAN,     background_color);
  init_pair(red_color,      COLOR_RED,      background_color);
  init_pair(green_color,    COLOR_GREEN,    background_color);
  init_pair(yellow_color,   COLOR_YELLOW,   background_color);
  init_pair(blue_color,     COLOR_BLUE,     background_color);
  init_pair(magenta_color,  COLOR_MAGENTA,  background_color);
}

struct nvtop_interface* initialize_curses(
    unsigned int num_devices,
    unsigned int biggest_device_name,
    bool use_color) {
  struct nvtop_interface *interface = calloc(1, sizeof(*interface));
  interface->devices_win = calloc(num_devices, sizeof(*interface->devices_win));
  interface->num_devices = num_devices;
  sizeof_device_field[device_name] = biggest_device_name + 11;
  initscr();
  if (use_color && has_colors() == TRUE) {
    initialize_colors();
  }
  cbreak();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);
  interface->process.offset = 0;
  interface->option_window.offset = 0;
  interface->option_window.state = nvtop_option_state_hidden;
  interface->process.selected_row = 0;
  interface->process.sort_criterion = process_memory;
  interface->process.sort_asc = true;
  interface->process.size_processes_buffer = 50;
  interface->process.all_process = malloc(interface->process.size_processes_buffer *
      sizeof(*interface->process.all_process));
  initialize_all_windows(interface);
  refresh();
  return interface;
}

void clean_ncurses(struct nvtop_interface *interface) {
  endwin();
  delete_all_windows(interface);
  for (unsigned int i = 0; i < interface->num_devices; ++i)
    free(interface->devices_win[i].device_name);
  free(interface->devices_win);
  free(interface->process.all_process);
  free(interface);
}

static void draw_bare_percentage(
    WINDOW *win,
    const char *prelude, unsigned int new_percentage,
    const char inside_braces_right[1024]) {
  int rows, cols;
  getmaxyx(win, rows, cols);
  (void) rows;
  size_t size_prelude = strlen(prelude);
  wattron(win, COLOR_PAIR(cyan_color));
  mvwprintw(win, 0, 0, "%s", prelude);
  wattroff(win, COLOR_PAIR(cyan_color));
  waddch(win, '[');
  int curx, cury;
  curx = getcurx(win);
  cury = getcury(win);
  int between_sbraces = cols - size_prelude - 2;
  double usage =
    round((float) between_sbraces *  new_percentage / 100.f);
  int represent_usage = (int) usage;
  whline(win, '|', (int)represent_usage);
  mvwhline(win, cury, curx+represent_usage, ' ', between_sbraces - represent_usage);
  mvwaddch(win, cury, curx + between_sbraces, ']');
  unsigned int right_side_braces_space_required = strlen(inside_braces_right);
  wmove(win, cury, curx + between_sbraces - right_side_braces_space_required);
  wprintw(win, "%s", inside_braces_right);
  mvwchgat(win, cury, curx, represent_usage, 0, green_color, NULL);
  wnoutrefresh(win);
}

static const char* memory_prefix[] = { "B", "k", "M", "G", "T", "P" };

static void draw_temp_color(WINDOW *win,
    unsigned int temp,
    unsigned int temp_slowdown) {
  mvwprintw(win, 0, 0, "TEMP %3u", temp);
  waddch(win, ACS_DEGREE);
  waddch(win, 'C');
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
    val_d = val_d / 1000.;
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

static void draw_devices(
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

  unsigned int num_devices = interface->num_devices;
  for (unsigned int i = 0; i < num_devices; ++i) {
    struct device_window *dev = &interface->devices_win[i];
    struct device_info *dinfo = &dev_info[i];
    if (dev->device_name == NULL) {
      size_t namelen = strlen(dinfo->device_name) + 1;
      dev->device_name = malloc(namelen);
      memcpy(dev->device_name, dinfo->device_name, namelen);
      wattron(dev->name_win, COLOR_PAIR(cyan_color));
      mvwprintw(dev->name_win, 0, 0, "Device %-2u", i);
      wattroff(dev->name_win, COLOR_PAIR(cyan_color));
      wprintw(dev->name_win, "[%s]", dev->device_name);
      wnoutrefresh(dev->name_win);
    }
    char buff[1024];
    if (IS_VALID(gpu_util_rate_valid, dinfo->valid)) {
      snprintf(buff, 1024, "%u%%", dinfo->gpu_util_rate);
      draw_bare_percentage(dev->gpu_util, "GPU-Util", dinfo->gpu_util_rate, buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(dev->gpu_util, "GPU-Util", 0, buff);
    }

    if (IS_VALID(total_memory_valid, dinfo->valid) && IS_VALID(used_memory_valid, dinfo->valid)) {
      double total_mem = dinfo->total_memory;
      double used_mem = dinfo->used_memory;
      size_t prefix_off;
      for (prefix_off = 0; prefix_off < 5 && total_mem >= 1000; ++prefix_off) {
        total_mem /= 1000;
        used_mem /= 1000;
      }
      snprintf(buff, 1024, "%.1f%s/%.1f%s",
          used_mem,  memory_prefix[prefix_off],
          total_mem, memory_prefix[prefix_off]);
      draw_bare_percentage(dev->mem_util, "MEM-Util",
          (unsigned int)(100. * dinfo->used_memory / (double)dinfo->total_memory),
          buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(dev->mem_util, "MEM-Util", 0, buff);
    }
    if (IS_VALID(encoder_rate_valid, dinfo->valid)) {
      snprintf(buff, 1024, "%u%%", dinfo->encoder_rate);
      draw_bare_percentage(dev->encode_util, "Encoder",
          dinfo->encoder_rate,
          buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(dev->encode_util, "Encoder", 0, buff);
    }
    if (IS_VALID(decoder_rate_valid, dinfo->valid)) {
      snprintf(buff, 1024, "%u%%", dinfo->decoder_rate);
      draw_bare_percentage(dev->decode_util, "Decoder",
          dinfo->decoder_rate,
          buff);
    } else {
      snprintf(buff, 1024, "N/A");
      draw_bare_percentage(dev->decode_util, "Decoder", 0, buff);
    }
    if (IS_VALID(gpu_temp_valid         , dinfo->valid) &&
        IS_VALID(gpu_temp_slowdown_valid, dinfo->valid))
      draw_temp_color(dev->temperature,
          dinfo->gpu_temp,
          dinfo->gpu_temp_slowdown);
    else {
      mvwprintw(dev->temperature, 0, 0, "TEMP N/A°C");
      wnoutrefresh(dev->temperature);
    }

    // FAN
    if (IS_VALID(fan_speed_valid, dinfo->valid))
      mvwprintw(dev->fan_speed, 0, 0, "FAN %3u%%", dinfo->fan_speed);
    else
      mvwprintw(dev->fan_speed, 0, 0, "FAN N/A%%");
    mvwchgat(dev->fan_speed, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->fan_speed);

    // GPU CLOCK
    werase(dev->gpu_clock_info);
    if (IS_VALID(gpu_clock_speed_valid, dinfo->valid))
      mvwprintw(dev->gpu_clock_info, 0, 0,
          "GPU %uMHz",
          dinfo->gpu_clock_speed);
    else
      mvwprintw(dev->gpu_clock_info, 0, 0, "GPU N/A MHz");

    mvwchgat(dev->gpu_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->gpu_clock_info);

    // MEM CLOCK
    werase(dev->mem_clock_info);
    if (IS_VALID(mem_clock_speed_valid, dinfo->valid))
      mvwprintw(dev->mem_clock_info, 0, 0,
          "MEM %uMHz",
          dinfo->mem_clock_speed);
    else
      mvwprintw(dev->mem_clock_info, 0, 0, "MEM N/A MHz");
    mvwchgat(dev->mem_clock_info, 0, 0, 3, 0, cyan_color, NULL);
    wnoutrefresh(dev->mem_clock_info);

    // POWER
    werase(dev->power_info);
    if (IS_VALID(power_draw_valid    , dinfo->valid) &&
        IS_VALID(power_draw_max_valid, dinfo->valid))
      mvwprintw(dev->power_info, 0, 0,
          "POW %3u / %3u W",
          dinfo->power_draw / 1000, dinfo->power_draw_max / 1000);
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
    if (IS_VALID(cur_pcie_link_gen_valid  , dinfo->valid) &&
        IS_VALID(cur_pcie_link_width_valid, dinfo->valid))
      wprintw(dev->pcie_info, "%u@%2ux", dinfo->cur_pcie_link_gen,
          dinfo->cur_pcie_link_width);
    else
      wprintw(dev->pcie_info, "N/A");

    wattron(dev->pcie_info, COLOR_PAIR(magenta_color));
    wprintw(dev->pcie_info, " RX: ");
    wattroff(dev->pcie_info, COLOR_PAIR(magenta_color));
    if (IS_VALID(pcie_rx_valid, dinfo->valid))
      print_pcie_at_scale(dev->pcie_info, dinfo->pcie_rx);
    else
      wprintw(dev->pcie_info, "N/A");
    wattron(dev->pcie_info, COLOR_PAIR(magenta_color));
    wprintw(dev->pcie_info, " TX: ");
    wattroff(dev->pcie_info, COLOR_PAIR(magenta_color));
    if (IS_VALID(pcie_tx_valid, dinfo->valid))
      print_pcie_at_scale(dev->pcie_info, dinfo->pcie_tx);
    else
      wprintw(dev->pcie_info, "N/A");

    wnoutrefresh(dev->pcie_info);
  }
}

static inline unsigned int max_val(unsigned int a, unsigned int b) {
  return a > b ? a : b;
}

static unsigned int copy_processes_for_processing(
    unsigned int num_devices,
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

  unsigned int maximum_name_length = 0;
  unsigned int total_processes = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    total_processes += dev_info[i].num_compute_procs + dev_info[i].num_graphical_procs;
  }
  while (total_processes > interface->process.size_processes_buffer) {
    interface->process.size_processes_buffer *= 2;
    interface->process.all_process = realloc(interface->process.all_process,
        interface->process.size_processes_buffer *
        sizeof(*interface->process.all_process));
  }
  unsigned int offset = 0;
  for (unsigned int i = 0; i < num_devices; ++i) {
    for (unsigned int j = 0; j < dev_info[i].num_compute_procs; ++j) {
      interface->process.all_process[offset+j].is_graphical = false;
      interface->process.all_process[offset+j].gpu_id = i;
      interface->process.all_process[offset+j].pid =
        dev_info[i].compute_procs[j].pid;
      interface->process.all_process[offset+j].process_name = dev_info[i].compute_procs[j].process_name;
      interface->process.all_process[offset+j].user_name = dev_info[i].compute_procs[j].user_name;
      interface->process.all_process[offset+j].used_memory =
        dev_info[i].compute_procs[j].used_memory;
      maximum_name_length = max_val(maximum_name_length,
            strlen(dev_info[i].compute_procs[j].user_name));
      interface->process.all_process[offset+j].mem_percentage =
        interface->process.all_process[offset+j].used_memory * 100. /
        (double) dev_info[i].total_memory;
    }
    offset += dev_info[i].num_compute_procs;
    for (unsigned int j = 0; j < dev_info[i].num_graphical_procs; ++j) {
      interface->process.all_process[offset+j].is_graphical = true;
      interface->process.all_process[offset+j].gpu_id = i;
      interface->process.all_process[offset+j].pid =
        dev_info[i].graphic_procs[j].pid;
      interface->process.all_process[offset+j].process_name = dev_info[i].graphic_procs[j].process_name;
      interface->process.all_process[offset+j].user_name = dev_info[i].graphic_procs[j].user_name;
      interface->process.all_process[offset+j].used_memory =
        dev_info[i].graphic_procs[j].used_memory;
      maximum_name_length = max_val(maximum_name_length,
            strlen(dev_info[i].graphic_procs[j].user_name));
      interface->process.all_process[offset+j].mem_percentage =
        interface->process.all_process[offset+j].used_memory * 100. /
        (double) dev_info[i].total_memory;
    }
    offset += dev_info[i].num_graphical_procs;
  }
  interface->process.size_biggest_name = max_val(interface->process.size_biggest_name, maximum_name_length);

  return total_processes;
}

static int compare_pid_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->pid + p2->pid;
}

static int compare_pid_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_pid_desc(pp1, pp2);
}

static int compare_username_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return -strcmp(p1->user_name, p2->user_name);
}

static int compare_username_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_username_desc(pp1, pp2);
}

static int compare_process_name_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return -strncmp(p1->process_name, p2->process_name, sizeof(p1->user_name));
}

static int compare_process_name_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_process_name_desc(pp1, pp2);
}

static int compare_mem_usage_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->used_memory + p2->used_memory;
}

static int compare_mem_usage_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_mem_usage_desc(pp1, pp2);
}

static int compare_gpu_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->gpu_id + p2->gpu_id;
}

static int compare_gpu_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_gpu_desc(pp1, pp2);
}

static int compare_process_type_desc(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return p1->is_graphical != p2->is_graphical;
}

static int compare_process_type_asc(
    const void *pp1,
    const void *pp2) {
  return -compare_process_name_desc(pp1, pp2);
}

static void sort_process(
    struct all_gpu_processes *proc,
    unsigned int total_process,
    enum process_field criterion,
    bool asc_sort) {
  int (*sort_fun)(const void*, const void*);
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
    case process_end:
    default:
      return;
  }
  qsort(proc, total_process, sizeof(*proc), sort_fun);
}

static const char *columnName[] = {
  "PID",
  "USER",
  "GPU",
  "TYPE",
  "MEM",
  "Command",
};

static void print_process_preamble(WINDOW *win,
    unsigned int sorted_by) {
  // PID | Username | GPU id | GPU_TYPE | Mem usage | Process name
  /*wmove(win, 0, 0);*/
  for (unsigned int i = 0; i < process_command; ++i) {
    if (sorted_by == i)
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    else
      wattron(win, COLOR_PAIR(green_color) | A_STANDOUT);

    wprintw(win, "%*s ", sizeof_process_field[i], columnName[i]);

    if (sorted_by == i)
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    else
      wattroff(win, COLOR_PAIR(green_color) | A_STANDOUT);
  }
  if (sorted_by == process_command)
    wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
  else
    wattron(win, COLOR_PAIR(green_color) | A_STANDOUT);

  wprintw(win, "Command");

  if (sorted_by == process_command)
    wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
  else
    wattroff(win, COLOR_PAIR(green_color) | A_STANDOUT);
  wchgat(win, -1, A_STANDOUT, green_color, NULL);
  wnoutrefresh(win);
}

static void update_selected_offset_with_window_size(
    unsigned int *selected_row,
    unsigned int *offset,
    unsigned int row_available_to_draw,
    unsigned int num_to_draw) {

  if (*selected_row > num_to_draw - 1)
    *selected_row = num_to_draw - 1;

  if (*offset > *selected_row)
    *offset = *selected_row;

  if (*offset + row_available_to_draw - 1 < *selected_row)
    *offset = *selected_row - row_available_to_draw + 1;

  while (row_available_to_draw > num_to_draw - *offset &&
      *offset != 0)
    *offset -= 1;
}

static void print_processes_on_screen(
    unsigned int num_process, struct process_window *process) {
  WINDOW *win = process->process_win;
  struct all_gpu_processes *proc = process->all_process;

  werase(win);
  print_process_preamble(win, process->sort_criterion);

  unsigned int rows, cols;
  getmaxyx(win, rows, cols);
  rows -= 1;

  update_selected_offset_with_window_size(
      &process->selected_row,
      &process->offset,
      rows,
      num_process);

  size_t special_row = process->selected_row;

  char pid_str[sizeof_process_field[process_pid]+1];
  char guid_str[sizeof_process_field[process_gpu_id]+1];
  char memory[sizeof_process_field[process_memory]+1];

  unsigned int start_at_process = process->offset;
  unsigned int end_at_process = start_at_process + rows;

  for (unsigned int i = start_at_process; i < end_at_process && i < num_process; ++i) {
    if (i == special_row) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    unsigned int write_at = i+1-start_at_process;
    size_t size = snprintf(pid_str, sizeof_process_field[process_pid]+1,
        "%" PRIdMAX, proc[i].pid);
    if (size == sizeof_process_field[process_pid]+1)
      pid_str[sizeof_process_field[process_pid]] = '\0';
    size = snprintf(guid_str, sizeof_process_field[process_gpu_id]+1,
        "%u", proc[i].gpu_id);
    if (size == sizeof_process_field[process_gpu_id]+1)
      pid_str[sizeof_process_field[process_gpu_id]] = '\0';
    size = snprintf(memory, sizeof_process_field[process_memory]+1,
        "%lluMB %.1f%%", proc[i].used_memory/1000000,
        proc[i].mem_percentage);
    mvwprintw(win, write_at, 0, "%*s %*s %*s",
        sizeof_process_field[process_pid],
        pid_str,
        sizeof_process_field[process_user],
        proc[i].user_name,
        sizeof_process_field[process_gpu_id],
        guid_str);
    if (proc[i].is_graphical) {
      if (i != special_row)
        wattron(win, COLOR_PAIR(yellow_color));
      wprintw(win, " %*s ", sizeof_process_field[process_type], "Graphic");
      if (i != special_row)
        wattroff(win, COLOR_PAIR(yellow_color));
    } else {
      if (i != special_row)
        wattron(win, COLOR_PAIR(magenta_color));
      wprintw(win, " %*s ", sizeof_process_field[process_type], "Compute");
      if (i != special_row)
        wattroff(win, COLOR_PAIR(magenta_color));
    }
    wprintw(win, "%*s ",
        sizeof_process_field[process_memory],
        memory);
    wprintw(win, "%s", proc[i].process_name);
    if (i == special_row) {
      unsigned int cur_row, cur_col;
      getyx(win, cur_row, cur_col);
      (void) cur_row;
      for (unsigned int j = cur_col; j < cols; ++j)
        wprintw(win, " ");
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
  }
  wnoutrefresh(win);
}

static void draw_processes(
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

  if (interface->process.process_win == NULL)
    return;
  unsigned int num_devices = interface->num_devices;
  unsigned int total_processes = 0;
  if (interface->option_window.state == nvtop_option_state_hidden) {
    total_processes = copy_processes_for_processing(num_devices,
        dev_info, interface);
    interface->process.num_processes = total_processes;
    sort_process(interface->process.all_process, total_processes,
        interface->process.sort_criterion, interface->process.sort_asc);
  } else {
    total_processes = interface->process.num_processes;
  }

  unsigned int rows, cols;
  getmaxyx(interface->process.process_win, rows, cols);
  rows -= 1;
  sizeof_process_field[process_user] = interface->process.size_biggest_name;

  size_t process_name_size = cols;
  for (enum process_field i = process_pid; i < process_end; ++i) {
    size_t size_field = sizeof_process_field[i] + 1;
    if (process_name_size > size_field) {
      process_name_size -= size_field;
    } else {
      process_name_size = 0;
      break;
    }
  }
  for (size_t i = 0; i < total_processes; ++i)
    if (strlen(interface->process.all_process[i].process_name) > process_name_size)
      interface->process.all_process[i].process_name[process_name_size] = '\0';

  print_processes_on_screen(total_processes, &interface->process);
}

static const char *signalsName[] = {
"Cancel",
"SIGABRT",
"SIGALRM",
"SIGBUS",
"SIGCHLD",
"SIGCONT",
"SIGFPE",
"SIGHUP",
"SIGILL",
"SIGINT",
"SIGKILL",
"SIGPIPE",
"SIGQUIT",
"SIGSEGV",
"SIGSTOP",
"SIGTERM",
"SIGTSTP",
"SIGTTIN",
"SIGTTOU",
"SIGUSR1",
"SIGUSR2",
"SIGPOLL",
"SIGPROF",
"SIGSYS",
"SIGTRAP",
"SIGURG",
"SIGVTALRM",
"SIGXCPU",
"SIGXFSZ",
};

static const int signalsValues[] = {
SIGABRT  ,
SIGALRM  ,
SIGBUS   ,
SIGCHLD  ,
SIGCONT  ,
SIGFPE   ,
SIGHUP   ,
SIGILL   ,
SIGINT   ,
SIGKILL  ,
SIGPIPE  ,
SIGQUIT  ,
SIGSEGV  ,
SIGSTOP  ,
SIGTERM  ,
SIGTSTP  ,
SIGTTIN  ,
SIGTTOU  ,
SIGUSR1  ,
SIGUSR2  ,
SIGPOLL  ,
SIGPROF  ,
SIGSYS   ,
SIGTRAP  ,
SIGURG   ,
SIGVTALRM,
SIGXCPU  ,
SIGXFSZ  ,
};

static const size_t nvtop_num_signals = 28;

static void draw_kill_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->option_window.option_win;
  wattron(win, COLOR_PAIR(green_color) | A_REVERSE);
  mvwprintw(win, 0, 0, "Send signal:");
  wattroff(win, COLOR_PAIR(green_color) | A_REVERSE);
  wprintw(win, " ");
  int rows, cols;
  getmaxyx(win, rows, cols);

  size_t start_at_option = interface->option_window.offset;
  size_t end_at_option = start_at_option + rows - 1;

  for (size_t i = start_at_option; i < end_at_option && i <= nvtop_num_signals; ++i) {
    if (i == interface->option_window.selected_row) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "%*d %s", 2, i, signalsName[i]);
    getyx(win, rows, cols);

    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (i == interface->option_window.selected_row) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size-1, " ");
    }
  }
  wnoutrefresh(win);
}

static void draw_sort_option(struct nvtop_interface *interface) {
  WINDOW *win = interface->option_window.option_win;
  wattron(win, COLOR_PAIR(green_color) | A_REVERSE);
  mvwprintw(win, 0, 0, "Sort by     ");
  wattroff(win, COLOR_PAIR(green_color) | A_REVERSE);
  wprintw(win, " ");
  int rows, cols;
  if (interface->option_window.offset == 0) {
    if (interface->option_window.selected_row == 0) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "Cancel");
    getyx(win, rows, cols);
    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (interface->option_window.selected_row == 0) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size-1, " ");
    }
  }
  getmaxyx(win, rows, cols);

  size_t start_at_option =
    interface->option_window.offset == 0 ?
    interface->option_window.offset :
    interface->option_window.offset- 1;
  size_t end_at_option =
    interface->option_window.offset == 0 ?
    start_at_option + rows - 2 :
    start_at_option + rows - 1;

  for (size_t i = start_at_option; i < end_at_option && i < process_end; ++i) {
    if (i+1 == interface->option_window.selected_row) {
      wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
    }
    wprintw(win, "%s", columnName[i]);
    getyx(win, rows, cols);
    for (unsigned int j = cols; j < option_window_size; ++j)
      wprintw(win, " ");
    if (i+1 == interface->option_window.selected_row) {
      wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      mvwprintw(win, rows, option_window_size-1, " ");
    }
  }
  wnoutrefresh(win);
}

static void draw_options(struct nvtop_interface *interface) {
  unsigned int rows, cols;
  getmaxyx(interface->option_window.option_win, rows, cols);
  rows -= 1;
  (void) cols;
  unsigned int num_options = 0;
  switch (interface->option_window.state) {
    case nvtop_option_state_kill:
      num_options = nvtop_num_signals + 1; // Option + Cancel
      break;
    case nvtop_option_state_sort_by:
      num_options = process_end + 1; // Option + Cancel
      break;
    case nvtop_option_state_hidden:
    default:
      break;
  }

  update_selected_offset_with_window_size(
      &interface->option_window.selected_row,
      &interface->option_window.offset,
      rows,
      num_options);

  switch (interface->option_window.state) {
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
  "Kill",
  "Sort",
  "Quit",
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
  WINDOW *win = interface->option_window.option_selection_window;
  wmove(win, 0, 0);
  switch(interface->option_window.state) {
    case nvtop_option_state_hidden:
      for (size_t i = 0; i < 3; ++i) {
        wprintw(win, "F%zu", i+1);
        wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
        wprintw(win, "%s", option_selection_hidden[i]);
        for (size_t j = strlen(option_selection_hidden[i]); j < option_selection_width; ++j)
          wprintw(win, " ");
        wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      }
      break;
    case nvtop_option_state_kill:
      for (size_t i = 0; i < 2; ++i) {
        wprintw(win, "%s", option_selection_kill[i][0]);
        wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
        wprintw(win, "%s", option_selection_kill[i][1]);
        for (size_t j = strlen(option_selection_kill[i][1]); j < option_selection_width; ++j)
          wprintw(win, " ");
        wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      }
      break;
    case nvtop_option_state_sort_by:
      for (size_t i = 0; i < 4; ++i) {
        wprintw(win, "%s", option_selection_sort[i][0]);
        wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
        wprintw(win, "%s", option_selection_sort[i][1]);
        for (size_t j = strlen(option_selection_sort[i][1]); j < option_selection_width; ++j)
          wprintw(win, " ");
        wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
      }
      break;
    default:
      break;
  }
  unsigned int cur_col, maxcols, tmp;
  getmaxyx(stdscr, tmp, maxcols);
  getyx(win, tmp, cur_col);
  (void) tmp;
  wattron(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
  for (unsigned int j = cur_col; j < maxcols; ++j)
    wprintw(win, " ");
  wattroff(win, COLOR_PAIR(cyan_color) | A_STANDOUT);
  wnoutrefresh(win);
}

void draw_gpu_info_ncurses(
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

  draw_devices(dev_info, interface);
  draw_processes(dev_info, interface);
  if (interface->option_window.state != nvtop_option_state_hidden)
    draw_options(interface);
  draw_option_selection(interface);
  doupdate();
  refresh();

}

void update_window_size_to_terminal_size(struct nvtop_interface *inter) {
  endwin();
  erase();
  refresh();
  refresh();
  delete_all_windows(inter);
  struct option_window options = inter->option_window;
  initialize_all_windows(inter);
  inter->option_window.selected_row = options.selected_row;
  inter->option_window.state = options.state;
}

bool is_escape_for_quit(struct nvtop_interface *inter) {
  if (inter->option_window.state == nvtop_option_state_hidden)
    return true;
  else
    return false;
}

static void option_do_kill(struct nvtop_interface *inter) {
  if (inter->option_window.selected_row == 0)
    return;
  pid_t pid = inter->process.all_process[inter->process.selected_row].pid;
  int sig = signalsValues[inter->option_window.selected_row-1];
  kill(pid,sig);
}

static void option_change_sort(struct nvtop_interface *inter) {
  if (inter->option_window.selected_row == 0)
    return;
  inter->process.sort_criterion = process_pid + inter->option_window.selected_row - 1;
}

void interface_key(int keyId, struct nvtop_interface *inter) {
  switch(keyId) {
      case KEY_F(1):
        if (inter->option_window.state == nvtop_option_state_hidden) {
          inter->option_window.state = nvtop_option_state_kill;
          inter->option_window.selected_row = 0;
          alloc_process_with_option(inter, true);
        }
        break;
      case KEY_F(2):
        if (inter->option_window.state == nvtop_option_state_hidden) {
          inter->option_window.state = nvtop_option_state_sort_by;
          inter->option_window.selected_row = 0;
          alloc_process_with_option(inter, true);
        }
        break;
      case KEY_UP:
        switch (inter->option_window.state) {
          case nvtop_option_state_kill:
          case nvtop_option_state_sort_by:
            if (inter->option_window.selected_row != 0)
              inter->option_window.selected_row --;
            break;
          case nvtop_option_state_hidden:
            if (inter->process.selected_row != 0)
              inter->process.selected_row --;
            break;
          default:
            break;
        }
        break;
      case KEY_DOWN:
        switch (inter->option_window.state) {
          case nvtop_option_state_kill:
          case nvtop_option_state_sort_by:
              inter->option_window.selected_row ++;
            break;
          case nvtop_option_state_hidden:
              inter->process.selected_row ++;
            break;
          default:
            break;
        }
        break;
      case '+':
        inter->process.sort_asc = true;
        break;
      case '-':
        inter->process.sort_asc = false;
        break;
      case '\n':
      case KEY_ENTER:
        switch (inter->option_window.state) {
          case nvtop_option_state_kill:
            option_do_kill(inter);
            inter->option_window.state = nvtop_option_state_hidden;
            alloc_process_with_option(inter, false);
            break;
          case nvtop_option_state_sort_by:
            option_change_sort(inter);
            inter->option_window.state = nvtop_option_state_hidden;
            alloc_process_with_option(inter, false);
            break;
          case nvtop_option_state_hidden:
          default:
            break;
        }
        break;
      case 27:
        alloc_process_with_option(inter, false);
        inter->option_window.state = nvtop_option_state_hidden;
        break;
      default:
        break;
  }
}
