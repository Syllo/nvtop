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
  unsigned int pid;
  char process_name[64];
  char user_name[64];
  unsigned long long used_memory;
  double mem_percentage;
  unsigned int gpu_id;
  bool is_graphical;
};

struct process_window {
  unsigned int size_biggest_name;
  unsigned int size_processes_buffer;
  unsigned int offset;
  struct all_gpu_processes *all_process;
  WINDOW *process_win;
};

struct nvtop_interface {
  size_t num_devices;
  struct device_window *devices_win;
  struct process_window process;
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

enum process_field {
  process_pid = 0,
  process_user,
  process_memory,
  process_gpu_id,
  process_type,
};

static unsigned int sizeof_process_field[] = {
  [process_pid] = 5,
  [process_user] = 4,
  [process_memory] = 6,
  [process_gpu_id] = 3,
  [process_type] = 7,
};

static void alloc_device_window_data(
    unsigned int biggest_device_name_size,
    unsigned int row,
    unsigned int totalcol,
    struct device_window *dwin) {

  unsigned int size_name_win = 11 // strlen("  Device   []")
    + biggest_device_name_size;
  sizeof_device_field[device_name] = size_name_win;

  const unsigned int spacer = 1;

  // Assume at least 80 columns

  // Line 1 = Name | PCIe info

  dwin->name_win = newwin(1, sizeof_device_field[device_name],
      row,
      spacer);
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

static void free_device_window_data(struct device_window *dwin) {
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
  free(dwin->device_name);
}


static void initialize_interface(
    struct nvtop_interface *dwin,
    unsigned int num_devices,
    unsigned int biggest_device_name_size) {

  int rows, cols;
  getmaxyx(stdscr, rows, cols);

  dwin->devices_win = malloc(num_devices * sizeof(*dwin->devices_win));
  memset(dwin->devices_win, 0, num_devices * sizeof(*dwin->devices_win));
  dwin->num_devices = num_devices;
  for (unsigned int i = 0; i < num_devices; ++i) {
    alloc_device_window_data(biggest_device_name_size, i*4, cols, &dwin->devices_win[i]);
  }

  int remaining_rows = rows - num_devices * 4 - 1;
  if (remaining_rows > 0) {
    dwin->process.size_processes_buffer =
      remaining_rows > 50 ? remaining_rows : 50;
    dwin->process.process_win = newwin(remaining_rows, cols-2, num_devices*4, 1);
    dwin->process.all_process = malloc(dwin->process.size_processes_buffer *
        sizeof(*dwin->process.all_process));
  } else {
    memset(&dwin->process, 0, sizeof(dwin->process));
  }
  dwin->process.offset = 0;
}

static void clear_interface(
    struct nvtop_interface *dwin) {
  for (unsigned int i = 0; i < dwin->num_devices; ++i) {
    free_device_window_data(&dwin->devices_win[i]);
  }
  free(dwin->devices_win);
  free(dwin->process.all_process);
  delwin(dwin->process.process_win);
  memset(dwin, 0, sizeof(*dwin));
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
  init_pair(cyan_color,  COLOR_CYAN,  COLOR_BLACK);
  init_pair(red_color,   COLOR_RED,   COLOR_BLACK);
  init_pair(green_color, COLOR_GREEN, COLOR_BLACK);
  init_pair(yellow_color, COLOR_YELLOW, COLOR_BLACK);
  init_pair(blue_color, COLOR_BLUE, COLOR_BLACK);
  init_pair(magenta_color, COLOR_MAGENTA, COLOR_BLACK);
}

struct nvtop_interface* initialize_curses(void) {
  struct nvtop_interface *interface = malloc(sizeof(*interface));
  memset(interface, 0, sizeof(*interface));
  initscr();
  if (has_colors() == TRUE) {
    initialize_colors();
  }
  cbreak();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);
  refresh();
  return interface;
}

void clean_ncurses(struct nvtop_interface *interface) {
  endwin();
  clear_interface(interface);
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
  mvwprintw(win, 0, 0, "TEMP %3u°C", temp);
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
    unsigned int num_devices,
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

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
    else
      mvwprintw(dev->temperature, 0, 0, "TEMP N/A°C");

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
      memcpy(interface->process.all_process[offset+j].process_name,
          dev_info[i].compute_procs[j].process_name,
          sizeof(interface->process.all_process[offset+j].process_name));
      memcpy(interface->process.all_process[offset+j].user_name,
          dev_info[i].compute_procs[j].user_name,
          sizeof(interface->process.all_process[offset+j].user_name));
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
      memcpy(interface->process.all_process[offset+j].process_name,
          dev_info[i].graphic_procs[j].process_name,
          sizeof(interface->process.all_process[offset+j].process_name));
      memcpy(interface->process.all_process[offset+j].user_name,
          dev_info[i].graphic_procs[j].user_name,
          sizeof(interface->process.all_process[offset+j].user_name));
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

static int compare_pid(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->pid + p2->pid;
}

static int compare_username(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return -strncmp(p1->user_name, p2->user_name, sizeof(p1->user_name));
}

static int compare_process_name(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return -strncmp(p1->process_name, p2->process_name, sizeof(p1->user_name));
}

static int compare_mem_usage(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->used_memory + p2->used_memory;
}

static int compare_gpu(
    const void *pp1,
    const void *pp2) {
  const struct all_gpu_processes *p1 = (const struct all_gpu_processes*) pp1;
  const struct all_gpu_processes *p2 = (const struct all_gpu_processes*) pp2;
  return - p1->gpu_id + p2->gpu_id;
}

static void sort_process(
    struct all_gpu_processes *proc,
    unsigned int total_process,
    enum sort_gpu_process_by criterion) {
  int (*sort_fun)(const void*, const void*) = compare_mem_usage;
  switch (criterion) {
    case sort_pid:
      sort_fun = compare_pid;
      break;
    case sort_username:
      sort_fun = compare_username;
      break;
    case sort_process_name:
      sort_fun = compare_process_name;
      break;
    case sort_mem_usage:
      sort_fun = compare_mem_usage;
      break;
    case sort_by_gpu:
      sort_fun = compare_gpu;
      break;
    case sort_none:
      return;
  }
  qsort(proc, total_process, sizeof(*proc), sort_fun);
}

static void print_process_preamble(WINDOW *win) {
  // PID | Username | GPU id | GPU_TYPE | Mem usage | Process name
  wattron(win, COLOR_PAIR(green_color) | A_REVERSE);
  mvwprintw(win, 0, 0, "%*s %*s %*s %*s %*s %s",
      sizeof_process_field[process_pid],
      "PID",
      sizeof_process_field[process_user],
      "USER",
      sizeof_process_field[process_gpu_id],
      "GPU",
      sizeof_process_field[process_type],
      "TYPE",
      sizeof_process_field[process_memory],
      "MEM",
      "Command");
  wchgat(win, -1, A_REVERSE, green_color, NULL);
  wattroff(win, COLOR_PAIR(green_color) | A_REVERSE);
  wnoutrefresh(win);
}

static void print_processes_on_screen(
    unsigned int num_process,
    struct all_gpu_processes *proc,
    WINDOW *win,
    unsigned int offset) {
  werase(win);
  print_process_preamble(win);

  unsigned int rows, cols;
  getmaxyx(win, rows, cols);
  (void) cols;


  char pid_str[sizeof_process_field[process_pid]+1];
  char guid_str[sizeof_process_field[process_gpu_id]+1];
  char memory[sizeof_process_field[process_memory]+1];
  for (unsigned int i = 0; i < rows-1 && i < num_process; ++i) {
    unsigned int write_at = i+1;
    size_t size = snprintf(pid_str, sizeof_process_field[process_pid]+1,
        "%u", proc[i+offset].pid);
    if (size == sizeof_process_field[process_pid]+1)
      pid_str[sizeof_process_field[process_pid]] = '\0';
    size = snprintf(guid_str, sizeof_process_field[process_gpu_id]+1,
        "%u", proc[i+offset].gpu_id);
    if (size == sizeof_process_field[process_gpu_id]+1)
      pid_str[sizeof_process_field[process_gpu_id]] = '\0';
    size = snprintf(memory, sizeof_process_field[process_memory]+1,
        "%.1f%%", proc[i+offset].mem_percentage);
    mvwprintw(win, write_at, 0, "%*s %*s %*s",
        sizeof_process_field[process_pid],
        pid_str,
        sizeof_process_field[process_user],
        proc[i+offset].user_name,
        sizeof_process_field[process_gpu_id],
        guid_str);
    if (proc[i+offset].is_graphical) {
      wattron(win, COLOR_PAIR(yellow_color));
      wprintw(win, " %*s ", sizeof_process_field[process_type], "Graphic");
      wattroff(win, COLOR_PAIR(yellow_color));
    } else {
      wattron(win, COLOR_PAIR(magenta_color));
      wprintw(win, " %*s ", sizeof_process_field[process_type], "Compute");
      wattroff(win, COLOR_PAIR(magenta_color));
    }
    wprintw(win, "%*s ",
        sizeof_process_field[process_memory],
        memory);
    wprintw(win, "%s", proc[i+offset].process_name);
  }
  wnoutrefresh(win);
}

static void draw_processes(
    unsigned int num_devices,
    struct device_info *dev_info,
    struct nvtop_interface *interface,
    enum sort_gpu_process_by sort_criterion) {

  if (interface->process.process_win == NULL)
    return;

  unsigned int total_processes = copy_processes_for_processing(num_devices,
      dev_info, interface);
  sort_process(interface->process.all_process, total_processes, sort_criterion);

  // Offset
  unsigned int offset = interface->process.offset;
  unsigned int rows, cols;
  getmaxyx(interface->process.process_win, rows, cols);
  rows -= 1;
  unsigned int borne_max = rows + offset;
  if (borne_max > total_processes) {
    if (total_processes > rows)
      offset = total_processes - rows;
    else
      offset = 0;
  }
  interface->process.offset = offset;
  sizeof_process_field[process_user] = interface->process.size_biggest_name;

  size_t process_name_size = cols;
  for (enum process_field i = process_pid; i <= process_type; ++i) {
    size_t size_field = sizeof_process_field[i] + 1;
    if (process_name_size > size_field) {
      process_name_size -= size_field;
    } else {
      process_name_size = 0;
      break;
    }
  }
  for (size_t i = 0; i < total_processes; ++i)
    if (strlen(interface->process.all_process[i+interface->process.offset].process_name) > process_name_size)
      interface->process.all_process[i+interface->process.offset].process_name[process_name_size] = '\0';

  print_processes_on_screen(total_processes, interface->process.all_process,
      interface->process.process_win, interface->process.offset);

}

void draw_gpu_info_ncurses(
    unsigned int num_devices,
    struct device_info *dev_info,
    struct nvtop_interface *interface) {

  if (interface->devices_win == NULL) {
    size_t biggest_name = 0;
    for (size_t i = 0; i < num_devices; ++i) {
      size_t device_name_size = strlen(dev_info->device_name);
      if (device_name_size > biggest_name) {
        biggest_name = device_name_size;
      }
    }
    initialize_interface(interface, num_devices, biggest_name);
  }

  draw_devices(num_devices, dev_info, interface);
  draw_processes(num_devices, dev_info, interface, sort_mem_usage);
  doupdate();
  refresh();

}

void update_window_size_to_terminal_size(struct nvtop_interface *inter) {
  clear_interface(inter);
  endwin();
  erase();
  refresh();
  refresh();
}
