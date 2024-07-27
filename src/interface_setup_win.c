/*
 *
 * Copyright (C) 2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/interface_setup_win.h"
#include "nvtop/interface.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/interface_options.h"
#include "nvtop/interface_ring_buffer.h"

#include <ncurses.h>

static char *setup_window_category_names[setup_window_selection_count] = {"General", "Devices", "Chart", "Processes",
                                                                          "GPU Select"};

// All the windows used to display the setup
enum setup_window_type {
  setup_window_type_setup,
  setup_window_type_single,
  setup_window_type_split_left,
  setup_window_type_split_right,
  setup_window_type_count
};

// General Options

enum setup_general_options {
  setup_general_color,
  setup_general_show_startup_support_messages,
  setup_general_update_interval,
  setup_general_options_count
};

static const char *setup_general_option_description[setup_general_options_count] = {
    "Disable color (requires save and restart)", "Show support messages on startup", "Update interval (seconds)"};

// Header Options

enum setup_header_options {
  setup_header_toggle_fahrenheit,
  setup_header_enc_dec_timer,
  setup_header_gpu_info_bar,
  setup_header_options_count
};

static const char *setup_header_option_descriptions[setup_header_options_count] = {
    "Temperature in fahrenheit", "Keep displaying Encoder/Decoder rate (after reaching an idle state)",
    "Display extra GPU info bar"};

// Chart Options

enum setup_chart_options {
  setup_chart_reverse,
  setup_chart_all_gpu,
  setup_chart_start_gpu_list,
  setup_chart_options_count
};

static const char *setup_chart_options_descriptions[setup_chart_options_count] = {
    "Reverse plot direction", "Displayed all GPUs", "Displayed GPU"};

static const char *setup_chart_gpu_value_descriptions[plot_information_count] = {
    "GPU utilization rate", "GPU memory utilization rate",   "GPU encoder rate", "GPU decoder rate",
    "GPU temperature",      "Power draw rate (current/max)", "Fan speed",        "GPU clock rate",
    "GPU memory clock rate"};

// Process List Options

enum setup_proc_list_options {
  setup_proc_list_hide_process_list,
  setup_proc_list_hide_nvtop_process,
  setup_proc_list_sort_ascending,
  setup_proc_list_sort_by,
  setup_proc_list_display,
  setup_proc_list_options_count
};

static const char *setup_proc_list_option_description[setup_proc_list_options_count] = {
    "Don't display the process list", "Hide nvtop in the process list", "Sort Ascending", "Sort by", "Field Displayed"};

static const char *setup_proc_list_value_descriptions[process_field_count] = {
    "Process Id",    "User name",        "Device Id", "Workload type",    "GPU usage", "Encoder usage",
    "Decoder usage", "GPU memory usage", "CPU usage", "CPU memory usage", "Command"};

static unsigned int sizeof_setup_windows[setup_window_type_count] = {[setup_window_type_setup] = 11,
                                                                     [setup_window_type_single] = 0,
                                                                     [setup_window_type_split_left] = 26,
                                                                     [setup_window_type_split_right] = 0};

// For toggle options
// Show * if on, - if partial and nothing if off
enum option_state {
  option_off,
  option_on,
  option_partially_active,
};

static char option_state_char(enum option_state state) {
  switch (state) {
  case option_on:
    return '*';
  case option_partially_active:
    return '-';
  case option_off:
    return ' ';
  default:
    return ' ';
  }
}

void alloc_setup_window(struct window_position *position, struct setup_window *setup_win) {
  setup_win->visible = false;
  setup_win->clean_space = newwin(position->sizeY, position->sizeX, position->posY, position->posX);

  sizeof_setup_windows[setup_window_type_single] = position->sizeX - sizeof_setup_windows[setup_window_type_setup] - 1;
  if (sizeof_setup_windows[setup_window_type_single] > position->sizeX)
    sizeof_setup_windows[setup_window_type_single] = 0;

  sizeof_setup_windows[setup_window_type_split_right] = position->sizeX -
                                                        sizeof_setup_windows[setup_window_type_setup] -
                                                        sizeof_setup_windows[setup_window_type_split_left] - 2;
  if (sizeof_setup_windows[setup_window_type_split_right] > position->sizeX)
    sizeof_setup_windows[setup_window_type_split_right] = 0;

  setup_win->setup =
      newwin(position->sizeY, sizeof_setup_windows[setup_window_type_setup], position->posY, position->posX);

  setup_win->single = newwin(position->sizeY, sizeof_setup_windows[setup_window_type_single], position->posY,
                             position->posX + sizeof_setup_windows[setup_window_type_setup] + 1);

  setup_win->split[0] = newwin(position->sizeY, sizeof_setup_windows[setup_window_type_split_left], position->posY,
                               position->posX + sizeof_setup_windows[setup_window_type_setup] + 1);

  setup_win->split[1] = newwin(position->sizeY, sizeof_setup_windows[setup_window_type_split_right], position->posY,
                               position->posX + sizeof_setup_windows[setup_window_type_setup] +
                                   sizeof_setup_windows[setup_window_type_split_left] + 2);
}

void free_setup_window(struct setup_window *setup_win) {
  delwin(setup_win->clean_space);
  delwin(setup_win->setup);
  delwin(setup_win->single);
  delwin(setup_win->split[0]);
  delwin(setup_win->split[1]);
}

void show_setup_window(struct nvtop_interface *interface) {
  interface->setup_win.visible = true;
  touchwin(interface->setup_win.clean_space);
  wnoutrefresh(interface->setup_win.clean_space);
  interface->setup_win.selected_section = setup_general_selected;
  interface->setup_win.indentation_level = 0;
  interface->setup_win.options_selected[0] = 0;
  interface->setup_win.options_selected[1] = 0;
}

void hide_setup_window(struct nvtop_interface *interface) { interface->setup_win.visible = false; }

static void draw_setup_window_setup(struct nvtop_interface *interface) {
  werase(interface->setup_win.setup);
  mvwprintw(interface->setup_win.setup, 0, 0, "Setup");
  mvwchgat(interface->setup_win.setup, 0, 0, sizeof_setup_windows[setup_window_type_setup], A_STANDOUT, green_color,
           NULL);
  for (enum setup_window_section category = setup_general_selected; category < setup_window_selection_count;
       ++category) {
    mvwprintw(interface->setup_win.setup, category + 1, 0, "%s", setup_window_category_names[category]);
    if (interface->setup_win.selected_section == category) {
      if (interface->setup_win.indentation_level == 0) {
        set_attribute_between(interface->setup_win.setup, category + 1, 0,
                              sizeof_setup_windows[setup_window_type_setup], A_STANDOUT, cyan_color);
      } else {
        mvwprintw(interface->setup_win.setup, category + 1, sizeof_setup_windows[setup_window_type_setup] - 1, ">");
        set_attribute_between(interface->setup_win.setup, category + 1, 0,
                              sizeof_setup_windows[setup_window_type_setup], A_BOLD, cyan_color);
      }
    }
  }
  wnoutrefresh(interface->setup_win.setup);
}

static void draw_setup_window_general(struct nvtop_interface *interface) {
  if (interface->setup_win.indentation_level > 1)
    interface->setup_win.indentation_level = 1;
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] >= setup_general_options_count)
    interface->setup_win.options_selected[0] = setup_general_options_count - 1;

  wattr_set(interface->setup_win.single, A_STANDOUT, green_color, NULL);
  mvwprintw(interface->setup_win.single, 0, 0, "General Options");
  wstandend(interface->setup_win.single);

  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(interface->setup_win.single, tmp, maxcols);
  getyx(interface->setup_win.single, tmp, cur_col);
  mvwchgat(interface->setup_win.single, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);

  enum option_state option_state = !interface->options.use_color;
  mvwprintw(interface->setup_win.single, setup_general_color + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_general_option_description[setup_general_color]);
  if (interface->setup_win.indentation_level == 1 && interface->setup_win.options_selected[0] == setup_general_color) {
    mvwchgat(interface->setup_win.single, setup_general_color + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }
  option_state = interface->options.show_startup_messages;
  mvwprintw(interface->setup_win.single, setup_general_show_startup_support_messages + 1, 0, "[%c] %s",
            option_state_char(option_state),
            setup_general_option_description[setup_general_show_startup_support_messages]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_general_show_startup_support_messages) {
    mvwchgat(interface->setup_win.single, setup_general_show_startup_support_messages + 1, 0, 3, A_STANDOUT, cyan_color,
             NULL);
  }

  int update_deciseconds = (interface->options.update_interval / 100) % 10;
  int update_seconds = interface->options.update_interval / 1000;
  mvwprintw(interface->setup_win.single, setup_general_update_interval + 1, 0, "[%2u.%u] %s", update_seconds,
            update_deciseconds, setup_general_option_description[setup_general_update_interval]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_general_update_interval) {
    mvwchgat(interface->setup_win.single, setup_general_update_interval + 1, 0, 6, A_STANDOUT, cyan_color, NULL);
  }
  wnoutrefresh(interface->setup_win.single);
}

static void draw_setup_window_header(struct nvtop_interface *interface) {
  if (interface->setup_win.indentation_level > 1)
    interface->setup_win.indentation_level = 1;
  if (interface->setup_win.options_selected[0] >= setup_header_options_count)
    interface->setup_win.options_selected[0] = setup_header_options_count - 1;

  WINDOW *options_win = interface->setup_win.single;

  wattr_set(options_win, A_STANDOUT, green_color, NULL);
  mvwprintw(options_win, 0, 0, "Devices Display Options");
  wstandend(options_win);

  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(options_win, tmp, maxcols);
  getyx(options_win, tmp, cur_col);
  mvwchgat(options_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);

  enum option_state option_state;

  // Fahrenheit Option
  option_state = interface->options.temperature_in_fahrenheit;
  mvwprintw(options_win, setup_header_toggle_fahrenheit + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_header_option_descriptions[setup_header_toggle_fahrenheit]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_header_toggle_fahrenheit) {
    mvwchgat(options_win, setup_header_toggle_fahrenheit + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }

  // Encode/Decode hiding timer
  if (interface->options.encode_decode_hiding_timer > 0) {
    mvwprintw(options_win, setup_header_enc_dec_timer + 1, 0, "[%3.0fsec] %s",
              interface->options.encode_decode_hiding_timer,
              setup_header_option_descriptions[setup_header_enc_dec_timer]);
  } else {
    mvwprintw(options_win, setup_header_enc_dec_timer + 1, 0, "[always] %s",
              setup_header_option_descriptions[setup_header_enc_dec_timer]);
  }
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_header_enc_dec_timer) {
    mvwchgat(options_win, setup_header_enc_dec_timer + 1, 0, 8, A_STANDOUT, cyan_color, NULL);
  }

  // Extra GPU info bar
  option_state = interface->options.has_gpu_info_bar;
  mvwprintw(options_win, setup_header_gpu_info_bar + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_header_option_descriptions[setup_header_gpu_info_bar]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_header_gpu_info_bar) {
    mvwchgat(options_win, setup_header_gpu_info_bar + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }
  wnoutrefresh(options_win);
}

static void draw_setup_window_chart(unsigned devices_count, struct list_head *devices,
                                    struct nvtop_interface *interface) {
  WINDOW *option_list_win;

  // Fix indices for this window
  if (interface->setup_win.options_selected[0] > devices_count + 1)
    interface->setup_win.options_selected[0] = devices_count + 1;
  if (interface->setup_win.options_selected[0] > 0) {
    if (interface->setup_win.options_selected[1] >= plot_information_count)
      interface->setup_win.options_selected[1] = plot_information_count - 1;
    option_list_win = interface->setup_win.split[0];
  } else {
    if (interface->setup_win.indentation_level > 1)
      interface->setup_win.indentation_level = 1;
    option_list_win = interface->setup_win.single;
  }
  werase(interface->setup_win.single);
  wnoutrefresh(interface->setup_win.single);
  touchwin(interface->setup_win.split[0]);
  touchwin(interface->setup_win.split[1]);

  wattr_set(option_list_win, A_STANDOUT, green_color, NULL);
  mvwprintw(option_list_win, 0, 0, "Chart Options");
  wstandend(option_list_win);

  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(option_list_win, tmp, maxcols);
  getyx(option_list_win, tmp, cur_col);
  mvwchgat(option_list_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);

  enum option_state option_state;

  // Reverse plot
  option_state = interface->options.plot_left_to_right;
  mvwprintw(option_list_win, setup_chart_reverse + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_chart_options_descriptions[setup_chart_reverse]);
  if (interface->setup_win.indentation_level == 1 && interface->setup_win.options_selected[0] == setup_chart_reverse) {
    mvwchgat(option_list_win, setup_chart_reverse + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }

  // Set for all GPUs at once
  if (interface->setup_win.options_selected[0] == setup_chart_all_gpu) {
    if (interface->setup_win.indentation_level == 1)
      wattr_set(option_list_win, A_STANDOUT, cyan_color, NULL);
    if (interface->setup_win.indentation_level == 2)
      wattr_set(option_list_win, A_BOLD, cyan_color, NULL);
  }
  mvwaddch(option_list_win, setup_chart_all_gpu + 1, 1, ACS_HLINE);
  waddch(option_list_win, '>');
  wstandend(option_list_win);
  wprintw(option_list_win, " %s", setup_chart_options_descriptions[setup_chart_all_gpu]);

  // GPUs as a list
  for (unsigned i = 0; i < devices_count; ++i) {
    if (interface->setup_win.options_selected[0] == setup_chart_start_gpu_list + i) {
      if (interface->setup_win.indentation_level == 1)
        wattr_set(option_list_win, A_STANDOUT, cyan_color, NULL);
      if (interface->setup_win.indentation_level == 2)
        wattr_set(option_list_win, A_BOLD, cyan_color, NULL);
    }
    mvwaddch(option_list_win, setup_chart_start_gpu_list + 1 + i, 1, ACS_HLINE);
    waddch(option_list_win, '>');
    wstandend(option_list_win);
    wprintw(option_list_win, " %s %u", setup_chart_options_descriptions[setup_chart_start_gpu_list], i);
  }
  wnoutrefresh(option_list_win);

  // Window of list of metric to display in chart (4 maximum)
  if (interface->setup_win.options_selected[0] >= setup_chart_all_gpu) {
    WINDOW *value_list_win = interface->setup_win.split[1];
    wattr_set(value_list_win, A_STANDOUT, green_color, NULL);
    mvwprintw(value_list_win, 0, 0, "Metric Displayed in Graph");
    getmaxyx(value_list_win, tmp, maxcols);
    unsigned selected_gpu = interface->setup_win.options_selected[0] - setup_chart_start_gpu_list;
    if (interface->setup_win.options_selected[0] == setup_chart_all_gpu) {
      wprintw(value_list_win, " (All GPUs)");
    } else {
      // Get the selected device
      struct gpu_info *device;
      unsigned index = 0;
      list_for_each_entry(device, devices, list) {
        if (index == selected_gpu)
          break;
        index++;
      }
      if (IS_VALID(gpuinfo_device_name_valid, device->static_info.valid)) {
        getyx(value_list_win, tmp, cur_col);
        wprintw(value_list_win, " (%.*s)", maxcols - cur_col - 3, device->static_info.device_name);
      } else
        wprintw(value_list_win, " (GPU %u)", selected_gpu);
    }
    wclrtoeol(value_list_win);
    getyx(value_list_win, tmp, cur_col);
    mvwchgat(value_list_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);
    wattr_set(value_list_win, A_NORMAL, magenta_color, NULL);
    mvwprintw(value_list_win, 1, 0, "Maximum of 4 metrics per GPU");
    wstandend(value_list_win);

    for (enum plot_information i = plot_gpu_rate; i < plot_information_count; ++i) {
      if (interface->setup_win.options_selected[0] == setup_chart_all_gpu) {
        plot_info_to_draw draw_union = 0, draw_intersection = 0xffff;
        for (unsigned j = 0; j < devices_count; ++j) {
          draw_union |= interface->options.gpu_specific_opts[j].to_draw;
          draw_intersection = draw_intersection & interface->options.gpu_specific_opts[j].to_draw;
        }
        if (plot_isset_draw_info(i, draw_intersection)) {
          option_state = option_on;
        } else {
          if (plot_isset_draw_info(i, draw_union))
            option_state = option_partially_active;
          else
            option_state = option_off;
        }
      } else {
        option_state = plot_isset_draw_info(i, interface->options.gpu_specific_opts[selected_gpu].to_draw);
      }
      mvwprintw(value_list_win, i + 2, 0, "[%c] %s", option_state_char(option_state),
                setup_chart_gpu_value_descriptions[i]);
      if (interface->setup_win.indentation_level == 2 && interface->setup_win.options_selected[1] == i) {
        mvwchgat(value_list_win, i + 2, 0, 3, A_STANDOUT, cyan_color, NULL);
      }
    }
    wnoutrefresh(value_list_win);
  }
}

static void draw_setup_window_proc_list(struct nvtop_interface *interface) {
  WINDOW *option_list_win;
  if (interface->setup_win.options_selected[0] >= setup_proc_list_options_count)
    interface->setup_win.options_selected[0] = setup_proc_list_options_count - 1;
  if (interface->setup_win.options_selected[0] < setup_proc_list_sort_by) {
    option_list_win = interface->setup_win.single;
    if (interface->setup_win.indentation_level > 1)
      interface->setup_win.indentation_level = 1;
  } else {
    option_list_win = interface->setup_win.split[0];
    if (interface->setup_win.options_selected[0] == setup_proc_list_sort_by) {
      unsigned fields_count = process_field_displayed_count(interface->options.process_fields_displayed);
      if (!fields_count) {
        if (interface->setup_win.indentation_level > 1)
          interface->setup_win.indentation_level = 1;
      } else {
        if (interface->setup_win.options_selected[1] >= fields_count)
          interface->setup_win.options_selected[1] = fields_count - 1;
      }
    }
    if (interface->setup_win.options_selected[0] == setup_proc_list_display) {
      if (interface->setup_win.options_selected[1] >= process_field_count)
        interface->setup_win.options_selected[1] = process_field_count - 1;
    }
  }

  werase(interface->setup_win.single);
  wnoutrefresh(interface->setup_win.single);
  touchwin(interface->setup_win.split[0]);
  touchwin(interface->setup_win.split[1]);

  wattr_set(option_list_win, A_STANDOUT, green_color, NULL);
  mvwprintw(option_list_win, 0, 0, "Process List Options");
  wstandend(option_list_win);
  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(option_list_win, tmp, maxcols);
  getyx(option_list_win, tmp, cur_col);
  mvwchgat(option_list_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);

  // Sort Ascending
  enum option_state option_state = interface->options.hide_processes_list;
  mvwprintw(option_list_win, setup_proc_list_hide_process_list + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_proc_list_option_description[setup_proc_list_hide_process_list]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_proc_list_hide_process_list) {
    mvwchgat(option_list_win, setup_proc_list_hide_process_list + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }
  option_state = interface->options.filter_nvtop_pid;
  mvwprintw(option_list_win, setup_proc_list_hide_nvtop_process + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_proc_list_option_description[setup_proc_list_hide_nvtop_process]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_proc_list_hide_nvtop_process) {
    mvwchgat(option_list_win, setup_proc_list_hide_nvtop_process + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }
  option_state = !interface->options.sort_descending_order;
  mvwprintw(option_list_win, setup_proc_list_sort_ascending + 1, 0, "[%c] %s", option_state_char(option_state),
            setup_proc_list_option_description[setup_proc_list_sort_ascending]);
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] == setup_proc_list_sort_ascending) {
    mvwchgat(option_list_win, setup_proc_list_sort_ascending + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }

  for (enum setup_proc_list_options i = setup_proc_list_sort_by; i < setup_proc_list_options_count; ++i) {
    if (interface->setup_win.options_selected[0] == i) {
      if (interface->setup_win.indentation_level == 1)
        wattr_set(option_list_win, A_STANDOUT, cyan_color, NULL);
      if (interface->setup_win.indentation_level == 2)
        wattr_set(option_list_win, A_BOLD, cyan_color, NULL);
    }
    mvwaddch(option_list_win, i + 1, 1, ACS_HLINE);
    waddch(option_list_win, '>');
    wstandend(option_list_win);
    wprintw(option_list_win, " %s", setup_proc_list_option_description[i]);
    wnoutrefresh(option_list_win);
  }

  if (interface->setup_win.options_selected[0] >= setup_proc_list_sort_by) {
    WINDOW *value_list_win = interface->setup_win.split[1];
    // Sort by
    if (interface->setup_win.options_selected[0] == setup_proc_list_sort_by) {
      wattr_set(value_list_win, A_STANDOUT, green_color, NULL);
      mvwprintw(value_list_win, 0, 0, "Processes are sorted by:");
      wstandend(value_list_win);
      wclrtoeol(value_list_win);
      getmaxyx(value_list_win, tmp, maxcols);
      getyx(value_list_win, tmp, cur_col);
      mvwchgat(value_list_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);
      unsigned index = 0;
      for (enum process_field field = process_pid; field < process_field_count; ++field) {
        if (process_is_field_displayed(field, interface->options.process_fields_displayed)) {
          option_state = interface->options.sort_processes_by == field;
          mvwprintw(value_list_win, index + 1, 0, "[%c] %s", option_state_char(option_state),
                    setup_proc_list_value_descriptions[field]);
          wclrtoeol(value_list_win);
          if (interface->setup_win.indentation_level == 2 && interface->setup_win.options_selected[1] == index) {
            mvwchgat(value_list_win, index + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
            wmove(value_list_win, field + 2, 0);
          }
          index++;
        }
      }
      if (!index) {
        // Nothing displayed
        wcolor_set(value_list_win, magenta_color, NULL);
        mvwprintw(value_list_win, 1, 0, "Nothing to sort: none of the process fields are displayed");
        wstandend(value_list_win);
      }
    }
    // Process field displayed
    if (interface->setup_win.options_selected[0] == setup_proc_list_display) {
      wattr_set(value_list_win, A_STANDOUT, green_color, NULL);
      mvwprintw(value_list_win, 0, 0, "Process Field Displayed:");
      wstandend(value_list_win);
      wclrtoeol(value_list_win);
      getmaxyx(value_list_win, tmp, maxcols);
      getyx(value_list_win, tmp, cur_col);
      mvwchgat(value_list_win, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);
      for (enum process_field field = process_pid; field < process_field_count; ++field) {
        option_state = process_is_field_displayed(field, interface->options.process_fields_displayed);
        mvwprintw(value_list_win, field + 1, 0, "[%c] %s", option_state_char(option_state),
                  setup_proc_list_value_descriptions[field]);
        wclrtoeol(value_list_win);
        if (interface->setup_win.indentation_level == 2 && interface->setup_win.options_selected[1] == field) {
          mvwchgat(value_list_win, field + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
          wmove(value_list_win, field + 2, 0);
        }
      }
    }
    wclrtobot(value_list_win);
    wnoutrefresh(value_list_win);
  }
}

static void draw_setup_window_gpu_select(struct nvtop_interface *interface) {
  if (interface->setup_win.indentation_level > 1)
    interface->setup_win.indentation_level = 1;
  if (interface->setup_win.indentation_level == 1 &&
      interface->setup_win.options_selected[0] >= interface->total_dev_count)
    interface->setup_win.options_selected[0] = interface->total_dev_count - 1;

  wattr_set(interface->setup_win.single, A_STANDOUT, green_color, NULL);
  mvwprintw(interface->setup_win.single, 0, 0, "Select Monitored GPUs");
  wstandend(interface->setup_win.single);
  unsigned int cur_col, maxcols, tmp;
  (void)tmp;
  getmaxyx(interface->setup_win.single, tmp, maxcols);
  getyx(interface->setup_win.single, tmp, cur_col);
  mvwchgat(interface->setup_win.single, 0, cur_col, maxcols - cur_col, A_STANDOUT, green_color, NULL);

  for (unsigned devId = 0; devId < interface->total_dev_count; ++devId) {
    mvwprintw(interface->setup_win.single, devId + 1, 0, "[%c] %s",
              option_state_char(!interface->options.gpu_specific_opts[devId].doNotMonitor),
              interface->options.gpu_specific_opts[devId].linkedGpu->static_info.device_name);
    if (interface->setup_win.indentation_level == 1 && interface->setup_win.options_selected[0] == devId)
      mvwchgat(interface->setup_win.single, devId + 1, 0, 3, A_STANDOUT, cyan_color, NULL);
  }

  wnoutrefresh(interface->setup_win.single);
}

static const char *setup_window_shortcuts[] = {"Enter", "ESC", "Arrow keys", "+/-", "F12"};

static const char *setup_window_shortcut_description[] = {"Toggle", "Exit", "Navigate Menu",
                                                          "Increment/Decrement Values", "Save Config"};

void draw_setup_window_shortcuts(struct nvtop_interface *interface) {
  WINDOW *window = interface->shortcut_window;

  wmove(window, 0, 0);
  for (size_t i = 0; i < ARRAY_SIZE(setup_window_shortcuts); ++i) {
    wprintw(window, "%s", setup_window_shortcuts[i]);
    wattr_set(window, A_STANDOUT, cyan_color, NULL);
    wprintw(window, "%s ", setup_window_shortcut_description[i]);
    wstandend(window);
  }
  wclrtoeol(window);
  unsigned int cur_col, tmp;
  (void)tmp;
  getyx(window, tmp, cur_col);
  mvwchgat(window, 0, cur_col, -1, A_STANDOUT, cyan_color, NULL);
  wnoutrefresh(window);
}

void draw_setup_window(unsigned devices_count, struct list_head *devices, struct nvtop_interface *interface) {
  draw_setup_window_setup(interface);
  switch (interface->setup_win.selected_section) {
  case setup_general_selected:
    draw_setup_window_general(interface);
    break;
  case setup_header_selected:
    draw_setup_window_header(interface);
    break;
  case setup_chart_selected:
    draw_setup_window_chart(devices_count, devices, interface);
    break;
  case setup_process_list_selected:
    draw_setup_window_proc_list(interface);
    break;
  case setup_monitored_gpu_list_selected:
    draw_setup_window_gpu_select(interface);
    break;
  default:
    break;
  }
}

void handle_setup_win_keypress(int keyId, struct nvtop_interface *interface) {
  if (interface->setup_win.visible) {
    switch (keyId) {

    case 'l':
    case KEY_RIGHT:
      if (interface->setup_win.indentation_level < 2)
        interface->setup_win.indentation_level++;
      break;

    case 'h':
    case KEY_LEFT:
      if (interface->setup_win.indentation_level > 0)
        interface->setup_win.indentation_level--;
      break;

    case 'k':
    case KEY_UP:
      if (interface->setup_win.indentation_level == 0) {
        if (interface->setup_win.selected_section != setup_general_selected) {
          interface->setup_win.selected_section--;
          interface->setup_win.options_selected[0] = 0;
          interface->setup_win.options_selected[1] = 0;
          werase(interface->setup_win.single);
          werase(interface->setup_win.split[0]);
          werase(interface->setup_win.split[1]);
          wnoutrefresh(interface->setup_win.single);
        }
      } else {
        if (interface->setup_win.indentation_level == 1)
          interface->setup_win.options_selected[1] = 0;
        if (interface->setup_win.options_selected[interface->setup_win.indentation_level - 1] != 0)
          interface->setup_win.options_selected[interface->setup_win.indentation_level - 1]--;
      }
      break;

    case 'j':
    case KEY_DOWN:
      if (interface->setup_win.indentation_level == 0) {
        if (interface->setup_win.selected_section + 1 != setup_window_selection_count) {

          interface->setup_win.selected_section++;
          interface->setup_win.options_selected[0] = 0;
          interface->setup_win.options_selected[1] = 0;
          werase(interface->setup_win.single);
          werase(interface->setup_win.split[0]);
          werase(interface->setup_win.split[1]);
          wnoutrefresh(interface->setup_win.single);
        }
      } else {
        if (interface->setup_win.indentation_level == 1)
          interface->setup_win.options_selected[1] = 0;
        interface->setup_win.options_selected[interface->setup_win.indentation_level - 1]++;
      }
      break;

    case '+':
      // General Options
      if (interface->setup_win.selected_section == setup_general_selected) {
        if (interface->setup_win.options_selected[0] == setup_general_update_interval) {
          if (interface->options.update_interval <= 99800)
            interface->options.update_interval += 100;
        }
      }
      // Header options
      if (interface->setup_win.selected_section == setup_header_selected) {
        if (interface->setup_win.indentation_level == 1) {
          if (interface->setup_win.options_selected[0] == setup_header_enc_dec_timer) {
            interface->options.encode_decode_hiding_timer += 5.;
          }
        }
      }
      break;
    case '-':
      // General Options
      if (interface->setup_win.selected_section == setup_general_selected) {
        if (interface->setup_win.options_selected[0] == setup_general_update_interval) {
          if (interface->options.update_interval >= 200)
            interface->options.update_interval -= 100;
        }
      }
      // Header options
      if (interface->setup_win.selected_section == setup_header_selected) {
        if (interface->setup_win.indentation_level == 1) {
          if (interface->setup_win.options_selected[0] == setup_header_enc_dec_timer) {
            interface->options.encode_decode_hiding_timer -= 5.;
            if (interface->options.encode_decode_hiding_timer < 0.) {
              interface->options.encode_decode_hiding_timer = 0.;
            }
          }
        }
      }
      break;
    case '\n':
    case KEY_ENTER:
      if (interface->setup_win.indentation_level == 0) {
        handle_setup_win_keypress(KEY_RIGHT, interface);
        return;
      }
      // General Options
      if (interface->setup_win.selected_section == setup_general_selected) {
        if (interface->setup_win.options_selected[0] == setup_general_color) {
          interface->options.use_color = !interface->options.use_color;
        }
        if (interface->setup_win.options_selected[0] == setup_general_show_startup_support_messages) {
          interface->options.show_startup_messages = !interface->options.show_startup_messages;
        }
        if (interface->setup_win.options_selected[0] == setup_general_update_interval) {
        }
      }
      // Header Options
      if (interface->setup_win.selected_section == setup_header_selected) {
        if (interface->setup_win.indentation_level == 1) {
          if (interface->setup_win.options_selected[0] == setup_header_toggle_fahrenheit) {
            interface->options.temperature_in_fahrenheit = !interface->options.temperature_in_fahrenheit;
          }
          if (interface->setup_win.options_selected[0] == setup_header_enc_dec_timer) {
            if (interface->options.encode_decode_hiding_timer > 0.) {
              interface->options.encode_decode_hiding_timer = 0.;
            } else {
              interface->options.encode_decode_hiding_timer = 30.;
            }
          }
          if (interface->setup_win.options_selected[0] == setup_header_gpu_info_bar) {
            interface->options.has_gpu_info_bar = !interface->options.has_gpu_info_bar;
          }
        }
      }
      // Chart Options
      if (interface->setup_win.selected_section == setup_chart_selected) {
        if (interface->setup_win.indentation_level == 1) {
          if (interface->setup_win.options_selected[0] == setup_chart_reverse) {
            interface->options.plot_left_to_right = !interface->options.plot_left_to_right;
          }
          if (interface->setup_win.options_selected[0] >= setup_chart_all_gpu) {
            handle_setup_win_keypress(KEY_RIGHT, interface);
          }
        } else if (interface->setup_win.indentation_level == 2) {
          if (interface->setup_win.options_selected[0] == setup_chart_all_gpu) {
            plot_info_to_draw draw_intersection = 0xffff;
            for (unsigned j = 0; j < interface->monitored_dev_count; ++j) {
              draw_intersection = draw_intersection & interface->options.gpu_specific_opts[j].to_draw;
            }
            if (plot_isset_draw_info(interface->setup_win.options_selected[1], draw_intersection)) {
              for (unsigned i = 0; i < interface->monitored_dev_count; ++i) {
                interface->options.gpu_specific_opts[i].to_draw = plot_remove_draw_info(
                    interface->setup_win.options_selected[1], interface->options.gpu_specific_opts[i].to_draw);
                interface_ring_buffer_empty(&interface->saved_data_ring, i);
              }
            } else {
              for (unsigned i = 0; i < interface->monitored_dev_count; ++i) {
                interface->options.gpu_specific_opts[i].to_draw = plot_add_draw_info(
                    interface->setup_win.options_selected[1], interface->options.gpu_specific_opts[i].to_draw);
                interface_ring_buffer_empty(&interface->saved_data_ring, i);
              }
            }
          }
          if (interface->setup_win.options_selected[0] > setup_chart_all_gpu) {
            unsigned selected_gpu = interface->setup_win.options_selected[0] - setup_chart_start_gpu_list;
            if (plot_isset_draw_info(interface->setup_win.options_selected[1],
                                     interface->options.gpu_specific_opts[selected_gpu].to_draw))
              interface->options.gpu_specific_opts[selected_gpu].to_draw = plot_remove_draw_info(
                  interface->setup_win.options_selected[1], interface->options.gpu_specific_opts[selected_gpu].to_draw);
            else
              interface->options.gpu_specific_opts[selected_gpu].to_draw = plot_add_draw_info(
                  interface->setup_win.options_selected[1], interface->options.gpu_specific_opts[selected_gpu].to_draw);
            interface_ring_buffer_empty(&interface->saved_data_ring, selected_gpu);
          }
        }
      }
      // Process List Options
      if (interface->setup_win.selected_section == setup_process_list_selected) {
        if (interface->setup_win.indentation_level == 1) {
          if (interface->setup_win.options_selected[0] == setup_proc_list_sort_ascending) {
            interface->options.sort_descending_order = !interface->options.sort_descending_order;
          } else if (interface->setup_win.options_selected[0] == setup_proc_list_hide_nvtop_process) {
            interface->options.filter_nvtop_pid = !interface->options.filter_nvtop_pid;
          } else if (interface->setup_win.options_selected[0] == setup_proc_list_hide_process_list) {
            interface->options.hide_processes_list = !interface->options.hide_processes_list;
          } else if (interface->setup_win.options_selected[0] == setup_proc_list_sort_by) {
            handle_setup_win_keypress(KEY_RIGHT, interface);
          }
        } else if (interface->setup_win.indentation_level == 2) {
          if (interface->setup_win.options_selected[0] == setup_proc_list_sort_by) {
            unsigned index = 0;
            for (enum process_field field = process_pid; field < process_field_count; ++field) {
              if (process_is_field_displayed(field, interface->options.process_fields_displayed)) {
                if (index == interface->setup_win.options_selected[1])
                  interface->options.sort_processes_by = field;
                index++;
              }
            }
          }
          if (interface->setup_win.options_selected[0] == setup_proc_list_display) {
            if (process_is_field_displayed(interface->setup_win.options_selected[1],
                                           interface->options.process_fields_displayed)) {
              interface->options.process_fields_displayed = process_remove_field_to_display(
                  interface->setup_win.options_selected[1], interface->options.process_fields_displayed);
            } else {
              interface->options.process_fields_displayed = process_add_field_to_display(
                  interface->setup_win.options_selected[1], interface->options.process_fields_displayed);
            }
            if (!process_is_field_displayed(interface->options.sort_processes_by,
                                            interface->options.process_fields_displayed)) {
              interface->options.sort_processes_by =
                  process_default_sort_by_from(interface->options.process_fields_displayed);
            }
          }
        }
      }
      if (interface->setup_win.selected_section == setup_monitored_gpu_list_selected) {
        if (interface->setup_win.indentation_level == 1) {
          interface->options.gpu_specific_opts[interface->setup_win.options_selected[0]].doNotMonitor =
              !interface->options.gpu_specific_opts[interface->setup_win.options_selected[0]].doNotMonitor;
          interface->options.has_monitored_set_changed = true;
        }
      }
      break;
    case KEY_F(2):
    case 27:
      interface->setup_win.visible = false;
      update_window_size_to_terminal_size(interface);
      break;
    case KEY_F(12):
      save_interface_options_to_config_file(interface->total_dev_count, &interface->options);
      break;
    default:
      break;
    }
  }
}
