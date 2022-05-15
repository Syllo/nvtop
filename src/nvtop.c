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

#include <getopt.h>
#include <ncurses.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <locale.h>

#include "nvtop/extract_gpuinfo.h"
#include "nvtop/interface.h"
#include "nvtop/interface_common.h"
#include "nvtop/interface_options.h"
#include "nvtop/time.h"
#include "nvtop/version.h"

static volatile sig_atomic_t signal_exit = 0;
static volatile sig_atomic_t signal_resize_win = 0;

static void exit_handler(int signum) {
  (void)signum;
  signal_exit = 1;
}

static void resize_handler(int signum) {
  (void)signum;
  signal_resize_win = 1;
}

static const char helpstring[] =
    "Available options:\n"
    "  -d --delay        : Select the refresh rate (1 == 0.1s)\n"
    "  -v --version      : Print the version and exit\n"
    "  -c --config-file  : Provide a custom config file location to load/save "
    "preferences\n"
    "  -s --gpu-select   : Colon separated list of GPU IDs to monitor\n"
    "  -i --gpu-ignore   : Colon separated list of GPU IDs to ignore\n"
    "  -p --no-plot      : Disable bar plot\n"
    "  -r --reverse-abs  : Reverse abscissa: plot the recent data left and "
    "older on the right\n"
    "  -C --no-color     : No colors\n"
    "line information\n"
    "  -f --freedom-unit : Use fahrenheit\n"
    "  -E --encode-hide  : Set encode/decode auto hide time in seconds "
    "(default 30s, negative = always on screen)\n"
    "  -h --help         : Print help and exit\n";

static const char versionString[] = "nvtop version " NVTOP_VERSION_STRING;

static const struct option long_opts[] = {
    {.name = "delay", .has_arg = required_argument, .flag = NULL, .val = 'd'},
    {.name = "version", .has_arg = no_argument, .flag = NULL, .val = 'v'},
    {.name = "help", .has_arg = no_argument, .flag = NULL, .val = 'h'},
    {.name = "config-file",
     .has_arg = required_argument,
     .flag = NULL,
     .val = 'c'},
    {.name = "no-color", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "no-colour", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "freedom-unit", .has_arg = no_argument, .flag = NULL, .val = 'f'},
    {.name = "gpu-select",
     .has_arg = required_argument,
     .flag = NULL,
     .val = 's'},
    {.name = "gpu-ignore",
     .has_arg = required_argument,
     .flag = NULL,
     .val = 'i'},
    {.name = "encode-hide",
     .has_arg = required_argument,
     .flag = NULL,
     .val = 'E'},
    {.name = "no-plot", .has_arg = no_argument, .flag = NULL, .val = 'p'},
    {.name = "reverse-abs", .has_arg = no_argument, .flag = NULL, .val = 'r'},
    {0, 0, 0, 0},
};

static const char opts[] = "hvd:s:i:c:CfE:pr";

static size_t update_mask_value(const char *str, size_t entry_mask,
                                bool addTo) {
  char *saveptr;
  char *option_copy = malloc((strlen(str) + 1) * sizeof(*option_copy));
  strcpy(option_copy, str);
  char *gpu_num = strtok_r(option_copy, ":", &saveptr);
  while (gpu_num != NULL) {
    char *endptr;
    unsigned num_used = strtoul(gpu_num, &endptr, 0);
    if (endptr == gpu_num) {
      fprintf(stderr, "Use GPU IDs (unsigned integer) to select GPU with "
                      "option 's' or 'i'\n");
      exit(EXIT_FAILURE);
    }
    if (num_used >= CHAR_BIT * sizeof(entry_mask)) {
      fprintf(stderr,
              "Select GPU X with option 's' or 'i' where 0 <= X < %zu\n",
              CHAR_BIT * sizeof(entry_mask));
      exit(EXIT_FAILURE);
    }
    if (addTo)
      entry_mask |= 1 << num_used;
    else
      entry_mask &= ~(1 << num_used);
    gpu_num = strtok_r(NULL, ":", &saveptr);
  }
  free(option_copy);
  return entry_mask;
}

int main(int argc, char **argv) {
  (void)setlocale(LC_CTYPE, "");

  opterr = 0;
  bool update_interval_option_set = false;
  int update_interval_option;
  char *selectedGPU = NULL;
  char *ignoredGPU = NULL;
  bool no_color_option = false;
  bool use_fahrenheit_option = false;
  bool hide_plot_option = false;
  bool reverse_plot_direction_option = false;
  bool encode_decode_timer_option_set = false;
  double encode_decode_hide_time = -1.;
  char *custom_config_file_path = NULL;
  while (true) {
    int optchar = getopt_long(argc, argv, opts, long_opts, NULL);
    if (optchar == -1)
      break;
    switch (optchar) {
    case 'd': {
      char *endptr = NULL;
      long int delay_val = strtol(optarg, &endptr, 0);
      if (endptr == optarg) {
        fprintf(stderr, "Error: The delay must be a positive value "
                        "representing tenths of seconds\n");
        exit(EXIT_FAILURE);
      }
      if (delay_val < 0) {
        fprintf(stderr, "Error: A negative delay requires a time machine!\n");
        exit(EXIT_FAILURE);
      }
      update_interval_option_set = true;
      update_interval_option = (int)delay_val * 100u;
      if (update_interval_option > 99900)
        update_interval_option = 99900;
      if (update_interval_option < 100)
        update_interval_option = 100;
    } break;
    case 's':
      selectedGPU = optarg;
      break;
    case 'i':
      ignoredGPU = optarg;
      break;
    case 'v':
      printf("%s\n", versionString);
      exit(EXIT_SUCCESS);
    case 'h':
      printf("%s\n%s", versionString, helpstring);
      exit(EXIT_SUCCESS);
    case 'c':
      custom_config_file_path = optarg;
      break;
    case 'C':
      no_color_option = true;
      break;
    case 'f':
      use_fahrenheit_option = true;
      break;
    case 'E': {
      if (sscanf(optarg, "%lf", &encode_decode_hide_time) == EOF) {
        fprintf(stderr, "Invalid format for encode/decode hide time: %s\n",
                optarg);
        exit(EXIT_FAILURE);
      }
      encode_decode_timer_option_set = true;
    } break;
    case 'p':
      hide_plot_option = true;
      break;
    case 'r':
      reverse_plot_direction_option = true;
      break;
    case ':':
    case '?':
      switch (optopt) {
      case 'd':
        fprintf(stderr, "Error: The delay option takes a positive value "
                        "representing tenths of seconds\n");
        break;
      default:
        fprintf(stderr, "Unhandled error in getopt missing argument\n");
        exit(EXIT_FAILURE);
        break;
      }
      exit(EXIT_FAILURE);
    }
  }

  setenv("ESCDELAY", "10", 1);

  struct sigaction siga;
  siga.sa_flags = 0;
  sigemptyset(&siga.sa_mask);
  siga.sa_handler = exit_handler;

  if (sigaction(SIGINT, &siga, NULL) != 0) {
    perror("Impossible to set signal handler for SIGINT: ");
    exit(EXIT_FAILURE);
  }
  if (sigaction(SIGQUIT, &siga, NULL) != 0) {
    perror("Impossible to set signal handler for SIGQUIT: ");
    exit(EXIT_FAILURE);
  }
  siga.sa_handler = resize_handler;
  if (sigaction(SIGWINCH, &siga, NULL) != 0) {
    perror("Impossible to set signal handler for SIGWINCH: ");
    exit(EXIT_FAILURE);
  }

  ssize_t gpu_mask;
  if (selectedGPU != NULL) {
    gpu_mask = 0;
    gpu_mask = update_mask_value(selectedGPU, gpu_mask, true);
  } else {
    gpu_mask = UINT_MAX;
  }
  if (ignoredGPU != NULL) {
    gpu_mask = update_mask_value(ignoredGPU, gpu_mask, false);
  }

  unsigned devices_count = 0;
  LIST_HEAD(devices);
  if (!gpuinfo_init_info_extraction(gpu_mask, &devices_count, &devices))
    return EXIT_FAILURE;
  if (devices_count == 0) {
    fprintf(stdout, "No GPU to monitor.\n");
    return EXIT_SUCCESS;
  }

  nvtop_interface_option interface_options;
  alloc_interface_options_internals(custom_config_file_path, devices_count,
                                    &interface_options);
  load_interface_options_from_config_file(devices_count, &interface_options);
  for (unsigned i = 0; i < devices_count; ++i) {
    // Nothing specified in the file
    if (!plot_isset_draw_info(plot_information_count,
                              interface_options.device_information_drawn[i])) {
      interface_options.device_information_drawn[i] = plot_default_draw_info();
    } else {
      interface_options.device_information_drawn[i] =
          plot_remove_draw_info(plot_information_count,
                                interface_options.device_information_drawn[i]);
    }
  }
  if (!process_is_field_displayed(process_field_count,
                                  interface_options.process_fields_displayed)) {
    interface_options.process_fields_displayed =
        process_default_displayed_field();
  } else {
    interface_options.process_fields_displayed =
        process_remove_field_to_display(
            process_field_count, interface_options.process_fields_displayed);
  }
  if (no_color_option)
    interface_options.use_color = false;
  if (hide_plot_option) {
    for (unsigned i = 0; i < devices_count; ++i) {
      interface_options.device_information_drawn[i] = 0;
    }
  }
  if (encode_decode_timer_option_set) {
    interface_options.encode_decode_hiding_timer = encode_decode_hide_time;
    if (interface_options.encode_decode_hiding_timer < 0.)
      interface_options.encode_decode_hiding_timer = 0.;
  }
  if (reverse_plot_direction_option)
    interface_options.plot_left_to_right = true;
  if (use_fahrenheit_option)
    interface_options.temperature_in_fahrenheit = true;
  if (update_interval_option_set)
    interface_options.update_interval = update_interval_option;

  gpuinfo_populate_static_infos(&devices);

  size_t biggest_name = 0;
  struct gpu_info *device;
  list_for_each_entry(device, &devices, list) {
    size_t device_name_size;
    if (IS_VALID(gpuinfo_device_name_valid, device->static_info.valid))
      device_name_size = strlen(device->static_info.device_name);
    else
      device_name_size = 4;
    if (device_name_size > biggest_name) {
      biggest_name = device_name_size;
    }
  }
  struct nvtop_interface *interface =
      initialize_curses(devices_count, biggest_name, interface_options);
  timeout(interface_update_interval(interface));

  double time_slept = interface_update_interval(interface);
  while (!signal_exit) {
    if (signal_resize_win) {
      update_window_size_to_terminal_size(interface);
      signal_resize_win = 0;
    }
    if (time_slept >= interface_update_interval(interface)) {
      gpuinfo_refresh_dynamic_info(&devices);
      if (!interface_freeze_processes(interface)) {
        gpuinfo_refresh_processes(&devices);
        gpuinfo_fix_dynamic_info_from_process_info(&devices);
      }
      save_current_data_to_ring(&devices, interface);
      timeout(interface_update_interval(interface));
      time_slept = 0.;
    } else {
      int next_sleep = interface_update_interval(interface) - (int)time_slept;
      timeout(next_sleep);
    }
    draw_gpu_info_ncurses(devices_count, &devices, interface);

    nvtop_time time_before_sleep, time_after_sleep;
    nvtop_get_current_time(&time_before_sleep);
    int input_char = getch();
    nvtop_get_current_time(&time_after_sleep);
    time_slept += nvtop_difftime(time_before_sleep, time_after_sleep) * 1000;
    switch (input_char) {
    case 27: // ESC
    {
      timeout(0);
      int in = getch();
      if (in == ERR) { // ESC alone
        if (is_escape_for_quit(interface))
          signal_exit = 1;
        else
          interface_key(27, interface);
      }
      // else ALT key
    } break;
    case KEY_F(10):
      if (is_escape_for_quit(interface))
        signal_exit = 1;
      break;
    case 'q':
      signal_exit = 1;
      break;
    case KEY_F(2):
    case KEY_F(9):
    case KEY_F(6):
    case KEY_F(12):
    case '+':
    case '-':
      interface_key(input_char, interface);
      break;
    case KEY_UP:
    case KEY_DOWN:
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_ENTER:
    case '\n':
      interface_key(input_char, interface);
      break;
    case ERR:
    default:
      break;
    }
  }

  clean_ncurses(interface);
  gpuinfo_shutdown_info_extraction(&devices);

  return EXIT_SUCCESS;
}
