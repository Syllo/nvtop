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

#include <getopt.h>
#include <ncurses.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <locale.h>

#include "nvtop/interface.h"
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
    "  -s --gpu-select   : Colon separated list of GPU IDs to monitor\n"
    "  -i --gpu-ignore   : Colon separated list of GPU IDs to ignore\n"
    "  -p --no-plot      : Disable bar plot\n"
    "  -C --no-color     : No colors\n"
    "  -N --no-cache     : Always query the system for user names and command "
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
    {.name = "no-color", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "no-colour", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "no-cache", .has_arg = no_argument, .flag = NULL, .val = 'N'},
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
    {0, 0, 0, 0},
};

static const char opts[] = "hvd:s:i:CNfE:p";

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
  int refresh_interval = 1000;
  char *selectedGPU = NULL;
  char *ignoredGPU = NULL;
  bool use_color_if_available = true;
  bool cache_pid_infos = true;
  bool use_fahrenheit = false;
  bool show_plot = true;
  double encode_decode_hide_time = 30.;
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
      refresh_interval = (int)delay_val * 100u;
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
    case 'C':
      use_color_if_available = false;
      break;
    case 'N':
      cache_pid_infos = false;
      break;
    case 'f':
      use_fahrenheit = true;
      break;
    case 'E': {
      if (sscanf(optarg, "%lf", &encode_decode_hide_time) == EOF) {
        fprintf(stderr, "Invalid format for encode/decode hide time: %s\n",
                optarg);
        exit(EXIT_FAILURE);
      }
    } break;
    case 'p':
      show_plot = false;
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

  size_t gpu_mask;
  if (selectedGPU != NULL) {
    gpu_mask = 0;
    gpu_mask = update_mask_value(selectedGPU, gpu_mask, true);
  } else {
    gpu_mask = UINT_MAX;
  }
  if (ignoredGPU != NULL) {
    gpu_mask = update_mask_value(ignoredGPU, gpu_mask, false);
  }

  if (!init_gpu_info_extraction())
    return EXIT_FAILURE;
  size_t num_devices;
  struct device_info *dev_infos;
  num_devices = initialize_device_info(&dev_infos, gpu_mask);
  if (num_devices == 0) {
    fprintf(stdout, "No GPU left to monitor.\n");
    free(dev_infos);
    return EXIT_SUCCESS;
  }
  size_t biggest_name = 0;
  for (size_t i = 0; i < num_devices; ++i) {
    size_t device_name_size = strlen(dev_infos->device_name);
    if (device_name_size > biggest_name) {
      biggest_name = device_name_size;
    }
  }
  struct nvtop_interface *interface = initialize_curses(
      num_devices, biggest_name, use_color_if_available, use_fahrenheit,
      show_plot, encode_decode_hide_time, refresh_interval);
  timeout(refresh_interval);

  double time_slept = refresh_interval;
  while (!signal_exit) {
    if (signal_resize_win) {
      update_window_size_to_terminal_size(interface);
      signal_resize_win = 0;
    }
    if (!cache_pid_infos)
      clean_pid_cache();
    if (time_slept >= refresh_interval) {
      update_device_infos(num_devices, dev_infos);
      timeout(refresh_interval);
      time_slept = 0.;
    } else {
      int next_sleep = (int)((refresh_interval - time_slept));
      timeout(next_sleep);
    }
    draw_gpu_info_ncurses(dev_infos, interface);

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
    case KEY_F(9):
    case KEY_F(6):
    case '+':
    case '-':
      interface_key(input_char, interface);
      break;
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_UP:
    case KEY_DOWN:
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
  clean_device_info(num_devices, dev_infos);
  shutdown_gpu_info_extraction();

  return EXIT_SUCCESS;
}
