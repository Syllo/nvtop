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
#include "nvtop/info_messages.h"
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

static const char helpstring[] = "Available options:\n"
                                 "  -d --delay        : Select the refresh rate (1 == 0.1s)\n"
                                 "  -v --version      : Print the version and exit\n"
                                 "  -c --config-file  : Provide a custom config file location to load/save "
                                 "preferences\n"
                                 "  -p --no-plot      : Disable bar plot\n"
                                 "  -P --no-processes : Disable process list\n"
                                 "  -r --reverse-abs  : Reverse abscissa: plot the recent data left and "
                                 "older on the right\n"
                                 "  -C --no-color     : No colors\n"
                                 "line information\n"
                                 "  -f --freedom-unit : Use fahrenheit\n"
                                 "  -i --gpu-info     : Show bar with additional GPU parametres\n"
                                 "  -E --encode-hide  : Set encode/decode auto hide time in seconds "
                                 "(default 30s, negative = always on screen)\n"
                                 "  -h --help         : Print help and exit\n";

static const char versionString[] = "nvtop version " NVTOP_VERSION_STRING;

static const struct option long_opts[] = {
    {.name = "delay", .has_arg = required_argument, .flag = NULL, .val = 'd'},
    {.name = "version", .has_arg = no_argument, .flag = NULL, .val = 'v'},
    {.name = "help", .has_arg = no_argument, .flag = NULL, .val = 'h'},
    {.name = "config-file", .has_arg = required_argument, .flag = NULL, .val = 'c'},
    {.name = "no-color", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "no-colour", .has_arg = no_argument, .flag = NULL, .val = 'C'},
    {.name = "freedom-unit", .has_arg = no_argument, .flag = NULL, .val = 'f'},
    {.name = "gpu-info", .has_arg = no_argument, .flag = NULL, .val = 'i'},
    {.name = "encode-hide", .has_arg = required_argument, .flag = NULL, .val = 'E'},
    {.name = "no-plot", .has_arg = no_argument, .flag = NULL, .val = 'p'},
    {.name = "no-processes", .has_arg = no_argument, .flag = NULL, .val = 'P'},
    {.name = "reverse-abs", .has_arg = no_argument, .flag = NULL, .val = 'r'},
    {0, 0, 0, 0},
};

static const char opts[] = "hvd:c:CfE:pPri";

int main(int argc, char **argv) {
  (void)setlocale(LC_CTYPE, "");

  opterr = 0;
  bool update_interval_option_set = false;
  int update_interval_option;
  bool no_color_option = false;
  bool use_fahrenheit_option = false;
  bool hide_plot_option = false;
  bool hide_processes_option = false;
  bool reverse_plot_direction_option = false;
  bool encode_decode_timer_option_set = false;
  bool show_gpu_info_bar = false;
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
    case 'i':
      show_gpu_info_bar = true;
      break;
    case 'E': {
      if (sscanf(optarg, "%lf", &encode_decode_hide_time) == EOF) {
        fprintf(stderr, "Invalid format for encode/decode hide time: %s\n", optarg);
        exit(EXIT_FAILURE);
      }
      encode_decode_timer_option_set = true;
    } break;
    case 'p':
      hide_plot_option = true;
      break;
    case 'P':
      hide_processes_option = true;
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

  unsigned allDevCount = 0;
  LIST_HEAD(monitoredGpus);
  LIST_HEAD(nonMonitoredGpus);
  if (!gpuinfo_init_info_extraction(&allDevCount, &monitoredGpus))
    return EXIT_FAILURE;
  if (allDevCount == 0) {
    fprintf(stdout, "No GPU to monitor.\n");
    return EXIT_SUCCESS;
  }

  unsigned numWarningMessages = 0;
  const char **warningMessages;
  get_info_messages(&monitoredGpus, &numWarningMessages, &warningMessages);

  nvtop_interface_option allDevicesOptions;
  alloc_interface_options_internals(custom_config_file_path, allDevCount, &monitoredGpus, &allDevicesOptions);
  load_interface_options_from_config_file(allDevCount, &allDevicesOptions);
  for (unsigned i = 0; i < allDevCount; ++i) {
    // Nothing specified in the file
    if (!plot_isset_draw_info(plot_information_count, allDevicesOptions.gpu_specific_opts[i].to_draw)) {
      allDevicesOptions.gpu_specific_opts[i].to_draw = plot_default_draw_info();
    } else {
      allDevicesOptions.gpu_specific_opts[i].to_draw =
          plot_remove_draw_info(plot_information_count, allDevicesOptions.gpu_specific_opts[i].to_draw);
    }
  }
  if (!process_is_field_displayed(process_field_count, allDevicesOptions.process_fields_displayed)) {
    allDevicesOptions.process_fields_displayed = process_default_displayed_field();
  } else {
    allDevicesOptions.process_fields_displayed =
        process_remove_field_to_display(process_field_count, allDevicesOptions.process_fields_displayed);
  }
  if (no_color_option)
    allDevicesOptions.use_color = false;
  if (hide_plot_option) {
    for (unsigned i = 0; i < allDevCount; ++i) {
      allDevicesOptions.gpu_specific_opts[i].to_draw = 0;
    }
  }
  allDevicesOptions.hide_processes_list = hide_processes_option;
  if (encode_decode_timer_option_set) {
    allDevicesOptions.encode_decode_hiding_timer = encode_decode_hide_time;
    if (allDevicesOptions.encode_decode_hiding_timer < 0.)
      allDevicesOptions.encode_decode_hiding_timer = 0.;
  }
  if (reverse_plot_direction_option)
    allDevicesOptions.plot_left_to_right = true;
  if (use_fahrenheit_option)
    allDevicesOptions.temperature_in_fahrenheit = true;
  if (update_interval_option_set)
    allDevicesOptions.update_interval = update_interval_option;
  allDevicesOptions.has_gpu_info_bar = allDevicesOptions.has_gpu_info_bar || show_gpu_info_bar;

  gpuinfo_populate_static_infos(&monitoredGpus);
  unsigned numMonitoredGpus =
      interface_check_and_fix_monitored_gpus(allDevCount, &monitoredGpus, &nonMonitoredGpus, &allDevicesOptions);

  if (allDevicesOptions.show_startup_messages) {
    bool dont_show_again = show_information_messages(numWarningMessages, warningMessages);
    if (dont_show_again) {
      allDevicesOptions.show_startup_messages = false;
      save_interface_options_to_config_file(allDevCount, &allDevicesOptions);
    }
  }

  struct nvtop_interface *interface =
      initialize_curses(allDevCount, numMonitoredGpus, interface_largest_gpu_name(&monitoredGpus), allDevicesOptions);
  timeout(interface_update_interval(interface));

  double time_slept = interface_update_interval(interface);
  while (!signal_exit) {
    if (signal_resize_win) {
      signal_resize_win = 0;
      update_window_size_to_terminal_size(interface);
    }
    interface_check_monitored_gpu_change(&interface, allDevCount, &numMonitoredGpus, &monitoredGpus, &nonMonitoredGpus);
    if (time_slept >= interface_update_interval(interface)) {
      gpuinfo_refresh_dynamic_info(&monitoredGpus);
      if (!interface_freeze_processes(interface)) {
        gpuinfo_refresh_processes(&monitoredGpus);
        gpuinfo_utilisation_rate(&monitoredGpus);
        gpuinfo_fix_dynamic_info_from_process_info(&monitoredGpus);
      }
      save_current_data_to_ring(&monitoredGpus, interface);
      timeout(interface_update_interval(interface));
      time_slept = 0.;
    } else {
      int next_sleep = interface_update_interval(interface) - (int)time_slept;
      timeout(next_sleep);
    }
    draw_gpu_info_ncurses(numMonitoredGpus, &monitoredGpus, interface);

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
    case 'k':
    case KEY_UP:
    case 'j':
    case KEY_DOWN:
    case 'h':
    case KEY_LEFT:
    case 'l':
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
  gpuinfo_shutdown_info_extraction(&monitoredGpus);

  return EXIT_SUCCESS;
}
