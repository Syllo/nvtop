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

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <ncurses.h>

#include <locale.h>

#include "interface.h"

#define STOP_SIGNAL 0x1
#define RESIZE_SIGNAL 0x2

static volatile unsigned char signal_bits = 0;

static void exit_handler(int signum) {
  (void) signum;
  signal_bits |= STOP_SIGNAL;
}

static void resize_handler(int signum) {
  (void) signum;
  signal_bits |= RESIZE_SIGNAL;
}

int main (int argc, char **argv) {
  (void) argc; (void) argv;
  (void) setlocale(LC_CTYPE, "");

  int refresh_interval = 1000;
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
    perror("Impossible to set signal handler for SIGQUIT: ");
    exit(EXIT_FAILURE);
  }

  init_gpu_info_extraction();
  size_t num_devices;
  struct device_info *dev_infos;
  num_devices = initialize_device_info(&dev_infos);
  struct nvtop_interface *interface = initialize_curses();
  timeout(refresh_interval);

  while (!(signal_bits & STOP_SIGNAL)) {
    update_device_infos(num_devices, dev_infos);
    if (signal_bits & RESIZE_SIGNAL) {
      update_window_size_to_terminal_size(interface);
      signal_bits &= ~RESIZE_SIGNAL;
    }
    draw_gpu_info_ncurses(num_devices, dev_infos, interface);

    int input_char = getch();
    switch (input_char) {
      case 27:
        {
          timeout(0);
          int in = getch();
          timeout(refresh_interval);
          if (in == ERR) {
            signal_bits |= STOP_SIGNAL;
          } else { // ALT key

          }
        }
        break;
      case 'q':
        signal_bits |= STOP_SIGNAL;
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
