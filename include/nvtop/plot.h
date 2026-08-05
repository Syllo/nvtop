/*
 *
 * Copyright (C) 2018 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef __PLOT_H_
#define __PLOT_H_

#include "nvtop/common.h"

#include <ncurses.h>
#include <stdbool.h>
#include <stddef.h>

#define PLOT_MAX_LEGEND_SIZE 35

// PCIe directions tracked for the bandwidth overlay: rx first, then tx.
#define PCIE_DIRECTION_COUNT 2

void nvtop_line_plot(WINDOW *win, size_t num_data, const double *data, unsigned num_plots, bool legend_left,
                     char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]);

// Theoretical unidirectional payload bandwidth of a PCIe link, in KB/s.
// Returns 0 when the generation or width is unknown.
unsigned nvtop_pcie_link_max_kbs(unsigned gen, unsigned width);

// Shade the background of an already drawn plot from the bottom of each column
// up to the given utilization. rx_fraction[] and tx_fraction[] hold one value in
// [0,1] per terminal column. Only cell attributes are touched, so the lines and
// legend drawn by nvtop_line_plot stay where they are.
void nvtop_bandwidth_overlay(WINDOW *win, size_t num_data, const double *rx_fraction, const double *tx_fraction,
                             short rx_color, short tx_color);

void draw_rectangle(WINDOW *win, unsigned startX, unsigned startY, unsigned sizeX, unsigned sizeY);

#endif // __PLOT_H_
