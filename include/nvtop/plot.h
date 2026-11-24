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
#include <stdint.h>

#define PLOT_MAX_LEGEND_SIZE 35

// Per plot window state of the braille renderer.
//
// A braille cell has a single color. Like the legacy renderer, updates take
// one character column per metric, so every metric has columns of its own
// where it is drawn in its own color. The other metrics continue flat
// through these columns, in the cells the column's metric does not use.
struct nvtop_braille_plot {
  unsigned rows, cols;
  // Scratch: one dot mask per cell for every line.
  uint8_t *masks;
};

// Allocate the state for a plot window of rows x cols cells. Returns false on
// allocation failure.
bool nvtop_braille_plot_init(struct nvtop_braille_plot *plot, unsigned rows, unsigned cols);
void nvtop_braille_plot_free(struct nvtop_braille_plot *plot);

// Samples per line the braille renderer expects for a plot window cols
// characters wide showing num_lines metrics. Every update takes a group of
// num_lines columns, one per metric; the oldest group may only be partially
// visible. One more sample is needed than shown: the oldest shown samples are
// joined to it.
size_t nvtop_plot_braille_samples(unsigned cols, unsigned num_lines);

// braille is only used by the braille renderer and must have been initialized
// for the size of win; otherwise the legacy renderer is used.
void nvtop_line_plot(WINDOW *win, size_t num_samples, const double *data, unsigned num_lines,
                     enum nvtop_plot_style style, bool newest_left, struct nvtop_braille_plot *braille,
                     char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]);

// Whether the current build and locale can display Unicode braille patterns.
bool nvtop_plot_supports_braille(void);

void draw_rectangle(WINDOW *win, unsigned startX, unsigned startY, unsigned sizeX, unsigned sizeY);

#endif // __PLOT_H_
