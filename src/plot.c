/*
 *
 * Copyright (C) 2019-2021 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/plot.h"
#include "nvtop/common.h"

#include <assert.h>
#include <ncurses.h>
#include <stdbool.h>
#include <string.h>
#include <tgmath.h>

static inline int data_level(double rows, double data, double increment) {
  return (int)(rows - round(data / increment));
}

void nvtop_line_plot(WINDOW *win, size_t num_data, const double *data, unsigned num_lines, bool legend_left,
                     char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {
  if (num_data == 0)
    return;
  int rows, cols;
  getmaxyx(win, rows, cols);
  rows -= 1;
  double increment = 100. / (double)(rows);

  assert(num_lines <= MAX_LINES_PER_PLOT && "Cannot plot more than " EXPAND_AND_QUOTE(MAX_LINES_PER_PLOT) " lines");
  static const short plot_line_colors[MAX_LINES_PER_PLOT] = {7, 8, 9, 10};
  unsigned lvl_before[MAX_LINES_PER_PLOT];
  for (size_t k = 0; k < num_lines; ++k)
    lvl_before[k] = data_level(rows, data[k], increment);

  for (size_t i = 0; i < num_data || i < (size_t)cols; i += num_lines) {
    for (unsigned k = 0; k < num_lines; ++k) {
      unsigned lvl_now_k = data_level(rows, data[i + k], increment);
      wcolor_set(win, plot_line_colors[k], NULL);
      // Three cases: has increased, has decreased and remained level
      if (lvl_before[k] < lvl_now_k || lvl_before[k] > lvl_now_k) {
        // Case 1 and 2: has increased/decreased

        // An increase goes down on the plot because (0,0) is top left
        bool drawing_down = lvl_before[k] < lvl_now_k;
        unsigned bottom = drawing_down ? lvl_before[k] : lvl_now_k;
        unsigned top = drawing_down ? lvl_now_k : lvl_before[k];

        // Draw the vertical line corners
        mvwaddch(win, bottom, i + k, drawing_down ? ACS_URCORNER : ACS_ULCORNER);
        mvwaddch(win, top, i + k, drawing_down ? ACS_LLCORNER : ACS_LRCORNER);
        // Draw the vertical line between the corners
        if (top - bottom > 1) {
          mvwvline(win, bottom + 1, i + k, 0, top - bottom - 1);
        }

        // Draw the continuation of the other metrics
        for (unsigned j = 0; j < num_lines; ++j) {
          if (j != k) {
            if (lvl_before[j] == top)
              // The continuation is at the same level as the bottom corner
              mvwaddch(win, top, i + k, ACS_BTEE);
            else if (lvl_before[j] == bottom)
              // The continuation is at the same level as the top corner
              mvwaddch(win, bottom, i + k, ACS_TTEE);
            else if (lvl_before[j] > bottom && lvl_before[j] < top)
              // The continuation lies on the vertical line
              mvwaddch(win, lvl_before[j], i + k, ACS_PLUS);
            else {
              // The continuation lies outside the update interval so keep the
              // color
              wcolor_set(win, plot_line_colors[j], NULL);
              mvwaddch(win, lvl_before[j], i + k, ACS_HLINE);
              wcolor_set(win, plot_line_colors[k], NULL);
            }
          }
        }
      } else {
        // Case 3: stayed level
        mvwhline(win, lvl_now_k, i + k, 0, 1);
        for (unsigned j = 0; j < num_lines; ++j) {
          if (j != k) {
            if (lvl_before[j] != lvl_now_k) {
              // Add the continuation of other metric lines
              wcolor_set(win, plot_line_colors[j], NULL);
              mvwaddch(win, lvl_before[j], i + k, ACS_HLINE);
              wcolor_set(win, plot_line_colors[k], NULL);
            }
          }
        }
      }
      lvl_before[k] = lvl_now_k;
    }
  }
  int plot_y_position = 0;
  for (unsigned i = 0; i < num_lines && plot_y_position < rows; ++i) {
    wcolor_set(win, plot_line_colors[i], NULL);
    if (legend_left) {
      mvwprintw(win, plot_y_position, 0, "%.*s", cols, legend[i]);
    } else {
      size_t length = strlen(legend[i]);
      if (length <= (size_t)cols) {
        mvwprintw(win, plot_y_position, cols - length, "%s", legend[i]);
      } else {
        mvwprintw(win, plot_y_position, 0, "%.*s", (int)(length - cols), legend[i]);
      }
    }
    plot_y_position++;
  }
}

// Per-lane unidirectional payload bandwidth in KB/s, indexed by PCIe generation.
// Gen1/2 are 8b/10b encoded, Gen3 to Gen5 are 128b/130b, Gen6 is PAM4 with FLIT
// mode. These are the raw link rates minus encoding overhead; real payload
// throughput also loses a few percent to TLP headers.
static const unsigned pcie_lane_kbs_per_gen[] = {
    0,       // unknown
    250000,  // Gen1  2.5 GT/s
    500000,  // Gen2  5 GT/s
    984615,  // Gen3  8 GT/s
    1969231, // Gen4  16 GT/s
    3938461, // Gen5  32 GT/s
    7563000, // Gen6  64 GT/s
};

unsigned nvtop_pcie_link_max_kbs(unsigned gen, unsigned width) {
  static const unsigned num_gens = sizeof(pcie_lane_kbs_per_gen) / sizeof(*pcie_lane_kbs_per_gen);
  if (gen == 0 || width == 0)
    return 0;
  if (gen >= num_gens)
    gen = num_gens - 1;
  return pcie_lane_kbs_per_gen[gen] * width;
}

// Height in cells of a bar covering the given fraction of a plot_rows tall plot.
// Traffic worth less than half a cell shades nothing, so an idle link reads as
// idle instead of as a permanent one row floor.
static unsigned bar_cells(double fraction, int plot_rows) {
  if (!(fraction > 0.))
    return 0;
  if (fraction > 1.)
    fraction = 1.;
  double cells = round(fraction * (double)plot_rows);
  return (unsigned)cells;
}

void nvtop_bandwidth_overlay(WINDOW *win, size_t num_data, const double *rx_fraction, const double *tx_fraction,
                             short rx_color, short tx_color) {
  int rows, cols;
  getmaxyx(win, rows, cols);
  // nvtop_line_plot places 100% at row 0 and 0% at row rows-1, using the full
  // window height. A bar covering the whole height (plot_rows == rows) is
  // needed to reach row 0 at 100%, matching that gridline.
  int plot_rows = rows;
  if (plot_rows < 1)
    return;

  for (size_t i = 0; i < num_data && i < (size_t)cols; ++i) {
    const double fraction[PCIE_DIRECTION_COUNT] = {rx_fraction[i], tx_fraction[i]};
    const short color[PCIE_DIRECTION_COUNT] = {rx_color, tx_color};
    // Both directions rise from the bottom of the plot, so paint the taller one
    // first: the shorter one then reads as a band inside it and neither height
    // is lost.
    unsigned tallest = fraction[0] >= fraction[1] ? 0 : 1;
    for (unsigned o = 0; o < PCIE_DIRECTION_COUNT; ++o) {
      unsigned which = o == 0 ? tallest : PCIE_DIRECTION_COUNT - 1 - tallest;
      unsigned cells = bar_cells(fraction[which], plot_rows);
      for (unsigned r = 0; r < cells; ++r) {
        int row = rows - 1 - (int)r;
        // Reverse video turns the cell background into the pair's color while
        // leaving whatever the line plot drew there legible on top of it. The
        // cell's own attributes have to be carried over: dropping A_ALTCHARSET
        // would turn the line drawing glyphs back into the ASCII they map to.
        chtype existing = mvwinch(win, row, i) & A_ATTRIBUTES & ~A_COLOR;
        mvwchgat(win, row, i, 1, existing | A_REVERSE, color[which], NULL);
      }
    }
  }
}

void draw_rectangle(WINDOW *win, unsigned startX, unsigned startY, unsigned sizeX, unsigned sizeY) {
  mvwhline(win, startY, startX + 1, 0, sizeX - 2);
  mvwhline(win, startY + sizeY - 1, startX + 1, 0, sizeX - 2);

  mvwvline(win, startY + 1, startX, 0, sizeY - 2);
  mvwvline(win, startY + 1, startX + sizeX - 1, 0, sizeY - 2);

  mvwaddch(win, startY, startX, ACS_ULCORNER);
  mvwaddch(win, startY, startX + sizeX - 1, ACS_URCORNER);
  mvwaddch(win, startY + sizeY - 1, startX, ACS_LLCORNER);
  mvwaddch(win, startY + sizeY - 1, startX + sizeX - 1, ACS_LRCORNER);
}
