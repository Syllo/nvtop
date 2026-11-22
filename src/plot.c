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
#include <locale.h>
#include <ncurses.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <wchar.h>

static const short plot_line_colors[MAX_LINES_PER_PLOT] = {7, 8, 9, 10};

static inline int data_level(double rows, double data, double increment) {
  return (int)(rows - round(data / increment));
}

static void draw_plot_legend(WINDOW *win, int rows, int cols, unsigned num_lines, bool legend_left,
                             char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {
  int plot_y_position = 0;
  for (unsigned i = 0; i < num_lines && plot_y_position < rows - 1; ++i) {
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

// Legacy renderer using ncurses line-drawing characters. Kept as a fallback
// for terminals or locales where Unicode braille patterns are unavailable.
static void nvtop_line_plot_line(WINDOW *win, size_t num_data, const double *data, unsigned num_lines) {
  int rows = getmaxy(win);
  rows -= 1;
  if (rows <= 0)
    return;
  double increment = 100. / (double)(rows);

  assert(num_lines <= MAX_LINES_PER_PLOT && "Cannot plot more than " EXPAND_AND_QUOTE(MAX_LINES_PER_PLOT) " lines");
  unsigned lvl_before[MAX_LINES_PER_PLOT];
  for (size_t k = 0; k < num_lines; ++k)
    lvl_before[k] = data_level(rows, data[k], increment);

  size_t num_samples = num_data / num_lines;
  for (size_t sample = 0; sample < num_samples; ++sample) {
    size_t i = sample * num_lines;
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
}

#if defined(NCURSES_WIDECHAR) && NCURSES_WIDECHAR

#define BRAILLE_BASE 0x2800u
#define BRAILLE_CELL_ROWS 4
#define BRAILLE_CELL_COLS 2

// Braille dots are numbered 1..8 and stored in bits that do not follow the
// on-screen 2x4 layout. This table maps a (row, column) position inside a
// character cell to the corresponding bit.
static const uint8_t braille_dot_bits[BRAILLE_CELL_ROWS][BRAILLE_CELL_COLS] = {
    {0x01, 0x08}, // row 0: dots 1 and 4
    {0x02, 0x10}, // row 1: dots 2 and 5
    {0x04, 0x20}, // row 2: dots 3 and 6
    {0x40, 0x80}, // row 3: dots 7 and 8
};

static bool terminal_supports_braille(void) {
  // Braille is only printable when the locale uses a multi-byte encoding in
  // which ncursesw can represent U+2800..U+28FF.
  return MB_CUR_MAX > 1 && wcwidth((wchar_t)(BRAILLE_BASE + 0xFF)) > 0;
}

// Plot values are percentages in the [0, 100] range.
static unsigned plot_value_to_dot_row(double value, unsigned dot_rows) {
  if (value < 0.)
    value = 0.;
  if (value > 100.)
    value = 100.;
  double row = (100. - value) * (double)(dot_rows - 1) / 100.;
  return (unsigned)llround(row);
}

// Spread the samples over the full width of the graph so each plot line uses
// every column available in the window.
static unsigned sample_to_dot_col(size_t sample, size_t num_samples, unsigned dot_cols) {
  if (num_samples <= 1)
    return 0;
  double col = (double)sample * (double)(dot_cols - 1) / (double)(num_samples - 1);
  return (unsigned)llround(col);
}

static inline void braille_set_dot(uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x, int y) {
  if (x < 0 || y < 0 || x >= (int)(cols * BRAILLE_CELL_COLS) || y >= (int)(rows * BRAILLE_CELL_ROWS))
    return;
  unsigned cell_col = (unsigned)x / BRAILLE_CELL_COLS;
  unsigned cell_row = (unsigned)y / BRAILLE_CELL_ROWS;
  size_t cell = (size_t)cell_row * cols + cell_col;
  masks[(size_t)line * rows * cols + cell] |=
      braille_dot_bits[(unsigned)y % BRAILLE_CELL_ROWS][(unsigned)x % BRAILLE_CELL_COLS];
}

// Bresenham line drawing in dot space. Connecting consecutive samples produces
// a continuous curve instead of a chain of horizontal/vertical steps.
static void braille_draw_segment(uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x0, int y0, int x1,
                                 int y1) {
  int dx = abs(x1 - x0);
  int sx = x0 < x1 ? 1 : -1;
  int dy = -abs(y1 - y0);
  int sy = y0 < y1 ? 1 : -1;
  int err = dx + dy;
  for (;;) {
    braille_set_dot(masks, rows, cols, line, x0, y0);
    if (x0 == x1 && y0 == y1)
      break;
    int e2 = 2 * err;
    if (e2 >= dy) {
      err += dy;
      x0 += sx;
    }
    if (e2 <= dx) {
      err += dx;
      y0 += sy;
    }
  }
}

static unsigned bit_count8(uint8_t value) {
  unsigned count = 0;
  while (value) {
    count += value & 1u;
    value >>= 1;
  }
  return count;
}

// Draw the plot using Unicode braille patterns. Each character cell provides a
// 2x4 dot matrix, i.e. eight times more detail than a regular character.
static bool nvtop_line_plot_braille(WINDOW *win, size_t num_data, const double *data, unsigned num_lines, int rows,
                                    int cols) {
  size_t num_samples = num_data / num_lines;
  if (num_samples == 0 || rows <= 0 || cols <= 0)
    return true;

  size_t cell_count = (size_t)rows * (size_t)cols;
  uint8_t *masks = calloc((size_t)num_lines * cell_count, sizeof(*masks));
  if (!masks)
    return false;

  unsigned dot_cols = (unsigned)cols * BRAILLE_CELL_COLS;
  unsigned dot_rows = (unsigned)rows * BRAILLE_CELL_ROWS;

  for (unsigned line = 0; line < num_lines; ++line) {
    int prev_x = 0, prev_y = 0;
    for (size_t sample = 0; sample < num_samples; ++sample) {
      double value = data[sample * num_lines + line];
      int x = (int)sample_to_dot_col(sample, num_samples, dot_cols);
      int y = (int)plot_value_to_dot_row(value, dot_rows);
      if (sample == 0)
        braille_set_dot(masks, (unsigned)rows, (unsigned)cols, line, x, y);
      else
        braille_draw_segment(masks, (unsigned)rows, (unsigned)cols, line, prev_x, prev_y, x, y);
      prev_x = x;
      prev_y = y;
    }
  }

  for (unsigned row = 0; row < (unsigned)rows; ++row) {
    for (unsigned col = 0; col < (unsigned)cols; ++col) {
      size_t cell = (size_t)row * cols + col;
      uint8_t mask = 0;
      unsigned owner = 0, owner_dots = 0;
      for (unsigned line = 0; line < num_lines; ++line) {
        uint8_t line_mask = masks[(size_t)line * cell_count + cell];
        if (!line_mask)
          continue;
        mask |= line_mask;
        unsigned dots = bit_count8(line_mask);
        if (dots > owner_dots) {
          owner_dots = dots;
          owner = line;
        }
      }
      if (mask) {
        wchar_t glyph[2] = {(wchar_t)(BRAILLE_BASE + mask), L'\0'};
        wcolor_set(win, plot_line_colors[owner], NULL);
        mvwaddnwstr(win, row, col, glyph, 1);
      }
    }
  }

  free(masks);
  return true;
}

#endif // NCURSES_WIDECHAR

void nvtop_line_plot(WINDOW *win, size_t num_data, const double *data, unsigned num_lines, enum nvtop_plot_style style,
                     bool legend_left, char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {
  if (num_data == 0 || num_lines == 0)
    return;
  int rows, cols;
  getmaxyx(win, rows, cols);
  if (rows <= 0 || cols <= 0)
    return;

  bool plotted = false;
#if defined(NCURSES_WIDECHAR) && NCURSES_WIDECHAR
  if (style == nvtop_plot_style_braille && terminal_supports_braille()) {
    plotted = nvtop_line_plot_braille(win, num_data, data, num_lines, rows, cols);
  }
#endif
  if (!plotted) {
    nvtop_line_plot_line(win, num_data, data, num_lines);
  }

  draw_plot_legend(win, rows, cols, num_lines, legend_left, legend);
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
