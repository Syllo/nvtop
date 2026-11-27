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
#include "nvtop/terminal.h"

#include <assert.h>
#include <ncurses.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <wchar.h>

static const short plot_line_colors[MAX_LINES_PER_PLOT] = {7, 8, 9, 10};

// Plot values are percentages (callers already scale every series to 0..100).
// Non-finite samples map to 0 and the rest is clamped, so both renderers place
// any sample the same way.
static inline double plot_clamp_percentage(double value) {
  if (!isfinite(value) || value < 0.)
    return 0.;
  if (value > 100.)
    return 100.;
  return value;
}

static inline int data_level(double rows, double data, double increment) {
  return (int)(rows - round(plot_clamp_percentage(data) / increment));
}

static void draw_plot_legend(WINDOW *win, int rows, int cols, unsigned num_lines, bool legend_left,
                             char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {
  for (unsigned i = 0; i < num_lines && (int)i < rows - 1; ++i) {
    size_t length = strlen(legend[i]);
    wcolor_set(win, plot_line_colors[i], NULL);
    if (legend_left)
      mvwprintw(win, (int)i, 0, "%.*s", cols, legend[i]);
    else if (length <= (size_t)cols)
      mvwprintw(win, (int)i, cols - (int)length, "%s", legend[i]);
    else
      mvwprintw(win, (int)i, 0, "%s", legend[i] + length - cols);
  }
}

// Legacy renderer using ncurses line-drawing characters. Kept as a fallback
// for terminals or locales where Unicode braille patterns are unavailable.
// Each update occupies num_lines consecutive columns, one per metric, so the
// graph advances by num_lines columns per update.
static void nvtop_line_plot_line(WINDOW *win, size_t num_samples, const double *data, unsigned num_lines,
                                 bool newest_left) {
  int rows = getmaxy(win);
  int cols = getmaxx(win);
  rows -= 1;
  if (rows <= 0 || cols <= 0)
    return;
  double increment = 100. / (double)(rows);

  assert(num_lines <= MAX_LINES_PER_PLOT && "Cannot plot more than " EXPAND_AND_QUOTE(MAX_LINES_PER_PLOT) " lines");
  int base_x = newest_left ? 0 : cols - (int)(num_samples * num_lines);
  // Only draw the samples with a column on screen: from the first one that is
  // at least partially visible, starting from the levels of the one before.
  size_t first = base_x < 0 ? (size_t)(-base_x) / num_lines : 0;
  unsigned lvl_before[MAX_LINES_PER_PLOT];
  for (size_t k = 0; k < num_lines; ++k)
    lvl_before[k] = data_level(rows, data[(first > 0 ? first - 1 : 0) * num_lines + k], increment);

  for (size_t sample = first; sample < num_samples && base_x + (int)(sample * num_lines) < cols; ++sample) {
    for (unsigned k = 0; k < num_lines; ++k) {
      int x = base_x + (int)(sample * num_lines + k);
      unsigned lvl_now_k = data_level(rows, data[sample * num_lines + k], increment);
      if (x < 0 || x >= cols) {
        // Clipped: the next visible column still starts from this level.
        lvl_before[k] = lvl_now_k;
        continue;
      }
      wcolor_set(win, plot_line_colors[k], NULL);
      // Three cases: has increased, has decreased and remained level
      if (lvl_before[k] < lvl_now_k || lvl_before[k] > lvl_now_k) {
        // Case 1 and 2: has increased/decreased

        // An increase goes down on the plot because (0,0) is top left
        bool drawing_down = lvl_before[k] < lvl_now_k;
        unsigned bottom = drawing_down ? lvl_before[k] : lvl_now_k;
        unsigned top = drawing_down ? lvl_now_k : lvl_before[k];

        // Draw the vertical line corners
        mvwaddch(win, bottom, x, drawing_down ? ACS_URCORNER : ACS_ULCORNER);
        mvwaddch(win, top, x, drawing_down ? ACS_LLCORNER : ACS_LRCORNER);
        // Draw the vertical line between the corners
        if (top - bottom > 1) {
          mvwvline(win, bottom + 1, x, 0, top - bottom - 1);
        }

        // Draw the continuation of the other metrics
        for (unsigned j = 0; j < num_lines; ++j) {
          if (j != k) {
            if (lvl_before[j] == top)
              // The continuation is at the same level as the bottom corner
              mvwaddch(win, top, x, ACS_BTEE);
            else if (lvl_before[j] == bottom)
              // The continuation is at the same level as the top corner
              mvwaddch(win, bottom, x, ACS_TTEE);
            else if (lvl_before[j] > bottom && lvl_before[j] < top)
              // The continuation lies on the vertical line
              mvwaddch(win, lvl_before[j], x, ACS_PLUS);
            else {
              // The continuation lies outside the update interval so keep the
              // color
              wcolor_set(win, plot_line_colors[j], NULL);
              mvwaddch(win, lvl_before[j], x, ACS_HLINE);
              wcolor_set(win, plot_line_colors[k], NULL);
            }
          }
        }
      } else {
        // Case 3: stayed level
        mvwhline(win, lvl_now_k, x, 0, 1);
        for (unsigned j = 0; j < num_lines; ++j) {
          if (j != k) {
            if (lvl_before[j] != lvl_now_k) {
              // Add the continuation of other metric lines
              wcolor_set(win, plot_line_colors[j], NULL);
              mvwaddch(win, lvl_before[j], x, ACS_HLINE);
              wcolor_set(win, plot_line_colors[k], NULL);
            }
          }
        }
      }
      lvl_before[k] = lvl_now_k;
    }
  }
}

#ifdef NVTOP_HAVE_WIDE_CURSES

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
  // Braille is only printable when the locale is UTF-8 and the wide character
  // is printable in that encoding, and visible when the terminal's font has
  // the glyphs.
  return MB_CUR_MAX > 1 && nvtop_locale_is_utf8() && wcwidth((wchar_t)(BRAILLE_BASE + 0xFF)) == 1 &&
         !nvtop_terminal_is_linux_console();
}

static unsigned plot_value_to_dot_row(double value, unsigned dot_rows) {
  double row = (100. - plot_clamp_percentage(value)) * (double)(dot_rows - 1) / 100.;
  return (unsigned)llround(row);
}

// Index in masks of the cell holding dot (x, y) of `line`, and the dot's bit.
// Returns false for dots outside the graph.
static inline bool braille_dot_position(unsigned rows, unsigned cols, unsigned line, int x, int y, size_t *index,
                                        uint8_t *bit) {
  if (x < 0 || y < 0 || x >= (int)(cols * BRAILLE_CELL_COLS) || y >= (int)(rows * BRAILLE_CELL_ROWS))
    return false;
  size_t cell = (size_t)((unsigned)y / BRAILLE_CELL_ROWS) * cols + (unsigned)x / BRAILLE_CELL_COLS;
  *index = (size_t)line * rows * cols + cell;
  *bit = braille_dot_bits[(unsigned)y % BRAILLE_CELL_ROWS][(unsigned)x % BRAILLE_CELL_COLS];
  return true;
}

static inline void braille_set_dot(uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x, int y) {
  size_t index;
  uint8_t bit;
  if (braille_dot_position(rows, cols, line, x, y, &index, &bit))
    masks[index] |= bit;
}

static inline void braille_set_vertical(uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x, int y0,
                                        int y1) {
  int step = y0 <= y1 ? 1 : -1;
  for (int y = y0;; y += step) {
    braille_set_dot(masks, rows, cols, line, x, y);
    if (y == y1)
      break;
  }
}

static inline bool braille_dot_is_set(const uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x, int y) {
  size_t index;
  uint8_t bit;
  return braille_dot_position(rows, cols, line, x, y, &index, &bit) && (masks[index] & bit);
}

// Whether `line` has any dot in the cell holding dot (x, y).
static inline bool braille_cell_is_used(const uint8_t *masks, unsigned rows, unsigned cols, unsigned line, int x,
                                        int y) {
  size_t index;
  uint8_t bit;
  return braille_dot_position(rows, cols, line, x, y, &index, &bit) && masks[index] != 0;
}

// Whether a line other than `line` has the dot (x, y).
static bool braille_other_line_dot(const uint8_t *masks, unsigned rows, unsigned cols, unsigned num_lines,
                                   unsigned line, int x, int y) {
  for (unsigned other = 0; other < num_lines; ++other)
    if (other != line && braille_dot_is_set(masks, rows, cols, other, x, y))
      return true;
  return false;
}

// Dot row of the sample of `line` taken `age` updates ago (0 is the newest one).
// Data is sample major, in screen order. Samples older than the oldest one
// repeat it.
static int braille_sample_row(const double *data, size_t num_samples, unsigned num_lines, bool newest_left,
                              unsigned dot_rows, size_t age, unsigned line) {
  if (age >= num_samples)
    age = num_samples - 1;
  size_t sample = newest_left ? age : num_samples - 1 - age;
  return (int)plot_value_to_dot_row(data[sample * num_lines + line], dot_rows);
}

// Metric whose samples character column col draws: groups of num_lines columns
// start at the newest edge.
static unsigned braille_column_line(int col, int cols, unsigned num_lines, bool newest_left) {
  int from_newest_group = newest_left ? col : col - cols;
  return (unsigned)((from_newest_group % (int)num_lines + (int)num_lines) % (int)num_lines);
}

// Geometry and data of a plot being drawn.
struct braille_canvas {
  uint8_t *masks;
  unsigned rows, cols, num_lines, dot_rows;
  bool newest_left;
  const double *data;
  size_t num_samples;
  // Character columns drawn: the groups having their samples.
  int first_col, last_col;
};

// Whether dot column x is in the drawn columns.
static inline bool braille_canvas_has(const struct braille_canvas *canvas, int x) {
  return x >= 0 && x / BRAILLE_CELL_COLS >= canvas->first_col && x / BRAILLE_CELL_COLS <= canvas->last_col;
}

// Level a straight line from y_prev to y_new is at when it leaves the columns
// of the other metrics, entering the line's own column.
static inline int braille_straight_level(int y_prev, int y_new, unsigned num_lines) {
  return (int)lround(y_prev + (double)(y_new - y_prev) * (num_lines - 1) / num_lines);
}

// First dot of the join from y_from to y_new in the joining dot column.
static inline int braille_join_start(int y_from, int y_new) {
  if (abs(y_new - y_from) <= 1)
    return y_from;
  return y_from + (y_from < y_new ? 1 : -1);
}

// Whether the metric of character column col (possibly outside the window) may
// use the cell holding dot row y there: between the start of its join when it
// arrives flat and its sample. Arriving straight, its join is within. Only
// depends on the samples, so the choice of a line does not depend on the
// choices of the others, nor on what is visible.
static bool braille_owner_may_use(const struct braille_canvas *canvas, int col, int y) {
  unsigned n = canvas->num_lines;
  unsigned owner = braille_column_line(col, (int)canvas->cols, n, canvas->newest_left);
  size_t age = (size_t)(canvas->newest_left ? col : (int)canvas->cols - 1 - col) / n;
  int y_new =
      braille_sample_row(canvas->data, canvas->num_samples, n, canvas->newest_left, canvas->dot_rows, age, owner);
  int y_prev =
      braille_sample_row(canvas->data, canvas->num_samples, n, canvas->newest_left, canvas->dot_rows, age + 1, owner);
  int start = braille_join_start(y_prev, y_new);
  int lo = (start < y_new ? start : y_new) / BRAILLE_CELL_ROWS;
  int hi = (start < y_new ? y_new : start) / BRAILLE_CELL_ROWS;
  int row = y / BRAILLE_CELL_ROWS;
  return row >= lo && row <= hi;
}

// Set dot (x, y) of `line` if drawing, else count it when the metric of its
// column may use its cell: there, it would be hidden.
static unsigned braille_path_dot(struct braille_canvas *canvas, unsigned line, int x, int y, bool draw) {
  if (draw) {
    if (braille_canvas_has(canvas, x))
      braille_set_dot(canvas->masks, canvas->rows, canvas->cols, line, x, y);
    return 0;
  }
  int col = x >= 0 ? x / BRAILLE_CELL_COLS : -1 - (-1 - x) / BRAILLE_CELL_COLS;
  return braille_owner_may_use(canvas, col, y) ? 1 : 0;
}

// Path of `line` through the dot columns of the other metrics, between its
// sample at dot column x_from (level y_from) and its next own column, where it
// arrives at y_to. Straight when smooth, else flat at y_from. Draws the path or
// returns the number of its dots that would be hidden.
static unsigned braille_path(struct braille_canvas *canvas, unsigned line, int x_from, int y_from, int y_to,
                             bool smooth, bool draw) {
  int dir = canvas->newest_left ? -1 : 1;
  // The dot columns of the other metrics.
  int span = 2 * (int)canvas->num_lines - 2;
  unsigned hidden = 0;
  int last = y_from;
  for (int i = 1; i <= span; ++i) {
    int x = x_from + dir * i;
    int y = smooth ? (int)lround(y_from + (double)(y_to - y_from) * i / span) : y_from;
    int step = y > last ? 1 : -1;
    for (int dot = abs(y - last) > 1 ? last + step : y;; dot += step) {
      hidden += braille_path_dot(canvas, line, x, dot, draw);
      if (dot == y)
        break;
    }
    last = y;
  }
  return hidden;
}

// Level `line` arrives at in its own character column col, drawing the sample
// taken `age` updates ago: straight towards the sample, or flat at the
// previous one where a straight line may be hidden more than a flat one.
static int braille_arrival_level(struct braille_canvas *canvas, int col, unsigned line, size_t age) {
  unsigned n = canvas->num_lines;
  int y_prev =
      braille_sample_row(canvas->data, canvas->num_samples, n, canvas->newest_left, canvas->dot_rows, age + 1, line);
  int y_new =
      braille_sample_row(canvas->data, canvas->num_samples, n, canvas->newest_left, canvas->dot_rows, age, line);
  int y_straight = braille_straight_level(y_prev, y_new, n);
  if (n == 1 || y_straight == y_prev)
    return y_prev;
  // The line's previous sample, in its previous own column.
  int dir = canvas->newest_left ? -1 : 1;
  int x_prev = col * BRAILLE_CELL_COLS + (canvas->newest_left ? 0 : 1) - dir * (int)n * BRAILLE_CELL_COLS;
  bool smooth = braille_path(canvas, line, x_prev, y_prev, y_straight, true, false) <=
                braille_path(canvas, line, x_prev, y_prev, y_prev, false, false);
  return smooth ? y_straight : y_prev;
}

// Draw the plot using Unicode braille patterns. Each character cell provides a
// 2x4 dot matrix, i.e. eight times more detail than a regular character.
//
// A cell has a single color, so as in the legacy renderer the metrics take
// turns: each update takes num_lines character columns, column k of the group
// drawing the sample of metric k. The newest group is at the edge of the
// newest data, the oldest one may be cut by the other edge. In its column, a
// metric's sample is on the dot column facing the newest data and the other
// dot column joins it to where the metric arrives from (on that level for a one
// dot climb).
//
// Between two of its columns, a metric goes through the columns of the other
// metrics in a straight line towards its next sample. Where the line may be
// hidden by the columns' metrics more than a flat one, the metric continues
// flat at its previous level and joins its next sample in its own column
// instead. After its newest sample, a metric continues flat to the newest edge.
//
// A cell only shows the dots of one metric, in its color, so no metric is ever
// drawn in the color of another one. It is the column's metric when it has
// dots there, so a metric's samples are always shown. Otherwise, it is the
// metric going through the cell whose own column is the nearest (the earlier
// one in time on a tie), so that a row shared by several metrics is evenly
// split between them. Crossing a climb, a flat line has a one cell gap.
// Columns move as a whole when the graph scrolls.
static void nvtop_line_plot_braille(WINDOW *win, size_t num_samples, const double *data, unsigned num_lines, int rows,
                                    int cols, bool newest_left, struct nvtop_braille_plot *state) {
  uint8_t *masks = state->masks;
  size_t cell_count = (size_t)rows * (size_t)cols;
  memset(masks, 0, (size_t)num_lines * cell_count);

  unsigned dot_rows = (unsigned)rows * BRAILLE_CELL_ROWS;
  // Draw the groups that have their samples (and the previous one), from the
  // newest one.
  unsigned groups = ((unsigned)cols + num_lines - 1) / num_lines;
  if (num_samples <= groups)
    groups = num_samples > 1 ? (unsigned)(num_samples - 1) : 1;
  struct braille_canvas canvas = {.masks = masks,
                                  .rows = (unsigned)rows,
                                  .cols = (unsigned)cols,
                                  .num_lines = num_lines,
                                  .dot_rows = dot_rows,
                                  .newest_left = newest_left,
                                  .data = data,
                                  .num_samples = num_samples,
                                  .first_col = 0,
                                  .last_col = cols - 1};
  if (newest_left)
    canvas.last_col = (int)(groups * num_lines) - 1;
  else
    canvas.first_col = cols - (int)(groups * num_lines);
  // Dot column direction towards the newest data.
  int dir = newest_left ? -1 : 1;

  for (unsigned group = 0; group < groups; ++group) {
    for (unsigned line = 0; line < num_lines; ++line) {
      int col = newest_left ? (int)(group * num_lines + line) : cols - (int)((group + 1) * num_lines) + (int)line;
      if (col < 0 || col >= cols)
        continue;
      // Dot columns of the cell, towards the newest data and away from it.
      int x_new = col * BRAILLE_CELL_COLS + (newest_left ? 0 : 1);
      int x_old = x_new - dir;
      // The line's previous sample, in its previous own column.
      int x_prev = x_new - dir * (int)num_lines * BRAILLE_CELL_COLS;
      int y_prev = braille_sample_row(data, num_samples, num_lines, newest_left, dot_rows, group + 1, line);
      int y_new = braille_sample_row(data, num_samples, num_lines, newest_left, dot_rows, group, line);
      int y_from = braille_arrival_level(&canvas, col, line, group);
      braille_path(&canvas, line, x_prev, y_prev, y_from, y_from != y_prev, true);
      // The join in the own column.
      int join_start = braille_join_start(y_from, y_new);
      int join_end = abs(y_new - y_from) > 1 ? y_new + (y_from < y_new ? -1 : 1) : join_start;
      braille_set_vertical(masks, (unsigned)rows, (unsigned)cols, line, x_old, join_start, join_end);
      braille_set_dot(masks, (unsigned)rows, (unsigned)cols, line, x_new, y_new);
    }
  }
  // After their newest sample, the metrics continue flat to the newest edge.
  for (unsigned line = 0; line < num_lines; ++line) {
    int col = newest_left ? (int)line : cols - (int)num_lines + (int)line;
    int later_cols = newest_left ? (int)line : (int)num_lines - 1 - (int)line;
    int x_new = col * BRAILLE_CELL_COLS + (newest_left ? 0 : 1);
    int y = braille_sample_row(data, num_samples, num_lines, newest_left, dot_rows, 0, line);
    for (int i = 1; i <= later_cols * BRAILLE_CELL_COLS; ++i)
      braille_path_dot(&canvas, line, x_new + dir * i, y, true);
  }

  // The column's metric owns its cells, so another line going through them is
  // hidden there. Where that line is on one end of the join, draw the end in
  // the joining dot column as well: landing on another line, the line keeps its
  // shape instead of losing a dot; leaving one from a cell the stroke already
  // uses, the stroke stays attached to it. These dots are in cells the column's
  // metric already uses.
  for (int col = canvas.first_col; col <= canvas.last_col; ++col) {
    if (col < 0 || col >= cols)
      continue;
    unsigned line = braille_column_line(col, cols, num_lines, newest_left);
    size_t age = (size_t)(newest_left ? col : cols - 1 - col) / num_lines;
    int x_new = col * BRAILLE_CELL_COLS + (newest_left ? 0 : 1);
    int x_old = x_new - dir;
    int y_new = braille_sample_row(data, num_samples, num_lines, newest_left, dot_rows, age, line);
    int y_from = braille_arrival_level(&canvas, col, line, age);
    if (y_new != y_from && braille_other_line_dot(masks, (unsigned)rows, (unsigned)cols, num_lines, line, x_old, y_new))
      braille_set_dot(masks, (unsigned)rows, (unsigned)cols, line, x_old, y_new);
    if (abs(y_new - y_from) > 1 && braille_cell_is_used(masks, (unsigned)rows, (unsigned)cols, line, x_old, y_from) &&
        braille_other_line_dot(masks, (unsigned)rows, (unsigned)cols, num_lines, line, x_old, y_from))
      braille_set_dot(masks, (unsigned)rows, (unsigned)cols, line, x_old, y_from);
  }

  for (int col = 0; col < cols; ++col) {
    unsigned column_line = braille_column_line(col, cols, num_lines, newest_left);
    // The other metrics by preference: the nearest own column first, then the
    // metric whose own column of the group came earlier in time.
    unsigned order[MAX_LINES_PER_PLOT];
    unsigned num_order = 0;
    for (unsigned distance = 1; distance <= num_lines / 2; ++distance) {
      unsigned right = (column_line + distance) % num_lines;
      unsigned left = (column_line + num_lines - distance) % num_lines;
      // Columns follow the time order towards the newest data.
      unsigned earlier = newest_left ? right : left;
      unsigned later = newest_left ? left : right;
      order[num_order++] = earlier;
      if (later != earlier)
        order[num_order++] = later;
    }
    for (int row = 0; row < rows; ++row) {
      size_t cell = (size_t)row * (size_t)cols + (size_t)col;
      uint8_t own = masks[(size_t)column_line * cell_count + cell];
      int shown = own ? (int)column_line : -1;
      // A free cell goes to the preferred other metric there.
      for (unsigned i = 0; shown < 0 && i < num_order; ++i)
        if (masks[(size_t)order[i] * cell_count + cell])
          shown = (int)order[i];
      if (shown < 0)
        continue;
      wchar_t glyph[2] = {(wchar_t)(BRAILLE_BASE + masks[(size_t)shown * cell_count + cell]), L'\0'};
      wcolor_set(win, plot_line_colors[shown], NULL);
      mvwaddnwstr(win, row, col, glyph, 1);
    }
  }
}

#endif // NVTOP_HAVE_WIDE_CURSES

bool nvtop_braille_plot_init(struct nvtop_braille_plot *plot, unsigned rows, unsigned cols) {
  memset(plot, 0, sizeof(*plot));
  plot->rows = rows;
  plot->cols = cols;
  // Sized for the most lines a plot can hold, independently of the plot style.
  // An empty window still gets a buffer, so allocation failures are the only
  // reason to fail.
  size_t mask_count = (size_t)MAX_LINES_PER_PLOT * (size_t)rows * (size_t)cols;
  plot->masks = malloc(mask_count ? mask_count : 1);
  return plot->masks != NULL;
}

void nvtop_braille_plot_free(struct nvtop_braille_plot *plot) {
  free(plot->masks);
  memset(plot, 0, sizeof(*plot));
}

size_t nvtop_plot_braille_samples(unsigned cols, unsigned num_lines) {
  if (num_lines == 0)
    num_lines = 1;
  // With several metrics, one more: the strokes of the other metrics' columns
  // the oldest line crosses start from the sample before.
  return ((size_t)cols + num_lines - 1) / num_lines + (num_lines > 1 ? 2 : 1);
}

// The locale is set once at startup (before any drawing), so the capability
// checks are computed on first use and cached.
#ifdef NVTOP_HAVE_WIDE_CURSES
static bool cached_check(int *cache, bool (*check)(void)) {
  if (*cache < 0)
    *cache = check();
  return *cache;
}

bool nvtop_plot_supports_braille(void) {
  static int supported = -1;
  return cached_check(&supported, terminal_supports_braille);
}
#else
bool nvtop_plot_supports_braille(void) { return false; }
#endif

void nvtop_line_plot(WINDOW *win, size_t num_samples, const double *data, unsigned num_lines,
                     enum nvtop_plot_style style, bool newest_left, struct nvtop_braille_plot *braille,
                     char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE]) {
  if (num_samples == 0 || num_lines == 0 || num_lines > MAX_LINES_PER_PLOT)
    return;
#ifndef NVTOP_HAVE_WIDE_CURSES
  // Only the braille renderer, which needs wide characters, uses them.
  (void)style;
  (void)braille;
#endif
  int rows, cols;
  getmaxyx(win, rows, cols);
  if (rows <= 0 || cols <= 0)
    return;

#ifdef NVTOP_HAVE_WIDE_CURSES
  // The braille state must match the window; otherwise its buffers are too
  // small and the legacy renderer is used instead.
  bool braille_state_fits =
      braille && braille->masks && braille->rows == (unsigned)rows && braille->cols == (unsigned)cols;
  if (style == nvtop_plot_style_braille && nvtop_plot_supports_braille() && braille_state_fits) {
    nvtop_line_plot_braille(win, num_samples, data, num_lines, rows, cols, newest_left, braille);
  } else
#endif
    nvtop_line_plot_line(win, num_samples, data, num_lines, newest_left);

  // The legend sits on the side of the oldest data.
  draw_plot_legend(win, rows, cols, num_lines, !newest_left, legend);
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
