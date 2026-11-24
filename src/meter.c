/*
 *
 * Copyright (C) 2026 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#include "nvtop/meter.h"
#include "nvtop/interface_internal_common.h"
#include "nvtop/terminal.h"

#include <ncurses.h>
#include <stdlib.h>
#include <string.h>
#include <tgmath.h>
#include <wchar.h>

static short percentage_meter_color(unsigned int percentage, struct nvtop_meter_levels levels) {
  if (levels.red && percentage >= levels.red_from)
    return red_color;
  if (levels.yellow && percentage >= levels.yellow_from)
    return yellow_color;
  return green_color;
}

static short percentage_meter_effective_block_color(short fill_color) {
  if (fill_color == red_color)
    return bar_effective_red_color;
  if (fill_color == yellow_color)
    return bar_effective_yellow_color;
  return bar_effective_green_color;
}

// Extent of a meter fill, computed with 1/8-cell precision. With Unicode block
// elements the remainder is drawn as a partial block glyph in cell `full`;
// without them it is rounded to the nearest whole cell. The painter and the
// value text both use this, so they always agree on where the fill ends.
struct percentage_meter_fill {
  unsigned full;    // Cells painted solid
  unsigned partial; // Eighths (1..7) of the partial block glyph, 0 when none
};

static struct percentage_meter_fill percentage_meter_fill(unsigned int percentage, int bar_width, bool eighth_blocks) {
  struct percentage_meter_fill fill = {0, 0};
  if (bar_width <= 0)
    return fill;
  if (percentage > 100)
    percentage = 100;
  unsigned total_eighths = (unsigned)bar_width * percentage * 8u / 100u;
  fill.full = total_eighths / 8u;
  unsigned eighth = total_eighths % 8u;
  if (fill.full >= (unsigned)bar_width) {
    fill.full = (unsigned)bar_width;
    return fill;
  }
  if (eighth_blocks)
    fill.partial = eighth;
  else if (eighth >= 4)
    fill.full += 1;
  return fill;
}

// Number of character cells the fill touches. The value text uses the fill
// background on all of them; a cell holding a partial glyph counts as filled
// because the text glyph replaces it.
static unsigned percentage_meter_fill_cells(struct percentage_meter_fill fill) {
  return fill.full + (fill.partial != 0);
}

// Paint a fill: solid reversed cells in full_color, then the partial block
// glyph (if any) in partial_color.
static void percentage_meter_paint_fill(WINDOW *win, int row, int col, struct percentage_meter_fill fill,
                                        short full_color, short partial_color) {
  if (fill.full > 0) {
    // Replace the dim track dots with blank cells before painting the fill.
    mvwhline(win, row, col, ' ', (int)fill.full);
    mvwchgat(win, row, col, (int)fill.full, A_REVERSE, full_color, NULL);
  }
#ifdef NVTOP_HAVE_WIDE_CURSES
  if (fill.partial > 0) {
    wchar_t glyph[2] = {(wchar_t)(0x2590u - fill.partial), L'\0'};
    wcolor_set(win, partial_color, NULL);
    mvwaddnwstr(win, row, col + (int)fill.full, glyph, 1);
    wstandend(win);
  }
#else
  (void)partial_color;
#endif
}

// Width of a meter bar, which is also the room available for its value text.
// The bar follows the prelude and one separator (' ' or '['); the legacy meter
// also reserves the closing ']'. Every meter derives its geometry from this so
// the drawn bar and the callers choosing a value format always agree.
static int percentage_meter_bar_width(WINDOW *win, const char *prelude, bool legacy) {
  int width = getmaxx(win) - (int)strlen(prelude) - (legacy ? 2 : 1);
  return width < 0 ? 0 : width;
}

struct percentage_meter_layout {
  int curx;
  int bar_width;
  int value_space;
};

// Geometry after the prelude has been printed: the bar starts at the cursor,
// and the right-aligned value takes the end of the bar. A value longer than
// the bar is not drawn rather than cut, which would show a different value.
static struct percentage_meter_layout percentage_meter_layout(WINDOW *win, const char *prelude, const char *value) {
  int curx = getcurx(win);
  int bar_width = percentage_meter_bar_width(win, prelude, false);
  int value_space = (int)strlen(value);
  if (value_space > bar_width)
    value_space = 0;
  return (struct percentage_meter_layout){.curx = curx, .bar_width = bar_width, .value_space = value_space};
}

// Draw the meter extent as dim centered dots in the terminal's default
// foreground. A single dot per cell leaves the 1/8-block fill untouched.
static void percentage_meter_draw_track(WINDOW *win, int row, int col, int width, bool middle_dot) {
#ifndef NVTOP_HAVE_WIDE_CURSES
  (void)middle_dot;
#endif
  if (width <= 0)
    return;
  wattron(win, A_DIM);
#ifdef NVTOP_HAVE_WIDE_CURSES
  if (middle_dot) {
    wchar_t dot[2] = {L'\u00B7', L'\0'};
    for (int i = 0; i < width; ++i)
      mvwaddnwstr(win, row, col + i, dot, 1);
  } else
#endif
  {
    mvwhline(win, row, col, '.', width);
  }
  wattroff(win, A_DIM);
  wstandend(win);
}

// Draw the right-aligned value. Each glyph keeps the underlying bar/track color
// as its background and uses a contrasting foreground, so the bar remains
// visible behind the text. The filled cells come from percentage_meter_fill, so
// the text background ends exactly where the painted bar does.
static void percentage_meter_draw_value(WINDOW *win, int row, int col, int bar_width, int value_space,
                                        unsigned util_cells, unsigned effective_cells, short util_color,
                                        const char *value) {
  if (value_space <= 0)
    return;
  int value_start = col + bar_width - value_space;
  for (int i = 0; i < value_space; ++i) {
    unsigned cell = (unsigned)(value_start - col + i);
    short fill_color = 0;
    if (cell < effective_cells)
      fill_color = cyan_color;
    else if (cell < util_cells)
      fill_color = util_color;
    if (fill_color) {
      // Reverse the fill pair so the terminal's default background color
      // becomes the text color. Themes choose their fill colors to contrast
      // with that background, so this adapts to bright and dark palettes.
      wcolor_set(win, fill_color, NULL);
      wattron(win, A_REVERSE);
      mvwaddch(win, row, value_start + i, value[i]);
      wattroff(win, A_REVERSE);
    } else {
      wstandend(win);
      mvwaddch(win, row, value_start + i, value[i]);
    }
  }
  wstandend(win);
}

// Draw the full meter: prelude, track, utilization fill, optional effective
// overlay, then the value on top.
static void percentage_meter_draw(WINDOW *win, const char *prelude, unsigned int util_percentage, bool draw_effective,
                                  unsigned int effective_percentage, const char *value,
                                  struct nvtop_meter_glyphs glyphs, struct nvtop_meter_levels levels) {
  wcolor_set(win, cyan_color, NULL);
  mvwprintw(win, 0, 0, "%s", prelude);
  wstandend(win);
  waddch(win, ' ');

  struct percentage_meter_layout layout = percentage_meter_layout(win, prelude, value);
  percentage_meter_draw_track(win, 0, layout.curx, layout.bar_width, glyphs.middle_dot);

  short base_fill_color = percentage_meter_color(util_percentage, levels);
  struct percentage_meter_fill util_fill =
      percentage_meter_fill(util_percentage, layout.bar_width, glyphs.eighth_blocks);
  percentage_meter_paint_fill(win, 0, layout.curx, util_fill, base_fill_color, base_fill_color);

  // Overlay the effective-load portion in cyan, starting at 0. When effective
  // load equals total utilization the whole bar becomes cyan.
  struct percentage_meter_fill effective_fill = {0, 0};
  if (draw_effective) {
    if (effective_percentage > util_percentage)
      effective_percentage = util_percentage;
    if (effective_percentage > 0) {
      effective_fill = percentage_meter_fill(effective_percentage, layout.bar_width, glyphs.eighth_blocks);
      // A cell has one background: when the effective and utilization
      // boundaries share a cell, the cyan block is drawn over the utilization
      // color, rounding that utilization edge up to the cell boundary.
      short partial_color = effective_percentage == util_percentage
                                ? cyan_color
                                : percentage_meter_effective_block_color(base_fill_color);
      percentage_meter_paint_fill(win, 0, layout.curx, effective_fill, cyan_color, partial_color);
    }
  }

  percentage_meter_draw_value(win, 0, layout.curx, layout.bar_width, layout.value_space,
                              percentage_meter_fill_cells(util_fill), percentage_meter_fill_cells(effective_fill),
                              base_fill_color, value);
  wnoutrefresh(win);
}

// Legacy bracketed pipe meters from the original interface.
static void draw_percentage_meter_legacy(WINDOW *win, const char *prelude, unsigned int new_percentage,
                                         const char *inside_braces_right) {
  wcolor_set(win, cyan_color, NULL);
  mvwprintw(win, 0, 0, "%s", prelude);
  wstandend(win);
  waddch(win, '[');
  int curx, cury;
  curx = getcurx(win);
  cury = getcury(win);
  int between_sbraces = percentage_meter_bar_width(win, prelude, true);
  if (new_percentage > 100)
    new_percentage = 100;
  float usage = round((float)between_sbraces * new_percentage / 100.f);
  int represent_usage = (int)usage;
  whline(win, '|', represent_usage);
  mvwhline(win, cury, curx + represent_usage, ' ', between_sbraces - represent_usage);
  mvwaddch(win, cury, curx + between_sbraces, ']');
  // As for the graphical meters, a value that does not fit is not drawn.
  int value_length = (int)strlen(inside_braces_right);
  if (value_length <= between_sbraces)
    mvwprintw(win, cury, curx + between_sbraces - value_length, "%s", inside_braces_right);
  mvwchgat(win, cury, curx, represent_usage, 0, green_color, NULL);
  wnoutrefresh(win);
}

static void draw_percentage_meter_with_yellow_highlight_legacy(WINDOW *win, const char *prelude,
                                                               unsigned int new_percentage,
                                                               unsigned int yellow_percentage,
                                                               const char *inside_braces_right) {
  draw_percentage_meter_legacy(win, prelude, new_percentage, inside_braces_right);
  if (yellow_percentage > new_percentage)
    yellow_percentage = new_percentage;
  if (yellow_percentage > 100)
    yellow_percentage = 100;
  size_t size_prelude = strlen(prelude);
  int between_sbraces = percentage_meter_bar_width(win, prelude, true);
  float usage = round((float)between_sbraces * yellow_percentage / 100.f);
  mvwchgat(win, 0, (int)size_prelude + 1, (int)usage, 0, yellow_color, NULL);
  wnoutrefresh(win);
}

struct nvtop_meter_glyphs nvtop_meter_available_glyphs(void) {
#ifdef NVTOP_HAVE_WIDE_CURSES
  // The locale is set once at startup, before any drawing: computed on first
  // use.
  static bool known = false;
  static struct nvtop_meter_glyphs glyphs;
  if (!known) {
    bool utf8 = MB_CUR_MAX > 1 && nvtop_locale_is_utf8();
    // The Linux console fonts have the full and half blocks, not the eighths.
    glyphs.eighth_blocks = utf8 && wcwidth((wchar_t)0x258F) == 1 && !nvtop_terminal_is_linux_console();
    glyphs.middle_dot = utf8 && wcwidth((wchar_t)0x00B7) == 1;
    known = true;
  }
  return glyphs;
#else
  return (struct nvtop_meter_glyphs){.eighth_blocks = false, .middle_dot = false};
#endif
}

void nvtop_meter_init_colors(void) {
  // The meters only use the terminal's ANSI palette, so they follow its theme.
  // The fills are the reversed green, yellow, red and cyan pairs, on the
  // terminal's default background. A cell where the effective load ends inside
  // the utilization fill holds a cyan partial block on the utilization color.
  init_pair(bar_effective_green_color, COLOR_CYAN, COLOR_GREEN);
  init_pair(bar_effective_yellow_color, COLOR_CYAN, COLOR_YELLOW);
  init_pair(bar_effective_red_color, COLOR_CYAN, COLOR_RED);
}

int nvtop_meter_value_space(WINDOW *win, const char *prelude, bool legacy) {
  return percentage_meter_bar_width(win, prelude, legacy);
}

void nvtop_meter_draw(WINDOW *win, const char *prelude, unsigned int percentage, bool draw_effective,
                      unsigned int effective_percentage, const char *value, bool legacy,
                      struct nvtop_meter_glyphs glyphs, struct nvtop_meter_levels levels) {
  if (legacy && draw_effective)
    draw_percentage_meter_with_yellow_highlight_legacy(win, prelude, percentage, effective_percentage, value);
  else if (legacy)
    draw_percentage_meter_legacy(win, prelude, percentage, value);
  else
    percentage_meter_draw(win, prelude, percentage, draw_effective, effective_percentage, value, glyphs, levels);
}
