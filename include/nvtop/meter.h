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

#ifndef NVTOP_METER_H_
#define NVTOP_METER_H_

#include "nvtop/common.h"

#include <ncurses.h>
#include <stdbool.h>

// Glyphs the graphical meters may use, beyond ASCII.
struct nvtop_meter_glyphs {
  bool eighth_blocks; // U+2589..U+258F, for a fill with 1/8 cell precision
  bool middle_dot;    // U+00B7, for the track
};

// The glyphs the build, the locale and the terminal can display.
struct nvtop_meter_glyphs nvtop_meter_available_glyphs(void);

// Initialize the color pairs only the meters use (bar_effective_*_color). The
// other ones are the interface's green, yellow, red and cyan pairs.
void nvtop_meter_init_colors(void);

// Room for the value of a meter drawn in win after prelude: the bar width.
// Longer values are not drawn.
int nvtop_meter_value_space(WINDOW *win, const char *prelude, bool legacy);

// Draw a meter on the first row of win: prelude, then a bar filled to
// percentage, its first effective_percentage drawn as effective load when
// draw_effective is set, and value right aligned over the bar. Legacy meters
// are the original bracketed pipe meters; graphical meters are colored bars
// using glyphs, their fill green, yellow or red at the given levels.
void nvtop_meter_draw(WINDOW *win, const char *prelude, unsigned int percentage, bool draw_effective,
                      unsigned int effective_percentage, const char *value, bool legacy,
                      struct nvtop_meter_glyphs glyphs, struct nvtop_meter_levels levels);

#endif // NVTOP_METER_H_
