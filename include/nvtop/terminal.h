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

#ifndef NVTOP_TERMINAL_H_
#define NVTOP_TERMINAL_H_

#include <langinfo.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

// Whether the locale encodes characters in UTF-8, which the Unicode glyphs
// need.
static inline bool nvtop_locale_is_utf8(void) {
  const char *codeset = nl_langinfo(CODESET);
  return codeset && (strcasecmp(codeset, "UTF-8") == 0 || strcasecmp(codeset, "UTF8") == 0);
}

// Whether the terminal is the Linux virtual console, whose fonts only have a
// few Unicode glyphs (no braille patterns, no 1/8 blocks).
static inline bool nvtop_terminal_is_linux_console(void) {
  const char *term = getenv("TERM");
  return term && (strcmp(term, "linux") == 0 || strncmp(term, "linux-", 6) == 0);
}

#endif // NVTOP_TERMINAL_H_
