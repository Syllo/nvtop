/*
 *
 * Copyright (C) 2022 Maxime Schmitt <maxime.schmitt91@gmail.com>
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

#ifndef NVTOP_COMMON_H__
#define NVTOP_COMMON_H__

#if !defined(HAS_REALLOCARRAY)

#include <errno.h>
#include <stdlib.h>

#undef reallocarray

// Reallocarray from musl libc (https://git.musl-libc.org/cgit/musl/tree/src/malloc/reallocarray.c)
static void *nvtop_reallocarray__(void *ptr, size_t nmemb, size_t size) {
  if (size && nmemb > -1 / size) {
    errno = ENOMEM;
    return NULL;
  }
  return realloc(ptr, nmemb * size);
}
#define reallocarray(ptr, nmemb, size) nvtop_reallocarray__(ptr, nmemb, size)

#endif // !defined(HAS_REALLOCARRAY)

// Increment for the number of tracked processes
// 16 has been experimentally selected for being small while avoiding multipe allocations in most common cases
#define COMMON_PROCESS_LINEAR_REALLOC_INC 16

#define MAX_LINES_PER_PLOT 4

// Helper macro to stringify an integer
#define QUOTE(x) #x
#define EXPAND_AND_QUOTE(x) QUOTE(x)

#endif // NVTOP_COMMON_H__
