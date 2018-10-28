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

#ifndef NVTOP_TIME_H_
#define NVTOP_TIME_H_

#include <time.h>

#ifdef CLOCK_MONOTONIC_RAW
#define NVTOP_CLOCK CLOCK_MONOTONIC_RAW
#else
#define NVTOP_CLOCK CLOCK_MONOTONIC
#endif

typedef struct timespec nvtop_time;

inline void nvtop_get_current_time(nvtop_time *time) {
  clock_gettime(NVTOP_CLOCK, time);
}

inline double nvtop_difftime(nvtop_time t0, nvtop_time t1) {
  double secdiff = difftime(t1.tv_sec, t0.tv_sec);
  if (t1.tv_nsec < t0.tv_nsec) {
    long val = 1000000000l - t0.tv_nsec + t1.tv_nsec;
    secdiff += (double)val / 1e9 - 1.;
  } else {
    long val = t1.tv_nsec - t0.tv_nsec;
    secdiff += (double)val / 1e9;
  }
  return secdiff;
}

inline bool nvtop_has_elapsed_time(
    nvtop_time less, nvtop_time more, nvtop_time elapsed) {
}

#endif // NVTOP_TIME_H_
