#include <math.h>

#include "nvtop/plot.h"

static inline int data_level(double data, double increment) {
  return (int)(nearbyint(data / increment));
}

void nvtop_line_plot(WINDOW *win, size_t num_data, const double *data,
                     double min, double max) {
  if (num_data == 0)
    return;
  int rows, cols;
  getmaxyx(win, rows, cols);
  double increment = (max - min) / (double)rows;
  int level_previous = data_level(data[0], increment);
  int level_next, level_current;
  for (size_t i = 0; i < num_data || i < (size_t) cols; ++i) {
    level_next = i + 1 == num_data ? level_next : data_level(data[i+1], increment);
    level_current = data_level(data[i], increment);
    int top, bottom;
    if (level_current == level_previous) {
      mvwaddch(win, level_current, i, ACS_HLINE);
    } else {
      if (level_current < level_previous) {
        top = level_previous;
        bottom = level_current;
      } else {
        top = level_current;
        bottom = level_previous;
      }
      mvwaddch(win, top, i, ACS_ULCORNER);
      for (int j = bottom + 1; j < top - 1; j++) {
        mvwaddch(win, j, i, ACS_VLINE);
      }
      mvwaddch(win, bottom, i, ACS_LRCORNER);
    }
  }
}

void nvtop_bar_plot(WINDOW *win, size_t num_data, const double *data,
                     double min, double max) {
  /*ACS_BLOCK*/
}
