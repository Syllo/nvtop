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

#include <algorithm>
#include <gtest/gtest.h>
#include <iostream>
#include <vector>

extern "C" {
#include "nvtop/interface.h"
#include "nvtop/interface_layout_selection.h"
}

static std::ostream &operator<<(std::ostream &os, const struct window_position &win) {
  os << "Win (" << win.posX << ", " << win.posY << ")--(" << win.posX + win.sizeX << "," << win.posY + win.sizeY << ")";
  return os;
}

namespace {

// Returns true if the two windows overlap, false otherwise.
bool window_position_overlap(struct window_position &w1, struct window_position &w2) {
  bool overlapX = w1.posX == w2.posX || (w1.posX < w2.posX && w1.posX + w1.sizeX - 1 >= w2.posX) ||
                  (w1.posX > w2.posX && w2.posX + w2.sizeX - 1 >= w1.posX);
  bool overlapY = w1.posY == w2.posY || (w1.posY < w2.posY && w1.posY + w1.sizeY - 1 >= w2.posY) ||
                  (w1.posY > w2.posY && w2.posY + w2.sizeY - 1 >= w1.posY);
  return overlapX && overlapY;
}

bool window_is_empty(const struct window_position &w1) { return w1.sizeX == 0 || w1.sizeY == 0; }

// Check that the window is not empty
// Returns true if the window is not empty
bool check_non_empty_window(const struct window_position &w1) {
  EXPECT_GT(w1.sizeX, 0) << "Window " << w1 << " should not be empty";
  EXPECT_GT(w1.sizeY, 0) << "Window " << w1 << " should not be empty";
  return not window_is_empty(w1);
}

// Check that the none of the windows overlap any other
bool check_no_windows_overlap(std::vector<struct window_position> &windows) {
  bool has_overlap = false;
  for (unsigned winId = 0; winId < windows.size(); ++winId) {
    struct window_position &currentWin = windows[winId];
    for (unsigned winCompareId = winId + 1; winCompareId < windows.size(); ++winCompareId) {
      struct window_position &compareTo = windows[winCompareId];
      bool overlaps = window_position_overlap(currentWin, compareTo);
      EXPECT_FALSE(overlaps) << "Between " << currentWin << " and " << compareTo;
      has_overlap = has_overlap || overlaps;
    }
  }
  return !has_overlap;
}

// Check that win does not extend past the container
// Returns true if win is contained inside the container
bool check_window_inside(const struct window_position &win, const struct window_position &container) {
  bool insideX = win.posX >= container.posX && win.posX + win.sizeX - 1 <= container.posX + container.sizeX - 1;
  EXPECT_TRUE(insideX) << win << " is not inside " << container;
  bool insideY = win.posY >= container.posY && win.posY + win.sizeY - 1 <= container.posY + container.sizeY - 1;
  EXPECT_TRUE(insideY) << win << " is not inside " << container;
  return insideX && insideY;
}

// Check that w1 is below w2
bool check_window_below(const struct window_position &w1, const struct window_position &w2) {
  EXPECT_GT(w1.posY, w2.posY + w2.sizeY - 1) << w1 << " is not below " << w2;
  return w1.posY > w2.posY + w2.sizeY - 1;
}

// Returns true if the layout is valid
bool check_layout(struct window_position screen, std::vector<struct window_position> &dev_pos,
                  std::vector<struct window_position> &plot_position, struct window_position process_position,
                  struct window_position setup_position) {
  bool layout_valid = true;

  // Header
  // A particularity of the header is that it can be bigger than the screen
  for (auto const &dev_win : dev_pos) {
    bool valid = check_non_empty_window(dev_win);
    if (!valid)
      std::cout << "Error with header window: " << dev_win << std::endl;
    layout_valid = layout_valid && valid;
  }
  layout_valid = layout_valid && check_no_windows_overlap(dev_pos);

  // Plots
  for (auto const &plot_win : plot_position) {
    bool valid = check_non_empty_window(plot_win);
    if (!valid)
      std::cout << "Error with plot window: " << plot_win << std::endl;
    layout_valid = layout_valid && valid;
  }
  for (auto const &dev_win : dev_pos) {
    for (auto const &plot_win : plot_position) {
      layout_valid = layout_valid && check_window_below(plot_win, dev_win);
      layout_valid = layout_valid && check_window_inside(plot_win, screen);
    }
  }
  layout_valid = layout_valid && check_no_windows_overlap(plot_position);

  // Processes
  for (auto const &plot_win : plot_position) {
    layout_valid = layout_valid && check_window_below(process_position, plot_win);
  }
  if (not window_is_empty(process_position))
    layout_valid = layout_valid && check_window_inside(process_position, screen);

  return layout_valid;
}

bool test_with_terminal_size(unsigned device_count, unsigned header_rows, unsigned header_cols, unsigned rows,
                             unsigned cols) {
  struct window_position screen = {.posX = 0, .posY = 0, .sizeX = cols, .sizeY = rows};

  nvtop_interface_gpu_opts to_draw_default = {.to_draw = plot_default_draw_info()};
  std::vector<nvtop_interface_gpu_opts> plot_display(device_count, to_draw_default);

  process_field_displayed proc_display = process_default_displayed_field();

  unsigned num_plots = 0;
  std::vector<struct window_position> dev_positions(device_count);
  std::vector<struct window_position> plot_positions(MAX_CHARTS);
  struct window_position process_position;
  struct window_position setup_position;
  std::vector<unsigned> map_dev_to_plot(device_count);
  compute_sizes_from_layout(device_count, header_rows, header_cols, rows, cols, plot_display.data(), proc_display,
                            dev_positions.data(), &num_plots, plot_positions.data(), map_dev_to_plot.data(),
                            &process_position, &setup_position, false);
  plot_positions.resize(num_plots);

  return check_layout(screen, dev_positions, plot_positions, process_position, setup_position);
}

} // namespace

TEST(InterfaceLayout, LayoutSelection_issue_147) { test_with_terminal_size(8, 3, 78, 26, 189); }

TEST(InterfaceLayout, CheckEmptyProcessWindow) {
  unsigned device_count = 3, header_rows = 3, header_cols = 55, rows = 4, cols = 120;
  struct window_position screen = {.posX = 0, .posY = 0, .sizeX = cols, .sizeY = rows};

  nvtop_interface_gpu_opts to_draw_default = {.to_draw = plot_default_draw_info()};
  std::vector<nvtop_interface_gpu_opts> plot_display(device_count, to_draw_default);

  process_field_displayed proc_display = process_default_displayed_field();

  unsigned num_plots = 0;
  std::vector<struct window_position> dev_positions(device_count);
  std::vector<struct window_position> plot_positions(MAX_CHARTS);
  struct window_position process_position;
  struct window_position setup_position;
  std::vector<unsigned> map_dev_to_plot(device_count);
  compute_sizes_from_layout(device_count, header_rows, header_cols, rows, cols, plot_display.data(), proc_display,
                            dev_positions.data(), &num_plots, plot_positions.data(), map_dev_to_plot.data(),
                            &process_position, &setup_position, false);
  plot_positions.resize(num_plots);
  EXPECT_EQ(num_plots, 0);
  EXPECT_TRUE(window_is_empty(process_position));
}

TEST(InterfaceLayout, FixInfiniteLoop) {
  unsigned device_count = 3, header_rows = 3, header_cols = 55, rows = 22, cols = 25;
  struct window_position screen = {.posX = 0, .posY = 0, .sizeX = cols, .sizeY = rows};

  nvtop_interface_gpu_opts to_draw_default = {.to_draw = plot_default_draw_info()};
  std::vector<nvtop_interface_gpu_opts> plot_display(device_count, to_draw_default);

  process_field_displayed proc_display = process_default_displayed_field();

  unsigned num_plots = 0;
  std::vector<struct window_position> dev_positions(device_count);
  std::vector<struct window_position> plot_positions(MAX_CHARTS);
  struct window_position process_position;
  struct window_position setup_position;
  std::vector<unsigned> map_dev_to_plot(device_count);
  compute_sizes_from_layout(device_count, header_rows, header_cols, rows, cols, plot_display.data(), proc_display,
                            dev_positions.data(), &num_plots, plot_positions.data(), map_dev_to_plot.data(),
                            &process_position, &setup_position, false);
  plot_positions.resize(num_plots);
}

TEST(InterfaceLayout, LayoutSelection_test_fail_case1) { test_with_terminal_size(32, 3, 55, 16, 1760); }

#ifdef THOROUGH_TESTING

TEST(InterfaceLayout, CheckManyTermSize) {
  const std::array<unsigned, 8> dev_count_to_test = {0, 1, 2, 3, 6, 16, 32, 64};
  const std::map<unsigned, unsigned> extra_increment = {{0, 0}, {1, 0},  {2, 0},  {3, 0},
                                                        {6, 4}, {16, 6}, {32, 8}, {64, 17}};
  for (unsigned dev_count : dev_count_to_test) {
    for (unsigned screen_rows = 1; screen_rows < 2048; screen_rows += 1 + extra_increment.at(dev_count)) {
      for (unsigned screen_cols = 1; screen_cols < 2048; screen_cols += 1 + extra_increment.at(dev_count)) {
        for (unsigned header_cols = 55; header_cols < 120; header_cols += 1 + extra_increment.at(dev_count)) {
          ASSERT_TRUE(test_with_terminal_size(dev_count, 3, header_cols, screen_rows, screen_cols))
              << "Problem found with " << dev_count << " devices, (" << 3 << ", header " << header_cols
              << "), terminal size (" << screen_rows << ", " << screen_cols << ")";
        }
      }
    }
  }
}

#endif // THOROUGH_TESTING