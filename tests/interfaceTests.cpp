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
#include <cstdio>
#include <cstdlib>
#include <gtest/gtest.h>
#include <iostream>
#include <unistd.h>
#include <vector>

extern "C" {
#include "nvtop/extract_gpuinfo_common.h"
#include "nvtop/interface.h"
#include "nvtop/interface_layout_selection.h"
#include "nvtop/interface_options.h"
#include "nvtop/pcie_utilization.h"
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

namespace {

// Build a gpuinfo_static_info with valid max PCIe gen / width fields.
struct gpuinfo_static_info make_static_info(unsigned gen, unsigned width) {
  struct gpuinfo_static_info info = {};
  SET_GPUINFO_STATIC(&info, max_pcie_gen, gen);
  SET_GPUINFO_STATIC(&info, max_pcie_link_width, width);
  return info;
}

} // namespace

TEST(PcieUtilization, MaxBandwidthPerGeneration) {
  // Per-lane effective rate (KB/s) multiplied by the link width.
  struct gpuinfo_static_info gen1 = make_static_info(1, 16);
  struct gpuinfo_static_info gen2 = make_static_info(2, 16);
  struct gpuinfo_static_info gen3 = make_static_info(3, 16);
  struct gpuinfo_static_info gen4 = make_static_info(4, 16);
  struct gpuinfo_static_info gen5 = make_static_info(5, 16);
  struct gpuinfo_static_info gen6 = make_static_info(6, 16);
  struct gpuinfo_static_info gen5x4 = make_static_info(5, 4);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen1), 250000u * 16u);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen2), 500000u * 16u);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen3), 984600u * 16u);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen4), 1969000u * 16u);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen5), 3938000u * 16u);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen6), 7563000u * 16u);
  // Width scales the bandwidth linearly.
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen5x4), 3938000u * 4u);
}

TEST(PcieUtilization, MaxBandwidthUnknownReturnsZero) {
  // Unsupported / unknown generation.
  struct gpuinfo_static_info gen7 = make_static_info(7, 16);
  EXPECT_EQ(pcie_max_bandwidth_kbs(&gen7), 0u);
  // Missing validity flags.
  struct gpuinfo_static_info no_info = {};
  EXPECT_EQ(pcie_max_bandwidth_kbs(&no_info), 0u);
}

TEST(PcieUtilization, LoadPercentClampsAndScales) {
  struct gpuinfo_static_info info = make_static_info(5, 16); // 63,008,000 KB/s max
  unsigned max_bw = 3938000u * 16u;
  EXPECT_EQ(pcie_load_percent(0u, &info), 0u);
  EXPECT_EQ(pcie_load_percent(max_bw / 2u, &info), 50u);
  EXPECT_EQ(pcie_load_percent(max_bw, &info), 100u);
  // Above the theoretical max is clamped to 100%.
  EXPECT_EQ(pcie_load_percent(max_bw * 2u, &info), 100u);
}

TEST(PcieUtilization, LoadPercentNoMaxReturnsZero) {
  struct gpuinfo_static_info no_info = {};
  EXPECT_EQ(pcie_load_percent(1000000u, &no_info), 0u);
}

TEST(BusyUsage, ScalesAndRounds) {
  const uint64_t second = UINT64_C(1000000000);
  EXPECT_EQ(busy_usage_from_time_usage_round(0, 0, second), 0u);
  EXPECT_EQ(busy_usage_from_time_usage_round(second / 2, 0, second), 50u);
  EXPECT_EQ(busy_usage_from_time_usage_round(3 * second + 994000000, 3 * second, second), 99u);
  EXPECT_EQ(busy_usage_from_time_usage_round(3 * second + 996000000, 3 * second, second), 100u);
  EXPECT_EQ(busy_usage_from_time_usage_round(second, 0, second), 100u);
}

TEST(BusyUsage, ClampsBusyTimeLongerThanInterval) {
  // i915 accounts the busy time in chunks: a fully busy engine can report e.g. 1.16s over a 1s interval.
  const uint64_t second = UINT64_C(1000000000);
  EXPECT_EQ(busy_usage_from_time_usage_round(second + 160000000, 0, second), 100u);
  EXPECT_EQ(busy_usage_from_time_usage_round(UINT64_C(10) * second, 0, second), 100u);
}

TEST(BusyUsage, ZeroIntervalReturnsZero) {
  // No elapsed time means no measurable rate; must not divide by zero.
  EXPECT_EQ(busy_usage_from_time_usage_round(0, 0, 0), 0u);
  EXPECT_EQ(busy_usage_from_time_usage_round(UINT64_C(1000000), 0, 0), 0u);
}

TEST(InterfaceOptions, NpuDefaultPlotsRespectMaxLines) {
  plot_info_to_draw npu = plot_npu_default_draw_info();
  // The NPU defaults add HVX/HMX on top of the GPU defaults.
  EXPECT_TRUE(plot_isset_draw_info(plot_hvx_util_rate, npu));
  EXPECT_TRUE(plot_isset_draw_info(plot_hmx_util_rate, npu));
  // They must never exceed the number of plot lines nvtop can draw.
  EXPECT_LE(plot_count_draw_info(npu), MAX_LINES_PER_PLOT);
}

TEST(InterfaceOptions, AddingManyPlotsRespectsMaxLines) {
  // Mirrors loading a config file that selects more metrics than can be drawn.
  plot_info_to_draw to_draw = 0;
  for (int i = plot_gpu_rate; i < plot_information_count; ++i)
    to_draw = plot_add_draw_info((enum plot_information)i, to_draw);
  EXPECT_LE(plot_count_draw_info(to_draw), MAX_LINES_PER_PLOT);
}

TEST(InterfaceOptions, ProcessingUnitNames) {
  EXPECT_STREQ(processing_unit_name(gpu_processing_unit_gpu), "GPU");
  EXPECT_STREQ(processing_unit_name(gpu_processing_unit_npu), "NPU");
  // Out-of-range values fall back to the default.
  EXPECT_STREQ(processing_unit_name((enum gpu_processing_unit)gpu_processing_unit_count), "GPU");
}

TEST(InterfaceOptions, DynamicMemoryUnitsConfig) {
  char config_path[] = "interface-options-XXXXXX";
  int fd = mkstemp(config_path);
  ASSERT_NE(fd, -1);
  close(fd);

  nvtop_interface_option options = {};
  options.config_file_location = config_path;
  options.sort_processes_by = process_memory;
  options.process_fields_displayed = process_default_displayed_field();
  options.dynamic_memory_units = true;

  ASSERT_TRUE(save_interface_options_to_config_file(0, &options));
  options.dynamic_memory_units = false;
  ASSERT_TRUE(load_interface_options_from_config_file(0, &options));
  EXPECT_TRUE(options.dynamic_memory_units);

  EXPECT_EQ(std::remove(config_path), 0);
}

TEST(InterfaceOptions, UnmonitoredGpusDropStaleProcesses) {
  // An unmonitored GPU is no longer refreshed, so its process entries would
  // otherwise keep pointers to process-cache strings freed by later refreshes.
  // They must be dropped when monitoring is disabled.
  struct gpu_info monitored = {};
  struct gpu_info unmonitored = {};
  monitored.processes_count = 3;
  unmonitored.processes_count = 2;

  LIST_HEAD(monitored_gpus);
  LIST_HEAD(non_monitored_gpus);
  list_add_tail(&monitored.list, &monitored_gpus);
  list_add_tail(&unmonitored.list, &non_monitored_gpus);

  nvtop_interface_option options = {};
  options.gpu_specific_opts = static_cast<nvtop_interface_gpu_opts *>(calloc(2, sizeof(*options.gpu_specific_opts)));
  ASSERT_NE(options.gpu_specific_opts, nullptr);
  options.gpu_specific_opts[0].doNotMonitor = false;
  options.gpu_specific_opts[1].doNotMonitor = true;

  EXPECT_EQ(interface_check_and_fix_monitored_gpus(2, &monitored_gpus, &non_monitored_gpus, &options), 1u);
  EXPECT_EQ(monitored.processes_count, 3u);
  EXPECT_EQ(unmonitored.processes_count, 0u);

  // Re-monitoring must not resurrect the dropped entries before the next refresh.
  options.gpu_specific_opts[1].doNotMonitor = false;
  EXPECT_EQ(interface_check_and_fix_monitored_gpus(2, &monitored_gpus, &non_monitored_gpus, &options), 2u);
  EXPECT_EQ(unmonitored.processes_count, 0u);

  free(options.gpu_specific_opts);
}

TEST(InterfaceScroll, UpdateSelectedOffset) {
  // No entries: nothing moves.
  unsigned selected = 0, offset = 0;
  update_selected_offset_with_window_size(&selected, &offset, 10, 0);
  EXPECT_EQ(selected, 0u);
  EXPECT_EQ(offset, 0u);

  // Fewer entries than lines: the list starts at the top.
  selected = 3;
  offset = 0;
  update_selected_offset_with_window_size(&selected, &offset, 10, 5);
  EXPECT_EQ(selected, 3u);
  EXPECT_EQ(offset, 0u);

  // Selection past the end is clamped to the last entry.
  selected = 9;
  offset = 0;
  update_selected_offset_with_window_size(&selected, &offset, 10, 5);
  EXPECT_EQ(selected, 4u);
  EXPECT_EQ(offset, 0u);

  // Selection above the window pulls the offset down to the selection.
  selected = 2;
  offset = 9;
  update_selected_offset_with_window_size(&selected, &offset, 5, 20);
  EXPECT_EQ(selected, 2u);
  EXPECT_EQ(offset, 2u);

  // Selection below the window scrolls minimally.
  selected = 15;
  offset = 0;
  update_selected_offset_with_window_size(&selected, &offset, 5, 20);
  EXPECT_EQ(offset, 11u);

  // Not enough entries left below the offset to fill the window: scroll back.
  selected = 17;
  offset = 16;
  update_selected_offset_with_window_size(&selected, &offset, 5, 18);
  EXPECT_EQ(offset, 13u);
}

TEST(InterfaceScroll, KeepTotalRowVisible) {
  // Window entirely above the last process: TOTAL stays out of view.
  EXPECT_EQ(keep_total_row_visible(0, 0, 3, 5), 0u);

  // Last process in view and selection inside the shifted window: shift up by one.
  EXPECT_EQ(keep_total_row_visible(4, 2, 3, 5), 3u);

  // Shift would scroll the selection out of view: keep the offset.
  EXPECT_EQ(keep_total_row_visible(2, 2, 3, 5), 2u);

  // Exactly as many processes as lines with the selection at the top: the TOTAL
  // row is not shown until the selection moves (the window cannot scroll).
  EXPECT_EQ(keep_total_row_visible(0, 0, 3, 3), 0u);

  // Single line window: the TOTAL row cannot share the line with a process.
  EXPECT_EQ(keep_total_row_visible(0, 0, 1, 1), 0u);
  EXPECT_EQ(keep_total_row_visible(2, 2, 1, 3), 2u);

  // All processes already fit in the window: nothing to do.
  EXPECT_EQ(keep_total_row_visible(1, 0, 3, 2), 0u);
}

TEST(InterfaceTotalRow, SumProcesses) {
  // The TOTAL row sums GPU, encoder, decoder, GPU memory, VRAM percentage, CPU
  // and CPU memory across all processes. Each process reports VRAM percentage of
  // its own device, so the summed percentage is the share of VRAM in use.
  struct gpu_process p1 = {};
  SET_GPUINFO_PROCESS(&p1, gpu_usage, 10);
  SET_GPUINFO_PROCESS(&p1, encode_usage, 5);
  SET_GPUINFO_PROCESS(&p1, decode_usage, 3);
  SET_GPUINFO_PROCESS(&p1, gpu_memory_usage, 1000);
  SET_GPUINFO_PROCESS(&p1, gpu_memory_percentage, 7);
  SET_GPUINFO_PROCESS(&p1, cpu_usage, 20);
  SET_GPUINFO_PROCESS(&p1, cpu_memory_res, 4000);

  struct gpu_process p2 = {};
  SET_GPUINFO_PROCESS(&p2, gpu_usage, 30);
  SET_GPUINFO_PROCESS(&p2, decode_usage, 2);
  SET_GPUINFO_PROCESS(&p2, gpu_memory_usage, 2000);
  SET_GPUINFO_PROCESS(&p2, gpu_memory_percentage, 9);
  SET_GPUINFO_PROCESS(&p2, cpu_usage, 40);
  SET_GPUINFO_PROCESS(&p2, cpu_memory_res, 1000);

  struct gpuid_and_process procs[2] = {{0, &p1}, {1, &p2}};
  all_processes all_procs = {2, procs};

  struct gpu_process total = {};
  sum_processes(all_procs, &total);

  EXPECT_TRUE(GPUINFO_PROCESS_FIELD_VALID(&total, gpu_usage));
  EXPECT_EQ(total.gpu_usage, 40u);
  EXPECT_EQ(total.encode_usage, 5u); // only p1 reports encode usage
  EXPECT_EQ(total.decode_usage, 5u);
  EXPECT_EQ(total.gpu_memory_usage, 3000ULL);
  EXPECT_EQ(total.cpu_usage, 60u);
  EXPECT_EQ(total.cpu_memory_res, 5000u);
  EXPECT_TRUE(GPUINFO_PROCESS_FIELD_VALID(&total, gpu_memory_percentage));
  EXPECT_EQ(total.gpu_memory_percentage, 16u); // 7% + 9% of the device memory

  // Fields reported by no process stay invalid, so the TOTAL row shows N/A for them.
  struct gpu_process p3 = {};
  struct gpu_process p4 = {};
  struct gpuid_and_process procs_na[2] = {{0, &p3}, {1, &p4}};
  all_processes empty_procs = {2, procs_na};
  struct gpu_process total_na = {};
  sum_processes(empty_procs, &total_na);
  EXPECT_FALSE(GPUINFO_PROCESS_FIELD_VALID(&total_na, gpu_usage));
}

TEST(InterfaceTotalRow, SumProcessesEmpty) {
  struct gpu_process total = {};
  all_processes empty = {0, nullptr};
  sum_processes(empty, &total);
  EXPECT_EQ(total.gpu_usage, 0u);
  EXPECT_EQ(total.gpu_memory_usage, 0ULL);
}

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
