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

#include <gtest/gtest.h>

extern "C" {
#include "nvtop/interface_layout_selection.h"
}

TEST(InterfaceLayour, LayoutSelection_issue_147) {
    plot_info_to_draw to_draw_default = plot_default_draw_info();
    unsigned nGpu = 8;
    plot_info_to_draw plot_display[8];
    for (unsigned i = 0; i < nGpu; ++i)
        plot_display[i] = to_draw_default;
    process_field_displayed proc_display = process_default_displayed_field();
    struct window_position dev_positions[8];
    struct window_position plot_positions[MAX_CHARTS];
    unsigned num_plots = 0;
    unsigned map_dev_to_plot[8];
    struct window_position process_position;
    struct window_position setup_position;
    compute_sizes_from_layout(8, 3, 78, 26, 189, plot_display, proc_display, dev_positions, &num_plots, plot_positions,
                              map_dev_to_plot, &process_position, &setup_position);
}
