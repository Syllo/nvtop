/*
 * Utilization meter rendering tests: fill extent and colors, partial blocks,
 * track, effective load overlay, value text, for the graphical and legacy
 * meters.
 *
 * Meters are rendered into an off-screen ncurses window and read back cell by
 * cell. Every window is one row; the meters below use the prelude "GPU", so
 * the graphical bar starts at column 4 and the legacy one at column 4, after
 * "GPU[". Without the wide character ncurses API, the cases needing Unicode
 * glyphs are left out.
 */

#include "nvtop/interface_internal_common.h"
#include "nvtop/meter.h"

#include <locale.h>
#include <ncurses.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>

#define CHECK(condition)                                                                                               \
  do {                                                                                                                 \
    if (!(condition)) {                                                                                                \
      fprintf(stderr, "%s:%d: %s\n", __FILE__, __LINE__, #condition);                                                  \
      return EXIT_FAILURE;                                                                                             \
    }                                                                                                                  \
  } while (0)

#define SKIP_RETURN_CODE 77
#define BAR_START 4

static const struct nvtop_meter_glyphs ascii_glyphs = {.eighth_blocks = false, .middle_dot = false};
// Yellow from 50%, red from 80%.
static const struct nvtop_meter_levels levels = {.yellow = true, .red = true, .yellow_from = 50, .red_from = 80};

struct cell {
  wchar_t c;
  short pair;
  attr_t attrs;
};

static struct cell read_cell(WINDOW *win, int x) {
  struct cell cell = {0, -1, 0};
#ifdef NVTOP_HAVE_WIDE_CURSES
  cchar_t cchar;
  wchar_t text[CCHARW_MAX + 1] = {0};
  mvwin_wch(win, 0, x, &cchar);
  getcchar(&cchar, text, &cell.attrs, &cell.pair, NULL);
  cell.c = text[0];
#else
  chtype ch = mvwinch(win, 0, x);
  cell.c = (wchar_t)(ch & A_CHARTEXT);
  cell.pair = (short)PAIR_NUMBER(ch);
  cell.attrs = ch & A_ATTRIBUTES & ~A_COLOR;
#endif
  return cell;
}

// A filled cell: blank, reversed, in the fill color.
static bool is_fill(WINDOW *win, int x, short pair) {
  struct cell cell = read_cell(win, x);
  return cell.c == L' ' && (cell.attrs & A_REVERSE) && cell.pair == pair;
}

// A track cell: a dim dot, the middle dot when available.
static bool is_track(WINDOW *win, int x, bool middle_dot) {
  struct cell cell = read_cell(win, x);
  return cell.c == (middle_dot ? (wchar_t)0x00B7 : L'.') && (cell.attrs & A_DIM) && !(cell.attrs & A_REVERSE);
}

// A value character: reversed in the fill color on a filled cell, else plain.
static bool is_value(WINDOW *win, int x, wchar_t c, short fill_pair) {
  struct cell cell = read_cell(win, x);
  if (fill_pair)
    return cell.c == c && (cell.attrs & A_REVERSE) && cell.pair == fill_pair;
  return cell.c == c && !(cell.attrs & A_REVERSE) && cell.pair == 0;
}

static WINDOW *draw_with(int cols, unsigned percentage, bool effective, unsigned effective_percentage,
                         const char *value, bool legacy, struct nvtop_meter_glyphs glyphs,
                         struct nvtop_meter_levels meter_levels) {
  WINDOW *win = newwin(1, cols, 0, 0);
  werase(win);
  nvtop_meter_draw(win, "GPU", percentage, effective, effective_percentage, value, legacy, glyphs, meter_levels);
  return win;
}

static WINDOW *draw(int cols, unsigned percentage, bool effective, unsigned effective_percentage, const char *value,
                    bool legacy, struct nvtop_meter_glyphs glyphs) {
  return draw_with(cols, percentage, effective, effective_percentage, value, legacy, glyphs, levels);
}

static int check_threshold(unsigned percentage, struct nvtop_meter_levels meter_levels, short pair) {
  WINDOW *win = draw_with(BAR_START + 20, percentage, false, 0, "", false, ascii_glyphs, meter_levels);
  CHECK(is_fill(win, BAR_START, pair));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_fill_colors(void) {
  if (check_threshold(49, levels, green_color) != EXIT_SUCCESS ||
      check_threshold(50, levels, yellow_color) != EXIT_SUCCESS ||
      check_threshold(79, levels, yellow_color) != EXIT_SUCCESS ||
      check_threshold(80, levels, red_color) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  return EXIT_SUCCESS;
}

static int test_fill_colors_disabled_levels(void) {
  // Without yellow, green up to red; without red, yellow from yellow on;
  // without both, always green.
  struct nvtop_meter_levels no_yellow = levels, no_red = levels, none = levels;
  no_yellow.yellow = false;
  no_red.red = false;
  none.yellow = none.red = false;
  if (check_threshold(79, no_yellow, green_color) != EXIT_SUCCESS ||
      check_threshold(80, no_yellow, red_color) != EXIT_SUCCESS ||
      check_threshold(100, no_red, yellow_color) != EXIT_SUCCESS ||
      check_threshold(100, none, green_color) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  // Yellow above red: red wins from its level on.
  struct nvtop_meter_levels crossed = {.yellow = true, .red = true, .yellow_from = 90, .red_from = 60};
  if (check_threshold(59, crossed, green_color) != EXIT_SUCCESS ||
      check_threshold(95, crossed, red_color) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  return EXIT_SUCCESS;
}

static int test_fill_whole_cells(void) {
  // Without the block elements, 53% of 20 cells (10 cells and 4 eighths)
  // rounds up to 11 cells.
  WINDOW *win = draw(BAR_START + 20, 53, false, 0, "", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 11; ++x)
    CHECK(is_fill(win, x, yellow_color));
  for (int x = BAR_START + 11; x < BAR_START + 20; ++x)
    CHECK(is_track(win, x, false));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_fill_clamped(void) {
  WINDOW *win = draw(BAR_START + 20, 150, false, 0, "", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 20; ++x)
    CHECK(is_fill(win, x, red_color));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_effective_overlay(void) {
  // 60% utilization with 30% effective load: 6 cyan cells, then 6 yellow.
  WINDOW *win = draw(BAR_START + 20, 60, true, 30, "", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 6; ++x)
    CHECK(is_fill(win, x, cyan_color));
  for (int x = BAR_START + 6; x < BAR_START + 12; ++x)
    CHECK(is_fill(win, x, yellow_color));
  CHECK(is_track(win, BAR_START + 12, false));
  delwin(win);
  // Effective load equal to utilization: the whole fill is cyan.
  win = draw(BAR_START + 20, 60, true, 60, "", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 12; ++x)
    CHECK(is_fill(win, x, cyan_color));
  CHECK(is_track(win, BAR_START + 12, false));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_effective_clamped(void) {
  // Effective load above utilization is drawn as the utilization: 6 cells.
  WINDOW *win = draw(BAR_START + 20, 30, true, 80, "", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 6; ++x)
    CHECK(is_fill(win, x, cyan_color));
  CHECK(is_track(win, BAR_START + 6, false));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_value_over_bar(void) {
  // Right aligned, reversed in the fill color on filled cells.
  WINDOW *win = draw(BAR_START + 10, 100, false, 0, "42%", false, ascii_glyphs);
  CHECK(is_value(win, BAR_START + 7, L'4', red_color) && is_value(win, BAR_START + 8, L'2', red_color) &&
        is_value(win, BAR_START + 9, L'%', red_color));
  delwin(win);
  // On the track, plain text.
  win = draw(BAR_START + 10, 0, false, 0, "0%", false, ascii_glyphs);
  CHECK(is_value(win, BAR_START + 8, L'0', 0) && is_value(win, BAR_START + 9, L'%', 0));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_value_over_effective_fill(void) {
  // 100% utilization, 80% effective load, a value over the whole bar: cyan on
  // the 8 effective cells, red on the last 2.
  WINDOW *win = draw(BAR_START + 10, 100, true, 80, "abcdefghij", false, ascii_glyphs);
  for (int i = 0; i < 10; ++i)
    CHECK(is_value(win, BAR_START + i, (wchar_t)(L'a' + i), i < 8 ? cyan_color : red_color));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_value_too_long(void) {
  // A value longer than the bar is not drawn, rather than cut.
  WINDOW *win = draw(BAR_START + 5, 0, false, 0, "123456", false, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 5; ++x)
    CHECK(is_track(win, x, false));
  delwin(win);
  win = draw(BAR_START + 6, 0, false, 0, "123456", true, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 4; ++x)
    CHECK(read_cell(win, x).c == L' ');
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_value_space(void) {
  WINDOW *win = newwin(1, BAR_START + 20, 0, 0);
  CHECK(nvtop_meter_value_space(win, "GPU", false) == 20);
  CHECK(nvtop_meter_value_space(win, "GPU", true) == 19);
  CHECK(nvtop_meter_value_space(win, "A long prelude, wider than the window", false) == 0);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_effective_colors(void) {
  // A cell where the effective load ends inside the utilization fill: a cyan
  // partial block on the utilization color.
  static const struct {
    short pair, background;
  } expected[] = {{bar_effective_green_color, COLOR_GREEN},
                  {bar_effective_yellow_color, COLOR_YELLOW},
                  {bar_effective_red_color, COLOR_RED}};
  for (size_t i = 0; i < sizeof(expected) / sizeof(*expected); ++i) {
    short foreground, background;
    CHECK(pair_content(expected[i].pair, &foreground, &background) == OK);
    CHECK(foreground == COLOR_CYAN && background == expected[i].background);
  }
  return EXIT_SUCCESS;
}

static int test_legacy(void) {
  // "GPU[" then 19 cells and "]": 50% is 10 pipes, in green.
  WINDOW *win = draw(BAR_START + 20, 50, false, 0, "50%", true, ascii_glyphs);
  CHECK(read_cell(win, BAR_START - 1).c == L'[');
  CHECK(read_cell(win, BAR_START + 19).c == L']');
  for (int x = BAR_START; x < BAR_START + 10; ++x) {
    struct cell cell = read_cell(win, x);
    CHECK(cell.c == L'|' && cell.pair == green_color);
  }
  CHECK(read_cell(win, BAR_START + 10).c == L' ');
  const wchar_t *expected = L"50%";
  for (int i = 0; i < 3; ++i)
    CHECK(read_cell(win, BAR_START + 16 + i).c == expected[i]);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_legacy_effective(void) {
  // The effective load part of the pipes is yellow.
  WINDOW *win = draw(BAR_START + 20, 60, true, 30, "", true, ascii_glyphs);
  for (int x = BAR_START; x < BAR_START + 6; ++x)
    CHECK(read_cell(win, x).pair == yellow_color);
  for (int x = BAR_START + 6; x < BAR_START + 11; ++x)
    CHECK(read_cell(win, x).pair == green_color);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_legacy_effective_clamped(void) {
  // Effective load above utilization is highlighted as the utilization: 30%
  // of the 19 cells is 6 pipes, the cell after them is not yellow.
  WINDOW *win = draw(BAR_START + 20, 30, true, 80, "", true, ascii_glyphs);
  CHECK(read_cell(win, BAR_START + 5).pair == yellow_color);
  CHECK(read_cell(win, BAR_START + 6).pair != yellow_color);
  delwin(win);
  return EXIT_SUCCESS;
}

#ifdef NVTOP_HAVE_WIDE_CURSES

static const struct nvtop_meter_glyphs unicode_glyphs = {.eighth_blocks = true, .middle_dot = true};

// The partial block glyph of a cell filled for `eighths` eighths, from the
// left.
static wchar_t partial_block(unsigned eighths) { return (wchar_t)(0x2590 - eighths); }

static int test_fill_eighths(void) {
  // 53% of 20 cells is 10 cells and 4 eighths: a half block after 10 cells,
  // then the track in middle dots.
  WINDOW *win = draw(BAR_START + 20, 53, false, 0, "", false, unicode_glyphs);
  for (int x = BAR_START; x < BAR_START + 10; ++x)
    CHECK(is_fill(win, x, yellow_color));
  struct cell partial = read_cell(win, BAR_START + 10);
  CHECK(partial.c == partial_block(4) && partial.pair == yellow_color && !(partial.attrs & A_REVERSE));
  for (int x = BAR_START + 11; x < BAR_START + 20; ++x)
    CHECK(is_track(win, x, true));
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_partial_block_glyphs(void) {
  // The partial block grows from the left: 1% of 20 cells is 1/8 of a cell,
  // 3% of 30 cells is 7/8 of one.
  WINDOW *win = draw(BAR_START + 20, 1, false, 0, "", false, unicode_glyphs);
  CHECK(read_cell(win, BAR_START).c == (wchar_t)0x258F);
  delwin(win);
  win = draw(BAR_START + 30, 3, false, 0, "", false, unicode_glyphs);
  CHECK(read_cell(win, BAR_START).c == (wchar_t)0x2589);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_console_glyphs(void) {
  // The Linux console fonts have the middle dot but not the 1/8 blocks. The
  // glyphs are only looked up once, here.
  setenv("TERM", "linux", 1);
  struct nvtop_meter_glyphs glyphs = nvtop_meter_available_glyphs();
  CHECK(!glyphs.eighth_blocks && glyphs.middle_dot);
  return EXIT_SUCCESS;
}

static int test_effective_partial_block(void) {
  // 100% utilization, 33% effective load: 6 cells and 4 eighths of cyan, the
  // partial block on the red utilization fill.
  WINDOW *win = draw(BAR_START + 20, 100, true, 33, "", false, unicode_glyphs);
  for (int x = BAR_START; x < BAR_START + 6; ++x)
    CHECK(is_fill(win, x, cyan_color));
  struct cell partial = read_cell(win, BAR_START + 6);
  CHECK(partial.c == partial_block(4) && partial.pair == bar_effective_red_color);
  CHECK(is_fill(win, BAR_START + 7, red_color));
  delwin(win);
  // The same on the yellow and green utilization fills.
  static const struct {
    unsigned percentage;
    short pair;
  } on_fill[] = {{60, bar_effective_yellow_color}, {40, bar_effective_green_color}};
  for (size_t i = 0; i < sizeof(on_fill) / sizeof(*on_fill); ++i) {
    win = draw(BAR_START + 20, on_fill[i].percentage, true, 33, "", false, unicode_glyphs);
    partial = read_cell(win, BAR_START + 6);
    CHECK(partial.c == partial_block(4) && partial.pair == on_fill[i].pair);
    delwin(win);
  }
  // Effective load equal to utilization: the partial block is cyan on the
  // track.
  win = draw(BAR_START + 20, 53, true, 53, "", false, unicode_glyphs);
  partial = read_cell(win, BAR_START + 10);
  CHECK(partial.c == partial_block(4) && partial.pair == cyan_color);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_value_over_partial_block(void) {
  // A value over a partial block takes the fill color, as the glyph it
  // replaces: 53% of 20 cells fills 10 cells and part of the 11th one.
  WINDOW *win = draw(BAR_START + 20, 53, false, 0, "aaaaaaaaaaaaaaaaaaaa", false, unicode_glyphs);
  CHECK(is_value(win, BAR_START + 10, L'a', yellow_color));
  CHECK(is_value(win, BAR_START + 11, L'a', 0));
  delwin(win);
  return EXIT_SUCCESS;
}

#endif // NVTOP_HAVE_WIDE_CURSES

struct test_case {
  const char *name;
  int (*run)(void);
};

static const struct test_case tests[] = {
    {"fill_colors", test_fill_colors},
    {"fill_colors_disabled_levels", test_fill_colors_disabled_levels},
    {"fill_whole_cells", test_fill_whole_cells},
    {"fill_clamped", test_fill_clamped},
    {"effective_overlay", test_effective_overlay},
    {"effective_clamped", test_effective_clamped},
    {"value_over_bar", test_value_over_bar},
    {"value_over_effective_fill", test_value_over_effective_fill},
    {"value_too_long", test_value_too_long},
    {"value_space", test_value_space},
    {"effective_colors", test_effective_colors},
    {"legacy", test_legacy},
    {"legacy_effective", test_legacy_effective},
    {"legacy_effective_clamped", test_legacy_effective_clamped},
#ifdef NVTOP_HAVE_WIDE_CURSES
    {"fill_eighths", test_fill_eighths},
    {"partial_block_glyphs", test_partial_block_glyphs},
    {"console_glyphs", test_console_glyphs},
    {"effective_partial_block", test_effective_partial_block},
    {"value_over_partial_block", test_value_over_partial_block},
#endif
};

int main(int argc, char **argv) {
#ifdef NVTOP_HAVE_WIDE_CURSES
  if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
    fprintf(stderr, "No UTF-8 locale available\n");
    return SKIP_RETURN_CODE;
  }
#endif
  FILE *out = fopen("/dev/null", "w");
  FILE *in = fopen("/dev/null", "r");
  SCREEN *screen = out && in ? newterm("xterm-256color", out, in) : NULL;
  if (!screen)
    screen = out && in ? newterm("xterm", out, in) : NULL;
  if (!screen) {
    fprintf(stderr, "Cannot create an ncurses screen\n");
    return SKIP_RETURN_CODE;
  }
  start_color();
  if (COLOR_PAIRS <= bar_effective_red_color) {
    fprintf(stderr, "Not enough color pairs\n");
    return SKIP_RETURN_CODE;
  }
  for (short pair = 1; pair <= bar_effective_red_color; ++pair)
    init_pair(pair, COLOR_WHITE, COLOR_BLACK);
  nvtop_meter_init_colors();

  int status = EXIT_SUCCESS;
  bool ran = false;
  for (size_t i = 0; i < sizeof(tests) / sizeof(*tests); ++i) {
    if (argc > 1 && strcmp(argv[1], tests[i].name) != 0)
      continue;
    ran = true;
    if (tests[i].run() != EXIT_SUCCESS) {
      fprintf(stderr, "FAILED: %s\n", tests[i].name);
      status = EXIT_FAILURE;
    }
  }
  if (!ran) {
    fprintf(stderr, "Unknown test: %s\n", argv[1]);
    status = EXIT_FAILURE;
  }
  endwin();
  delscreen(screen);
  fclose(out);
  fclose(in);
  return status;
}
