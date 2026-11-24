/*
 * Braille plot rendering tests: the glyphs drawn for a line, the columns each
 * metric owns (one per metric per update), and that a cell only ever shows the
 * dots of the line whose color it has.
 *
 * Plots are rendered into an off-screen ncurses window and read back cell by
 * cell. Line i is drawn with color pair 7 + i. Without the wide character
 * ncurses API, only the tests not involving the braille renderer run.
 */

#include "nvtop/plot.h"

#include <locale.h>
#include <math.h>
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
#define MAX_SAMPLES 64
#define PAIR_LINE0 7
#define PAIR_LINE1 8

static char legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE] = {"A", "B", "C", "D"};

#ifdef NVTOP_HAVE_WIDE_CURSES
static short cell_pair(WINDOW *win, int y, int x) {
  cchar_t cell;
  wchar_t text[CCHARW_MAX + 1];
  attr_t attrs;
  short pair = -1;
  mvwin_wch(win, y, x, &cell);
  getcchar(&cell, text, &attrs, &pair, NULL);
  return pair;
}

static wchar_t cell_char(WINDOW *win, int y, int x) {
  cchar_t cell;
  wchar_t text[CCHARW_MAX + 1] = {0};
  attr_t attrs;
  short pair;
  mvwin_wch(win, y, x, &cell);
  getcchar(&cell, text, &attrs, &pair, NULL);
  return text[0];
}
#else
static short cell_pair(WINDOW *win, int y, int x) { return (short)PAIR_NUMBER(mvwinch(win, y, x)); }

static wchar_t cell_char(WINDOW *win, int y, int x) { return (wchar_t)(mvwinch(win, y, x) & A_CHARTEXT); }
#endif

// Renderer independent tests, also run without the wide character API.

static int test_legacy_clipped_samples(void) {
  // The legacy renderer gets more samples than fit: the clipped ones still set
  // the level the first visible column starts from, so a line flat at 100 in
  // every visible column does not climb from the clipped samples' 0.
  enum { rows = 4, cols = 4, samples = 10 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  const double data[samples] = {0, 0, 0, 0, 0, 100, 100, 100, 100, 100};
  werase(win);
  nvtop_line_plot(win, samples, data, 1, nvtop_plot_style_line, false, NULL, legend);
  for (int y = 1; y < rows; ++y)
    CHECK(cell_char(win, y, 0) == L' ');
  // The first visible column joins the last clipped sample (67, the second
  // row) to its own (33, the third row).
  const double drop[samples] = {0, 0, 0, 0, 0, 67, 33, 33, 33, 33};
  werase(win);
  nvtop_line_plot(win, samples, drop, 1, nvtop_plot_style_line, false, NULL, legend);
  CHECK(cell_char(win, 1, 0) != L' ' && cell_char(win, 2, 0) != L' ');
  // Newest on the left, the samples past the right edge are not drawn.
  const double newest_left[samples] = {50, 50, 50, 50, 100, 0, 0, 0, 0, 0};
  werase(win);
  nvtop_line_plot(win, samples, newest_left, 1, nvtop_plot_style_line, true, NULL, legend);
  for (int x = 0; x < cols; ++x)
    CHECK(cell_char(win, 1, x) != L' ' && cell_char(win, 2, x) == L' ');
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_legacy_clipped_two_metrics(void) {
  // Two metrics in 5 columns: the oldest visible group is cut, line 0's
  // column of it is clipped. Line 0 is at 0 until that clipped sample, then
  // at 67 (the second row): its next column, the second one, is flat on the
  // second row instead of climbing from the bottom one.
  enum { rows = 4, cols = 5, samples = 10 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  double data[samples * 2];
  for (int s = 0; s < samples; ++s) {
    data[s * 2] = s < 7 ? 0 : 67;
    data[s * 2 + 1] = 0;
  }
  werase(win);
  nvtop_line_plot(win, samples, data, 2, nvtop_plot_style_line, false, NULL, legend);
  CHECK(cell_char(win, 1, 1) != L' ' && cell_char(win, 2, 1) == L' ');
  delwin(win);
  return EXIT_SUCCESS;
}

// Values outside 0..100 are drawn as the nearest bound, non-finite ones as 0.
static int check_out_of_range(enum nvtop_plot_style style) {
  enum { rows = 3, cols = 4, samples = cols + 1 };
  static const double outside[][2] = {{150, 100}, {1e300, 100}, {-5, 0}, {NAN, 0}, {INFINITY, 0}};
  WINDOW *win = newwin(rows, cols, 0, 0);
  WINDOW *reference = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  for (size_t i = 0; i < sizeof(outside) / sizeof(*outside); ++i) {
    double data[samples], expected[samples];
    for (int s = 0; s < samples; ++s) {
      data[s] = outside[i][0];
      expected[s] = outside[i][1];
    }
    werase(win);
    werase(reference);
    nvtop_line_plot(win, samples, data, 1, style, false, &state, legend);
    nvtop_line_plot(reference, samples, expected, 1, style, false, &state, legend);
    for (int y = 0; y < rows; ++y)
      for (int x = 0; x < cols; ++x)
        CHECK(cell_char(win, y, x) == cell_char(reference, y, x) && cell_pair(win, y, x) == cell_pair(reference, y, x));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  delwin(reference);
  return EXIT_SUCCESS;
}

static int test_out_of_range_values(void) {
  if (check_out_of_range(nvtop_plot_style_line) != EXIT_SUCCESS)
    return EXIT_FAILURE;
#ifdef NVTOP_HAVE_WIDE_CURSES
  if (check_out_of_range(nvtop_plot_style_braille) != EXIT_SUCCESS)
    return EXIT_FAILURE;
#endif
  return EXIT_SUCCESS;
}

static int test_legend_truncation(void) {
  // A label longer than the plot keeps its start when left aligned and its end
  // when right aligned (newest data on the left), the side next to the plot.
  enum { rows = 3, cols = 6, samples = 6 };
  char long_legend[MAX_LINES_PER_PLOT][PLOT_MAX_LEGEND_SIZE] = {"ABCDEFGHIJ"};
  const double data[samples] = {0};
  static const wchar_t *expected[2] = {L"ABCDEF", L"EFGHIJ"};
  for (int newest_left = 0; newest_left < 2; ++newest_left) {
    WINDOW *win = newwin(rows, cols, 0, 0);
    werase(win);
    nvtop_line_plot(win, samples, data, 1, nvtop_plot_style_line, newest_left, NULL, long_legend);
    for (int x = 0; x < cols; ++x)
      CHECK(cell_char(win, 0, x) == expected[newest_left][x]);
    delwin(win);
  }
  return EXIT_SUCCESS;
}

static int test_sample_counts(void) {
  CHECK(nvtop_plot_braille_samples(10, 1) == 11);
  CHECK(nvtop_plot_braille_samples(10, 3) == 5);
  CHECK(nvtop_plot_braille_samples(9, 3) == 4);
  return EXIT_SUCCESS;
}

#ifdef NVTOP_HAVE_WIDE_CURSES

// Braille renderer tests.

// Plot num_shown samples per line given in screen order, sample-major. The
// renderer also takes the sample preceding the oldest shown one, which is not
// shown; here it repeats the oldest shown sample.
static void plot_shown(WINDOW *win, struct nvtop_braille_plot *state, size_t num_shown, unsigned num_lines,
                       const double *data, bool newest_left) {
  double extended[(2 * MAX_SAMPLES + 1) * MAX_LINES_PER_PLOT];
  size_t first_shown = newest_left ? 0 : 1;
  size_t hidden = newest_left ? num_shown : 0;
  size_t oldest = newest_left ? num_shown - 1 : 0;
  for (size_t s = 0; s < num_shown; ++s)
    for (unsigned line = 0; line < num_lines; ++line)
      extended[(s + first_shown) * num_lines + line] = data[s * num_lines + line];
  for (unsigned line = 0; line < num_lines; ++line)
    extended[hidden * num_lines + line] = data[oldest * num_lines + line];
  werase(win);
  nvtop_line_plot(win, num_shown + 1, extended, num_lines, nvtop_plot_style_braille, newest_left, state, legend);
}

// Render one line and compare the braille glyphs of a one-row plot with the
// expected dot masks (0 for an empty cell). values are the shown samples, the
// hidden one repeats the oldest of them, unless with_hidden is set: values
// then include it.
static int check_glyphs_of(int cols, size_t num_values, const double *values, bool newest_left, bool with_hidden,
                           const uint8_t *expected) {
  WINDOW *win = newwin(1, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, 1, (unsigned)cols));
  werase(win);
  if (with_hidden)
    nvtop_line_plot(win, num_values, values, 1, nvtop_plot_style_braille, newest_left, &state, legend);
  else
    plot_shown(win, &state, num_values, 1, values, newest_left);
  for (int x = 0; x < cols; ++x) {
    wchar_t want = expected[x] ? (wchar_t)(0x2800 + expected[x]) : L' ';
    if (cell_char(win, 0, x) != want) {
      fprintf(stderr, "column %d: got U+%04X, expected U+%04X\n", x, (unsigned)cell_char(win, 0, x), (unsigned)want);
      return EXIT_FAILURE;
    }
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int check_glyphs(int cols, size_t num_shown, const double *values, bool newest_left, const uint8_t *expected) {
  return check_glyphs_of(cols, num_shown, values, newest_left, false, expected);
}

// One row has four dot rows: values 100, 67, 34 and 0 are on dot rows 0 to 3.
// With one sample per column, samples are on the dot column facing the newest
// data and the dots joining two samples are in the column between them. The
// oldest shown sample is joined to the hidden one (here on the same level).
static int test_glyphs_flat(void) {
  const double values[] = {0, 0, 0, 0};
  const uint8_t newest_right[] = {0xc0, 0xc0, 0xc0, 0xc0};
  const uint8_t newest_left[] = {0xc0, 0xc0, 0xc0, 0xc0};
  if (check_glyphs(4, 4, values, false, newest_right) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  return check_glyphs(4, 4, values, true, newest_left);
}

static int test_glyphs_one_dot_climbs(void) {
  // A climb of one dot keeps the joining dot on the older sample's level.
  const double rising[] = {0, 34, 67, 100};
  const uint8_t newest_right[] = {0xc0, 0x60, 0x14, 0x0a};
  if (check_glyphs(4, 4, rising, false, newest_right) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  const double rising_newest_first[] = {100, 67, 34, 0};
  const uint8_t newest_left[] = {0x11, 0x22, 0x84, 0xc0};
  return check_glyphs(4, 4, rising_newest_first, true, newest_left);
}

static int test_glyphs_steep_climbs(void) {
  // A steep climb is a vertical stroke between the two samples' dot rows.
  const double values[] = {0, 100, 100, 0};
  const uint8_t newest_right[] = {0xc0, 0x0e, 0x09, 0x86};
  const uint8_t newest_left[] = {0x70, 0x09, 0x31, 0xc0};
  if (check_glyphs(4, 4, values, false, newest_right) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  return check_glyphs(4, 4, values, true, newest_left);
}

static int test_glyphs_hidden_sample(void) {
  // The hidden sample (100) is not shown, but the climb joining it to the
  // oldest shown sample (0) is.
  const double newest_last[] = {100, 0, 0, 0, 0};
  const uint8_t newest_right[] = {0x86, 0xc0, 0xc0, 0xc0};
  if (check_glyphs_of(4, 5, newest_last, false, true, newest_right) != EXIT_SUCCESS)
    return EXIT_FAILURE;
  const double newest_first[] = {0, 0, 0, 0, 100};
  const uint8_t newest_left[] = {0xc0, 0xc0, 0xc0, 0x70};
  return check_glyphs_of(4, 5, newest_first, true, true, newest_left);
}

static wchar_t glyph(uint8_t mask) { return (wchar_t)(0x2800 + mask); }

// A non-empty braille pattern: the braille renderer drew the cell, not the
// legacy one.
static bool is_braille(WINDOW *win, int y, int x) {
  wchar_t c = cell_char(win, y, x);
  return c > 0x2800 && c <= 0x28FF;
}

// Constant lines, except for the newest sample, in screen order.
static void render_steps(WINDOW *win, struct nvtop_braille_plot *state, size_t num_shown, unsigned num_lines,
                         const double *older, const double *newest, bool newest_left) {
  double data[MAX_LINES_PER_PLOT * (2 * MAX_SAMPLES + 1)];
  for (size_t s = 0; s < num_shown; ++s) {
    bool is_newest = newest_left ? s == 0 : s == num_shown - 1;
    for (unsigned line = 0; line < num_lines; ++line)
      data[s * num_lines + line] = is_newest ? newest[line] : older[line];
  }
  plot_shown(win, state, num_shown, num_lines, data, newest_left);
}

// Each update takes one column per metric: column k of a group is metric k's.
static int check_columns_alternate(bool newest_left) {
  enum { cols = 8 };
  WINDOW *win = newwin(1, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, 1, cols));
  const double level[] = {50, 50};
  render_steps(win, &state, cols / 2, 2, level, level, newest_left);
  for (int x = 0; x < cols; ++x) {
    CHECK(cell_pair(win, 0, x) == (x % 2 ? PAIR_LINE1 : PAIR_LINE0));
    CHECK(cell_char(win, 0, x) == glyph(0x24));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_columns_alternate(void) { return check_columns_alternate(false); }
static int test_columns_alternate_newest_left(void) { return check_columns_alternate(true); }

static int test_lines_apart_keep_colors(void) {
  // In different character rows, lines continue through the other metric's
  // columns in their own color.
  enum { rows = 2, cols = 8 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double levels[] = {100, 0};
  render_steps(win, &state, cols / 2, 2, levels, levels, false);
  // The legend ("A") is in the top left cell.
  for (int x = 0; x < cols; ++x) {
    CHECK(x == 0 || (cell_pair(win, 0, x) == PAIR_LINE0 && cell_char(win, 0, x) == glyph(0x09)));
    CHECK(cell_pair(win, 1, x) == PAIR_LINE1 && cell_char(win, 1, x) == glyph(0xc0));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_shared_cell_shows_owner_only(void) {
  // Both lines in the same character row: a column only shows its metric's
  // dots, never the other line's dots in its color.
  enum { cols = 8 };
  WINDOW *win = newwin(1, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, 1, cols));
  const double levels[] = {100, 0};
  render_steps(win, &state, cols / 2, 2, levels, levels, false);
  for (int x = 0; x < cols; x += 2) {
    CHECK(cell_pair(win, 0, x) == PAIR_LINE0 && cell_char(win, 0, x) == glyph(0x09));
    CHECK(cell_pair(win, 0, x + 1) == PAIR_LINE1 && cell_char(win, 0, x + 1) == glyph(0xc0));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_free_cell_goes_to_nearest_metric(void) {
  // Four metrics, GPU and MEM at 100 on the top row, ENC and DEC at 0 on the
  // bottom one. In a column whose metric is not on a row, the metric of the
  // nearest column takes the cell, so each pair splits its row evenly.
  enum { rows = 2, cols = 8 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double levels[] = {100, 100, 0, 0};
  render_steps(win, &state, cols / 4, 4, levels, levels, false);
  static const short top[4] = {PAIR_LINE0, PAIR_LINE1, PAIR_LINE1, PAIR_LINE0};
  static const short bottom[4] = {PAIR_LINE0 + 3, PAIR_LINE0 + 2, PAIR_LINE0 + 2, PAIR_LINE0 + 3};
  for (int x = 0; x < cols; ++x) {
    // The legend ("A") is in the top left cell.
    CHECK(x == 0 || (cell_pair(win, 0, x) == top[x % 4] && is_braille(win, 0, x)));
    CHECK(cell_pair(win, 1, x) == bottom[x % 4] && is_braille(win, 1, x));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_column_metric_keeps_its_cells(void) {
  // Two rows of four dots. Line 0 drops from 100 (dot row 0) onto line 1's
  // level, dot row 4. The bottom cell of line 0's newest column still shows
  // line 0, so its sample is not drawn as line 1's, and as it lands on line 1
  // the joining dot column ends on that level too: line 1 has no notch.
  enum { rows = 2, cols = 4 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double older[] = {100, 43};
  const double newest[] = {43, 43};
  render_steps(win, &state, cols / 2, 2, older, newest, false);
  CHECK(cell_pair(win, 1, 2) == PAIR_LINE0 && cell_char(win, 1, 2) == glyph(0x09));
  CHECK(cell_pair(win, 0, 2) == PAIR_LINE0 && cell_char(win, 0, 2) == glyph(0x46));
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_climb_crosses_flat_line(void) {
  // Line 0 jumps from 0 to 100 through line 1, flat at 50. In line 0's column
  // the climb is drawn, in its color; line 1 has a gap there.
  enum { rows = 3, cols = 8 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double older[] = {0, 50};
  const double newest[] = {100, 50};
  render_steps(win, &state, cols / 2, 2, older, newest, false);
  // Twelve dot rows: line 1 is on dot row 6, the second row's third one. The
  // climb fills the left dot column of the middle row.
  CHECK(cell_pair(win, 1, cols - 2) == PAIR_LINE0);
  CHECK(cell_char(win, 1, cols - 2) == glyph(0x47));
  CHECK(cell_pair(win, 1, cols - 1) == PAIR_LINE1);
  CHECK(cell_char(win, 1, cols - 1) == glyph(0x24));
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_climb_leaves_other_line(void) {
  // Two rows of four dots. Line 0 climbs from 0 (dot row 7) to 100 while line
  // 1 stays at 0. Line 0's stroke would start one dot above line 1, but it
  // leaves from line 1's level: its foot is drawn there, attached to it.
  enum { rows = 2, cols = 4 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double older[] = {0, 0};
  const double newest[] = {100, 0};
  render_steps(win, &state, cols / 2, 2, older, newest, false);
  CHECK(cell_pair(win, 1, cols - 2) == PAIR_LINE0 && cell_char(win, 1, cols - 2) == glyph(0x47));
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

// Other metrics continue through a column at their level when it comes in
// time: before their own column of the update, the previous one; after it, the
// new one. Two rows of four dots: line 0 is flat at 100, line 1 climbs from 0
// (dot row 7) to 40 (dot row 4) in the newest update.
static int test_continuation_level(void) {
  enum { rows = 2, cols = 4 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double older[] = {100, 0};
  const double newest[] = {100, 40};
  // Newest on the right: line 0's column comes first, line 1 is still at 0.
  render_steps(win, &state, cols / 2, 2, older, newest, false);
  CHECK(cell_pair(win, 1, 2) == PAIR_LINE1 && cell_char(win, 1, 2) == glyph(0xc0));
  CHECK(cell_pair(win, 1, 3) == PAIR_LINE1 && cell_char(win, 1, 3) == glyph(0x0e));
  CHECK(cell_pair(win, 0, 3) == PAIR_LINE0 && cell_char(win, 0, 3) == glyph(0x09));
  // Newest on the left: line 1's column (further from the newest data) comes
  // first, so line 1 is already at 40 in line 0's column.
  render_steps(win, &state, cols / 2, 2, older, newest, true);
  CHECK(cell_pair(win, 1, 0) == PAIR_LINE1 && cell_char(win, 1, 0) == glyph(0x09));
  CHECK(cell_pair(win, 1, 1) == PAIR_LINE1 && cell_char(win, 1, 1) == glyph(0x31));
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int check_partial_group(bool newest_left) {
  // Five columns for two metrics: the oldest update only has one visible
  // column, its second metric's (newest right) or its first one's (left).
  enum { cols = 5 };
  WINDOW *win = newwin(1, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, 1, cols));
  const double level[] = {50, 50};
  render_steps(win, &state, 3, 2, level, level, newest_left);
  for (int x = 0; x < cols; ++x) {
    int line = newest_left ? x % 2 : (x + 1) % 2;
    CHECK(cell_pair(win, 0, x) == (line ? PAIR_LINE1 : PAIR_LINE0) && is_braille(win, 0, x));
  }
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_partial_group(void) { return check_partial_group(false); }
static int test_partial_group_newest_left(void) { return check_partial_group(true); }

static int test_three_metrics(void) {
  enum { cols = 9 };
  WINDOW *win = newwin(1, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, 1, cols));
  const double level[] = {50, 50, 50};
  render_steps(win, &state, cols / 3, 3, level, level, false);
  for (int x = 0; x < cols; ++x)
    CHECK(cell_pair(win, 0, x) == PAIR_LINE0 + x % 3 && is_braille(win, 0, x));
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int check_legend(bool newest_left) {
  // The legend is on the side of the oldest data, in the line's color.
  enum { rows = 3, cols = 20 };
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, rows, cols));
  const double level[] = {0, 0};
  render_steps(win, &state, cols / 2, 2, level, level, newest_left);
  int label_x = newest_left ? cols - 1 : 0;
  CHECK(cell_char(win, 0, label_x) == L'A' && cell_pair(win, 0, label_x) == PAIR_LINE0);
  CHECK(cell_char(win, 1, label_x) == L'B' && cell_pair(win, 1, label_x) == PAIR_LINE1);
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int test_legend(void) { return check_legend(false); }
static int test_legend_newest_left(void) { return check_legend(true); }

// Feed random lines to the renderer the way the interface does: after each new
// sample, the graph is rebuilt from the newest samples, plus the hidden one
// preceding them. Each update moves the graph by one column per metric: all
// the other columns must keep their glyph and color, only the newest group is
// new. The legend corner is skipped. Checked for several numbers of metrics
// and plot sizes, widths that are not a multiple of the number of metrics
// included (the oldest group is then only partially visible).
enum { max_stable_rows = 5, max_stable_cols = 40, stable_steps = 400, legend_width = 12 };

struct frame {
  wchar_t glyph[max_stable_rows][max_stable_cols];
  short pair[max_stable_rows][max_stable_cols];
};

static void snapshot(WINDOW *win, int rows, int cols, struct frame *frame) {
  for (int y = 0; y < rows; ++y)
    for (int x = 0; x < cols; ++x) {
      frame->glyph[y][x] = cell_char(win, y, x);
      frame->pair[y][x] = cell_pair(win, y, x);
    }
}

// The legend takes one row per line, the bottom row excepted.
static bool in_legend(int y, int x, unsigned lines, int rows, int cols, bool newest_left) {
  int from_oldest = newest_left ? cols - 1 - x : x;
  return y < (int)lines && y < rows - 1 && from_oldest < legend_width;
}

static unsigned next_random(unsigned *seed) {
  *seed = *seed * 1103515245u + 12345u;
  return (*seed >> 16) & 0x7fff;
}

static double clamp_value(double value) { return value < 0 ? 0 : value > 100 ? 100 : value; }

static int check_scroll_stability(unsigned lines, int rows, int cols, bool newest_left) {
  size_t num_samples = nvtop_plot_braille_samples((unsigned)cols, lines);
  WINDOW *win = newwin(rows, cols, 0, 0);
  struct nvtop_braille_plot state;
  CHECK(nvtop_braille_plot_init(&state, (unsigned)rows, (unsigned)cols));
  static double history[stable_steps][MAX_LINES_PER_PLOT];
  unsigned seed = 42;
  double base = 50;
  for (int t = 0; t < stable_steps; ++t) {
    // Mostly small moves, with an occasional steep jump.
    if (next_random(&seed) % 8 == 0)
      base = clamp_value(base + (double)((int)(next_random(&seed) % 121) - 60));
    else
      base = clamp_value(base + (double)((int)(next_random(&seed) % 21) - 10));
    for (unsigned line = 0; line < lines; ++line) {
      // Mostly on the same level, sometimes slightly or clearly apart.
      static const double offsets[] = {0, 0, 0, 0, 3, -3, 12, -12};
      history[t][line] = clamp_value(base + offsets[next_random(&seed) % 8]);
    }
  }

  static struct frame previous;
  static struct frame current;
  double data[(max_stable_cols + 1) * MAX_LINES_PER_PLOT];
  int compared = 0;
  for (int t = 0; t < stable_steps; ++t) {
    memset(data, 0, sizeof(data));
    for (size_t j = 0; j < num_samples && (int)j <= t; ++j) {
      size_t index = newest_left ? j : num_samples - 1 - j;
      for (unsigned line = 0; line < lines; ++line)
        data[index * lines + line] = history[t - (int)j][line];
    }
    werase(win);
    nvtop_line_plot(win, num_samples, data, lines, nvtop_plot_style_braille, newest_left, &state, legend);
    snapshot(win, rows, cols, &current);

    // Compare with the previous frame, one group of columns earlier.
    if (t > 0) {
      int shift = newest_left ? (int)lines : -(int)lines;
      for (int y = 0; y < rows; ++y) {
        for (int x = 0; x < cols; ++x) {
          int old_x = x - shift;
          // Columns of the newest group, or only partially visible before.
          bool newest = newest_left ? x < (int)lines : x >= cols - (int)lines;
          if (newest || old_x < 0 || old_x >= cols || in_legend(y, x, lines, rows, cols, newest_left) ||
              in_legend(y, old_x, lines, rows, cols, newest_left))
            continue;
          if (current.glyph[y][x] != previous.glyph[y][old_x] || current.pair[y][x] != previous.pair[y][old_x]) {
            fprintf(stderr, "%u lines, %dx%d, t=%d row %d col %d: cell changed after scrolling\n", lines, rows, cols, t,
                    y, x);
            return EXIT_FAILURE;
          }
          compared++;
        }
      }
    }
    previous = current;
  }
  // The comparison must actually have covered cells.
  CHECK(compared >= stable_steps);
  nvtop_braille_plot_free(&state);
  delwin(win);
  return EXIT_SUCCESS;
}

static int check_scroll_stability_geometries(bool newest_left) {
  static const struct {
    unsigned lines;
    int rows, cols;
  } geometries[] = {{1, 2, 9}, {2, 4, 23}, {2, 1, 5}, {3, 3, 24}, {3, 5, 26}, {4, 3, 25}, {4, 2, 23}, {4, 4, 40}};
  for (size_t i = 0; i < sizeof(geometries) / sizeof(*geometries); ++i)
    if (check_scroll_stability(geometries[i].lines, geometries[i].rows, geometries[i].cols, newest_left) !=
        EXIT_SUCCESS)
      return EXIT_FAILURE;
  return EXIT_SUCCESS;
}

static int test_scroll_stability(void) { return check_scroll_stability_geometries(false); }
static int test_scroll_stability_newest_left(void) { return check_scroll_stability_geometries(true); }

#endif // NVTOP_HAVE_WIDE_CURSES

struct test_case {
  const char *name;
  int (*run)(void);
};

static const struct test_case tests[] = {
    {"legacy_clipped_samples", test_legacy_clipped_samples},
    {"legacy_clipped_two_metrics", test_legacy_clipped_two_metrics},
    {"out_of_range_values", test_out_of_range_values},
    {"legend_truncation", test_legend_truncation},
    {"sample_counts", test_sample_counts},
#ifdef NVTOP_HAVE_WIDE_CURSES
    {"glyphs_flat", test_glyphs_flat},
    {"glyphs_one_dot_climbs", test_glyphs_one_dot_climbs},
    {"glyphs_steep_climbs", test_glyphs_steep_climbs},
    {"glyphs_hidden_sample", test_glyphs_hidden_sample},
    {"columns_alternate", test_columns_alternate},
    {"columns_alternate_newest_left", test_columns_alternate_newest_left},
    {"lines_apart_keep_colors", test_lines_apart_keep_colors},
    {"shared_cell_shows_owner_only", test_shared_cell_shows_owner_only},
    {"free_cell_goes_to_nearest_metric", test_free_cell_goes_to_nearest_metric},
    {"column_metric_keeps_its_cells", test_column_metric_keeps_its_cells},
    {"climb_crosses_flat_line", test_climb_crosses_flat_line},
    {"climb_leaves_other_line", test_climb_leaves_other_line},
    {"continuation_level", test_continuation_level},
    {"partial_group", test_partial_group},
    {"partial_group_newest_left", test_partial_group_newest_left},
    {"three_metrics", test_three_metrics},
    {"legend", test_legend},
    {"legend_newest_left", test_legend_newest_left},
    {"scroll_stability", test_scroll_stability},
    {"scroll_stability_newest_left", test_scroll_stability_newest_left},
#endif
};

int main(int argc, char **argv) {
#ifdef NVTOP_HAVE_WIDE_CURSES
  if (!setlocale(LC_ALL, "C.UTF-8") && !setlocale(LC_ALL, "en_US.UTF-8")) {
    fprintf(stderr, "No UTF-8 locale available\n");
    return SKIP_RETURN_CODE;
  }
#endif
  // Run on its own (the capability is only checked once): no braille on the
  // Linux console.
  if (argc > 1 && strcmp(argv[1], "linux_console") == 0) {
    setenv("TERM", "linux", 1);
    return nvtop_plot_supports_braille() ? EXIT_FAILURE : EXIT_SUCCESS;
  }
#ifdef NVTOP_HAVE_WIDE_CURSES
  // The plots are rendered for the terminal type given to newterm below, not
  // the one the tests run in (braille is off on the Linux console).
  setenv("TERM", "xterm-256color", 1);
  if (!nvtop_plot_supports_braille()) {
    fprintf(stderr, "Braille plots are not supported with this locale\n");
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
  for (short pair = PAIR_LINE0; pair < PAIR_LINE0 + MAX_LINES_PER_PLOT; ++pair)
    init_pair(pair, COLOR_WHITE, COLOR_BLACK);

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
