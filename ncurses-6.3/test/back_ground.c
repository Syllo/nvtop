/****************************************************************************
 * Copyright 2021 Thomas E. Dickey                                          *
 *                                                                          *
 * Permission is hereby granted, free of charge, to any person obtaining a  *
 * copy of this software and associated documentation files (the            *
 * "Software"), to deal in the Software without restriction, including      *
 * without limitation the rights to use, copy, modify, merge, publish,      *
 * distribute, distribute with modifications, sublicense, and/or sell       *
 * copies of the Software, and to permit persons to whom the Software is    *
 * furnished to do so, subject to the following conditions:                 *
 *                                                                          *
 * The above copyright notice and this permission notice shall be included  *
 * in all copies or substantial portions of the Software.                   *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
 * IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
 *                                                                          *
 * Except as contained in this notice, the name(s) of the above copyright   *
 * holders shall not be used in advertising or otherwise to promote the     *
 * sale, use or other dealings in this Software without prior written       *
 * authorization.                                                           *
 ****************************************************************************/
/*
 * $Id: back_ground.c,v 1.5 2021/02/20 12:23:21 tom Exp $
 */

#include <test.priv.h>

#if USE_WIDEC_SUPPORT

#define NEED_COLOR_CODE 1
#define NEED_COLOR_NAME 1
#include <color_name.h>
#include <dump_window.h>

static int default_bg = COLOR_BLACK;
static int default_fg = COLOR_WHITE;
static wchar_t wide_fill = L' ';

static wchar_t
decode_wchar(const char *value)
{
    long result;
    char *next = NULL;
    int radix = 0;

    if (!strncmp(value, "U+", 2)) {
	value += 2;
	radix = 16;
    }
    result = strtol(value, &next, radix);
    if (next == value || (next == NULL || *next != '\0')) {
	fprintf(stderr, "decoding wchar_t: %s\n", value);
	exit(EXIT_FAILURE);
    }
    return (wchar_t) result;
}

static void
test_background(void)
{
    NCURSES_COLOR_T f, b;
    int row;
    int chr;
    wchar_t blank[2];
    wchar_t graphics[2];
    cchar_t data;

    if (pair_content(0, &f, &b) == ERR) {
	printw("pair 0 contains no data\n");
    } else {
	printw("pair 0 contains (%d,%d)\n", (int) f, (int) b);
    }
    dump_window(stdscr);

    blank[0] = wide_fill;
    blank[1] = L'\0';

    printw("Initializing pair 1 to red/%s\n", color_name(default_bg));
    init_pair(1, COLOR_RED, (NCURSES_COLOR_T) default_bg);
    setcchar(&data, blank, A_NORMAL, 1, NULL);
    bkgrndset(&data);
    printw("RED/BLACK\n");
    dump_window(stdscr);

    printw("Initializing pair 2 to %s/blue\n", color_name(default_fg));
    init_pair(2, (NCURSES_COLOR_T) default_fg, COLOR_BLUE);
    setcchar(&data, blank, A_NORMAL, 2, NULL);
    bkgrndset(&data);
    printw("This line should be %s/blue\n", color_name(default_fg));
    dump_window(stdscr);

    printw("Initializing pair 3 to %s/cyan (ACS_HLINE)\n", color_name(default_fg));
    init_pair(3, (NCURSES_COLOR_T) default_fg, COLOR_CYAN);
    printw("...and drawing a box which should be followed by lines\n");
    graphics[0] = ACS_HLINE & A_CHARTEXT;
    graphics[1] = L'\0';
    setcchar(&data, graphics, A_ALTCHARSET, 3, NULL);
    bkgrndset(&data);
    /*
     * Characters from vt100 line-drawing should be mapped to line-drawing,
     * since A_ALTCHARSET is set in the background, and the character part
     * of the background is replaced by the nonblank characters written.
     *
     * Characters not in the line-drawing range are usually sent as-is.
     *
     * With SVr4 curses it is possible to rely on this to mix uppercase text
     * with the (lowercase) line-drawing characters.  ncurses uses some of
     * the uppercase characters for encoding thick- and double-lines.
     */
    row = 7;
    mvprintw(row++, 10, "l");
    for (chr = 0; chr < 32; ++chr)
	AddCh(' ');
    printw("x\n");
    chr = 32;
    while (chr < 128) {
	if ((chr % 32) == 0)
	    mvprintw(row++, 10, "x");
	AddCh((chr == 127) ? ' ' : chr);
	if ((++chr % 32) == 0)
	    printw("x\n");
    }
    mvprintw(row++, 10, "m");
    for (chr = 0; chr < 32; ++chr)
	AddCh(' ');
    printw("j\n");
    dump_window(stdscr);

    setcchar(&data, blank, A_NORMAL, 0, NULL);
    bkgrndset(&data);
    printw("Default Colors\n");
    dump_window(stdscr);

    printw("Resetting colors to pair 1\n");
    setcchar(&data, blank, A_NORMAL, 1, NULL);
    bkgrndset(&data);
    printw("This line should be red/%s\n", color_name(default_bg));
    dump_window(stdscr);

    printw("Setting screen to pair 0\n");
    setcchar(&data, blank, A_NORMAL, 0, NULL);
    bkgrndset(&data);
    dump_window(stdscr);

    printw("Setting screen to pair 1\n");
    setcchar(&data, blank, A_NORMAL, 1, NULL);
    bkgrndset(&data);
    dump_window(stdscr);

    printw("Setting screen to pair 2\n");
    setcchar(&data, blank, A_NORMAL, 2, NULL);
    bkgrndset(&data);
    dump_window(stdscr);

    printw("Setting screen to pair 3\n");
    setcchar(&data, blank, A_NORMAL, 3, NULL);
    bkgrndset(&data);
    dump_window(stdscr);

    printw("Setting screen to pair 0\n");
    setcchar(&data, blank, A_NORMAL, 0, NULL);
    bkgrndset(&data);
    dump_window(stdscr);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: background [options]"
	,""
	,"Options:"
#if HAVE_ASSUME_DEFAULT_COLORS
	," -a       invoke assume_default_colors, repeat to use in init_pair"
#endif
	," -b XXX   specify background color"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors, repeat to use in init_pair"
#endif
	," -f XXX   specify foreground color"
	," -l FILE  log window-dumps to this file"
	," -w       fill background with stipple pattern"
	," -W CODE  fill background with this Unicode value"
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
#if HAVE_ASSUME_DEFAULT_COLORS
    int a_option = 0;
#endif
#if HAVE_USE_DEFAULT_COLORS
    int d_option = 0;
#endif
    int n;

    setlocale(LC_ALL, "");

    while ((n = getopt(argc, argv, "ab:df:l:wW:")) != -1) {
	switch (n) {
#if HAVE_ASSUME_DEFAULT_COLORS
	case 'a':
	    ++a_option;
	    break;
#endif
	case 'b':
	    default_bg = color_code(optarg);
	    break;
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    ++d_option;
	    break;
#endif
	case 'f':
	    default_fg = color_code(optarg);
	    break;
	case 'l':
	    if (!open_dump(optarg))
		usage();
	    break;
	case 'w':
	    wide_fill = L'\u2591';
	    break;
	case 'W':
	    wide_fill = decode_wchar(optarg);
	    break;
	default:
	    usage();
	}
    }
#if HAVE_USE_DEFAULT_COLORS && HAVE_ASSUME_DEFAULT_COLORS
    if (a_option && d_option) {
	fprintf(stderr, "Use either -a or -d option, but not both\n");
	ExitProgram(EXIT_FAILURE);
    }
#endif

    initscr();
    cbreak();
    noecho();

    if (has_colors()) {
	start_color();

#if HAVE_USE_DEFAULT_COLORS
	if (d_option) {
	    printw("Using default colors...\n");
	    use_default_colors();
	    if (d_option > 1) {
		default_fg = -1;
		default_bg = -1;
	    }
	}
#endif
#if HAVE_ASSUME_DEFAULT_COLORS
	if (a_option) {
	    printw("Using assumed colors %s/%s...\n",
		   color_name(default_fg),
		   color_name(default_bg));
	    assume_default_colors(default_fg, default_bg);
	    if (a_option > 1) {
		default_fg = -1;
		default_bg = -1;
	    }
	}
#endif

	test_background();

    } else {
	printw("This demo requires a color terminal");
	getch();
    }
    endwin();
    close_dump();
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the wide-curses library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif /* USE_WIDEC_SUPPORT */
