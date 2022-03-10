/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 1998-2014,2017 Free Software Foundation, Inc.                  *
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
 * $Id: firework.c,v 1.37 2020/02/02 23:34:34 tom Exp $
 */
#include <test.priv.h>

#include <time.h>

static short my_bg = COLOR_BLACK;

static void
cleanup(void)
{
    stop_curses();
}

static void
onsig(int n GCC_UNUSED)
{
    cleanup();
    ExitProgram(EXIT_FAILURE);
}

static void
showit(void)
{
    int ch;
    napms(120);
    if ((ch = getch()) != ERR) {
#ifdef KEY_RESIZE
	if (ch == KEY_RESIZE) {
	    erase();
	} else
#endif
	if (ch == 'q') {
	    cleanup();
	    ExitProgram(EXIT_SUCCESS);
	} else if (ch == 's') {
	    nodelay(stdscr, FALSE);
	} else if (ch == ' ') {
	    nodelay(stdscr, TRUE);
	}
    }
}

static short
get_colour(chtype *bold)
{
    int attr;
    attr = (rand() % 16) + 1;

    *bold = A_NORMAL;
    if (attr > 8) {
	*bold = A_BOLD;
	attr &= 7;
    }
    return (short) (attr);
}

static
void
explode(int row, int col)
{
    chtype bold;
    erase();
    MvPrintw(row, col, "-");
    showit();

    init_pair(1, get_colour(&bold), my_bg);
    (void) attrset(AttrArg(COLOR_PAIR(1), bold));
    MvPrintw(row - 1, col - 1, " - ");
    MvPrintw(row + 0, col - 1, "-+-");
    MvPrintw(row + 1, col - 1, " - ");
    showit();

    init_pair(1, get_colour(&bold), my_bg);
    (void) attrset(AttrArg(COLOR_PAIR(1), bold));
    MvPrintw(row - 2, col - 2, " --- ");
    MvPrintw(row - 1, col - 2, "-+++-");
    MvPrintw(row + 0, col - 2, "-+#+-");
    MvPrintw(row + 1, col - 2, "-+++-");
    MvPrintw(row + 2, col - 2, " --- ");
    showit();

    init_pair(1, get_colour(&bold), my_bg);
    (void) attrset(AttrArg(COLOR_PAIR(1), bold));
    MvPrintw(row - 2, col - 2, " +++ ");
    MvPrintw(row - 1, col - 2, "++#++");
    MvPrintw(row + 0, col - 2, "+# #+");
    MvPrintw(row + 1, col - 2, "++#++");
    MvPrintw(row + 2, col - 2, " +++ ");
    showit();

    init_pair(1, get_colour(&bold), my_bg);
    (void) attrset(AttrArg(COLOR_PAIR(1), bold));
    MvPrintw(row - 2, col - 2, "  #  ");
    MvPrintw(row - 1, col - 2, "## ##");
    MvPrintw(row + 0, col - 2, "#   #");
    MvPrintw(row + 1, col - 2, "## ##");
    MvPrintw(row + 2, col - 2, "  #  ");
    showit();

    init_pair(1, get_colour(&bold), my_bg);
    (void) attrset(AttrArg(COLOR_PAIR(1), bold));
    MvPrintw(row - 2, col - 2, " # # ");
    MvPrintw(row - 1, col - 2, "#   #");
    MvPrintw(row + 0, col - 2, "     ");
    MvPrintw(row + 1, col - 2, "#   #");
    MvPrintw(row + 2, col - 2, " # # ");
    showit();
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: firework [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors, repeat to use in init_pair"
#endif
    };
    size_t n;

    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);

    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int ch;
    int start, end;
    int row, diff;
    int flag = 0;
    int direction;
    unsigned seed;
#if HAVE_USE_DEFAULT_COLORS
    bool d_option = FALSE;
#endif

    while ((ch = getopt(argc, argv, "d")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
	default:
	    usage();
	}
    }
    if (optind < argc)
	usage();

    InitAndCatch(initscr(), onsig);
    noecho();
    cbreak();
    keypad(stdscr, TRUE);
    nodelay(stdscr, TRUE);

    if (has_colors()) {
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() == OK))
	    my_bg = -1;
#endif
    }
    curs_set(0);

    seed = (unsigned) time((time_t *) 0);
    srand(seed);
    for (;;) {
	do {
	    start = rand() % (COLS - 3);
	    end = rand() % (COLS - 3);
	    start = (start < 2) ? 2 : start;
	    end = (end < 2) ? 2 : end;
	    direction = (start > end) ? -1 : 1;
	    diff = abs(start - end);
	} while (diff < 2 || diff >= LINES - 2);
	(void) attrset(AttrArg(0, A_NORMAL));
	for (row = 1; row < diff; row++) {
	    MvPrintw(LINES - row, start + (row * direction),
		     (direction < 0) ? "\\" : "/");
	    if (flag++) {
		showit();
		erase();
		flag = 0;
	    }
	}
	if (flag++) {
	    showit();
	    flag = 0;
	}
	seed = (unsigned) time((time_t *) 0);
	srand(seed);
	explode(LINES - row, start + (diff * direction));
	erase();
	showit();
    }
}
