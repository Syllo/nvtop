/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2006-2012,2017 Free Software Foundation, Inc.                  *
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
 * $Id: redraw.c,v 1.13 2021/06/17 21:26:02 tom Exp $
 *
 * Demonstrate the redrawwin() and wredrawln() functions.
 * Thomas Dickey - 2006/11/4
 */

#include <test.priv.h>
#include <popup_msg.h>

static void
trash(int beg_x, int max_x, int cur_x)
{
    int x;

    for (x = cur_x; x > beg_x; --x) {
	putchar('\b');
    }
    for (x = beg_x; x < max_x; ++x) {
	if (x < cur_x)
	    putchar('<');
	else if (x == cur_x)
	    putchar('=');
	else if (x > cur_x)
	    putchar('>');
    }
    for (x = max_x; x > cur_x; --x) {
	putchar('\b');
    }
    fflush(stdout);
}

static void
test_redraw(WINDOW *win)
{
    static const char *help[] =
    {
	"Commands:",
	"  ^Q/ESC/q   - quit",
	"  w          - recur in a new window",
	"  !          - overwrite current line using stdio outside curses.",
#ifdef NCURSES_VERSION
	"  @          - run \"date\" command, to put its output on screen.",
#endif
	"  ^L         - call redrawwin() for current window.",
	"  ^W         - call wredrawln() for current line/current window.",
	"  arrow-keys - move cursor on the screen",
	"",
	"Other control characters are added to the screen in printable form.",
	"Other printable characters are added to the screen as is.",
	0
    };

    WINDOW *win1;
    WINDOW *win2;
    bool done = FALSE;
    int max_y, max_x;
    int beg_y, beg_x;

    assert(win != 0);

    scrollok(win, TRUE);
    keypad(win, TRUE);
    getmaxyx(win, max_y, max_x);
    getbegyx(win, beg_y, beg_x);

    while (!done) {
	int ch = wgetch(win);
	int y, x;

	getyx(win, y, x);
	switch (ch) {
	case 'q':
	    /* FALLTHRU */
	case QUIT:
	case ESCAPE:
	    done = TRUE;
	    break;
	case 'w':
	    win1 = newwin(max_y, max_x,
			  beg_y, beg_x);
	    win2 = newwin(max_y - 2, max_x - 2,
			  beg_y + 1, beg_x + 1);
	    box(win1, 0, 0);
	    wrefresh(win1);

	    test_redraw(win2);

	    delwin(win2);
	    delwin(win1);

	    touchwin(win);
	    break;

	case '!':
	    /*
	     * redrawwin() and wredrawln() do not take into account the
	     * possibility that the cursor may have moved.  That makes them
	     * cumbersome for using with a shell command.  So we simply
	     * trash the current line of the window using backspace/overwrite.
	     */
	    trash(beg_x, max_x, x + beg_x);
	    break;

#ifdef NCURSES_VERSION
	case '@':
	    /*
	     * For a shell command, we can work around the problem noted above
	     * using mvcur().  It is ifdef'd for NCURSES, since X/Open does
	     * not define the case where the old location is unknown.
	     */
	    IGNORE_RC(system("date"));
	    mvcur(-1, -1, y, x);
	    break;
#endif

	case CTRL('W'):
	    redrawwin(win);
	    break;

	case CTRL('L'):
	    wredrawln(win, y, 1);
	    break;

	case KEY_UP:
	    if (y > 0)
		wmove(win, y - 1, x);
	    break;

	case KEY_DOWN:
	    if (y < max_y)
		wmove(win, y + 1, x);
	    break;

	case KEY_LEFT:
	    if (x > 0)
		wmove(win, y, x - 1);
	    break;

	case KEY_RIGHT:
	    if (x < max_x)
		wmove(win, y, x + 1);
	    break;

	case HELP_KEY_1:
	    popup_msg(win, help);
	    break;

	default:
	    if (ch > KEY_MIN) {
		waddstr(win, keyname(ch));
		waddch(win, '\n');
	    } else {
		waddstr(win, unctrl(UChar(ch)));
	    }
	    break;
	}
	wnoutrefresh(win);
	doupdate();
    }
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: redraw [options]"
	,""
	,"Options:"
	,"  -e      use stderr (default stdout)"
	,"  -n      do not initialize terminal"
    };
    unsigned n;
    for (n = 0; n < SIZEOF(tbl); ++n)
	fprintf(stderr, "%s\n", tbl[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int ch;
    bool no_init = FALSE;
    FILE *my_fp = stdout;

    while ((ch = getopt(argc, argv, "en")) != -1) {
	switch (ch) {
	case 'e':
	    my_fp = stderr;
	    break;
	case 'n':
	    no_init = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }
    if (optind < argc)
	usage();

    if (no_init) {
	START_TRACE();
    } else {
	newterm((char *) 0, my_fp, stdin);
    }

    raw();
    noecho();
    test_redraw(stdscr);
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
