/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
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
 * Author: Thomas E. Dickey
 *
 * $Id: demo_tabs.c,v 1.5 2020/02/02 23:34:34 tom Exp $
 *
 * A simple demo of tabs in curses.
 */
#define USE_TINFO
#include "test.priv.h"

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: demo_tabs [options]",
	"",
	"Print a grid to test tab-stops with the curses interface",
	"",
	"Options:",
	" -l COUNT total number of lines to show",
	" -t NUM   set TABSIZE variable to the given value",
    };
    unsigned n;
    for (n = 0; n < SIZEOF(msg); ++n) {
	fprintf(stderr, "%s\n", msg[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int tabstop;
    int n, col, row, step;
    int line_limit = -1;
    int curses_stops = -1;

    while ((n = getopt(argc, argv, "l:t:")) != -1) {
	switch (n) {
	case 'l':
	    line_limit = atoi(optarg);
	    break;
	case 't':
	    curses_stops = atoi(optarg);
	    break;
	default:
	    usage();
	    break;
	}
    }

    initscr();
    noecho();
    cbreak();
    if (curses_stops > 0)
	set_tabsize(curses_stops);
#if HAVE_TIGETNUM
    tabstop = tigetnum("it");
    if (tabstop <= 0)
#endif
	tabstop = 8;
    for (row = 0; row < LINES; ++row) {
	move(row, 0);
	for (col = step = 0; col < COLS - 1; ++col) {
	    if (row == 0) {
		chtype ch = '-';
		if ((col % tabstop) == 0)
		    ch = '+';
		addch(ch);
	    } else if (col + 1 < row) {
		addch('*');
	    } else {
		printw("%x", step);
		col = (row + (tabstop * ++step));
		col /= tabstop;
		col *= tabstop;
		col -= 1;
		if ((col + tabstop) < COLS)
		    addch('\t');
		refresh();
	    }
	}
	addch('\n');
	if (line_limit > 0 && row >= line_limit)
	    break;
    }
    getch();
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
