/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 2008-2014,2017 Free Software Foundation, Inc.                  *
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
 * $Id: insdelln.c,v 1.14 2020/02/02 23:34:34 tom Exp $
 *
 * test-driver for deleteln, wdeleteln, insdelln, winsdelln, insertln, winsertln
 */

#include <test.priv.h>

#if HAVE_WINSDELLN

#include <popup_msg.h>

#define SHOW(n) ((n) == ERR ? "ERR" : "OK")
#define COLOR_DEFAULT (-1)

typedef struct {
    unsigned c;
    unsigned v;
    int pair;
    unsigned attr;
    int count;
    int ch;
    const char *c_msg;
    const char *v_msg;
    int y_val;
    int x_val;
    int y_beg, x_beg;
    int y_max, x_max;
} STATUS;

static const char *
color_params(unsigned state, int *pair)
{
    /* *INDENT-OFF* */
    static struct {
	short pair;
	short fg, bg;
	const char *msg;
    } table[] = {
	{ 0, COLOR_DEFAULT, COLOR_DEFAULT, "default" },
	{ 1, COLOR_RED,     COLOR_BLACK,   "red/black" },
	{ 2, COLOR_WHITE,   COLOR_BLUE,    "white/blue" },
    };
    /* *INDENT-ON* */

    const char *result = 0;

    if (has_colors()) {
	static bool first = TRUE;
	if (first) {
	    unsigned n;

	    start_color();
	    for (n = 0; n < SIZEOF(table); ++n) {
		init_pair(table[n].pair, table[n].fg, table[n].bg);
	    }
	}
	if (state < SIZEOF(table)) {
	    *pair = table[state].pair;
	    result = table[state].msg;
	}
    }
    return result;
}

static const char *
video_params(unsigned state, unsigned *attr)
{
    /* *INDENT-OFF* */
    static struct {
	unsigned attr;
	const char *msg;
    } table[] = {
	{ A_NORMAL,	"normal" },
	{ A_BOLD,	"bold" },
	{ A_REVERSE,	"reverse" },
	{ A_UNDERLINE,	"underline" },
	{ A_BLINK, 	"blink" },
    };
    /* *INDENT-ON* */

    const char *result = 0;

    if (state < SIZEOF(table)) {
	*attr = table[state].attr;
	result = table[state].msg;
    }
    return result;
}

/* fill the window with a test-pattern */
static void
fill_window(WINDOW *win)
{
    int y, x;
    int y0 = -1, x0 = -1;

    getyx(win, y, x);
    wmove(win, 0, 0);
    while (waddstr(win, "0123456789 abcdefghijklmnopqrstuvwxyz ") != ERR) {
	int y1, x1;
	getyx(win, y1, x1);
	if (y1 == y0 && x1 == x0)
	    break;
	x0 = x1;
	y0 = y1;
    }
    wmove(win, y, x);
}

static void
show_status(WINDOW *win, STATUS * sp)
{
    int y, x;

    getyx(win, y, x);
    wmove(win, 0, 0);
    wprintw(win, "Count %d", sp->count);
    if (sp->v_msg != 0)
	wprintw(win, " Video %s", sp->v_msg);
    if (sp->c_msg != 0)
	wprintw(win, " Color %s", sp->c_msg);
    wclrtoeol(win);
    wmove(win, y, x);
}

static void
reshow_status(WINDOW *win, STATUS * sp)
{
    fill_window(win);
    show_status(win, sp);
}

static void
do_subwindow(WINDOW *win, STATUS * sp, void func(WINDOW *))
{
    WINDOW *win1 = newwin(sp->y_max - 2, sp->x_max - 2,
			  sp->y_beg + 1, sp->x_beg + 1);

    if (win1 != 0 && sp->y_max > 4 && sp->x_max > 4) {
	WINDOW *win2 = derwin(win1, sp->y_max - 4, sp->x_max - 4, 1, 1);

	if (win2 != 0) {
	    box(win1, 0, 0);
	    wrefresh(win1);
	    func(win2);

	    delwin(win2);
	} else {
	    beep();
	}
	delwin(win1);
	touchwin(win);
    } else {
	if (win1)
	    delwin(win1);
	beep();
    }
}

static void
init_status(WINDOW *win, STATUS * sp)
{
    memset(sp, 0, sizeof(*sp));
    sp->c = 99;
    sp->v = 99;
    sp->ch = ' ';

    keypad(win, TRUE);
    fill_window(win);

    getbegyx(win, sp->y_beg, sp->x_beg);
    getmaxyx(win, sp->y_max, sp->x_max);
}

static void
show_help(WINDOW *win)
{
    static const char *table[] =
    {
	"Basic commands:"
	,"Use h/j/k/l or arrow keys to move the cursor."
	,"Set the count parameter for insert/delete by entering digits 0-9."
	,""
	,"Other commands:"
	,"space toggles through the set of video attributes and colors."
	,"t     touches (forces repaint) of the current line."
	,"i     calls insertln at the current position with the given count."
	,"d     calls deleteln at the window beginning with the given count."
	,"I     calls insdelln at the window beginning with the given count."
	,"D     calls insdelln at the window beginning with the given -count."
	,"f     refills the window with test-pattern using current attributes."
	,"w     recur to test windows other than stdscr"
	,"q     quit"
	,"=     resets count to zero."
	,"?     shows this help-window"
	,0
    };

    popup_msg(win, table);
}

static void
update_status(WINDOW *win, STATUS * sp)
{
    switch (sp->ch) {
    case ' ':			/* next test-iteration */
	if (has_colors()) {
	    if ((sp->c_msg = color_params(++(sp->c), &(sp->pair))) == 0) {
		sp->c_msg = color_params(sp->c = 0, &(sp->pair));
		if ((sp->v_msg = video_params(++(sp->v), &(sp->attr))) == 0) {
		    sp->v_msg = video_params(sp->v = 0, &(sp->attr));
		}
	    }
	} else {
	    if ((sp->v_msg = video_params(++(sp->v), &(sp->attr))) == 0) {
		sp->v_msg = video_params(sp->v = 0, &(sp->attr));
	    }
	}
	sp->count = 0;
	show_status(win, sp);
	break;
    case KEY_LEFT:
    case 'h':
	if (sp->x_val > 0)
	    wmove(win, sp->y_val, --(sp->x_val));
	break;
    case KEY_DOWN:
    case 'j':
	if (sp->y_val < sp->y_max)
	    wmove(win, ++(sp->y_val), sp->x_val);
	break;
    case KEY_UP:
    case 'k':
	if (sp->y_val > 0)
	    wmove(win, --(sp->y_val), sp->x_val);
	break;
    case KEY_RIGHT:
    case 'l':
	if (sp->x_val < sp->x_max)
	    wmove(win, sp->y_val, ++(sp->x_val));
	break;
    case 't':
	touchline(win, sp->y_val, 1);
	break;
    case '=':
	sp->count = 0;
	show_status(win, sp);
	break;
    case HELP_KEY_1:
	show_help(win);
	break;
    default:
	if (isdigit(sp->ch)) {
	    sp->count = (sp->count * 10) + (sp->ch - '0');
	    show_status(win, sp);
	} else {
	    beep();
	}
	break;
    }
}

static void
test_winsdelln(WINDOW *win)
{
    STATUS st;
    int n;

    init_status(win, &st);

    do {
	(void) wattrset(win, AttrArg(COLOR_PAIR(st.pair), st.attr));
	switch (st.ch) {
	case 'i':
	    for (n = 0; n < st.count; ++n)
		winsertln(win);
	    break;
	case 'd':
	    for (n = 0; n < st.count; ++n)
		wdeleteln(win);
	    break;
	case 'I':
	    winsdelln(win, st.count);
	    break;
	case 'D':
	    winsdelln(win, -st.count);
	    break;
	case 'f':
	    fill_window(win);
	    reshow_status(win, &st);
	    break;
	case 'w':
	    do_subwindow(win, &st, test_winsdelln);
	    break;
	case 'q':
	    return;
	default:
	    update_status(win, &st);
	    break;
	}
    } while ((st.ch = wgetch(win)) != ERR);
}

static void
test_insdelln(void)
{
    STATUS st;
    int n;

    init_status(stdscr, &st);

    do {
	(void) attrset(AttrArg(COLOR_PAIR(st.pair), st.attr));
	switch (st.ch) {
	case 'i':
	    for (n = 0; n < st.count; ++n)
		insertln();
	    break;
	case 'd':
	    for (n = 0; n < st.count; ++n)
		deleteln();
	    break;
	case 'I':
	    insdelln(st.count);
	    break;
	case 'D':
	    insdelln(-st.count);
	    break;
	case 'f':
	    fill_window(stdscr);
	    reshow_status(stdscr, &st);
	    break;
	case 'w':
	    do_subwindow(stdscr, &st, test_winsdelln);
	    break;
	case 'q':
	    return;
	default:
	    update_status(stdscr, &st);
	    break;
	}
    } while ((st.ch = getch()) != ERR);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    initscr();
    cbreak();
    noecho();

    test_insdelln();
    endwin();

    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the curses winsdelln function\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
