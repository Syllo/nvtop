/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
 * Copyright 2006-2013,2017 Free Software Foundation, Inc.                  *
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
 * $Id: movewindow.c,v 1.51 2020/02/02 23:34:34 tom Exp $
 *
 * Demonstrate move functions for windows and derived windows from the curses
 * library.
 *
 * Author: Thomas E. Dickey
 */
/*
derwin
mvderwin
subwin
mvwin

TODO:
    add command to reset subwindow's origin to coincide with parent.
    add command to delete subwindow (check if it has subwindows though)
 */

#include <test.priv.h>

#if HAVE_MVDERWIN && HAVE_MVWIN

#include <popup_msg.h>

#ifdef HAVE_XCURSES
#undef derwin
#endif

#if defined(NCURSES_CONST)
#define CONST_FMT NCURSES_CONST
#elif defined(PDCURSES)
#define CONST_FMT const
#else
#define CONST_FMT		/* nothing */
#endif

#undef LINE_MAX

#define LINE_MIN	2
#define LINE_MAX	(LINES - 2)
#define COL_MIN		2
#define COL_MAX		(COLS - 2)

typedef struct {
    int y, x;
} PAIR;

typedef struct {
    WINDOW *parent;		/* need this since WINDOW->_parent is not portable */
    WINDOW *child;		/* the actual value */
} FRAME;

static void head_line(CONST_FMT char *fmt, ...) GCC_PRINTFLIKE(1, 2);
static void tail_line(CONST_FMT char *fmt, ...) GCC_PRINTFLIKE(1, 2);

static unsigned num_windows;
static FRAME *all_windows;

static void
failed(const char *s)
{
    perror(s);
    endwin();
    ExitProgram(EXIT_FAILURE);
}

static void
message(int lineno, CONST_FMT char *fmt, va_list argp)
{
    int y, x;

    getyx(stdscr, y, x);
    move(lineno, 0);
    clrtoeol();

#ifdef HAVE_XCURSES
    {
	char buffer[1024];
	vsprintf(buffer, fmt, argp);
	addstr(buffer);
    }
#elif defined(HAVE_VW_PRINTW)
    vw_printw(stdscr, fmt, argp);
#else
    vwprintw(stdscr, fmt, argp);
#endif

    move(y, x);
    refresh();
}

static void
head_line(CONST_FMT char *fmt, ...)
{
    va_list argp;

    va_start(argp, fmt);
    message(0, fmt, argp);
    va_end(argp);
}

static void
tail_line(CONST_FMT char *fmt, ...)
{
    va_list argp;

    va_start(argp, fmt);
    message(LINES - 1, fmt, argp);
    va_end(argp);
}

/*
 * Arrow keys move cursor, return location at current on non-arrow key.
 */
static PAIR *
selectcell(WINDOW *parent,
	   WINDOW *child,
	   int uli, int ulj,
	   int lri, int lrj,
	   bool relative,
	   bool * more)
{
    static PAIR res;		/* result cell */
    int si = lri - uli + 1;	/* depth of the select area */
    int sj = lrj - ulj + 1;	/* width of the select area */
    int i = 0, j = 0;		/* offsets into the select area */

    res.y = uli;
    res.x = ulj;

    if (child != 0) {
	if (relative) {
	    getparyx(child, i, j);
	} else {
	    getbegyx(child, i, j);
	    i -= uli + getbegy(parent);
	    j -= ulj + getbegx(parent);
	}
    }

    if (more)
	*more = FALSE;

    for (;;) {
	bool moved = FALSE;

	tail_line("Upper left [%2d,%2d] Lower right [%2d,%2d] -> %d,%d -> %d,%d",
		  uli, ulj,
		  lri, lrj,
		  i, j,
		  uli + i, ulj + j);
	wmove(parent, uli + i, ulj + j);

	switch (wgetch(parent)) {
	case KEY_UP:
	    i += si - 1;
	    moved = TRUE;
	    break;
	case KEY_DOWN:
	    i++;
	    moved = TRUE;
	    break;
	case KEY_LEFT:
	    j += sj - 1;
	    moved = TRUE;
	    break;
	case KEY_RIGHT:
	    j++;
	    moved = TRUE;
	    break;
	case QUIT:
	    /* FALLTHRU */
	case ESCAPE:
	    return ((PAIR *) 0);
#ifdef NCURSES_MOUSE_VERSION
	case KEY_MOUSE:
	    {
		MEVENT event;

		getmouse(&event);
		if (event.y > uli && event.x > ulj) {
		    if (parent != stdscr) {
			i = event.y - getbegy(parent) - uli;
			j = event.x - getbegx(parent) - ulj;
		    } else {
			i = event.y - uli;
			j = event.x - ulj;
		    }
		} else {
		    beep();
		    break;
		}
	    }
#endif
	    /* FALLTHRU */
	default:
	    res.y = uli + i;
	    res.x = ulj + j;
	    return (&res);
	}

	if (si <= 0)
	    i = 0;
	else
	    i %= si;

	if (sj <= 0)
	    j = 0;
	else
	    j %= sj;

	/*
	 * If the caller can handle continuous movement, return the result.
	 */
	if (moved && more) {
	    *more = TRUE;
	    res.y = uli + i;
	    res.x = ulj + j;
	    return (&res);
	}
    }
}

/*
 * Ask user for a window definition.
 */
static bool
getwindow(WINDOW *parent, PAIR * ul, PAIR * lr)
{
    int min_col = (parent == stdscr) ? COL_MIN : 0;
    int max_col = (parent == stdscr) ? COL_MAX : getmaxx(parent);
    int min_line = (parent == stdscr) ? LINE_MIN : 0;
    int max_line = (parent == stdscr) ? LINE_MAX : getmaxy(parent);
    PAIR *tmp;
    bool result = FALSE;

    head_line("Use arrows to move cursor, anything else to mark corner 1");
    if ((tmp = selectcell(parent, 0,
			  min_line, min_col,
			  max_line, max_col,
			  FALSE,
			  (bool *) 0)) != 0) {
	*ul = *tmp;
	MvWAddCh(parent, ul->y, ul->x, '*');

	head_line("Use arrows to move cursor, anything else to mark corner 2");
	if ((tmp = selectcell(parent, 0,
			      ul->y, ul->x,
			      max_line, max_col,
			      FALSE,
			      (bool *) 0)) != 0) {
	    *lr = *tmp;
	    MvWAddCh(parent, lr->y, lr->x, '*');
	    wmove(parent, lr->y, lr->x);
	    wsyncdown(parent);
	    wrefresh(parent);
	    result = (lr->y != ul->y && lr->x != ul->x);
	}
    }
    head_line("done");
    return result;
}

/*
 * Draw a box inside the given window.
 */
static void
box_inside(WINDOW *win)
{
    int y0, x0;
    int y1, x1;

    getyx(win, y0, x0);
    getmaxyx(win, y1, x1);

    MvWHLine(win, 0, 0, ACS_HLINE, x1);
    MvWHLine(win, y1 - 1, 0, ACS_HLINE, x1);

    MvWVLine(win, 0, 0, ACS_VLINE, y1);
    MvWVLine(win, 0, x1 - 1, ACS_VLINE, y1);

    MvWAddCh(win, 0, 0, ACS_ULCORNER);
    MvWAddCh(win, y1 - 1, 0, ACS_LLCORNER);
    MvWAddCh(win, 0, x1 - 1, ACS_URCORNER);
    MvWAddCh(win, y1 - 1, x1 - 1, ACS_LRCORNER);

    wsyncdown(win);
    wmove(win, y0, x0);
    wrefresh(win);
}

/*
 * Add a window to our list.
 */
static void
add_window(WINDOW *parent, WINDOW *child)
{
    static unsigned have = 0;
    unsigned need = ((num_windows + 1) | 31) + 1;

    keypad(child, TRUE);
    if (need > have) {
	all_windows = typeRealloc(FRAME, need, all_windows);
	if (!all_windows)
	    failed("add_window");
    }
    all_windows[num_windows].parent = parent;
    all_windows[num_windows].child = child;
    num_windows++;
}

static int
window2num(WINDOW *win)
{
    int n;
    int result = -1;
    for (n = 0; n < (int) num_windows; ++n) {
	if (win == all_windows[n].child) {
	    result = n;
	    break;
	}
    }
    return result;
}

static WINDOW *
parent_of(WINDOW *win)
{
    WINDOW *result = 0;
    int n = window2num(win);
    if (n >= 0)
	result = all_windows[n].parent;
    return result;
}

static void
repaint_one(WINDOW *win)
{
    touchwin(win);
    wnoutrefresh(win);
}

static void
refresh_all(WINDOW *win)
{
    unsigned n;

    for (n = 0; n < num_windows; ++n) {
	if (all_windows[n].child != win) {
	    repaint_one(all_windows[n].child);
	}
    }

    repaint_one(win);
    doupdate();
}

static WINDOW *
next_window(WINDOW *win)
{
    WINDOW *result = win;
    int n = window2num(win);

    if (n++ >= 0) {
	result = all_windows[(unsigned) n % num_windows].child;
	wmove(result, 0, 0);
	wrefresh(result);
    }
    return result;
}

static WINDOW *
prev_window(WINDOW *win)
{
    WINDOW *result = win;
    int n = window2num(win);

    if (n-- >= 0) {
	if (n < 0)
	    n = (int) (num_windows - 1);
	result = all_windows[(unsigned) n % num_windows].child;
	wmove(result, 0, 0);
	wrefresh(result);
    }
    return result;
}

static void
recur_move_window(WINDOW *parent, int dy, int dx)
{
    unsigned n;

    for (n = 0; n < num_windows; ++n) {
	if (all_windows[n].parent == parent) {
	    mvwin(all_windows[n].child, dy, dx);
	    recur_move_window(all_windows[n].child, dy, dx);
	}
    }
}

/*
 * test mvwin().
 */
static bool
move_window(WINDOW *win, bool recur)
{
    WINDOW *parent = parent_of(win);
    bool result = FALSE;

    if (parent != 0) {
	bool top = (parent == stdscr);
	int min_col = top ? COL_MIN : 0;
	int max_col = top ? COL_MAX : getmaxx(parent);
	int min_line = top ? LINE_MIN : 0;
	int max_line = top ? LINE_MAX : getmaxy(parent);
	PAIR *tmp;
	bool more;

	head_line("Select new position for %swindow", top ? "" : "sub");

	while ((tmp = selectcell(parent,
				 win,
				 min_line, min_col,
				 max_line, max_col,
				 FALSE,
				 &more)) != 0) {
	    int y0, x0;
	    getbegyx(parent, y0, x0);
	    /*
	     * Moving a subwindow has the effect of moving a viewport around
	     * the screen.  The parent window retains the contents of the
	     * subwindow in the original location, but the viewport will show
	     * the contents (again) at the new location.  So it will look odd
	     * when testing.
	     */
	    if (mvwin(win, y0 + tmp->y, x0 + tmp->x) != ERR) {
		if (recur) {
		    recur_move_window(win, tmp->y, tmp->x);
		}
		refresh_all(win);
		doupdate();
		result = TRUE;
	    } else {
		result = FALSE;
	    }
	    if (!more)
		break;
	}
    }
    head_line("done");
    return result;
}

static void
show_derwin(WINDOW *win)
{
    int pary, parx, maxy, maxx;

    getmaxyx(win, maxy, maxx);
    getparyx(win, pary, parx);

    head_line("Select new position for derived window at %d,%d (%d,%d)",
	      pary, parx, maxy, maxx);
}

/*
 * test mvderwin().
 */
static bool
move_derwin(WINDOW *win)
{
    WINDOW *parent = parent_of(win);
    bool result = FALSE;

    if (parent != 0) {
	bool top = (parent == stdscr);
	int min_col = top ? COL_MIN : 0;
	int max_col = top ? COL_MAX : getmaxx(parent);
	int min_line = top ? LINE_MIN : 0;
	int max_line = top ? LINE_MAX : getmaxy(parent);
	PAIR *tmp;
	bool more;

	show_derwin(win);
	while ((tmp = selectcell(parent,
				 win,
				 min_line, min_col,
				 max_line, max_col,
				 TRUE,
				 &more)) != 0) {
	    if (mvderwin(win, tmp->y, tmp->x) != ERR) {
		refresh_all(win);
		doupdate();
		repaint_one(win);
		doupdate();
		result = TRUE;
		show_derwin(win);
	    } else {
		flash();
	    }
	    if (!more)
		break;
	}
    }
    head_line("done");
    return result;
}

static void
fill_window(WINDOW *win, chtype ch)
{
    int y, x;
    int y0, x0;
    int y1, x1;

    getyx(win, y0, x0);
    getmaxyx(win, y1, x1);
    for (y = 0; y < y1; ++y) {
	for (x = 0; x < x1; ++x) {
	    MvWAddCh(win, y, x, ch);
	}
    }
    wsyncdown(win);
    wmove(win, y0, x0);
    wrefresh(win);
}

static void
fill_with_pattern(WINDOW *win)
{
    int y, x;
    int y0, x0;
    int y1, x1;
    int ch = 'a';

    getyx(win, y0, x0);
    getmaxyx(win, y1, x1);
    for (y = 0; y < y1; ++y) {
	for (x = 0; x < x1; ++x) {
	    MvWAddCh(win, y, x, (chtype) ch);
	    if (++ch > 'z')
		ch = 'a';
	}
    }
    wsyncdown(win);
    wmove(win, y0, x0);
    wrefresh(win);
}

#define lines_of(ul,lr)	(lr.y - ul.y + 1)
#define cols_of(ul,lr)	(lr.x - ul.x + 1)
#define pair_of(ul)	ul.y, ul.x

static WINDOW *
create_my_window(WINDOW *current)
{
    PAIR ul, lr;
    WINDOW *result = 0;

    if (getwindow(stdscr, &ul, &lr)) {
	result = newwin(lines_of(ul, lr), cols_of(ul, lr), pair_of(ul));
	if (result != 0) {
	    fill_window(result, 'c');
	    add_window(stdscr, result);
	}
    }
    if (result == 0)
	result = current;
    return result;
}

static WINDOW *
create_my_derwin(WINDOW *parent)
{
    PAIR ul, lr;
    WINDOW *result = 0;

    if (getwindow(parent, &ul, &lr)) {
	result = derwin(parent, lines_of(ul, lr), cols_of(ul, lr), pair_of(ul));
	if (result != 0) {
	    fill_window(result, 'd');
	    add_window(parent, result);
	}
    }
    if (result == 0)
	result = parent;
    return result;
}

static WINDOW *
create_my_subwin(WINDOW *parent)
{
    PAIR ul, lr;
    WINDOW *result = 0;

    if (getwindow(parent, &ul, &lr)) {
	result = subwin(parent,
			lines_of(ul, lr),
			cols_of(ul, lr),
			ul.y + getbegy(parent),
			ul.x + getbegx(parent));
	if (result != 0) {
	    fill_window(result, 's');
	    add_window(parent, result);
	}
    }
    if (result == 0)
	result = parent;
    return result;
}

static void
show_help(WINDOW *current)
{
    /* *INDENT-OFF* */
    static struct {
	int	key;
	CONST_FMT char * msg;
    } help[] = {
	{ HELP_KEY_1,	"Show this screen" },
	{ 'b',		"Draw a box inside the current window" },
	{ 'c',		"Create a new window" },
	{ 'd',		"Create a new derived window" },
	{ 'D',		"Move derived window (moves viewport)" },
	{ 'f',		"Fill the current window with the next character" },
	{ 'F',		"Fill the current window with a pattern" },
	{ 'm',		"Move the current window" },
	{ 'M',		"Move the current window (and its children)" },
	{ 'q',		"Quit" },
	{ 's',		"Create a new subwindow" },
	{ CTRL('L'),	"Repaint all windows, doing current one last" },
	{ CTRL('N'),	"Cursor to next window" },
	{ CTRL('P'),	"Cursor to previous window" },
    };
    /* *INDENT-ON* */

    char **msgs = typeCalloc(char *, SIZEOF(help) + 1);
    size_t n;

    for (n = 0; n < SIZEOF(help); ++n) {
	size_t need = (21 + strlen(help[n].msg));
	msgs[n] = typeMalloc(char, need);
	_nc_SPRINTF(msgs[n], _nc_SLIMIT(need)
		    "%-20s%s", keyname(help[n].key), help[n].msg);
    }
    popup_msg2(current, msgs);
    for (n = 0; n < SIZEOF(help); ++n) {
	free(msgs[n]);
    }
    free(msgs);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    WINDOW *current_win;
    int ch;
    bool done = FALSE;

    initscr();
    cbreak();
    noecho();
    nonl();
    intrflush(stdscr, FALSE);

    add_window(0, current_win = stdscr);

#ifdef NCURSES_MOUSE_VERSION
    (void) mousemask(BUTTON1_CLICKED, (mmask_t *) NULL);
#endif /* NCURSES_MOUSE_VERSION */

    while (!done && (ch = wgetch(current_win)) != ERR) {
	int y, x;

	getyx(current_win, y, x);

	switch (ch) {
	case HELP_KEY_1:
	    show_help(current_win);
	    break;
	case 'b':
	    box_inside(current_win);
	    break;
	case 'c':
	    current_win = create_my_window(current_win);
	    break;
	case 'd':
	    current_win = create_my_derwin(current_win);
	    break;
	case 'D':
	    if (!move_derwin(current_win)) {
		tail_line("error");
		continue;
	    }
	    break;
	case 'f':
	    fill_window(current_win, (chtype) wgetch(current_win));
	    break;
	case 'F':
	    fill_with_pattern(current_win);
	    break;
	case 'm':
	case 'M':
	    if (!move_window(current_win, (ch == 'M'))) {
		tail_line("error");
		continue;
	    }
	    break;
	case 'q':
	    done = TRUE;
	    break;
	case 's':
	    current_win = create_my_subwin(current_win);
	    break;
	case CTRL('L'):
	    refresh_all(current_win);
	    break;
	case CTRL('N'):
	    current_win = next_window(current_win);
	    break;
	case CTRL('P'):
	    current_win = prev_window(current_win);
	    break;
#if 0
	    /* want to allow cursor to move around the current window too */
	    /* want to test the resizing of windows and subwindows too */
	    /* want to allow deleting a window also */
#endif
	default:
	    wmove(current_win, y, x);
	    tail_line("unrecognized key (use '?' for help)");
	    beep();
	    continue;
	}
	tail_line("size [%d,%d] begin [%d,%d] parent [%d,%d]",
		  getmaxy(current_win),
		  getmaxx(current_win),
		  getbegy(current_win),
		  getbegx(current_win),
		  getpary(current_win),
		  getparx(current_win));
	wmove(current_win, 0, 0);
    }
    endwin();
#if NO_LEAKS
    free(all_windows);
#endif
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires the curses mvderwin and mvwin functions\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
