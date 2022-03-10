/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2016,2017 Free Software Foundation, Inc.                  *
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
/****************************************************************************

NAME
   ncurses.c --- ncurses library exerciser

SYNOPSIS
   ncurses

DESCRIPTION
   An interactive test module for the ncurses library.

AUTHOR
   Author: Eric S. Raymond <esr@snark.thyrsus.com> 1993
           Thomas E. Dickey (beginning revision 1.27 in 1996).

$Id: ncurses.c,v 1.527 2021/09/04 10:31:03 tom Exp $

***************************************************************************/

#include <test.priv.h>

#ifdef __hpux
#undef mvwdelch			/* HPUX 11.23 macro will not compile */
#endif

#if HAVE_GETTIMEOFDAY
#if HAVE_SYS_TIME_H && HAVE_SYS_TIME_SELECT
#include <sys/time.h>
#endif
#if HAVE_SYS_SELECT_H
#include <sys/select.h>
#endif
#endif

#if USE_LIBPANEL
#include <panel.h>
#endif

#if USE_LIBMENU
#include <menu.h>
#endif

#if USE_LIBFORM
#include <form.h>
#endif

#ifdef NCURSES_VERSION

#define NCURSES_CONST_PARAM const void

#ifdef TRACE
static unsigned save_trace = TRACE_ORDINARY | TRACE_ICALLS | TRACE_CALLS;
#endif

#else

#define NCURSES_CONST_PARAM char

#define mmask_t chtype		/* not specified in XSI */

#ifndef ACS_S3
#ifdef CURSES_ACS_ARRAY
#define ACS_S3          (CURSES_ACS_ARRAY['p'])		/* scan line 3 */
#define ACS_S7          (CURSES_ACS_ARRAY['r'])		/* scan line 7 */
#define ACS_LEQUAL      (CURSES_ACS_ARRAY['y'])		/* less/equal */
#define ACS_GEQUAL      (CURSES_ACS_ARRAY['z'])		/* greater/equal */
#define ACS_PI          (CURSES_ACS_ARRAY['{'])		/* Pi */
#define ACS_NEQUAL      (CURSES_ACS_ARRAY['|'])		/* not equal */
#define ACS_STERLING    (CURSES_ACS_ARRAY['}'])		/* UK pound sign */
#else
#define ACS_S3          (A_ALTCHARSET + 'p')	/* scan line 3 */
#define ACS_S7          (A_ALTCHARSET + 'r')	/* scan line 7 */
#define ACS_LEQUAL      (A_ALTCHARSET + 'y')	/* less/equal */
#define ACS_GEQUAL      (A_ALTCHARSET + 'z')	/* greater/equal */
#define ACS_PI          (A_ALTCHARSET + '{')	/* Pi */
#define ACS_NEQUAL      (A_ALTCHARSET + '|')	/* not equal */
#define ACS_STERLING    (A_ALTCHARSET + '}')	/* UK pound sign */
#endif
#endif /* ACS_S3 */

#ifndef WACS_S3
#ifdef CURSES_WACS_ARRAY
#define WACS_S3         (&(CURSES_WACS_ARRAY['p']))	/* scan line 3 */
#define WACS_S7         (&(CURSES_WACS_ARRAY['r']))	/* scan line 7 */
#define WACS_LEQUAL     (&(CURSES_WACS_ARRAY['y']))	/* less/equal */
#define WACS_GEQUAL     (&(CURSES_WACS_ARRAY['z']))	/* greater/equal */
#define WACS_PI         (&(CURSES_WACS_ARRAY['{']))	/* Pi */
#define WACS_NEQUAL     (&(CURSES_WACS_ARRAY['|']))	/* not equal */
#define WACS_STERLING   (&(CURSES_WACS_ARRAY['}']))	/* UK pound sign */
#endif
#endif

#endif

#if HAVE_WCSRTOMBS
#define count_wchars(src, len, state)      wcsrtombs(0,   &src, len, state)
#define trans_wchars(dst, src, len, state) wcsrtombs(dst, &src, len, state)
#define reset_wchars(state) init_mb(state)
#elif HAVE_WCSTOMBS && HAVE_MBTOWC && HAVE_MBLEN
#define count_wchars(src, len, state)      wcstombs(0,   src, len)
#define trans_wchars(dst, src, len, state) wcstombs(dst, src, len)
#define reset_wchars(state) IGNORE_RC(mblen(NULL, 0)), IGNORE_RC(mbtowc(NULL, NULL, 0))
#define state_unused
#endif

#if HAVE_MBSRTOWCS
#define count_mbytes(src, len, state)      mbsrtowcs(0,   &src, len, state)
#define trans_mbytes(dst, src, len, state) mbsrtowcs(dst, &src, len, state)
#define reset_mbytes(state) init_mb(state)
#elif HAVE_MBSTOWCS && HAVE_MBTOWC && HAVE_MBLEN
#define count_mbytes(src, len, state)      mbstowcs(0,   src, len)
#define trans_mbytes(dst, src, len, state) mbstowcs(dst, src, len)
#define reset_mbytes(state) IGNORE_RC(mblen(NULL, 0)), IGNORE_RC(mbtowc(NULL, NULL, 0))
#define state_unused
#endif

#define ToggleAcs(temp,real) temp = ((temp == real) ? NULL : real)

#define P(string)	printw("%s\n", string)

#define BLANK		' '	/* this is the background character */

static int MaxColors;		/* the actual number of colors we'll use */
static int MinColors;		/* the minimum color code */
static bool UseColors;		/* true if we use colors */

#undef max_pairs
static int max_pairs;		/* ...and the number of color pairs */

#if HAVE_COLOR_CONTENT
typedef struct {
    NCURSES_COLOR_T red;
    NCURSES_COLOR_T green;
    NCURSES_COLOR_T blue;
} RGB_DATA;

static RGB_DATA *all_colors;
#endif

static void main_menu(bool);
static GCC_NORETURN void failed(const char *s);

static void
failed(const char *s)
{
    perror(s);
    endwin();
    ExitProgram(EXIT_FAILURE);
}

static void
Repaint(void)
{
    touchwin(stdscr);
#if HAVE_CURSCR
    touchwin(curscr);
    wrefresh(curscr);
#else
    wrefresh(stdscr);
#endif
}

static bool
isQuit(int c, bool escape)
{
    return ((c) == QUIT || (escape && ((c) == ESCAPE)));
}
#define case_QUIT	QUIT: case ESCAPE

/* Common function to allow ^T to toggle trace-mode in the middle of a test
 * so that trace-files can be made smaller.
 */
static int
wGetchar(WINDOW *win)
{
    int c;
#ifdef TRACE
    while ((c = wgetch(win)) == CTRL('T')) {
	if (_nc_tracing) {
	    save_trace = _nc_tracing;
	    Trace(("TOGGLE-TRACING OFF"));
	    _nc_tracing = 0;
	} else {
	    _nc_tracing = save_trace;
	}
	curses_trace(_nc_tracing);
	if (_nc_tracing)
	    Trace(("TOGGLE-TRACING ON"));
    }
#else
    c = wgetch(win);
#endif
    return c;
}
#define Getchar() wGetchar(stdscr)

#if USE_SOFTKEYS
/* replaces wgetnstr(), since we want to be able to edit values */
static void
wGetstring(WINDOW *win, char *buffer, int limit)
{
    int y0, x0, x;
    bool done = FALSE;

    echo();
    getyx(win, y0, x0);
    (void) wattrset(win, A_REVERSE);

    x = (int) strlen(buffer);
    while (!done) {
	int ch;
	if (x > (int) strlen(buffer))
	    x = (int) strlen(buffer);
	wmove(win, y0, x0);
	wprintw(win, "%-*s", limit, buffer);
	wmove(win, y0, x0 + x);
	switch (ch = wGetchar(win)) {
	case '\n':
	case KEY_ENTER:
	    done = TRUE;
	    break;
	case CTRL('U'):
	    *buffer = '\0';
	    break;
	case '\b':
	case KEY_BACKSPACE:
	case KEY_DC:
	    if (x > 0) {
		int j;
		for (j = --x; (buffer[j] = buffer[j + 1]) != '\0'; ++j) {
		    ;
		}
	    } else {
		beep();
	    }
	    break;
	case KEY_LEFT:
	    if (x > 0) {
		--x;
	    } else {
		flash();
	    }
	    break;
	case KEY_RIGHT:
	    ++x;
	    break;
	default:
	    if (!isprint(ch) || ch >= KEY_MIN) {
		beep();
	    } else if ((int) strlen(buffer) < limit) {
		int j;
		for (j = (int) strlen(buffer) + 1; j > x; --j) {
		    buffer[j] = buffer[j - 1];
		}
		buffer[x++] = (char) ch;
	    } else {
		flash();
	    }
	}
    }

    wattroff(win, A_REVERSE);
    wmove(win, y0, x0);
    noecho();
}
#endif

#if USE_WIDEC_SUPPORT
static wchar_t
fullwidth_digit(int ch)
{
    return (wchar_t) (ch + 0xff10 - '0');
}

static void
make_fullwidth_text(wchar_t *target, const char *source)
{
    int ch;
    while ((ch = *source++) != 0) {
	*target++ = fullwidth_digit(ch);
    }
    *target = 0;
}

static void
make_narrow_text(wchar_t *target, const char *source)
{
    int ch;
    while ((ch = *source++) != 0) {
	*target++ = (wchar_t) ch;
    }
    *target = 0;
}

#if USE_LIBPANEL
static void
make_fullwidth_digit(cchar_t *target, int digit)
{
    wchar_t source[2];

    source[0] = fullwidth_digit(digit + '0');
    source[1] = 0;
    setcchar(target, source, A_NORMAL, 0, 0);
}
#endif

static int
wGet_wchar(WINDOW *win, wint_t *result)
{
    int c;
#ifdef TRACE
    while ((c = wget_wch(win, result)) == CTRL('T')) {
	if (_nc_tracing) {
	    save_trace = _nc_tracing;
	    Trace(("TOGGLE-TRACING OFF"));
	    _nc_tracing = 0;
	} else {
	    _nc_tracing = save_trace;
	}
	curses_trace(_nc_tracing);
	if (_nc_tracing)
	    Trace(("TOGGLE-TRACING ON"));
    }
#else
    c = wget_wch(win, result);
#endif
    return c;
}
#define Get_wchar(result) wGet_wchar(stdscr, result)

/* replaces wgetn_wstr(), since we want to be able to edit values */
#if USE_SOFTKEYS
static void
wGet_wstring(WINDOW *win, wchar_t *buffer, int limit)
{
    int y0, x0, x;
    wint_t ch;
    bool done = FALSE;
    bool fkey = FALSE;

    echo();
    getyx(win, y0, x0);
    (void) wattrset(win, A_REVERSE);

    x = (int) wcslen(buffer);
    while (!done) {
	if (x > (int) wcslen(buffer))
	    x = (int) wcslen(buffer);

	/* clear the "window' */
	wmove(win, y0, x0);
	wprintw(win, "%*s", limit, " ");

	/* write the existing buffer contents */
	wmove(win, y0, x0);
	waddnwstr(win, buffer, limit);

	/* positions the cursor past character 'x' */
	wmove(win, y0, x0);
	waddnwstr(win, buffer, x);

	switch (wGet_wchar(win, &ch)) {
	case KEY_CODE_YES:
	    fkey = TRUE;
	    switch (ch) {
	    case KEY_ENTER:
		ch = '\n';
		fkey = FALSE;
		break;
	    case KEY_BACKSPACE:
	    case KEY_DC:
		ch = '\b';
		fkey = FALSE;
		break;
	    case KEY_LEFT:
	    case KEY_RIGHT:
		break;
	    default:
		ch = (wint_t) -1;
		break;
	    }
	    break;
	case OK:
	    fkey = FALSE;
	    break;
	default:
	    ch = (wint_t) -1;
	    fkey = TRUE;
	    break;
	}

	switch (ch) {
	case '\n':
	    done = TRUE;
	    break;
	case CTRL('U'):
	    *buffer = '\0';
	    break;
	case '\b':
	    if (x > 0) {
		int j;
		for (j = --x; (buffer[j] = buffer[j + 1]) != '\0'; ++j) {
		    ;
		}
	    } else {
		beep();
	    }
	    break;
	case KEY_LEFT:
	    if (x > 0) {
		--x;
	    } else {
		beep();
	    }
	    break;
	case KEY_RIGHT:
	    ++x;
	    break;
	default:
	    if (fkey) {
		beep();
	    } else if ((int) wcslen(buffer) < limit) {
		int j;
		for (j = (int) wcslen(buffer) + 1; j > x; --j) {
		    buffer[j] = buffer[j - 1];
		}
		buffer[x++] = (wchar_t) ch;
	    } else {
		beep();
	    }
	}
    }

    wattroff(win, A_REVERSE);
    wmove(win, y0, x0);
    noecho();
}
#endif /* USE_SOFTKEYS */

#endif /* USE_WIDEC_SUPPORT */

static void
Pause(void)
{
    move(LINES - 1, 0);
    addstr("Press any key to continue... ");
    (void) Getchar();
}

static void
Cannot(const char *what)
{
    printw("\nThis %s terminal %s\n\n", getenv("TERM"), what);
    Pause();
    endwin();
}

static void
ShellOut(bool message)
{
    if (message)
	addstr("Shelling out...");
    def_prog_mode();
    endwin();
#ifdef _NC_WINDOWS
    system("cmd.exe");
#else
    IGNORE_RC(system("sh"));
#endif
    if (message)
	addstr("returned from shellout.\n");
    refresh();
}

#ifdef NCURSES_MOUSE_VERSION
/*
 * This function is the same as _tracemouse(), but we cannot count on that
 * being available in the non-debug library.
 */
static const char *
mouse_decode(MEVENT const *ep)
{
    static char buf[80 + (5 * 10) + (32 * 15)];

    (void) _nc_SPRINTF(buf, _nc_SLIMIT(sizeof(buf))
		       "id %2d at (%2d, %2d, %d) state %4lx = {",
		       ep->id, ep->x, ep->y, ep->z, (unsigned long) ep->bstate);

#define SHOW(m, s) \
	if ((ep->bstate & m)==m) { \
		_nc_STRCAT(buf, s, sizeof(buf)); \
		_nc_STRCAT(buf, ", ", sizeof(buf)); \
	}

    SHOW(BUTTON1_RELEASED, "release-1");
    SHOW(BUTTON1_PRESSED, "press-1");
    SHOW(BUTTON1_CLICKED, "click-1");
    SHOW(BUTTON1_DOUBLE_CLICKED, "doubleclick-1");
    SHOW(BUTTON1_TRIPLE_CLICKED, "tripleclick-1");
#if NCURSES_MOUSE_VERSION == 1
    SHOW(BUTTON1_RESERVED_EVENT, "reserved-1");
#endif

    SHOW(BUTTON2_RELEASED, "release-2");
    SHOW(BUTTON2_PRESSED, "press-2");
    SHOW(BUTTON2_CLICKED, "click-2");
    SHOW(BUTTON2_DOUBLE_CLICKED, "doubleclick-2");
    SHOW(BUTTON2_TRIPLE_CLICKED, "tripleclick-2");
#if NCURSES_MOUSE_VERSION == 1
    SHOW(BUTTON2_RESERVED_EVENT, "reserved-2");
#endif

    SHOW(BUTTON3_RELEASED, "release-3");
    SHOW(BUTTON3_PRESSED, "press-3");
    SHOW(BUTTON3_CLICKED, "click-3");
    SHOW(BUTTON3_DOUBLE_CLICKED, "doubleclick-3");
    SHOW(BUTTON3_TRIPLE_CLICKED, "tripleclick-3");
#if NCURSES_MOUSE_VERSION == 1
    SHOW(BUTTON3_RESERVED_EVENT, "reserved-3");
#endif

    SHOW(BUTTON4_RELEASED, "release-4");
    SHOW(BUTTON4_PRESSED, "press-4");
    SHOW(BUTTON4_CLICKED, "click-4");
    SHOW(BUTTON4_DOUBLE_CLICKED, "doubleclick-4");
    SHOW(BUTTON4_TRIPLE_CLICKED, "tripleclick-4");
#if NCURSES_MOUSE_VERSION == 1
    SHOW(BUTTON4_RESERVED_EVENT, "reserved-4");
#endif

#if NCURSES_MOUSE_VERSION == 2
    SHOW(BUTTON5_RELEASED, "release-5");
    SHOW(BUTTON5_PRESSED, "press-5");
    SHOW(BUTTON5_CLICKED, "click-5");
    SHOW(BUTTON5_DOUBLE_CLICKED, "doubleclick-5");
    SHOW(BUTTON5_TRIPLE_CLICKED, "tripleclick-5");
#endif

    SHOW(BUTTON_CTRL, "ctrl");
    SHOW(BUTTON_SHIFT, "shift");
    SHOW(BUTTON_ALT, "alt");
    SHOW(ALL_MOUSE_EVENTS, "all-events");
    SHOW(REPORT_MOUSE_POSITION, "position");

#undef SHOW

    if (buf[strlen(buf) - 1] == ' ')
	buf[strlen(buf) - 2] = '\0';
    _nc_STRCAT(buf, "}", sizeof(buf));
    return (buf);
}

static void
show_mouse(WINDOW *win)
{
    MEVENT event;
    bool outside;
    bool show_loc;

    getmouse(&event);
    outside = !wenclose(win, event.y, event.x);

    if (outside) {
	(void) wstandout(win);
	waddstr(win, "KEY_MOUSE");
	(void) wstandend(win);
    } else {
	waddstr(win, "KEY_MOUSE");
    }
    wprintw(win, ", %s", mouse_decode(&event));

    if (outside)
	win = stdscr;

    show_loc = wmouse_trafo(win, &event.y, &event.x, FALSE);

    if (show_loc) {
	int y, x;
	getyx(win, y, x);
	wmove(win, event.y, event.x);
	waddch(win, '*');
	wmove(win, y, x);
    }

    if (outside)
	wnoutrefresh(win);
}
#endif /* NCURSES_MOUSE_VERSION */

/****************************************************************************
 *
 * Character input test
 *
 ****************************************************************************/

#define NUM_GETCH_FLAGS 256
typedef bool GetchFlags[NUM_GETCH_FLAGS];

static void
setup_getch(WINDOW *win, GetchFlags flags)
{
    keypad(win, flags['k']);	/* should be redundant, but for testing */
    meta(win, flags['m']);	/* force this to a known state */
    if (flags['e'])
	echo();
    else
	noecho();
}

static void
init_getch(WINDOW *win, GetchFlags flags, int delay)
{
    memset(flags, FALSE, NUM_GETCH_FLAGS);
    flags[UChar('k')] = (win == stdscr);
    flags[UChar('m')] = TRUE;
    flags[UChar('t')] = (delay != 0);

    setup_getch(win, flags);
}

static bool
blocking_getch(GetchFlags flags, int delay)
{
    return ((delay < 0) && flags['t']);
}

#define ExitOnEscape() (flags[UChar('k')] && flags[UChar('t')])

static void
wgetch_help(WINDOW *win, GetchFlags flags)
{
    static const char *help[] =
    {
	"e  -- toggle echo mode"
	,"g  -- triggers a getstr test"
	,"k  -- toggle keypad/literal mode"
	,"m  -- toggle meta (7-bit/8-bit) mode"
	,"^q -- quit"
	,"s  -- shell out"
	,"t  -- toggle timeout"
	,"w  -- create a new window"
#ifdef SIGTSTP
	,"z  -- suspend this process"
#endif
    };
    int y, x;
    unsigned chk = ((SIZEOF(help) + 1) / 2);
    unsigned n;

    getyx(win, y, x);
    move(0, 0);
    printw("Type any key to see its %s value.  Also:\n",
	   flags['k'] ? "keypad" : "literal");
    for (n = 0; n < SIZEOF(help); ++n) {
	const char *msg = help[n];
	int row = 1 + (int) (n % chk);
	int col = (n >= chk) ? COLS / 2 : 0;
	int flg = ((strstr(msg, "toggle") != 0)
		   && (flags[UChar(*msg)] != FALSE));
	if (*msg == '^' && ExitOnEscape())
	    msg = "^[,^q -- quit";
	if (flg)
	    (void) standout();
	MvPrintw(row, col, "%s", msg);
	if (col == 0)
	    clrtoeol();
	if (flg)
	    (void) standend();
    }
    wrefresh(stdscr);
    wmove(win, y, x);
}

static void
wgetch_wrap(WINDOW *win, int first_y)
{
    int last_y = getmaxy(win) - 1;
    int y = getcury(win) + 1;

    if (y >= last_y)
	y = first_y;
    wmove(win, y, 0);
    wclrtoeol(win);
}

#if defined(KEY_RESIZE) && HAVE_WRESIZE
typedef struct {
    WINDOW *text;
    WINDOW *frame;
} WINSTACK;

static WINSTACK *winstack = 0;
static unsigned len_winstack = 0;

static void
forget_boxes(void)
{
    if (winstack != 0) {
	free(winstack);
    }
    winstack = 0;
    len_winstack = 0;
}

static void
remember_boxes(unsigned level, WINDOW *txt_win, WINDOW *box_win)
{
    unsigned need = (level + 1) * 2;

    assert(level < (unsigned) COLS);

    if (winstack == 0) {
	len_winstack = 20;
	winstack = typeMalloc(WINSTACK, len_winstack);
    } else if (need >= len_winstack) {
	len_winstack = need;
	winstack = typeRealloc(WINSTACK, len_winstack, winstack);
    }
    if (!winstack)
	failed("remember_boxes");
    winstack[level].text = txt_win;
    winstack[level].frame = box_win;
}

#if USE_SOFTKEYS && (defined(NCURSES_VERSION_PATCH) && NCURSES_VERSION_PATCH < 20071229) && NCURSES_EXT_FUNCS
static void
slk_repaint(void)
{
    /* this chunk is now done in resize_term() */
    slk_touch();
    slk_clear();
    slk_noutrefresh();
}

#else
#define slk_repaint()		/* nothing */
#endif

#if defined(NCURSES_VERSION) && defined(KEY_RESIZE) && HAVE_WRESIZE
/*
 * For wgetch_test(), we create pairs of windows - one for a box, one for text.
 * Resize both and paint the box in the parent.
 */
static void
resize_boxes(unsigned level, WINDOW *win)
{
    unsigned n;
    int base = 5;
    int high = LINES - base;
    int wide = COLS;

    touchwin(stdscr);
    wnoutrefresh(stdscr);

    slk_repaint();

    for (n = 0; n < level; ++n) {
	wresize(winstack[n].frame, high, wide);
	wresize(winstack[n].text, high - 2, wide - 2);
	high -= 2;
	wide -= 2;
	werase(winstack[n].text);
	box(winstack[n].frame, 0, 0);
	wnoutrefresh(winstack[n].frame);
	wprintw(winstack[n].text,
		"size %dx%d\n",
		getmaxy(winstack[n].text),
		getmaxx(winstack[n].text));
	wnoutrefresh(winstack[n].text);
	if (winstack[n].text == win)
	    break;
    }
    doupdate();
}
#endif /* resize_boxes */
#else
#define forget_boxes()		/* nothing */
#define remember_boxes(level,text,frame)	/* nothing */
#endif

/*
 * Return-code is OK/ERR or a keyname.
 */
static const char *
ok_keyname(int code)
{
    return ((code == OK) ? "OK" : ((code == ERR) ? "ERR" : keyname(code)));
}

static void
wgetch_test(unsigned level, WINDOW *win, int delay)
{
    char buf[BUFSIZ];
    int first_y, first_x;
    int incount = 0;
    GetchFlags flags;

    init_getch(win, flags, delay);
    notimeout(win, FALSE);
    wtimeout(win, delay);
    getyx(win, first_y, first_x);

    wgetch_help(win, flags);
    wsetscrreg(win, first_y, getmaxy(win) - 1);
    scrollok(win, TRUE);

    for (;;) {
	int c;

	while ((c = wGetchar(win)) == ERR) {
	    incount++;
	    if (blocking_getch(flags, delay)) {
		(void) wprintw(win, "%05d: input error", incount);
		break;
	    } else {
		(void) wprintw(win, "%05d: input timed out", incount);
	    }
	    wgetch_wrap(win, first_y);
	}
	if (c == ERR && blocking_getch(flags, delay)) {
	    wprintw(win, "ERR");
	    wgetch_wrap(win, first_y);
	} else if (isQuit(c, ExitOnEscape())) {
	    break;
	} else if (c == 'e') {
	    flags[UChar('e')] = !flags[UChar('e')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 'g') {
	    waddstr(win, "wgetnstr test: ");
	    echo();
	    c = wgetnstr(win, buf, sizeof(buf) - 1);
	    noecho();
	    wprintw(win, "I saw %d characters:\n\t`%s' (%s).",
		    (int) strlen(buf), buf,
		    ok_keyname(c));
	    wclrtoeol(win);
	    wgetch_wrap(win, first_y);
	} else if (c == 'k') {
	    flags[UChar('k')] = !flags[UChar('k')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 'm') {
	    flags[UChar('m')] = !flags[UChar('m')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 's') {
	    ShellOut(TRUE);
	} else if (c == 't') {
	    notimeout(win, flags[UChar('t')]);
	    flags[UChar('t')] = !flags[UChar('t')];
	    wgetch_help(win, flags);
	} else if (c == 'w') {
	    int high = getmaxy(win) - 1 - first_y + 1;
	    int wide = getmaxx(win) - first_x;
	    int old_y, old_x;
	    int new_y = first_y + getbegy(win);
	    int new_x = first_x + getbegx(win);

	    getyx(win, old_y, old_x);
	    if (high > 2 && wide > 2) {
		WINDOW *wb = newwin(high, wide, new_y, new_x);
		WINDOW *wi = newwin(high - 2, wide - 2, new_y + 1, new_x + 1);

		box(wb, 0, 0);
		wrefresh(wb);
		wmove(wi, 0, 0);
		remember_boxes(level, wi, wb);
		wgetch_test(level + 1, wi, delay);
		delwin(wi);
		delwin(wb);

		wgetch_help(win, flags);
		wmove(win, old_y, old_x);
		touchwin(win);
		wrefresh(win);
		doupdate();
	    }
#ifdef SIGTSTP
	} else if (c == 'z') {
	    kill(getpid(), SIGTSTP);
#endif
	} else {
	    wprintw(win, "Key pressed: %04o ", c);
#ifdef NCURSES_MOUSE_VERSION
	    if (c == KEY_MOUSE) {
		show_mouse(win);
	    } else
#endif /* NCURSES_MOUSE_VERSION */
	    if (c >= KEY_MIN) {
#if defined(NCURSES_VERSION) && defined(KEY_RESIZE) && HAVE_WRESIZE
		if (c == KEY_RESIZE) {
		    resize_boxes(level, win);
		}
#endif
		(void) waddstr(win, keyname(c));
	    } else if (c >= 0x80) {
		unsigned c2 = (unsigned) c;
#if !(defined(NCURSES_VERSION) || defined(_XOPEN_CURSES))
		/* at least Solaris SVR4 curses breaks unctrl(128), etc. */
		c2 &= 0x7f;
#endif
		if (isprint(c))
		    (void) wprintw(win, "%c", UChar(c));
		else if (c2 != UChar(c))
		    (void) wprintw(win, "M-%s", unctrl(c2));
		else
		    (void) wprintw(win, "%s", unctrl(c2));
		waddstr(win, " (high-half character)");
	    } else {
		if (isprint(c))
		    (void) wprintw(win, "%c (ASCII printable character)", c);
		else
		    (void) wprintw(win, "%s (ASCII control character)",
				   unctrl(UChar(c)));
	    }
	    wgetch_wrap(win, first_y);
	}
    }

    wtimeout(win, -1);

    if (!level)
	init_getch(win, flags, delay);
}

static int
begin_getch_test(void)
{
    char buf[BUFSIZ];
    int delay;

    refresh();

#ifdef NCURSES_MOUSE_VERSION
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, (mmask_t *) 0);
#endif

    (void) printw("Delay in 10ths of a second (<CR> for blocking input)? ");
    echo();
    getnstr(buf, sizeof(buf) - 1);
    noecho();
    nonl();

    if (isdigit(UChar(buf[0]))) {
	delay = atoi(buf) * 100;
    } else {
	delay = -1;
    }
    raw();
    move(6, 0);
    return delay;
}

static void
finish_getch_test(void)
{
#ifdef NCURSES_MOUSE_VERSION
    mousemask(0, (mmask_t *) 0);
#endif
    erase();
    noraw();
    nl();
    endwin();
}

static int
getch_test(bool recur GCC_UNUSED)
{
    int delay = begin_getch_test();

    slk_restore();
    wgetch_test(0, stdscr, delay);
    forget_boxes();
    finish_getch_test();
    slk_clear();
    return OK;
}

#if USE_WIDEC_SUPPORT
/*
 * For wget_wch_test(), we create pairs of windows - one for a box, one for text.
 * Resize both and paint the box in the parent.
 */
#if defined(KEY_RESIZE) && HAVE_WRESIZE
static void
resize_wide_boxes(unsigned level, WINDOW *win)
{
    unsigned n;
    int base = 5;
    int high = LINES - base;
    int wide = COLS;

    touchwin(stdscr);
    wnoutrefresh(stdscr);

    slk_repaint();

    for (n = 0; n < level; ++n) {
	wresize(winstack[n].frame, high, wide);
	wresize(winstack[n].text, high - 2, wide - 2);
	high -= 2;
	wide -= 2;
	werase(winstack[n].text);
	box_set(winstack[n].frame, 0, 0);
	wnoutrefresh(winstack[n].frame);
	wprintw(winstack[n].text,
		"size %dx%d\n",
		getmaxy(winstack[n].text),
		getmaxx(winstack[n].text));
	wnoutrefresh(winstack[n].text);
	if (winstack[n].text == win)
	    break;
    }
    doupdate();
}
#endif /* KEY_RESIZE */

static char *
wcstos(const wchar_t *src)
{
    int need;
    char *result = 0;
    const wchar_t *tmp = src;
#ifndef state_unused
    mbstate_t state;
#endif

    reset_wchars(state);
    if ((need = (int) count_wchars(tmp, 0, &state)) > 0) {
	unsigned have = (unsigned) need;
	if ((result = typeCalloc(char, have + 1)) != 0) {
	    tmp = src;
	    if (trans_wchars(result, tmp, have, &state) != have) {
		free(result);
		result = 0;
	    }
	} else {
	    failed("wcstos");
	}
    }
    return result;
}

static void
wget_wch_test(unsigned level, WINDOW *win, int delay)
{
    wchar_t wchar_buf[BUFSIZ];
    wint_t wint_buf[BUFSIZ];
    int first_y, first_x;
    wint_t c;
    int incount = 0;
    GetchFlags flags;
    char *temp;

    init_getch(win, flags, delay);
    notimeout(win, FALSE);
    wtimeout(win, delay);
    getyx(win, first_y, first_x);

    wgetch_help(win, flags);
    wsetscrreg(win, first_y, getmaxy(win) - 1);
    scrollok(win, TRUE);

    for (;;) {
	int code;

	while ((code = wGet_wchar(win, &c)) == ERR) {
	    incount++;
	    if (blocking_getch(flags, delay)) {
		(void) wprintw(win, "%05d: input error", incount);
		break;
	    } else {
		(void) wprintw(win, "%05d: input timed out", incount);
	    }
	    wgetch_wrap(win, first_y);
	}
	if (code == ERR && blocking_getch(flags, delay)) {
	    wprintw(win, "ERR");
	    wgetch_wrap(win, first_y);
	} else if (isQuit((int) c, ExitOnEscape())) {
	    break;
	} else if (c == 'e') {
	    flags[UChar('e')] = !flags[UChar('e')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 'g') {
	    waddstr(win, "wgetn_str test: ");
	    echo();
	    code = wgetn_wstr(win, wint_buf, BUFSIZ - 1);
	    noecho();
	    if (code == ERR) {
		wprintw(win, "wgetn_wstr returns an error.");
	    } else {
		int n;
		for (n = 0; (wchar_buf[n] = (wchar_t) wint_buf[n]) != 0; ++n) {
		    ;
		}
		if ((temp = wcstos(wchar_buf)) != 0) {
		    wprintw(win, "I saw %d characters:\n\t`%s'.",
			    (int) wcslen(wchar_buf), temp);
		    free(temp);
		} else {
		    wprintw(win, "I saw %d characters (cannot convert).",
			    (int) wcslen(wchar_buf));
		}
	    }
	    wclrtoeol(win);
	    wgetch_wrap(win, first_y);
	} else if (c == 'k') {
	    flags[UChar('k')] = !flags[UChar('k')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 'm') {
	    flags[UChar('m')] = !flags[UChar('m')];
	    setup_getch(win, flags);
	    wgetch_help(win, flags);
	} else if (c == 's') {
	    ShellOut(TRUE);
	} else if (c == 't') {
	    notimeout(win, flags[UChar('t')]);
	    flags[UChar('t')] = !flags[UChar('t')];
	    wgetch_help(win, flags);
	} else if (c == 'w') {
	    int high = getmaxy(win) - 1 - first_y + 1;
	    int wide = getmaxx(win) - first_x;
	    int old_y, old_x;
	    int new_y = first_y + getbegy(win);
	    int new_x = first_x + getbegx(win);

	    getyx(win, old_y, old_x);
	    if (high > 2 && wide > 2) {
		WINDOW *wb = newwin(high, wide, new_y, new_x);
		WINDOW *wi = newwin(high - 2, wide - 2, new_y + 1, new_x + 1);

		box_set(wb, 0, 0);
		wrefresh(wb);
		wmove(wi, 0, 0);
		remember_boxes(level, wi, wb);
		wget_wch_test(level + 1, wi, delay);
		delwin(wi);
		delwin(wb);

		wgetch_help(win, flags);
		wmove(win, old_y, old_x);
		touchwin(win);
		wrefresh(win);
	    }
#ifdef SIGTSTP
	} else if (c == 'z') {
	    kill(getpid(), SIGTSTP);
#endif
	} else {
	    wprintw(win, "Key pressed: %04o ", (int) c);
#ifdef NCURSES_MOUSE_VERSION
	    if (c == KEY_MOUSE) {
		show_mouse(win);
	    } else
#endif /* NCURSES_MOUSE_VERSION */
	    if (code == KEY_CODE_YES) {
#if defined(KEY_RESIZE) && HAVE_WRESIZE
		if (c == KEY_RESIZE) {
		    resize_wide_boxes(level, win);
		}
#endif
		(void) waddstr(win, keyname((wchar_t) c));
	    } else {
		(void) waddstr(win, key_name((wchar_t) c));
		if (c < 256 && iscntrl(c)) {
		    (void) wprintw(win, " (control character)");
		} else {
		    (void) wprintw(win, " = %#x (printable character)",
				   (unsigned) c);
		}
	    }
	    wgetch_wrap(win, first_y);
	}
    }

    wtimeout(win, -1);

    if (!level)
	init_getch(win, flags, delay);
}

static int
x_getch_test(bool recur GCC_UNUSED)
{
    int delay = begin_getch_test();

    slk_restore();
    wget_wch_test(0, stdscr, delay);
    forget_boxes();
    finish_getch_test();
    slk_clear();
    return OK;
}
#endif

/****************************************************************************
 *
 * Character attributes test
 *
 ****************************************************************************/

#if HAVE_SETUPTERM || HAVE_TGETENT
#define get_ncv() TIGETNUM("ncv","NC")
#define get_xmc() TIGETNUM("xmc","sg")
#else
#define get_ncv() -1
#define get_xmc() -1
#endif

#if !HAVE_TERMATTRS
static chtype
my_termattrs(void)
{
    static int first = TRUE;
    static chtype result = 0;

    if (first) {
#if !HAVE_TIGETSTR
	char buffer[4096];
	char parsed[4096];
	char *area_pointer = parsed;

	tgetent(buffer, getenv("TERM"));
#endif

	if (TIGETSTR("smso", "so"))
	    result |= A_STANDOUT;
	if (TIGETSTR("smul", "us"))
	    result |= A_UNDERLINE;
	if (TIGETSTR("rev", "mr"))
	    result |= A_REVERSE;
	if (TIGETSTR("blink", "mb"))
	    result |= A_BLINK;
	if (TIGETSTR("dim", "mh"))
	    result |= A_DIM;
	if (TIGETSTR("bold", "md"))
	    result |= A_BOLD;
	if (TIGETSTR("smacs", "ac"))
	    result |= A_ALTCHARSET;

	first = FALSE;
    }
    return result;
}
#define termattrs() my_termattrs()
#endif

#define ATTRSTRING_1ST 32	/* ' ' */
#define ATTRSTRING_END 126	/* '~' */

#define COLS_PRE_ATTRS 5
#define COLS_AFT_ATTRS 15
#define COL_ATTRSTRING (COLS_PRE_ATTRS + 17)
#define LEN_ATTRSTRING (COLS - (COL_ATTRSTRING + COLS_AFT_ATTRS))
#define MAX_ATTRSTRING (ATTRSTRING_END + 1 - ATTRSTRING_1ST)

static char attr_test_string[MAX_ATTRSTRING + 1];

static void
attr_legend(WINDOW *helpwin)
{
    int row = 1;
    int col = 1;

    MvWPrintw(helpwin, row++, col,
	      "ESC to exit.");
    MvWPrintw(helpwin, row++, col,
	      "^L repaints.");
    ++row;
    MvWPrintw(helpwin, row++, col,
	      "Modify the test strings:");
    MvWPrintw(helpwin, row++, col,
	      "  A digit sets gaps on each side of displayed attributes");
    MvWPrintw(helpwin, row++, col,
	      "  </> shifts the text left/right. ");
    ++row;
    MvWPrintw(helpwin, row++, col,
	      "Toggles:");
    if (UseColors) {
	MvWPrintw(helpwin, row++, col,
		  "  f/F/b/B toggle foreground/background background color");
	MvWPrintw(helpwin, row++, col,
		  "  t/T     toggle text/background color attribute");
    }
    MvWPrintw(helpwin, row++, col,
	      "  a/A     toggle ACS (alternate character set) mapping");
    MvWPrintw(helpwin, row, col,
	      "  v/V     toggle video attribute to combine with each line");
#if USE_WIDEC_SUPPORT
    MvWPrintw(helpwin, row, col,
	      "  w/W     toggle normal/wide (double-width) test-characters");
#endif
}

static void
show_color_attr(int fg, int bg, int tx)
{
    if (UseColors) {
	printw("  Colors (fg %d, bg %d", fg, bg);
	if (tx >= 0)
	    printw(", text %d", tx);
	printw("),");
    }
}

static bool
cycle_color_attr(int ch, NCURSES_COLOR_T *fg, NCURSES_COLOR_T *bg, NCURSES_COLOR_T *tx)
{
    bool error = FALSE;

    if (UseColors) {
	switch (ch) {
	case 'f':
	    *fg = (NCURSES_COLOR_T) (*fg + 1);
	    break;
	case 'F':
	    *fg = (NCURSES_COLOR_T) (*fg - 1);
	    break;
	case 'b':
	    *bg = (NCURSES_COLOR_T) (*bg + 1);
	    break;
	case 'B':
	    *bg = (NCURSES_COLOR_T) (*bg - 1);
	    break;
	case 't':
	    *tx = (NCURSES_COLOR_T) (*tx + 1);
	    break;
	case 'T':
	    *tx = (NCURSES_COLOR_T) (*tx - 1);
	    break;
	default:
	    beep();
	    error = TRUE;
	    break;
	}
	if (*fg >= COLORS)
	    *fg = (NCURSES_COLOR_T) MinColors;
	if (*fg < MinColors)
	    *fg = (NCURSES_COLOR_T) (COLORS - 1);
	if (*bg >= COLORS)
	    *bg = (NCURSES_COLOR_T) MinColors;
	if (*bg < MinColors)
	    *bg = (NCURSES_COLOR_T) (COLORS - 1);
	if (*tx >= COLORS)
	    *tx = -1;
	if (*tx < -1)
	    *tx = (NCURSES_COLOR_T) (COLORS - 1);
    } else {
	beep();
	error = TRUE;
    }
    return error;
}

static void
adjust_attr_string(int adjust)
{
    char save = attr_test_string[0];
    int first = ((int) UChar(save)) + adjust;

    if (first >= ATTRSTRING_1ST) {
	int j, k;

	for (j = 0, k = first; j < MAX_ATTRSTRING; ++j, ++k) {
	    if (k > ATTRSTRING_END)
		break;
	    attr_test_string[j] = (char) k;
	    if (((k + 1 - first) % 5) == 0) {
		if (++j >= MAX_ATTRSTRING)
		    break;
		attr_test_string[j] = ' ';
	    }
	}
	if ((LEN_ATTRSTRING - j) > 5) {
	    attr_test_string[0] = save;
	    adjust_attr_string(adjust - 1);
	} else {
	    while (j < MAX_ATTRSTRING)
		attr_test_string[j++] = ' ';
	    attr_test_string[j] = '\0';
	}
    }
}

/*
 * Prefer the right-end of the string for starting, since that maps to the
 * VT100 line-drawing.
 */
static int
default_attr_string(void)
{
    int result = (ATTRSTRING_END - LEN_ATTRSTRING);
    result += (LEN_ATTRSTRING / 5);
    if (result < ATTRSTRING_1ST)
	result = ATTRSTRING_1ST;
    return result;
}

static void
init_attr_string(void)
{
    attr_test_string[0] = (char) default_attr_string();
    adjust_attr_string(0);
}

static int
show_attr(WINDOW *win, int row, int skip, bool arrow, chtype attr, const char *name)
{
    int ncv = get_ncv();
    chtype test = attr & (chtype) (~(A_ALTCHARSET | A_CHARTEXT));

    if (arrow)
	MvPrintw(row, COLS_PRE_ATTRS - 3, "-->");
    MvPrintw(row, COLS_PRE_ATTRS, "%s mode:", name);
    MvPrintw(row, COL_ATTRSTRING - 1, "|");
    if (skip)
	printw("%*s", skip, " ");
    /*
     * Just for testing, write text using the alternate character set one
     * character at a time (to pass its rendition directly), and use the
     * string operation for the other attributes.
     */
    wmove(win, 0, 0);
    werase(win);
    if (attr & A_ALTCHARSET) {
	const char *s;

	for (s = attr_test_string; *s != '\0'; ++s) {
	    chtype ch = UChar(*s);
	    (void) waddch(win, ch | attr);
	}
    } else {
	(void) wattrset(win, AttrArg(attr, 0));
	(void) waddstr(win, attr_test_string);
	(void) wattroff(win, (int) attr);
    }
    if (skip)
	printw("%*s", skip, " ");
    MvPrintw(row, COL_ATTRSTRING + LEN_ATTRSTRING, "|");
    if (test != A_NORMAL) {
	if (!(termattrs() & test)) {
	    printw(" (N/A)");
	} else {
	    if (ncv > 0 && stdscr && (getbkgd(stdscr) & A_COLOR)) {
		static const chtype table[] =
		{
		    A_STANDOUT,
		    A_UNDERLINE,
		    A_REVERSE,
		    A_BLINK,
		    A_DIM,
		    A_BOLD,
#ifdef A_INVIS
		    A_INVIS,
#endif
#ifdef A_ITALIC
		    A_ITALIC,
#endif
		    A_PROTECT,
		    A_ALTCHARSET
		};
		unsigned n;
		bool found = FALSE;
		for (n = 0; n < SIZEOF(table); n++) {
		    if ((table[n] & attr) != 0
			&& ((1 << n) & ncv) != 0) {
			found = TRUE;
			break;
		    }
		}
		if (found)
		    printw(" (NCV)");
	    }
	    if ((termattrs() & test) != test) {
		printw(" (Part)");
	    }
	}
    }
    return row + 2;
}

typedef struct {
    chtype attr;
    NCURSES_CONST char *name;
} ATTR_TBL;
/* *INDENT-OFF* */
static const ATTR_TBL attrs_to_test[] = {
    { A_STANDOUT,	"STANDOUT" },
    { A_REVERSE,	"REVERSE" },
    { A_BOLD,		"BOLD" },
    { A_UNDERLINE,	"UNDERLINE" },
    { A_DIM,		"DIM" },
    { A_BLINK,		"BLINK" },
    { A_PROTECT,	"PROTECT" },
#ifdef A_INVIS
    { A_INVIS,		"INVISIBLE" },
#endif
#ifdef A_ITALIC
    { A_ITALIC,		"ITALIC" },
#endif
    { A_NORMAL,		"NORMAL" },
};
/* *INDENT-ON* */

static unsigned
init_attr_list(ATTR_TBL * target, attr_t attrs)
{
    unsigned result = 0;
    size_t n;

    for (n = 0; n < SIZEOF(attrs_to_test); ++n) {
	attr_t test = attrs_to_test[n].attr;
	if (test == A_NORMAL || (test & attrs) != 0) {
	    target[result++] = attrs_to_test[n];
	}
    }
    return result;
}

#if USE_WIDEC_SUPPORT
typedef struct {
    attr_t attr;
    NCURSES_CONST char *name;
} W_ATTR_TBL;
/* *INDENT-OFF* */
static const W_ATTR_TBL w_attrs_to_test[] = {
    { WA_STANDOUT,	"STANDOUT" },
    { WA_REVERSE,	"REVERSE" },
    { WA_BOLD,		"BOLD" },
    { WA_UNDERLINE,	"UNDERLINE" },
    { WA_DIM,		"DIM" },
    { WA_BLINK,		"BLINK" },
    { WA_PROTECT,	"PROTECT" },
#ifdef WA_INVIS
    { WA_INVIS,		"INVISIBLE" },
#endif
#ifdef WA_ITALIC
    { WA_ITALIC,	"ITALIC" },
#endif
    { WA_NORMAL,	"NORMAL" },
};
/* *INDENT-ON* */

static unsigned
init_w_attr_list(W_ATTR_TBL * target, attr_t attrs)
{
    unsigned result = 0;
    size_t n;

    for (n = 0; n < SIZEOF(w_attrs_to_test); ++n) {
	attr_t test = w_attrs_to_test[n].attr;
	if (test == WA_NORMAL || (test & attrs) != 0) {
	    target[result++] = w_attrs_to_test[n];
	}
    }
    return result;
}
#endif

static bool
attr_getc(int *skip,
	  NCURSES_COLOR_T *fg,
	  NCURSES_COLOR_T *bg,
	  NCURSES_COLOR_T *tx,
	  int *ac,
	  unsigned *kc,
	  unsigned limit)
{
    bool result = TRUE;
    bool error = FALSE;
    WINDOW *helpwin;

    do {
	int ch = Getchar();

	error = FALSE;
	if (ch < 256 && isdigit(ch)) {
	    *skip = (ch - '0');
	} else {
	    switch (ch) {
	    case CTRL('L'):
		Repaint();
		break;
	    case HELP_KEY_1:
		if ((helpwin = newwin(LINES - 1, COLS - 2, 0, 0)) != 0) {
		    box(helpwin, 0, 0);
		    attr_legend(helpwin);
		    wGetchar(helpwin);
		    delwin(helpwin);
		}
		break;
	    case 'a':
		*ac = 0;
		break;
	    case 'A':
		*ac = A_ALTCHARSET;
		break;
	    case 'v':
		if (*kc == 0)
		    *kc = limit - 1;
		else
		    *kc -= 1;
		break;
	    case 'V':
		*kc += 1;
		if (*kc >= limit)
		    *kc = 0;
		break;
	    case '<':
		adjust_attr_string(-1);
		break;
	    case '>':
		adjust_attr_string(1);
		break;
	    case case_QUIT:
		result = FALSE;
		break;
	    default:
		error = cycle_color_attr(ch, fg, bg, tx);
		break;
	    }
	}
    } while (error);
    return result;
}

static int
attr_test(bool recur GCC_UNUSED)
/* test text attributes */
{
    int n;
    int skip = get_xmc();
    NCURSES_COLOR_T fg = COLOR_BLACK;	/* color pair 0 is special */
    NCURSES_COLOR_T bg = COLOR_BLACK;
    NCURSES_COLOR_T tx = -1;
    int ac = 0;
    WINDOW *my_wins[SIZEOF(attrs_to_test)];
    ATTR_TBL my_list[SIZEOF(attrs_to_test)];
    unsigned my_size = init_attr_list(my_list, termattrs());

    if (my_size > 1) {
	unsigned j, k;

	for (j = 0; j < my_size; ++j) {
	    my_wins[j] = subwin(stdscr,
				1, LEN_ATTRSTRING,
				2 + (int) (2 * j), COL_ATTRSTRING);
	    scrollok(my_wins[j], FALSE);
	}

	if (skip < 0)
	    skip = 0;

	n = skip;		/* make it easy */
	k = my_size - 1;
	init_attr_string();

	do {
	    int row = 2;
	    chtype normal = A_NORMAL | BLANK;
	    chtype extras = (chtype) ac;

	    if (UseColors) {
		NCURSES_PAIRS_T pair = 0;
		if ((fg != COLOR_BLACK) || (bg != COLOR_BLACK)) {
		    pair = 1;
		    if (init_pair(pair, fg, bg) == ERR) {
			beep();
		    } else {
			normal |= (chtype) COLOR_PAIR(pair);
		    }
		}
		if (tx >= 0) {
		    pair = 2;
		    if (init_pair(pair, tx, bg) == ERR) {
			beep();
		    } else {
			extras |= (chtype) COLOR_PAIR(pair);
			normal &= ~A_COLOR;
		    }
		}
	    }
	    bkgd(normal);
	    bkgdset(normal);
	    erase();

	    box(stdscr, 0, 0);
	    MvAddStr(0, 20, "Character attribute test display");

	    for (j = 0; j < my_size; ++j) {
		bool arrow = (j == k);
		row = show_attr(my_wins[j], row, n, arrow,
				normal |
				extras |
				my_list[j].attr |
				my_list[k].attr,
				my_list[j].name);
	    }

	    MvPrintw(row, COLS_PRE_ATTRS,
		     "This terminal does %shave the magic-cookie glitch",
		     get_xmc() > -1 ? "" : "not ");
	    MvPrintw(row + 1, COLS_PRE_ATTRS, "Enter '?' for help.");
	    show_color_attr(fg, bg, tx);
	    printw("  ACS (%d)", ac != 0);

	    refresh();
	} while (attr_getc(&n, &fg, &bg, &tx, &ac, &k, my_size));

	bkgdset(A_NORMAL | BLANK);
	erase();
	endwin();
	return OK;
    } else {
	Cannot("does not support video attributes.");
	return ERR;
    }
}

#if USE_WIDEC_SUPPORT
static bool use_fullwidth;
static wchar_t wide_attr_test_string[MAX_ATTRSTRING + 1];

#define FULL_LO 0xff00
#define FULL_HI 0xff5e
#define HALF_LO 0x20

#define isFullWidth(ch)   ((int)(ch) >= FULL_LO && (int)(ch) <= FULL_HI)
#define ToNormalWidth(ch) (wchar_t) (((int)(ch) - FULL_LO) + HALF_LO)
#define ToFullWidth(ch)   (wchar_t) (((int)(ch) - HALF_LO) + FULL_LO)

/*
 * Returns an ASCII code in [32..126]
 */
static wchar_t
normal_wchar(int ch)
{
    wchar_t result = (wchar_t) ch;
    if (isFullWidth(ch))
	result = ToNormalWidth(ch);
    return result;
}

/*
 * Returns either an ASCII code in in [32..126] or full-width in
 * [0xff00..0xff5e], according to use_fullwidth setting.
 */
static wchar_t
target_wchar(int ch)
{
    wchar_t result = (wchar_t) ch;
    if (use_fullwidth) {
	if (!isFullWidth(ch))
	    result = ToFullWidth(ch);
    } else {
	if (isFullWidth(ch))
	    result = ToNormalWidth(ch);
    }
    return result;
}

static void
wide_adjust_attr_string(int adjust)
{
    wchar_t save = wide_attr_test_string[0];
    int first = ((int) normal_wchar(save)) + adjust;

    if (first >= ATTRSTRING_1ST) {
	int j, k;

	for (j = 0, k = first; j < MAX_ATTRSTRING; ++j, ++k) {
	    if (k > ATTRSTRING_END)
		break;
	    wide_attr_test_string[j] = target_wchar(k);
	    if (((k + 1 - first) % 5) == 0) {
		if (++j >= MAX_ATTRSTRING)
		    break;
		wide_attr_test_string[j] = ' ';
	    }
	}
	if ((LEN_ATTRSTRING - j) > 5) {
	    wide_attr_test_string[0] = save;
	    wide_adjust_attr_string(adjust - 1);
	} else {
	    while (j < MAX_ATTRSTRING)
		wide_attr_test_string[j++] = ' ';
	    wide_attr_test_string[j] = '\0';
	}
    }
}

static void
wide_init_attr_string(void)
{
    use_fullwidth = FALSE;
    wide_attr_test_string[0] = (wchar_t) default_attr_string();
    wide_adjust_attr_string(0);
}

static void
set_wide_background(NCURSES_PAIRS_T pair)
{
    cchar_t normal;
    wchar_t blank[2];

    blank[0] = ' ';
    blank[1] = 0;
    setcchar(&normal, blank, A_NORMAL, pair, 0);
    bkgrnd(&normal);
    bkgrndset(&normal);
}

static attr_t
get_wide_background(void)
{
    attr_t result = WA_NORMAL;
    attr_t attr;
    cchar_t ch;
    NCURSES_PAIRS_T pair;

    memset(&ch, 0, sizeof(ch));
    if (getbkgrnd(&ch) != ERR) {
	wchar_t wch[CCHARW_MAX];

	if (getcchar(&ch, wch, &attr, &pair, 0) != ERR) {
	    result = attr;
	}
    }
    return result;
}

static int
wide_show_attr(WINDOW *win,
	       int row,
	       int skip,
	       bool arrow,
	       attr_t attr,
	       NCURSES_PAIRS_T pair,
	       const char *name)
{
    int ncv = get_ncv();
    attr_t test = attr & ~WA_ALTCHARSET;

    if (arrow)
	MvPrintw(row, COLS_PRE_ATTRS - 3, "-->");
    MvPrintw(row, COLS_PRE_ATTRS, "%s mode:", name);
    MvPrintw(row, COL_ATTRSTRING - 1, "|");
    if (skip)
	printw("%*s", skip, " ");

    /*
     * Just for testing, write text using the alternate character set one
     * character at a time (to pass its rendition directly), and use the
     * string operation for the other attributes.
     */
    wmove(win, 0, 0);
    werase(win);
    if (attr & WA_ALTCHARSET) {
	const wchar_t *s;
	cchar_t ch;

	for (s = wide_attr_test_string; *s != L'\0'; ++s) {
	    wchar_t fill[2];
	    fill[0] = *s;
	    fill[1] = L'\0';
	    setcchar(&ch, fill, attr, pair, 0);
	    (void) wadd_wch(win, &ch);
	}
    } else {
	attr_t old_attr = 0;
	NCURSES_PAIRS_T old_pair = 0;

	(void) (wattr_get) (win, &old_attr, &old_pair, 0);
	(void) wattr_set(win, attr, pair, 0);
	(void) waddwstr(win, wide_attr_test_string);
	(void) wattr_set(win, old_attr, old_pair, 0);
    }
    if (skip)
	printw("%*s", skip, " ");
    MvPrintw(row, COL_ATTRSTRING + LEN_ATTRSTRING, "|");
    if (test != A_NORMAL) {
	if (!(term_attrs() & test)) {
	    printw(" (N/A)");
	} else {
	    if (ncv > 0 && (get_wide_background() & A_COLOR)) {
		static const attr_t table[] =
		{
		    WA_STANDOUT,
		    WA_UNDERLINE,
		    WA_REVERSE,
		    WA_BLINK,
		    WA_DIM,
		    WA_BOLD,
		    WA_INVIS,
		    WA_PROTECT,
		    WA_ALTCHARSET
		};
		unsigned n;
		bool found = FALSE;
		for (n = 0; n < SIZEOF(table); n++) {
		    if ((table[n] & attr) != 0
			&& ((1 << n) & ncv) != 0) {
			found = TRUE;
			break;
		    }
		}
		if (found)
		    printw(" (NCV)");
	    }
	    if ((term_attrs() & test) != test) {
		printw(" (Part)");
	    }
	}
    }
    return row + 2;
}

static bool
wide_attr_getc(int *skip,
	       NCURSES_COLOR_T *fg, NCURSES_COLOR_T *bg,
	       NCURSES_COLOR_T *tx, int *ac,
	       unsigned *kc, unsigned limit)
{
    bool result = TRUE;
    bool error = FALSE;
    WINDOW *helpwin;

    do {
	int ch = Getchar();

	error = FALSE;
	if (ch < 256 && isdigit(ch)) {
	    *skip = (ch - '0');
	} else {
	    switch (ch) {
	    case CTRL('L'):
		Repaint();
		break;
	    case HELP_KEY_1:
		if ((helpwin = newwin(LINES - 1, COLS - 2, 0, 0)) != 0) {
		    box_set(helpwin, 0, 0);
		    attr_legend(helpwin);
		    wGetchar(helpwin);
		    delwin(helpwin);
		}
		break;
	    case 'a':
		*ac = 0;
		break;
	    case 'A':
		*ac = A_ALTCHARSET;
		break;
	    case 'v':
		if (*kc == 0)
		    *kc = limit - 1;
		else
		    *kc -= 1;
		break;
	    case 'V':
		*kc += 1;
		if (*kc >= limit)
		    *kc = 0;
		break;
	    case 'w':
		use_fullwidth = FALSE;
		wide_adjust_attr_string(0);
		break;
	    case 'W':
		use_fullwidth = TRUE;
		wide_adjust_attr_string(0);
		break;
	    case '<':
		wide_adjust_attr_string(-1);
		break;
	    case '>':
		wide_adjust_attr_string(1);
		break;
	    case case_QUIT:
		result = FALSE;
		break;
	    default:
		error = cycle_color_attr(ch, fg, bg, tx);
		break;
	    }
	}
    } while (error);
    return result;
}

static int
x_attr_test(bool recur GCC_UNUSED)
/* test text attributes using wide-character calls */
{
    int n;
    int skip = get_xmc();
    NCURSES_COLOR_T fg = COLOR_BLACK;	/* color pair 0 is special */
    NCURSES_COLOR_T bg = COLOR_BLACK;
    NCURSES_COLOR_T tx = -1;
    int ac = 0;
    W_ATTR_TBL my_list[SIZEOF(w_attrs_to_test)];
    WINDOW *my_wins[SIZEOF(w_attrs_to_test)];
    unsigned my_size = init_w_attr_list(my_list, term_attrs());

    if (my_size > 1) {
	unsigned j, k;

	for (j = 0; j < my_size; ++j) {
	    my_wins[j] = subwin(stdscr,
				1, LEN_ATTRSTRING,
				2 + (int) (2 * j), COL_ATTRSTRING);
	    scrollok(my_wins[j], FALSE);
	}

	if (skip < 0)
	    skip = 0;

	n = skip;		/* make it easy */
	k = my_size - 1;
	wide_init_attr_string();

	do {
	    int row = 2;
	    NCURSES_PAIRS_T pair = 0;
	    NCURSES_PAIRS_T extras = 0;

	    if (UseColors) {
		pair = (NCURSES_PAIRS_T) (fg != COLOR_BLACK || bg != COLOR_BLACK);
		if (pair != 0) {
		    pair = 1;
		    if (init_pair(pair, fg, bg) == ERR) {
			beep();
		    }
		}
		extras = pair;
		if (tx >= 0) {
		    extras = 2;
		    if (init_pair(extras, tx, bg) == ERR) {
			beep();
		    }
		}
	    }
	    set_wide_background(pair);
	    erase();

	    box_set(stdscr, 0, 0);
	    MvAddStr(0, 20, "Character attribute test display");

	    for (j = 0; j < my_size; ++j) {
		row = wide_show_attr(my_wins[j], row, n, (j == k),
				     ((attr_t) ac |
				      my_list[j].attr |
				      my_list[k].attr),
				     extras,
				     my_list[j].name);
	    }

	    MvPrintw(row, COLS_PRE_ATTRS,
		     "This terminal does %shave the magic-cookie glitch",
		     get_xmc() > -1 ? "" : "not ");
	    MvPrintw(row + 1, COLS_PRE_ATTRS, "Enter '?' for help.");
	    show_color_attr(fg, bg, tx);
	    printw("  ACS (%d)", ac != 0);

	    refresh();
	} while (wide_attr_getc(&n, &fg, &bg, &tx, &ac, &k, my_size));

	set_wide_background(0);
	erase();
	endwin();
	return OK;
    } else {
	Cannot("does not support extended video attributes.");
	return ERR;
    }
}
#endif

/****************************************************************************
 *
 * Color support tests
 *
 ****************************************************************************/

static NCURSES_CONST char *the_color_names[] =
{
    "black",
    "red",
    "green",
    "yellow",
    "blue",
    "magenta",
    "cyan",
    "white",
    "BLACK",
    "RED",
    "GREEN",
    "YELLOW",
    "BLUE",
    "MAGENTA",
    "CYAN",
    "WHITE"
};

static void
show_color_name(int y, int x, int color, bool wide, int zoom)
{
    if (move(y, x) != ERR) {
	char temp[80];
	int width = 8;

	if (wide || zoom) {
	    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
			"%02d", color);
	    if (wide)
		width = 4;
	    if ((int) strlen(temp) >= width) {
		int pwr2 = 0;
		while ((1 << pwr2) < color)
		    ++pwr2;
		_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
			    width > 4 ? "2^%d" : "^%d", pwr2);
	    }
	} else if (color >= 8) {
	    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
			"[%02d]", color);
	} else if (color < 0) {
	    _nc_STRCPY(temp, "default", sizeof(temp));
	} else {
	    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
			"%.*s", 16, the_color_names[color]);
	}
	printw("%-*.*s", width, width, temp);
    }
}

static void
color_legend(WINDOW *helpwin, bool wide)
{
    int row = 1;
    int col = 1;

    MvWPrintw(helpwin, row++, col,
	      "ESC to exit.");
    ++row;
    MvWPrintw(helpwin, row++, col,
	      "Use up/down arrow to scroll through the display if it is");
    MvWPrintw(helpwin, row++, col,
	      "longer than one screen. Control/N and Control/P can be used");
    MvWPrintw(helpwin, row++, col,
	      "in place of up/down arrow.  Use pageup/pagedown to scroll a");
    MvWPrintw(helpwin, row++, col,
	      "full screen; control/B and control/F can be used here.");
    ++row;
    MvWPrintw(helpwin, row++, col,
	      "Toggles:");
    MvWPrintw(helpwin, row++, col,
	      "  a/A     toggle altcharset off/on");
    MvWPrintw(helpwin, row++, col,
	      "  b/B     toggle bold off/on");
    if (has_colors()) {
	MvWPrintw(helpwin, row++, col,
		  "  c/C     cycle used-colors through 8,16,...,COLORS");
    }
    MvWPrintw(helpwin, row++, col,
	      "  n/N     toggle text/number on/off");
    MvWPrintw(helpwin, row++, col,
	      "  r/R     toggle reverse on/off");
    MvWPrintw(helpwin, row++, col,
	      "  w/W     switch width between 4/8 columns");
    MvWPrintw(helpwin, row++, col,
	      "  z/Z     zoom out (or in)");
#if USE_WIDEC_SUPPORT
    if (wide) {
	MvWPrintw(helpwin, row++, col,
		  "Wide characters:");
	MvWPrintw(helpwin, row, col,
		  "  x/X     toggle text between ASCII and wide-character");
    }
#else
    (void) wide;
#endif
}

#define set_color_test(name, value) if (name != value) { name = value; base_row = 0; }

static int
color_cycle(int current, int step)
{
    int result = current;
    if (step < 0) {
	if (current <= 8) {
	    result = COLORS;
	} else {
	    result = 8;
	    if ((result * 2) > COLORS) {
		result = COLORS;
	    } else {
		while ((result * 2) < current) {
		    result *= 2;
		}
	    }
	}
    } else {
	if (current >= COLORS) {
	    result = 8;
	} else {
	    result *= 2;
	}
	if (result > COLORS)
	    result = COLORS;
    }
    return result;
}

/* generate a color test pattern */
static int
color_test(bool recur GCC_UNUSED)
{
    NCURSES_PAIRS_T i;
    int top = 0, width;
    int base_row = 0;
    int grid_top = top + 3;
    int page_size = (LINES - grid_top);
    int pairs_max;
    int colors_max = COLORS;
    int col_limit;
    int row_limit;
    int per_row;
    char *numbered = 0;
    const char *hello;
    bool done = FALSE;
    bool opt_acsc = FALSE;
    bool opt_bold = FALSE;
    bool opt_revs = FALSE;
    bool opt_nums = FALSE;
    bool opt_wide = FALSE;
    int opt_zoom = 0;
    WINDOW *helpwin;

    if (!UseColors) {
	Cannot("does not support color.");
	return ERR;
    }

    numbered = typeCalloc(char, COLS + 1);
    done = ((COLS < 16) || (numbered == 0));

    /*
     * Because the number of colors is usually a power of two, we also use
     * a power of two for the number of colors shown per line (to be tidy).
     */
    for (col_limit = 1; col_limit * 2 < COLS; col_limit *= 2) ;

  reloop:
    while (!done) {
	int shown = 0;
	int zoom_size = (1 << opt_zoom);
	int colors_max1 = colors_max / zoom_size;
	double colors_max2 = (double) colors_max1 * (double) colors_max1;

	pairs_max = PAIR_NUMBER(A_COLOR) + 1;
	if (colors_max2 <= COLOR_PAIRS) {
	    int limit = (colors_max1 - MinColors) * (colors_max1 - MinColors);
	    if (pairs_max > limit)
		pairs_max = limit;
	}
	if (pairs_max > COLOR_PAIRS)
	    pairs_max = COLOR_PAIRS;
	if (pairs_max < colors_max1)
	    pairs_max = colors_max1;

	/* this assumes an 80-column line */
	if (opt_wide) {
	    width = 4;
	    hello = "Test";
	    per_row = (col_limit / ((colors_max1 > 8) ? width : 8));
	} else {
	    width = 8;
	    hello = "Hello";
	    per_row = (col_limit / width);
	}
	per_row -= MinColors;

	row_limit = (pairs_max + per_row - 1) / per_row;

	move(0, 0);
	(void) printw("There are %d color pairs and %d colors",
		      pairs_max, COLORS);
	if (colors_max1 != COLORS)
	    (void) printw(" (using %d colors)", colors_max1);
	if (MinColors)
	    (void) addstr(" besides 'default'");
	if (opt_zoom)
	    (void) printw(" zoom:%d", opt_zoom);

	clrtobot();
	MvPrintw(top + 1, 0,
		 "%dx%d matrix of foreground/background colors, bold *%s*\n",
		 row_limit,
		 per_row,
		 opt_bold ? "on" : "off");

	/* show color names/numbers across the top */
	for (i = 0; i < per_row; i++) {
	    show_color_name(top + 2,
			    (i + 1) * width,
			    (int) i * zoom_size + MinColors,
			    opt_wide,
			    opt_zoom);
	}

	/* show a grid of colors, with color names/ numbers on the left */
	for (i = (NCURSES_PAIRS_T) (base_row * per_row); i < pairs_max; i++) {
	    int row = grid_top + (i / per_row) - base_row;
	    int col = (i % per_row + 1) * width;
	    NCURSES_PAIRS_T pair = i;

	    if ((i / per_row) > row_limit)
		break;

#define InxToFG(i) (int)((((unsigned long)(i) * (unsigned long)zoom_size) % (unsigned long)(colors_max1 - MinColors)) + (unsigned long)MinColors)
#define InxToBG(i) (int)((((unsigned long)(i) * (unsigned long)zoom_size) / (unsigned long)(colors_max1 - MinColors)) + (unsigned long)MinColors)
	    if (row >= 0 && move(row, col) != ERR) {
		NCURSES_COLOR_T fg = (NCURSES_COLOR_T) InxToFG(i);
		NCURSES_COLOR_T bg = (NCURSES_COLOR_T) InxToBG(i);

		init_pair(pair, fg, bg);
		attron(COLOR_PAIR(pair));
		if (opt_acsc)
		    attron(A_ALTCHARSET);
		if (opt_bold)
		    attron(A_BOLD);
		if (opt_revs)
		    attron(A_REVERSE);

		if (opt_nums) {
		    _nc_SPRINTF(numbered, _nc_SLIMIT((size_t) (COLS + 1))
				"{%02X}", (int) i);
		    hello = numbered;
		}
		printw("%-*.*s", width, width, hello);
		(void) attrset(A_NORMAL);

		if ((i % per_row) == 0 && InxToFG(i) == MinColors) {
		    show_color_name(row, 0,
				    InxToBG(i),
				    opt_wide,
				    opt_zoom);
		}
		++shown;
	    } else if (shown) {
		break;
	    }
	}

	switch (wGetchar(stdscr)) {
	case 'a':
	    opt_acsc = FALSE;
	    break;
	case 'A':
	    opt_acsc = TRUE;
	    break;
	case 'b':
	    opt_bold = FALSE;
	    break;
	case 'B':
	    opt_bold = TRUE;
	    break;
	case 'c':
	    colors_max = color_cycle(colors_max, -1);
	    break;
	case 'C':
	    colors_max = color_cycle(colors_max, 1);
	    break;
	case 'n':
	    opt_nums = FALSE;
	    break;
	case 'N':
	    opt_nums = TRUE;
	    break;
	case 'r':
	    opt_revs = FALSE;
	    break;
	case 'R':
	    opt_revs = TRUE;
	    break;
	case case_QUIT:
	    done = TRUE;
	    continue;
	case 'w':
	    set_color_test(opt_wide, FALSE);
	    break;
	case 'W':
	    set_color_test(opt_wide, TRUE);
	    break;
	case 'z':
	    if (opt_zoom <= 0) {
		beep();
	    } else {
		--opt_zoom;
		goto reloop;
	    }
	    break;
	case 'Z':
	    if ((1 << opt_zoom) >= colors_max) {
		beep();
	    } else {
		++opt_zoom;
		goto reloop;
	    }
	    break;
	case CTRL('p'):
	case KEY_UP:
	    if (base_row <= 0) {
		beep();
	    } else {
		base_row -= 1;
	    }
	    break;
	case CTRL('n'):
	case KEY_DOWN:
	    if (base_row + page_size >= row_limit) {
		beep();
	    } else {
		base_row += 1;
	    }
	    break;
	case CTRL('b'):
	case KEY_PREVIOUS:
	case KEY_PPAGE:
	    if (base_row <= 0) {
		beep();
	    } else {
		base_row -= (page_size - 1);
		if (base_row < 0)
		    base_row = 0;
	    }
	    break;
	case CTRL('f'):
	case KEY_NEXT:
	case KEY_NPAGE:
	    if (base_row + page_size >= row_limit) {
		beep();
	    } else {
		base_row += page_size - 1;
		if (base_row + page_size >= row_limit) {
		    base_row = row_limit - page_size - 1;
		}
	    }
	    break;
	case HELP_KEY_1:
	    if ((helpwin = newwin(LINES - 1, COLS - 2, 0, 0)) != 0) {
		box(helpwin, 0, 0);
		color_legend(helpwin, FALSE);
		wGetchar(helpwin);
		delwin(helpwin);
	    }
	    break;
	default:
	    beep();
	    continue;
	}
    }

    erase();
    endwin();

    free(numbered);
    return OK;
}

#if USE_WIDEC_SUPPORT

#if USE_EXTENDED_COLOR
#define InitExtendedPair(p,f,g) init_extended_pair((p),(f),(g))
#define ExtendedColorSet(p)     color_set((NCURSES_PAIRS_T) (p), &(p))
#define EXTENDED_PAIRS_T int
#else
#define InitExtendedPair(p,f,g) init_pair((NCURSES_PAIRS_T) (p),(NCURSES_COLOR_T)(f),(NCURSES_COLOR_T)(g))
#define ExtendedColorSet(p)     color_set((NCURSES_PAIRS_T) (p), NULL)
#define EXTENDED_PAIRS_T NCURSES_PAIRS_T
#endif

/* generate a color test pattern */
static int
x_color_test(bool recur GCC_UNUSED)
{
    long i;
    int top = 0, width;
    int base_row = 0;
    int grid_top = top + 3;
    int page_size = (LINES - grid_top);
    int pairs_max;
    int colors_max = COLORS;
    int col_limit;
    int row_limit;
    int per_row;
    char *numbered = 0;
    const char *hello;
    bool done = FALSE;
    bool opt_acsc = FALSE;
    bool opt_bold = FALSE;
    bool opt_revs = FALSE;
    bool opt_wide = FALSE;
    bool opt_nums = FALSE;
    bool opt_xchr = FALSE;
    int opt_zoom = 0;
    wchar_t *buffer = 0;
    WINDOW *helpwin;

    if (!UseColors) {
	Cannot("does not support color.");
	return ERR;
    }
    numbered = typeCalloc(char, COLS + 1);
    buffer = typeCalloc(wchar_t, COLS + 1);
    done = ((COLS < 16) || (numbered == 0) || (buffer == 0));

    /*
     * Because the number of colors is usually a power of two, we also use
     * a power of two for the number of colors shown per line (to be tidy).
     */
    for (col_limit = 1; col_limit * 2 < COLS; col_limit *= 2) ;

  reloop:
    while (!done) {
	int shown = 0;
	int zoom_size = (1 << opt_zoom);
	int colors_max1 = colors_max / zoom_size;
	double colors_max2 = (double) colors_max1 * (double) colors_max1;

	pairs_max = ((unsigned) (-1)) / 2;
	if (colors_max2 <= COLOR_PAIRS) {
	    int limit = (colors_max1 - MinColors) * (colors_max1 - MinColors);
	    if (pairs_max > limit)
		pairs_max = limit;
	}
	if (pairs_max > COLOR_PAIRS)
	    pairs_max = COLOR_PAIRS;
	if (pairs_max < colors_max1)
	    pairs_max = colors_max1;

	if (opt_wide) {
	    width = 4;
	    hello = "Test";
	    per_row = (col_limit / ((colors_max1 > 8) ? width : 8));
	} else {
	    width = 8;
	    hello = "Hello";
	    per_row = (col_limit / width);
	}
	per_row -= MinColors;

	if (opt_xchr) {
	    make_fullwidth_text(buffer, hello);
	    width *= 2;
	    per_row /= 2;
	} else {
	    make_narrow_text(buffer, hello);
	}

	row_limit = (pairs_max + per_row - 1) / per_row;

	move(0, 0);
	(void) printw("There are %d color pairs and %d colors",
		      pairs_max, COLORS);
	if (colors_max1 != COLORS)
	    (void) printw(" (using %d colors)", colors_max1);
	if (MinColors)
	    (void) addstr(" besides 'default'");
	if (opt_zoom)
	    (void) printw(" zoom:%d", opt_zoom);

	clrtobot();
	MvPrintw(top + 1, 0,
		 "%dx%d matrix of foreground/background colors, bold *%s*\n",
		 row_limit,
		 per_row,
		 opt_bold ? "on" : "off");

	/* show color names/numbers across the top */
	for (i = 0; i < per_row; i++) {
	    show_color_name(top + 2,
			    ((int) i + 1) * width,
			    (int) i * zoom_size + MinColors,
			    opt_wide,
			    opt_zoom);
	}

	/* show a grid of colors, with color names/ numbers on the left */
	for (i = (base_row * per_row); i < pairs_max; i++) {
	    int row = grid_top + ((int) i / per_row) - base_row;
	    int col = ((int) i % per_row + 1) * width;
	    int pair = (int) i;

	    if ((i / per_row) > row_limit)
		break;

	    if (row >= 0 && move(row, col) != ERR) {
		InitExtendedPair(pair, InxToFG(i), InxToBG(i));
		(void) ExtendedColorSet(pair);
		if (opt_acsc)
		    attr_on(WA_ALTCHARSET, NULL);
		if (opt_bold)
		    attr_on(WA_BOLD, NULL);
		if (opt_revs)
		    attr_on(WA_REVERSE, NULL);

		if (opt_nums) {
		    _nc_SPRINTF(numbered,
				_nc_SLIMIT((size_t) (COLS + 1) * sizeof(wchar_t))
				"{%02X}", (unsigned) i);
		    if (opt_xchr) {
			make_fullwidth_text(buffer, numbered);
		    } else {
			make_narrow_text(buffer, numbered);
		    }
		}
		addnwstr(buffer, width);
		(void) attr_set(A_NORMAL, 0, NULL);

		if ((i % per_row) == 0 && InxToFG(i) == MinColors) {
		    show_color_name(row, 0,
				    InxToBG(i),
				    opt_wide,
				    opt_zoom);
		}
		++shown;
	    } else if (shown) {
		break;
	    }
	}

	switch (wGetchar(stdscr)) {
	case 'a':
	    opt_acsc = FALSE;
	    break;
	case 'A':
	    opt_acsc = TRUE;
	    break;
	case 'b':
	    opt_bold = FALSE;
	    break;
	case 'B':
	    opt_bold = TRUE;
	    break;
	case 'c':
	    colors_max = color_cycle(colors_max, -1);
	    break;
	case 'C':
	    colors_max = color_cycle(colors_max, 1);
	    break;
	case 'n':
	    opt_nums = FALSE;
	    break;
	case 'N':
	    opt_nums = TRUE;
	    break;
	case 'r':
	    opt_revs = FALSE;
	    break;
	case 'R':
	    opt_revs = TRUE;
	    break;
	case case_QUIT:
	    done = TRUE;
	    continue;
	case 'w':
	    set_color_test(opt_wide, FALSE);
	    break;
	case 'W':
	    set_color_test(opt_wide, TRUE);
	    break;
	case 'x':
	    opt_xchr = FALSE;
	    break;
	case 'X':
	    opt_xchr = TRUE;
	    break;
	case 'z':
	    if (opt_zoom <= 0) {
		beep();
	    } else {
		--opt_zoom;
		goto reloop;
	    }
	    break;
	case 'Z':
	    if ((1 << opt_zoom) >= colors_max) {
		beep();
	    } else {
		++opt_zoom;
		goto reloop;
	    }
	    break;
	case CTRL('p'):
	case KEY_UP:
	    if (base_row <= 0) {
		beep();
	    } else {
		base_row -= 1;
	    }
	    break;
	case CTRL('n'):
	case KEY_DOWN:
	    if (base_row + page_size >= row_limit) {
		beep();
	    } else {
		base_row += 1;
	    }
	    break;
	case CTRL('b'):
	case KEY_PREVIOUS:
	case KEY_PPAGE:
	    if (base_row <= 0) {
		beep();
	    } else {
		base_row -= (page_size - 1);
		if (base_row < 0)
		    base_row = 0;
	    }
	    break;
	case CTRL('f'):
	case KEY_NEXT:
	case KEY_NPAGE:
	    if (base_row + page_size >= row_limit) {
		beep();
	    } else {
		base_row += page_size - 1;
		if (base_row + page_size >= row_limit) {
		    base_row = row_limit - page_size - 1;
		}
	    }
	    break;
	case HELP_KEY_1:
	    if ((helpwin = newwin(LINES - 1, COLS - 2, 0, 0)) != 0) {
		box(helpwin, 0, 0);
		color_legend(helpwin, TRUE);
		wGetchar(helpwin);
		delwin(helpwin);
	    }
	    break;
	default:
	    beep();
	    continue;
	}
    }

    erase();
    endwin();

    free(numbered);
    free(buffer);
    return OK;
}
#endif /* USE_WIDEC_SUPPORT */

#if HAVE_COLOR_CONTENT
static void
change_color(NCURSES_PAIRS_T current, int field, int value, int usebase)
{
    NCURSES_COLOR_T red, green, blue;

    color_content(current, &red, &green, &blue);

    switch (field) {
    case 0:
	red = (NCURSES_COLOR_T) (usebase ? (red + value) : value);
	break;
    case 1:
	green = (NCURSES_COLOR_T) (usebase ? (green + value) : value);
	break;
    case 2:
	blue = (NCURSES_COLOR_T) (usebase ? (blue + value) : value);
	break;
    }

    if (init_color(current, red, green, blue) == ERR)
	beep();
}

static void
reset_all_colors(void)
{
    NCURSES_PAIRS_T c;

    for (c = 0; c < COLORS; ++c)
	init_color(c,
		   all_colors[c].red,
		   all_colors[c].green,
		   all_colors[c].blue);
}

#define okCOLOR(n) ((n) >= 0 && (n) < MaxColors)
#define okRGB(n)   ((n) >= 0 && (n) <= 1000)
#define DecodeRGB(n) (NCURSES_COLOR_T) ((n * 1000) / 0xffff)

static void
init_all_colors(bool xterm_colors, char *palette_file)
{
    NCURSES_PAIRS_T cp;
    all_colors = typeMalloc(RGB_DATA, (unsigned) MaxColors);
    if (!all_colors)
	failed("all_colors");
    for (cp = 0; cp < MaxColors; ++cp) {
	color_content(cp,
		      &all_colors[cp].red,
		      &all_colors[cp].green,
		      &all_colors[cp].blue);
    }
    /* xterm and compatible terminals can read results of an OSC string
     * asking for the current color palette.
     */
    if (xterm_colors) {
	int n;
	char result[BUFSIZ];
	int check_n;
	unsigned check_r, check_g, check_b;

	raw();
	noecho();

	for (n = 0; n < MaxColors; ++n) {
	    int got;

	    fprintf(stderr, "\033]4;%d;?\007", n);
	    got = (int) read(0, result, sizeof(result) - 1);
	    if (got < 0)
		break;
	    result[got] = '\0';
	    if (sscanf(result, "\033]4;%d;rgb:%x/%x/%x\007",
		       &check_n,
		       &check_r,
		       &check_g,
		       &check_b) == 4 &&
		check_n == n) {
		all_colors[n].red = DecodeRGB(check_r);
		all_colors[n].green = DecodeRGB(check_g);
		all_colors[n].blue = DecodeRGB(check_b);
	    } else {
		break;
	    }
	}
	reset_prog_mode();
    }
    if (palette_file != 0) {
	FILE *fp = fopen(palette_file, "r");
	if (fp != 0) {
	    char buffer[BUFSIZ];
	    int red, green, blue;
	    int scale = 1000;
	    int c;
	    while (fgets(buffer, sizeof(buffer), fp) != 0) {
		if (sscanf(buffer, "scale:%d", &c) == 1) {
		    scale = c;
		} else if (sscanf(buffer, "%d:%d %d %d",
				  &c,
				  &red,
				  &green,
				  &blue) == 4
			   && okCOLOR(c)
			   && okRGB(red)
			   && okRGB(green)
			   && okRGB(blue)) {
#define Scaled(n) (NCURSES_COLOR_T) (((n) * 1000) / scale)
		    all_colors[c].red = Scaled(red);
		    all_colors[c].green = Scaled(green);
		    all_colors[c].blue = Scaled(blue);
		}
	    }
	    fclose(fp);
	}
    }
}

#define scaled_rgb(n) ((255 * (n)) / 1000)

static int
color_edit(bool recur GCC_UNUSED)
/* display the color test pattern, without trying to edit colors */
{
    int i;
    int current;
    int this_c, value, field;
    int last_c;
    int top_color;
    int page_size;

    if (!UseColors) {
	Cannot("does not support color.");
	return ERR;
    } else if (!can_change_color()) {
	Cannot("has hardwired color values.");
	return ERR;
    }

    reset_all_colors();
#ifdef KEY_RESIZE
  retry:
#endif
    current = 0;
    this_c = 0;
    value = 0;
    field = 0;
    top_color = 0;
    page_size = (LINES - 6);
    erase();

    for (i = 0; i < MaxColors; i++)
	init_pair((NCURSES_PAIRS_T) i,
		  (NCURSES_COLOR_T) COLOR_WHITE,
		  (NCURSES_COLOR_T) i);

    MvPrintw(LINES - 2, 0, "Number: %d", value);

    do {
	NCURSES_COLOR_T red, green, blue;

	attron(A_BOLD);
	MvAddStr(0, 20, "Color RGB Value Editing");
	attroff(A_BOLD);

	for (i = (NCURSES_COLOR_T) top_color;
	     (i - top_color < page_size)
	     && (i < MaxColors); i++) {
	    char numeric[80];

	    _nc_SPRINTF(numeric, _nc_SLIMIT(sizeof(numeric)) "[%d]", i);
	    MvPrintw(2 + i - top_color, 0, "%c %-8s:",
		     (i == current ? '>' : ' '),
		     (i < (int) SIZEOF(the_color_names)
		      ? the_color_names[i] : numeric));
	    (void) attrset(AttrArg(COLOR_PAIR(i), 0));
	    addstr("        ");
	    (void) attrset(A_NORMAL);

	    color_content((NCURSES_PAIRS_T) i, &red, &green, &blue);
	    addstr("   R = ");
	    if (current == i && field == 0)
		attron(A_STANDOUT);
	    printw("%04d", (int) red);
	    if (current == i && field == 0)
		(void) attrset(A_NORMAL);
	    addstr(", G = ");
	    if (current == i && field == 1)
		attron(A_STANDOUT);
	    printw("%04d", (int) green);
	    if (current == i && field == 1)
		(void) attrset(A_NORMAL);
	    addstr(", B = ");
	    if (current == i && field == 2)
		attron(A_STANDOUT);
	    printw("%04d", (int) blue);
	    if (current == i && field == 2)
		(void) attrset(A_NORMAL);
	    (void) attrset(A_NORMAL);
	    printw(" ( %3d %3d %3d )",
		   (int) scaled_rgb(red),
		   (int) scaled_rgb(green),
		   (int) scaled_rgb(blue));
	}

	MvAddStr(LINES - 3, 0,
		 "Use up/down to select a color, left/right to change fields.");
	MvAddStr(LINES - 2, 0,
		 "Modify field by typing nnn=, nnn-, or nnn+.  ? for help.");

	move(2 + current - top_color, 0);

	last_c = this_c;
	this_c = Getchar();
	if (this_c < 256 && isdigit(this_c) && !isdigit(last_c))
	    value = 0;

	switch (this_c) {
#ifdef KEY_RESIZE
	case KEY_RESIZE:
	    move(0, 0);
	    goto retry;
#endif
	case '!':
	    ShellOut(FALSE);
	    /* FALLTHRU */
	case CTRL('r'):
	    endwin();
	    refresh();
	    break;
	case CTRL('l'):
	    refresh();
	    break;
	case CTRL('b'):
	case KEY_PPAGE:
	    if (current > 0)
		current -= (page_size - 1);
	    else
		beep();
	    break;

	case CTRL('f'):
	case KEY_NPAGE:
	    if (current < (MaxColors - 1))
		current += (page_size - 1);
	    else
		beep();
	    break;

	case CTRL('p'):
	case KEY_UP:
	    current = (current == 0 ? (MaxColors - 1) : current - 1);
	    break;

	case CTRL('n'):
	case KEY_DOWN:
	    current = (current == (MaxColors - 1) ? 0 : current + 1);
	    break;

	case '\t':
	case KEY_RIGHT:
	    field = (field == 2 ? 0 : field + 1);
	    break;

	case KEY_BTAB:
	case KEY_LEFT:
	    field = (field == 0 ? 2 : field - 1);
	    break;

	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	    value = value * 10 + (this_c - '0');
	    break;

	case '+':
	    change_color((NCURSES_PAIRS_T) current, field, value, 1);
	    break;

	case '-':
	    change_color((NCURSES_PAIRS_T) current, field, -value, 1);
	    break;

	case '=':
	    change_color((NCURSES_PAIRS_T) current, field, value, 0);
	    break;

	case HELP_KEY_1:
	    erase();
	    P("                      RGB Value Editing Help");
	    P("");
	    P("You are in the RGB value editor.  Use the arrow keys to select one of");
	    P("the fields in one of the RGB triples of the current colors; the one");
	    P("currently selected will be reverse-video highlighted.");
	    P("");
	    P("To change a field, enter the digits of the new value; they are echoed");
	    P("as entered.  Finish by typing `='.  The change will take effect instantly.");
	    P("To increment or decrement a value, use the same procedure, but finish");
	    P("with a `+' or `-'.");
	    P("");
	    P("Use `!' to shell-out, ^R or ^L to repaint the screen.");
	    P("");
	    P("Press 'm' to invoke the top-level menu with the current color settings.");
	    P("To quit, do ESC");

	    Pause();
	    erase();
	    break;

	case 'm':
	    endwin();
	    main_menu(FALSE);
	    for (i = 0; i < MaxColors; i++)
		init_pair((NCURSES_PAIRS_T) i,
			  (NCURSES_COLOR_T) COLOR_WHITE,
			  (NCURSES_COLOR_T) i);
	    refresh();
	    break;

	case case_QUIT:
	    break;

	default:
	    beep();
	    break;
	}

	if (current < 0)
	    current = 0;
	if (current >= MaxColors)
	    current = MaxColors - 1;
	if (current < top_color)
	    top_color = current;
	if (current - top_color >= page_size)
	    top_color = current - (page_size - 1);

	MvPrintw(LINES - 1, 0, "Number: %d", value);
	clrtoeol();
    } while
	(!isQuit(this_c, TRUE));

    erase();

    /*
     * ncurses does not reset each color individually when calling endwin().
     */
    reset_all_colors();

    endwin();
    return OK;
}
#endif /* HAVE_COLOR_CONTENT */

/****************************************************************************
 *
 * Alternate character-set stuff
 *
 ****************************************************************************/
static bool
cycle_attr(int ch, unsigned *at_code, chtype *attr, ATTR_TBL * list, unsigned limit)
{
    bool result = TRUE;

    switch (ch) {
    case 'v':
	if ((*at_code += 1) >= limit)
	    *at_code = 0;
	break;
    case 'V':
	if (*at_code == 0)
	    *at_code = limit - 1;
	else
	    *at_code -= 1;
	break;
    default:
	result = FALSE;
	break;
    }
    if (result)
	*attr = list[*at_code].attr;
    return result;
}

#if USE_WIDEC_SUPPORT
static bool
cycle_w_attr(int ch, unsigned *at_code, attr_t *attr, W_ATTR_TBL * list, unsigned limit)
{
    bool result = TRUE;

    switch (ch) {
    case 'v':
	if ((*at_code += 1) >= limit)
	    *at_code = 0;
	break;
    case 'V':
	if (*at_code == 0)
	    *at_code = limit - 1;
	else
	    *at_code -= 1;
	break;
    default:
	result = FALSE;
	break;
    }
    if (result)
	*attr = list[*at_code].attr;
    return result;
}
#endif

static bool
cycle_colors(int ch, int *fg, int *bg, NCURSES_PAIRS_T *pair)
{
    bool result = FALSE;

    if (UseColors) {
	result = TRUE;
	switch (ch) {
	case 'F':
	    if ((*fg -= 1) < 0)
		*fg = COLORS - 1;
	    break;
	case 'f':
	    if ((*fg += 1) >= COLORS)
		*fg = 0;
	    break;
	case 'B':
	    if ((*bg -= 1) < 0)
		*bg = COLORS - 1;
	    break;
	case 'b':
	    if ((*bg += 1) >= COLORS)
		*bg = 0;
	    break;
	default:
	    result = FALSE;
	    break;
	}
	if (result) {
	    *pair = (NCURSES_PAIRS_T) (*fg != COLOR_BLACK || *bg != COLOR_BLACK);
	    if (*pair != 0) {
		*pair = 1;
		if (init_pair(*pair,
			      (NCURSES_COLOR_T) *fg,
			      (NCURSES_COLOR_T) *bg) == ERR) {
		    result = FALSE;
		}
	    }
	}
    }
    return result;
}

/****************************************************************************
 *
 * Soft-key label test
 *
 ****************************************************************************/

#if USE_SOFTKEYS

#define SLK_HELP 17
#define SLK_WORK (SLK_HELP + 3)

static void
slk_help(void)
{
    static const char *table[] =
    {
	"Available commands are:"
	,""
	,"^L         -- repaint this message and activate soft keys"
	,"a/d        -- activate/disable soft keys"
	,"c          -- set centered format for labels"
	,"l          -- set left-justified format for labels"
	,"r          -- set right-justified format for labels"
	,"[12345678] -- set label; labels are numbered 1 through 8"
	,"e          -- erase stdscr (should not erase labels)"
	,"s          -- test scrolling of shortened screen"
	,"v/V        -- cycle through video attributes"
#if HAVE_SLK_COLOR
	,"F/f/B/b    -- cycle through foreground/background colors"
#endif
	,"ESC        -- return to main menu"
	,""
	,"Note: if activating the soft keys causes your terminal to scroll up"
	,"one line, your terminal auto-scrolls when anything is written to the"
	,"last screen position.  The ncurses code does not yet handle this"
	,"gracefully."
    };
    unsigned j;

    move(2, 0);
    for (j = 0; j < SIZEOF(table); ++j) {
	P(table[j]);
    }
    refresh();
}

#if HAVE_SLK_COLOR
static void
call_slk_color(int fg, int bg)
{
    init_pair(1, (NCURSES_COLOR_T) bg, (NCURSES_COLOR_T) fg);
    slk_color(1);
    MvPrintw(SLK_WORK, 0, "Colors %d/%d\n", fg, bg);
    clrtoeol();
    slk_touch();
    slk_noutrefresh();
    refresh();
}
#endif

static int
slk_test(bool recur GCC_UNUSED)
/* exercise the soft keys */
{
    int c, fmt = 1;
    char buf[9];
    char *s;
    chtype attr = A_NORMAL;
    unsigned at_code = 0;
#if HAVE_SLK_COLOR
    int fg = COLOR_BLACK;
    int bg = COLOR_WHITE;
    NCURSES_PAIRS_T pair = 0;
#endif
    ATTR_TBL my_list[SIZEOF(attrs_to_test)];
    unsigned my_size = init_attr_list(my_list, termattrs());

    c = CTRL('l');
#if HAVE_SLK_COLOR
    if (UseColors) {
	call_slk_color(fg, bg);
    }
#endif

    do {
	move(0, 0);
	switch (c) {
	case CTRL('l'):
	    erase();
	    attron(A_BOLD);
	    MvAddStr(0, 20, "Soft Key Exerciser");
	    attroff(A_BOLD);

	    slk_help();
	    /* fall through */

	case 'a':
	    slk_restore();
	    break;

	case 'e':
	    wclear(stdscr);
	    break;

	case 's':
	    MvPrintw(SLK_WORK, 0, "Press Q to stop the scrolling-test: ");
	    while ((c = Getchar()) != 'Q' && (c != ERR))
		AddCh(c);
	    break;

	case 'd':
	    slk_clear();
	    break;

	case 'l':
	    fmt = 0;
	    break;

	case 'c':
	    fmt = 1;
	    break;

	case 'r':
	    fmt = 2;
	    break;

	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	    MvAddStr(SLK_WORK, 0, "Please enter the label value: ");
	    _nc_STRCPY(buf, "", sizeof(buf));
	    if ((s = slk_label(c - '0')) != 0) {
		_nc_STRNCPY(buf, s, (size_t) 8);
	    }
	    wGetstring(stdscr, buf, 8);
	    slk_set((c - '0'), buf, fmt);
	    slk_refresh();
	    move(SLK_WORK, 0);
	    clrtobot();
	    break;

	case case_QUIT:
	    goto done;

#if defined(NCURSES_VERSION) && defined(KEY_RESIZE) && HAVE_WRESIZE
	case KEY_RESIZE:
	    wnoutrefresh(stdscr);
	    break;
#endif

	default:
	    if (cycle_attr(c, &at_code, &attr, my_list, my_size)) {
		slk_attrset(attr);
		slk_touch();
		slk_noutrefresh();
		break;
	    }
#if HAVE_SLK_COLOR
	    if (cycle_colors(c, &fg, &bg, &pair)) {
		if (UseColors) {
		    call_slk_color(fg, bg);
		} else {
		    beep();
		}
		break;
	    }
#endif
	    beep();
	    break;
	}
    } while (!isQuit(c = Getchar(), TRUE));

  done:
    slk_clear();
    erase();
    endwin();
    return OK;
}

#if USE_WIDEC_SUPPORT
#define SLKLEN 8
static int
x_slk_test(bool recur GCC_UNUSED)
/* exercise the soft keys */
{
    int c, fmt = 1;
    wchar_t buf[SLKLEN + 1];
    char *s;
    attr_t attr = WA_NORMAL;
    unsigned at_code = 0;
    int fg = COLOR_BLACK;
    int bg = COLOR_WHITE;
    NCURSES_PAIRS_T pair = 0;
    W_ATTR_TBL my_list[SIZEOF(w_attrs_to_test)];
    unsigned my_size = init_w_attr_list(my_list, term_attrs());

    c = CTRL('l');
    if (UseColors) {
	call_slk_color(fg, bg);
    }
    do {
	move(0, 0);
	switch (c) {
	case CTRL('l'):
	    erase();
	    attr_on(WA_BOLD, NULL);
	    MvAddStr(0, 20, "Soft Key Exerciser");
	    attr_off(WA_BOLD, NULL);

	    slk_help();
	    /* fall through */

	case 'a':
	    slk_restore();
	    break;

	case 'e':
	    wclear(stdscr);
	    break;

	case 's':
	    MvPrintw(SLK_WORK, 0, "Press Q to stop the scrolling-test: ");
	    while ((c = Getchar()) != 'Q' && (c != ERR))
		AddCh(c);
	    break;

	case 'd':
	    slk_clear();
	    break;

	case 'l':
	    fmt = 0;
	    break;

	case 'c':
	    fmt = 1;
	    break;

	case 'r':
	    fmt = 2;
	    break;

	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	    MvAddStr(SLK_WORK, 0, "Please enter the label value: ");
	    *buf = 0;
	    if ((s = slk_label(c - '0')) != 0) {
		char *temp = strdup(s);
		size_t used = strlen(temp);
		size_t want = SLKLEN;
#ifndef state_unused
		mbstate_t state;
#endif

		buf[0] = L'\0';
		while (want > 0 && used != 0) {
		    size_t test;
		    const char *base = s;

		    reset_mbytes(state);
		    test = count_mbytes(base, 0, &state);
		    if (test == (size_t) -1) {
			temp[--used] = 0;
		    } else if (test > want) {
			temp[--used] = 0;
		    } else {
			reset_mbytes(state);
			trans_mbytes(buf, base, want, &state);
			break;
		    }
		}
		free(temp);
	    }
	    wGet_wstring(stdscr, buf, SLKLEN);
	    slk_wset((c - '0'), buf, fmt);
	    slk_refresh();
	    move(SLK_WORK, 0);
	    clrtobot();
	    break;

	case case_QUIT:
	    goto done;

	case 'F':
	    if (UseColors) {
		fg = (NCURSES_COLOR_T) ((fg + 1) % COLORS);
		call_slk_color(fg, bg);
	    }
	    break;
	case 'B':
	    if (UseColors) {
		bg = (NCURSES_COLOR_T) ((bg + 1) % COLORS);
		call_slk_color(fg, bg);
	    }
	    break;
#if defined(NCURSES_VERSION) && defined(KEY_RESIZE) && HAVE_WRESIZE
	case KEY_RESIZE:
	    wnoutrefresh(stdscr);
	    break;
#endif
	default:
	    if (cycle_w_attr(c, &at_code, &attr, my_list, my_size)) {
		slk_attr_set(attr, (NCURSES_COLOR_T) (fg || bg), NULL);
		slk_touch();
		slk_noutrefresh();
		break;
	    }
#if HAVE_SLK_COLOR
	    if (cycle_colors(c, &fg, &bg, &pair)) {
		if (UseColors) {
		    call_slk_color(fg, bg);
		} else {
		    beep();
		}
		break;
	    }
#endif
	    beep();
	    break;
	}
    } while (!isQuit(c = Getchar(), TRUE));

  done:
    slk_clear();
    erase();
    endwin();
    return OK;
}
#endif
#endif /* SLK_INIT */

static void
show_256_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    unsigned first = 0;
    unsigned last = 255;
    unsigned code;
    int count;

    erase();
    attron(A_BOLD);
    MvPrintw(0, 20, "Display of Character Codes %#0x to %#0x",
	     first, last);
    attroff(A_BOLD);
    refresh();

    for (code = first; code <= last; ++code) {
	int row = (int) (2 + (code / 16));
	int col = (int) (5 * (code % 16));
	IGNORE_RC(mvaddch(row, col, colored_chtype(code, attr, pair)));
	for (count = 1; count < repeat; ++count) {
	    AddCh(colored_chtype(code, attr, pair));
	}
    }

}

/*
 * Show a slice of 32 characters, allowing those to be repeated up to the
 * screen's width.
 *
 * ISO 6429:  codes 0x80 to 0x9f may be control characters that cause the
 * terminal to perform functions.  The remaining codes can be graphic.
 */
static void
show_upper_chars(int base, int pagesize, int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    unsigned code;
    unsigned first = (unsigned) base;
    unsigned last = first + (unsigned) pagesize - 2;
    bool C1 = (first == 128);
    int reply;

    erase();
    attron(A_BOLD);
    MvPrintw(0, 20, "Display of %s Character Codes %d to %d",
	     C1 ? "C1" : "GR", first, last);
    attroff(A_BOLD);
    refresh();

    for (code = first; code <= last; code++) {
	int count = repeat;
	int row = 2 + ((int) (code - first) % (pagesize / 2));
	int col = ((int) (code - first) / (pagesize / 2)) * COLS / 2;
	char tmp[80];
	_nc_SPRINTF(tmp, _nc_SLIMIT(sizeof(tmp)) "%3u (0x%x)", code, code);
	MvPrintw(row, col, "%*s: ", COLS / 4, tmp);

	do {
	    if (C1)
		nodelay(stdscr, TRUE);
	    echochar(colored_chtype(code, attr, pair));
	    if (C1) {
		/* (yes, this _is_ crude) */
		while ((reply = Getchar()) != ERR) {
		    AddCh(UChar(reply));
		    napms(10);
		}
		nodelay(stdscr, FALSE);
	    }
	} while (--count > 0);
    }
}

#define PC_COLS 4

static void
show_pc_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    unsigned code;

    erase();
    attron(A_BOLD);
    MvPrintw(0, 20, "Display of PC Character Codes");
    attroff(A_BOLD);
    refresh();

    for (code = 0; code < 16; ++code) {
	MvPrintw(2, (int) code * PC_COLS + 8, "%X", code);
    }
    for (code = 0; code < 256; code++) {
	int count = repeat;
	int row = 3 + (int) (code / 16) + (code >= 128);
	int col = 8 + (int) (code % 16) * PC_COLS;
	if ((code % 16) == 0)
	    MvPrintw(row, 0, "0x%02x:", code);
	move(row, col);
	do {
	    switch (code) {
	    case '\n':
	    case '\r':
	    case '\b':
	    case '\f':
	    case '\033':
	    case 0x9b:
		/*
		 * Skip the ones that do not work.
		 */
		break;
	    default:
		AddCh(colored_chtype(code, A_ALTCHARSET | attr, pair));
		break;
	    }
	} while (--count > 0);
    }
}

static void
show_box_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    (void) repeat;

    attr |= (attr_t) COLOR_PAIR(pair);

    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the ACS Line-Drawing Set");
    attroff(A_BOLD);
    refresh();
    /* *INDENT-OFF* */
    wborder(stdscr,
	    colored_chtype(ACS_VLINE,	 attr, pair),
	    colored_chtype(ACS_VLINE,	 attr, pair),
            colored_chtype(ACS_HLINE,    attr, pair),
	    colored_chtype(ACS_HLINE,	 attr, pair),
	    colored_chtype(ACS_ULCORNER, attr, pair),
	    colored_chtype(ACS_URCORNER, attr, pair),
            colored_chtype(ACS_LLCORNER, attr, pair),
	    colored_chtype(ACS_LRCORNER, attr, pair));
    MvHLine(LINES / 2, 0,        colored_chtype(ACS_HLINE, attr, pair), COLS);
    MvVLine(0,         COLS / 2, colored_chtype(ACS_VLINE, attr, pair), LINES);
    MvAddCh(0,         COLS / 2, colored_chtype(ACS_TTEE,  attr, pair));
    MvAddCh(LINES / 2, COLS / 2, colored_chtype(ACS_PLUS,  attr, pair));
    MvAddCh(LINES - 1, COLS / 2, colored_chtype(ACS_BTEE,  attr, pair));
    MvAddCh(LINES / 2, 0,        colored_chtype(ACS_LTEE,  attr, pair));
    MvAddCh(LINES / 2, COLS - 1, colored_chtype(ACS_RTEE,  attr, pair));
    /* *INDENT-ON* */
}

static int
show_1_acs(int n, int repeat, const char *name, chtype code)
{
    const int height = 16;
    int row = 2 + (n % height);
    int col = (n / height) * COLS / 2;

    MvPrintw(row, col, "%*s : ", COLS / 4, name);
    do {
	AddCh(code);
    } while (--repeat > 0);
    return n + 1;
}

static void
show_acs_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
/* display the ACS character set */
{
    int n;

#define BOTH(name) #name, colored_chtype(name, attr, (chtype) pair)

    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the ACS Character Set");
    attroff(A_BOLD);
    refresh();

    n = show_1_acs(0, repeat, BOTH(ACS_ULCORNER));
    n = show_1_acs(n, repeat, BOTH(ACS_URCORNER));
    n = show_1_acs(n, repeat, BOTH(ACS_LLCORNER));
    n = show_1_acs(n, repeat, BOTH(ACS_LRCORNER));

    n = show_1_acs(n, repeat, BOTH(ACS_LTEE));
    n = show_1_acs(n, repeat, BOTH(ACS_RTEE));
    n = show_1_acs(n, repeat, BOTH(ACS_TTEE));
    n = show_1_acs(n, repeat, BOTH(ACS_BTEE));

    n = show_1_acs(n, repeat, BOTH(ACS_HLINE));
    n = show_1_acs(n, repeat, BOTH(ACS_VLINE));

    /*
     * HPUX's ACS definitions are broken here.  Just give up.
     */
#if !(defined(__hpux) && !defined(NCURSES_VERSION))
    n = show_1_acs(n, repeat, BOTH(ACS_LARROW));
    n = show_1_acs(n, repeat, BOTH(ACS_RARROW));
    n = show_1_acs(n, repeat, BOTH(ACS_UARROW));
    n = show_1_acs(n, repeat, BOTH(ACS_DARROW));

    n = show_1_acs(n, repeat, BOTH(ACS_BLOCK));
    n = show_1_acs(n, repeat, BOTH(ACS_BOARD));
    n = show_1_acs(n, repeat, BOTH(ACS_LANTERN));
    n = show_1_acs(n, repeat, BOTH(ACS_BULLET));
    n = show_1_acs(n, repeat, BOTH(ACS_CKBOARD));
    n = show_1_acs(n, repeat, BOTH(ACS_DEGREE));
    n = show_1_acs(n, repeat, BOTH(ACS_DIAMOND));
    n = show_1_acs(n, repeat, BOTH(ACS_PLMINUS));
    n = show_1_acs(n, repeat, BOTH(ACS_PLUS));

    n = show_1_acs(n, repeat, BOTH(ACS_GEQUAL));
    n = show_1_acs(n, repeat, BOTH(ACS_NEQUAL));
    n = show_1_acs(n, repeat, BOTH(ACS_LEQUAL));

    n = show_1_acs(n, repeat, BOTH(ACS_STERLING));
    n = show_1_acs(n, repeat, BOTH(ACS_PI));
    n = show_1_acs(n, repeat, BOTH(ACS_S1));
    n = show_1_acs(n, repeat, BOTH(ACS_S3));
    n = show_1_acs(n, repeat, BOTH(ACS_S7));
    (void) show_1_acs(n, repeat, BOTH(ACS_S9));
#endif
#undef BOTH
}

static int
acs_test(bool recur GCC_UNUSED)
{
    int c = 'a';
    int pagesize = 32;
    char *term = getenv("TERM");
    const char *pch_kludge = ((term != 0 && strstr(term, "linux"))
			      ? "p=PC, "
			      : "");
    chtype attr = A_NORMAL;
    int digit = 0;
    int repeat = 1;
    int fg = COLOR_BLACK;
    int bg = COLOR_BLACK;
    unsigned at_code = 0;
    NCURSES_PAIRS_T pair = 0;
    void (*last_show_acs) (int, attr_t, NCURSES_PAIRS_T) = 0;
    ATTR_TBL my_list[SIZEOF(attrs_to_test)];
    unsigned my_size = init_attr_list(my_list, termattrs());

    do {
	switch (c) {
	case CTRL('L'):
	    Repaint();
	    break;
	case 'a':
	    ToggleAcs(last_show_acs, show_acs_chars);
	    break;
	case 'p':
	    if (*pch_kludge)
		ToggleAcs(last_show_acs, show_pc_chars);
	    else
		beep();
	    break;
	case 'w':
	    if (pagesize == 32) {
		pagesize = 256;
	    } else {
		pagesize = 32;
	    }
	    break;
	case 'x':
	    ToggleAcs(last_show_acs, show_box_chars);
	    break;
	case '0':
	case '1':
	case '2':
	case '3':
	    digit = (c - '0');
	    last_show_acs = 0;
	    break;
	case '-':
	    if (digit > 0) {
		--digit;
		last_show_acs = 0;
	    } else {
		beep();
	    }
	    break;
	case '+':
	    if (digit < 3) {
		++digit;
		last_show_acs = 0;
	    } else {
		beep();
	    }
	    break;
	case '>':
	    if (repeat < (COLS / 4))
		++repeat;
	    break;
	case '<':
	    if (repeat > 1)
		--repeat;
	    break;
	default:
	    if (cycle_attr(c, &at_code, &attr, my_list, my_size)
		|| cycle_colors(c, &fg, &bg, &pair)) {
		break;
	    } else {
		beep();
	    }
	    break;
	}
	if (pagesize != 32) {
	    show_256_chars(repeat, attr, pair);
	} else if (last_show_acs != 0) {
	    last_show_acs(repeat, attr, pair);
	} else {
	    show_upper_chars(digit * pagesize + 128, pagesize, repeat, attr, pair);
	}

	MvPrintw(LINES - 3, 0,
		 "Note: ANSI terminals may not display C1 characters.");
	MvPrintw(LINES - 2, 0,
		 "Select: a=ACS, w=all x=box, %s0=C1, 1-3,+/- non-ASCII, </> repeat, ESC=quit",
		 pch_kludge);
	if (UseColors) {
	    MvPrintw(LINES - 1, 0,
		     "v/V, f/F, b/B cycle through video attributes (%s) and color %d/%d.",
		     my_list[at_code].name,
		     fg, bg);
	} else {
	    MvPrintw(LINES - 1, 0,
		     "v/V cycles through video attributes (%s).",
		     my_list[at_code].name);
	}
	refresh();
    } while (!isQuit(c = Getchar(), TRUE));

    Pause();
    erase();
    endwin();
    return OK;
}

#if USE_WIDEC_SUPPORT
static cchar_t *
merge_wide_attr(cchar_t *dst, const cchar_t *src, attr_t attr, NCURSES_PAIRS_T pair)
{

    *dst = *src;
    do {
	int count;
	TEST_CCHAR(src, count, {
	    attr |= (test_attrs & A_ALTCHARSET);
	    setcchar(dst, test_wch, attr, pair, NULL);
	}, {
	    ;
	});
    } while (0);
    return dst;
}

/*
 * Header/legend take up no more than 8 lines, leaving 16 lines on a 24-line
 * display.  If there are no repeats, we could normally display 16 lines of 64
 * characters (1024 total).  However, taking repeats and double-width cells
 * into account, use 256 characters for the page.
 */
static void
show_paged_widechars(unsigned base,
		     unsigned pagesize,
		     int repeat,
		     int space,
		     attr_t attr,
		     NCURSES_PAIRS_T pair)
{
    unsigned first = base * pagesize;
    unsigned last = first + pagesize - 1;
    int per_line = 16;
    cchar_t temp;
    wchar_t code;
    wchar_t codes[10];

    erase();
    attron(A_BOLD);
    MvPrintw(0, 20, "Display of Character Codes %#x to %#x", first, last);
    attroff(A_BOLD);

    for (code = (wchar_t) first; code <= (wchar_t) last; code++) {
	int row = (2 + (int) (code - (wchar_t) first) / per_line);
	int col = 5 * ((int) code % per_line);
	int count;

	memset(&codes, 0, sizeof(codes));
	codes[0] = code;
	setcchar(&temp, codes, attr, pair, 0);
	move(row, col);
	if (wcwidth(code) == 0 && code != 0) {
	    AddCh((chtype) space |
		  (A_REVERSE ^ attr) |
		  (attr_t) COLOR_PAIR(pair));
	}
	add_wch(&temp);
	for (count = 1; count < repeat; ++count) {
	    add_wch(&temp);
	}
    }
}

static void
show_upper_widechars(unsigned first, int repeat, int space, attr_t attr, NCURSES_PAIRS_T pair)
{
    cchar_t temp;
    wchar_t code;
    unsigned last = first + 31;

    erase();
    attron(A_BOLD);
    MvPrintw(0, 20, "Display of Character Codes %d to %d", first, last);
    attroff(A_BOLD);

    for (code = (wchar_t) first; code <= (wchar_t) last; code++) {
	int row = 2 + ((int) (code - (wchar_t) first) % 16);
	int col = ((int) (code - (wchar_t) first) / 16) * COLS / 2;
	wchar_t codes[10];
	char tmp[80];
	int count = repeat;

	_nc_SPRINTF(tmp, _nc_SLIMIT(sizeof(tmp))
		    "%3ld (0x%lx)", (long) code, (long) code);
	MvPrintw(row, col, "%*s: ", COLS / 4, tmp);

	memset(&codes, 0, sizeof(codes));
	codes[0] = code;
	setcchar(&temp, codes, attr, pair, 0);

	do {
	    int y, x;

	    /*
	     * Give non-spacing characters something to combine with.  If we
	     * don't, they'll bunch up in a heap on the space after the ":".
	     * Mark them with reverse-video to make them simpler to find on
	     * the display.
	     */
	    if (wcwidth(code) == 0) {
		AddCh((chtype) space |
		      (A_REVERSE ^ attr) |
		      (attr_t) COLOR_PAIR(pair));
	    }
	    /*
	     * This uses echo_wchar(), for comparison with the normal 'f'
	     * test (and to make a test-case for echo_wchar()).  The screen
	     * may flicker because the erase() at the top of the function
	     * is met by the builtin refresh() in echo_wchar().
	     */
	    echo_wchar(&temp);
	    /*
	     * The repeat-count may make text wrap - avoid that.
	     */
	    getyx(stdscr, y, x);
	    (void) y;
	    if (x >= col + (COLS / 2) - 2)
		break;
	} while (--count > 0);
    }
}

static int
show_1_wacs(int n, int repeat, const char *name, const cchar_t *code)
{
    const int height = 16;
    int row = 2 + (n % height);
    int col = (n / height) * COLS / 2;

    MvPrintw(row, col, "%*s : ", COLS / 4, name);
    while (--repeat >= 0) {
	add_wch(code);
    }
    return n + 1;
}

#define MERGE_ATTR(wch) merge_wide_attr(&temp, wch, attr, pair)

static void
show_wacs_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
/* display the wide-ACS character set */
{
    cchar_t temp;

    int n;

/*#define BOTH2(name) #name, &(name) */
#define BOTH2(name) #name, MERGE_ATTR(name)

    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the Wide-ACS Character Set");
    attroff(A_BOLD);
    refresh();

    n = show_1_wacs(0, repeat, BOTH2(WACS_ULCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_URCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LLCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LRCORNER));

    n = show_1_wacs(n, repeat, BOTH2(WACS_LTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_RTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_TTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BTEE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_HLINE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_VLINE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_LARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_RARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_UARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DARROW));

    n = show_1_wacs(n, repeat, BOTH2(WACS_BLOCK));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LANTERN));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BULLET));
    n = show_1_wacs(n, repeat, BOTH2(WACS_CKBOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DEGREE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DIAMOND));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PLMINUS));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PLUS));

#ifdef CURSES_WACS_ARRAY
    n = show_1_wacs(n, repeat, BOTH2(WACS_GEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_NEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LEQUAL));

    n = show_1_wacs(n, repeat, BOTH2(WACS_STERLING));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PI));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S1));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S3));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S7));
    (void) show_1_wacs(n, repeat, BOTH2(WACS_S9));
#endif
}

#ifdef WACS_D_PLUS
static void
show_wacs_chars_double(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
/* display the wide-ACS character set */
{
    cchar_t temp;

    int n;

/*#define BOTH2(name) #name, &(name) */
#define BOTH2(name) #name, MERGE_ATTR(name)

    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the Wide-ACS Character Set");
    attroff(A_BOLD);
    refresh();

    n = show_1_wacs(0, repeat, BOTH2(WACS_D_ULCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_URCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_LLCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_LRCORNER));

    n = show_1_wacs(n, repeat, BOTH2(WACS_D_LTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_RTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_TTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_BTEE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_D_HLINE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_VLINE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_LARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_RARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_UARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DARROW));

    n = show_1_wacs(n, repeat, BOTH2(WACS_BLOCK));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LANTERN));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BULLET));
    n = show_1_wacs(n, repeat, BOTH2(WACS_CKBOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DEGREE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DIAMOND));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PLMINUS));
    n = show_1_wacs(n, repeat, BOTH2(WACS_D_PLUS));

#ifdef CURSES_WACS_ARRAY
    n = show_1_wacs(n, repeat, BOTH2(WACS_GEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_NEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LEQUAL));

    n = show_1_wacs(n, repeat, BOTH2(WACS_STERLING));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PI));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S1));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S3));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S7));
    (void) show_1_wacs(n, repeat, BOTH2(WACS_S9));
#endif
}
#endif

#ifdef WACS_T_PLUS
static void
show_wacs_chars_thick(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
/* display the wide-ACS character set */
{
    cchar_t temp;

    int n;

/*#define BOTH2(name) #name, &(name) */
#define BOTH2(name) #name, MERGE_ATTR(name)

    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the Wide-ACS Character Set");
    attroff(A_BOLD);
    refresh();

    n = show_1_wacs(0, repeat, BOTH2(WACS_T_ULCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_URCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_LLCORNER));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_LRCORNER));

    n = show_1_wacs(n, repeat, BOTH2(WACS_T_LTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_RTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_TTEE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_BTEE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_T_HLINE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_VLINE));

    n = show_1_wacs(n, repeat, BOTH2(WACS_LARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_RARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_UARROW));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DARROW));

    n = show_1_wacs(n, repeat, BOTH2(WACS_BLOCK));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LANTERN));
    n = show_1_wacs(n, repeat, BOTH2(WACS_BULLET));
    n = show_1_wacs(n, repeat, BOTH2(WACS_CKBOARD));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DEGREE));
    n = show_1_wacs(n, repeat, BOTH2(WACS_DIAMOND));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PLMINUS));
    n = show_1_wacs(n, repeat, BOTH2(WACS_T_PLUS));

#ifdef CURSES_WACS_ARRAY
    n = show_1_wacs(n, repeat, BOTH2(WACS_GEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_NEQUAL));
    n = show_1_wacs(n, repeat, BOTH2(WACS_LEQUAL));

    n = show_1_wacs(n, repeat, BOTH2(WACS_STERLING));
    n = show_1_wacs(n, repeat, BOTH2(WACS_PI));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S1));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S3));
    n = show_1_wacs(n, repeat, BOTH2(WACS_S7));
    (void) show_1_wacs(n, repeat, BOTH2(WACS_S9));
#endif
}
#endif

#undef MERGE_ATTR

#define MERGE_ATTR(n,wch) merge_wide_attr(&temp[n], wch, attr, pair)

static void
show_wbox_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    cchar_t temp[8];

    (void) repeat;
    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the Wide-ACS Line-Drawing Set");
    attroff(A_BOLD);
    refresh();

    wborder_set(stdscr,
		MERGE_ATTR(0, WACS_VLINE),
		MERGE_ATTR(1, WACS_VLINE),
		MERGE_ATTR(2, WACS_HLINE),
		MERGE_ATTR(3, WACS_HLINE),
		MERGE_ATTR(4, WACS_ULCORNER),
		MERGE_ATTR(5, WACS_URCORNER),
		MERGE_ATTR(6, WACS_LLCORNER),
		MERGE_ATTR(7, WACS_LRCORNER));
    /* *INDENT-OFF* */
    (void) mvhline_set(LINES / 2, 0,        MERGE_ATTR(0, WACS_HLINE), COLS);
    (void) mvvline_set(0,         COLS / 2, MERGE_ATTR(0, WACS_VLINE), LINES);
    (void) mvadd_wch(0,           COLS / 2, MERGE_ATTR(0, WACS_TTEE));
    (void) mvadd_wch(LINES / 2,   COLS / 2, MERGE_ATTR(0, WACS_PLUS));
    (void) mvadd_wch(LINES - 1,   COLS / 2, MERGE_ATTR(0, WACS_BTEE));
    (void) mvadd_wch(LINES / 2,   0,        MERGE_ATTR(0, WACS_LTEE));
    (void) mvadd_wch(LINES / 2,   COLS - 1, MERGE_ATTR(0, WACS_RTEE));
    /* *INDENT-ON* */
}

#undef MERGE_ATTR

static int
show_2_wacs(int n, const char *name, const char *code, attr_t attr, NCURSES_PAIRS_T pair)
{
    const int height = 16;
    int row = 2 + (n % height);
    int col = (n / height) * COLS / 2;
    char temp[80];

    MvPrintw(row, col, "%*s : ", COLS / 4, name);
    (void) attr_set(attr, pair, 0);
    _nc_STRNCPY(temp, code, 20);
    addstr(temp);
    (void) attr_set(A_NORMAL, 0, 0);
    return n + 1;
}

#define SHOW_UTF8(n, name, code) show_2_wacs(n, name, code, attr, pair)

static void
show_utf8_chars(int repeat, attr_t attr, NCURSES_PAIRS_T pair)
{
    int n;

    (void) repeat;
    erase();
    attron(A_BOLD);
    MvAddStr(0, 20, "Display of the Wide-ACS Character Set");
    attroff(A_BOLD);
    refresh();
    /* *INDENT-OFF* */
    n = SHOW_UTF8(0, "WACS_ULCORNER",	"\342\224\214");
    n = SHOW_UTF8(n, "WACS_URCORNER",	"\342\224\220");
    n = SHOW_UTF8(n, "WACS_LLCORNER",	"\342\224\224");
    n = SHOW_UTF8(n, "WACS_LRCORNER",	"\342\224\230");

    n = SHOW_UTF8(n, "WACS_LTEE",	"\342\224\234");
    n = SHOW_UTF8(n, "WACS_RTEE",	"\342\224\244");
    n = SHOW_UTF8(n, "WACS_TTEE",	"\342\224\254");
    n = SHOW_UTF8(n, "WACS_BTEE",	"\342\224\264");

    n = SHOW_UTF8(n, "WACS_HLINE",	"\342\224\200");
    n = SHOW_UTF8(n, "WACS_VLINE",	"\342\224\202");

    n = SHOW_UTF8(n, "WACS_LARROW",	"\342\206\220");
    n = SHOW_UTF8(n, "WACS_RARROW",	"\342\206\222");
    n = SHOW_UTF8(n, "WACS_UARROW",	"\342\206\221");
    n = SHOW_UTF8(n, "WACS_DARROW",	"\342\206\223");

    n = SHOW_UTF8(n, "WACS_BLOCK",	"\342\226\256");
    n = SHOW_UTF8(n, "WACS_BOARD",	"\342\226\222");
    n = SHOW_UTF8(n, "WACS_LANTERN",	"\342\230\203");
    n = SHOW_UTF8(n, "WACS_BULLET",	"\302\267");
    n = SHOW_UTF8(n, "WACS_CKBOARD",	"\342\226\222");
    n = SHOW_UTF8(n, "WACS_DEGREE",	"\302\260");
    n = SHOW_UTF8(n, "WACS_DIAMOND",	"\342\227\206");
    n = SHOW_UTF8(n, "WACS_PLMINUS",	"\302\261");
    n = SHOW_UTF8(n, "WACS_PLUS",	"\342\224\274");
    n = SHOW_UTF8(n, "WACS_GEQUAL",	"\342\211\245");
    n = SHOW_UTF8(n, "WACS_NEQUAL",	"\342\211\240");
    n = SHOW_UTF8(n, "WACS_LEQUAL",	"\342\211\244");

    n = SHOW_UTF8(n, "WACS_STERLING",	"\302\243");
    n = SHOW_UTF8(n, "WACS_PI",		"\317\200");
    n = SHOW_UTF8(n, "WACS_S1",		"\342\216\272");
    n = SHOW_UTF8(n, "WACS_S3",		"\342\216\273");
    n = SHOW_UTF8(n, "WACS_S7",		"\342\216\274");
    (void) SHOW_UTF8(n, "WACS_S9",	"\342\216\275");
    /* *INDENT-ON* */
}

/* display the wide-ACS character set */
static int
x_acs_test(bool recur GCC_UNUSED)
{
    int c = 'a';
    unsigned digit = 0;
    int repeat = 1;
    int space = ' ';
    unsigned pagesize = 32;
    attr_t attr = WA_NORMAL;
    int fg = COLOR_BLACK;
    int bg = COLOR_BLACK;
    unsigned at_code = 0;
    NCURSES_PAIRS_T pair = 0;
    void (*last_show_wacs) (int, attr_t, NCURSES_PAIRS_T) = 0;
    W_ATTR_TBL my_list[SIZEOF(w_attrs_to_test)];
    unsigned my_size = init_w_attr_list(my_list, term_attrs());
    char at_page[20];
    bool pending_code = FALSE;

    at_page[0] = '\0';
    do {
	switch (c) {
	case CTRL('L'):
	    Repaint();
	    break;
	case 'a':
	    ToggleAcs(last_show_wacs, show_wacs_chars);
	    break;
#ifdef WACS_D_PLUS
	case 'd':
	    ToggleAcs(last_show_wacs, show_wacs_chars_double);
	    break;
#endif
#ifdef WACS_T_PLUS
	case 't':
	    ToggleAcs(last_show_wacs, show_wacs_chars_thick);
	    break;
#endif
	case 'w':
	    if (pagesize == 32) {
		pagesize = 256;
	    } else {
		pagesize = 32;
	    }
	    break;
	case 'x':
	    ToggleAcs(last_show_wacs, show_wbox_chars);
	    break;
	case 'u':
	    ToggleAcs(last_show_wacs, show_utf8_chars);
	    break;
	case '@':
	    pending_code = !pending_code;
	    if (pending_code) {
		_nc_SPRINTF(at_page, _nc_SLIMIT(sizeof(at_page)) "%02x", digit);
	    } else if (at_page[0] != '\0') {
		_nc_SPRINTF(at_page, _nc_SLIMIT(sizeof(at_page)) "%x", digit);
	    }
	    break;
	default:
	    if (pending_code && isxdigit(c)) {
		size_t len = strlen(at_page);
		if (len && at_page[0] == '0') {
		    memmove(at_page, at_page + 1, len--);
		}
		if (len < sizeof(at_page) - 1) {
		    at_page[len++] = (char) c;
		    at_page[len] = '\0';
		}
	    } else if (pending_code
		       && (c == '\b' || c == KEY_BACKSPACE || c == KEY_DC)) {
		size_t len = strlen(at_page);
		if (len)
		    at_page[--len] = '\0';
	    } else if (c < 256 && isdigit(c)) {
		digit = (unsigned) (c - '0');
		last_show_wacs = 0;
	    } else if (c == '+') {
		++digit;
		_nc_SPRINTF(at_page, _nc_SLIMIT(sizeof(at_page)) "%02x", digit);
		last_show_wacs = 0;
	    } else if (c == '-' && digit > 0) {
		--digit;
		_nc_SPRINTF(at_page, _nc_SLIMIT(sizeof(at_page)) "%02x",
			    UChar(digit));
		last_show_wacs = 0;
	    } else if (c == '>' && repeat < (COLS / 4)) {
		++repeat;
	    } else if (c == '<' && repeat > 1) {
		--repeat;
	    } else if (c == '_') {
		space = (space == ' ') ? '_' : ' ';
		last_show_wacs = 0;
	    } else if (cycle_w_attr(c, &at_code, &attr, my_list, my_size)
		       || cycle_colors(c, &fg, &bg, &pair)) {
		if (last_show_wacs != 0)
		    break;
	    } else {
		beep();
		break;
	    }
	    break;
	}
	if (pagesize != 32) {
	    show_paged_widechars(digit, pagesize, repeat, space, attr, pair);
	} else if (last_show_wacs != 0) {
	    last_show_wacs(repeat, attr, pair);
	} else {
	    show_upper_widechars(digit * 32 + 128, repeat, space, attr, pair);
	}

	MvPrintw(LINES - 4, 0,
		 "Select: a/d/t WACS, w=%d/page, @",
		 pagesize);
	printw("%s",
	       pending_code ? at_page : "page");
	addstr(", x=box, u UTF-8, ^L repaint");
	MvPrintw(LINES - 3, 2,
		 "0-9,+/- non-ASCII, </> repeat, _ space, ESC=quit");
	if (UseColors) {
	    MvPrintw(LINES - 2, 2,
		     "v/V, f/F, b/B cycle through video attributes (%s) and color %d/%d.",
		     my_list[at_code].name,
		     fg, bg);
	} else {
	    MvPrintw(LINES - 2, 2,
		     "v/V cycles through video attributes (%s).",
		     my_list[at_code].name);
	}
	refresh();
    } while (!isQuit(c = Getchar(), TRUE));

    Pause();
    erase();
    endwin();
    return OK;
}

#endif

/*
 * Graphic-rendition test (adapted from vttest)
 */
static int
sgr_attr_test(bool recur GCC_UNUSED)
{
    int pass;

    for (pass = 0; pass < 2; pass++) {
	chtype normal = ((pass == 0 ? A_NORMAL : A_REVERSE)) | BLANK;

	/* Use non-default colors if possible to exercise bce a little */
	if (UseColors) {
	    init_pair(1, COLOR_WHITE, COLOR_BLUE);
	    normal |= (chtype) COLOR_PAIR(1);
	}
	bkgdset(normal);
	erase();
	MvPrintw(1, 20, "Graphic rendition test pattern:");

	MvPrintw(4, 1, "vanilla");

#define set_sgr(mask) bkgdset((normal^(mask)));
	set_sgr(A_BOLD);
	MvPrintw(4, 40, "bold");

	set_sgr(A_UNDERLINE);
	MvPrintw(6, 6, "underline");

	set_sgr(A_BOLD | A_UNDERLINE);
	MvPrintw(6, 45, "bold underline");

	set_sgr(A_BLINK);
	MvPrintw(8, 1, "blink");

	set_sgr(A_BLINK | A_BOLD);
	MvPrintw(8, 40, "bold blink");

	set_sgr(A_UNDERLINE | A_BLINK);
	MvPrintw(10, 6, "underline blink");

	set_sgr(A_BOLD | A_UNDERLINE | A_BLINK);
	MvPrintw(10, 45, "bold underline blink");

	set_sgr(A_REVERSE);
	MvPrintw(12, 1, "negative");

	set_sgr(A_BOLD | A_REVERSE);
	MvPrintw(12, 40, "bold negative");

	set_sgr(A_UNDERLINE | A_REVERSE);
	MvPrintw(14, 6, "underline negative");

	set_sgr(A_BOLD | A_UNDERLINE | A_REVERSE);
	MvPrintw(14, 45, "bold underline negative");

	set_sgr(A_BLINK | A_REVERSE);
	MvPrintw(16, 1, "blink negative");

	set_sgr(A_BOLD | A_BLINK | A_REVERSE);
	MvPrintw(16, 40, "bold blink negative");

	set_sgr(A_UNDERLINE | A_BLINK | A_REVERSE);
	MvPrintw(18, 6, "underline blink negative");

	set_sgr(A_BOLD | A_UNDERLINE | A_BLINK | A_REVERSE);
	MvPrintw(18, 45, "bold underline blink negative");

	bkgdset(normal);
	MvPrintw(LINES - 2, 1, "%s background. ", pass == 0 ? "Dark" :
		 "Light");
	clrtoeol();
	Pause();
    }

    bkgdset(A_NORMAL | BLANK);
    erase();
    endwin();
    return OK;
}

/****************************************************************************
 *
 * Windows and scrolling tester.
 *
 ****************************************************************************/

#define BOTLINES	4	/* number of line stolen from screen bottom */

typedef struct {
    int y, x;
} pair;

#define FRAME struct frame
FRAME
{
    FRAME *next, *last;
    bool do_scroll;
    bool do_keypad;
    WINDOW *wind;
};

#if defined(NCURSES_VERSION) && NCURSES_EXT_FUNCS
#if (NCURSES_VERSION_PATCH < 20070331)
#define is_keypad(win)   (win)->_use_keypad
#define is_scrollok(win) (win)->_scroll
#endif
#else
#define is_keypad(win)   FALSE
#define is_scrollok(win) FALSE
#endif

static WINDOW *
frame_win(FRAME * curp)
{
    return (curp != 0) ? curp->wind : stdscr;
}

/* We need to know if these flags are actually set, so don't look in FRAME.
 * These names are known to work with SVr4 curses as well as ncurses.  The
 * _use_keypad name does not work with Solaris 8.
 */
static bool
HaveKeypad(FRAME * curp)
{
    WINDOW *win = frame_win(curp);
    (void) win;
    return is_keypad(win);
}

static bool
HaveScroll(FRAME * curp)
{
    WINDOW *win = frame_win(curp);
    (void) win;
    return is_scrollok(win);
}

static void
newwin_legend(FRAME * curp)
{
#define DATA(num, name) { name, num }
    static const struct {
	const char *msg;
	int code;
    } legend[] = {
	DATA(0, "^C = create window"),
	    DATA(0, "^N = next window"),
	    DATA(0, "^P = previous window"),
	    DATA(0, "^F = scroll forward"),
	    DATA(0, "^B = scroll backward"),
	    DATA(1, "^K = keypad(%s)"),
	    DATA(2, "^S = scrollok(%s)"),
	    DATA(0, "^W = save window"),
	    DATA(0, "^R = restore window"),
#if HAVE_WRESIZE
	    DATA(0, "^X = resize"),
#endif
	    DATA(3, "^Q%s = exit")
    };
#undef DATA
    size_t n;
    bool do_keypad = HaveKeypad(curp);
    bool do_scroll = HaveScroll(curp);
    char buf[BUFSIZ];

    move(LINES - 4, 0);

    for (n = 0; n < SIZEOF(legend); n++) {
	int x;

	switch (legend[n].code) {
	default:
	    _nc_STRCPY(buf, legend[n].msg, sizeof(buf));
	    break;
	case 1:
	    _nc_SPRINTF(buf, _nc_SLIMIT(sizeof(buf))
			legend[n].msg, do_keypad ? "yes" : "no");
	    break;
	case 2:
	    _nc_SPRINTF(buf, _nc_SLIMIT(sizeof(buf))
			legend[n].msg, do_scroll ? "yes" : "no");
	    break;
	case 3:
	    _nc_SPRINTF(buf, _nc_SLIMIT(sizeof(buf))
			legend[n].msg, do_keypad ? "/ESC" : "");
	    break;
	}
	x = getcurx(stdscr);
	addstr((COLS < (x + 3 + (int) strlen(buf))) ? "\n" : (n ? ", " : ""));
	addstr(buf);
    }
    clrtoeol();
}

static void
transient(FRAME * curp, NCURSES_CONST char *msg)
{
    newwin_legend(curp);
    if (msg) {
	MvAddStr(LINES - 1, 0, msg);
	refresh();
	napms(1000);
    }

    move(LINES - 1, 0);
    printw("%s characters are echoed, window should %sscroll.",
	   HaveKeypad(curp) ? "Non-arrow" : "All other",
	   HaveScroll(curp) ? "" : "not ");
    clrtoeol();
}

static void
newwin_report(FRAME * curp)
/* report on the cursor's current position, then restore it */
{
    WINDOW *win = frame_win(curp);
    int y, x;

    if (win != stdscr)
	transient(curp, (char *) 0);
    getyx(win, y, x);
    move(LINES - 1, COLS - 17);
    printw("Y = %2d X = %2d", y, x);
    if (win != stdscr)
	refresh();
    else
	wmove(win, y, x);
}

static pair *
selectcell(int uli, int ulj, int lri, int lrj)
/* arrows keys move cursor, return location at current on non-arrow key */
{
    static pair res;		/* result cell */
    int si = lri - uli + 1;	/* depth of the select area */
    int sj = lrj - ulj + 1;	/* width of the select area */
    int i = 0, j = 0;		/* offsets into the select area */

    res.y = uli;
    res.x = ulj;
    for (;;) {
	move(uli + i, ulj + j);
	newwin_report((FRAME *) 0);

	switch (Getchar()) {
	case KEY_UP:
	    i += si - 1;
	    break;
	case KEY_DOWN:
	    i++;
	    break;
	case KEY_LEFT:
	    j += sj - 1;
	    break;
	case KEY_RIGHT:
	    j++;
	    break;
	case case_QUIT:
	    return ((pair *) 0);
#ifdef NCURSES_MOUSE_VERSION
	case KEY_MOUSE:
	    {
		MEVENT event;

		getmouse(&event);
		if (event.y > uli && event.x > ulj) {
		    i = event.y - uli;
		    j = event.x - ulj;
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
	i %= si;
	j %= sj;
    }
}

static void
outerbox(pair ul, pair lr, bool onoff)
/* draw or erase a box *outside* the given pair of corners */
{
    MvAddCh(ul.y - 1, lr.x - 1, onoff ? ACS_ULCORNER : ' ');
    MvAddCh(ul.y - 1, lr.x + 1, onoff ? ACS_URCORNER : ' ');
    MvAddCh(lr.y + 1, lr.x + 1, onoff ? ACS_LRCORNER : ' ');
    MvAddCh(lr.y + 1, ul.x - 1, onoff ? ACS_LLCORNER : ' ');
    move(ul.y - 1, ul.x);
    hline(onoff ? ACS_HLINE : ' ', lr.x - ul.x + 1);
    move(ul.y, ul.x - 1);
    vline(onoff ? ACS_VLINE : ' ', lr.y - ul.y + 1);
    move(lr.y + 1, ul.x);
    hline(onoff ? ACS_HLINE : ' ', lr.x - ul.x + 1);
    move(ul.y, lr.x + 1);
    vline(onoff ? ACS_VLINE : ' ', lr.y - ul.y + 1);
}

static WINDOW *
getwindow(void)
/* Ask user for a window definition */
{
    WINDOW *rwindow;
    pair ul, lr, *tmp;

    move(0, 0);
    clrtoeol();
    addstr("Use arrows to move cursor, anything else to mark corner 1");
    refresh();
    if ((tmp = selectcell(2, 1, LINES - BOTLINES - 2, COLS - 2)) == (pair *) 0)
	return ((WINDOW *) 0);
    memcpy(&ul, tmp, sizeof(pair));
    MvAddCh(ul.y - 1, ul.x - 1, ACS_ULCORNER);
    move(0, 0);
    clrtoeol();
    addstr("Use arrows to move cursor, anything else to mark corner 2");
    refresh();
    if ((tmp = selectcell(ul.y, ul.x, LINES - BOTLINES - 2, COLS - 2)) ==
	(pair *) 0)
	return ((WINDOW *) 0);
    memcpy(&lr, tmp, sizeof(pair));

    rwindow = subwin(stdscr, lr.y - ul.y + 1, lr.x - ul.x + 1, ul.y, ul.x);

    outerbox(ul, lr, TRUE);
    refresh();

    if (rwindow != 0)
	wrefresh(rwindow);

    move(0, 0);
    clrtoeol();
    return (rwindow);
}

static void
newwin_move(FRAME * curp, int dy, int dx)
{
    WINDOW *win = frame_win(curp);
    int cur_y, cur_x;
    int max_y, max_x;

    getyx(win, cur_y, cur_x);
    getmaxyx(win, max_y, max_x);
    if ((cur_x += dx) < 0)
	cur_x = 0;
    else if (cur_x >= max_x)
	cur_x = max_x - 1;
    if ((cur_y += dy) < 0)
	cur_y = 0;
    else if (cur_y >= max_y)
	cur_y = max_y - 1;
    wmove(win, cur_y, cur_x);
}

static FRAME *
delete_framed(FRAME * fp, bool showit)
{
    FRAME *np = 0;

    if (fp != 0) {
	fp->last->next = fp->next;
	fp->next->last = fp->last;

	if (showit) {
	    werase(fp->wind);
	    wrefresh(fp->wind);
	}
	delwin(fp->wind);

	np = (fp == fp->next) ? NULL : fp->next;
	free(fp);
    }
    return np;
}

static int
scroll_test(bool recur GCC_UNUSED)
/* Demonstrate windows */
{
    int c;
    FRAME *current = (FRAME *) 0, *neww;
    WINDOW *usescr;
#if HAVE_PUTWIN && HAVE_GETWIN
    FILE *fp;
#endif

#define DUMPFILE	"screendump"

#ifdef NCURSES_MOUSE_VERSION
    mousemask(BUTTON1_CLICKED, (mmask_t *) 0);
#endif
    c = CTRL('C');
    raw();
    do {
	transient((FRAME *) 0, (char *) 0);
	switch (c) {
	case CTRL('C'):
	    if ((neww = typeCalloc(FRAME, (size_t) 1)) == 0) {
		failed("scroll_test");
		goto breakout;
	    }
	    if ((neww->wind = getwindow()) == (WINDOW *) 0) {
		failed("scroll_test");
		free(neww);
		goto breakout;
	    }

	    if (current == 0) {	/* First element,  */
		neww->next = neww;	/*   so point it at itself */
		neww->last = neww;
	    } else {
		neww->next = current->next;
		neww->last = current;
		neww->last->next = neww;
		neww->next->last = neww;
	    }
	    current = neww;
	    /* SVr4 curses sets the keypad on all newly-created windows to
	     * false.  Someone reported that PDCurses makes new windows inherit
	     * this flag.  Remove the following 'keypad()' call to test this
	     */
	    keypad(current->wind, TRUE);
	    current->do_keypad = HaveKeypad(current);
	    current->do_scroll = HaveScroll(current);
	    break;

	case CTRL('N'):	/* go to next window */
	    if (current)
		current = current->next;
	    break;

	case CTRL('P'):	/* go to previous window */
	    if (current)
		current = current->last;
	    break;

	case CTRL('F'):	/* scroll current window forward */
	    if (current)
		wscrl(frame_win(current), 1);
	    break;

	case CTRL('B'):	/* scroll current window backwards */
	    if (current)
		wscrl(frame_win(current), -1);
	    break;

	case CTRL('K'):	/* toggle keypad mode for current */
	    if (current) {
		current->do_keypad = !current->do_keypad;
		keypad(current->wind, current->do_keypad);
	    }
	    break;

	case CTRL('S'):
	    if (current) {
		current->do_scroll = !current->do_scroll;
		scrollok(current->wind, current->do_scroll);
	    }
	    break;

#if HAVE_PUTWIN && HAVE_GETWIN
	case CTRL('W'):	/* save and delete window */
	    if ((current != 0) && (current == current->next)) {
		transient(current, "Will not save/delete ONLY window");
		break;
	    } else if ((fp = fopen(DUMPFILE, "w")) == (FILE *) 0) {
		transient(current, "Can't open screen dump file");
	    } else {
		int rc = putwin(frame_win(current), fp);
		(void) fclose(fp);

		if (rc == OK) {
		    current = delete_framed(current, TRUE);
		} else {
		    transient(current, "Can't write screen dump file");
		}
	    }
	    break;

	case CTRL('R'):	/* restore window */
	    if ((fp = fopen(DUMPFILE, "r")) == (FILE *) 0) {
		transient(current, "Can't open screen dump file");
	    } else {
		if ((neww = typeCalloc(FRAME, (size_t) 1)) != 0) {

		    neww->next = current ? current->next : 0;
		    neww->last = current;
		    if (neww->last != 0)
			neww->last->next = neww;
		    if (neww->next != 0)
			neww->next->last = neww;

		    neww->wind = getwin(fp);

		    wrefresh(neww->wind);
		} else {
		    failed("scroll_test");
		}
		(void) fclose(fp);
	    }
	    break;
#endif

#if HAVE_WRESIZE
	case CTRL('X'):	/* resize window */
	    if (current) {
		pair *tmp, ul, lr;
		int mx, my;

		move(0, 0);
		clrtoeol();
		addstr("Use arrows to move cursor, anything else to mark new corner");
		refresh();

		getbegyx(current->wind, ul.y, ul.x);

		tmp = selectcell(ul.y, ul.x, LINES - BOTLINES - 2, COLS - 2);
		if (tmp == (pair *) 0) {
		    beep();
		    break;
		}

		getmaxyx(current->wind, lr.y, lr.x);
		lr.y += (ul.y - 1);
		lr.x += (ul.x - 1);
		outerbox(ul, lr, FALSE);
		wnoutrefresh(stdscr);

		/* strictly cosmetic hack for the test */
		getmaxyx(current->wind, my, mx);
		if (my > tmp->y - ul.y) {
		    getyx(current->wind, lr.y, lr.x);
		    wmove(current->wind, tmp->y - ul.y + 1, 0);
		    wclrtobot(current->wind);
		    wmove(current->wind, lr.y, lr.x);
		}
		if (mx > tmp->x - ul.x) {
		    int i;
		    for (i = 0; i < my; i++) {
			wmove(current->wind, i, tmp->x - ul.x + 1);
			wclrtoeol(current->wind);
		    }
		}
		wnoutrefresh(current->wind);

		memcpy(&lr, tmp, sizeof(pair));
		(void) wresize(current->wind, lr.y - ul.y + 0, lr.x - ul.x + 0);

		getbegyx(current->wind, ul.y, ul.x);
		getmaxyx(current->wind, lr.y, lr.x);
		lr.y += (ul.y - 1);
		lr.x += (ul.x - 1);
		outerbox(ul, lr, TRUE);
		wnoutrefresh(stdscr);

		wnoutrefresh(current->wind);
		move(0, 0);
		clrtoeol();
		doupdate();
	    }
	    break;
#endif /* HAVE_WRESIZE */

	case KEY_UP:
	    newwin_move(current, -1, 0);
	    break;
	case KEY_DOWN:
	    newwin_move(current, 1, 0);
	    break;
	case KEY_LEFT:
	    newwin_move(current, 0, -1);
	    break;
	case KEY_RIGHT:
	    newwin_move(current, 0, 1);
	    break;

	case KEY_BACKSPACE:
	    /* FALLTHROUGH */
	case KEY_DC:
	    {
		int y, x;
		getyx(frame_win(current), y, x);
		if (--x < 0) {
		    if (--y < 0)
			break;
		    x = getmaxx(frame_win(current)) - 1;
		}
		(void) mvwdelch(frame_win(current), y, x);
	    }
	    break;

	case '\r':
	    c = '\n';
	    /* FALLTHROUGH */

	default:
	    if (current)
		waddch(current->wind, (chtype) c);
	    else
		beep();
	    break;
	}
	newwin_report(current);
	usescr = frame_win(current);
	wrefresh(usescr);
    } while
	(!isQuit(c = wGetchar(usescr), TRUE)
	 && (c != ERR));

  breakout:
    while (current != 0)
	current = delete_framed(current, FALSE);

    scrollok(stdscr, TRUE);	/* reset to driver's default */
#ifdef NCURSES_MOUSE_VERSION
    mousemask(0, (mmask_t *) 0);
#endif
    noraw();
    erase();
    endwin();
    return OK;
}

/****************************************************************************
 *
 * Panels tester
 *
 ****************************************************************************/

#if USE_LIBPANEL
static int nap_msec = 1;

static NCURSES_CONST char *mod[] =
{
    "test ",
    "TEST ",
    "(**) ",
    "*()* ",
    "<--> ",
    "LAST "
};

/*+-------------------------------------------------------------------------
	wait_a_while(msec)
--------------------------------------------------------------------------*/
static void
wait_a_while(int msec GCC_UNUSED)
{
#if HAVE_NAPMS
    if (nap_msec == 1)
	wGetchar(stdscr);
    else
	napms(nap_msec);
#else
    if (nap_msec == 1)
	wGetchar(stdscr);
    else if (msec > 1000)
	sleep((unsigned) msec / 1000);
    else
	sleep(1);
#endif
}				/* end of wait_a_while */

/*+-------------------------------------------------------------------------
	saywhat(text)
--------------------------------------------------------------------------*/
static void
saywhat(NCURSES_CONST char *text)
{
    wmove(stdscr, LINES - 1, 0);
    wclrtoeol(stdscr);
    if (text != 0 && *text != '\0') {
	waddstr(stdscr, text);
	waddstr(stdscr, "; ");
    }
    waddstr(stdscr, "press any key to continue");
}				/* end of saywhat */

/*+-------------------------------------------------------------------------
	mkpanel(rows,cols,tly,tlx) - alloc a win and panel and associate them
--------------------------------------------------------------------------*/
static PANEL *
mkpanel(NCURSES_COLOR_T color, int rows, int cols, int tly, int tlx)
{
    WINDOW *win;
    PANEL *pan = 0;

    if ((win = newwin(rows, cols, tly, tlx)) != 0) {
	if ((pan = new_panel(win)) == 0) {
	    delwin(win);
	} else if (UseColors) {
	    NCURSES_COLOR_T fg = (NCURSES_COLOR_T) ((color == COLOR_BLUE)
						    ? COLOR_WHITE
						    : COLOR_BLACK);
	    NCURSES_COLOR_T bg = color;

	    init_pair(color, fg, bg);
	    wbkgdset(win, (attr_t) (COLOR_PAIR(color) | ' '));
	} else {
	    wbkgdset(win, A_BOLD | ' ');
	}
    }
    return pan;
}				/* end of mkpanel */

/*+-------------------------------------------------------------------------
	rmpanel(pan)
--------------------------------------------------------------------------*/
static void
rmpanel(PANEL *pan)
{
    WINDOW *win = panel_window(pan);
    del_panel(pan);
    delwin(win);
}				/* end of rmpanel */

/*+-------------------------------------------------------------------------
	pflush()
--------------------------------------------------------------------------*/
static void
pflush(void)
{
    update_panels();
    doupdate();
}				/* end of pflush */

/*+-------------------------------------------------------------------------
	fill_panel(win)
--------------------------------------------------------------------------*/
static void
init_panel(WINDOW *win)
{
    register int y, x;

    for (y = 0; y < LINES - 1; y++) {
	for (x = 0; x < COLS; x++)
	    wprintw(win, "%d", (y + x) % 10);
    }
}

static void
fill_panel(PANEL *pan)
{
    WINDOW *win = panel_window(pan);
    const char *userptr = (const char *) panel_userptr(pan);
    int num = (userptr && *userptr) ? userptr[1] : '?';
    int y, x;

    wmove(win, 1, 1);
    wprintw(win, "-pan%c-", num);
    wclrtoeol(win);
    box(win, 0, 0);
    for (y = 2; y < getmaxy(win) - 1; y++) {
	for (x = 1; x < getmaxx(win) - 1; x++) {
	    wmove(win, y, x);
	    waddch(win, UChar(num));
	}
    }
}

#if USE_WIDEC_SUPPORT
static void
init_wide_panel(WINDOW *win)
{
    int digit;
    cchar_t temp[10];

    for (digit = 0; digit < 10; ++digit)
	make_fullwidth_digit(&temp[digit], digit);

    do {
	int y, x;
	getyx(stdscr, y, x);
	digit = (y + x / 2) % 10;
    } while (wadd_wch(win, &temp[digit]) != ERR);
}

static void
fill_wide_panel(PANEL *pan)
{
    WINDOW *win = panel_window(pan);
    const char *userptr = (const char *) panel_userptr(pan);
    int num = (userptr && *userptr) ? userptr[1] : '?';
    int y, x;

    wmove(win, 1, 1);
    wprintw(win, "-pan%c-", num);
    wclrtoeol(win);
    box(win, 0, 0);
    for (y = 2; y < getmaxy(win) - 1; y++) {
	for (x = 1; x < getmaxx(win) - 1; x++) {
	    wmove(win, y, x);
	    waddch(win, UChar(num));
	}
    }
}
#endif

#define MAX_PANELS 5

static void
canned_panel(PANEL *px[MAX_PANELS + 1], NCURSES_CONST char *cmd)
{
    int which = cmd[1] - '0';

    saywhat(cmd);
    switch (*cmd) {
    case 'h':
	hide_panel(px[which]);
	break;
    case 's':
	show_panel(px[which]);
	break;
    case 't':
	top_panel(px[which]);
	break;
    case 'b':
	bottom_panel(px[which]);
	break;
    case 'd':
	rmpanel(px[which]);
	break;
    }
    pflush();
    wait_a_while(nap_msec);
}

static int
demo_panels(void (*InitPanel) (WINDOW *), void (*FillPanel) (PANEL *))
{
    int count;
    int itmp;
    PANEL *px[MAX_PANELS + 1];

    scrollok(stdscr, FALSE);	/* we don't want stdscr to scroll! */
    refresh();

    InitPanel(stdscr);
    for (count = 0; count < 5; count++) {
	px[1] = mkpanel(COLOR_RED,
			LINES / 2 - 2,
			COLS / 8 + 1,
			0,
			0);
	set_panel_userptr(px[1], (NCURSES_CONST void *) "p1");

	px[2] = mkpanel(COLOR_GREEN,
			LINES / 2 + 1,
			COLS / 7,
			LINES / 4,
			COLS / 10);
	set_panel_userptr(px[2], (NCURSES_CONST void *) "p2");

	px[3] = mkpanel(COLOR_YELLOW,
			LINES / 4,
			COLS / 10,
			LINES / 2,
			COLS / 9);
	set_panel_userptr(px[3], (NCURSES_CONST void *) "p3");

	px[4] = mkpanel(COLOR_BLUE,
			LINES / 2 - 2,
			COLS / 8,
			LINES / 2 - 2,
			COLS / 3);
	set_panel_userptr(px[4], (NCURSES_CONST void *) "p4");

	px[5] = mkpanel(COLOR_MAGENTA,
			LINES / 2 - 2,
			COLS / 8,
			LINES / 2,
			COLS / 2 - 2);
	set_panel_userptr(px[5], (NCURSES_CONST void *) "p5");

	FillPanel(px[1]);
	FillPanel(px[2]);
	FillPanel(px[3]);
	FillPanel(px[4]);
	FillPanel(px[5]);

	hide_panel(px[4]);
	hide_panel(px[5]);
	pflush();
	saywhat("");
	wait_a_while(nap_msec);

	saywhat("h3 s1 s2 s4 s5");
	move_panel(px[1], 0, 0);
	hide_panel(px[3]);
	show_panel(px[1]);
	show_panel(px[2]);
	show_panel(px[4]);
	show_panel(px[5]);
	pflush();
	wait_a_while(nap_msec);

	canned_panel(px, "s1");
	canned_panel(px, "s2");

	saywhat("m2");
	move_panel(px[2], LINES / 3 + 1, COLS / 8);
	pflush();
	wait_a_while(nap_msec);

	canned_panel(px, "s3");

	saywhat("m3");
	move_panel(px[3], LINES / 4 + 1, COLS / 15);
	pflush();
	wait_a_while(nap_msec);

	canned_panel(px, "b3");
	canned_panel(px, "s4");
	canned_panel(px, "s5");
	canned_panel(px, "t3");
	canned_panel(px, "t1");
	canned_panel(px, "t2");
	canned_panel(px, "t3");
	canned_panel(px, "t4");

	for (itmp = 0; itmp < 6; itmp++) {
	    WINDOW *w4 = panel_window(px[4]);
	    WINDOW *w5 = panel_window(px[5]);

	    saywhat("m4");
	    wmove(w4, LINES / 8, 1);
	    waddstr(w4, mod[itmp]);
	    move_panel(px[4], LINES / 6, itmp * (COLS / 8));
	    wmove(w5, LINES / 6, 1);
	    waddstr(w5, mod[itmp]);
	    pflush();
	    wait_a_while(nap_msec);

	    saywhat("m5");
	    wmove(w4, LINES / 6, 1);
	    waddstr(w4, mod[itmp]);
	    move_panel(px[5], LINES / 3 - 1, (itmp * 10) + 6);
	    wmove(w5, LINES / 8, 1);
	    waddstr(w5, mod[itmp]);
	    pflush();
	    wait_a_while(nap_msec);
	}

	saywhat("m4");
	move_panel(px[4], LINES / 6, itmp * (COLS / 8));
	pflush();
	wait_a_while(nap_msec);

	canned_panel(px, "t5");
	canned_panel(px, "t2");
	canned_panel(px, "t1");
	canned_panel(px, "d2");
	canned_panel(px, "h3");
	canned_panel(px, "d1");
	canned_panel(px, "d4");
	canned_panel(px, "d5");
	canned_panel(px, "d3");

	wait_a_while(nap_msec);
	if (nap_msec == 1)
	    break;
	nap_msec = 100L;
    }

    erase();
    endwin();
    return OK;
}

#if USE_LIBPANEL
static int
panel_test(bool recur GCC_UNUSED)
{
    return demo_panels(init_panel, fill_panel);
}
#endif

#if USE_WIDEC_SUPPORT && USE_LIBPANEL
static int
x_panel_test(bool recur GCC_UNUSED)
{
    return demo_panels(init_wide_panel, fill_wide_panel);
}
#endif
#endif /* USE_LIBPANEL */

/****************************************************************************
 *
 * Pad tester
 *
 ****************************************************************************/

#if HAVE_NEWPAD

/* The behavior of mvhline, mvvline for negative/zero length is unspecified,
 * though we can rely on negative x/y values to stop the macro.
 */
static void
do_h_line(int y, int x, chtype c, int to)
{
    if ((to) > (x))
	MvHLine(y, x, c, (to) - (x));
}

static void
do_v_line(int y, int x, chtype c, int to)
{
    if ((to) > (y))
	MvVLine(y, x, c, (to) - (y));
}

#define GRIDSIZE	3

static bool pending_pan = FALSE;
static bool show_panner_legend = TRUE;

static int
panner_legend(int line)
{
    static const char *const legend[] =
    {
	"Use arrow keys (or U,D,L,R) to pan, ESC to quit, ! to shell-out.",
	"Use +,- (or j,k) to grow/shrink the panner vertically.",
	"Use <,> (or h,l) to grow/shrink the panner horizontally.",
	"Number repeats.  Toggle legend:? filler:a timer:t scrollmark:s."
    };
    int n = ((int) SIZEOF(legend) - (LINES - line));
    if (n >= 0 && n < (int) SIZEOF(legend)) {
	if (move(line, 0) != ERR) {
	    if (show_panner_legend)
		printw("%s", legend[n]);
	    clrtoeol();
	    return show_panner_legend;
	}
    }
    return FALSE;
}

static void
panner_h_cleanup(int from_y, int from_x, int to_x)
{
    if (!panner_legend(from_y))
	do_h_line(from_y, from_x, ' ', to_x);
}

static void
panner_v_cleanup(int from_y, int from_x, int to_y)
{
    if (!panner_legend(from_y))
	do_v_line(from_y, from_x, ' ', to_y);
}

static void
fill_pad(WINDOW *panpad, bool pan_lines, bool colored)
{
    int y, x;
    unsigned gridcount = 0;
    chtype fill = 0;
#ifdef A_COLOR
    if (colored)
	fill = (chtype) COLOR_PAIR(1);
#endif

    wmove(panpad, 0, 0);
    for (y = 0; y < getmaxy(panpad); y++) {
	for (x = 0; x < getmaxx(panpad); x++) {
	    if (y % GRIDSIZE == 0 && x % GRIDSIZE == 0) {
		if (y == 0 && x == 0)
		    waddch(panpad, pan_lines ? ACS_ULCORNER : '+');
		else if (y == 0)
		    waddch(panpad, pan_lines ? ACS_TTEE : '+');
		else if (y == 0 || x == 0)
		    waddch(panpad, pan_lines ? ACS_LTEE : '+');
		else
		    waddch(panpad, (chtype) ((pan_lines ? 'a' : 'A') +
					     (int) (gridcount++ % 26)) | fill);
	    } else if (y % GRIDSIZE == 0)
		waddch(panpad, pan_lines ? ACS_HLINE : '-');
	    else if (x % GRIDSIZE == 0)
		waddch(panpad, pan_lines ? ACS_VLINE : '|');
	    else
		waddch(panpad, ' ');
	}
    }
}

static void
panner(WINDOW *pad,
       int top_x, int top_y, int porty, int portx,
       int (*pgetc) (WINDOW *),
       bool colored)
{
#if HAVE_GETTIMEOFDAY
    struct timeval before, after;
    bool timing = TRUE;
#endif
    bool pan_lines = FALSE;
    bool scrollers = TRUE;
    int basex = 0;
    int basey = 0;
    int pxmax, pymax, lowend, highend, c;

    getmaxyx(pad, pymax, pxmax);
    scrollok(stdscr, FALSE);	/* we don't want stdscr to scroll! */

    c = KEY_REFRESH;
    do {
#ifdef NCURSES_VERSION
	/*
	 * During shell-out, the user may have resized the window.  Adjust
	 * the port size of the pad to accommodate this.  Ncurses automatically
	 * resizes all of the normal windows to fit on the new screen.
	 */
	if (top_x > COLS)
	    top_x = COLS;
	if (portx > COLS)
	    portx = COLS;
	if (top_y > LINES)
	    top_y = LINES;
	if (porty > LINES)
	    porty = LINES;
#endif
	switch (c) {
	case KEY_REFRESH:
	    erase();

	    /* FALLTHRU */
	case HELP_KEY_1:
	    if (c == HELP_KEY_1)
		show_panner_legend = !show_panner_legend;
	    panner_legend(LINES - 4);
	    panner_legend(LINES - 3);
	    panner_legend(LINES - 2);
	    panner_legend(LINES - 1);
	    break;
	case 'a':
	    pan_lines = !pan_lines;
	    fill_pad(pad, pan_lines, colored);
	    pending_pan = FALSE;
	    break;

#if HAVE_GETTIMEOFDAY
	case 't':
	    timing = !timing;
	    if (!timing)
		panner_legend(LINES - 1);
	    break;
#endif
	case 's':
	    scrollers = !scrollers;
	    break;

	    /* Move the top-left corner of the pad, keeping the bottom-right
	     * corner fixed.
	     */
	case 'h':		/* increase-columns: move left edge to left */
	    if (top_x <= 0)
		beep();
	    else {
		panner_v_cleanup(top_y, top_x, porty);
		top_x--;
	    }
	    break;

	case 'j':		/* decrease-lines: move top-edge down */
	    if (top_y >= porty)
		beep();
	    else {
		panner_h_cleanup(top_y - 1, top_x - (top_x > 0), portx);
		top_y++;
	    }
	    break;

	case 'k':		/* increase-lines: move top-edge up */
	    if (top_y <= 0)
		beep();
	    else {
		top_y--;
		panner_h_cleanup(top_y, top_x, portx);
	    }
	    break;

	case 'l':		/* decrease-columns: move left-edge to right */
	    if (top_x >= portx)
		beep();
	    else {
		panner_v_cleanup(top_y - (top_y > 0), top_x - 1, porty);
		top_x++;
	    }
	    break;

	    /* Move the bottom-right corner of the pad, keeping the top-left
	     * corner fixed.
	     */
	case KEY_IC:		/* increase-columns: move right-edge to right */
	    if (portx >= pxmax || portx >= COLS)
		beep();
	    else {
		panner_v_cleanup(top_y - (top_y > 0), portx - 1, porty);
		++portx;
	    }
	    break;

	case KEY_IL:		/* increase-lines: move bottom-edge down */
	    if (porty >= pymax || porty >= LINES)
		beep();
	    else {
		panner_h_cleanup(porty - 1, top_x - (top_x > 0), portx);
		++porty;
	    }
	    break;

	case KEY_DC:		/* decrease-columns: move bottom edge up */
	    if (portx <= top_x)
		beep();
	    else {
		portx--;
		panner_v_cleanup(top_y - (top_y > 0), portx, porty);
	    }
	    break;

	case KEY_DL:		/* decrease-lines */
	    if (porty <= top_y)
		beep();
	    else {
		porty--;
		panner_h_cleanup(porty, top_x - (top_x > 0), portx);
	    }
	    break;

	case KEY_LEFT:		/* pan leftwards */
	    if (basex > 0)
		basex--;
	    else
		beep();
	    break;

	case KEY_RIGHT:	/* pan rightwards */
	    if (basex + portx - (pymax > porty) < pxmax)
		basex++;
	    else
		beep();
	    break;

	case KEY_UP:		/* pan upwards */
	    if (basey > 0)
		basey--;
	    else
		beep();
	    break;

	case KEY_DOWN:		/* pan downwards */
	    if (basey + porty - (pxmax > portx) < pymax)
		basey++;
	    else
		beep();
	    break;

	case 'H':
	case KEY_HOME:
	case KEY_FIND:
	    basey = 0;
	    break;

	case 'E':
	case KEY_END:
	case KEY_SELECT:
	    basey = pymax - porty;
	    if (basey < 0)
		basey = 0;
	    break;

	default:
	    beep();
	    break;
	}

	MvAddCh(top_y - 1, top_x - 1, ACS_ULCORNER);
	do_v_line(top_y, top_x - 1, ACS_VLINE, porty);
	do_h_line(top_y - 1, top_x, ACS_HLINE, portx);

	if (scrollers && (pxmax > portx - 1)) {
	    int length = (portx - top_x - 1);
	    float ratio = ((float) length) / ((float) pxmax);

	    lowend = (int) ((float) top_x + ((float) basex * ratio));
	    highend = (int) ((float) top_x + ((float) (basex + length) * ratio));

	    do_h_line(porty - 1, top_x, ACS_HLINE, lowend);
	    if (highend < portx) {
		attron(A_REVERSE);
		do_h_line(porty - 1, lowend, ' ', highend + 1);
		attroff(A_REVERSE);
		do_h_line(porty - 1, highend + 1, ACS_HLINE, portx);
	    }
	} else
	    do_h_line(porty - 1, top_x, ACS_HLINE, portx);

	if (scrollers && (pymax > porty - 1)) {
	    int length = (porty - top_y - 1);
	    float ratio = ((float) length) / ((float) pymax);

	    lowend = (int) ((float) top_y + ((float) basey * ratio));
	    highend = (int) ((float) top_y + ((float) (basey + length) * ratio));

	    do_v_line(top_y, portx - 1, ACS_VLINE, lowend);
	    if (highend < porty) {
		attron(A_REVERSE);
		do_v_line(lowend, portx - 1, ' ', highend + 1);
		attroff(A_REVERSE);
		do_v_line(highend + 1, portx - 1, ACS_VLINE, porty);
	    }
	} else
	    do_v_line(top_y, portx - 1, ACS_VLINE, porty);

	MvAddCh(top_y - 1, portx - 1, ACS_URCORNER);
	MvAddCh(porty - 1, top_x - 1, ACS_LLCORNER);
	MvAddCh(porty - 1, portx - 1, ACS_LRCORNER);

	if (!pending_pan) {
#if HAVE_GETTIMEOFDAY
	    gettimeofday(&before, 0);
#endif
	    wnoutrefresh(stdscr);

	    pnoutrefresh(pad,
			 basey, basex,
			 top_y, top_x,
			 porty - (pxmax > portx) - 1,
			 portx - (pymax > porty) - 1);

	    doupdate();
#if HAVE_GETTIMEOFDAY
#define TIMEVAL2S(data) ((double) data.tv_sec + ((double) data.tv_usec / 1.0e6))
	    if (timing) {
		double elapsed;
		gettimeofday(&after, 0);
		elapsed = (TIMEVAL2S(after) - TIMEVAL2S(before));
		move(LINES - 1, COLS - 12);
		printw("Secs: %2.03f", elapsed);
		refresh();
	    }
#endif
	}

    } while
	((c = pgetc(pad)) != KEY_EXIT);

    scrollok(stdscr, TRUE);	/* reset to driver's default */
}

static int
padgetch(WINDOW *win)
{
    static int count;
    static int last;

    if ((pending_pan = (count > 0)) != FALSE) {
	count--;
	pending_pan = (count != 0);
    } else {
	for (;;) {
	    int c;
	    switch (c = wGetchar(win)) {
	    case '!':
		ShellOut(FALSE);
		/* FALLTHRU */
	    case CTRL('r'):
		endwin();
		refresh();
		c = KEY_REFRESH;
		break;
	    case CTRL('l'):
		c = KEY_REFRESH;
		break;
	    case 'U':
		c = KEY_UP;
		break;
	    case 'D':
		c = KEY_DOWN;
		break;
	    case 'R':
		c = KEY_RIGHT;
		break;
	    case 'L':
		c = KEY_LEFT;
		break;
	    case '+':
		c = KEY_IL;
		break;
	    case '-':
		c = KEY_DL;
		break;
	    case '>':
		c = KEY_IC;
		break;
	    case '<':
		c = KEY_DC;
		break;
	    case ERR:		/* FALLTHRU */
	    case case_QUIT:
		count = 0;
		c = KEY_EXIT;
		break;
	    default:
		if (c >= '0' && c <= '9') {
		    count = count * 10 + (c - '0');
		    continue;
		}
		break;
	    }
	    last = c;
	    break;
	}
	if (count > 0)
	    count--;
    }
    return (last);
}

#define PAD_HIGH 200
#define PAD_WIDE 200

static int
pad_test(bool recur GCC_UNUSED)
/* Demonstrate pads. */
{
    WINDOW *panpad = newpad(PAD_HIGH, PAD_WIDE);

    if (panpad == 0) {
	Cannot("cannot create requested pad");
	return ERR;
    }
#ifdef A_COLOR
    if (UseColors) {
	init_pair(1, COLOR_BLACK, COLOR_GREEN);
	init_pair(2, COLOR_CYAN, COLOR_BLUE);
	wbkgd(panpad, (chtype) (COLOR_PAIR(2) | ' '));
    }
#endif
    fill_pad(panpad, FALSE, TRUE);

    panner_legend(LINES - 4);
    panner_legend(LINES - 3);
    panner_legend(LINES - 2);
    panner_legend(LINES - 1);

    keypad(panpad, TRUE);

    /* Make the pad (initially) narrow enough that a trace file won't wrap.
     * We'll still be able to widen it during a test, since that's required
     * for testing boundaries.
     */
    panner(panpad, 2, 2, LINES - 5, COLS - 15, padgetch, TRUE);

    delwin(panpad);
    endwin();
    erase();
    return OK;
}
#endif /* HAVE_NEWPAD */

/****************************************************************************
 *
 * Tests from John Burnell's PDCurses tester
 *
 ****************************************************************************/

static void
Continue(WINDOW *win)
{
    noecho();
    wmove(win, 10, 1);
    MvWAddStr(win, 10, 1, " Press any key to continue");
    wrefresh(win);
    wGetchar(win);
}

static int
flushinp_test(bool recur GCC_UNUSED)
/* Input test, adapted from John Burnell's PDCurses tester */
{
    WINDOW *win = stdscr;
    int w, h, bx, by, sw, sh, i;

    WINDOW *subWin;
    wclear(win);

    getmaxyx(win, h, w);
    getbegyx(win, by, bx);
    sw = w / 3;
    sh = h / 3;
    if ((subWin = subwin(win, sh, sw, by + h - sh - 2, bx + w - sw - 2)) == 0)
	return ERR;

#ifdef A_COLOR
    if (UseColors) {
	init_pair(2, COLOR_CYAN, COLOR_BLUE);
	wbkgd(subWin, (chtype) (COLOR_PAIR(2) | ' '));
    }
#endif
    (void) wattrset(subWin, A_BOLD);
    box(subWin, ACS_VLINE, ACS_HLINE);
    MvWAddStr(subWin, 2, 1, "This is a subwindow");
    wrefresh(win);

    /*
     * This used to set 'nocbreak()'.  However, Alexander Lukyanov says that
     * it only happened to "work" on SVr4 because that implementation does not
     * emulate nocbreak+noecho mode, whereas ncurses does.  To get the desired
     * test behavior, we're using 'cbreak()', which will allow a single
     * character to return without needing a newline. - T.Dickey 1997/10/11.
     */
    cbreak();
    MvWAddStr(win, 0, 1, "This is a test of the flushinp() call.");

    MvWAddStr(win, 2, 1, "Type random keys for 5 seconds.");
    MvWAddStr(win, 3, 1,
	      "These should be discarded (not echoed) after the subwindow goes away.");
    wrefresh(win);

    for (i = 0; i < 5; i++) {
	MvWPrintw(subWin, 1, 1, "Time = %d", i);
	wrefresh(subWin);
	napms(1000);
	flushinp();
    }

    delwin(subWin);
    werase(win);
    flash();
    wrefresh(win);
    napms(1000);

    MvWAddStr(win, 2, 1,
	      "If you were still typing when the window timer expired,");
    MvWAddStr(win, 3, 1,
	      "or else you typed nothing at all while it was running,");
    MvWAddStr(win, 4, 1,
	      "test was invalid.  You'll see garbage or nothing at all. ");
    MvWAddStr(win, 6, 1, "Press a key");
    wmove(win, 9, 10);
    wrefresh(win);
    echo();
    wGetchar(win);
    flushinp();
    MvWAddStr(win, 12, 0,
	      "If you see any key other than what you typed, flushinp() is broken.");
    Continue(win);

    wmove(win, 9, 10);
    wdelch(win);
    wrefresh(win);
    wmove(win, 12, 0);
    clrtoeol();
    waddstr(win,
	    "What you typed should now have been deleted; if not, wdelch() failed.");
    Continue(win);

    cbreak();
    return OK;
}

/****************************************************************************
 *
 * Menu test
 *
 ****************************************************************************/

#if USE_LIBMENU

#define MENU_Y	8
#define MENU_X	8

static int
menu_virtualize(int c)
{
    if (c == '\n' || c == KEY_EXIT)
	return (MAX_COMMAND + 1);
    else if (c == 'u')
	return (REQ_SCR_ULINE);
    else if (c == 'd')
	return (REQ_SCR_DLINE);
    else if (c == 'b' || c == KEY_NPAGE)
	return (REQ_SCR_UPAGE);
    else if (c == 'f' || c == KEY_PPAGE)
	return (REQ_SCR_DPAGE);
    else if (c == 'n' || c == KEY_DOWN)
	return (REQ_NEXT_ITEM);
    else if (c == 'p' || c == KEY_UP)
	return (REQ_PREV_ITEM);
    else if (c == ' ')
	return (REQ_TOGGLE_ITEM);
    else {
	if (c != KEY_MOUSE)
	    beep();
	return (c);
    }
}

static CONST_MENUS char *animals[] =
{
    "Lions",
    "Tigers",
    "Bears",
    "(Oh my!)",
    "Newts",
    "Platypi",
    "Lemurs",
    "(Oh really?!)",
    "Leopards",
    "Panthers",
    "Pumas",
    "Lions, Tigers, Bears, (Oh my!), Newts, Platypi, Lemurs",
    "Lions, Tigers, Bears, (Oh my!), Newts, Platypi, Lemurs, Lions, Tigers, Bears, (Oh my!), Newts, Platypi, Lemurs",
    (char *) 0
};

static int
menu_test(bool recur GCC_UNUSED)
{
    MENU *m;
    ITEM *items[SIZEOF(animals)];
    ITEM **ip = items;
    CONST_MENUS char **ap;
    int mrows, mcols, c;
    WINDOW *menuwin;

#ifdef NCURSES_MOUSE_VERSION
    mousemask(ALL_MOUSE_EVENTS, (mmask_t *) 0);
#endif
    MvAddStr(0, 0, "This is the menu test:");
    MvAddStr(2, 0, "  Use up and down arrow to move the select bar.");
    MvAddStr(3, 0, "  'n' and 'p' act like arrows.");
    MvAddStr(4, 0,
	     "  'b' and 'f' scroll up/down (page), 'u' and 'd' (line).");
    MvAddStr(5, 0, "  Press return to exit.");
    refresh();

    for (ap = animals; *ap; ap++) {
	if ((*ip = new_item(*ap, "")) != 0)
	    ++ip;
    }
    *ip = (ITEM *) 0;

    m = new_menu(items);

    set_menu_format(m, (SIZEOF(animals) + 1) / 2, 1);
    scale_menu(m, &mrows, &mcols);

    menuwin = newwin(mrows + 2, mcols + 2, MENU_Y, MENU_X);
    set_menu_win(m, menuwin);
    keypad(menuwin, TRUE);
    box(menuwin, 0, 0);

    set_menu_sub(m, derwin(menuwin, mrows, mcols, 1, 1));

    post_menu(m);

    while ((c = menu_driver(m, menu_virtualize(wGetchar(menuwin)))) != E_UNKNOWN_COMMAND) {
	if (c == E_NOT_POSTED)
	    break;
	if (c == E_REQUEST_DENIED)
	    beep();
	continue;
    }

    MvPrintw(LINES - 2, 0,
	     "You chose: %s\n", item_name(current_item(m)));
    (void) addstr("Press any key to continue...");
    wGetchar(stdscr);

    unpost_menu(m);
    delwin(menuwin);

    free_menu(m);
    for (ip = items; *ip; ip++)
	free_item(*ip);
#ifdef NCURSES_MOUSE_VERSION
    mousemask(0, (mmask_t *) 0);
#endif
    return OK;
}

#ifdef TRACE
#define T_TBL(name) { #name, name }
static struct {
    const char *name;
    unsigned mask;
} t_tbl[] = {

    T_TBL(TRACE_DISABLE),
	T_TBL(TRACE_TIMES),
	T_TBL(TRACE_TPUTS),
	T_TBL(TRACE_UPDATE),
	T_TBL(TRACE_MOVE),
	T_TBL(TRACE_CHARPUT),
	T_TBL(TRACE_ORDINARY),
	T_TBL(TRACE_CALLS),
	T_TBL(TRACE_VIRTPUT),
	T_TBL(TRACE_IEVENT),
	T_TBL(TRACE_BITS),
	T_TBL(TRACE_ICALLS),
	T_TBL(TRACE_CCALLS),
	T_TBL(TRACE_DATABASE),
	T_TBL(TRACE_ATTRS),
	T_TBL(TRACE_MAXIMUM),
    {
	(char *) 0, 0
    }
};

static char *
tracetrace(unsigned tlevel)
{
    static char *buf;
    static size_t need = 12;
    int n;

    if (buf == 0) {
	for (n = 0; t_tbl[n].name != 0; n++)
	    need += strlen(t_tbl[n].name) + 2;
	buf = typeMalloc(char, need);
	if (!buf)
	    failed("tracetrace");
    }
    _nc_SPRINTF(buf, _nc_SLIMIT(need) "0x%02x = {", tlevel);
    if (tlevel == 0) {
	_nc_STRCAT(buf, t_tbl[0].name ? t_tbl[0].name : "", need);
	_nc_STRCAT(buf, ", ", need);
    } else {
	for (n = 1; t_tbl[n].name != 0; n++)
	    if ((tlevel & t_tbl[n].mask) == t_tbl[n].mask) {
		_nc_STRCAT(buf, t_tbl[n].name, need);
		_nc_STRCAT(buf, ", ", need);
	    }
    }
    if (buf[strlen(buf) - 2] == ',')
	buf[strlen(buf) - 2] = '\0';
    _nc_STRCAT(buf, "}", need);
    return buf;
}

/* fake a dynamically reconfigurable menu using the 0th entry to deselect
 * the others
 */
static int
run_trace_menu(MENU * m)
{
    ITEM **items;
    ITEM *i, **p;

    for (;;) {
	bool changed = FALSE;
	switch (menu_driver(m, menu_virtualize(wGetchar(menu_win(m))))) {
	case E_UNKNOWN_COMMAND:
	    return FALSE;
	default:
	    items = menu_items(m);
	    i = current_item(m);
	    if (i == items[0]) {
		if (item_value(i)) {
		    for (p = items + 1; *p != 0; p++)
			if (item_value(*p)) {
			    set_item_value(*p, FALSE);
			    changed = TRUE;
			}
		}
	    } else {
		for (p = items + 1; *p != 0; p++)
		    if (item_value(*p)) {
			set_item_value(items[0], FALSE);
			changed = TRUE;
			break;
		    }
	    }
	    if (!changed)
		return TRUE;
	}
    }
}

static int
trace_set(bool recur GCC_UNUSED)
/* interactively set the trace level */
{
    MENU *m;
    ITEM *items[SIZEOF(t_tbl)];
    ITEM **ip = items;
    int mrows, mcols;
    unsigned newtrace;
    int n;
    WINDOW *menuwin;

    MvAddStr(0, 0, "Interactively set trace level:");
    MvAddStr(2, 0, "  Press space bar to toggle a selection.");
    MvAddStr(3, 0, "  Use up and down arrow to move the select bar.");
    MvAddStr(4, 0, "  Press return to set the trace level.");
    MvPrintw(6, 0, "(Current trace level is %s)", tracetrace(_nc_tracing));

    refresh();

    for (n = 0; t_tbl[n].name != 0; n++) {
	if ((*ip = new_item(t_tbl[n].name, "")) != 0) {
	    ++ip;
	}
    }
    *ip = (ITEM *) 0;

    m = new_menu(items);

    set_menu_format(m, 0, 2);
    scale_menu(m, &mrows, &mcols);

    menu_opts_off(m, O_ONEVALUE);
    menuwin = newwin(mrows + 2, mcols + 2, MENU_Y, MENU_X);
    set_menu_win(m, menuwin);
    keypad(menuwin, TRUE);
    box(menuwin, 0, 0);

    set_menu_sub(m, derwin(menuwin, mrows, mcols, 1, 1));

    post_menu(m);

    for (ip = menu_items(m); *ip; ip++) {
	unsigned mask = t_tbl[item_index(*ip)].mask;
	if (mask == 0)
	    set_item_value(*ip, _nc_tracing == 0);
	else if ((mask & _nc_tracing) == mask)
	    set_item_value(*ip, TRUE);
    }

    while (run_trace_menu(m))
	continue;

    newtrace = 0;
    for (ip = menu_items(m); *ip; ip++)
	if (item_value(*ip))
	    newtrace |= t_tbl[item_index(*ip)].mask;
    curses_trace(newtrace);
    Trace(("trace level interactively set to %s", tracetrace(_nc_tracing)));

    MvPrintw(LINES - 2, 0,
	     "Trace level is %s\n", tracetrace(_nc_tracing));
    (void) addstr("Press any key to continue...");
    wGetchar(stdscr);

    unpost_menu(m);
    delwin(menuwin);

    free_menu(m);
    for (ip = items; *ip; ip++)
	free_item(*ip);

    return OK;
}
#endif /* TRACE */
#endif /* USE_LIBMENU */

/****************************************************************************
 *
 * Forms test
 *
 ****************************************************************************/
#if USE_LIBFORM
static FIELD *
make_label(int frow, int fcol, NCURSES_CONST char *label)
{
    FIELD *f = new_field(1, (int) strlen(label), frow, fcol, 0, 0);

    if (f) {
	set_field_buffer(f, 0, label);
	set_field_opts(f, (int) ((unsigned) field_opts(f) & ~O_ACTIVE));
    }
    return (f);
}

static FIELD *
make_field(int frow, int fcol, int rows, int cols, bool secure)
{
    FIELD *f = new_field(rows, cols, frow, fcol, 0, secure ? 1 : 0);

    if (f) {
	set_field_back(f, A_UNDERLINE);
	set_field_userptr(f, (void *) 0);
    }
    return (f);
}

static void
display_form(FORM *f)
{
    WINDOW *w;
    int rows, cols;

    scale_form(f, &rows, &cols);

    if ((w = newwin(rows + 2, cols + 4, 0, 0)) != (WINDOW *) 0) {
	set_form_win(f, w);
	set_form_sub(f, derwin(w, rows, cols, 1, 2));
	box(w, 0, 0);
	keypad(w, TRUE);
	if (post_form(f) != E_OK)
	    wrefresh(w);
    }
}

static void
erase_form(FORM *f)
{
    WINDOW *w = form_win(f);
    WINDOW *s = form_sub(f);

    unpost_form(f);
    werase(w);
    wrefresh(w);
    delwin(s);
    delwin(w);
}

static int
edit_secure(FIELD *me, int c)
{
    int rows, cols, frow, fcol, nrow, nbuf;

    if (field_info(me, &rows, &cols, &frow, &fcol, &nrow, &nbuf) == E_OK
	&& nbuf > 0) {
	char *source = field_buffer(me, 1);
	size_t have = (source ? strlen(source) : 0) + 1;
	size_t need = 80 + have;
	char *temp = malloc(need);

	if (temp != 0) {
	    size_t len;
	    _nc_STRNCPY(temp, source ? source : "", have + 1);
	    len = (size_t) (char *) field_userptr(me);
	    if (c <= KEY_MAX) {
		if (isgraph(c) && (len + 1) < sizeof(temp)) {
		    temp[len++] = (char) c;
		    temp[len] = 0;
		    set_field_buffer(me, 1, temp);
		    c = '*';
		} else {
		    c = 0;
		}
	    } else {
		switch (c) {
		case REQ_BEG_FIELD:
		case REQ_CLR_EOF:
		case REQ_CLR_EOL:
		case REQ_DEL_LINE:
		case REQ_DEL_WORD:
		case REQ_DOWN_CHAR:
		case REQ_END_FIELD:
		case REQ_INS_CHAR:
		case REQ_INS_LINE:
		case REQ_LEFT_CHAR:
		case REQ_NEW_LINE:
		case REQ_NEXT_WORD:
		case REQ_PREV_WORD:
		case REQ_RIGHT_CHAR:
		case REQ_UP_CHAR:
		    c = 0;	/* we don't want to do inline editing */
		    break;
		case REQ_CLR_FIELD:
		    if (len) {
			temp[0] = 0;
			set_field_buffer(me, 1, temp);
		    }
		    break;
		case REQ_DEL_CHAR:
		case REQ_DEL_PREV:
		    if (len) {
			temp[--len] = 0;
			set_field_buffer(me, 1, temp);
		    }
		    break;
		}
	    }
	    set_field_userptr(me, (void *) len);
	    free(temp);
	}
    }
    return c;
}

static int
form_virtualize(FORM *f, WINDOW *w)
{
    /* *INDENT-OFF* */
    static const struct {
	int code;
	int result;
    } lookup[] = {
	{ CTRL('A'),	REQ_NEXT_CHOICE },
	{ CTRL('B'),	REQ_PREV_WORD },
	{ CTRL('C'),	REQ_CLR_EOL },
	{ CTRL('D'),	REQ_DOWN_FIELD },
	{ CTRL('E'),	REQ_END_FIELD },
	{ CTRL('F'),	REQ_NEXT_PAGE },
	{ CTRL('G'),	REQ_DEL_WORD },
	{ CTRL('H'),	REQ_DEL_PREV },
	{ CTRL('I'),	REQ_INS_CHAR },
	{ CTRL('K'),	REQ_CLR_EOF },
	{ CTRL('L'),	REQ_LEFT_FIELD },
	{ CTRL('M'),	REQ_NEW_LINE },
	{ CTRL('N'),	REQ_NEXT_FIELD },
	{ CTRL('O'),	REQ_INS_LINE },
	{ CTRL('P'),	REQ_PREV_FIELD },
	{ CTRL('R'),	REQ_RIGHT_FIELD },
	{ CTRL('S'),	REQ_BEG_FIELD },
	{ CTRL('U'),	REQ_UP_FIELD },
	{ CTRL('V'),	REQ_DEL_CHAR },
	{ CTRL('W'),	REQ_NEXT_WORD },
	{ CTRL('X'),	REQ_CLR_FIELD },
	{ CTRL('Y'),	REQ_DEL_LINE },
	{ CTRL('Z'),	REQ_PREV_CHOICE },
	{ ESCAPE,	MAX_FORM_COMMAND + 1 },
	{ KEY_BACKSPACE, REQ_DEL_PREV },
	{ KEY_DOWN,	REQ_DOWN_CHAR },
	{ KEY_END,	REQ_LAST_FIELD },
	{ KEY_HOME,	REQ_FIRST_FIELD },
	{ KEY_LEFT,	REQ_LEFT_CHAR },
	{ KEY_LL,	REQ_LAST_FIELD },
	{ KEY_NEXT,	REQ_NEXT_FIELD },
	{ KEY_NPAGE,	REQ_NEXT_PAGE },
	{ KEY_PPAGE,	REQ_PREV_PAGE },
	{ KEY_PREVIOUS, REQ_PREV_FIELD },
	{ KEY_RIGHT,	REQ_RIGHT_CHAR },
	{ KEY_UP,	REQ_UP_CHAR },
	{ QUIT,		MAX_FORM_COMMAND + 1 }
    };
    /* *INDENT-ON* */

    static int mode = REQ_INS_MODE;
    int c = wGetchar(w);
    FIELD *me = current_field(f);
    bool current = TRUE;

    if (c == CTRL(']')) {
	if (mode == REQ_INS_MODE) {
	    mode = REQ_OVL_MODE;
	} else {
	    mode = REQ_INS_MODE;
	}
	c = mode;
    } else {
	unsigned n;
	for (n = 0; n < SIZEOF(lookup); n++) {
	    if (lookup[n].code == c) {
		c = lookup[n].result;
		break;
	    }
	}
    }
    MvPrintw(0, COLS - 6, "(%s)", mode == REQ_INS_MODE ? "INS" : "OVL");

    /*
     * Force the field that the user is typing into to be in reverse video,
     * while the other fields are shown underlined.
     */
    switch (c) {
    case REQ_BEG_FIELD:
    case REQ_CLR_EOF:
    case REQ_CLR_EOL:
    case REQ_CLR_FIELD:
    case REQ_DEL_CHAR:
    case REQ_DEL_LINE:
    case REQ_DEL_PREV:
    case REQ_DEL_WORD:
    case REQ_END_FIELD:
    case REQ_INS_CHAR:
    case REQ_INS_LINE:
    case REQ_LEFT_CHAR:
    case REQ_LEFT_FIELD:
    case REQ_NEXT_WORD:
    case REQ_RIGHT_CHAR:
	current = TRUE;
	break;
    default:
	current = (c < KEY_MAX);
	break;
    }
    if (current) {
	c = edit_secure(me, c);
	set_field_back(me, A_REVERSE);
    } else {
	c = edit_secure(me, c);
	set_field_back(me, A_UNDERLINE);
    }
    return c;
}

static int
my_form_driver(FORM *form, int c)
{
    if (c == (MAX_FORM_COMMAND + 1)
	&& form_driver(form, REQ_VALIDATION) == E_OK)
	return (TRUE);
    else {
	beep();
	return (FALSE);
    }
}

#ifdef NCURSES_VERSION
#define FIELDCHECK_CB(func) bool func(FIELD * fld, const void * data GCC_UNUSED)
#define CHAR_CHECK_CB(func) bool func(int ch, const void *data GCC_UNUSED)
#else
#define FIELDCHECK_CB(func) int func(FIELD * fld, char * data GCC_UNUSED)
#define CHAR_CHECK_CB(func) int func(int ch, char *data GCC_UNUSED)
#endif

/*
 * Allow a middle initial, optionally with a '.' to end it.
 */
static
FIELDCHECK_CB(mi_field_check)
{
    char *s = field_buffer(fld, 0);
    int state = 0;
    int n;

    for (n = 0; s[n] != '\0'; ++n) {
	switch (state) {
	case 0:
	    if (s[n] == '.') {
		if (n != 1)
		    return FALSE;
		state = 2;
	    } else if (isspace(UChar(s[n]))) {
		state = 2;
	    }
	    break;
	case 2:
	    if (!isspace(UChar(s[n])))
		return FALSE;
	    break;
	}
    }

    /* force the form to display a leading capital */
    if (islower(UChar(s[0]))) {
	s[0] = (char) toupper(UChar(s[0]));
	set_field_buffer(fld, 0, s);
    }
    return TRUE;
}

static
CHAR_CHECK_CB(mi_char_check)
{
    return ((isalpha(ch) || ch == '.') ? TRUE : FALSE);
}

/*
 * Passwords should be at least 6 characters.
 */
static
FIELDCHECK_CB(pw_field_check)
{
    char *s = field_buffer(fld, 0);
    int n;

    for (n = 0; s[n] != '\0'; ++n) {
	if (isspace(UChar(s[n]))) {
	    if (n < 6)
		return FALSE;
	}
    }
    return TRUE;
}

static
CHAR_CHECK_CB(pw_char_check)
{
    return (isgraph(ch) ? TRUE : FALSE);
}

static int
form_test(bool recur GCC_UNUSED)
{
    FORM *form;
    FIELD *f[12], *secure;
    FIELDTYPE *fty_middle = new_fieldtype(mi_field_check, mi_char_check);
    FIELDTYPE *fty_passwd = new_fieldtype(pw_field_check, pw_char_check);
    int c;
    unsigned n = 0;

#ifdef NCURSES_MOUSE_VERSION
    mousemask(ALL_MOUSE_EVENTS, (mmask_t *) 0);
#endif

    move(18, 0);
    addstr("Defined edit/traversal keys:   ^Q/ESC- exit form\n");
    addstr("^N   -- go to next field       ^P  -- go to previous field\n");
    addstr("Home -- go to first field      End -- go to last field\n");
    addstr("^L   -- go to field to left    ^R  -- go to field to right\n");
    addstr("^U   -- move upward to field   ^D  -- move downward to field\n");
    addstr("^W   -- go to next word        ^B  -- go to previous word\n");
    addstr("^S   -- go to start of field   ^E  -- go to end of field\n");
    addstr("^H   -- delete previous char   ^Y  -- delete line\n");
    addstr("^G   -- delete current word    ^C  -- clear to end of line\n");
    addstr("^K   -- clear to end of field  ^X  -- clear field\n");
    addstr("Arrow keys move within a field as you would expect. ^] toggles overlay mode.");

    MvAddStr(4, 57, "Forms Entry Test");

    refresh();

    /* describe the form */
    memset(f, 0, sizeof(f));
    f[n++] = make_label(0, 15, "Sample Form");

    f[n++] = make_label(2, 0, "Last Name");
    f[n++] = make_field(3, 0, 1, 18, FALSE);
    set_field_type(f[n - 1], TYPE_ALPHA, 1);

    f[n++] = make_label(2, 20, "First Name");
    f[n++] = make_field(3, 20, 1, 12, FALSE);
    set_field_type(f[n - 1], TYPE_ALPHA, 1);

    f[n++] = make_label(2, 34, "Middle Name");
    f[n++] = make_field(3, 34, 1, 12, FALSE);
    set_field_type(f[n - 1], fty_middle);

    f[n++] = make_label(5, 0, "Comments");
    f[n++] = make_field(6, 0, 4, 46, FALSE);

    f[n++] = make_label(5, 20, "Password:");
    secure =
	f[n++] = make_field(5, 30, 1, 9, TRUE);
    set_field_type(f[n - 1], fty_passwd);
    f[n] = (FIELD *) 0;

    if ((form = new_form(f)) != 0) {
	WINDOW *w;
	int finished = 0;

	display_form(form);

	w = form_win(form);
	raw();
	nonl();			/* lets us read ^M's */
	while (!finished) {
	    switch (form_driver(form, c = form_virtualize(form, w))) {
	    case E_OK:
		MvAddStr(5, 57, field_buffer(secure, 1));
		clrtoeol();
		refresh();
		break;
	    case E_UNKNOWN_COMMAND:
		finished = my_form_driver(form, c);
		break;
	    default:
		beep();
		break;
	    }
	}

	erase_form(form);

	free_form(form);
    }
    for (c = 0; f[c] != 0; c++)
	free_field(f[c]);
    free_fieldtype(fty_middle);
    free_fieldtype(fty_passwd);
    noraw();
    nl();

#ifdef NCURSES_MOUSE_VERSION
    mousemask(ALL_MOUSE_EVENTS, (mmask_t *) 0);
#endif
    return OK;
}
#endif /* USE_LIBFORM */

/****************************************************************************
 *
 * Overlap test
 *
 ****************************************************************************/

#if HAVE_COPYWIN		/* ...and overlay, overwrite */

static const int overlap_HEAD = 1;
static const int overlap_FOOT = 6;

static WINDOW *
make_overlap(int n)
{
    WINDOW *result;
    int y, x;

    getmaxyx(stdscr, y, x);
    if (y < 23 || x < 80) {
	Cannot("The screen is too small for this test");
	result = 0;
    } else {
	int ymax = y - (overlap_HEAD + overlap_FOOT);
	int high = ymax / 5;	/* equal-sized parts for cross */
	int xmax = x - 2;	/* margin */
	int wide = (xmax / 5) & ~1;
	int lmar, tmar;

	if (high > 8)
	    high = 8;

	if (wide > 8)
	    wide = 8;

	tmar = (ymax - (5 * high)) / 2 + overlap_HEAD;
	lmar = (xmax - (5 * wide)) / 2;

	if (n == 0) {
	    result = newwin(3 * high, 3 * wide, tmar, lmar);
	} else {
	    result = newwin(3 * high, 3 * wide, tmar + 2 * high, lmar + 2 * wide);
	}
    }
    return result;
}

static void
clear_overlap(void)
{
    int row;

    for (row = overlap_HEAD; row < LINES - overlap_FOOT; ++row) {
	move(row, 0);
	clrtoeol();
    }
}

static int
move_overlap(int shift, WINDOW *win1)
{
    int ymax = getmaxy(stdscr) - (overlap_HEAD + overlap_FOOT);
    int high = ymax / 5;	/* equal-sized parts for cross */
    int tmar;
    int xmax1 = getmaxx(win1) + 1;
    int lmar1 = (COLS - (5 * (xmax1) / 3)) / 2;
    int rc = ERR;

    if (high > 8)
	high = 8;
    tmar = (ymax - (5 * high)) / 2 + overlap_HEAD;

    rc = mvwin(win1, tmar, lmar1 + shift);
    return rc;
}

static void
fillwin(WINDOW *win, char ch)
{
    int y, x;
    int y1, x1;

    getmaxyx(win, y1, x1);
    for (y = 0; y < y1; y++) {
	wmove(win, y, 0);
	for (x = 0; x < x1; x++)
	    waddch(win, UChar(ch));
    }
}

#define InCross(x,y, x1,y1) \
	    (((x > (x1 - 1) / 3) && (x <= (2 * (x1 - 1)) / 3)) \
		|| (((y > (y1 - 1) / 3) && (y <= (2 * (y1 - 1)) / 3))))

static void
crosswin(WINDOW *win, char ch)
{
    int y, x;
    int y1, x1;
    int xw = 1;

    getmaxyx(win, y1, x1);
    for (y = 0; y < y1; y++) {
	for (x = 0; x < x1; x += xw) {
	    if (InCross(x, y, x1, y1)) {
		wmove(win, y, x);
		waddch(win, UChar(ch));
	    }
	}
    }
}

/*
 * Match "crosswin()", but using line-drawing characters.  This could be done
 * a little simpler using box(), but the reason for this example is to test
 * hline/vline and addch with line-drawing vs the copy/overlay functions.
 */
static void
crossbox(WINDOW *win)
{
    int y1, x1;
    int ymax, xmax;

    getmaxyx(win, y1, x1);

    ymax = (y1 + 1);
    xmax = (x1 + 1);

    mvwhline(win, 0, (xmax / 3), ACS_HLINE, (xmax / 3));
    mvwhline(win, ymax / 3, 0, ACS_HLINE, xmax);
    mvwhline(win, ((2 * ymax) / 3) - 1, 0, ACS_HLINE, xmax);
    mvwhline(win, y1 - 1, (xmax / 3), ACS_HLINE, (xmax / 3));

    mvwvline(win, (ymax / 3), 0, ACS_VLINE, (ymax / 3));
    mvwvline(win, 0, xmax / 3, ACS_VLINE, ymax);
    mvwvline(win, 0, ((2 * xmax) / 3) - 1, ACS_VLINE, ymax);
    mvwvline(win, (ymax / 3), x1 - 1, ACS_VLINE, (ymax / 3));

    mvwaddch(win, 0, (xmax / 3), ACS_ULCORNER);
    mvwaddch(win, 0, ((2 * xmax) / 3) - 1, ACS_URCORNER);
    mvwaddch(win, y1 - 1, (xmax / 3), ACS_LLCORNER);
    mvwaddch(win, y1 - 1, ((2 * xmax) / 3) - 1, ACS_LRCORNER);

    mvwaddch(win, (ymax / 3), 0, ACS_ULCORNER);
    mvwaddch(win, ((2 * ymax) / 3) - 1, 0, ACS_LLCORNER);
    mvwaddch(win, (ymax / 3), x1 - 1, ACS_URCORNER);
    mvwaddch(win, ((2 * ymax) / 3) - 1, x1 - 1, ACS_LRCORNER);

    mvwaddch(win, (ymax / 3), (xmax / 3), ACS_PLUS);
    mvwaddch(win, (ymax / 3), ((2 * xmax) / 3) - 1, ACS_PLUS);
    mvwaddch(win, ((2 * ymax) / 3) - 1, ((2 * xmax) / 3) - 1, ACS_PLUS);
    mvwaddch(win, ((2 * ymax) / 3) - 1, (xmax / 3), ACS_PLUS);
}

typedef enum {
    otBASE_refresh = 0
    ,otBASE_fill
    ,otBASE_draw
    ,otBASE_clear
    ,otBASE_copy
} otBASE;

#define OVERLAP_FLAVORS 6

typedef enum {
    otFILL_normal = 0
    ,otFILL_bold
    ,otFILL_color
    ,otFILL_bright
} otFILL;

#define LimitFILL() UseColors ? 4 : 2

typedef enum {
    otDRAW_text_cross = 0
    ,otDRAW_line_box
    ,otDRAW_line_cross
    ,otDRAW_set_bg
    ,otDRAW_reset_bg
} otDRAW;

#define LimitDRAW() UseColors ? 5 : 3

typedef enum {
    otCOPY_overwrite = 0
    ,otCOPY_merge
    ,otCOPY_force
    ,otCOPY_overlay
} otCOPY;

#define LimitCOPY() 4

static void
overlap_helpitem(int state, int item, char *message)
{
    int row = (item / 2);
    int col = ((item % 2) ? COLS / 2 : 0);

    move(LINES - 6 + row, col);
    printw("%c%c = %s", state == row ? '>' : ' ', 'a' + item, message);
    clrtoeol();
}

static void
overlap_test_1_attr(WINDOW *win, int flavor, int col)
{
    NCURSES_PAIRS_T cpair = (NCURSES_PAIRS_T) (1 + (flavor * 2) + col);

    switch ((otFILL) flavor) {
    case otFILL_normal:
	(void) wattrset(win, A_NORMAL);
	break;
    case otFILL_bold:
	(void) wattrset(win, A_BOLD);
	break;
    case otFILL_color:
	init_pair(cpair, COLOR_BLUE, COLOR_WHITE);
	(void) wattrset(win, AttrArg(COLOR_PAIR(cpair), A_NORMAL));
	break;
    case otFILL_bright:
	init_pair(cpair, COLOR_WHITE, COLOR_BLUE);
	(void) wattrset(win, AttrArg(COLOR_PAIR(cpair), A_BOLD));
	break;
    }
}

static void
overlap_test_2_attr(WINDOW *win, int flavor, int col)
{
    NCURSES_PAIRS_T cpair = (NCURSES_PAIRS_T) (9 + (flavor * 2) + col);

    switch ((otDRAW) flavor) {
    case otDRAW_text_cross:
	/* no effect */
	break;
    case otDRAW_line_box:
	/* no effect */
	break;
    case otDRAW_line_cross:
	/* no effect */
	break;
    case otDRAW_set_bg:
	init_pair(cpair, COLOR_RED, COLOR_GREEN);
	wbkgdset(win, colored_chtype(' ', A_BLINK, cpair));
	break;
    case otDRAW_reset_bg:
	wbkgdset(win, ' ' | A_NORMAL);
	break;
    }
}

static int
overlap_help(int state, int flavors[OVERLAP_FLAVORS])
{
    int item;
    int limit[OVERLAP_FLAVORS];
    char msg[80];

    if (state < 0)
	state += OVERLAP_FLAVORS;
    state = state % OVERLAP_FLAVORS;
    assert(state >= 0 && state < OVERLAP_FLAVORS);

    for (item = 0; item < (2 * OVERLAP_FLAVORS); ++item) {
	int row = item / 2;
	int col = item % 2;
	const char *ths = col ? "B" : "A";
	const char *tht = col ? "A" : "B";

	switch ((otBASE) row) {
	case otBASE_refresh:
	    limit[row] = 1;
	    flavors[row] = 0;
	    _nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			"refresh %s, then %s, then doupdate.", ths, tht);
	    break;
	case otBASE_fill:
	    limit[row] = LimitFILL();
	    flavors[row] %= limit[row];
	    overlap_test_1_attr(stdscr, flavors[row], col);
	    _nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			"fill window %s with letter %s.", ths, ths);
	    break;
	case otBASE_draw:
	    limit[row] = LimitDRAW();
	    flavors[row] %= limit[row];
	    switch ((otDRAW) flavors[row]) {
	    case otDRAW_text_cross:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "cross text-pattern in window %s.", ths);
		break;
	    case otDRAW_line_box:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "draw line-box in window %s.", ths);
		break;
	    case otDRAW_line_cross:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "draw line-cross in window %s.", ths);
		break;
	    case otDRAW_set_bg:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "set background of window %s.", ths);
		break;
	    case otDRAW_reset_bg:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "reset background of window %s.", ths);
		break;
	    }
	    break;
	case otBASE_clear:
	    limit[row] = 1;
	    flavors[row] = 0;
	    _nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			"clear window %s.", ths);
	    break;
	case otBASE_copy:
	    limit[row] = LimitCOPY();
	    flavors[row] %= limit[row];
	    switch ((otCOPY) flavors[row]) {
	    case otCOPY_overwrite:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "overwrite %s onto %s.", ths, tht);
		break;
	    case otCOPY_merge:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "copywin(FALSE) %s onto %s.", ths, tht);
		break;
	    case otCOPY_force:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "copywin(TRUE) %s onto %s.", ths, tht);
		break;
	    case otCOPY_overlay:
		_nc_SPRINTF(msg, _nc_SLIMIT(sizeof(msg))
			    "overlay %s onto %s.", ths, tht);
		break;
	    }
	    break;
	}
	overlap_helpitem(state, item, msg);
	(void) wattrset(stdscr, A_NORMAL);
	wbkgdset(stdscr, ' ' | A_NORMAL);
    }
    move(LINES - 1, 0);
    printw("^Q/ESC = terminate test. </> shift. Up/down/space select (row %d",
	   state + 1);
    if (limit[state] > 1)
	printw(" test %d:%d", 1 + flavors[state], limit[state]);
    printw(").");
    clrtoeol();

    return state;
}

static void
overlap_test_0(WINDOW *a, WINDOW *b)
{
    touchwin(a);
    touchwin(b);
    wnoutrefresh(a);
    wnoutrefresh(b);
    doupdate();
}

static void
overlap_test_1(int flavor, int col, WINDOW *a, char fill)
{
    overlap_test_1_attr(a, flavor, col);
    fillwin(a, fill);
    (void) wattrset(a, A_NORMAL);
}

static void
overlap_test_2(int flavor, int col, WINDOW *a, char fill)
{
    overlap_test_2_attr(a, flavor, col);
    switch ((otDRAW) flavor) {
    case otDRAW_text_cross:
	crosswin(a, fill);
	break;
    case otDRAW_line_box:
	box(a, 0, 0);
	break;
    case otDRAW_line_cross:
	crossbox(a);
	break;
    case otDRAW_set_bg:
	/* done in overlap_test_2_attr */
	break;
    case otDRAW_reset_bg:
	/* done in overlap_test_2_attr */
	break;
    }
}

static void
overlap_test_3(WINDOW *a)
{
    wclear(a);
    wmove(a, 0, 0);
}

static void
overlap_test_4(int flavor, WINDOW *a, WINDOW *b)
{
    switch ((otCOPY) flavor) {
    case otCOPY_overwrite:
	overwrite(a, b);
	break;
    case otCOPY_merge:
	copywin(a, b, 0, 0, 0, 0, getmaxy(b), getmaxx(b), FALSE);
	break;
    case otCOPY_force:
	copywin(a, b, 0, 0, 0, 0, getmaxy(b), getmaxx(b), TRUE);
	break;
    case otCOPY_overlay:
	overlay(a, b);
	break;
    }
}

/* test effects of overlapping windows */
static int
overlap_test(bool recur GCC_UNUSED)
{
    WINDOW *win1, *win2;
    int ch;
    int shift = 0, last_refresh = -1;
    int state, flavor[OVERLAP_FLAVORS];

    if ((win1 = make_overlap(0)) == 0) {
	return ERR;
    } else if ((win2 = make_overlap(1)) == 0) {
	delwin(win1);
	return ERR;
    }

    curs_set(0);
    raw();
    refresh();
    move(0, 0);
    printw("Test wnoutrefresh() for two overlapping windows:");

    memset(flavor, 0, sizeof(flavor));
    state = overlap_help(0, flavor);

    while (!isQuit(ch = Getchar(), TRUE)) {
	switch (ch) {
	case 'a':		/* refresh window A first, then B */
	    overlap_test_0(win1, win2);
	    break;

	case 'b':		/* refresh window B first, then A */
	    overlap_test_0(win2, win1);
	    break;

	case 'c':		/* fill window A so it is visible */
	    overlap_test_1(flavor[otBASE_fill], 0, win1, 'A');
	    break;

	case 'd':		/* fill window B so it is visible */
	    overlap_test_1(flavor[otBASE_fill], 1, win2, 'B');
	    break;

	case 'e':		/* cross test pattern in window A */
	    overlap_test_2(flavor[otBASE_draw], 0, win1, 'A');
	    break;

	case 'f':		/* cross test pattern in window A */
	    overlap_test_2(flavor[otBASE_draw], 1, win2, 'B');
	    break;

	case 'g':		/* clear window A */
	    overlap_test_3(win1);
	    break;

	case 'h':		/* clear window B */
	    overlap_test_3(win2);
	    break;

	case 'i':		/* overwrite A onto B */
	    overlap_test_4(flavor[otBASE_copy], win1, win2);
	    break;

	case 'j':		/* overwrite B onto A */
	    overlap_test_4(flavor[otBASE_copy], win2, win1);
	    break;

	case CTRL('n'):
	case KEY_DOWN:
	    state = overlap_help(state + 1, flavor);
	    break;

	case CTRL('p'):
	case KEY_UP:
	    state = overlap_help(state - 1, flavor);
	    break;

	case ' ':
	    flavor[state] += 1;
	    state = overlap_help(state, flavor);
	    break;

	case HELP_KEY_1:
	    state = overlap_help(state, flavor);
	    break;

	case '<':
	    /* FALLTHRU */
	case '>':
	    /* see below */
	    break;

	default:
	    beep();
	    break;
	}

	switch (ch) {
	case 'a':
	    /* FALLTHRU */
	case 'b':
	    last_refresh = ch;
	    break;
	case '<':
	    shift -= 2;
	    /* FALLTHRU */
	case '>':
	    shift += 1;
	    if (move_overlap(shift, win1) != OK) {
		flash();
		shift += (ch == '>') ? -1 : 1;
	    } else if (last_refresh > 0) {
		clear_overlap();
		wnoutrefresh(stdscr);
		if (last_refresh == 'a')
		    overlap_test_0(win1, win2);
		else
		    overlap_test_0(win2, win1);
	    }
	    break;
	default:
	    last_refresh = -1;
	    break;
	}
    }

    delwin(win2);
    delwin(win1);
    erase();
    stop_curses();
    return OK;
}

#if USE_WIDEC_SUPPORT
static void
x_fillwin(WINDOW *win, wchar_t ch)
{
    int y, x;
    int y1, x1;

    getmaxyx(win, y1, x1);
    x1 /= 2;
    for (y = 0; y < y1; y++) {
	wmove(win, y, 0);
	for (x = 0; x < x1; x++)
	    waddnwstr(win, &ch, 1);
    }
}

static void
x_crosswin(WINDOW *win, wchar_t ch)
{
    int y, x;
    int y1, x1;
    int xw = 2;

    getmaxyx(win, y1, x1);
    for (y = 0; y < y1; y++) {
	for (x = 0; x < x1; x += xw) {
	    if (InCross(x, y, x1, y1)) {
		wmove(win, y, x);
		waddnwstr(win, &ch, 1);
	    }
	}
    }
}

static void
x_overlap_test_1(int flavor, int col, WINDOW *a, wchar_t fill)
{
    overlap_test_1_attr(a, flavor, col);
    x_fillwin(a, fill);
    (void) wattrset(a, A_NORMAL);
}

static void
x_overlap_test_2(int flavor, int col, WINDOW *a, wchar_t fill)
{
    overlap_test_2_attr(a, flavor, col);
    switch ((otDRAW) flavor) {
    case otDRAW_text_cross:
	x_crosswin(a, fill);
	break;
    case otDRAW_line_box:
	box(a, 0, 0);
	break;
    case otDRAW_line_cross:
	crossbox(a);
	break;
    case otDRAW_set_bg:
	/* done in overlap_test_2_attr */
	break;
    case otDRAW_reset_bg:
	/* done in overlap_test_2_attr */
	break;
    }
}

/* test effects of overlapping windows */
static int
x_overlap_test(bool recur GCC_UNUSED)
{
    const wchar_t WIDE_A = 0xff21;
    const wchar_t WIDE_B = 0xff22;
    WINDOW *win1, *win2;
    int ch;
    int shift = 0, last_refresh = -1;
    int state, flavor[OVERLAP_FLAVORS];

    if ((win1 = make_overlap(0)) == 0) {
	return ERR;
    } else if ((win2 = make_overlap(1)) == 0) {
	delwin(win1);
	return ERR;
    }

    curs_set(0);
    raw();
    refresh();
    move(0, 0);
    printw("Test wnoutrefresh() for overlapping windows with double-cell characters:");

    memset(flavor, 0, sizeof(flavor));
    state = overlap_help(0, flavor);

    while (!isQuit(ch = Getchar(), TRUE)) {
	switch (ch) {
	case 'a':		/* refresh window A first, then B */
	    overlap_test_0(win1, win2);
	    break;

	case 'b':		/* refresh window B first, then A */
	    overlap_test_0(win2, win1);
	    break;

	case 'c':		/* fill window A so it is visible */
	    x_overlap_test_1(flavor[otBASE_fill], 0, win1, WIDE_A);
	    break;

	case 'd':		/* fill window B so it is visible */
	    x_overlap_test_1(flavor[otBASE_fill], 1, win2, WIDE_B);
	    break;

	case 'e':		/* cross test pattern in window A */
	    x_overlap_test_2(flavor[otBASE_draw], 0, win1, WIDE_A);
	    break;

	case 'f':		/* cross test pattern in window A */
	    x_overlap_test_2(flavor[otBASE_draw], 1, win2, WIDE_B);
	    break;

	case 'g':		/* clear window A */
	    overlap_test_3(win1);
	    break;

	case 'h':		/* clear window B */
	    overlap_test_3(win2);
	    break;

	case 'i':		/* overwrite A onto B */
	    overlap_test_4(flavor[otBASE_copy], win1, win2);
	    break;

	case 'j':		/* overwrite B onto A */
	    overlap_test_4(flavor[otBASE_copy], win2, win1);
	    break;

	case CTRL('n'):
	case KEY_DOWN:
	    state = overlap_help(state + 1, flavor);
	    break;

	case CTRL('p'):
	case KEY_UP:
	    state = overlap_help(state - 1, flavor);
	    break;

	case ' ':
	    flavor[state] += 1;
	    state = overlap_help(state, flavor);
	    break;

	case HELP_KEY_1:
	    state = overlap_help(state, flavor);
	    break;

	case '<':
	    /* FALLTHRU */
	case '>':
	    /* see below */
	    break;

	default:
	    beep();
	    break;
	}

	switch (ch) {
	case 'a':
	    /* FALLTHRU */
	case 'b':
	    last_refresh = ch;
	    break;
	case '<':
	    shift -= 2;
	    /* FALLTHRU */
	case '>':
	    shift += 1;
	    if (move_overlap(shift, win1) != OK) {
		flash();
		shift += (ch == '>') ? -1 : 1;
	    } else if (last_refresh > 0) {
		clear_overlap();
		wnoutrefresh(stdscr);
		if (last_refresh == 'a')
		    overlap_test_0(win1, win2);
		else
		    overlap_test_0(win2, win1);
	    }
	    break;
	default:
	    last_refresh = -1;
	    break;
	}
    }

    delwin(win2);
    delwin(win1);
    erase();
    stop_curses();
    return OK;
}
#endif /* USE_WIDEC_SUPPORT */

#endif /* HAVE_COPYWIN */

static void
show_setting_name(const char *name)
{
    printw("%-25s ", name);
}

static void
show_string_setting(const char *name, const char *value)
{
    show_setting_name(name);
    if (value) {
	printw("\"%s\"", value);
    } else {
	attron(A_REVERSE);
	addstr("<NULL>");
	attroff(A_REVERSE);
    }
    AddCh('\n');
}

static void
show_number_setting(const char *name, int value)
{
    show_setting_name(name);
    if (value >= 0) {
	printw("%d", value);
    } else {
	attron(A_REVERSE);
	printw("%d", value);
	attroff(A_REVERSE);
    }
    AddCh('\n');
}

static void
show_boolean_setting(const char *name, int value)
{
    show_setting_name(name);
    if (value >= 0) {
	printw("%s", value ? "TRUE" : "FALSE");
    } else {
	attron(A_REVERSE);
	printw("%d", value);
	attroff(A_REVERSE);
    }
    AddCh('\n');
}

static int
settings_test(bool recur GCC_UNUSED)
{
#if USE_WIDEC_SUPPORT
    wchar_t ch;
#endif

    move(0, 0);
    show_string_setting("termname", termname());
    show_string_setting("longname", longname());
    show_number_setting("baudrate", baudrate());
    if (erasechar() > 0) {
	show_string_setting("unctrl(erasechar)", unctrl((chtype) erasechar()));
	show_string_setting("keyname(erasechar)", keyname(erasechar()));
    }
    if (killchar() > 0) {
	show_string_setting("unctrl(killchar)", unctrl((chtype) killchar()));
	show_string_setting("keyname(killchar)", keyname(killchar()));
    }
#if USE_WIDEC_SUPPORT
    if (erasewchar(&ch) == OK) {
	show_string_setting("key_name(erasewchar)", key_name(ch));
    }
    if (killwchar(&ch) == OK) {
	show_string_setting("key_name(killwchar)", key_name(ch));
    }
#endif
    show_boolean_setting("has_ic", has_ic());
    show_boolean_setting("has_il", has_il());
    show_boolean_setting("has_colors", has_colors());
#if HAVE_COLOR_CONTENT
    show_boolean_setting("can_change_color", can_change_color());
#endif
    show_setting_name("LINES");
    printw("%d\n", LINES);
    show_setting_name("COLS");
    printw("%d\n", COLS);
    Pause();
    erase();
    stop_curses();
    return OK;
}

/****************************************************************************
 *
 * Main sequence
 *
 ****************************************************************************/

static void
usage(void)
{
    static const char *const tbl[] =
    {
	"Usage: ncurses [options]"
	,""
	,"Options:"
#ifdef NCURSES_VERSION
	,"  -a f,b   set default-colors (assumed white-on-black)"
	,"  -d       use default-colors if terminal supports them"
#endif
#if HAVE_USE_ENV
	,"  -E       call use_env(FALSE) to ignore $LINES and $COLUMNS"
#endif
#if USE_SOFTKEYS
	,"  -e fmt   specify format for soft-keys test (e)"
#endif
#if HAVE_RIPOFFLINE
	,"  -f       rip-off footer line (can repeat)"
	,"  -h       rip-off header line (can repeat)"
#endif
	,"  -m       do not use colors"
#if HAVE_COLOR_CONTENT
	,"  -p file  rgb values to use in 'd' rather than ncurses's builtin"
#endif
#if USE_LIBPANEL
	,"  -s msec  specify nominal time for panel-demo (default: 1, to hold)"
#endif
#if defined(NCURSES_VERSION_PATCH) && (NCURSES_VERSION_PATCH >= 20120714) && !defined(_NC_WINDOWS)
	,"  -T       call use_tioctl(TRUE) to allow SIGWINCH to override environment"
#endif
#ifdef TRACE
	,"  -t mask  specify default trace-level (may toggle with ^T)"
#endif
#if HAVE_COLOR_CONTENT
	,"  -x       use xterm-compatible control for reading color palette"
#endif
    };
    size_t n;
    for (n = 0; n < SIZEOF(tbl); n++)
	fprintf(stderr, "%s\n", tbl[n]);
    ExitProgram(EXIT_FAILURE);
}

static void
set_terminal_modes(void)
{
    noraw();
    cbreak();
    noecho();
    scrollok(stdscr, TRUE);
    idlok(stdscr, TRUE);
    keypad(stdscr, TRUE);
}

#ifdef SIGUSR1
static void
announce_sig(int sig)
{
    (void) fprintf(stderr, "Handled signal %d\r\n", sig);
}
#endif

#if HAVE_RIPOFFLINE
static int
rip_footer(WINDOW *win, int cols)
{
    wbkgd(win, A_REVERSE);
    werase(win);
    wmove(win, 0, 0);
    wprintw(win, "footer: window %p, %d columns", (void *) win, cols);
    wnoutrefresh(win);
    return OK;
}

static int
rip_header(WINDOW *win, int cols)
{
    wbkgd(win, A_REVERSE);
    werase(win);
    wmove(win, 0, 0);
    wprintw(win, "header: window %p, %d columns", (void *) win, cols);
    wnoutrefresh(win);
    return OK;
}
#endif /* HAVE_RIPOFFLINE */

static void
main_menu(bool top)
{
#if USE_WIDEC_SUPPORT
    typedef struct {
	bool recur;
	int (*narrow_func) (bool);
	int (*wide_func) (bool);
	int code;
	const char *help;
    } MyCmds;
#define BOTH(a)   a, x_ ## a
#define ONLY(a)   a, NULL
#define CMDS(recur, funcs,code,help) { recur, funcs, code, help }
#else
    typedef struct {
	bool recur;
	int (*narrow_func) (bool);
	int code;
	const char *help;
    } MyCmds;
#define BOTH(a)   a
#define ONLY(a)   a
#define CMDS(recur, funcs,code,help) { recur, funcs, code, help }
#endif
    /* *INDENT-OFF* */
    static MyCmds cmds[] =
    {
	CMDS(TRUE, BOTH(getch_test),	'a', "keyboard and mouse input test"),
	CMDS(TRUE, BOTH(attr_test),	'b', "character attribute test"),
	CMDS(TRUE, BOTH(color_test),	'c', "color test pattern"),
#if HAVE_COLOR_CONTENT
	CMDS(FALSE, ONLY(color_edit),	'd', "edit RGB color values"),
#endif
#if USE_SOFTKEYS
	CMDS(TRUE, BOTH(slk_test),	'e', "exercise soft keys"),
#endif
	CMDS(TRUE, BOTH(acs_test),	'f', "display ACS characters"),
	CMDS(TRUE, ONLY(scroll_test),   'g', "display windows and scrolling"),
	CMDS(TRUE, ONLY(flushinp_test),	'i', "test flushinp()"),
	CMDS(TRUE, ONLY(sgr_attr_test),	'k', "display character attributes"),
#if USE_LIBMENU
	CMDS(TRUE, ONLY(menu_test),	'm', "exercise menu library"),
#endif
#if USE_LIBPANEL
	CMDS(TRUE, BOTH(panel_test),	'o', "exercise panel library"),
#endif
#if HAVE_NEWPAD
	CMDS(TRUE, ONLY(pad_test),	'p', "exercise pad features"),
#endif
	CMDS(TRUE, ONLY(NULL),		'q', "quit"),
#if USE_LIBMENU
	CMDS(TRUE, ONLY(form_test),	'r', "exercise form library"),
#endif
#if HAVE_COPYWIN
	CMDS(TRUE, BOTH(overlap_test),	's', "overlapping-refresh test"),
#endif
#if USE_LIBMENU && defined(TRACE)
	CMDS(TRUE, ONLY(trace_set),	't', "set trace level"),
#endif
	CMDS(TRUE, ONLY(settings_test),	'v', "show terminal name and settings"),
	CMDS(FALSE, ONLY(NULL),		'?', "repeat this command summary")
    };
    /* *INDENT-ON* */

    int (*doit) (bool);
    char command;
    unsigned n;
    do {
	printf("This is the ncurses main menu (uppercase for wide-characters)\n");
	for (n = 0; n < SIZEOF(cmds); ++n) {
	    if (top || cmds[n].recur) {
		putchar(' ');
#if USE_WIDEC_SUPPORT
		if (cmds[n].wide_func) {
		    printf("%c,", toupper(cmds[n].code));
		}
#endif
		printf("%c\t= %s\n", cmds[n].code, cmds[n].help);
	    }
	}

	(void) fputs("> ", stdout);
	(void) fflush(stdout);	/* necessary under SVr4 curses */

	/*
	 * This used to be an 'fgets()' call (until 1996/10).  However with
	 * some runtime libraries, mixing stream I/O and 'read()' causes the
	 * input stream to be flushed when switching between the two.
	 */
	command = 0;
	for (;;) {
	    char ch = '\0';
	    if (read(fileno(stdin), &ch, (size_t) 1) <= 0) {
		int save_err = errno;
		perror("\nOOPS");
		if (save_err == EINTR) {
		    clearerr(stdin);
		    continue;
		} else if (command == 0) {
		    command = 'q';
		}
		break;
	    } else if (command == 0 && !isspace(UChar(ch))) {
		command = ch;
	    } else if (ch == '\n' || ch == '\r') {
		if ((command == 'd') && !top) {
		    (void) fputs("Do not nest test-d\n", stdout);
		    command = 0;
		}
		if (command != 0)
		    break;
		(void) fputs("> ", stdout);
		(void) fflush(stdout);
	    }
	}

	doit = NULL;
	for (n = 0; n < SIZEOF(cmds); ++n) {
	    if (cmds[n].code == command) {
		doit = cmds[n].narrow_func;
		break;
	    }
#if USE_WIDEC_SUPPORT
	    if (toupper(cmds[n].code) == command) {
		doit = cmds[n].wide_func;
		break;
	    }
#endif
	}

	if (doit != NULL && doit(FALSE) == OK) {
	    /*
	     * This may be overkill; it is intended to reset everything back
	     * to the initial terminal modes so that tests don't get in
	     * each other's way.
	     */
	    flushinp();
	    set_terminal_modes();
	    reset_prog_mode();
	    clear();
	    refresh();
	    endwin();
	    if (command == '?') {
		(void) puts("This is the ncurses capability tester.");
		(void)
		    puts("You may select a test from the main menu by typing the");
		(void)
		    puts("key letter of the choice (the letter to left of the =)");
		(void)
		    puts("at the > prompt.  Type `q' to exit.");
	    }
	    continue;
	}
    } while
	(command != 'q');
}

/*+-------------------------------------------------------------------------
	main(argc,argv)
--------------------------------------------------------------------------*/

int
main(int argc, char *argv[])
{
    int c;
    int my_e_param = 1;
#ifdef NCURSES_VERSION_PATCH
#if HAVE_USE_DEFAULT_COLORS
    int default_fg = COLOR_WHITE;
    int default_bg = COLOR_BLACK;
    bool default_colors = FALSE;
#if HAVE_ASSUME_DEFAULT_COLORS
    bool assumed_colors = FALSE;
#endif
#endif
#endif
    bool monochrome = FALSE;
#if HAVE_COLOR_CONTENT
    bool xterm_colors = FALSE;
    char *palette_file = 0;
#endif

    setlocale(LC_ALL, "");

    while ((c = getopt(argc, argv, "a:dEe:fhmp:s:Tt:x")) != -1) {
	switch (c) {
#ifdef NCURSES_VERSION_PATCH
#if HAVE_USE_DEFAULT_COLORS
#if HAVE_ASSUME_DEFAULT_COLORS
	case 'a':
	    assumed_colors = TRUE;
	    switch (sscanf(optarg, "%d,%d", &default_fg, &default_bg)) {
	    case 0:
		default_fg = COLOR_WHITE;
		/* FALLTHRU */
	    case 1:
		default_bg = COLOR_BLACK;
		break;
	    }
	    break;
#endif
	case 'd':
	    default_colors = TRUE;
	    break;
#endif
#endif
#if HAVE_USE_ENV
	case 'E':
	    use_env(FALSE);
	    break;
#endif
	case 'e':
	    my_e_param = atoi(optarg);
#ifdef NCURSES_VERSION
	    if (my_e_param > 3)	/* allow extended layouts */
		usage();
#else
	    if (my_e_param > 1)
		usage();
#endif
	    break;
#if HAVE_RIPOFFLINE
	case 'f':
	    ripoffline(-1, rip_footer);
	    break;
	case 'h':
	    ripoffline(1, rip_header);
	    break;
#endif /* HAVE_RIPOFFLINE */
	case 'm':
	    monochrome = TRUE;
	    break;
#if HAVE_COLOR_CONTENT
	case 'p':
	    palette_file = optarg;
	    break;
#endif
#if USE_LIBPANEL
	case 's':
	    nap_msec = (int) atol(optarg);
	    break;
#endif
#if defined(NCURSES_VERSION_PATCH) && (NCURSES_VERSION_PATCH >= 20120714) && !defined(_NC_WINDOWS)
	case 'T':
	    use_tioctl(TRUE);
	    break;
#endif
#ifdef TRACE
	case 't':
	    save_trace = (unsigned) strtol(optarg, 0, 0);
	    break;
#endif
#if HAVE_COLOR_CONTENT
	case 'x':
	    xterm_colors = TRUE;
	    break;
#endif
	default:
	    usage();
	}
    }

    /*
     * If there's no menus (unlikely for ncurses!), then we'll have to set
     * tracing on initially, just in case the user wants to test something that
     * doesn't involve wGetchar.
     */
#ifdef TRACE
    /* enable debugging */
#if !USE_LIBMENU
    curses_trace(save_trace);
#else
    if (!isatty(fileno(stdin)))
	curses_trace(save_trace);
#endif /* USE_LIBMENU */
#endif /* TRACE */

#if USE_SOFTKEYS
    /* tell it we're going to play with soft keys */
    slk_init(my_e_param);
#endif

#ifdef SIGUSR1
    /* set up null signal catcher so we can see what interrupts to getch do */
    signal(SIGUSR1, announce_sig);
#endif

    /* we must initialize the curses data structure only once */
    initscr();
    bkgdset(BLANK);

    set_terminal_modes();
    def_prog_mode();

    /* tests, in general, will want these modes */
    UseColors = (bool) (monochrome ? FALSE : has_colors());

    if (UseColors) {
	start_color();
#ifdef NCURSES_VERSION_PATCH
	MaxColors = COLORS;	/* was > 16 ? 16 : COLORS */
#if HAVE_USE_DEFAULT_COLORS
	if (default_colors) {
	    use_default_colors();
	    MinColors = -1;
	}
#if HAVE_ASSUME_DEFAULT_COLORS
	if (assumed_colors)
	    assume_default_colors(default_fg, default_bg);
#endif
#endif
#else /* normal SVr4 curses */
	MaxColors = COLORS;	/* was > 8 ? 8 : COLORS */
#endif
	max_pairs = COLOR_PAIRS;	/* was > 256 ? 256 : COLOR_PAIRS */

#if HAVE_COLOR_CONTENT
	if (can_change_color()) {
	    init_all_colors(xterm_colors, palette_file);
	}
#endif
    }

    /*
     * Return to terminal mode, so we're guaranteed of being able to
     * select terminal commands even if the capabilities are wrong.
     */
    endwin();

#if HAVE_CURSES_VERSION
    (void) printf("Welcome to %s.  Press ? for help.\n", curses_version());
#elif defined(NCURSES_VERSION_MAJOR) && defined(NCURSES_VERSION_MINOR) && defined(NCURSES_VERSION_PATCH)
    (void) printf("Welcome to ncurses %d.%d.%d.  Press ? for help.\n",
		  NCURSES_VERSION_MAJOR,
		  NCURSES_VERSION_MINOR,
		  NCURSES_VERSION_PATCH);
#else
    (void) puts("Welcome to ncurses.  Press ? for help.");
#endif

    main_menu(TRUE);

    ExitProgram(EXIT_SUCCESS);
}

/* ncurses.c ends here */
