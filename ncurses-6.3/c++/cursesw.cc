// * this is for making emacs happy: -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 1998-2012,2014 Free Software Foundation, Inc.                  *
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
 * Authors:
 *	Thomas E. Dickey
 *	Juergen Pfeifer
 *
 * The NCursesWindow class was originally based on a file written by
 * Eric Newton, later modified by Ulrich Drepper and Anatoly Ivasyuk.
 * However, aside from the compatible interface definition, no trace
 * of the original code remains in this version: it consists only of
 * changes introduced since 1995.
 */

#include "internal.h"
#include "cursesw.h"

MODULE_ID("$Id: cursesw.cc,v 1.56 2020/02/02 23:34:34 tom Exp $")

#define COLORS_NEED_INITIALIZATION  -1
#define COLORS_NOT_INITIALIZED       0
#define COLORS_MONOCHROME            1
#define COLORS_ARE_REALLY_THERE      2

#define HaveColors() (colorInitialized == COLORS_ARE_REALLY_THERE)

// declare static variables for the class
long NCursesWindow::count = 0L;
bool NCursesWindow::b_initialized = FALSE;

int
NCursesWindow::scanw(const char* fmt, ...)
{
    int result = ERR;

    va_list args;
    va_start(args, fmt);
    result = ::vw_scanw (w, const_cast<NCURSES_CONST char *>(fmt), args);
    va_end(args);

    return result;
}


int
NCursesWindow::scanw(int y, int x, const char* fmt, ...)
{
    int result = ERR;

    if (::wmove(w, y, x) != ERR) {
	va_list args;
	va_start(args, fmt);
	result = ::vw_scanw (w, const_cast<NCURSES_CONST char *>(fmt), args);
	va_end(args);
    }
    return result;
}


int
NCursesWindow::scanw(const char* fmt, va_list args)
{
    int result = ERR;

    result = ::vw_scanw (w, const_cast<NCURSES_CONST char *>(fmt), args);

    return result;
}


int
NCursesWindow::scanw(int y, int x, const char* fmt, va_list args)
{
    int result = ERR;

    if (::wmove(w, y, x) != ERR) {
	result = ::vw_scanw (w, const_cast<NCURSES_CONST char *>(fmt), args);
    }
    return result;
}


int
NCursesWindow::printw(const char * fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int result = ::vw_printw(w, fmt, args);
    va_end(args);
    return result;
}


int
NCursesWindow::printw(int y, int x, const char * fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    int result = ::wmove(w, y, x);
    if (result == OK) {
	result = ::vw_printw(w, fmt, args);
    }
    va_end(args);
    return result;
}


int
NCursesWindow::printw(const char * fmt, va_list args)
{
    int result = ::vw_printw(w, fmt, args);
    return result;
}


int
NCursesWindow::printw(int y, int x, const char * fmt, va_list args)
{
    int result = ::wmove(w, y, x);
    if (result == OK) {
	result = ::vw_printw(w, fmt, args);
    }
    return result;
}


void
NCursesWindow::set_keyboard(void)
{
    keypad(TRUE);
    meta(TRUE);
}

void
NCursesWindow::err_handler(const char *msg) const THROWS(NCursesException)
{
  THROW(new NCursesException(msg));
}

void
NCursesWindow::initialize()
{
    if (!b_initialized) {
	::initscr();
	b_initialized = TRUE;
	if (colorInitialized == COLORS_NEED_INITIALIZATION) {
	    colorInitialized = COLORS_NOT_INITIALIZED;
	    useColors();
	}
	::noecho();
	::cbreak();
    }
}

void
NCursesWindow::constructing()
{
    initialize();
    ++count;
}

NCursesWindow::NCursesWindow()
  : w(0), alloced(FALSE), par(0), subwins(0), sib(0)
{
    constructing();

    w = static_cast<WINDOW *>(0);
}

NCursesWindow::NCursesWindow(int nlines, int ncols, int begin_y, int begin_x)
  : w(0), alloced(TRUE), par(0), subwins(0), sib(0)
{
    constructing();

    w = ::newwin(nlines, ncols, begin_y, begin_x);
    if (w == 0) {
	err_handler("Cannot construct window");
    }
    set_keyboard();
}

NCursesWindow::NCursesWindow(WINDOW* window)
  : w(0), alloced(FALSE), par(0), subwins(0), sib(0)
{
    constructing();

    // We used to use a reference on the "window" parameter, but we cannot do
    // that with an opaque pointer (see NCURSES_OPAQUE).  If the parameter was
    // "::stdscr", that is first set via the "constructing() call, and is null
    // up to that point.  So we allow a null pointer here as meaning the "same"
    // as "::stdscr".
    w = window ? window : ::stdscr;
    set_keyboard();
}

NCursesWindow::NCursesWindow(NCursesWindow& win, int ny, int nx,
			     int begin_y, int begin_x, char absrel)
  : w(0), alloced(TRUE), par(0), subwins(0), sib(0)
{
    constructing();
    if (absrel == 'a') {	// absolute origin
	begin_y -= win.begy();
	begin_x -= win.begx();
    }

    // Link this window into its parent's list of subwindows.
    // We use derwin(), since this also works for pads.
    w = ::derwin(win.w, ny, nx, begin_y, begin_x);
    if (w == 0) {
	err_handler("Cannot construct subwindow");
    }

    par = &win;
    sib = win.subwins;
    win.subwins = this;
}

NCursesWindow::NCursesWindow(NCursesWindow& win,
				bool do_box NCURSES_PARAM_INIT(TRUE))
  : w(0), alloced(TRUE), par(0), subwins(0), sib(0)
{
    constructing();
    int myHeight = win.height();
    int myWidth  = win.width();
    w = :: derwin(win.w, myHeight - 2, myWidth - 2, 1, 1);
    if (w == 0) {
	err_handler("Cannot construct subwindow");
    }

    par = &win;
    sib = win.subwins;
    win.subwins = this;
    subwins = 0;

    if (do_box) {
	win.box();
	win.touchwin();
    }
}

NCursesWindow NCursesWindow::Clone()
{
    WINDOW *d = ::dupwin(w);
    NCursesWindow W(d);
    W.subwins = subwins;
    W.sib = sib;
    W.par = par;
    W.alloced = alloced;
    return W;
}

typedef int (*RIPOFFINIT)(NCursesWindow&);
static RIPOFFINIT R_INIT[5];       // There can't be more
static int r_init_idx   = 0;
static RIPOFFINIT* prip = R_INIT;

NCursesWindow::NCursesWindow(WINDOW *win, int ncols)
  : w(0), alloced(FALSE), par(0), subwins(0), sib(0)
{
    (void) ncols;
    initialize();
    w = win;
}

int _nc_xx_ripoff_init(WINDOW *w, int ncols)
{
    (void) ncols;
    int res = ERR;

    RIPOFFINIT init = *prip++;
    if (init) {
	res = init(*(new NCursesWindow(w,ncols)));
    }
    return res;
}

int NCursesWindow::ripoffline(int ripoff_lines,
			      int (*init)(NCursesWindow& win))
{
    int code = ::_nc_ripoffline(ripoff_lines,_nc_xx_ripoff_init);
    if (code == OK && init && ripoff_lines) {
	R_INIT[r_init_idx++] = init;
    }
    return code;
}

bool
NCursesWindow::isDescendant(NCursesWindow& win)
{
    bool result = FALSE;

    for (NCursesWindow* p = subwins; p != NULL; p = p->sib) {
	if (p == &win || p->isDescendant(win)) {
	    result = TRUE;
	    break;
	}
    }
    return result;
}

void
NCursesWindow::kill_subwindows()
{
    NCursesWindow* p = subwins;

    subwins = 0;
    while (p != 0) {
	NCursesWindow* q = p->sib;
	p->kill_subwindows();
	if (p->alloced) {
	    if (p->w != 0)
		::delwin(p->w);
	}
	delete p;
	p = q;
    }
}


NCursesWindow::~NCursesWindow() THROWS(NCursesException)
{
    kill_subwindows();

    if (par != 0) {
	// Remove this window from the parent's list of subwindows.
	NCursesWindow * next = par->subwins;
	NCursesWindow * prev = 0;
	while (next != 0) {
	    if (next == this) {
		if (prev != 0) {
		    prev->sib = next->sib;
		} else {
		    par->subwins = next->sib;
		}
		break;
	    }
	    prev = next;
	    next = next->sib;
	}
    }

    if (alloced && w != 0)
	::delwin(w);

    if (alloced) {
	--count;
	if (count == 0) {
	    ::endwin();
	} else if (count < 0) { // cannot happen!
	    err_handler("Too many windows destroyed");
	}
    }
}

// ---------------------------------------------------------------------
// Color stuff
//
int NCursesWindow::colorInitialized = COLORS_NOT_INITIALIZED;

void
NCursesWindow::useColors(void)
{
    if (colorInitialized == COLORS_NOT_INITIALIZED) {
	if (b_initialized) {
	    if (::has_colors()) {
		::start_color();
		colorInitialized = COLORS_ARE_REALLY_THERE;
	    } else {
		colorInitialized = COLORS_MONOCHROME;
	    }
	} else {
	    colorInitialized = COLORS_NEED_INITIALIZATION;
	}
    }
}

NCURSES_PAIRS_T
NCursesWindow::getPair() const
{
    return static_cast<NCURSES_PAIRS_T>(PAIR_NUMBER(getattrs(w)));
}

NCURSES_COLOR_T
NCursesWindow::getcolor(int getback) const
{
    NCURSES_COLOR_T fore, back;

    if (HaveColors()) {
	if (::pair_content(getPair(), &fore, &back) == ERR)
	    err_handler("Can't get color pair");
    } else {
	// Monochrome means white on black
	back = COLOR_BLACK;
	fore = COLOR_WHITE;
    }
    return getback ? back : fore;
}

int NCursesWindow::NumberOfColors()
{
    return (HaveColors()) ? COLORS : 1;
}

NCURSES_PAIRS_T
NCursesWindow::getcolor() const
{
    return (HaveColors()) ? getPair() : 0;
}

int
NCursesWindow::setpalette(NCURSES_COLOR_T fore, NCURSES_COLOR_T back, NCURSES_PAIRS_T pair)
{
    return (HaveColors()) ? ::init_pair(pair, fore, back) : OK;
}

int
NCursesWindow::setpalette(NCURSES_COLOR_T fore, NCURSES_COLOR_T back)
{
    return setpalette(fore, back, getPair());
}


int
NCursesWindow::setcolor(NCURSES_PAIRS_T pair)
{
    if (HaveColors()) {
	if ((pair < 1) || (pair > COLOR_PAIRS))
	    err_handler("Can't set color pair");

	attroff(A_COLOR);
	attrset(COLOR_PAIR(pair));
    }
    return OK;
}

#if HAVE_HAS_KEY
bool NCursesWindow::has_mouse() const
{
    return ((::has_key(KEY_MOUSE) || ::has_mouse())
	     ? TRUE : FALSE);
}
#endif
