/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
 * Copyright 2008-2016,2017 Free Software Foundation, Inc.                  *
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
 *  Author: Juergen Pfeifer                                                 *
 *     and: Thomas E. Dickey                                                *
 ****************************************************************************/

/*
 * TODO - improve screen-repainting performance, using implied wraparound to reduce write's
 * TODO - make it optional whether screen is restored or not when non-buffered
 */

#include <curses.priv.h>
#ifdef _NC_WINDOWS
#if (defined(__MINGW32__) || defined(__MINGW64__))
#include <wchar.h>
#else
#include <tchar.h>
#endif
#include <io.h>

#define CUR TerminalType(my_term).

MODULE_ID("$Id: win32_driver.c,v 1.2 2020/11/21 23:35:56 tom Exp $")

#define WINMAGIC NCDRV_MAGIC(NCDRV_WINCONSOLE)
#define EXP_OPTIMIZE 0

static bool console_initialized = FALSE;

#define AssertTCB() assert(TCB != 0 && (TCB->magic == WINMAGIC))
#define validateConsoleHandle() (AssertTCB() , console_initialized ||\
                                 (console_initialized=\
                                  _nc_console_checkinit(TRUE,FALSE)))
#define SetSP() assert(TCB->csp != 0); sp = TCB->csp; (void) sp
#define AdjustY() (WINCONSOLE.buffered ?\
                   0 : (int) WINCONSOLE.SBI.srWindow.Top)
#define RevAttr(attr) (WORD) (((attr) & 0xff00) |   \
                              ((((attr) & 0x07) << 4) | \
                               (((attr) & 0x70) >> 4)))

#if USE_WIDEC_SUPPORT
#define write_screen WriteConsoleOutputW
#define read_screen  ReadConsoleOutputW
#else
#define write_screen WriteConsoleOutput
#define read_screen  ReadConsoleOutput
#endif

static WORD
MapAttr(WORD res, attr_t ch)
{
    if (ch & A_COLOR) {
	int p;

	p = PairNumber(ch);
	if (p > 0 && p < CON_NUMPAIRS) {
	    WORD a;
	    a = WINCONSOLE.pairs[p];
	    res = (WORD) ((res & 0xff00) | a);
	}
    }

    if (ch & A_REVERSE) {
	res = RevAttr(res);
    }

    if (ch & A_STANDOUT) {
	res = RevAttr(res) | BACKGROUND_INTENSITY;
    }

    if (ch & A_BOLD)
	res |= FOREGROUND_INTENSITY;

    if (ch & A_DIM)
	res |= BACKGROUND_INTENSITY;

    return res;
}

#if 0				/* def TRACE */
static void
dump_screen(const char *fn, int ln)
{
    int max_cells = (WINCONSOLE.SBI.dwSize.Y *
		     (1 + WINCONSOLE.SBI.dwSize.X)) + 1;
    char output[max_cells];
    CHAR_INFO save_screen[max_cells];
    COORD save_size;
    SMALL_RECT save_region;
    COORD bufferCoord;

    T(("dump_screen %s@%d", fn, ln));

    save_region.Top = WINCONSOLE.SBI.srWindow.Top;
    save_region.Left = WINCONSOLE.SBI.srWindow.Left;
    save_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
    save_region.Right = WINCONSOLE.SBI.srWindow.Right;

    save_size.X = (SHORT) (save_region.Right - save_region.Left + 1);
    save_size.Y = (SHORT) (save_region.Bottom - save_region.Top + 1);

    bufferCoord.X = bufferCoord.Y = 0;

    if (read_screen(WINCONSOLE.hdl,
		    save_screen,
		    save_size,
		    bufferCoord,
		    &save_region)) {
	int i, j;
	int ij = 0;
	int k = 0;

	for (i = save_region.Top; i <= save_region.Bottom; ++i) {
	    for (j = save_region.Left; j <= save_region.Right; ++j) {
		output[k++] = save_screen[ij++].Char.AsciiChar;
	    }
	    output[k++] = '\n';
	}
	output[k] = 0;

	T(("DUMP: %d,%d - %d,%d",
	   save_region.Top,
	   save_region.Left,
	   save_region.Bottom,
	   save_region.Right));
	T(("%s", output));
    }
}

#else
#define dump_screen(fn,ln)	/* nothing */
#endif

#if USE_WIDEC_SUPPORT
/*
 * TODO: support surrogate pairs
 * TODO: support combining characters
 * TODO: support acsc
 * TODO: _nc_wacs should be part of sp.
 */
static BOOL
con_write16(TERMINAL_CONTROL_BLOCK * TCB,
	    int y, int x, cchar_t *str, int limit)
{
    int actual = 0;
    CHAR_INFO *ci = TypeAlloca(CHAR_INFO, limit);
    COORD loc, siz;
    SMALL_RECT rec;
    int i;
    cchar_t ch;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    for (i = actual = 0; i < limit; i++) {
	ch = str[i];
	if (isWidecExt(ch))
	    continue;
	ci[actual].Char.UnicodeChar = CharOf(ch);
	ci[actual].Attributes = MapAttr(WINCONSOLE.SBI.wAttributes,
					AttrOf(ch));
	if (AttrOf(ch) & A_ALTCHARSET) {
	    if (_nc_wacs) {
		int which = CharOf(ch);
		if (which > 0
		    && which < ACS_LEN
		    && CharOf(_nc_wacs[which]) != 0) {
		    ci[actual].Char.UnicodeChar = CharOf(_nc_wacs[which]);
		} else {
		    ci[actual].Char.UnicodeChar = ' ';
		}
	    }
	}
	++actual;
    }

    loc.X = (SHORT) 0;
    loc.Y = (SHORT) 0;
    siz.X = (SHORT) actual;
    siz.Y = 1;

    rec.Left = (SHORT) x;
    rec.Top = (SHORT) (y + AdjustY());
    rec.Right = (SHORT) (x + limit - 1);
    rec.Bottom = rec.Top;

    return write_screen(WINCONSOLE.hdl, ci, siz, loc, &rec);
}
#define con_write(tcb, y, x, str, n) con_write16(tcb, y, x, str, n)
#else
static BOOL
con_write8(TERMINAL_CONTROL_BLOCK * TCB, int y, int x, chtype *str, int n)
{
    CHAR_INFO *ci = TypeAlloca(CHAR_INFO, n);
    COORD loc, siz;
    SMALL_RECT rec;
    int i;
    chtype ch;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    for (i = 0; i < n; i++) {
	ch = str[i];
	ci[i].Char.AsciiChar = ChCharOf(ch);
	ci[i].Attributes = MapAttr(WINCONSOLE.SBI.wAttributes,
				   ChAttrOf(ch));
	if (ChAttrOf(ch) & A_ALTCHARSET) {
	    if (sp->_acs_map)
		ci[i].Char.AsciiChar =
		ChCharOf(NCURSES_SP_NAME(_nc_acs_char) (sp, ChCharOf(ch)));
	}
    }

    loc.X = (short) 0;
    loc.Y = (short) 0;
    siz.X = (short) n;
    siz.Y = 1;

    rec.Left = (short) x;
    rec.Top = (short) y;
    rec.Right = (short) (x + n - 1);
    rec.Bottom = rec.Top;

    return write_screen(WINCONSOLE.hdl, ci, siz, loc, &rec);
}
#define con_write(tcb, y, x, str, n) con_write8(tcb, y, x, str, n)
#endif

#if EXP_OPTIMIZE
/*
 * Comparing new/current screens, determine the last column-index for a change
 * beginning on the given row,col position.  Unlike a serial terminal, there is
 * no cost for "moving" the "cursor" on the line as we update it.
 */
static int
find_end_of_change(SCREEN *sp, int row, int col)
{
    int result = col;
    struct ldat *curdat = CurScreen(sp)->_line + row;
    struct ldat *newdat = NewScreen(sp)->_line + row;

    while (col <= newdat->lastchar) {
#if USE_WIDEC_SUPPORT
	if (isWidecExt(curdat->text[col]) ||
	    isWidecExt(newdat->text[col])) {
	    result = col;
	} else if (memcmp(&curdat->text[col],
			  &newdat->text[col],
			  sizeof(curdat->text[0]))) {
	    result = col;
	} else {
	    break;
	}
#else
	if (curdat->text[col] != newdat->text[col]) {
	    result = col;
	} else {
	    break;
	}
#endif
	++col;
    }
    return result;
}

/*
 * Given a row,col position at the end of a change-chunk, look for the
 * beginning of the next change-chunk.
 */
static int
find_next_change(SCREEN *sp, int row, int col)
{
    struct ldat *curdat = CurScreen(sp)->_line + row;
    struct ldat *newdat = NewScreen(sp)->_line + row;
    int result = newdat->lastchar + 1;

    while (++col <= newdat->lastchar) {
#if USE_WIDEC_SUPPORT
	if (isWidecExt(curdat->text[col]) !=
	    isWidecExt(newdat->text[col])) {
	    result = col;
	    break;
	} else if (memcmp(&curdat->text[col],
			  &newdat->text[col],
			  sizeof(curdat->text[0]))) {
	    result = col;
	    break;
	}
#else
	if (curdat->text[col] != newdat->text[col]) {
	    result = col;
	    break;
	}
#endif
    }
    return result;
}

#define EndChange(first) \
	find_end_of_change(sp, y, first)
#define NextChange(last)                        \
	find_next_change(sp, y, last)

#endif /* EXP_OPTIMIZE */

#define MARK_NOCHANGE(win,row)                 \
    win->_line[row].firstchar = _NOCHANGE;     \
    win->_line[row].lastchar  = _NOCHANGE

static bool
restore_original_screen(void)
{
    COORD bufferCoord;
    bool result = FALSE;
    SMALL_RECT save_region = WINCONSOLE.save_region;

    T(("... restoring %s", WINCONSOLE.window_only ?
       "window" : "entire buffer"));

    bufferCoord.X = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Left : 0);
    bufferCoord.Y = (SHORT) (WINCONSOLE.window_only ?
			     WINCONSOLE.SBI.srWindow.Top : 0);

    if (write_screen(WINCONSOLE.hdl,
		     WINCONSOLE.save_screen,
		     WINCONSOLE.save_size,
		     bufferCoord,
		     &save_region)) {
	result = TRUE;
	mvcur(-1, -1, LINES - 2, 0);
	T(("... restore original screen contents ok %dx%d (%d,%d - %d,%d)",
	   WINCONSOLE.save_size.Y,
	   WINCONSOLE.save_size.X,
	   save_region.Top,
	   save_region.Left,
	   save_region.Bottom,
	   save_region.Right));
    } else {
	T(("... restore original screen contents err"));
    }
    return result;
}

static const char *
wcon_name(TERMINAL_CONTROL_BLOCK * TCB)
{
    (void) TCB;
    return "win32console";
}

static int
wcon_doupdate(TERMINAL_CONTROL_BLOCK * TCB)
{
    int result = ERR;
    int y, nonempty, n, x0, x1, Width, Height;
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_doupdate(%p)"), TCB));
    if (validateConsoleHandle()) {
	SetSP();

	Width = screen_columns(sp);
	Height = screen_lines(sp);
	nonempty = min(Height, NewScreen(sp)->_maxy + 1);

	T(("... %dx%d clear cur:%d new:%d",
	   Height, Width,
	   CurScreen(sp)->_clear,
	   NewScreen(sp)->_clear));

	if (SP_PARM->_endwin == ewSuspend) {

	    T(("coming back from shell mode"));
	    NCURSES_SP_NAME(reset_prog_mode) (NCURSES_SP_ARG);

	    NCURSES_SP_NAME(_nc_mvcur_resume) (NCURSES_SP_ARG);
	    NCURSES_SP_NAME(_nc_screen_resume) (NCURSES_SP_ARG);
	    SP_PARM->_mouse_resume(SP_PARM);

	    SP_PARM->_endwin = ewRunning;
	}

	if ((CurScreen(sp)->_clear || NewScreen(sp)->_clear)) {
	    int x;
#if USE_WIDEC_SUPPORT
	    cchar_t *empty = TypeAlloca(cchar_t, Width);
	    wchar_t blank[2] =
	    {
		L' ', L'\0'
	    };

	    for (x = 0; x < Width; x++)
		setcchar(&empty[x], blank, 0, 0, 0);
#else
	    chtype *empty = TypeAlloca(chtype, Width);

	    for (x = 0; x < Width; x++)
		empty[x] = ' ';
#endif

	    for (y = 0; y < nonempty; y++) {
		con_write(TCB, y, 0, empty, Width);
		memcpy(empty,
		       CurScreen(sp)->_line[y].text,
		       (size_t) Width * sizeof(empty[0]));
	    }
	    CurScreen(sp)->_clear = FALSE;
	    NewScreen(sp)->_clear = FALSE;
	    touchwin(NewScreen(sp));
	    T(("... cleared %dx%d lines @%d of screen", nonempty, Width,
	       AdjustY()));
	}

	for (y = 0; y < nonempty; y++) {
	    x0 = NewScreen(sp)->_line[y].firstchar;
	    if (x0 != _NOCHANGE) {
#if EXP_OPTIMIZE
		int x2;
		int limit = NewScreen(sp)->_line[y].lastchar;
		while ((x1 = EndChange(x0)) <= limit) {
		    while ((x2 = NextChange(x1)) <=
			   limit && x2 <= (x1 + 2)) {
			x1 = x2;
		    }
		    n = x1 - x0 + 1;
		    memcpy(&CurScreen(sp)->_line[y].text[x0],
			   &NewScreen(sp)->_line[y].text[x0],
			   n * sizeof(CurScreen(sp)->_line[y].text[x0]));
		    con_write(TCB,
			      y,
			      x0,
			      &CurScreen(sp)->_line[y].text[x0], n);
		    x0 = NextChange(x1);
		}

		/* mark line changed successfully */
		if (y <= NewScreen(sp)->_maxy) {
		    MARK_NOCHANGE(NewScreen(sp), y);
		}
		if (y <= CurScreen(sp)->_maxy) {
		    MARK_NOCHANGE(CurScreen(sp), y);
		}
#else
		x1 = NewScreen(sp)->_line[y].lastchar;
		n = x1 - x0 + 1;
		if (n > 0) {
		    memcpy(&CurScreen(sp)->_line[y].text[x0],
			   &NewScreen(sp)->_line[y].text[x0],
			   (size_t) n *
			   sizeof(CurScreen(sp)->_line[y].text[x0]));
		    con_write(TCB,
			      y,
			      x0,
			      &CurScreen(sp)->_line[y].text[x0], n);

		    /* mark line changed successfully */
		    if (y <= NewScreen(sp)->_maxy) {
			MARK_NOCHANGE(NewScreen(sp), y);
		    }
		    if (y <= CurScreen(sp)->_maxy) {
			MARK_NOCHANGE(CurScreen(sp), y);
		    }
		}
#endif
	    }
	}

	/* put everything back in sync */
	for (y = nonempty; y <= NewScreen(sp)->_maxy; y++) {
	    MARK_NOCHANGE(NewScreen(sp), y);
	}
	for (y = nonempty; y <= CurScreen(sp)->_maxy; y++) {
	    MARK_NOCHANGE(CurScreen(sp), y);
	}

	if (!NewScreen(sp)->_leaveok) {
	    CurScreen(sp)->_curx = NewScreen(sp)->_curx;
	    CurScreen(sp)->_cury = NewScreen(sp)->_cury;

	    TCB->drv->td_hwcur(TCB,
			       0,
			       0,
			       CurScreen(sp)->_cury,
			       CurScreen(sp)->_curx);
	}
	_nc_console_selectActiveHandle();
	result = OK;
    }
    returnCode(result);
}

static bool
wcon_CanHandle(TERMINAL_CONTROL_BLOCK * TCB,
	       const char *tname,
	       int *errret GCC_UNUSED)
{
    bool code = FALSE;

    T((T_CALLED("win32con::wcon_CanHandle(%p)"), TCB));

    assert((TCB != 0) && (tname != 0));

    TCB->magic = WINMAGIC;

    if (tname == 0 || *tname == 0) {
	if (!_nc_console_vt_supported())
	    code = TRUE;
    } else if (tname != 0 && *tname == '#') {
	/*
	 * Use "#" (a character which cannot begin a terminal's name) to
	 * select specific driver from the table.
	 *
	 * In principle, we could have more than one non-terminfo driver,
	 * e.g., "win32gui".
	 */
	size_t n = strlen(tname + 1);
	if (n != 0
	    && ((strncmp(tname + 1, "win32console", n) == 0)
		|| (strncmp(tname + 1, "win32con", n) == 0))) {
	    code = TRUE;
	}
    } else if (tname != 0 && stricmp(tname, "unknown") == 0) {
	code = TRUE;
    }

    /*
     * This is intentional, to avoid unnecessary breakage of applications
     * using <term.h> symbols.
     */
    if (code && (TerminalType(&TCB->term).Booleans == 0)) {
	_nc_init_termtype(&TerminalType(&TCB->term));
#if NCURSES_EXT_NUMBERS
	_nc_export_termtype2(&TCB->term.type, &TerminalType(&TCB->term));
#endif
    }

    if (!code) {
	if (_nc_console_test(0)) {
	    T(("isTermInfoConsole=TRUE"));
	    WINCONSOLE.isTermInfoConsole = TRUE;
	}
    }
    returnBool(code);
}

static int
wcon_dobeepflash(TERMINAL_CONTROL_BLOCK * TCB,
		 int beepFlag)
{
    SCREEN *sp;
    int res = ERR;

    int high = (WINCONSOLE.SBI.srWindow.Bottom -
		WINCONSOLE.SBI.srWindow.Top + 1);
    int wide = (WINCONSOLE.SBI.srWindow.Right -
		WINCONSOLE.SBI.srWindow.Left + 1);
    int max_cells = (high * wide);
    int i;

    CHAR_INFO *this_screen = TypeAlloca(CHAR_INFO, max_cells);
    CHAR_INFO *that_screen = TypeAlloca(CHAR_INFO, max_cells);
    COORD this_size;
    SMALL_RECT this_region;
    COORD bufferCoord;

    if (validateConsoleHandle()) {
	SetSP();
	this_region.Top = WINCONSOLE.SBI.srWindow.Top;
	this_region.Left = WINCONSOLE.SBI.srWindow.Left;
	this_region.Bottom = WINCONSOLE.SBI.srWindow.Bottom;
	this_region.Right = WINCONSOLE.SBI.srWindow.Right;

	this_size.X = (SHORT) wide;
	this_size.Y = (SHORT) high;

	bufferCoord.X = this_region.Left;
	bufferCoord.Y = this_region.Top;

	if (!beepFlag &&
	    read_screen(WINCONSOLE.hdl,
			this_screen,
			this_size,
			bufferCoord,
			&this_region)) {

	    memcpy(that_screen,
		   this_screen,
		   sizeof(CHAR_INFO) * (size_t) max_cells);

	    for (i = 0; i < max_cells; i++) {
		that_screen[i].Attributes =
		    RevAttr(that_screen[i].Attributes);
	    }

	    write_screen(WINCONSOLE.hdl, that_screen, this_size,
			 bufferCoord, &this_region);
	    Sleep(200);
	    write_screen(WINCONSOLE.hdl, this_screen, this_size,
			 bufferCoord, &this_region);

	} else {
	    MessageBeep(MB_ICONWARNING);	/* MB_OK might be better */
	}
	res = OK;
    }
    return res;
}

static int
wcon_print(TERMINAL_CONTROL_BLOCK * TCB,
	   char *data GCC_UNUSED,
	   int len GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();

    return ERR;
}

static int
wcon_defaultcolors(TERMINAL_CONTROL_BLOCK * TCB,
		   int fg GCC_UNUSED,
		   int bg GCC_UNUSED)
{
    SCREEN *sp;
    int code = ERR;

    AssertTCB();
    SetSP();

    return (code);
}

static void
wcon_setcolor(TERMINAL_CONTROL_BLOCK * TCB,
	      int fore,
	      int color,
	      int (*outc) (SCREEN *, int) GCC_UNUSED)
{
    (void) TCB;
    if (validateConsoleHandle()) {
	WORD a = _nc_console_MapColor(fore, color);
	a |= (WORD) ((WINCONSOLE.SBI.wAttributes) & (fore ? 0xfff8 : 0xff8f));
	SetConsoleTextAttribute(WINCONSOLE.hdl, a);
	_nc_console_get_SBI();
    }
}

static bool
wcon_rescol(TERMINAL_CONTROL_BLOCK * TCB)
{
    bool res = FALSE;

    (void) TCB;
    if (validateConsoleHandle()) {
	WORD a = FOREGROUND_BLUE | FOREGROUND_RED | FOREGROUND_GREEN;
	SetConsoleTextAttribute(WINCONSOLE.hdl, a);
	_nc_console_get_SBI();
	res = TRUE;
    }
    return res;
}

static bool
wcon_rescolors(TERMINAL_CONTROL_BLOCK * TCB)
{
    int result = FALSE;
    SCREEN *sp;

    AssertTCB();
    SetSP();

    return result;
}

static int
wcon_size(TERMINAL_CONTROL_BLOCK * TCB, int *Lines, int *Cols)
{
    int result = ERR;

    T((T_CALLED("win32con::wcon_size(%p)"), TCB));

    if (validateConsoleHandle() &&
	(Lines != NULL) && (Cols != NULL)) {
	_nc_console_size(Lines, Cols);
	result = OK;
    }
    returnCode(result);
}

static int
wcon_setsize(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED,
	     int l GCC_UNUSED,
	     int c GCC_UNUSED)
{
    AssertTCB();
    return ERR;
}

static int
wcon_sgmode(TERMINAL_CONTROL_BLOCK * TCB, int setFlag, TTY * buf)
{
    int result = ERR;

    T((T_CALLED("win32con::wcon_sgmode(TCB=(%p),setFlag=%d,TTY=(%p)"),
       TCB, setFlag, buf));
    if (buf != NULL && validateConsoleHandle()) {

	if (setFlag) {
	    _nc_console_setmode(WINCONSOLE.hdl, buf);
	    TCB->term.Nttyb = *buf;
	} else {
	    _nc_console_getmode(WINCONSOLE.hdl, &(TCB->term.Nttyb));
	    *buf = TCB->term.Nttyb;
	}
	result = OK;
    }
    returnCode(result);
}

#define MIN_WIDE 80
#define MIN_HIGH 24

static int
wcon_mode(TERMINAL_CONTROL_BLOCK * TCB, int progFlag, int defFlag)
{
    SCREEN *sp;
    TERMINAL *_term = (TERMINAL *) TCB;
    int code = ERR;

    if (validateConsoleHandle()) {
	sp = TCB->csp;

	T((T_CALLED("win32con::wcon_mode(%p, progFlag=%d, defFlag=%d)"),
	   TCB, progFlag, defFlag));

	WINCONSOLE.progMode = progFlag;
	WINCONSOLE.lastOut = progFlag ? WINCONSOLE.hdl : WINCONSOLE.out;
	SetConsoleActiveScreenBuffer(WINCONSOLE.lastOut);

	if (progFlag) /* prog mode */  {
	    if (defFlag) {
		if ((wcon_sgmode(TCB, FALSE, &(_term->Nttyb)) == OK)) {
		    code = OK;
		}
	    } else {
		/* reset_prog_mode */
		if (wcon_sgmode(TCB, TRUE, &(_term->Nttyb)) == OK) {
		    if (sp) {
			if (sp->_keypad_on)
			    _nc_keypad(sp, TRUE);
		    }
		    if (!WINCONSOLE.buffered) {
			_nc_console_set_scrollback(FALSE, &WINCONSOLE.SBI);
		    }
		    code = OK;
		}
	    }
	    T(("... buffered:%d, clear:%d",
	       WINCONSOLE.buffered, CurScreen(sp)->_clear));
	} else {		/* shell mode */
	    if (defFlag) {
		/* def_shell_mode */
		if (wcon_sgmode(TCB, FALSE, &(_term->Ottyb)) == OK) {
		    code = OK;
		}
	    } else {
		/* reset_shell_mode */
		if (sp) {
		    _nc_keypad(sp, FALSE);
		    NCURSES_SP_NAME(_nc_flush) (sp);
		}
		code = wcon_sgmode(TCB, TRUE, &(_term->Ottyb));
		if (!WINCONSOLE.buffered) {
		    _nc_console_set_scrollback(TRUE, &WINCONSOLE.save_SBI);
		    if (!restore_original_screen())
			code = ERR;
		}
		SetConsoleCursorInfo(WINCONSOLE.hdl, &WINCONSOLE.save_CI);
	    }
	}

    }
    returnCode(code);
}

static void
wcon_screen_init(SCREEN *sp GCC_UNUSED)
{
}

static void
wcon_wrap(SCREEN *sp GCC_UNUSED)
{
}

static void
wcon_release(TERMINAL_CONTROL_BLOCK * TCB)
{
    T((T_CALLED("win32con::wcon_release(%p)"), TCB));

    AssertTCB();
    if (TCB->prop)
	free(TCB->prop);

    returnVoid;
}

static void
wcon_init(TERMINAL_CONTROL_BLOCK * TCB)
{
    T((T_CALLED("win32con::wcon_init(%p)"), TCB));

    AssertTCB();

    if (!(console_initialized = _nc_console_checkinit(TRUE, FALSE))) {
	returnVoid;
    }

    if (TCB) {
	TCB->info.initcolor = TRUE;
	TCB->info.canchange = FALSE;
	TCB->info.hascolor = TRUE;
	TCB->info.caninit = TRUE;

	TCB->info.maxpairs = CON_NUMPAIRS;
	TCB->info.maxcolors = 8;
	TCB->info.numlabels = 0;
	TCB->info.labelwidth = 0;
	TCB->info.labelheight = 0;
	TCB->info.nocolorvideo = 1;
	TCB->info.tabsize = 8;

	TCB->info.numbuttons = WINCONSOLE.numButtons;
	TCB->info.defaultPalette = _nc_cga_palette;

    }
    returnVoid;
}

static void
wcon_initpair(TERMINAL_CONTROL_BLOCK * TCB,
	      int pair,
	      int f,
	      int b)
{
    SCREEN *sp;

    if (validateConsoleHandle()) {
	SetSP();

	if ((pair > 0) && (pair < CON_NUMPAIRS) && (f >= 0) && (f < 8)
	    && (b >= 0) && (b < 8)) {
	    WINCONSOLE.pairs[pair] =
		_nc_console_MapColor(true, f) |
		_nc_console_MapColor(false, b);
	}
    }
}

static void
wcon_initcolor(TERMINAL_CONTROL_BLOCK * TCB,
	       int color GCC_UNUSED,
	       int r GCC_UNUSED,
	       int g GCC_UNUSED,
	       int b GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_do_color(TERMINAL_CONTROL_BLOCK * TCB,
	      int old_pair GCC_UNUSED,
	      int pair GCC_UNUSED,
	      int reverse GCC_UNUSED,
	      int (*outc) (SCREEN *, int) GCC_UNUSED
)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_initmouse(TERMINAL_CONTROL_BLOCK * TCB)
{
    SCREEN *sp;

    if (validateConsoleHandle()) {
	SetSP();

	sp->_mouse_type = M_TERM_DRIVER;
    }
}

static int
wcon_testmouse(TERMINAL_CONTROL_BLOCK * TCB,
	       int delay
	       EVENTLIST_2nd(_nc_eventlist * evl))
{
    int rc = 0;
    SCREEN *sp;

    if (validateConsoleHandle()) {
	SetSP();

	if (sp->_drv_mouse_head < sp->_drv_mouse_tail) {
	    rc = TW_MOUSE;
	} else {
	    rc = TCBOf(sp)->drv->td_twait(TCBOf(sp),
					  TWAIT_MASK,
					  delay,
					  (int *) 0
					  EVENTLIST_2nd(evl));
	}
    }

    return rc;
}

static int
wcon_mvcur(TERMINAL_CONTROL_BLOCK * TCB,
	   int yold GCC_UNUSED, int xold GCC_UNUSED,
	   int y, int x)
{
    int ret = ERR;

    (void) TCB;
    if (validateConsoleHandle()) {
	COORD loc;
	loc.X = (short) x;
	loc.Y = (short) (y + AdjustY());
	SetConsoleCursorPosition(WINCONSOLE.hdl, loc);
	ret = OK;
    }
    return ret;
}

static void
wcon_hwlabel(TERMINAL_CONTROL_BLOCK * TCB,
	     int labnum GCC_UNUSED,
	     char *text GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_hwlabelOnOff(TERMINAL_CONTROL_BLOCK * TCB,
		  int OnFlag GCC_UNUSED)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static chtype
wcon_conattr(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED)
{
    chtype res = A_NORMAL;
    res |= (A_BOLD | A_DIM | A_REVERSE | A_STANDOUT | A_COLOR);
    return res;
}

static void
wcon_setfilter(TERMINAL_CONTROL_BLOCK * TCB)
{
    SCREEN *sp;

    AssertTCB();
    SetSP();
}

static void
wcon_initacs(TERMINAL_CONTROL_BLOCK * TCB,
	     chtype *real_map GCC_UNUSED,
	     chtype *fake_map GCC_UNUSED)
{
#define DATA(a,b) { a, b }
    static struct {
	int acs_code;
	int use_code;
    } table[] = {
	DATA('a', 0xb1),	/* ACS_CKBOARD  */
	    DATA('f', 0xf8),	/* ACS_DEGREE   */
	    DATA('g', 0xf1),	/* ACS_PLMINUS  */
	    DATA('j', 0xd9),	/* ACS_LRCORNER */
	    DATA('l', 0xda),	/* ACS_ULCORNER */
	    DATA('k', 0xbf),	/* ACS_URCORNER */
	    DATA('m', 0xc0),	/* ACS_LLCORNER */
	    DATA('n', 0xc5),	/* ACS_PLUS     */
	    DATA('q', 0xc4),	/* ACS_HLINE    */
	    DATA('t', 0xc3),	/* ACS_LTEE     */
	    DATA('u', 0xb4),	/* ACS_RTEE     */
	    DATA('v', 0xc1),	/* ACS_BTEE     */
	    DATA('w', 0xc2),	/* ACS_TTEE     */
	    DATA('x', 0xb3),	/* ACS_VLINE    */
	    DATA('y', 0xf3),	/* ACS_LEQUAL   */
	    DATA('z', 0xf2),	/* ACS_GEQUAL   */
	    DATA('0', 0xdb),	/* ACS_BLOCK    */
	    DATA('{', 0xe3),	/* ACS_PI       */
	    DATA('}', 0x9c),	/* ACS_STERLING */
	    DATA(',', 0xae),	/* ACS_LARROW   */
	    DATA('+', 0xaf),	/* ACS_RARROW   */
	    DATA('~', 0xf9),	/* ACS_BULLET   */
    };
#undef DATA
    unsigned n;

    SCREEN *sp;
    if (validateConsoleHandle()) {
	SetSP();

	for (n = 0; n < SIZEOF(table); ++n) {
	    real_map[table[n].acs_code] =
		(chtype) table[n].use_code | A_ALTCHARSET;
	    if (sp != 0)
		sp->_screen_acs_map[table[n].acs_code] = TRUE;
	}
    }
}

static int
wcon_twait(TERMINAL_CONTROL_BLOCK * TCB,
	   int mode,
	   int milliseconds,
	   int *timeleft
	   EVENTLIST_2nd(_nc_eventlist * evl))
{
    SCREEN *sp;
    int code = 0;

    if (validateConsoleHandle()) {
	SetSP();

	code = _nc_console_twait(sp,
				 WINCONSOLE.inp,
				 mode,
				 milliseconds,
				 timeleft EVENTLIST_2nd(evl));
    }
    return code;
}

static int
wcon_read(TERMINAL_CONTROL_BLOCK * TCB, int *buf)
{
    SCREEN *sp;
    int n = -1;

    T((T_CALLED("win32con::wcon_read(%p)"), TCB));

    assert(buf);
    if (validateConsoleHandle()) {
	SetSP();

	n = _nc_console_read(sp, WINCONSOLE.inp, buf);
    }
    returnCode(n);
}

static int
wcon_nap(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int ms)
{
    T((T_CALLED("win32con::wcon_nap(%p, %d)"), TCB, ms));
    Sleep((DWORD) ms);
    returnCode(OK);
}

static int
wcon_cursorSet(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int mode)
{
    int res = -1;

    T((T_CALLED("win32con:wcon_cursorSet(%d)"), mode));
    if (validateConsoleHandle()) {
	CONSOLE_CURSOR_INFO this_CI = WINCONSOLE.save_CI;
	switch (mode) {
	case 0:
	    this_CI.bVisible = FALSE;
	    break;
	case 1:
	    break;
	case 2:
	    this_CI.dwSize = 100;
	    break;
	}
	SetConsoleCursorInfo(WINCONSOLE.hdl, &this_CI);
    }
    returnCode(res);
}

static bool
wcon_kyExist(TERMINAL_CONTROL_BLOCK * TCB GCC_UNUSED, int keycode)
{
    bool found = FALSE;

    T((T_CALLED("win32con::wcon_kyExist(%d)"), keycode));
    found = _nc_console_keyExist(keycode);
    returnBool(found);
}

static int
wcon_kpad(TERMINAL_CONTROL_BLOCK * TCB, int flag GCC_UNUSED)
{
    SCREEN *sp;
    int code = ERR;

    T((T_CALLED("win32con::wcon_kpad(%p, %d)"), TCB, flag));

    if (validateConsoleHandle()) {
	SetSP();

	if (sp) {
	    code = OK;
	}
    }
    returnCode(code);
}

static int
wcon_keyok(TERMINAL_CONTROL_BLOCK * TCB,
	   int keycode,
	   int flag)
{
    int code = ERR;
    SCREEN *sp;

    T((T_CALLED("win32con::wcon_keyok(%p, %d, %d)"), TCB, keycode, flag));

    if (validateConsoleHandle()) {
	SetSP();
	if (sp) {
	    code = _nc_console_keyok(keycode, flag);
	}
    }
    returnCode(code);
}

NCURSES_EXPORT_VAR (TERM_DRIVER) _nc_WIN_DRIVER = {
    FALSE,
	wcon_name,		/* Name          */
	wcon_CanHandle,		/* CanHandle     */
	wcon_init,		/* init          */
	wcon_release,		/* release       */
	wcon_size,		/* size          */
	wcon_sgmode,		/* sgmode        */
	wcon_conattr,		/* conattr       */
	wcon_mvcur,		/* hwcur         */
	wcon_mode,		/* mode          */
	wcon_rescol,		/* rescol        */
	wcon_rescolors,		/* rescolors     */
	wcon_setcolor,		/* color         */
	wcon_dobeepflash,	/* DoBeepFlash   */
	wcon_initpair,		/* initpair      */
	wcon_initcolor,		/* initcolor     */
	wcon_do_color,		/* docolor       */
	wcon_initmouse,		/* initmouse     */
	wcon_testmouse,		/* testmouse     */
	wcon_setfilter,		/* setfilter     */
	wcon_hwlabel,		/* hwlabel       */
	wcon_hwlabelOnOff,	/* hwlabelOnOff  */
	wcon_doupdate,		/* update        */
	wcon_defaultcolors,	/* defaultcolors */
	wcon_print,		/* print         */
	wcon_size,		/* getsize       */
	wcon_setsize,		/* setsize       */
	wcon_initacs,		/* initacs       */
	wcon_screen_init,	/* scinit        */
	wcon_wrap,		/* scexit        */
	wcon_twait,		/* twait         */
	wcon_read,		/* read          */
	wcon_nap,		/* nap           */
	wcon_kpad,		/* kpad          */
	wcon_keyok,		/* kyOk          */
	wcon_kyExist,		/* kyExist       */
	wcon_cursorSet		/* cursorSet     */
};

#endif /* _NC_WINDOWS */
