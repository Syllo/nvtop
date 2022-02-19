/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2009-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: test_add_wchstr.c,v 1.29 2021/05/08 20:04:10 tom Exp $
 *
 * Demonstrate the waddwchstr() and wadd_wch functions.
 * Thomas Dickey - 2009/9/12
 *
 * Note: to provide inputs for *add_wch(), we use setcchar().  A quirk of the
 * X/Open definition for that function is that the string contains no
 * characters with negative width.  Any control character (such as tab) falls
 * into that category.  So it follows that *add_wch() cannot render a tab
 * character because there is no legal way to construct a cchar_t containing
 * one.  X/Open does not document this, and it would be logical to assume that
 * *addwchstr() has the same limitation, but it uses a wchar_t string directly,
 * and does not document how tabs are handled.
 */

#include <test.priv.h>

#if USE_WIDEC_SUPPORT

#define WIDE_LINEDATA
#include <linedata.h>

#undef AddCh
#undef MvAddCh
#undef MvAddStr
#undef MvWAddCh
#undef MvWAddChStr
#undef MvWAddStr
#undef WAddCh

/*
 * redefinitions to simplify comparison between test_*str programs
 */
#define AddNStr    add_wchnstr
#define AddStr     add_wchstr
#define MvAddNStr  (void) mvadd_wchnstr
#define MvAddStr   (void) mvadd_wchstr
#define MvWAddNStr (void) mvwadd_wchnstr
#define MvWAddStr  (void) mvwadd_wchstr
#define MvWAddChStr(w,y,x,s)	(void) mvwadd_wchstr((w),(y),(x),(s))
#define WAddNStr   wadd_wchnstr
#define WAddStr    wadd_wchstr

#define MY_TABSIZE 8

typedef enum {
    oDefault = 0,
    oMove = 1,
    oWindow = 2,
    oMoveWindow = 3
} Options;

static bool m_opt = FALSE;
static bool pass_ctls = FALSE;
static bool w_opt = FALSE;
static int n_opt = -1;

static cchar_t *temp_buffer;
static size_t temp_length;

#define TempBuffer(source_len, source_cast) \
    if (source != 0) { \
	const char *temp; \
	size_t need = source_len + 1; \
	wchar_t have[2]; \
	int n = 0; \
 \
	if (need > temp_length) { \
	    temp_length = need * 2; \
	    temp_buffer = typeRealloc(cchar_t, temp_length, temp_buffer); \
	    if (!temp_buffer) \
		failed("TempBuffer"); \
	} \
	have[0] = 0; \
	have[1] = 0; \
	do { \
	    have[0] = source_cast; \
	    if (!pass_ctls \
	     && have[0] != 0 \
	     && have[0] < 256 \
	     && (temp = unctrl((chtype) have[0])) != 0 \
	     && strlen(temp) > 1) { \
		while (*temp != '\0') { \
		    have[0] = (wchar_t) *temp++; \
		    setcchar(&temp_buffer[n++], have, A_NORMAL, 0, NULL); \
		} \
	    } else { \
		setcchar(&temp_buffer[n++], have, A_NORMAL, 0, NULL); \
	    } \
	} while (have[0] != 0); \
    } else if (temp_buffer != 0) { \
	free(temp_buffer); \
	temp_buffer = 0; \
	temp_length = 0; \
    } \
    return temp_buffer;

static size_t
ChWLen(const wchar_t *source)
{
    size_t result = wcslen(source);

    if (!pass_ctls) {
	size_t adjust = 0;
	size_t n;

	for (n = 0; source[n] != 0; ++n) {
	    const char *s;

	    if ((source[n] < 256) && (s = unctrl((chtype) source[n])) != 0) {
		adjust += (strlen(s) - 1);
	    }
	}
	result += adjust;
    }
    return result;
}

static cchar_t *
ChStr(const char *source)
{
    TempBuffer(strlen(source), UChar(*source++));
}

static cchar_t *
ChWStr(const wchar_t *source)
{
    TempBuffer(ChWLen(source), *source++);
}

static void
legend(WINDOW *win, int level, Options state, wchar_t *buffer, int length)
{
    const char *showstate;

    switch (state) {
    default:
    case oDefault:
	showstate = "";
	break;
    case oMove:
	showstate = " (mvXXX)";
	break;
    case oWindow:
	showstate = " (winXXX)";
	break;
    case oMoveWindow:
	showstate = " (mvwinXXX)";
	break;
    }

    wmove(win, 0, 0);
    wprintw(win,
	    "The Strings/Chars displays should match.  Enter any characters, except:\n");
    wprintw(win,
	    "down-arrow or ^N to repeat on next line, ^W for inner window, ESC to exit.\n");
    wclrtoeol(win);
    wprintw(win, "Level %d,%s added %d characters <", level,
	    showstate, length);
    waddwstr(win, buffer);
    waddstr(win, ">");
}

static int
ColOf(wchar_t *buffer, int length, int margin)
{
    int n;
    int result;

    for (n = 0, result = margin + 1; n < length; ++n) {
	int ch = buffer[n];
	switch (ch) {
	case '\n':
	    /* actually newline should clear the remainder of the line
	     * and move to the next line - but that seems a little awkward
	     * in this example.
	     */
	case '\r':
	    result = 0;
	    break;
	case '\b':
	    if (result > 0)
		--result;
	    break;
	case '\t':
	    result += (MY_TABSIZE - (result % MY_TABSIZE));
	    break;
	case '\177':
	    result += 2;
	    break;
	default:
	    result += wcwidth((wchar_t) ch);
	    if (ch < 32)
		++result;
	    break;
	}
    }
    return result;
}

static int
ConvertCh(chtype source, cchar_t *target)
{
    wchar_t tmp_wchar[2];

    tmp_wchar[0] = (wchar_t) source;
    tmp_wchar[1] = 0;
    if (setcchar(target, tmp_wchar, A_NORMAL, 0, (void *) 0) == ERR) {
	beep();
	return FALSE;
    }
    return TRUE;
}

static int
MvWAddCh(WINDOW *win, int y, int x, chtype ch)
{
    int code;
    cchar_t tmp_cchar;

    if (ConvertCh(ch, &tmp_cchar)) {
	code = mvwadd_wch(win, y, x, &tmp_cchar);
    } else {
	code = mvwaddch(win, y, x, ch);
    }
    return code;
}

static int
MvAddCh(int y, int x, chtype ch)
{
    int code;
    cchar_t tmp_cchar;

    if (ConvertCh(ch, &tmp_cchar)) {
	code = mvadd_wch(y, x, &tmp_cchar);
    } else {
	code = mvaddch(y, x, ch);
    }
    return code;
}

static int
WAddCh(WINDOW *win, chtype ch)
{
    int code;
    cchar_t tmp_cchar;

    if (ConvertCh(ch, &tmp_cchar)) {
	code = wadd_wch(win, &tmp_cchar);
    } else {
	code = waddch(win, ch);
    }
    return code;
}

static int
AddCh(chtype ch)
{
    int code;
    cchar_t tmp_cchar;

    if (ConvertCh(ch, &tmp_cchar)) {
	code = add_wch(&tmp_cchar);
    } else {
	code = addch(ch);
    }
    return code;
}

#define LEN(n) ((length - (n) > n_opt) ? n_opt : (length - (n)))
static void
recursive_test(int level)
{
    static bool first = TRUE;

    int ch;
    int limit;
    int row = 1;
    int col;
    int row2, col2;
    int length;
    wchar_t buffer[BUFSIZ];
    WINDOW *look = 0;
    WINDOW *work = 0;
    WINDOW *show = 0;
    int margin = (2 * MY_TABSIZE) - 1;
    Options option = (Options) ((unsigned) (m_opt
					    ? oMove
					    : oDefault)
				| (unsigned) ((w_opt || (level > 0))
					      ? oWindow
					      : oDefault));

    if (first) {
	static char cmd[80];
	setlocale(LC_ALL, "");

	_nc_STRCPY(cmd, "TABSIZE=8", sizeof(cmd));
	putenv(cmd);

	initscr();
	(void) cbreak();	/* take input chars one at a time, no wait for \n */
	(void) noecho();	/* don't echo input */
	keypad(stdscr, TRUE);

	/*
	 * Show the characters added in color, to distinguish from those that
	 * are shifted.
	 */
	if (has_colors()) {
	    start_color();
	    init_pair(1, COLOR_WHITE, COLOR_BLUE);
	}
    }

    limit = LINES - 5;
    if (level > 0) {
	look = newwin(limit, COLS - (2 * (level - 1)), 0, level - 1);
	work = newwin(limit - 2, COLS - (2 * level), 1, level);
	show = newwin(4, COLS, limit + 1, 0);
	box(look, 0, 0);
	wnoutrefresh(look);
	limit -= 2;
    } else {
	work = stdscr;
	show = derwin(stdscr, 4, COLS, limit + 1, 0);
    }
    keypad(work, TRUE);

    for (col = margin + 1; col < COLS; col += MY_TABSIZE)
	MvWVLine(work, row, col, '.', limit - 2);

    MvWVLine(work, row, margin, ACS_VLINE, limit - 2);
    MvWVLine(work, row, margin + 1, ACS_VLINE, limit - 2);
    limit /= 2;

    MvWAddChStr(work, 1, 2, ChStr("String"));
    MvWAddChStr(work, limit + 1, 2, ChStr("Chars"));
    wnoutrefresh(work);

    buffer[length = 0] = '\0';
    legend(show, level, option, buffer, length);
    wnoutrefresh(show);

    doupdate();

    if (has_colors()) {
	wbkgdset(work, (chtype) (COLOR_PAIR(1) | ' '));
    }

    while ((ch = read_linedata(work)) != ERR && !isQUIT(ch)) {
	wmove(work, row, margin + 1);
	switch (ch) {
	case key_RECUR:
	    recursive_test(level + 1);

	    if (look)
		touchwin(look);
	    touchwin(work);
	    touchwin(show);

	    if (look)
		wnoutrefresh(look);
	    wnoutrefresh(work);
	    wnoutrefresh(show);

	    doupdate();
	    break;
	case key_NEWLINE:
	    if (row < limit) {
		++row;
		/* put the whole string in, all at once */
		col2 = margin + 1;
		switch (option) {
		case oDefault:
		    if (n_opt > 1) {
			for (col = 0; col < length; col += n_opt) {
			    col2 = ColOf(buffer, col, margin);
			    if (move(row, col2) != ERR) {
				AddNStr(ChWStr(buffer + col), LEN(col));
			    }
			}
		    } else {
			if (move(row, col2) != ERR) {
			    AddStr(ChWStr(buffer));
			}
		    }
		    break;
		case oMove:
		    if (n_opt > 1) {
			for (col = 0; col < length; col += n_opt) {
			    col2 = ColOf(buffer, col, margin);
			    MvAddNStr(row, col2, ChWStr(buffer + col), LEN(col));
			}
		    } else {
			MvAddStr(row, col2, ChWStr(buffer));
		    }
		    break;
		case oWindow:
		    if (n_opt > 1) {
			for (col = 0; col < length; col += n_opt) {
			    col2 = ColOf(buffer, col, margin);
			    if (wmove(work, row, col2) != ERR) {
				WAddNStr(work, ChWStr(buffer + col), LEN(col));
			    }
			}
		    } else {
			if (wmove(work, row, col2) != ERR) {
			    WAddStr(work, ChWStr(buffer));
			}
		    }
		    break;
		case oMoveWindow:
		    if (n_opt > 1) {
			for (col = 0; col < length; col += n_opt) {
			    col2 = ColOf(buffer, col, margin);
			    MvWAddNStr(work, row, col2, ChWStr(buffer +
							       col), LEN(col));
			}
		    } else {
			MvWAddStr(work, row, col2, ChWStr(buffer));
		    }
		    break;
		}

		/* do the corresponding single-character add */
		row2 = limit + row;
		for (col = 0; col < length; ++col) {
		    col2 = ColOf(buffer, col, margin);
		    switch (option) {
		    case oDefault:
			if (move(row2, col2) != ERR) {
			    AddCh((chtype) buffer[col]);
			}
			break;
		    case oMove:
			MvAddCh(row2, col2, (chtype) buffer[col]);
			break;
		    case oWindow:
			if (wmove(work, row2, col2) != ERR) {
			    WAddCh(work, (chtype) buffer[col]);
			}
			break;
		    case oMoveWindow:
			MvWAddCh(work, row2, col2, (chtype) buffer[col]);
			break;
		    }
		}
	    } else {
		beep();
	    }
	    break;
	default:
	    buffer[length++] = (wchar_t) ch;
	    buffer[length] = '\0';

	    /* put the string in, one character at a time */
	    col = ColOf(buffer, length - 1, margin);
	    switch (option) {
	    case oDefault:
		if (move(row, col) != ERR) {
		    AddStr(ChWStr(buffer + length - 1));
		}
		break;
	    case oMove:
		MvAddStr(row, col, ChWStr(buffer + length - 1));
		break;
	    case oWindow:
		if (wmove(work, row, col) != ERR) {
		    WAddStr(work, ChWStr(buffer + length - 1));
		}
		break;
	    case oMoveWindow:
		MvWAddStr(work, row, col, ChWStr(buffer + length - 1));
		break;
	    }

	    /* do the corresponding single-character add */
	    switch (option) {
	    case oDefault:
		if (move(limit + row, col) != ERR) {
		    AddCh((chtype) ch);
		}
		break;
	    case oMove:
		MvAddCh(limit + row, col, (chtype) ch);
		break;
	    case oWindow:
		if (wmove(work, limit + row, col) != ERR) {
		    WAddCh(work, (chtype) ch);
		}
		break;
	    case oMoveWindow:
		MvWAddCh(work, limit + row, col, (chtype) ch);
		break;
	    }

	    wnoutrefresh(work);

	    legend(show, level, option, buffer, length);
	    wnoutrefresh(show);

	    doupdate();
	    break;
	}
    }
    delwin(show);
    if (level > 0) {
	delwin(work);
	delwin(look);
    }
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: test_add_wchstr [options]"
	,""
	,"Options:"
	,"  -f FILE read data from given file"
	,"  -n NUM  limit string-adds to NUM bytes on ^N replay"
	,"  -m      perform wmove/move separately from add-functions"
	,"  -p      pass-thru control characters without using unctrl()"
	,"  -w      use window-parameter even when stdscr would be implied"
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

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "f:mn:pw")) != -1) {
	switch (ch) {
	case 'f':
	    init_linedata(optarg);
	    break;
	case 'm':
	    m_opt = TRUE;
	    break;
	case 'n':
	    n_opt = atoi(optarg);
	    if (n_opt == 0)
		n_opt = -1;
	    break;
	case 'p':
	    pass_ctls = TRUE;
	    break;
	case 'w':
	    w_opt = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }
    if (optind < argc)
	usage();

    recursive_test(0);
    endwin();
#if NO_LEAKS
    free(temp_buffer);
#endif
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires the wide-ncurses library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
