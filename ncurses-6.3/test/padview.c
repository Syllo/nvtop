/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 2017 Free Software Foundation, Inc.                            *
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
 * clone of view.c, using pads
 *
 * $Id: padview.c,v 1.18 2021/06/12 23:16:31 tom Exp $
 */

#include <test.priv.h>
#include <widechars.h>
#include <popup_msg.h>

#include <sys/stat.h>
#include <time.h>

#if HAVE_NEWPAD

static GCC_NORETURN void finish(int sig);

#define my_pair 1

static int shift = 0;
static bool try_color = FALSE;

static char *fname;
static int num_lines;

#if USE_WIDEC_SUPPORT
static bool n_option = FALSE;
#endif

static GCC_NORETURN void usage(void);

static void
failed(const char *msg)
{
    endwin();
    fprintf(stderr, "%s\n", msg);
    ExitProgram(EXIT_FAILURE);
}

static void
finish(int sig)
{
    endwin();
    ExitProgram(sig != 0 ? EXIT_FAILURE : EXIT_SUCCESS);
}

static void
show_all(const char *tag, WINDOW *my_pad, int my_row)
{
    int i;
    int digits;
    char temp[BUFSIZ];
    time_t this_time;

    for (digits = 1, i = num_lines; i > 0; i /= 10) {
	++digits;
    }

    wattrset(stdscr, COLOR_PAIR(my_pair));
    clear();

    _nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
		"view %.*s", (int) strlen(tag), tag);
    i = (int) strlen(temp);
    _nc_SPRINTF(temp + i, _nc_SLIMIT(sizeof(temp) - (size_t) i)
		" %.*s", (int) sizeof(temp) - i - 2, fname);
    mvprintw(0, 0, "%.*s", COLS, temp);
    this_time = time((time_t *) 0);
    _nc_STRNCPY(temp, ctime(&this_time), (size_t) 30);
    if ((i = (int) strlen(temp)) != 0) {
	temp[--i] = 0;
	mvprintw(0, COLS - i - 2, "  %s", temp);
    }

    for (i = 1; i < LINES; i++) {
	int actual = my_row + i;
	if (actual > num_lines) {
	    break;
	}
	mvprintw(i, 0, "%*d:", digits, actual);
    }
    wnoutrefresh(stdscr);
    pnoutrefresh(my_pad, my_row, shift, 1, digits + 1, LINES - 1, COLS - 1);
    doupdate();
}

static WINDOW *
read_file(const char *filename)
{
    FILE *fp;
    int pass;
    int k;
    int height, width;
    size_t j;
    size_t len;
    struct stat sb;
    char *my_blob;
    char **my_vec = 0;
    WINDOW *my_pad;

    if (stat(filename, &sb) != 0
	|| (sb.st_mode & S_IFMT) != S_IFREG) {
	failed("input is not a file");
    }

    if (sb.st_size == 0) {
	failed("input is empty");
    }

    if ((fp = fopen(filename, "r")) == 0) {
	failed("cannot open input-file");
    }

    if ((my_blob = malloc((size_t) sb.st_size + 1)) == 0) {
	failed("cannot allocate memory for input-file");
    }

    len = fread(my_blob, sizeof(char), (size_t) sb.st_size, fp);
    fclose(fp);

    if (len > (size_t) sb.st_size)
	len = (size_t) sb.st_size;
    my_blob[len] = '\0';

    for (pass = 0; pass < 2; ++pass) {
	char *base = my_blob;
	k = 0;
	for (j = 0; j < len; ++j) {
	    if (my_blob[j] == '\n') {
		if (pass) {
		    my_vec[k] = base;
		    my_blob[j] = '\0';
		}
		base = my_blob + j + 1;
		++k;
	    }
	}
	if (base != (my_blob + j)) {
	    if (pass)
		my_vec[k] = base;
	    ++k;
	}
	num_lines = k;
	if (pass == 0) {
	    if (((my_vec = typeCalloc(char *, (size_t) k + 2)) == 0)) {
		failed("cannot allocate line-vector #1");
	    }
	} else {
	    if (my_vec[0] == NULL)
		my_vec[0] = my_blob;
	}
    }

#if USE_WIDEC_SUPPORT
    if (!memcmp("\357\273\277", my_blob, 3)) {
	char *s = my_blob + 3;
	char *d = my_blob;
	Trace(("trim BOM"));
	do {
	} while ((*d++ = *s++) != '\0');
    }
#endif

    height = num_lines;
    width = (int) strlen(my_vec[0]);
    for (k = 1; my_vec[k]; ++k) {
	int check = (int) (my_vec[k] - my_vec[k - 1]);
	if (width < check)
	    width = check;
    }
    width = (width + 1) * 5;
    my_pad = newpad(height, width);
    if (my_pad == 0)
	failed("cannot allocate pad workspace");
    if (try_color) {
	wattrset(my_pad, COLOR_PAIR(my_pair));
	wbkgd(my_pad, (chtype) (' ' | COLOR_PAIR(my_pair)));
    }

    /*
     * Use the curses library for rendering, including tab-conversion.
     */
    Trace(("slurp the file"));
    for (k = 0; my_vec[k]; ++k) {
	char *s;
#if USE_WIDEC_SUPPORT
	char *last = my_vec[k] + (int) strlen(my_vec[k]);
	wchar_t wch[2];
	size_t rc;
#ifndef state_unused
	mbstate_t state;
#endif
#endif /* USE_WIDEC_SUPPORT */

	wmove(my_pad, k, 0);
#if USE_WIDEC_SUPPORT
	wch[1] = 0;
	reset_mbytes(state);
#endif
	for (s = my_vec[k]; *s != '\0'; ++s) {
#if USE_WIDEC_SUPPORT
	    if (!n_option) {
		rc = (size_t) check_mbytes(wch[0], s, (size_t) (last - s), state);
		if ((long) rc == -1 || (long) rc == -2) {
		    break;
		}
		s += rc - 1;
		waddwstr(my_pad, wch);
	    } else
#endif
		waddch(my_pad, *s & 0xff);
	}
    }

    free(my_vec);
    free(my_blob);

    return my_pad;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: view [options] file"
	,""
	,"Options:"
	," -c       use color if terminal supports it"
	," -i       ignore INT, QUIT, TERM signals"
#if USE_WIDEC_SUPPORT
	," -n       use waddch (bytes) rather then wadd_wch (wide-chars)"
#endif
	," -s       start in single-step mode, waiting for input"
#ifdef TRACE
	," -t       trace screen updates"
	," -T NUM   specify trace mask"
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
    static const char *help[] =
    {
	"Commands:",
	"  q,^Q,ESC       - quit this program",
	"",
	"  p,<Up>         - scroll the viewport up by one row",
	"  n,<Down>       - scroll the viewport down by one row",
	"  l,<Left>       - scroll the viewport left by one column",
	"  r,<Right>      - scroll the viewport right by one column",
	"  <,>            - scroll the viewport left/right by 8 columns",
	"",
	"  h,<Home>       - scroll the viewport to top of file",
	"  ^F,<PageDn>    - scroll to the next page",
	"  ^B,<PageUp>    - scroll to the previous page",
	"  e,<End>        - scroll the viewport to end of file",
	"",
	"  ^L             - repaint using redrawwin()",
	"",
	"  0 through 9    - enter digits for count",
	"  s              - use entered count for halfdelay() parameter",
	"                 - if no entered count, stop nodelay()",
	"  <space>        - begin nodelay()",
	0
    };

    int i;
    int my_delay = 0;
    WINDOW *my_pad;
    int my_row = 0;
    int value = 0;
    bool done = FALSE;
    bool got_number = FALSE;
    bool ignore_sigs = FALSE;
    bool single_step = FALSE;
    const char *my_label = "Input";

    setlocale(LC_ALL, "");

    while ((i = getopt(argc, argv, "cinstT:")) != -1) {
	switch (i) {
	case 'c':
	    try_color = TRUE;
	    break;
	case 'i':
	    ignore_sigs = TRUE;
	    break;
#if USE_WIDEC_SUPPORT
	case 'n':
	    n_option = TRUE;
	    break;
#endif
	case 's':
	    single_step = TRUE;
	    break;
#ifdef TRACE
	case 'T':
	    {
		char *next = 0;
		int tvalue = (int) strtol(optarg, &next, 0);
		if (tvalue < 0 || (next != 0 && *next != 0))
		    usage();
		curses_trace((unsigned) tvalue);
	    }
	    break;
	case 't':
	    curses_trace(TRACE_CALLS);
	    break;
#endif
	default:
	    usage();
	}
    }
    if (optind + 1 != argc)
	usage();

    InitAndCatch(initscr(), ignore_sigs ? SIG_IGN : finish);
    keypad(stdscr, TRUE);	/* enable keyboard mapping */
    (void) nonl();		/* tell curses not to do NL->CR/NL on output */
    (void) cbreak();		/* take input chars one at a time, no wait for \n */
    (void) noecho();		/* don't echo input */
    if (!single_step)
	nodelay(stdscr, TRUE);
    idlok(stdscr, TRUE);	/* allow use of insert/delete line */

    if (try_color) {
	if (has_colors()) {
	    start_color();
	    init_pair(my_pair, COLOR_WHITE, COLOR_BLUE);
	    bkgd((chtype) (' ' | COLOR_PAIR(my_pair)));
	} else {
	    try_color = FALSE;
	}
    }

    /*
     * Do this after starting color, otherwise the pad's background will be
     * uncolored after the ncurses 6.1.20181208 fixes.
     */
    my_pad = read_file(fname = argv[optind]);

    my_row = 0;
    while (!done) {
	int n, c;

	if (!got_number)
	    show_all(my_label, my_pad, my_row);

	for (;;) {
	    c = getch();
	    if ((c < 127) && isdigit(c)) {
		if (!got_number) {
		    MvPrintw(0, 0, "Count: ");
		    clrtoeol();
		}
		addch(UChar(c));
		value = 10 * value + (c - '0');
		got_number = TRUE;
	    } else
		break;
	}
	if (got_number && value) {
	    n = value;
	} else {
	    n = 1;
	}

	if (c != ERR)
	    my_label = keyname(c);
	switch (c) {
	case KEY_DOWN:
	case 'n':
	    for (i = 0; i < n; i++)
		if (my_row < (num_lines - LINES + 1))
		    my_row++;
		else
		    break;
	    break;

	case KEY_UP:
	case 'p':
	    for (i = 0; i < n; i++)
		if (my_row > 0)
		    my_row--;
		else
		    break;
	    break;

	case 'h':
	    /* FALLTHRU */
	case KEY_HOME:
	    my_row = 0;
	    break;

	case '<':
	    if ((shift -= 8) < 0)
		shift = 0;
	    break;
	case '>':
	    shift += 8;
	    break;

	case 'e':
	    /* FALLTHRU */
	case KEY_END:
	    if (num_lines > LINES)
		my_row = (num_lines - LINES + 1);
	    else
		my_row = (num_lines - 2);
	    break;

	case CTRL('F'):
	    /* FALLTHRU */
	case KEY_NPAGE:
	    for (i = 0; i < n; i++) {
		if (my_row < (num_lines - 5))
		    my_row += (LINES - 1);
		else
		    my_row = (num_lines - 2);
	    }
	    break;

	case CTRL('B'):
	    /* FALLTHRU */
	case KEY_PPAGE:
	    for (i = 0; i < n; i++) {
		if (my_row >= LINES)
		    my_row -= (LINES - 1);
		else
		    my_row = 0;
	    }
	    break;

	case 'r':
	case KEY_RIGHT:
	    shift += n;
	    break;

	case 'l':
	case KEY_LEFT:
	    shift -= n;
	    if (shift < 0) {
		shift = 0;
		beep();
	    }
	    break;

	case 'q':
	case QUIT:
	case ESCAPE:
	    done = TRUE;
	    break;

#ifdef KEY_RESIZE
	case KEY_RESIZE:	/* ignore this; ncurses will repaint */
	    break;
#endif
	case 's':
#if HAVE_HALFDELAY
	    if (got_number) {
		halfdelay(my_delay = n);
	    } else {
		nodelay(stdscr, FALSE);
		my_delay = -1;
	    }
#else
	    nodelay(stdscr, FALSE);
	    my_delay = -1;
#endif
	    break;
	case ' ':
	    nodelay(stdscr, TRUE);
	    my_delay = 0;
	    break;
	case CTRL('L'):
	    redrawwin(stdscr);
	    break;
	case ERR:
	    if (!my_delay)
		napms(50);
	    break;
	case HELP_KEY_1:
	    popup_msg(stdscr, help);
	    break;
	default:
	    beep();
	    break;
	}
	if (c >= KEY_MIN || (c > 0 && !isdigit(c))) {
	    got_number = FALSE;
	    value = 0;
	}
    }

    finish(0);			/* we're done */
}
#else
int
main(void)
{
    printf("This program requires the curses pad functions\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
