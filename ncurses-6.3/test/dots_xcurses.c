/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
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
 * Author: Thomas E. Dickey
 *
 * $Id: dots_xcurses.c,v 1.24 2020/08/29 16:22:03 juergen Exp $
 *
 * A simple demo of the wide-curses interface used for comparison with termcap.
 */
#include <test.priv.h>

#if !defined(_NC_WINDOWS)
#include <sys/time.h>
#endif

#include <time.h>

#if USE_WIDEC_SUPPORT

#if HAVE_ALLOC_PAIR
#define NewPair(n) x_option ? ((void *)&(n)) : NULL
#else
#define NewPair(n) NULL
#endif

#define InitPair(p,fg,bg) init_pair((short) (p), (short) (fg), (short) (bg))

static bool interrupted = FALSE;
static long total_chars = 0;
static time_t started;

#if HAVE_ALLOC_PAIR
static bool x_option = FALSE;
#endif

static void
cleanup(void)
{
    endwin();

    fflush(stdout);
    fprintf(stderr, "\n\n%ld total cells, rate %.2f/sec\n",
	    total_chars,
	    ((double) (total_chars) / (double) (time((time_t *) 0) - started)));
}

static void
onsig(int n GCC_UNUSED)
{
    interrupted = TRUE;
}

static double
ranf(void)
{
    long r = (rand() & 077777);
    return ((double) r / 32768.);
}

static int
mypair(int fg, int bg)
{
    int result;
#if HAVE_ALLOC_PAIR
    if (x_option) {
	result = alloc_pair(fg, bg);
    } else
#endif
    {
	int pair = (fg * COLORS) + bg;
	result = (pair >= COLOR_PAIRS) ? -1 : pair;
    }
    return result;
}

static void
set_colors(int fg, int bg)
{
    int pair = mypair(fg, bg);
    if (pair > 0) {
	(void) color_set((short) pair, NewPair(pair));
    }
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: dots_xcurses [options]"
	,""
	,"Options:"
	," -T TERM  override $TERM"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors()"
#endif
#if HAVE_USE_ENV
	," -e       allow environment $LINES / $COLUMNS"
#endif
	," -m SIZE  set margin (default: 2)"
	," -r SECS  self-interrupt/exit after specified number of seconds"
	," -s MSECS delay 1% of the time (default: 1 msecs)"
#if HAVE_ALLOC_PAIR
	," -x       use alloc_pair() rather than init_pair()"
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
    int fg, bg, ch;
    wchar_t wch[2];
    double r;
    double c;
#if HAVE_USE_DEFAULT_COLORS
    bool d_option = FALSE;
#endif
    int m_option = 2;
    int r_option = 0;
    int s_option = 1;
    size_t need;
    char *my_env;

    while ((ch = getopt(argc, argv, "T:dem:r:s:x")) != -1) {
	switch (ch) {
	case 'T':
	    need = 6 + strlen(optarg);
	    my_env = malloc(need);
	    _nc_SPRINTF(my_env, _nc_SLIMIT(need) "TERM=%s", optarg);
	    putenv(my_env);
	    break;
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
#if HAVE_USE_ENV
	case 'e':
	    use_env(TRUE);
	    break;
#endif
	case 'm':
	    m_option = atoi(optarg);
	    break;
	case 'r':
	    r_option = atoi(optarg);
	    break;
	case 's':
	    s_option = atoi(optarg);
	    break;
#if HAVE_ALLOC_PAIR
	case 'x':
	    x_option = TRUE;
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }

    srand((unsigned) time(0));

    SetupAlarm(r_option);
    InitAndCatch(initscr(), onsig);
    if (has_colors()) {
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option)
	    use_default_colors();
#endif
#if HAVE_ALLOC_PAIR
	if (x_option) {
	    ;			/* nothing */
	} else
#endif
	{
	    for (fg = 0; fg < COLORS; fg++) {
		for (bg = 0; bg < COLORS; bg++) {
		    int pair;
		    if (interrupted) {
			cleanup();
			ExitProgram(EXIT_FAILURE);
		    }
		    pair = mypair(fg, bg);
		    if (pair > 0) {
			InitPair(pair, fg, bg);
		    }
		}
	    }
	}
    }

    r = (double) (LINES - (2 * m_option));
    c = (double) (COLS - (2 * m_option));
    started = time((time_t *) 0);

    fg = COLOR_WHITE;
    bg = COLOR_BLACK;
    wch[1] = 0;
    while (!interrupted) {
	int x = (int) (c * ranf()) + m_option;
	int y = (int) (r * ranf()) + m_option;
	int p = (ranf() > 0.9) ? '*' : ' ';

	move(y, x);
	if (has_colors()) {
	    int z = (int) (ranf() * COLORS);
	    if (ranf() > 0.01) {
		set_colors(fg = z, bg);
	    } else {
		set_colors(fg, bg = z);
		if (s_option)
		    napms(s_option);
	    }
	} else {
	    if (ranf() <= 0.01) {
		if (ranf() > 0.6) {
		    attr_on(WA_REVERSE, NULL);
		} else {
		    attr_off(WA_REVERSE, NULL);
		}
		if (s_option)
		    napms(s_option);
	    }
	}
	wch[0] = (wchar_t) p;
	addnwstr(wch, 1);
	refresh();
	++total_chars;
    }
    cleanup();
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
