/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
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
 * $Id: color_content.c,v 1.12 2020/02/02 23:34:34 tom Exp $
 */

#define NEED_TIME_H
#include <test.priv.h>

#if USE_EXTENDED_COLOR
typedef int my_color_t;
#else
typedef NCURSES_COLOR_T my_color_t;
#endif

typedef struct {
    my_color_t r;
    my_color_t g;
    my_color_t b;
} MYCOLOR;

static int f_opt;
static int i_opt;
static int l_opt;
static int n_opt;
static int p_opt;
static int r_opt;
static int s_opt;

#if USE_EXTENDED_COLOR
static int x_opt;
#endif

static MYCOLOR *expected;

#if HAVE_GETTIMEOFDAY
static struct timeval initial_time;
static struct timeval finish_time;
#endif

static void
failed(const char *msg)
{
    printw("%s", msg);
    getch();
    endwin();
    ExitProgram(EXIT_FAILURE);
}

#if USE_EXTENDED_COLOR
static int
InitColor(int pair, int r, int g, int b)
{
    int rc;
    if (x_opt) {
	rc = init_extended_color(pair, r, g, b);
    } else {
	rc = init_color((NCURSES_PAIRS_T) pair,
			(NCURSES_COLOR_T) r,
			(NCURSES_COLOR_T) g,
			(NCURSES_COLOR_T) b);
    }
    return rc;
}

static int
ColorContent(int color, int *rp, int *gp, int *bp)
{
    int rc;
    if (x_opt) {
	rc = extended_color_content(color, rp, gp, bp);
    } else {
	NCURSES_COLOR_T r, g, b;
	if ((rc = color_content((NCURSES_COLOR_T) color, &r, &g, &b)) == OK) {
	    *rp = r;
	    *gp = g;
	    *bp = b;
	}
    }
    return rc;
}
#else
#define InitColor(color,r,g,b)       init_color((NCURSES_COLOR_T)color,(NCURSES_COLOR_T)r,(NCURSES_COLOR_T)g,(NCURSES_COLOR_T)b)
#define ColorContent(color,rp,gp,bp) color_content((NCURSES_COLOR_T)color,rp,gp,bp)
#endif

static my_color_t
random_color(void)
{
    return (my_color_t) (rand() % 1000);
}

static void
setup_test(void)
{
    initscr();
    cbreak();
    noecho();
    scrollok(stdscr, TRUE);
    if (has_colors()) {
	start_color();
	if (!can_change_color() && !p_opt)
	    failed("this terminal cannot initialize colors");

	if (!f_opt)
	    f_opt = 0;
	if (!l_opt)
	    l_opt = COLORS;
	if (l_opt <= 0)
	    failed("color limit must be greater than zero");

	if (!n_opt) {
	    int color;
	    size_t need = (size_t) ((l_opt > COLORS) ? l_opt : COLORS) + 1;

	    expected = typeCalloc(MYCOLOR, need);
	    if (s_opt) {
		int r;
		int g;
		int b;
		color = f_opt;
		for (r = 0; r < 1000; ++r) {
		    for (g = 0; g < 1000; ++g) {
			for (b = 0; b < 1000; ++b) {
			    if (color < l_opt) {
				InitColor(color, r, g, b);
				expected[color].r = (my_color_t) r;
				expected[color].g = (my_color_t) g;
				expected[color].b = (my_color_t) b;
				++color;
			    } else {
				break;
			    }
			}
		    }
		}
	    } else {
		for (color = f_opt; color < l_opt; ++color) {
		    expected[color].r = random_color();
		    expected[color].g = random_color();
		    expected[color].b = random_color();
		    InitColor(color,
			      expected[color].r,
			      expected[color].g,
			      expected[color].b);
		}
	    }
	}
    } else {
	failed("This demo requires a color terminal");
    }
#if HAVE_GETTIMEOFDAY
    gettimeofday(&initial_time, 0);
#endif
}

static void
run_test(void)
{
    int color;
    bool success = TRUE;
    for (color = f_opt; color < l_opt; ++color) {
	my_color_t r;
	my_color_t g;
	my_color_t b;
	if (ColorContent(color, &r, &g, &b) == OK) {
	    if (expected != 0) {
		if (r != expected[color].r)
		    success = FALSE;
		if (g != expected[color].g)
		    success = FALSE;
		if (b != expected[color].b)
		    success = FALSE;
	    }
	}
    }
    if (i_opt) {
	addch(success ? '.' : '?');
	refresh();
    }
}

static void
finish_test(void)
{
    getch();
    endwin();
}

#if HAVE_GETTIMEOFDAY
static double
seconds(struct timeval *mark)
{
    double result = (double) mark->tv_sec;
    result += ((double) mark->tv_usec / 1e6);
    return result;
}
#endif

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: color_content [options]"
	,""
	,"Options:"
	," -f COLOR first color value to test (default: 0)"
	," -i       interactive, showing test-progress"
	," -l COLOR last color value to test (default: max_colors-1)"
	," -n       do not initialize color pairs"
	," -p       print data for color content instead of testing"
	," -r COUNT repeat for given count"
	," -s       initialize pairs sequentially rather than random"
#if USE_EXTENDED_COLOR
	," -x       use extended color pairs/values"
#endif
    };
    size_t n;
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int i;

    while ((i = getopt(argc, argv, "f:il:npr:sx")) != -1) {
	switch (i) {
	case 'f':
	    if ((f_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 'i':
	    i_opt = 1;
	    break;
	case 'l':
	    if ((l_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 'n':
	    n_opt = 1;
	    break;
	case 'p':
	    p_opt = 1;
	    break;
	case 'r':
	    if ((r_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 's':
	    s_opt = 1;
	    break;
#if USE_EXTENDED_COLOR
	case 'x':
	    x_opt = 1;
	    break;
#endif
	default:
	    usage();
	}
    }
    if (optind < argc)
	usage();
    if (r_opt <= 0)
	r_opt = 1;

    setup_test();
    if (p_opt) {
	endwin();
	for (i = 0; i < COLORS; ++i) {
	    my_color_t r, g, b;
	    if (ColorContent(i, &r, &g, &b) == OK) {
		printf("%d: %d %d %d\n", i, r, g, b);
	    } else {
		printf("%d: ? ?\n", i);
	    }
	}
    } else {
	int repeat;

	for (repeat = 0; repeat < r_opt; ++repeat) {
	    run_test();
	    if (i_opt) {
		addch('.');
		refresh();
	    }
	}

	if (i_opt) {
	    addch('\n');
	}
	printw("DONE: ");
#if HAVE_GETTIMEOFDAY
	gettimeofday(&finish_time, 0);
	printw("%.03f seconds",
	       seconds(&finish_time)
	       - seconds(&initial_time));
#endif
	finish_test();
    }

    ExitProgram(EXIT_SUCCESS);
}
