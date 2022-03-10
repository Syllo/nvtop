/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
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
/*

	 @@@        @@@    @@@@@@@@@@     @@@@@@@@@@@    @@@@@@@@@@@@
	 @@@        @@@   @@@@@@@@@@@@    @@@@@@@@@@@@   @@@@@@@@@@@@@
	 @@@        @@@  @@@@      @@@@   @@@@           @@@@ @@@  @@@@
	 @@@   @@   @@@  @@@        @@@   @@@            @@@  @@@   @@@
	 @@@  @@@@  @@@  @@@        @@@   @@@            @@@  @@@   @@@
	 @@@@ @@@@ @@@@  @@@        @@@   @@@            @@@  @@@   @@@
	  @@@@@@@@@@@@   @@@@      @@@@   @@@            @@@  @@@   @@@
	   @@@@  @@@@     @@@@@@@@@@@@    @@@            @@@  @@@   @@@
	    @@    @@       @@@@@@@@@@     @@@            @@@  @@@   @@@

				 Eric P. Scott
			  Caltech High Energy Physics
				 October, 1980

		Hacks to turn this into a test frame for cursor movement:
			Eric S. Raymond <esr@snark.thyrsus.com>
				January, 1995

		July 1995 (esr): worms is now in living color! :-)

  This program makes a good torture-test for the ncurses cursor-optimization
  code.  You can use -T to set the worm move interval over which movement
  traces will be dumped.  The program stops and waits for one character of
  input at the beginning and end of the interval.

  $Id: worm.c,v 1.82 2020/02/02 23:34:34 tom Exp $
*/

#include <test.priv.h>

#ifndef NCURSES_VERSION
#undef TRACE
#endif

#ifdef USE_PTHREADS
#include <pthread.h>
#endif

WANT_USE_WINDOW();

#define MAX_WORMS	40
#define MAX_LENGTH	1024

static chtype flavor[] =
{
    'O', '*', '#', '$', '%', '0', '@',
};
static const int xinc[] =
{
    1, 1, 1, 0, -1, -1, -1, 0
}, yinc[] =
{
    -1, 0, 1, 1, 1, 0, -1, -1
};

typedef struct worm {
    int orientation;
    int head;
    int *xpos;
    int *ypos;
    chtype attrs;
#ifdef USE_PTHREADS
    pthread_t thread;
#endif
} WORM;

static unsigned long sequence = 0;
static bool quitting = FALSE;

static WORM worm[MAX_WORMS];
static int max_refs;
static int **refs;
static int last_x, last_y;

static const char *field;
static int length = 16, number = 3;
static chtype trail = ' ';

static unsigned pending;
#ifdef TRACE
static int generation, trace_start, trace_end;
#endif /* TRACE */
/* *INDENT-OFF* */
static const struct options {
    int nopts;
    int opts[3];
} normal[8]={
    { 3, { 7, 0, 1 } },
    { 3, { 0, 1, 2 } },
    { 3, { 1, 2, 3 } },
    { 3, { 2, 3, 4 } },
    { 3, { 3, 4, 5 } },
    { 3, { 4, 5, 6 } },
    { 3, { 5, 6, 7 } },
    { 3, { 6, 7, 0 } }
}, upper[8]={
    { 1, { 1, 0, 0 } },
    { 2, { 1, 2, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 2, { 4, 5, 0 } },
    { 1, { 5, 0, 0 } },
    { 2, { 1, 5, 0 } }
}, left[8]={
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 2, { 2, 3, 0 } },
    { 1, { 3, 0, 0 } },
    { 2, { 3, 7, 0 } },
    { 1, { 7, 0, 0 } },
    { 2, { 7, 0, 0 } }
}, right[8]={
    { 1, { 7, 0, 0 } },
    { 2, { 3, 7, 0 } },
    { 1, { 3, 0, 0 } },
    { 2, { 3, 4, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 2, { 6, 7, 0 } }
}, lower[8]={
    { 0, { 0, 0, 0 } },
    { 2, { 0, 1, 0 } },
    { 1, { 1, 0, 0 } },
    { 2, { 1, 5, 0 } },
    { 1, { 5, 0, 0 } },
    { 2, { 5, 6, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } }
}, upleft[8]={
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 1, { 3, 0, 0 } },
    { 2, { 1, 3, 0 } },
    { 1, { 1, 0, 0 } }
}, upright[8]={
    { 2, { 3, 5, 0 } },
    { 1, { 3, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 1, { 5, 0, 0 } }
}, lowleft[8]={
    { 3, { 7, 0, 1 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 1, { 1, 0, 0 } },
    { 2, { 1, 7, 0 } },
    { 1, { 7, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } }
}, lowright[8]={
    { 0, { 0, 0, 0 } },
    { 1, { 7, 0, 0 } },
    { 2, { 5, 7, 0 } },
    { 1, { 5, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } },
    { 0, { 0, 0, 0 } }
};
/* *INDENT-ON* */

#if HAVE_USE_WINDOW
static int
safe_wgetch(WINDOW *w, void *data GCC_UNUSED)
{
    return wgetch(w);
}
static int
safe_wrefresh(WINDOW *w, void *data GCC_UNUSED)
{
    return wrefresh(w);
}
#endif

#ifdef KEY_RESIZE
static void
failed(const char *s)
{
    perror(s);
    stop_curses();
    ExitProgram(EXIT_FAILURE);
}
#endif

static void
cleanup(void)
{
    USING_WINDOW1(stdscr, wrefresh, safe_wrefresh);
    stop_curses();
}

static void
onsig(int sig GCC_UNUSED)
{
    cleanup();
    ExitProgram(EXIT_FAILURE);
}

static double
ranf(void)
{
    long r = (rand() & 077777);
    return ((double) r / 32768.);
}

static int
draw_worm(WINDOW *win, void *data)
{
    WORM *w = (WORM *) data;
    const struct options *op;
    unsigned mask = (unsigned) (~(1 << (w - worm)));
    chtype attrs = w->attrs | ((mask & pending) ? A_REVERSE : 0);

    int x;
    int y;
    int h;

    bool done = FALSE;

    if ((x = w->xpos[h = w->head]) < 0) {
	wmove(win, y = w->ypos[h] = last_y, x = w->xpos[h] = 0);
	waddch(win, attrs);
	refs[y][x]++;
    } else {
	y = w->ypos[h];
    }

    if (x > last_x)
	x = last_x;
    if (y > last_y)
	y = last_y;

    if (++h == length)
	h = 0;

    if (w->xpos[w->head = h] >= 0) {
	int x1, y1;
	x1 = w->xpos[h];
	y1 = w->ypos[h];
	if (y1 < LINES
	    && x1 < COLS
	    && --refs[y1][x1] == 0) {
	    wmove(win, y1, x1);
	    waddch(win, trail);
	}
    }

    op = &(x == 0
	   ? (y == 0
	      ? upleft
	      : (y == last_y
		 ? lowleft
		 : left))
	   : (x == last_x
	      ? (y == 0
		 ? upright
		 : (y == last_y
		    ? lowright
		    : right))
	      : (y == 0
		 ? upper
		 : (y == last_y
		    ? lower
		    : normal))))[w->orientation];

    switch (op->nopts) {
    case 0:
	done = TRUE;
	Trace(("done - draw_worm"));
	break;
    case 1:
	w->orientation = op->opts[0];
	break;
    default:
	w->orientation = op->opts[(int) (ranf() * (double) op->nopts)];
	break;
    }

    if (!done) {
	x += xinc[w->orientation];
	y += yinc[w->orientation];
	wmove(win, y, x);

	if (y < 0)
	    y = 0;
	waddch(win, attrs);

	w->ypos[h] = y;
	w->xpos[h] = x;
	refs[y][x]++;
    }

    return done;
}

#ifdef USE_PTHREADS
static bool
quit_worm(int bitnum)
{
    pending = (pending | (unsigned) (1 << bitnum));
    napms(10);			/* let the other thread(s) have a chance */
    pending = (pending & (unsigned) ~(1 << bitnum));
    return quitting;
}

static void *
start_worm(void *arg)
{
    unsigned long compare = 0;
    Trace(("start_worm"));
    while (!quit_worm((int) (((struct worm *) arg) - worm))) {
	while (compare < sequence) {
	    ++compare;
#if HAVE_USE_WINDOW
	    use_window(stdscr, draw_worm, arg);
#else
	    draw_worm(stdscr, arg);
#endif
	}
    }
    Trace(("...start_worm (done)"));
    return NULL;
}
#endif

static bool
draw_all_worms(void)
{
    bool done = FALSE;
    int n;
    struct worm *w;

#ifdef USE_PTHREADS
    static bool first = TRUE;
    if (first) {
	first = FALSE;
	for (n = 0, w = &worm[0]; n < number; n++, w++) {
	    (void) pthread_create(&(w->thread), NULL, start_worm, w);
	}
    }
#else
    for (n = 0, w = &worm[0]; n < number; n++, w++) {
	if (
#if HAVE_USE_WINDOW
	       USING_WINDOW2(stdscr, draw_worm, w)
#else
	       draw_worm(stdscr, w)
#endif
	    )
	    done = TRUE;
    }
#endif
    return done;
}

static int
get_input(void)
{
    int ch;
    ch = USING_WINDOW1(stdscr, wgetch, safe_wgetch);
    return ch;
}

#ifdef KEY_RESIZE
static int
update_refs(WINDOW *win, void *data)
{
    int x, y;

    (void) win;
    (void) data;
    if (last_x != COLS - 1) {
	for (y = 0; y <= last_y; y++) {
	    refs[y] = typeRealloc(int, (size_t) COLS, refs[y]);
	    if (!refs[y])
		failed("update_refs");
	    for (x = last_x + 1; x < COLS; x++)
		refs[y][x] = 0;
	}
	last_x = COLS - 1;
    }
    if (last_y != LINES - 1) {
	for (y = LINES; y <= last_y; y++)
	    free(refs[y]);
	max_refs = LINES;
	refs = typeRealloc(int *, (size_t) LINES, refs);
	for (y = last_y + 1; y < LINES; y++) {
	    refs[y] = typeMalloc(int, (size_t) COLS);
	    if (!refs[y])
		failed("update_refs");
	    for (x = 0; x < COLS; x++)
		refs[y][x] = 0;
	}
	last_y = LINES - 1;
    }
    return OK;
}
#endif

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: worm [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors"
#endif
	," -f       fill screen with copies of \"WORM\" at start"
	," -l <n>   set length of worms"
	," -n <n>   set number of worms"
	," -t       leave trail of \".\""
#ifdef TRACE
	," -T <start>,<end> set trace interval"
	," -N       suppress cursor-movement optimization"
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
    int ch;
    int x, y;
    int n;
    struct worm *w;
    int *ip;
    bool done = FALSE;
#if HAVE_USE_DEFAULT_COLORS
    bool opt_d = FALSE;
#endif

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "dfl:n:tT:N")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    opt_d = TRUE;
	    break;
#endif
	case 'f':
	    field = "WORM";
	    break;
	case 'l':
	    if ((length = atoi(optarg)) < 2 || length > MAX_LENGTH) {
		fprintf(stderr, "%s: Invalid length\n", *argv);
		usage();
	    }
	    break;
	case 'n':
	    if ((number = atoi(optarg)) < 1 || number > MAX_WORMS) {
		fprintf(stderr, "%s: Invalid number of worms\n", *argv);
		usage();
	    }
	    break;
	case 't':
	    trail = '.';
	    break;
#ifdef TRACE
	case 'T':
	    if (sscanf(optarg, "%d,%d", &trace_start, &trace_end) != 2)
		usage();
	    break;
	case 'N':
	    _nc_optimize_enable ^= OPTIMIZE_ALL;	/* declared by ncurses */
	    break;
#endif /* TRACE */
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    if (optind < argc)
	usage();

    signal(SIGINT, onsig);
    initscr();
    noecho();
    cbreak();
    nonl();

    curs_set(0);

    last_y = LINES - 1;
    last_x = COLS - 1;

#ifdef A_COLOR
    if (has_colors()) {
	int bg = COLOR_BLACK;
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (opt_d && (use_default_colors() == OK))
	    bg = -1;
#endif

#define SET_COLOR(num, fg) \
	    init_pair(num+1, (short) fg, (short) bg); \
	    flavor[num] |= (chtype) COLOR_PAIR(num+1) | A_BOLD

	SET_COLOR(0, COLOR_GREEN);
	SET_COLOR(1, COLOR_RED);
	SET_COLOR(2, COLOR_CYAN);
	SET_COLOR(3, COLOR_WHITE);
	SET_COLOR(4, COLOR_MAGENTA);
	SET_COLOR(5, COLOR_BLUE);
	SET_COLOR(6, COLOR_YELLOW);
    }
#endif /* A_COLOR */

    max_refs = LINES;
    refs = typeMalloc(int *, (size_t) max_refs);
    for (y = 0; y < max_refs; y++) {
	refs[y] = typeMalloc(int, (size_t) COLS);
	for (x = 0; x < COLS; x++) {
	    refs[y][x] = 0;
	}
    }

#ifdef BADCORNER
    /* if addressing the lower right corner doesn't work in your curses */
    refs[last_y][last_x] = 1;
#endif /* BADCORNER */

    for (n = number, w = &worm[0]; --n >= 0; w++) {
	w->attrs = flavor[(unsigned) n % SIZEOF(flavor)];
	w->orientation = 0;
	w->head = 0;

	if (!(ip = typeMalloc(int, (size_t) (length + 1)))) {
	    fprintf(stderr, "%s: out of memory\n", *argv);
	    ExitProgram(EXIT_FAILURE);
	}
	w->xpos = ip;
	for (x = length; --x >= 0;)
	    *ip++ = -1;
	if (!(ip = typeMalloc(int, (size_t) (length + 1)))) {
	    fprintf(stderr, "%s: out of memory\n", *argv);
	    ExitProgram(EXIT_FAILURE);
	}
	w->ypos = ip;
	for (y = length; --y >= 0;)
	    *ip++ = -1;
    }
    if (field) {
	const char *p;
	p = field;
	for (y = last_y; --y >= 0;) {
	    for (x = COLS; --x >= 0;) {
		addch((chtype) (*p++));
		if (!*p)
		    p = field;
	    }
	}
    }
    USING_WINDOW1(stdscr, wrefresh, safe_wrefresh);
    nodelay(stdscr, TRUE);

    while (!done) {
	++sequence;
	if ((ch = get_input()) > 0) {
#ifdef TRACE
	    if (trace_start || trace_end) {
		if (generation == trace_start) {
		    curses_trace(TRACE_CALLS);
		    get_input();
		} else if (generation == trace_end) {
		    curses_trace(0);
		    get_input();
		}

		generation++;
	    }
#endif

#ifdef KEY_RESIZE
	    if (ch == KEY_RESIZE) {
		USING_WINDOW(stdscr, update_refs);
	    }
#endif

	    /*
	     * Make it simple to put this into single-step mode, or resume
	     * normal operation -T.Dickey
	     */
	    if (ch == 'q') {
		quitting = TRUE;
		done = TRUE;
		Trace(("done - quitting"));
		continue;
	    } else if (ch == 's') {
		nodelay(stdscr, FALSE);
	    } else if (ch == ' ') {
		nodelay(stdscr, TRUE);
	    }
	}

	done = draw_all_worms();
	napms(10);
	USING_WINDOW1(stdscr, wrefresh, safe_wrefresh);
    }

    Trace(("Cleanup"));
    cleanup();
#if NO_LEAKS
    for (y = 0; y < max_refs; y++) {
	free(refs[y]);
    }
    free(refs);
    for (n = number, w = &worm[0]; --n >= 0; w++) {
	free(w->xpos);
	free(w->ypos);
    }
#endif
#ifdef USE_PTHREADS
    /*
     * Do this just in case one of the threads did not really exit.
     */
    Trace(("join all threads"));
    for (n = 0; n < number; n++) {
	pthread_join(worm[n].thread, NULL);
    }
#endif
    ExitProgram(EXIT_SUCCESS);
}
