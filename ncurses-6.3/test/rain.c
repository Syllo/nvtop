/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
 * Copyright 1998-2014,2017 Free Software Foundation, Inc.                  *
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
 * $Id: rain.c,v 1.52 2020/08/29 16:22:03 juergen Exp $
 */
#include <test.priv.h>
#include <popup_msg.h>

/* rain 11/3/1980 EPS/CITHEP */

#ifdef USE_PTHREADS
#include <pthread.h>
#endif

WANT_USE_WINDOW();

#define MAX_THREADS	10
#define MAX_DROP	5

struct DATA;

typedef void (*DrawPart) (struct DATA *);

typedef struct DATA {
    int y, x;
#ifdef USE_PTHREADS
    DrawPart func;
    int state;
#endif
} DATA;

#ifdef USE_PTHREADS
pthread_cond_t cond_next_drop;
pthread_mutex_t mutex_next_drop;
static int used_threads;

typedef struct {
    pthread_t myself;
    long counter;
} STATS;

static STATS drop_threads[MAX_THREADS];
#endif

#if HAVE_USE_WINDOW
static int
safe_wgetch(WINDOW *w, void *data GCC_UNUSED)
{
    return wgetch(w);
}
#endif

static void
onsig(int n GCC_UNUSED)
{
    stop_curses();
    ExitProgram(EXIT_FAILURE);
}

static double
ranf(void)
{
    long r = (rand() & 077777);
    return ((double) r / 32768.);
}

static int
random_x(void)
{
    return (int) (((double) (COLS - 4) * ranf()) + 2);
}

static int
random_y(void)
{
    return (int) (((double) (LINES - 4) * ranf()) + 2);
}

static int
next_j(int j)
{
    if (j == 0)
	j = MAX_DROP - 1;
    else
	--j;
    if (has_colors()) {
	int z = (int) (3 * ranf());
	(void) attrset(AttrArg(COLOR_PAIR(z), (z ? A_BOLD : A_NORMAL)));
    }
    return j;
}

static void
part1(DATA * drop)
{
    MvAddCh(drop->y, drop->x, '.');
}

static void
part2(DATA * drop)
{
    MvAddCh(drop->y, drop->x, 'o');
}

static void
part3(DATA * drop)
{
    MvAddCh(drop->y, drop->x, 'O');
}

static void
part4(DATA * drop)
{
    MvAddCh(drop->y - 1, drop->x, '-');
    MvAddStr(drop->y, drop->x - 1, "|.|");
    MvAddCh(drop->y + 1, drop->x, '-');
}

static void
part5(DATA * drop)
{
    MvAddCh(drop->y - 2, drop->x, '-');
    MvAddStr(drop->y - 1, drop->x - 1, "/ \\");
    MvAddStr(drop->y, drop->x - 2, "| O |");
    MvAddStr(drop->y + 1, drop->x - 1, "\\ /");
    MvAddCh(drop->y + 2, drop->x, '-');
}

static void
part6(DATA * drop)
{
    MvAddCh(drop->y - 2, drop->x, ' ');
    MvAddStr(drop->y - 1, drop->x - 1, "   ");
    MvAddStr(drop->y, drop->x - 2, "     ");
    MvAddStr(drop->y + 1, drop->x - 1, "   ");
    MvAddCh(drop->y + 2, drop->x, ' ');
}

#ifdef USE_PTHREADS
static void
napsome(void)
{
    napms(60);
}

/*
 * This runs inside the use_window() mutex.
 */
static int
really_draw(WINDOW *win, void *arg)
{
    DATA *data = (DATA *) arg;

    (void) win;
    next_j(data->state);
    data->func(data);
    refresh();
    return OK;
}

static void
draw_part(void (*func) (DATA *), int state, DATA * data)
{
    data->func = func;
    data->state = state;
    use_window(stdscr, really_draw, (void *) data);
    napsome();
}

/*
 * Tell the threads that one of them can start work on a new raindrop.
 * They may all be busy if we're sending requests too rapidly.
 */
static int
put_next_drop(void)
{
    pthread_cond_signal(&cond_next_drop);
    pthread_mutex_unlock(&mutex_next_drop);

    return 0;
}

/*
 * Wait until we're assigned the task of drawing a new raindrop.
 */
static int
get_next_drop(void)
{
    pthread_mutex_lock(&mutex_next_drop);
    pthread_cond_wait(&cond_next_drop, &mutex_next_drop);

    return TRUE;
}

static void *
draw_drop(void *arg)
{
    DATA mydata;
    int mystats;

    /*
     * Find myself in the list of threads so we can count the number of loops.
     */
    for (mystats = 0; mystats < MAX_THREADS; ++mystats) {
#if defined(_NC_WINDOWS) && !defined(__WINPTHREADS_VERSION)
	if (drop_threads[mystats].myself.p == pthread_self().p)
#else
	if (drop_threads[mystats].myself == pthread_self())
#endif
	    break;
    }

    do {
	if (mystats < MAX_THREADS)
	    drop_threads[mystats].counter++;

	/*
	 * Make a copy of caller's data.  We're cheating for the cases after
	 * the first loop since we still have a pointer into the main thread
	 * to the data which it uses for setting up this thread (but it has
	 * been modified to use different coordinates).
	 */
	mydata = *(DATA *) arg;

	draw_part(part1, 0, &mydata);
	draw_part(part2, 1, &mydata);
	draw_part(part3, 2, &mydata);
	draw_part(part4, 3, &mydata);
	draw_part(part5, 4, &mydata);
	draw_part(part6, 0, &mydata);
    } while (get_next_drop());

    return NULL;
}

/*
 * The description of pthread_create() is misleading, since it implies that
 * threads will exit cleanly after their function returns.
 *
 * Since they do not (and the number of threads is limited by system
 * resources), make a limited number of threads, and signal any that are
 * waiting when we want a thread past that limit.
 */
static int
start_drop(DATA * data)
{
    int rc;

    if (!used_threads) {
	/* mutex and condition for signalling thread */
	pthread_mutex_init(&mutex_next_drop, NULL);
	pthread_cond_init(&cond_next_drop, NULL);
    }

    if (used_threads < MAX_THREADS) {
	rc = pthread_create(&(drop_threads[used_threads].myself),
			    NULL,
			    draw_drop,
			    data);
	++used_threads;
    } else {
	rc = put_next_drop();
    }
    return rc;
}
#endif

static int
get_input(void)
{
    return USING_WINDOW1(stdscr, wgetch, safe_wgetch);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: rain [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors"
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
	" q/Q        exit the program",
	" s          do single-step",
	" <space>    undo single-step",
	"",
	0
    };

    bool done = FALSE;
    DATA drop;
#ifndef USE_PTHREADS
    DATA last[MAX_DROP];
#endif
    int j = 0;
    int ch;
#if HAVE_USE_DEFAULT_COLORS
    bool d_option = FALSE;
#endif

    while ((ch = getopt(argc, argv, "d")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    if (optind < argc)
	usage();

    setlocale(LC_ALL, "");

    InitAndCatch(initscr(), onsig);
    if (has_colors()) {
	int bg = COLOR_BLACK;
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() == OK))
	    bg = -1;
#endif
	init_pair(1, COLOR_BLUE, (short) bg);
	init_pair(2, COLOR_CYAN, (short) bg);
    }
    nl();
    noecho();
    curs_set(0);
    timeout(0);

#ifndef USE_PTHREADS
    for (j = MAX_DROP; --j >= 0;) {
	last[j].x = random_x();
	last[j].y = random_y();
    }
    j = 0;
#endif

    while (!done) {
	drop.x = random_x();
	drop.y = random_y();

#ifdef USE_PTHREADS
	if (start_drop(&drop) != 0) {
	    beep();
	}
#else
	/*
	 * The non-threaded code draws parts of each drop on each loop.
	 */
	part1(&drop);

	part2(&last[j]);

	j = next_j(j);
	part3(&last[j]);

	j = next_j(j);
	part4(&last[j]);

	j = next_j(j);
	part5(&last[j]);

	j = next_j(j);
	part6(&last[j]);

	last[j] = drop;
#endif

	switch (get_input()) {
	case ('q'):
	case ('Q'):
	    done = TRUE;
	    break;
	case 's':
	    nodelay(stdscr, FALSE);
	    break;
	case ' ':
	    nodelay(stdscr, TRUE);
	    break;
#ifdef KEY_RESIZE
	case (KEY_RESIZE):
	    break;
#endif
	case HELP_KEY_1:
	    popup_msg(stdscr, help);
	    break;
	case ERR:
	    break;
	default:
	    beep();
	}
	napms(50);
    }
    stop_curses();
#ifdef USE_PTHREADS
    printf("Counts per thread:\n");
    for (j = 0; j < MAX_THREADS; ++j)
	printf("  %d:%ld\n", j, drop_threads[j].counter);
#endif
    ExitProgram(EXIT_SUCCESS);
}
