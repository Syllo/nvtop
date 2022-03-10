/* $Id: tclock.c,v 1.42 2020/12/26 17:56:36 tom Exp $ */

#define NEED_TIME_H
#include <test.priv.h>

#if HAVE_MATH_H

#include <math.h>

/*
  tclock - analog/digital clock for curses.
  If it gives you joy, then
  (a) I'm glad
  (b) you need to get out more :-)

  This program is copyright Howard Jones, September 1994
  (ha.jones@ic.ac.uk). It may be freely distributed as
  long as this copyright message remains intact, and any
  modifications are clearly marked as such. [In fact, if
  you modify it, I wouldn't mind the modifications back,
  especially if they add any nice features. A good one
  would be a precalc table for the 60 hand positions, so
  that the floating point stuff can be ditched. As I said,
  it was a 20 hackup minute job.]

  COMING SOON: tfishtank. Be the envy of your mac-owning
  colleagues.
*/

/* To compile: cc -o tclock tclock.c -lcurses -lm */

#ifndef PI
#define PI 3.141592654
#endif

#define sign(_x) (_x<0?-1:1)

#define ASPECT 2.2
#define ROUND(value) ((int)((value) + 0.5))

#define A2X(angle,radius) ROUND(ASPECT * radius * sin(angle))
#define A2Y(angle,radius) ROUND(radius * cos(angle))

/* Plot a point */
static void
plot(int x, int y, int col)
{
    MvAddCh(y, x, (chtype) col);
}

/* Draw a diagonal(arbitrary) line using Bresenham's algorithm. */
static void
dline(int pair, int from_x, int from_y, int x2, int y2, int ch)
{
    int dx, dy;
    int ax, ay;
    int sx, sy;
    int x, y;
    int d;

    if (has_colors())
	(void) attrset(AttrArg(COLOR_PAIR(pair), 0));

    dx = x2 - from_x;
    dy = y2 - from_y;

    ax = abs(dx * 2);
    ay = abs(dy * 2);

    sx = sign(dx);
    sy = sign(dy);

    x = from_x;
    y = from_y;

    if (ax > ay) {
	d = ay - (ax / 2);

	while (1) {
	    plot(x, y, ch);
	    if (x == x2)
		return;

	    if (d >= 0) {
		y += sy;
		d -= ax;
	    }
	    x += sx;
	    d += ay;
	}
    } else {
	d = ax - (ay / 2);

	while (1) {
	    plot(x, y, ch);
	    if (y == y2)
		return;

	    if (d >= 0) {
		x += sx;
		d -= ay;
	    }
	    y += sy;
	    d += ax;
	}
    }
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: tclock [options]"
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
    int i, cx, cy;
    double cr, mradius, hradius, mangle, hangle;
    double sangle, sradius, hours;
    int hdx, hdy;
    int mdx, mdy;
    int sdx, sdy;
    int ch;
    int lastbeep = -1;
    bool odd = FALSE;
    time_t tim;
    struct tm *t;
    char szChar[20];
    char *text;
    short my_bg = COLOR_BLACK;
#if HAVE_GETTIMEOFDAY
    struct timeval current;
#endif
    double fraction = 0.0;
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

    initscr();
    noecho();
    cbreak();
    nodelay(stdscr, TRUE);
    curs_set(0);

    if (has_colors()) {
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() == OK))
	    my_bg = -1;
#endif
	init_pair(1, COLOR_RED, my_bg);
	init_pair(2, COLOR_MAGENTA, my_bg);
	init_pair(3, COLOR_GREEN, my_bg);
	init_pair(4, COLOR_WHITE, COLOR_BLUE);
    }
#ifdef KEY_RESIZE
    keypad(stdscr, TRUE);
  restart:
#endif
    cx = (COLS - 1) / 2;	/* 39 */
    cy = LINES / 2;		/* 12 */
    if (cx / ASPECT < cy)
	cr = cx / ASPECT;
    else
	cr = cy;
    sradius = (5 * cr) / 6;	/* 10 */
    mradius = (3 * cr) / 4;	/* 9 */
    hradius = cr / 2;		/* 6 */

    for (i = 0; i < 12; i++) {
	sangle = (i + 1) * (2.0 * PI) / 12.0;
	sdx = A2X(sangle, sradius);
	sdy = A2Y(sangle, sradius);
	_nc_SPRINTF(szChar, _nc_SLIMIT(sizeof(szChar)) "%d", i + 1);

	MvAddStr(cy - sdy, cx + sdx, szChar);
    }

    MvAddStr(0, 0, "ASCII Clock by Howard Jones (ha.jones@ic.ac.uk),1994");

    sradius = (4 * sradius) / 5;
    for (;;) {
	napms(100);

	tim = time(0);
	t = localtime(&tim);

	hours = (t->tm_hour + (t->tm_min / 60.0));
	if (hours > 12.0)
	    hours -= 12.0;

	mangle = ((t->tm_min + (t->tm_sec / 60.0)) * (2 * PI) / 60.0);
	mdx = A2X(mangle, mradius);
	mdy = A2Y(mangle, mradius);

	hangle = ((hours) * (2.0 * PI) / 12.0);
	hdx = A2X(hangle, hradius);
	hdy = A2Y(hangle, hradius);

#if HAVE_GETTIMEOFDAY
	gettimeofday(&current, 0);
	fraction = ((double) current.tv_usec / 1.0e6);
#endif
	sangle = ((t->tm_sec + fraction) * (2.0 * PI) / 60.0);
	sdx = A2X(sangle, sradius);
	sdy = A2Y(sangle, sradius);

	dline(3, cx, cy, cx + mdx, cy - mdy, '#');

	(void) attrset(A_REVERSE);
	dline(2, cx, cy, cx + hdx, cy - hdy, '.');
	attroff(A_REVERSE);

	if (has_colors())
	    (void) attrset(AttrArg(COLOR_PAIR(1), 0));

	dline(1, cx, cy, cx + sdx, cy - sdy, 'O');

	if (has_colors())
	    (void) attrset(AttrArg(COLOR_PAIR(0), 0));

	text = ctime(&tim);
	MvPrintw(2, 0, "%.*s", (int) (strlen(text) - 1), text);
	refresh();
	if ((t->tm_sec % 5) == 0
	    && t->tm_sec != lastbeep) {
	    lastbeep = t->tm_sec;
	    if (has_colors()) {
		odd = !odd;
		bkgd((chtype) (odd ? COLOR_PAIR(4) : COLOR_PAIR(0)));
	    }
	    beep();
	}

	if ((ch = getch()) != ERR) {
#ifdef KEY_RESIZE
	    if (ch == KEY_RESIZE) {
		flash();
		erase();
		wrefresh(curscr);
		goto restart;
	    }
#endif
	    break;
	}

	dline(0, cx, cy, cx + hdx, cy - hdy, ' ');
	dline(0, cx, cy, cx + mdx, cy - mdy, ' ');
	dline(0, cx, cy, cx + sdx, cy - sdy, ' ');

    }

    stop_curses();
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("This program requires the development header math.h\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
