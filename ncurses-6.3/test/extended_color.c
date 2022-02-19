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
 * $Id: extended_color.c,v 1.15 2020/02/02 23:34:34 tom Exp $
 */

#include <test.priv.h>

#if USE_EXTENDED_COLOR

#define SHOW(n) ((n) == ERR ? "ERR" : "OK")

#if USE_SP_FUNCS
static bool opt_s = FALSE;
#define if_opt_s(a,b) (opt_s ? (a) : (b))
#else
#define if_opt_s(a,b) (b)
#endif

static void
failed(const char *name)
{
    printw("...%s failed", name);
    getch();
    endwin();
    ExitProgram(EXIT_FAILURE);
}

static void
do_pair_content(SCREEN *sp, int pair)
{
    int i, f, b;

    (void) sp;
    i = if_opt_s(extended_pair_content_sp(sp, pair, &f, &b),
		 extended_pair_content(0, &f, &b));
    if (i != OK)
	failed("pair_content");
    printw("pair %d contains (%d,%d)\n", pair, f, b);
    getch();
}

static void
do_init_pair(SCREEN *sp, int pair, int fg, int bg)
{
    int i;

    (void) sp;
    i = if_opt_s(init_extended_pair_sp(sp, pair, fg, bg),
		 init_extended_pair(pair, fg, bg));
    if (i != OK)
	failed("init_pair");
}

static void
do_init_color(SCREEN *sp, int color, int adjust)
{
    int r, g, b;
    int i;

    (void) sp;
    i = if_opt_s(extended_color_content_sp(sp, color, &r, &g, &b),
		 extended_color_content(color, &r, &g, &b));
    if (i != OK)
	failed("color_content");

    r = (adjust + 1000 + r) % 1000;
    g = (adjust + 1000 + g) % 1000;
    b = (adjust + 1000 + b) % 1000;

    i = if_opt_s(init_extended_color_sp(sp, color, r, g, b),
		 init_extended_color(color, r, g, b));
    if (i != OK)
	failed("init_color");
}

static void
do_color_set(const char *expected, int pair)
{
    int i = color_set((short) pair, (void *) &pair);
    printw("%s (%s)\n", expected, SHOW(i));
    if (i != OK)
	failed("color_set");
    getch();
}

static void
show_1_rgb(SCREEN *sp, const char *name, int color, int y, int x)
{
    int r, g, b;
    int i;

    (void) sp;
    i = if_opt_s(extended_color_content_sp(sp, color, &r, &g, &b),
		 extended_color_content(color, &r, &g, &b));
    wmove(stdscr, y, x);
    if (i == OK) {
	printw("%-8s %3d/%3d/%3d", name, r, g, b);
    } else {
	printw("%-8s %s", name, SHOW(i));
    }
}

static void
show_rgb(SCREEN *sp)
{
    int y, x;
    getyx(stdscr, y, x);
    show_1_rgb(sp, "RED", COLOR_RED, y + 1, x);
    show_1_rgb(sp, "GREEN", COLOR_GREEN, y + 2, x);
    show_1_rgb(sp, "BLUE", COLOR_BLUE, y + 3, x);
    wmove(stdscr, y, x);
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: extended_color",
	"",
	"Options:",
	" -s   use sp-funcs",
	NULL
    };
    size_t n;
    for (n = 0; n < SIZEOF(tbl); ++n) {
	fprintf(stderr, "%s\n", tbl[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int i;
    SCREEN *sp;

    while ((i = getopt(argc, argv, "s")) != -1) {
	switch (i) {
#if USE_SP_FUNCS
	case 's':
	    opt_s = TRUE;
	    break;
#endif
	default:
	    usage();
	    /* NOTREACHED */
	}
    }

    slk_init(1);
    sp = newterm(NULL, stdout, stdin);
    cbreak();
    noecho();

    if (!has_colors()) {
	endwin();
	fprintf(stderr, "This demo requires a color terminal\n");
	ExitProgram(EXIT_FAILURE);
    }

    start_color();

    do_pair_content(sp, 0);

    printw("Initializing pair 1 to red/black\n");
    do_init_pair(sp, 1, COLOR_RED, COLOR_BLACK);
    do_color_set("RED/BLACK", 1);

    printw("Initializing pair 2 to white/blue\n");
    do_init_pair(sp, 2, COLOR_WHITE, COLOR_BLUE);
    do_color_set("WHITE/BLUE", 2);

    printw("Initializing pair 3 to green/black\n");
    do_init_pair(sp, 3, COLOR_GREEN, COLOR_BLACK);
    do_color_set("GREEN/BLACK", 3);

    printw("Resetting colors to pair 0\n");
    do_color_set("Default Colors", 0);

    printw("Resetting colors to pair 1\n");
    do_color_set("RED/BLACK", 1);

    printw("Drawing soft-key tabs with pair 2\n");
    slk_attrset(A_BOLD);	/* reverse-video is hard to see */
    (void) if_opt_s(extended_slk_color_sp(sp, 2),
		    extended_slk_color(2));
    for (i = 1; i <= 8; ++i) {
	char temp[80];
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "(SLK-%d)", i);
	slk_set(i, temp, 0);
    }
    slk_touch();
    slk_noutrefresh();

    i = if_opt_s(can_change_color_sp(sp),
		 can_change_color());
    if (i) {
	do_color_set("Default Colors", 0);
	printw("Press any key to stop...\n");
	nodelay(stdscr, TRUE);
	while (getch() == ERR) {
	    show_rgb(sp);
	    do_init_color(sp, COLOR_RED, 1);
	    do_init_color(sp, COLOR_BLUE, -1);
	    napms(50);
	}
	printw("...done");
	nodelay(stdscr, FALSE);
	getch();
    }

    endwin();

    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the ncurses extended color/pair functions\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
