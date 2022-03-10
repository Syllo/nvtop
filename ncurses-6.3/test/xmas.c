/******************************************************************************/
/* asciixmas                                                                  */
/* December 1989             Larry Bartz           Indianapolis, IN           */
/*                                                                            */
/*                                                                            */
/* I'm dreaming of an ascii character-based monochrome Christmas,             */
/* Just like the one's I used to know!                                        */
/* Via a full duplex communications channel,                                  */
/* At 9600 bits per second,                                                   */
/* Even though it's kinda slow.                                               */
/*                                                                            */
/* I'm dreaming of an ascii character-based monochrome Christmas,             */
/* With ev'ry C program I write!                                              */
/* May your screen be merry and bright!                                       */
/* And may all your Christmases be amber or green,                            */
/* (for reduced eyestrain and improved visibility)!                           */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* IMPLEMENTATION                                                             */
/*                                                                            */
/* Feel free to modify the defined string FROMWHO to reflect you, your        */
/* organization, your site, whatever.                                         */
/*                                                                            */
/* This really looks a lot better if you can turn off your cursor before      */
/* execution. I wanted to do that here but very few termcap entries or        */
/* terminfo definitions have the appropriate string defined. If you know      */
/* the string(s) for the terminal(s) you use or which your site supports,     */
/* you could call asciixmas from within a shell in which you issue the        */
/* string to the terminal. The cursor is distracting but it doesn't really    */
/* ruin the show.                                                             */
/*                                                                            */
/* At our site, we invoke this for our users just after login and the         */
/* determination of terminal type.                                            */
/*                                                                            */
/*                                                                            */
/* PORTABILITY                                                                */
/*                                                                            */
/* I wrote this using only the very simplest curses functions so that it      */
/* might be the most portable. I was personally able to test on five          */
/* different cpu/UNIX combinations.                                           */
/*                                                                            */
/*                                                                            */
/* COMPILE                                                                    */
/*                                                                            */
/* usually this:                                                              */
/*                                                                            */
/* cc -O asciixmas.c -lcurses -o asciixmas -s                                 */
/*                                                                            */
/*                                                                            */
/* Zilog S8000 models 11, 21, 31, etc with ZEUS variant of SYSTEM III         */
/* maybe other SYSTEM III also:                                               */
/*                                                                            */
/* cc asciixmas.c -lcurses -ltermlib -o asciixmas -s                          */
/*                                                                            */
/* as above with optional "peephole optimizer" installed:                     */
/*                                                                            */
/* cc -O asciixmas.c -lcurses -ltermlib -o asciixmas -s                       */
/*                                                                            */
/*                                                                            */
/* Zilog S8000 models 32, 130 with WE32100 chip and SYS V, REL2               */
/* maybe 3B2 also?                                                            */
/*                                                                            */
/* cc -f -O -K sd asciixmas.c -lcurses -o asciixmas -s                        */
/*                                                                            */
/*                                                                            */
/* Pyramid, Sequent, any other "dual universe" types compile and execute      */
/* under either universe. The compile line for the ucb universe (as you       */
/* might expect) is the same as for SYS III UNIX:                             */
/*                                                                            */
/* cc -O asciixmas.c -lcurses -ltermlib -o asciixmas -s                       */
/*                                                                            */
/* The above compile will also hold true for other BSD systems. (I hope)      */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/* For the Scrooges out there among you who don't want this thing to loop     */
/* forever (or until the user hits a key), insert this into your compile      */
/* line just after "cc" :                                                     */
/*                                                                            */
/* -DNOLOOP                                                                   */
/*                                                                            */
/* like so:                                                                   */
/*                                                                            */
/* cc -DNOLOOP -O asciixmas.c -lcurses -o asciixmas -s                        */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/

/*
 * $Id: xmas.c,v 1.36 2021/03/27 22:40:53 tom Exp $
 */
#include <test.priv.h>

#define FROMWHO "Mark Hessling - (M.Hessling@gu.edu.au)"

static int my_bg = COLOR_BLACK;
static int y_pos, x_pos;

static WINDOW *treescrn;
static WINDOW *treescrn2;
static WINDOW *treescrn3;
static WINDOW *treescrn4;
static WINDOW *treescrn5;
static WINDOW *treescrn6;
static WINDOW *treescrn7;
static WINDOW *treescrn8;
static WINDOW *dotdeer0;
static WINDOW *stardeer0;
static WINDOW *lildeer0;
static WINDOW *lildeer1;
static WINDOW *lildeer2;
static WINDOW *lildeer3;
static WINDOW *middeer0;
static WINDOW *middeer1;
static WINDOW *middeer2;
static WINDOW *middeer3;
static WINDOW *bigdeer0;
static WINDOW *bigdeer1;
static WINDOW *bigdeer2;
static WINDOW *bigdeer3;
static WINDOW *bigdeer4;
static WINDOW *lookdeer0;
static WINDOW *lookdeer1;
static WINDOW *lookdeer2;
static WINDOW *lookdeer3;
static WINDOW *lookdeer4;
static WINDOW *w_holiday;
static WINDOW *w_del_msg;
static bool *my_pairs;

static GCC_NORETURN void done(int sig);

static void
set_color(WINDOW *win, chtype color)
{
    if (has_colors()) {
	int n = (int) (color + 1);
	if (my_pairs == 0)
	    my_pairs = typeCalloc(bool, (size_t) (COLORS + 1));
	if (!my_pairs[n]) {
	    init_pair((short) n, (short) color, (short) my_bg);
	    my_pairs[n] = TRUE;
	}
	wattroff(win, A_COLOR);
	wattron(win, COLOR_PAIR(n));
    }
}

static void
unset_color(WINDOW *win)
{
    if (has_colors())
	(void) wattrset(win, COLOR_PAIR(0));
}

static void
look_out(int msecs)
{
    napms(msecs);
    if (getch() != ERR) {
	beep();
	done(0);
    }
}

static int
boxit(void)
{
    int x = 0;

    while (x < 20) {
	MvAddCh(x, 7, '|');
	++x;
    }

    x = 8;

    while (x < 80) {
	MvAddCh(19, x, '_');
	++x;
    }

    x = 0;

    while (x < 80) {
	MvAddCh(22, x, '_');
	++x;
    }

    return (0);
}

static int
seas(void)
{
    MvAddCh(4, 1, 'S');
    MvAddCh(6, 1, 'E');
    MvAddCh(8, 1, 'A');
    MvAddCh(10, 1, 'S');
    MvAddCh(12, 1, 'O');
    MvAddCh(14, 1, 'N');
    MvAddCh(16, 1, '`');
    MvAddCh(18, 1, 'S');

    return (0);
}

static int
greet(void)
{
    MvAddCh(3, 5, 'G');
    MvAddCh(5, 5, 'R');
    MvAddCh(7, 5, 'E');
    MvAddCh(9, 5, 'E');
    MvAddCh(11, 5, 'T');
    MvAddCh(13, 5, 'I');
    MvAddCh(15, 5, 'N');
    MvAddCh(17, 5, 'G');
    MvAddCh(19, 5, 'S');

    return (0);
}

static int
fromwho(void)
{
    MvAddStr(21, 13, FROMWHO);
    return (0);
}

static int
tree(void)
{
    set_color(treescrn, COLOR_GREEN);
    MvWAddCh(treescrn, 1, 11, (chtype) '/');
    MvWAddCh(treescrn, 2, 11, (chtype) '/');
    MvWAddCh(treescrn, 3, 10, (chtype) '/');
    MvWAddCh(treescrn, 4, 9, (chtype) '/');
    MvWAddCh(treescrn, 5, 9, (chtype) '/');
    MvWAddCh(treescrn, 6, 8, (chtype) '/');
    MvWAddCh(treescrn, 7, 7, (chtype) '/');
    MvWAddCh(treescrn, 8, 6, (chtype) '/');
    MvWAddCh(treescrn, 9, 6, (chtype) '/');
    MvWAddCh(treescrn, 10, 5, (chtype) '/');
    MvWAddCh(treescrn, 11, 3, (chtype) '/');
    MvWAddCh(treescrn, 12, 2, (chtype) '/');

    MvWAddCh(treescrn, 1, 13, (chtype) '\\');
    MvWAddCh(treescrn, 2, 13, (chtype) '\\');
    MvWAddCh(treescrn, 3, 14, (chtype) '\\');
    MvWAddCh(treescrn, 4, 15, (chtype) '\\');
    MvWAddCh(treescrn, 5, 15, (chtype) '\\');
    MvWAddCh(treescrn, 6, 16, (chtype) '\\');
    MvWAddCh(treescrn, 7, 17, (chtype) '\\');
    MvWAddCh(treescrn, 8, 18, (chtype) '\\');
    MvWAddCh(treescrn, 9, 18, (chtype) '\\');
    MvWAddCh(treescrn, 10, 19, (chtype) '\\');
    MvWAddCh(treescrn, 11, 21, (chtype) '\\');
    MvWAddCh(treescrn, 12, 22, (chtype) '\\');

    MvWAddCh(treescrn, 4, 10, (chtype) '_');
    MvWAddCh(treescrn, 4, 14, (chtype) '_');
    MvWAddCh(treescrn, 8, 7, (chtype) '_');
    MvWAddCh(treescrn, 8, 17, (chtype) '_');

    MvWAddStr(treescrn, 13, 0, "//////////// \\\\\\\\\\\\\\\\\\\\\\\\");

    MvWAddStr(treescrn, 14, 11, "| |");
    MvWAddStr(treescrn, 15, 11, "|_|");

    unset_color(treescrn);
    wrefresh(treescrn);
    wrefresh(w_del_msg);

    return (0);
}

static int
balls(void)
{
    overlay(treescrn, treescrn2);

    set_color(treescrn2, COLOR_BLUE);
    MvWAddCh(treescrn2, 3, 9, (chtype) '@');
    MvWAddCh(treescrn2, 3, 15, (chtype) '@');
    MvWAddCh(treescrn2, 4, 8, (chtype) '@');
    MvWAddCh(treescrn2, 4, 16, (chtype) '@');
    MvWAddCh(treescrn2, 5, 7, (chtype) '@');
    MvWAddCh(treescrn2, 5, 17, (chtype) '@');
    MvWAddCh(treescrn2, 7, 6, (chtype) '@');
    MvWAddCh(treescrn2, 7, 18, (chtype) '@');
    MvWAddCh(treescrn2, 8, 5, (chtype) '@');
    MvWAddCh(treescrn2, 8, 19, (chtype) '@');
    MvWAddCh(treescrn2, 10, 4, (chtype) '@');
    MvWAddCh(treescrn2, 10, 20, (chtype) '@');
    MvWAddCh(treescrn2, 11, 2, (chtype) '@');
    MvWAddCh(treescrn2, 11, 22, (chtype) '@');
    MvWAddCh(treescrn2, 12, 1, (chtype) '@');
    MvWAddCh(treescrn2, 12, 23, (chtype) '@');

    unset_color(treescrn2);
    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
star(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_YELLOW);

    MvWAddCh(treescrn2, 0, 12, (chtype) '*');
    (void) wstandend(treescrn2);

    unset_color(treescrn2);
    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
strng1(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_WHITE);

    MvWAddCh(treescrn2, 3, 13, (chtype) '\'');
    MvWAddCh(treescrn2, 3, 12, (chtype) ':');
    MvWAddCh(treescrn2, 3, 11, (chtype) '.');

    wattroff(treescrn2, A_BOLD | A_BLINK);
    unset_color(treescrn2);

    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
strng2(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_WHITE);

    MvWAddCh(treescrn2, 5, 14, (chtype) '\'');
    MvWAddCh(treescrn2, 5, 13, (chtype) ':');
    MvWAddCh(treescrn2, 5, 12, (chtype) '.');
    MvWAddCh(treescrn2, 5, 11, (chtype) ',');
    MvWAddCh(treescrn2, 6, 10, (chtype) '\'');
    MvWAddCh(treescrn2, 6, 9, (chtype) ':');

    wattroff(treescrn2, A_BOLD | A_BLINK);
    unset_color(treescrn2);

    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
strng3(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_WHITE);

    MvWAddCh(treescrn2, 7, 16, (chtype) '\'');
    MvWAddCh(treescrn2, 7, 15, (chtype) ':');
    MvWAddCh(treescrn2, 7, 14, (chtype) '.');
    MvWAddCh(treescrn2, 7, 13, (chtype) ',');
    MvWAddCh(treescrn2, 8, 12, (chtype) '\'');
    MvWAddCh(treescrn2, 8, 11, (chtype) ':');
    MvWAddCh(treescrn2, 8, 10, (chtype) '.');
    MvWAddCh(treescrn2, 8, 9, (chtype) ',');

    wattroff(treescrn2, A_BOLD | A_BLINK);
    unset_color(treescrn2);

    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
strng4(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_WHITE);

    MvWAddCh(treescrn2, 9, 17, (chtype) '\'');
    MvWAddCh(treescrn2, 9, 16, (chtype) ':');
    MvWAddCh(treescrn2, 9, 15, (chtype) '.');
    MvWAddCh(treescrn2, 9, 14, (chtype) ',');
    MvWAddCh(treescrn2, 10, 13, (chtype) '\'');
    MvWAddCh(treescrn2, 10, 12, (chtype) ':');
    MvWAddCh(treescrn2, 10, 11, (chtype) '.');
    MvWAddCh(treescrn2, 10, 10, (chtype) ',');
    MvWAddCh(treescrn2, 11, 9, (chtype) '\'');
    MvWAddCh(treescrn2, 11, 8, (chtype) ':');
    MvWAddCh(treescrn2, 11, 7, (chtype) '.');
    MvWAddCh(treescrn2, 11, 6, (chtype) ',');
    MvWAddCh(treescrn2, 12, 5, (chtype) '\'');

    wattroff(treescrn2, A_BOLD | A_BLINK);
    unset_color(treescrn2);

    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
strng5(void)
{
    (void) wattrset(treescrn2, A_BOLD | A_BLINK);
    set_color(treescrn2, COLOR_WHITE);

    MvWAddCh(treescrn2, 11, 19, (chtype) '\'');
    MvWAddCh(treescrn2, 11, 18, (chtype) ':');
    MvWAddCh(treescrn2, 11, 17, (chtype) '.');
    MvWAddCh(treescrn2, 11, 16, (chtype) ',');
    MvWAddCh(treescrn2, 12, 15, (chtype) '\'');
    MvWAddCh(treescrn2, 12, 14, (chtype) ':');
    MvWAddCh(treescrn2, 12, 13, (chtype) '.');
    MvWAddCh(treescrn2, 12, 12, (chtype) ',');

    wattroff(treescrn2, A_BOLD | A_BLINK);
    unset_color(treescrn2);

    /* save a fully lit tree */
    overlay(treescrn2, treescrn);

    wrefresh(treescrn2);
    wrefresh(w_del_msg);
    return (0);
}

static int
blinkit(void)
{
    static int cycle;

    if (cycle > 4) {
	cycle = 0;
    }

    touchwin(treescrn8);

    switch (cycle) {
    case 0:
	overlay(treescrn3, treescrn8);
	break;
    case 1:
	overlay(treescrn4, treescrn8);
	break;
    case 2:
	overlay(treescrn5, treescrn8);
	break;
    case 3:
	overlay(treescrn6, treescrn8);
	break;
    case 4:
	overlay(treescrn7, treescrn8);
	break;
    }
    touchwin(treescrn8);
    wrefresh(treescrn8);
    wrefresh(w_del_msg);
    look_out(50);

    /*ALL ON************************************************** */

    overlay(treescrn, treescrn8);
    wrefresh(treescrn8);
    wrefresh(w_del_msg);
    look_out(50);

    ++cycle;
    return (0);
}

static void
deer_step(WINDOW *win, int y, int x)
{
    mvwin(win, y, x);
    wrefresh(win);
    wrefresh(w_del_msg);
    look_out(5);
}

static int
reindeer(void)
{
    int looper;
    y_pos = 0;

    for (x_pos = 70; x_pos > 62; x_pos--) {
	for (looper = 0; looper < 4; looper++) {
	    MvWAddCh(dotdeer0, y_pos, x_pos, (chtype) '.');
	    wrefresh(dotdeer0);
	    wrefresh(w_del_msg);
	    werase(dotdeer0);
	    wrefresh(dotdeer0);
	    wrefresh(w_del_msg);
	    look_out(50);
	}
    }

    y_pos = 2;

    for (; x_pos > 50; x_pos--) {
	for (looper = 0; looper < 4; looper++) {

	    if (x_pos < 56) {
		y_pos = 3;

		MvWAddCh(stardeer0, y_pos, x_pos, (chtype) '*');
		wrefresh(stardeer0);
		wrefresh(w_del_msg);
		werase(stardeer0);
		wrefresh(stardeer0);
		wrefresh(w_del_msg);
	    } else {
		MvWAddCh(dotdeer0, y_pos, x_pos, (chtype) '*');
		wrefresh(dotdeer0);
		wrefresh(w_del_msg);
		werase(dotdeer0);
		wrefresh(dotdeer0);
		wrefresh(w_del_msg);
	    }
	}
    }

    x_pos = 58;

    for (y_pos = 2; y_pos < 5; y_pos++) {

	touchwin(lildeer0);
	wrefresh(lildeer0);
	wrefresh(w_del_msg);

	for (looper = 0; looper < 4; looper++) {
	    deer_step(lildeer3, y_pos, x_pos);
	    deer_step(lildeer2, y_pos, x_pos);
	    deer_step(lildeer1, y_pos, x_pos);
	    deer_step(lildeer2, y_pos, x_pos);
	    deer_step(lildeer3, y_pos, x_pos);

	    touchwin(lildeer0);
	    wrefresh(lildeer0);
	    wrefresh(w_del_msg);

	    x_pos -= 2;
	}
    }

    x_pos = 35;

    for (y_pos = 5; y_pos < 10; y_pos++) {

	touchwin(middeer0);
	wrefresh(middeer0);
	wrefresh(w_del_msg);

	for (looper = 0; looper < 2; looper++) {
	    deer_step(middeer3, y_pos, x_pos);
	    deer_step(middeer2, y_pos, x_pos);
	    deer_step(middeer1, y_pos, x_pos);
	    deer_step(middeer2, y_pos, x_pos);
	    deer_step(middeer3, y_pos, x_pos);

	    touchwin(middeer0);
	    wrefresh(middeer0);
	    wrefresh(w_del_msg);

	    x_pos -= 3;
	}
    }

    look_out(300);

    y_pos = 1;

    for (x_pos = 8; x_pos < 16; x_pos++) {
	deer_step(bigdeer4, y_pos, x_pos);
	deer_step(bigdeer3, y_pos, x_pos);
	deer_step(bigdeer2, y_pos, x_pos);
	deer_step(bigdeer1, y_pos, x_pos);
	deer_step(bigdeer2, y_pos, x_pos);
	deer_step(bigdeer3, y_pos, x_pos);
	deer_step(bigdeer4, y_pos, x_pos);
	deer_step(bigdeer0, y_pos, x_pos);
    }

    --x_pos;

    for (looper = 0; looper < 6; looper++) {
	deer_step(lookdeer4, y_pos, x_pos);
	deer_step(lookdeer3, y_pos, x_pos);
	deer_step(lookdeer2, y_pos, x_pos);
	deer_step(lookdeer1, y_pos, x_pos);
	deer_step(lookdeer2, y_pos, x_pos);
	deer_step(lookdeer3, y_pos, x_pos);
	deer_step(lookdeer4, y_pos, x_pos);
    }

    deer_step(lookdeer0, y_pos, x_pos);

    for (; y_pos < 10; y_pos++) {
	for (looper = 0; looper < 2; looper++) {
	    deer_step(bigdeer4, y_pos, x_pos);
	    deer_step(bigdeer3, y_pos, x_pos);
	    deer_step(bigdeer2, y_pos, x_pos);
	    deer_step(bigdeer1, y_pos, x_pos);
	    deer_step(bigdeer2, y_pos, x_pos);
	    deer_step(bigdeer3, y_pos, x_pos);
	    deer_step(bigdeer4, y_pos, x_pos);
	}
	deer_step(bigdeer0, y_pos, x_pos);
    }

    --y_pos;

    deer_step(lookdeer3, y_pos, x_pos);
    return (0);
}

static void
done(int sig GCC_UNUSED)
{
    move(LINES - 1, 0);
    refresh();
    stop_curses();

#if NO_LEAKS
    if (my_pairs != 0)
	free(my_pairs);
#endif

    ExitProgram(EXIT_SUCCESS);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: xmas [options]"
	,""
	,"Options:"
#if HAVE_USE_DEFAULT_COLORS
	," -d       invoke use_default_colors"
#endif
	," -q       execute once, then quit"
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
    int loopy;
#if HAVE_USE_DEFAULT_COLORS
    bool opt_d = FALSE;
#endif
    bool opt_q = FALSE;

    while ((ch = getopt(argc, argv, "dq")) != -1) {
	switch (ch) {
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    opt_d = TRUE;
	    break;
#endif
	case 'q':
	    opt_q = TRUE;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }

    setlocale(LC_ALL, "");

    InitAndCatch(initscr(), done);
    noecho();
    nonl();
    refresh();

    if (has_colors()) {
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (opt_d && (use_default_colors() == OK))
	    my_bg = -1;
#endif
    }
    curs_set(0);

    if ((treescrn = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn2 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn3 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn4 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn5 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn6 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn7 = newwin(16, 27, 3, 53)) == 0 ||
	(treescrn8 = newwin(16, 27, 3, 53)) == 0 ||

	(dotdeer0 = newwin(3, 71, 0, 8)) == 0 ||

	(stardeer0 = newwin(4, 56, 0, 8)) == 0 ||

	(lildeer0 = newwin(7, 53, 0, 8)) == 0 ||
	(lildeer1 = newwin(2, 4, 0, 0)) == 0 ||
	(lildeer2 = newwin(2, 4, 0, 0)) == 0 ||
	(lildeer3 = newwin(2, 4, 0, 0)) == 0 ||

	(middeer0 = newwin(15, 42, 0, 8)) == 0 ||
	(middeer1 = newwin(3, 7, 0, 0)) == 0 ||
	(middeer2 = newwin(3, 7, 0, 0)) == 0 ||
	(middeer3 = newwin(3, 7, 0, 0)) == 0 ||

	(bigdeer0 = newwin(10, 23, 0, 0)) == 0 ||
	(bigdeer1 = newwin(10, 23, 0, 0)) == 0 ||
	(bigdeer2 = newwin(10, 23, 0, 0)) == 0 ||
	(bigdeer3 = newwin(10, 23, 0, 0)) == 0 ||
	(bigdeer4 = newwin(10, 23, 0, 0)) == 0 ||

	(lookdeer0 = newwin(10, 25, 0, 0)) == 0 ||
	(lookdeer1 = newwin(10, 25, 0, 0)) == 0 ||
	(lookdeer2 = newwin(10, 25, 0, 0)) == 0 ||
	(lookdeer3 = newwin(10, 25, 0, 0)) == 0 ||
	(lookdeer4 = newwin(10, 25, 0, 0)) == 0 ||

	(w_holiday = newwin(1, 26, 3, 27)) == 0 ||

	(w_del_msg = newwin(1, 19, 23, 60)) == 0) {
	stop_curses();
	fprintf(stderr, "Cannot create windows - screen too small\n");
	ExitProgram(EXIT_FAILURE);
    }

    MvWAddStr(w_del_msg, 0, 0, "Hit any key to quit");

    MvWAddStr(w_holiday, 0, 0, "H A P P Y  H O L I D A Y S");

    /* set up the windows for our various reindeer */

    /* lildeer1 */
    MvWAddCh(lildeer1, 0, 0, (chtype) 'V');
    MvWAddCh(lildeer1, 1, 0, (chtype) '@');
    MvWAddCh(lildeer1, 1, 1, (chtype) '<');
    MvWAddCh(lildeer1, 1, 2, (chtype) '>');
    MvWAddCh(lildeer1, 1, 3, (chtype) '~');

    /* lildeer2 */
    MvWAddCh(lildeer2, 0, 0, (chtype) 'V');
    MvWAddCh(lildeer2, 1, 0, (chtype) '@');
    MvWAddCh(lildeer2, 1, 1, (chtype) '|');
    MvWAddCh(lildeer2, 1, 2, (chtype) '|');
    MvWAddCh(lildeer2, 1, 3, (chtype) '~');

    /* lildeer3 */
    MvWAddCh(lildeer3, 0, 0, (chtype) 'V');
    MvWAddCh(lildeer3, 1, 0, (chtype) '@');
    MvWAddCh(lildeer3, 1, 1, (chtype) '>');
    MvWAddCh(lildeer3, 1, 2, (chtype) '<');
    MvWAddCh(lildeer2, 1, 3, (chtype) '~');

    /* middeer1 */
    MvWAddCh(middeer1, 0, 2, (chtype) 'y');
    MvWAddCh(middeer1, 0, 3, (chtype) 'y');
    MvWAddCh(middeer1, 1, 2, (chtype) '0');
    MvWAddCh(middeer1, 1, 3, (chtype) '(');
    MvWAddCh(middeer1, 1, 4, (chtype) '=');
    MvWAddCh(middeer1, 1, 5, (chtype) ')');
    MvWAddCh(middeer1, 1, 6, (chtype) '~');
    MvWAddCh(middeer1, 2, 3, (chtype) '\\');
    MvWAddCh(middeer1, 2, 4, (chtype) '/');

    /* middeer2 */
    MvWAddCh(middeer2, 0, 2, (chtype) 'y');
    MvWAddCh(middeer2, 0, 3, (chtype) 'y');
    MvWAddCh(middeer2, 1, 2, (chtype) '0');
    MvWAddCh(middeer2, 1, 3, (chtype) '(');
    MvWAddCh(middeer2, 1, 4, (chtype) '=');
    MvWAddCh(middeer2, 1, 5, (chtype) ')');
    MvWAddCh(middeer2, 1, 6, (chtype) '~');
    MvWAddCh(middeer2, 2, 3, (chtype) '|');
    MvWAddCh(middeer2, 2, 5, (chtype) '|');

    /* middeer3 */
    MvWAddCh(middeer3, 0, 2, (chtype) 'y');
    MvWAddCh(middeer3, 0, 3, (chtype) 'y');
    MvWAddCh(middeer3, 1, 2, (chtype) '0');
    MvWAddCh(middeer3, 1, 3, (chtype) '(');
    MvWAddCh(middeer3, 1, 4, (chtype) '=');
    MvWAddCh(middeer3, 1, 5, (chtype) ')');
    MvWAddCh(middeer3, 1, 6, (chtype) '~');
    MvWAddCh(middeer3, 2, 2, (chtype) '/');
    MvWAddCh(middeer3, 2, 6, (chtype) '\\');

    /* bigdeer1 */
    MvWAddCh(bigdeer1, 0, 17, (chtype) '\\');
    MvWAddCh(bigdeer1, 0, 18, (chtype) '/');
    MvWAddCh(bigdeer1, 0, 20, (chtype) '\\');
    MvWAddCh(bigdeer1, 0, 21, (chtype) '/');
    MvWAddCh(bigdeer1, 1, 18, (chtype) '\\');
    MvWAddCh(bigdeer1, 1, 20, (chtype) '/');
    MvWAddCh(bigdeer1, 2, 19, (chtype) '|');
    MvWAddCh(bigdeer1, 2, 20, (chtype) '_');
    MvWAddCh(bigdeer1, 3, 18, (chtype) '/');
    MvWAddCh(bigdeer1, 3, 19, (chtype) '^');
    MvWAddCh(bigdeer1, 3, 20, (chtype) '0');
    MvWAddCh(bigdeer1, 3, 21, (chtype) '\\');
    MvWAddCh(bigdeer1, 4, 17, (chtype) '/');
    MvWAddCh(bigdeer1, 4, 18, (chtype) '/');
    MvWAddCh(bigdeer1, 4, 19, (chtype) '\\');
    MvWAddCh(bigdeer1, 4, 22, (chtype) '\\');
    MvWAddStr(bigdeer1, 5, 7, "^~~~~~~~~//  ~~U");
    MvWAddStr(bigdeer1, 6, 7, "( \\_____( /");
    MvWAddStr(bigdeer1, 7, 8, "( )    /");
    MvWAddStr(bigdeer1, 8, 9, "\\\\   /");
    MvWAddStr(bigdeer1, 9, 11, "\\>/>");

    /* bigdeer2 */
    MvWAddCh(bigdeer2, 0, 17, (chtype) '\\');
    MvWAddCh(bigdeer2, 0, 18, (chtype) '/');
    MvWAddCh(bigdeer2, 0, 20, (chtype) '\\');
    MvWAddCh(bigdeer2, 0, 21, (chtype) '/');
    MvWAddCh(bigdeer2, 1, 18, (chtype) '\\');
    MvWAddCh(bigdeer2, 1, 20, (chtype) '/');
    MvWAddCh(bigdeer2, 2, 19, (chtype) '|');
    MvWAddCh(bigdeer2, 2, 20, (chtype) '_');
    MvWAddCh(bigdeer2, 3, 18, (chtype) '/');
    MvWAddCh(bigdeer2, 3, 19, (chtype) '^');
    MvWAddCh(bigdeer2, 3, 20, (chtype) '0');
    MvWAddCh(bigdeer2, 3, 21, (chtype) '\\');
    MvWAddCh(bigdeer2, 4, 17, (chtype) '/');
    MvWAddCh(bigdeer2, 4, 18, (chtype) '/');
    MvWAddCh(bigdeer2, 4, 19, (chtype) '\\');
    MvWAddCh(bigdeer2, 4, 22, (chtype) '\\');
    MvWAddStr(bigdeer2, 5, 7, "^~~~~~~~~//  ~~U");
    MvWAddStr(bigdeer2, 6, 7, "(( )____( /");
    MvWAddStr(bigdeer2, 7, 7, "( /      |");
    MvWAddStr(bigdeer2, 8, 8, "\\/      |");
    MvWAddStr(bigdeer2, 9, 9, "|>     |>");

    /* bigdeer3 */
    MvWAddCh(bigdeer3, 0, 17, (chtype) '\\');
    MvWAddCh(bigdeer3, 0, 18, (chtype) '/');
    MvWAddCh(bigdeer3, 0, 20, (chtype) '\\');
    MvWAddCh(bigdeer3, 0, 21, (chtype) '/');
    MvWAddCh(bigdeer3, 1, 18, (chtype) '\\');
    MvWAddCh(bigdeer3, 1, 20, (chtype) '/');
    MvWAddCh(bigdeer3, 2, 19, (chtype) '|');
    MvWAddCh(bigdeer3, 2, 20, (chtype) '_');
    MvWAddCh(bigdeer3, 3, 18, (chtype) '/');
    MvWAddCh(bigdeer3, 3, 19, (chtype) '^');
    MvWAddCh(bigdeer3, 3, 20, (chtype) '0');
    MvWAddCh(bigdeer3, 3, 21, (chtype) '\\');
    MvWAddCh(bigdeer3, 4, 17, (chtype) '/');
    MvWAddCh(bigdeer3, 4, 18, (chtype) '/');
    MvWAddCh(bigdeer3, 4, 19, (chtype) '\\');
    MvWAddCh(bigdeer3, 4, 22, (chtype) '\\');
    MvWAddStr(bigdeer3, 5, 7, "^~~~~~~~~//  ~~U");
    MvWAddStr(bigdeer3, 6, 6, "( ()_____( /");
    MvWAddStr(bigdeer3, 7, 6, "/ /       /");
    MvWAddStr(bigdeer3, 8, 5, "|/          \\");
    MvWAddStr(bigdeer3, 9, 5, "/>           \\>");

    /* bigdeer4 */
    MvWAddCh(bigdeer4, 0, 17, (chtype) '\\');
    MvWAddCh(bigdeer4, 0, 18, (chtype) '/');
    MvWAddCh(bigdeer4, 0, 20, (chtype) '\\');
    MvWAddCh(bigdeer4, 0, 21, (chtype) '/');
    MvWAddCh(bigdeer4, 1, 18, (chtype) '\\');
    MvWAddCh(bigdeer4, 1, 20, (chtype) '/');
    MvWAddCh(bigdeer4, 2, 19, (chtype) '|');
    MvWAddCh(bigdeer4, 2, 20, (chtype) '_');
    MvWAddCh(bigdeer4, 3, 18, (chtype) '/');
    MvWAddCh(bigdeer4, 3, 19, (chtype) '^');
    MvWAddCh(bigdeer4, 3, 20, (chtype) '0');
    MvWAddCh(bigdeer4, 3, 21, (chtype) '\\');
    MvWAddCh(bigdeer4, 4, 17, (chtype) '/');
    MvWAddCh(bigdeer4, 4, 18, (chtype) '/');
    MvWAddCh(bigdeer4, 4, 19, (chtype) '\\');
    MvWAddCh(bigdeer4, 4, 22, (chtype) '\\');
    MvWAddStr(bigdeer4, 5, 7, "^~~~~~~~~//  ~~U");
    MvWAddStr(bigdeer4, 6, 6, "( )______( /");
    MvWAddStr(bigdeer4, 7, 5, "(/          \\");
    MvWAddStr(bigdeer4, 8, 0, "v___=             ----^");

    /* lookdeer1 */
    MvWAddStr(lookdeer1, 0, 16, "\\/     \\/");
    MvWAddStr(lookdeer1, 1, 17, "\\Y/ \\Y/");
    MvWAddStr(lookdeer1, 2, 19, "\\=/");
    MvWAddStr(lookdeer1, 3, 17, "^\\o o/^");
    MvWAddStr(lookdeer1, 4, 17, "//( )");
    MvWAddStr(lookdeer1, 5, 7, "^~~~~~~~~// \\O/");
    MvWAddStr(lookdeer1, 6, 7, "( \\_____( /");
    MvWAddStr(lookdeer1, 7, 8, "( )    /");
    MvWAddStr(lookdeer1, 8, 9, "\\\\   /");
    MvWAddStr(lookdeer1, 9, 11, "\\>/>");

    /* lookdeer2 */
    MvWAddStr(lookdeer2, 0, 16, "\\/     \\/");
    MvWAddStr(lookdeer2, 1, 17, "\\Y/ \\Y/");
    MvWAddStr(lookdeer2, 2, 19, "\\=/");
    MvWAddStr(lookdeer2, 3, 17, "^\\o o/^");
    MvWAddStr(lookdeer2, 4, 17, "//( )");
    MvWAddStr(lookdeer2, 5, 7, "^~~~~~~~~// \\O/");
    MvWAddStr(lookdeer2, 6, 7, "(( )____( /");
    MvWAddStr(lookdeer2, 7, 7, "( /      |");
    MvWAddStr(lookdeer2, 8, 8, "\\/      |");
    MvWAddStr(lookdeer2, 9, 9, "|>     |>");

    /* lookdeer3 */
    MvWAddStr(lookdeer3, 0, 16, "\\/     \\/");
    MvWAddStr(lookdeer3, 1, 17, "\\Y/ \\Y/");
    MvWAddStr(lookdeer3, 2, 19, "\\=/");
    MvWAddStr(lookdeer3, 3, 17, "^\\o o/^");
    MvWAddStr(lookdeer3, 4, 17, "//( )");
    MvWAddStr(lookdeer3, 5, 7, "^~~~~~~~~// \\O/");
    MvWAddStr(lookdeer3, 6, 6, "( ()_____( /");
    MvWAddStr(lookdeer3, 7, 6, "/ /       /");
    MvWAddStr(lookdeer3, 8, 5, "|/          \\");
    MvWAddStr(lookdeer3, 9, 5, "/>           \\>");

    /* lookdeer4 */
    MvWAddStr(lookdeer4, 0, 16, "\\/     \\/");
    MvWAddStr(lookdeer4, 1, 17, "\\Y/ \\Y/");
    MvWAddStr(lookdeer4, 2, 19, "\\=/");
    MvWAddStr(lookdeer4, 3, 17, "^\\o o/^");
    MvWAddStr(lookdeer4, 4, 17, "//( )");
    MvWAddStr(lookdeer4, 5, 7, "^~~~~~~~~// \\O/");
    MvWAddStr(lookdeer4, 6, 6, "( )______( /");
    MvWAddStr(lookdeer4, 7, 5, "(/          \\");
    MvWAddStr(lookdeer4, 8, 0, "v___=             ----^");

	/***********************************************/
    cbreak();
    nodelay(stdscr, TRUE);
    do {
	clear();
	werase(treescrn);
	touchwin(w_del_msg);
	touchwin(treescrn);
	werase(treescrn2);
	touchwin(treescrn2);
	werase(treescrn8);
	touchwin(treescrn8);
	refresh();
	look_out(150);
	boxit();
	refresh();
	look_out(150);
	seas();
	refresh();
	look_out(150);
	greet();
	refresh();
	look_out(150);
	fromwho();
	refresh();
	look_out(150);
	tree();
	look_out(150);
	balls();
	look_out(150);
	star();
	look_out(150);
	strng1();
	strng2();
	strng3();
	strng4();
	strng5();

	/* set up the windows for our blinking trees */
	/* **************************************** */
	/* treescrn3 */

	overlay(treescrn, treescrn3);

	/*balls */
	MvWAddCh(treescrn3, 4, 18, ' ');
	MvWAddCh(treescrn3, 7, 6, ' ');
	MvWAddCh(treescrn3, 8, 19, ' ');
	MvWAddCh(treescrn3, 11, 22, ' ');

	/*star */
	MvWAddCh(treescrn3, 0, 12, '*');

	/*strng1 */
	MvWAddCh(treescrn3, 3, 11, ' ');

	/*strng2 */
	MvWAddCh(treescrn3, 5, 13, ' ');
	MvWAddCh(treescrn3, 6, 10, ' ');

	/*strng3 */
	MvWAddCh(treescrn3, 7, 16, ' ');
	MvWAddCh(treescrn3, 7, 14, ' ');

	/*strng4 */
	MvWAddCh(treescrn3, 10, 13, ' ');
	MvWAddCh(treescrn3, 10, 10, ' ');
	MvWAddCh(treescrn3, 11, 8, ' ');

	/*strng5 */
	MvWAddCh(treescrn3, 11, 18, ' ');
	MvWAddCh(treescrn3, 12, 13, ' ');

	/* treescrn4 */

	overlay(treescrn, treescrn4);

	/*balls */
	MvWAddCh(treescrn4, 3, 9, ' ');
	MvWAddCh(treescrn4, 4, 16, ' ');
	MvWAddCh(treescrn4, 7, 6, ' ');
	MvWAddCh(treescrn4, 8, 19, ' ');
	MvWAddCh(treescrn4, 11, 2, ' ');
	MvWAddCh(treescrn4, 12, 23, ' ');

	/*star */
	(void) wstandout(treescrn4);
	MvWAddCh(treescrn4, 0, 12, '*');
	(void) wstandend(treescrn4);

	/*strng1 */
	MvWAddCh(treescrn4, 3, 13, ' ');

	/*strng2 */

	/*strng3 */
	MvWAddCh(treescrn4, 7, 15, ' ');
	MvWAddCh(treescrn4, 8, 11, ' ');

	/*strng4 */
	MvWAddCh(treescrn4, 9, 16, ' ');
	MvWAddCh(treescrn4, 10, 12, ' ');
	MvWAddCh(treescrn4, 11, 8, ' ');

	/*strng5 */
	MvWAddCh(treescrn4, 11, 18, ' ');
	MvWAddCh(treescrn4, 12, 14, ' ');

	/* treescrn5 */

	overlay(treescrn, treescrn5);

	/*balls */
	MvWAddCh(treescrn5, 3, 15, ' ');
	MvWAddCh(treescrn5, 10, 20, ' ');
	MvWAddCh(treescrn5, 12, 1, ' ');

	/*star */
	MvWAddCh(treescrn5, 0, 12, '*');

	/*strng1 */
	MvWAddCh(treescrn5, 3, 11, ' ');

	/*strng2 */
	MvWAddCh(treescrn5, 5, 12, ' ');

	/*strng3 */
	MvWAddCh(treescrn5, 7, 14, ' ');
	MvWAddCh(treescrn5, 8, 10, ' ');

	/*strng4 */
	MvWAddCh(treescrn5, 9, 15, ' ');
	MvWAddCh(treescrn5, 10, 11, ' ');
	MvWAddCh(treescrn5, 11, 7, ' ');

	/*strng5 */
	MvWAddCh(treescrn5, 11, 17, ' ');
	MvWAddCh(treescrn5, 12, 13, ' ');

	/* treescrn6 */

	overlay(treescrn, treescrn6);

	/*balls */
	MvWAddCh(treescrn6, 6, 7, ' ');
	MvWAddCh(treescrn6, 7, 18, ' ');
	MvWAddCh(treescrn6, 10, 4, ' ');
	MvWAddCh(treescrn6, 11, 23, ' ');

	/*star */
	(void) wstandout(treescrn6);
	MvWAddCh(treescrn6, 0, 12, '*');
	(void) wstandend(treescrn6);

	/*strng1 */

	/*strng2 */
	MvWAddCh(treescrn6, 5, 11, ' ');

	/*strng3 */
	MvWAddCh(treescrn6, 7, 13, ' ');
	MvWAddCh(treescrn6, 8, 9, ' ');

	/*strng4 */
	MvWAddCh(treescrn6, 9, 14, ' ');
	MvWAddCh(treescrn6, 10, 10, ' ');
	MvWAddCh(treescrn6, 11, 6, ' ');

	/*strng5 */
	MvWAddCh(treescrn6, 11, 16, ' ');
	MvWAddCh(treescrn6, 12, 12, ' ');

	/* treescrn7 */

	overlay(treescrn, treescrn7);

	/*balls */
	MvWAddCh(treescrn7, 3, 15, ' ');
	MvWAddCh(treescrn7, 6, 7, ' ');
	MvWAddCh(treescrn7, 7, 18, ' ');
	MvWAddCh(treescrn7, 10, 4, ' ');
	MvWAddCh(treescrn7, 11, 22, ' ');

	/*star */
	MvWAddCh(treescrn7, 0, 12, '*');

	/*strng1 */
	MvWAddCh(treescrn7, 3, 12, ' ');

	/*strng2 */
	MvWAddCh(treescrn7, 5, 13, ' ');
	MvWAddCh(treescrn7, 6, 9, ' ');

	/*strng3 */
	MvWAddCh(treescrn7, 7, 15, ' ');
	MvWAddCh(treescrn7, 8, 11, ' ');

	/*strng4 */
	MvWAddCh(treescrn7, 9, 16, ' ');
	MvWAddCh(treescrn7, 10, 12, ' ');
	MvWAddCh(treescrn7, 11, 8, ' ');

	/*strng5 */
	MvWAddCh(treescrn7, 11, 18, ' ');
	MvWAddCh(treescrn7, 12, 14, ' ');

	look_out(150);
	reindeer();

	touchwin(w_holiday);
	wrefresh(w_holiday);
	wrefresh(w_del_msg);

	look_out(500);
	for (loopy = 0; loopy < 100; loopy++) {
	    blinkit();
	}
    } while (!opt_q);
    done(0);
}
