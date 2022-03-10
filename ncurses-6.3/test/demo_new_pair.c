/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
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
 * $Id: demo_new_pair.c,v 1.24 2021/02/21 01:24:06 tom Exp $
 *
 * Demonstrate the alloc_pair() function.
 */

#include <test.priv.h>
#include <time.h>
#include <popup_msg.h>

#if HAVE_ALLOC_PAIR && USE_WIDEC_SUPPORT

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#define MAX_BITS 8		/* all but A_ALTCHARSET */
#define MAX_ATTR ((1<<MAX_BITS)-1)

static bool
valid_cap(NCURSES_CONST char *name)
{
    char *value = tigetstr(name);
    return (value != 0 && value != (char *) -1) ? TRUE : FALSE;
}

static attr_t
next_attr(int now)
{
    static bool init = FALSE;
    static attr_t table[MAX_BITS * MAX_BITS];
    static int limit = 0;

    if (!init) {
	int j, k;
	attr_t bits[MAX_BITS];

	init = TRUE;
	bits[limit++] = WA_NORMAL;
	if (valid_cap("smso"))
	    bits[limit++] = WA_STANDOUT;
	if (valid_cap("smul"))
	    bits[limit++] = WA_UNDERLINE;
	if (valid_cap("rev"))
	    bits[limit++] = WA_REVERSE;
	if (valid_cap("blink"))
	    bits[limit++] = WA_BLINK;
	if (valid_cap("dim"))
	    bits[limit++] = WA_DIM;
	if (valid_cap("bold"))
	    bits[limit++] = WA_BOLD;
	for (j = 0; j < limit; ++j) {
	    for (k = 0; k < limit; ++k) {
		table[j * limit + k] = bits[j] | bits[k];
	    }
	}
    }
    return table[now % limit];
}

static void
our_content(int pair, int *fg, int *bg)
{
    pair %= COLOR_PAIRS;
    *fg = (pair / COLORS) % COLORS;
    *bg = (pair % COLORS);
}

static int
make_color(int now)
{
    int fg, bg;
    our_content(now, &fg, &bg);
    return alloc_pair(fg, bg);
}

static int
next_color(int now)
{
    int result = 0;
    if ((short) now > 0) {
	if (now < COLOR_PAIRS) {
	    int fg, bg;
	    our_content(now, &fg, &bg);
	    if (init_pair((short) now, (short) fg, (short) bg) != OK)
		now = ERR;
	} else {
	    now %= COLOR_PAIRS;
	}
	result = now;
    }
    return result;
}

static time_t
now(void)
{
    return time((time_t *) 0);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: demo_new_pair [options]",
	"",
	"Repeatedly print using all possible color combinations.",
	"",
	"Options:",
	" -g       use getcchar to check setcchar",
	" -i       use init_pair rather than alloc_pair",
	" -p       start in paged-mode",
	" -s       start in single-step mode",
	" -w       print a wide-character cell",
    };
    unsigned n;
    for (n = 0; n < SIZEOF(msg); ++n) {
	fprintf(stderr, "%s\n", msg[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

#define use_pages() \
	paged_mode = TRUE, single_mode = TRUE

#define use_single() \
	paged_mode = FALSE, single_mode = TRUE

#define update_modes() \
	    scrollok(stdscr, !paged_mode); \
	    nodelay(stdscr, !single_mode || paged_mode)

int
main(int argc, char *argv[])
{
    static const char *help[] =
    {
	"This program iterates over the possible color combinations,",
	"allocating or initializing color pairs.  For best results,",
	"choose screen-width dividing evenly into the number of colors,",
	"e.g.,",
	"",
	"  32x64,32x128  256 colors",
	"  24x44,24x88   88 colors",
	"  32x64,24x128  16 colors",
	"",
	"Keys:",
	"  c      toggle between coloring and de-coloring cells",
	"  p      show one page at a time",
	"  s      show one character at a time",
	" <space> display char/page without pausing",
	"  v/V    cycle through video attributes",
	"  w      toggle between \"#\" and a double-width equivalent",
	"  ?      print this screen (exit on any character).",
	"",
	"To exit this program, press ^Q, ^[ or \"q\".",
	0
    };

    bool done = FALSE;
    bool check_set = FALSE;
    bool clobber = FALSE;
    bool hascolor = FALSE;
    bool use_init = FALSE;
    bool use_wide = FALSE;
    bool paged_mode = FALSE;
    bool single_mode = FALSE;
    int video_mode = 0;
    int current;
    int ch;
    wchar_t wch[2];
    time_t start = now();
    long total_cells = 0;
    FILE *output = 0;

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "gipsw")) != -1) {
	switch (ch) {
	case 'g':
	    check_set = TRUE;
	    break;
	case 'i':
	    use_init = TRUE;
	    break;
	case 'p':
	    use_pages();
	    break;
	case 's':
	    use_single();
	    break;
	case 'w':
	    use_wide = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }

    if (isatty(fileno(stderr))) {
	output = stderr;
    } else if ((ch = open("/dev/tty", O_WRONLY)) >= 0) {
	output = fdopen(ch, "w");
    } else {
	fprintf(stderr, "cannot open terminal for output\n");
	ExitProgram(EXIT_FAILURE);
    }
    if (newterm(NULL, output, stdin) == 0) {
	fprintf(stderr, "Cannot initialize terminal\n");
	fclose(output);
	ExitProgram(EXIT_FAILURE);
    }
    (void) cbreak();		/* read chars without wait for \n */
    (void) noecho();		/* don't echo input */
    update_modes();
    curs_set(0);

    keypad(stdscr, TRUE);

    if ((hascolor = has_colors())) {
	start_color();
	current = 1;
    } else {
	current = 0;
    }

    /*
     * Repeatedly cycle through all colors, initializing pairs as needed.
     * Provide for single-stepping, or page-at-a-time, as well as quitting.
     */
    while (!done) {
	cchar_t temp;
	attr_t my_attrs;
	int my_pair;

	switch (getch()) {
	case HELP_KEY_1:
	    popup_msg(stdscr, help);
	    break;
	case 'p':
	    /* step-by-page */
	    use_pages();
	    update_modes();
	    break;
	case 's':
	    /* step-by-char */
	    use_single();
	    update_modes();
	    break;
	case ' ':
	    single_mode = FALSE;
	    update_modes();
	    break;
	case QUIT:
	case ESCAPE:
	case 'q':
	    done = TRUE;
	    continue;
	case 'c':
	    clobber = !clobber;
	    continue;
	case 'v':
	    if (--video_mode < 0)
		video_mode = MAX_ATTR;
	    continue;
	case 'V':
	    if (video_mode > MAX_ATTR)
		video_mode = 0;
	    continue;
	case 'w':
	    use_wide = !use_wide;
	    continue;
	case ERR:
	    break;
	default:
	    beep();
	    break;
	}
	if (hascolor) {
	    my_attrs = next_attr(video_mode);
	    if (clobber) {
		int fg, bg;
		our_content(current, &fg, &bg);
		my_pair = find_pair(fg, bg);
		if (my_pair > 0) {
		    free_pair(my_pair);
		}
		my_pair = 0;
	    } else {
		my_pair = (use_init
			   ? next_color(current)
			   : make_color(current));
	    }
	} else {
	    my_attrs = next_attr(current);
	    my_pair = 0;
	}
	if (my_pair < 0)
	    break;
	wch[0] = use_wide ? 0xff03 : '#';
	wch[1] = 0;
	setcchar(&temp, wch, my_attrs,
		 (short) my_pair,
		 (use_init ? NULL : (void *) &my_pair));

	if (check_set) {
	    int problem = 0;
	    wchar_t chk_wch[2];
	    attr_t chk_attrs = 0;
	    short chk_pair = 0;
	    int chk_pair2 = 0;

#define AllButColor(a) ((a) & (A_ATTRIBUTES & ~A_COLOR))

	    if (getcchar(&temp, NULL, &chk_attrs, &chk_pair,
			 (use_init ? NULL : (void *) &chk_pair2)) != 2) {
		problem = 1;
	    } else if (getcchar(&temp, chk_wch, &chk_attrs, &chk_pair,
				(use_init ? NULL : (void *) &chk_pair2)) != OK) {
		problem = 2;
	    } else if (chk_wch[0] != wch[0]) {
		problem = 3;
	    } else if (AllButColor(my_attrs) != AllButColor(chk_attrs)) {
		problem = 4;
	    } else if (my_pair != chk_pair) {
		problem = 4;
	    } else if (!use_init && (my_pair != chk_pair2)) {
		problem = 5;
	    }
	    if (problem) {
		wch[0] = (wchar_t) (problem + '0');
		setcchar(&temp, wch, my_attrs,
			 (short) my_pair,
			 (use_init ? NULL : (void *) &my_pair));
	    }
	}

	/*
	 * At the end of a page, move the cursor to the home position.
	 */
	if ((add_wch(&temp) == ERR) && paged_mode) {
	    nodelay(stdscr, !single_mode);
	    move(0, 0);
	}
	total_cells += 1 + (use_wide ? 1 : 0);
	++current;
    }
    stop_curses();
    fclose(output);

    printf("%.1f cells/second\n",
	   (double) (total_cells) / (double) (now() - start));

    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the ncurses alloc_pair function\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
