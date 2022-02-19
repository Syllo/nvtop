/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 2006-2017,2018 Free Software Foundation, Inc.                  *
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
 * $Id: savescreen.c,v 1.58 2021/03/27 23:41:21 tom Exp $
 *
 * Demonstrate save/restore functions from the curses library.
 * Thomas Dickey - 2007/7/14
 */

#define NEED_TIME_H
#include <test.priv.h>
#include <popup_msg.h>
#include <parse_rgb.h>

#if HAVE_SCR_DUMP

#include <sys/types.h>
#include <sys/stat.h>

#if defined(__hpux)
#define MyMarker 'X'
#else
#define MyMarker ACS_DIAMOND
#endif

#define MAX_ANSI 8

static bool use_init = FALSE;
static bool keep_dumps = FALSE;

#if USE_WIDEC_SUPPORT
/* In HPUX curses, cchar_t is opaque; other implementations are not */
static wchar_t
BaseChar(cchar_t data)
{
    wchar_t my_wchar[CCHARW_MAX];
    wchar_t result = 0;
    attr_t my_attr;
    short my_pair;
    if (getcchar(&data, my_wchar, &my_attr, &my_pair, NULL) == OK)
	result = my_wchar[0];
    return result;
}
#endif

static int
fexists(const char *name)
{
    struct stat sb;
    return (stat(name, &sb) == 0 && (sb.st_mode & S_IFMT) == S_IFREG);
}

static void
setup_next(void)
{
    curs_set(1);
    reset_shell_mode();
}

static void
cleanup(char *files[])
{
    if (!keep_dumps) {
	int n;

	for (n = 0; files[n] != 0; ++n) {
	    unlink(files[n]);
	}
    }
}

static int
load_screen(char *filename)
{
    int result;

    if (use_init) {
	if ((result = scr_init(filename)) != ERR)
	    result = scr_restore(filename);
    } else {
	result = scr_set(filename);
    }
    return result;
}

/*
 * scr_restore() or scr_set() operates on curscr.  If we read a character using
 * getch() that will refresh stdscr, wiping out the result.  To avoid that,
 * copy the data back from curscr to stdscr.
 */
static void
after_load(void)
{
    overwrite(curscr, stdscr);
    doupdate();
}

static void
show_what(int color, int which, int last)
{
    int y, x, n;
    time_t now;
    char *mytime;

    getyx(stdscr, y, x);

    move(0, 0);
    printw("Color %d.  Saved %d of %d (? for help)", color, which, last + 1);

    now = time((time_t *) 0);
    mytime = ctime(&now);
    for (n = (int) strlen(mytime) - 1; n >= 0; --n) {
	if (isspace(UChar(mytime[n]))) {
	    mytime[n] = '\0';
	} else {
	    break;
	}
    }
    mvprintw(0, (COLS - n - 2), " %s", mytime);

    move(y, x);

    refresh();
}

static int
get_command(int color, int which, int last)
{
    int ch;

    timeout(50);

    do {
	show_what(color, which, last);
	ch = getch();
    } while (ch == ERR);

    return ch;
}

static int
dump_screen(char **files, int color, int which, int last, bool use_colors)
{
#if USE_WIDEC_SUPPORT
    cchar_t mycc;
#endif
    char *filename = files[which];
    bool dumped = FALSE;

    if (filename != 0) {
	dumped = TRUE;
	show_what(color, ++which, last);
	if (scr_dump(filename) == ERR) {
	    endwin();
	    printf("Cannot write screen-dump %s\n", filename);
	    cleanup(files);
	    ExitProgram(EXIT_SUCCESS);
	}
	if (use_colors) {
	    int cx, cy;
	    int pair = 1 + (which % MAX_ANSI);
	    /*
	     * Change the background color, to make it more obvious.  But that
	     * changes the existing text-color.  Copy the old values from the
	     * currently displayed screen.
	     */
	    bkgd((chtype) COLOR_PAIR(pair));
	    for (cy = 1; cy < LINES; ++cy) {
		for (cx = 0; cx < COLS; ++cx) {
		    wmove(curscr, cy, cx);
		    wmove(stdscr, cy, cx);
#if USE_WIDEC_SUPPORT
		    if (win_wch(curscr, &mycc) != ERR) {
			int myxx = wcwidth(BaseChar(mycc));
			if (myxx > 0) {
			    wadd_wchnstr(stdscr, &mycc, 1);
			    cx += (myxx - 1);
			}
		    }
#else
		    waddch(stdscr, winch(curscr));
#endif
		}
	    }
	}
    }
    return dumped;
}

static void
editor_help(void)
{
    static const char *msgs[] =
    {
	"You are now in the screen-editor, which allows you to make some",
	"lines on the screen, as well as save copies of the screen to a",
	"temporary file",
	"",
	"Keys:",
	"   q           quit",
	"   n           run the screen-loader to show the saved screens",
	"   <space>     dump a screen",
	"",
	"   a           toggle between '#' and graphic symbol for drawing",
	"   c           change color drawn by line to next in palette",
	"   h,j,k,l or arrows to move around the screen, drawing",
	0
    };
    popup_msg(stdscr, msgs);
}

static void
replay_help(void)
{
    static const char *msgs[] =
    {
	"You are now in the screen-loader, which allows you to view",
	"the dumped/restored screens.",
	"",
	"Keys:",
	"   q           quit",
	"   <space>     load the next screen",
	"   <backspace> load the previous screen",
	0
    };
    popup_msg(stdscr, msgs);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: savescreen [-r] files",
	"",
	"Options:",
	" -f file  fill/initialize screen using text from this file",
	" -i       use scr_init/scr_restore rather than scr_set",
	" -k       keep the restored dump-files rather than removing them",
	" -r       replay the screen-dump files"
    };
    unsigned n;
    for (n = 0; n < SIZEOF(msg); ++n) {
	fprintf(stderr, "%s\n", msg[n]);
    }
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int ch;
    int which = 0;
    int last;
    bool use_colors = FALSE;
    bool replaying = FALSE;
    bool done = FALSE;
    char **files;
    char *fill_by = 0;
#if USE_WIDEC_SUPPORT
    cchar_t mycc;
    static const wchar_t mywc[2] =
    {L'#', 0};
#endif

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "f:ikr")) != -1) {
	switch (ch) {
	case 'f':
	    fill_by = optarg;
	    break;
	case 'i':
	    use_init = TRUE;
	    break;
	case 'k':
	    keep_dumps = TRUE;
	    break;
	case 'r':
	    replaying = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }

    files = argv + optind;
    last = argc - optind - 1;

    if (replaying) {
	while (last >= 0 && !fexists(files[last]))
	    --last;
    }

    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);

    if (has_colors() && (start_color() == OK) && COLORS >= MAX_ANSI) {
#if USE_WIDEC_SUPPORT
	bool using_rgb = FALSE;
#endif
	static const struct {
	    int fg, bg;
	} table[MAX_ANSI] = {
#define DATA(fg,bg) { COLOR_##fg, COLOR_##bg }
	    DATA(RED, WHITE),
		DATA(GREEN, WHITE),
		DATA(YELLOW, BLACK),
		DATA(BLUE, WHITE),
		DATA(MAGENTA, WHITE),
		DATA(MAGENTA, BLACK),
		DATA(CYAN, WHITE),
		DATA(CYAN, BLACK),
#undef DATA
	};
	int n;
	int pair = 1;

	use_colors = TRUE;
	/*
	 * Discounting color-pair 0 (no color), make the next 8 color pairs
	 * useful for leaving a visually distinct trail of characters on the
	 * screen.
	 */
	for (n = 0; n < MAX_ANSI; ++n) {
	    init_pair((short) pair++, (short) table[n].fg, (short) table[n].bg);
	}
	/*
	 * After that, use color pairs for constructing a test-pattern, e.g.,
	 * imitating xterm's scripts.
	 */
	if (fill_by == 0) {
	    if (COLORS <= 256) {
		for (n = 0; n < COLORS; ++n)
		    init_pair((short) (n + MAX_ANSI), (short) n, (short) n);
	    }
#if HAVE_TIGETSTR && USE_WIDEC_SUPPORT
	    else {
		int r_max, g_max, b_max;

		if (parse_rgb(&r_max, &g_max, &b_max) > 0) {
		    int rows = LINES - 1;
		    int cols = COLS - 1;
		    int b_delta = (b_max / rows);
		    int r_delta = (r_max / cols);
		    int g_delta = (g_max / cols);
		    int row = 0;
		    int b = 0;

		    using_rgb = TRUE;
		    while (row++ < rows) {
			int col = 0;
			int r = 0;
			int g = g_max;
			while (col++ < cols) {
			    int color = (((r * (g_max + 1)) + g) * (b_max + 1)
					 + b + MAX_ANSI);
#if USE_EXTENDED_COLOR
			    init_extended_pair(pair, color, color);
#else
			    init_pair(pair, color, color);
#endif
			    pair++;
			    r += r_delta;
			    g -= g_delta;
			}
			b += b_delta;
		    }
		}
	    }
#endif
	}
	if ((fill_by == 0) && !replaying) {
#if USE_WIDEC_SUPPORT
	    int cube = 0;
#endif
	    /*
	     * Originally (before wide-characters) ncurses supported 16 colors.
	     */
	    if (COLORS >= 16 && COLORS <= 256) {
		mvprintw(2, 0, "System colors:\n");
		for (n = 0; n < 16; ++n) {
		    pair = n + MAX_ANSI;
		    addch((chtype) (' ' | COLOR_PAIR(pair)));
		    addch((chtype) (' ' | COLOR_PAIR(pair)));
		    if (((n + 1) % 8) == 0)
			addch('\n');
		}
	    }
	    /*
	     * Even with ncurses, you need wide-character support to have more
	     * than 16 colors.
	     */
#if USE_WIDEC_SUPPORT
	    if (COLORS == 88) {
		cube = 4;
	    } else if (COLORS == 256) {
		cube = 6;
	    }
	    if (cube != 0) {
		int r, g, b;
		int cube0 = 16;
		int cube1 = cube0 + (cube * cube * cube);

		addch('\n');
		printw("Color cube, %dx%dx%d:\n", cube, cube, cube);
		for (g = 0; g < cube; g++) {
		    for (r = 0; r < cube; r++) {
			for (b = 0; b < cube; b++) {
			    pair = MAX_ANSI
				+ 16
				+ (r * cube * cube) + (g * cube) + b;
			    setcchar(&mycc, mywc, 0, (short) pair, NULL);
			    add_wch(&mycc);
			    add_wch(&mycc);
			}
			addch(' ');
		    }
		    addch('\n');
		}
		addch('\n');
		printw("Grayscale ramp:\n");
		for (n = cube1; n < COLORS; ++n) {
		    pair = n + MAX_ANSI;
		    setcchar(&mycc, mywc, 0, (short) pair, NULL);
		    add_wch(&mycc);
		    add_wch(&mycc);
		}
	    } else if ((COLORS > 256) && using_rgb) {
		int rows = LINES - 1;
		int cols = COLS - 1;
		int row = 0;

		pair = MAX_ANSI;
		while (row++ < rows) {
		    int col = 0;
		    while (col++ < cols) {
			setcchar(&mycc, mywc, 0, (short) pair, &pair);
			add_wch(&mycc);
			++pair;
		    }
		    addch('\n');
		}
		addch('\n');
	    }
#endif
	}
    }

    if (fill_by != 0) {
	FILE *fp = fopen(fill_by, "r");
	if (fp != 0) {
	    bool filled = FALSE;
	    move(1, 0);
	    while ((ch = fgetc(fp)) != EOF) {
		if (addch(UChar(ch)) == ERR) {
		    filled = TRUE;
		    break;
		}
	    }
	    fclose(fp);
	    if (!filled) {
		while (addch(' ') != ERR) {
		    ;
		}
	    }
	    move(0, 0);
	} else {
	    stop_curses();
	    fprintf(stderr, "Cannot open \"%s\"\n", fill_by);
	    ExitProgram(EXIT_FAILURE);
	}
    }

    if (replaying) {

	/*
	 * Use the last file as the initial/current screen.
	 */
	if (last < 0) {
	    stop_curses();
	    printf("No screen-dumps given\n");
	    ExitProgram(EXIT_FAILURE);
	}

	which = last;
	if (load_screen(files[which]) == ERR) {
	    stop_curses();
	    printf("Cannot load screen-dump %s\n", files[which]);
	    ExitProgram(EXIT_FAILURE);
	}
	after_load();

	while (!done && (ch = getch()) != ERR) {
	    switch (ch) {
	    case 'n':
		/*
		 * If we got a "next" here, skip to the final screen before
		 * moving to the next process.
		 */
		setup_next();
		which = last;
		done = TRUE;
		break;
	    case 'q':
		cleanup(files);
		done = TRUE;
		break;
	    case KEY_BACKSPACE:
	    case '\b':
		if (--which < 0)
		    which = last;
		break;
	    case ' ':
		if (++which > last)
		    which = 0;
		break;
	    case HELP_KEY_1:
		replay_help();
		break;
	    default:
		beep();
		continue;
	    }

	    if (ch == 'q') {
		;
	    } else if (scr_restore(files[which]) == ERR) {
		endwin();
		printf("Cannot load screen-dump %s\n", files[which]);
		cleanup(files);
		ExitProgram(EXIT_FAILURE);
	    } else {
		wrefresh(curscr);
	    }
	}
	endwin();
    } else {
	int y = 0;
	int x = 0;
	int color = 0;
	int altchars = 0;
	bool dirty = use_colors || (fill_by != 0);

	while (!done) {
	    switch (get_command(color, which, last)) {
	    case 'n':
		if (dirty && files[which]) {
		    dump_screen(files, color, which, last, use_colors);
		}
		setup_next();
		done = TRUE;
		break;
	    case 'q':
		cleanup(files);
		done = TRUE;
		break;
	    case ' ':
		if (dump_screen(files, color, which, last, use_colors)) {
		    which = (which + 1) % MAX_ANSI;
		    dirty = FALSE;
		} else {
		    setup_next();
		    done = TRUE;
		}
		break;
	    case KEY_LEFT:
	    case 'h':
		if (--x < 0)
		    x = COLS - 1;
		break;
	    case KEY_DOWN:
	    case 'j':
		if (++y >= LINES)
		    y = 1;
		break;
	    case KEY_UP:
	    case 'k':
		if (--y < 1)
		    y = LINES - 1;
		break;
	    case KEY_RIGHT:
	    case 'l':
		if (++x >= COLS)
		    x = 0;
		break;
	    case 'a':
		altchars = !altchars;
		break;
	    case 'c':
		if (use_colors) {
		    color = (color + 1) % MAX_ANSI;
		}
		break;
	    case HELP_KEY_1:
		editor_help();
		break;
	    default:
		beep();
		continue;
	    }
	    if (!done) {
		chtype attr = A_REVERSE;
		chtype ch2 = (altchars ? MyMarker : '#');
		if (use_colors) {
		    attr |= (chtype) COLOR_PAIR(color);
		}
		move(y, x);
		AddCh(ch2 | attr);
		move(y, x);
		dirty = TRUE;
	    }
	}
	endwin();
    }
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("This program requires the screen-dump functions\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
