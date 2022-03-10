/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
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
 * Author:  Thomas E. Dickey 1998
 *
 * $Id: filter.c,v 1.35 2020/07/25 22:40:57 tom Exp $
 *
 * An example of the 'filter()' function in ncurses, this program prompts
 * for commands and executes them (like a command shell).  It illustrates
 * how ncurses can be used to implement programs that are not full-screen.
 *
 * Ncurses differs slightly from SVr4 curses.  The latter does not flush its
 * state when exiting program mode, so the attributes on the command lines of
 * this program 'bleed' onto the executed commands.  Rather than use the
 * reset_shell_mode() and reset_prog_mode() functions, we could invoke endwin()
 * and refresh(), but that does not work any better.
 */
#define NEED_KEY_EVENT
#include <test.priv.h>

#if HAVE_FILTER

#include <time.h>

static int
show_prompt(int underline, bool clocked)
{
    int limit = COLS;

    move(0, 0);
    attrset(A_NORMAL);
    clrtoeol();
    attrset(A_BOLD);
    addstr("Command: ");

    limit -= getcurx(stdscr);

    if (clocked) {
	if (limit >= 3) {
	    time_t now = time((time_t *) 0);
	    struct tm *my = localtime(&now);
	    char buffer[80];
	    int skip, y, x;
	    int margin;

	    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer)) "%02d:%02d:%02d",
			my->tm_hour,
			my->tm_min,
			my->tm_sec);

	    if (limit > 9) {
		skip = 0;
	    } else if (limit > 6) {
		skip = 3;
	    } else {
		skip = 6;
	    }
	    /*
	     * Write the clock message on the right-margin so we can show the
	     * results of resizing the screen.
	     */
	    getyx(stdscr, y, x);
	    margin = (int) strlen(buffer) - skip;
	    limit -= margin;
	    move(0, COLS - margin);
	    addstr(buffer);
	    move(y, x);
	}
    }
    attron(underline);
    return limit;
}

static int
new_command(char *buffer, int length, int underline, bool clocked, bool polled)
{
    int code = OK;

    if (polled) {
	bool done = FALSE;
	bool first = TRUE;
	int y = 0, x = 0;
	int n;
	int mark = 0;
	int used = 0;
	const int gap = 2;

	timeout(20);		/* no one types 50CPS... */
	while (!done) {
	    int limit;
	    int ch = getch();

	    buffer[used] = '\0';

	    limit = show_prompt(underline, clocked);
	    if (first) {
		getyx(stdscr, y, x);
		first = FALSE;
	    } else {
		int left = 0;

		/*
		 * if the screen is too narrow to show the whole buffer,
		 * shift the editing point left/right as needed.
		 */
		move(y, x);
		if ((used + gap) > limit) {
		    while ((mark - left + gap) > limit) {
			left += limit / 2;
		    }
		}
		printw("%.*s", limit, buffer + left);
		move(y, x + mark - left);
	    }

	    switch (ch) {
	    case ERR:
		continue;
	    case '\004':
		code = ERR;
		done = TRUE;
		break;
	    case KEY_ENTER:
	    case '\n':
		done = TRUE;
		break;
	    case KEY_BACKSPACE:
	    case '\b':
		if (used) {
		    if (mark < used) {
			/* getnstr does not do this */
			if (mark > 0) {
			    --mark;
			    for (n = mark; n < used; ++n) {
				buffer[n] = buffer[n + 1];
			    }
			} else {
			    flash();
			}
		    } else {
			/* getnstr does this */
			mark = --used;
			buffer[used] = '\0';
		    }
		} else {
		    flash();
		}
		break;
		/*
		 * Unlike getnstr, this function can move the cursor into the
		 * middle of the buffer and insert/delete at that point.
		 */
	    case KEY_HOME:
		mark = 0;
		break;
	    case KEY_END:
		mark = used;
		break;
	    case KEY_LEFT:
		if (mark > 0) {
		    mark--;
		} else {
		    flash();
		}
		break;
	    case KEY_RIGHT:
		if (mark < used) {
		    mark++;
		} else {
		    flash();
		}
		break;
#ifdef KEY_EVENT
	    case KEY_EVENT:
		continue;
#endif
#ifdef KEY_RESIZE
	    case KEY_RESIZE:
		/*
		 * Unlike getnstr, this function "knows" what the whole screen
		 * is supposed to look like, and can handle resize events.
		 */
		continue;
#endif
	    case '\t':
		ch = ' ';
		/* FALLTHRU */
	    default:
		if (ch >= KEY_MIN) {
		    flash();
		    continue;
		}
		if (mark < used) {
		    /* getnstr does not do this... */
		    for (n = used + 1; n > mark; --n) {
			buffer[n] = buffer[n - 1];
		    }
		    buffer[mark] = (char) ch;
		    used++;
		    mark++;
		} else {
		    /* getnstr does this part */
		    buffer[used] = (char) ch;
		    mark = ++used;
		}
		break;
	    }
	}
    } else {
	show_prompt(underline, clocked);

	code = getnstr(buffer, length);
	/*
	 * If this returns anything except ERR/OK, it would be one of ncurses's
	 * extensions.  Fill the buffer with something harmless that the shell
	 * will execute as a comment.
	 */
#ifdef KEY_EVENT
	if (code == KEY_EVENT)
	    _nc_STRCPY(buffer, "# event!", length);
#endif
#ifdef KEY_RESIZE
	if (code == KEY_RESIZE) {
	    _nc_STRCPY(buffer, "# resize!", length);
	    getch();
	}
#endif
    }
    attroff(underline);
    attroff(A_BOLD);
    refresh();

    return code;
}

#ifdef NCURSES_VERSION
/*
 * Cancel xterm's alternate-screen mode (from dialog -TD)
 */
#define isprivate(s) ((s) != 0 && strstr(s, "\033[?") != 0)
static void
cancel_altscreen(void)
{
    if (isatty(fileno(stdout))
	&& key_mouse != 0	/* xterm and kindred */
	&& isprivate(enter_ca_mode)
	&& isprivate(exit_ca_mode)) {
	/*
	 * initscr() or newterm() already wrote enter_ca_mode as a side effect
	 * of initializing the screen.  It would be nice to not even do that,
	 * but we do not really have access to the correct copy of the
	 * terminfo description until those functions have been invoked.
	 */
	(void) refresh();
	(void) putp(exit_ca_mode);
	(void) fflush(stdout);
	/*
	 * Prevent ncurses from switching "back" to the normal screen when
	 * exiting from this program.  That would move the cursor to the
	 * original location saved in xterm.  Normally curses sets the cursor
	 * position to the first line after the display, but the alternate
	 * screen switching is done after that point.
	 *
	 * Cancelling the strings altogether also works around the buggy
	 * implementation of alternate-screen in rxvt, etc., which clear more
	 * of the display than they should.
	 */
	enter_ca_mode = 0;
	exit_ca_mode = 0;
    }
}
#endif

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: filter [options]"
	,""
	,"Options:"
#ifdef NCURSES_VERSION
	,"  -a   suppress xterm alternate-screen by amending smcup/rmcup"
#endif
	,"  -c   show current time on prompt line with \"Command\""
#if HAVE_USE_DEFAULT_COLORS
	,"  -d   invoke use_default_colors"
#endif
	,"  -i   use initscr() rather than newterm()"
	,"  -p   poll for individual characters rather than using getnstr"
    };
    unsigned n;
    for (n = 0; n < SIZEOF(msg); n++)
	fprintf(stderr, "%s\n", msg[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc, char *argv[])
{
    int ch;
    char buffer[80];
    int underline;
#ifdef NCURSES_VERSION
    bool a_option = FALSE;
#endif
    bool c_option = FALSE;
#if HAVE_USE_DEFAULT_COLORS
    bool d_option = FALSE;
#endif
    bool i_option = FALSE;
    bool p_option = FALSE;

    setlocale(LC_ALL, "");

    while ((ch = getopt(argc, argv, "adcip")) != -1) {
	switch (ch) {
#ifdef NCURSES_VERSION
	case 'a':
	    a_option = TRUE;
	    break;
#endif
	case 'c':
	    c_option = TRUE;
	    break;
#if HAVE_USE_DEFAULT_COLORS
	case 'd':
	    d_option = TRUE;
	    break;
#endif
	case 'i':
	    i_option = TRUE;
	    break;
	case 'p':
	    p_option = TRUE;
	    break;
	default:
	    usage();
	}
    }

    printf("starting filter program using %s...\n",
	   i_option ? "initscr" : "newterm");
    filter();
    if (i_option) {
	initscr();
    } else {
	if (newterm((char *) 0, stdout, stdin) == 0) {
	    fprintf(stderr, "cannot initialize terminal\n");
	    ExitProgram(EXIT_FAILURE);
	}
    }
#ifdef NCURSES_VERSION
    if (a_option) {
	cancel_altscreen();
    }
#endif
    cbreak();
    keypad(stdscr, TRUE);

    if (has_colors()) {
	int background = COLOR_BLACK;
	start_color();
#if HAVE_USE_DEFAULT_COLORS
	if (d_option && (use_default_colors() != ERR))
	    background = -1;
#endif
	init_pair(1, COLOR_CYAN, (short) background);
	underline = COLOR_PAIR(1);
    } else {
	underline = A_UNDERLINE;
    }

    for (;;) {
	int code = new_command(buffer, sizeof(buffer) - 1,
			       underline, c_option, p_option);
	if (code == ERR || *buffer == '\0')
	    break;
	reset_shell_mode();
	printf("\n");
	fflush(stdout);
	IGNORE_RC(system(buffer));
	reset_prog_mode();
	touchwin(stdscr);
	erase();
	refresh();
    }
    clear();
    refresh();
    endwin();
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(void)
{
    printf("This program requires the filter function\n");
    ExitProgram(EXIT_FAILURE);
}
#endif /* HAVE_FILTER */
