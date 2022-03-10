/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
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

/****************************************************************************
 *  Author: Zeyd M. Ben-Halim <zmbenhal@netcom.com> 1992,1995               *
 *     and: Eric S. Raymond <esr@snark.thyrsus.com>                         *
 *     and: Thomas E. Dickey                        1996-on                 *
 ****************************************************************************/

/*
 * clear.c --  clears the terminal's screen
 */

#define USE_LIBTINFO
#include <clear_cmd.h>
#include <tty_settings.h>

MODULE_ID("$Id: clear.c,v 1.24 2021/03/20 18:23:14 tom Exp $")

const char *_nc_progname = "clear";

static GCC_NORETURN void
usage(void)
{
#define KEEP(s) s "\n"
    static const char msg[] =
    {
	KEEP("")
	KEEP("Options:")
	KEEP("  -T TERM     use this instead of $TERM")
	KEEP("  -V          print curses-version")
	KEEP("  -x          do not try to clear scrollback")
    };
#undef KEEP
    (void) fprintf(stderr, "Usage: %s [options]\n", _nc_progname);
    fputs(msg, stderr);
    ExitProgram(EXIT_FAILURE);
}

int
main(
	int argc GCC_UNUSED,
	char *argv[]GCC_UNUSED)
{
    TTY tty_settings;
    int fd;
    int c;
    char *term;
    bool opt_x = FALSE;		/* clear scrollback if possible */

    _nc_progname = _nc_rootname(argv[0]);
    term = getenv("TERM");

    while ((c = getopt(argc, argv, "T:Vx")) != -1) {
	switch (c) {
	case 'T':
	    use_env(FALSE);
	    use_tioctl(TRUE);
	    term = optarg;
	    break;
	case 'V':
	    puts(curses_version());
	    ExitProgram(EXIT_SUCCESS);
	case 'x':		/* do not try to clear scrollback */
	    opt_x = TRUE;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    if (optind < argc)
	usage();

    fd = save_tty_settings(&tty_settings, FALSE);

    setupterm(term, fd, (int *) 0);

    ExitProgram((clear_cmd(opt_x) == ERR)
		? EXIT_FAILURE
		: EXIT_SUCCESS);
}
