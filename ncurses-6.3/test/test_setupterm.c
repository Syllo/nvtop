/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2015,2016 Free Software Foundation, Inc.                       *
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
 * Author: Thomas E. Dickey
 *
 * $Id: test_setupterm.c,v 1.10 2020/02/02 23:34:34 tom Exp $
 *
 * A simple demo of setupterm/restartterm.
 */
#include <test.priv.h>

#if HAVE_TIGETSTR

static bool a_opt = FALSE;
static bool f_opt = FALSE;
static bool n_opt = FALSE;
static bool r_opt = FALSE;

static void
test_rc(NCURSES_CONST char *name, int actual_rc, int actual_err)
{
    int expect_rc = -1;
    int expect_err = -1;

    if (name == 0)
	name = getenv("TERM");
    if (name == 0)
	name = "?";

    switch (*name) {
    case 'v':			/* vt100 is normal */
    case 'd':			/* dumb has no special flags */
	expect_rc = 0;
	expect_err = 1;
	break;
    case 'l':			/* lpr is hardcopy */
	expect_err = 1;
	break;
    case 'u':			/* unknown is generic */
	expect_err = 0;
	break;
    default:
	break;
    }
    if (n_opt) {
	expect_rc = -1;
	expect_err = -1;
    }
    printf("%s",
	   ((actual_rc == expect_rc && actual_err == expect_err)
	    ? "OK"
	    : "ERR"));
    printf(" '%s'", name);
    if (actual_rc == expect_rc) {
	printf(" rc=%d", actual_rc);
    } else {
	printf(" rc=%d (%d)", actual_rc, expect_rc);
    }
    if (actual_err == expect_err) {
	printf(" err=%d", actual_err);
    } else {
	printf(" err=%d (%d)", actual_err, expect_err);
    }
    printf("\n");
}

static void
test_setupterm(NCURSES_CONST char *name)
{
    int rc;
    int err = -99;

#if HAVE_RESTARTTERM
    if (r_opt)
	rc = restartterm(name, 0, f_opt ? NULL : &err);
    else
#endif
	rc = setupterm(name, 0, f_opt ? NULL : &err);
    test_rc(name, rc, err);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: test_setupterm [options] [terminal]",
	"",
	"Demonstrate error-checking for setupterm and restartterm.",
	"",
	"Options:",
	" -a       automatic test for each success/error code",
	" -f       treat errors as fatal",
	" -n       set environment to disable terminfo database, assuming",
	"          the compiled-in paths for database also fail",
#if HAVE_RESTARTTERM
	" -r       test restartterm rather than setupterm",
#endif
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
    int n;

    while ((n = getopt(argc, argv, "afnr")) != -1) {
	switch (n) {
	case 'a':
	    a_opt = TRUE;
	    break;
	case 'f':
	    f_opt = TRUE;
	    break;
	case 'n':
	    n_opt = TRUE;
	    break;
#if HAVE_RESTARTTERM
	case 'r':
	    r_opt = TRUE;
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }

    if (n_opt) {
	static char none[][25] =
	{
	    "HOME=/GUI",
	    "TERMINFO=/GUI",
	    "TERMINFO_DIRS=/GUI"
	};
	/*
	 * We can turn this off, but not on again, because ncurses caches the
	 * directory locations.
	 */
	printf("** without database\n");
	for (n = 0; n < 3; ++n)
	    putenv(none[n]);
    } else {
	printf("** with database\n");
    }

    /*
     * The restartterm relies on an existing screen, so we make one here.
     */
    if (r_opt) {
	newterm("ansi", stdout, stdin);
	reset_shell_mode();
    }

    if (a_opt) {
	static char predef[][9] =
	{"vt100", "dumb", "lpr", "unknown", "none-such"};
	if (optind < argc) {
	    usage();
	}
	for (n = 0; n < 4; ++n) {
	    test_setupterm(predef[n]);
	}
    } else {
	if (optind < argc) {
	    for (n = optind; n < argc; ++n) {
		test_setupterm(argv[n]);
	    }
	} else {
	    test_setupterm(NULL);
	}
    }

    ExitProgram(EXIT_SUCCESS);
}

#else /* !HAVE_TIGETSTR */
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("This program requires the terminfo functions such as tigetstr\n");
    ExitProgram(EXIT_FAILURE);
}
#endif /* HAVE_TIGETSTR */
