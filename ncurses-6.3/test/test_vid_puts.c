/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2013-2014,2017 Free Software Foundation, Inc.                  *
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
 * $Id: test_vid_puts.c,v 1.12 2021/04/25 00:10:43 tom Exp $
 *
 * Demonstrate the vid_puts and vid_attr functions.
 * Thomas Dickey - 2013/01/12
 */

#define USE_TINFO
#include <test.priv.h>

#if USE_WIDEC_SUPPORT && HAVE_SETUPTERM && HAVE_VID_PUTS

static FILE *my_fp;
static bool p_opt = FALSE;

static
TPUTS_PROTO(outc, c)
{
    int rc;

    rc = putc(c, my_fp);
    TPUTS_RETURN(rc);
}

static bool
outs(const char *s)
{
    if (VALID_STRING(s)) {
	tputs(s, 1, outc);
	return TRUE;
    }
    return FALSE;
}

static void
cleanup(void)
{
    if (cur_term != 0) {
	outs(exit_attribute_mode);
	if (!outs(orig_colors))
	    outs(orig_pair);
	outs(cursor_normal);
    }
}

static void
change_attr(chtype attr)
{
    if (p_opt) {
	vid_puts(attr, (short) 0, (void *) 0, outc);
    } else {
	vid_attr(attr, (short) 0, (void *) 0);
    }
}

static void
test_vid_puts(void)
{
    fprintf(my_fp, "Name: ");
    change_attr(A_BOLD);
    fputs("Bold", my_fp);
    change_attr(A_REVERSE);
    fputs(" Reverse", my_fp);
    change_attr(A_NORMAL);
    fputs("\n", my_fp);
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: test_vid_puts [options]"
	,""
	,"Options:"
	,"  -e      use stderr (default stdout)"
	,"  -n      do not initialize terminal"
	,"  -p      use vid_puts (default vid_attr)"
    };
    unsigned n;
    for (n = 0; n < SIZEOF(tbl); ++n)
	fprintf(stderr, "%s\n", tbl[n]);
    ExitProgram(EXIT_FAILURE);
}

int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    int ch;
    bool no_init = FALSE;

    my_fp = stdout;

    while ((ch = getopt(argc, argv, "enp")) != -1) {
	switch (ch) {
	case 'e':
	    my_fp = stderr;
	    break;
	case 'n':
	    no_init = TRUE;
	    break;
	case 'p':
	    p_opt = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }
    if (optind < argc)
	usage();

    if (no_init) {
	START_TRACE();
    } else {
	setupterm((char *) 0, fileno(my_fp), (int *) 0);
    }
    test_vid_puts();
    cleanup();
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(void)
{
    printf("This program requires the wide-ncurses terminfo library\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
