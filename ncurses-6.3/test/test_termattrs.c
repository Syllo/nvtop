/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
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
 * $Id: test_termattrs.c,v 1.3 2020/02/02 23:34:34 tom Exp $
 *
 * Demonstrate the termattrs and term_attrs functions.
 */

#define USE_TINFO
#include <test.priv.h>

#if HAVE_SETUPTERM

static FILE *my_fp;

static void
test_termattrs(unsigned long value)
{
#define DATA(name) { name, #name }
    static struct {
	unsigned long code;
	const char *name;
    } table[] = {
#ifdef A_ATTRIBUTES
	DATA(A_ATTRIBUTES),
#endif
#ifdef A_CHARTEXT
	    DATA(A_CHARTEXT),
#endif
#ifdef A_COLOR
	    DATA(A_COLOR),
#endif
#ifdef A_STANDOUT
	    DATA(A_STANDOUT),
#endif
#ifdef A_UNDERLINE
	    DATA(A_UNDERLINE),
#endif
#ifdef A_REVERSE
	    DATA(A_REVERSE),
#endif
#ifdef A_BLINK
	    DATA(A_BLINK),
#endif
#ifdef A_DIM
	    DATA(A_DIM),
#endif
#ifdef A_BOLD
	    DATA(A_BOLD),
#endif
#ifdef A_ALTCHARSET
	    DATA(A_ALTCHARSET),
#endif
#ifdef A_INVIS
	    DATA(A_INVIS),
#endif
#ifdef A_PROTECT
	    DATA(A_PROTECT),
#endif
#ifdef A_HORIZONTAL
	    DATA(A_HORIZONTAL),
#endif
#ifdef A_LEFT
	    DATA(A_LEFT),
#endif
#ifdef A_LOW
	    DATA(A_LOW),
#endif
#ifdef A_RIGHT
	    DATA(A_RIGHT),
#endif
#ifdef A_TOP
	    DATA(A_TOP),
#endif
#ifdef A_VERTICAL
	    DATA(A_VERTICAL),
#endif
#ifdef A_ITALIC
	    DATA(A_ITALIC),
#endif
    };
    size_t n;
    fprintf(my_fp, "Result: %08lX\r\n", value);
    for (n = 0; n < SIZEOF(table); ++n) {
	if ((value & table[n].code) != 0) {
	    fprintf(my_fp, "%08lX %08lX %s\r\n",
		    table[n].code, value & table[n].code, table[n].name);
	}
    };
    fputs("\r\n", my_fp);
}

static void
usage(void)
{
    static const char *tbl[] =
    {
	"Usage: test_termattrs [options]"
	,""
	,"Options:"
	,"  -e      use stderr (default stdout)"
	,"  -n      do not initialize terminal"
	,"  -s      use setupterm rather than newterm"
#if USE_WIDEC_SUPPORT
	,"  -w      use term_attrs rather than termattrs"
#endif
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
    bool s_opt = FALSE;
#if USE_WIDEC_SUPPORT
    bool w_opt = FALSE;
#endif

    my_fp = stdout;

    while ((ch = getopt(argc, argv, "ensw")) != -1) {
	switch (ch) {
	case 'e':
	    my_fp = stderr;
	    break;
	case 'n':
	    no_init = TRUE;
	    break;
	case 's':
	    s_opt = TRUE;
	    break;
#if USE_WIDEC_SUPPORT
	case 'w':
	    w_opt = TRUE;
	    break;
#endif
	default:
	    usage();
	    break;
	}
    }
    if (optind < argc)
	usage();

    if (no_init) {
	START_TRACE();
    } else if (s_opt) {
	setupterm((char *) 0, fileno(my_fp), (int *) 0);
    } else {
	newterm((char *) 0, my_fp, stdin);
    }
#if USE_WIDEC_SUPPORT
    if (w_opt)
	test_termattrs((unsigned long) term_attrs());
    else
#endif
	test_termattrs((unsigned long) termattrs());
    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(int argc GCC_UNUSED,
     char *argv[]GCC_UNUSED)
{
    fprintf(stderr, "This program requires terminfo\n");
    exit(EXIT_FAILURE);
}
#endif
