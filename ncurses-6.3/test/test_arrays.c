/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2007-2010,2017 Free Software Foundation, Inc.                  *
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
 * $Id: test_arrays.c,v 1.9 2020/02/02 23:34:34 tom Exp $
 *
 * Author: Thomas E Dickey
 *
 * Demonstrate the public arrays from the terminfo library.

extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) boolnames[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) boolcodes[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) boolfnames[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) numnames[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) numcodes[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) numfnames[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) strnames[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) strcodes[];
extern NCURSES_EXPORT_VAR(NCURSES_CONST char * const ) strfnames[];

 */

#define USE_TINFO
#include <test.priv.h>

#if HAVE_TIGETSTR
#if defined(HAVE_CURSES_DATA_BOOLNAMES) || defined(DECL_CURSES_DATA_BOOLNAMES)

static bool opt_C;
static bool opt_T;
static bool opt_c;
static bool opt_f;
static bool opt_n;
static bool opt_t;

#define PLAIN(opts, name) if (opts) dump_array(#name, name)

static void
dump_array(const char *name, NCURSES_CONST char *const *list)
{
    int n;

    printf("%s:\n", name);
    for (n = 0; list[n] != 0; ++n) {
	printf("%5d:%s\n", n, list[n]);
    }
}

static void
dump_plain(void)
{
    PLAIN(opt_T && opt_n, boolnames);
    PLAIN(opt_C && opt_c, boolcodes);
    PLAIN(opt_T && opt_f, boolfnames);

    PLAIN(opt_T && opt_n, numnames);
    PLAIN(opt_C && opt_c, numcodes);
    PLAIN(opt_T && opt_f, numfnames);

    PLAIN(opt_T && opt_n, strnames);
    PLAIN(opt_C && opt_c, strcodes);
    PLAIN(opt_T && opt_f, strfnames);
}

#define STRING(opts, name) if (opts) { printf("%s\"%s\"", c++ ? "," : "", name); }
#define NUMBER(opts, value) if (opts) { printf("%s%d", c++ ? "," : "", value); }

static void
dump_table(void)
{
    int c = 0;
    int r;

    STRING(opt_t, "Index");
    STRING(opt_t, "Type");
    STRING(opt_n, "Name");
    STRING(opt_c, "Code");
    STRING(opt_f, "FName");
    printf("\n");

    for (r = 0; boolnames[r]; ++r) {
	c = 0;
	NUMBER(opt_t, r);
	STRING(opt_t, "bool");
	STRING(opt_T && opt_n, boolnames[r]);
	STRING(opt_C && opt_c, boolcodes[r]);
	STRING(opt_T && opt_f, boolfnames[r]);
	printf("\n");
    }

    for (r = 0; numnames[r]; ++r) {
	c = 0;
	NUMBER(opt_t, r);
	STRING(opt_t, "num");
	STRING(opt_T && opt_n, numnames[r]);
	STRING(opt_C && opt_c, numcodes[r]);
	STRING(opt_T && opt_f, numfnames[r]);
	printf("\n");
    }

    for (r = 0; strnames[r]; ++r) {
	c = 0;
	NUMBER(opt_t, r);
	STRING(opt_t, "str");
	STRING(opt_T && opt_n, strnames[r]);
	STRING(opt_C && opt_c, strcodes[r]);
	STRING(opt_T && opt_f, strfnames[r]);
	printf("\n");
    }
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: test_arrays [options]",
	"",
	"If no options are given, print all (boolean, numeric, string)",
	"capability names showing their index within the tables.",
	"",
	"Options:",
	" -C       print termcap names",
	" -T       print terminfo names",
	" -c       print termcap names",
	" -f       print full terminfo names",
	" -n       print short terminfo names",
	" -t       print the result as CSV table",
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

    while ((n = getopt(argc, argv, "CTcfnt")) != -1) {
	switch (n) {
	case 'C':
	    opt_C = TRUE;
	    break;
	case 'T':
	    opt_T = TRUE;
	    break;
	case 'c':
	    opt_c = TRUE;
	    break;
	case 'f':
	    opt_f = TRUE;
	    break;
	case 'n':
	    opt_n = TRUE;
	    break;
	case 't':
	    opt_t = TRUE;
	    break;
	default:
	    usage();
	    /* NOTREACHED */
	}
    }
    if (optind < argc)
	usage();

    if (!(opt_T || opt_C)) {
	opt_T = opt_C = TRUE;
    }
    if (!(opt_c || opt_f || opt_n)) {
	opt_c = opt_f = opt_n = TRUE;
    }

    if (opt_t) {
	dump_table();
    } else {
	dump_plain();
    }

    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("This program requires the terminfo arrays\n");
    ExitProgram(EXIT_FAILURE);
}
#endif
#else /* !HAVE_TIGETSTR */
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    printf("This program requires the terminfo functions such as tigetstr\n");
    ExitProgram(EXIT_FAILURE);
}
#endif /* HAVE_TIGETSTR */
