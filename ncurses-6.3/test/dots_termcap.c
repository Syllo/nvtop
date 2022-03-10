/****************************************************************************
 * Copyright 2018-2019,2020 Thomas E. Dickey                                *
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
 * Author: Thomas E. Dickey
 *
 * $Id: dots_termcap.c,v 1.26 2020/09/05 17:58:47 juergen Exp $
 *
 * A simple demo of the termcap interface.
 */
#define USE_TINFO
#include <test.priv.h>

#if !defined(_NC_WINDOWS)
#include <sys/time.h>
#endif

#if HAVE_TGETENT

#include <time.h>

static bool interrupted = FALSE;
static long total_chars = 0;
static time_t started;

static char *t_AB;
static char *t_AF;
static char *t_cl;
static char *t_cm;
static char *t_me;
static char *t_mr;
static char *t_oc;
static char *t_op;
static char *t_ve;
static char *t_vi;

static struct {
    NCURSES_CONST char *name;
    char **value;
} my_caps[] = {

    {
	"AB", &t_AB
    },
    {
	"AF", &t_AF
    },
    {
	"cl", &t_cl
    },
    {
	"cm", &t_cm
    },
    {
	"me", &t_me
    },
    {
	"mr", &t_mr
    },
    {
	"oc", &t_oc
    },
    {
	"op", &t_op
    },
    {
	"ve", &t_ve
    },
    {
	"vi", &t_vi
    },
};

static
TPUTS_PROTO(outc, c)
{
    int rc = c;

    if (interrupted) {
	char tmp = (char) c;
	if (write(STDOUT_FILENO, &tmp, (size_t) 1) == -1)
	    rc = EOF;
    } else {
	rc = putc(c, stdout);
    }
    TPUTS_RETURN(rc);
}

static bool
outs(char *s)
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
    outs(t_me);
    if (!outs(t_oc))
	outs(t_op);
    outs(t_cl);
    outs(t_ve);

    fflush(stdout);
    fprintf(stderr, "\n\n%ld total cells, rate %.2f/sec\n",
	    total_chars,
	    ((double) (total_chars) / (double) (time((time_t *) 0) - started)));
}

static void
onsig(int n GCC_UNUSED)
{
    interrupted = TRUE;
}

static double
ranf(void)
{
    long r = (rand() & 077777);
    return ((double) r / 32768.);
}

static void
my_napms(int ms)
{
    if (ms > 0) {
#if defined(_NC_WINDOWS) || !HAVE_GETTIMEOFDAY
	Sleep((unsigned int) ms);
#else
	struct timeval data;
	data.tv_sec = 0;
	data.tv_usec = ms * 1000;
	select(0, NULL, NULL, NULL, &data);
#endif
    }
}

static int
get_number(NCURSES_CONST char *cap, const char *env)
{
    int result = tgetnum(cap);
    char *value = env ? getenv(env) : 0;
    if (value != 0 && *value != 0) {
	char *next = 0;
	long check = strtol(value, &next, 10);
	if (check > 0 && *next == '\0')
	    result = (int) check;
    }
    return result;
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: dots_termcap [options]"
	,""
	,"Options:"
	," -T TERM  override $TERM"
	," -e       allow environment $LINES / $COLUMNS"
	," -m SIZE  set margin (default: 2)"
	," -r SECS  self-interrupt/exit after specified number of seconds"
	," -s MSECS delay 1% of the time (default: 1 msecs)"
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
    int num_colors;
    int num_lines;
    int num_columns;
    int e_option = 0;
    int m_option = 2;
    int r_option = 0;
    int s_option = 1;
    double r;
    double c;
    char buffer[1024];
    char area[1024];
    char *name;
    size_t need;
    char *my_env;

    while ((ch = getopt(argc, argv, "T:em:r:s:")) != -1) {
	switch (ch) {
	case 'T':
	    need = 6 + strlen(optarg);
	    my_env = malloc(need);
	    _nc_SPRINTF(my_env, _nc_SLIMIT(need) "TERM=%s", optarg);
	    putenv(my_env);
	    break;
	case 'e':
	    e_option = 1;
	    break;
	case 'm':
	    m_option = atoi(optarg);
	    break;
	case 'r':
	    r_option = atoi(optarg);
	    break;
	case 's':
	    s_option = atoi(optarg);
	    break;
	default:
	    usage();
	    break;
	}
    }

    if ((name = getenv("TERM")) == 0) {
	fprintf(stderr, "TERM is not set\n");
	ExitProgram(EXIT_FAILURE);
    }

    srand((unsigned) time(0));

    SetupAlarm((unsigned) r_option);
    InitAndCatch(ch = tgetent(buffer, name), onsig);
    if (ch < 0) {
	fprintf(stderr, "terminal description not found\n");
	ExitProgram(EXIT_FAILURE);
    } else {
	size_t t;
	char *ap = area;
	for (t = 0; t < SIZEOF(my_caps); ++t) {
	    *(my_caps[t].value) = tgetstr((NCURSES_CONST char *)
					  my_caps[t].name, &ap);
	}
    }

    num_colors = tgetnum("Co");
#define GetNumber(cap,env) get_number(cap, e_option ? env : 0)
    num_lines = GetNumber("li", "LINES");
    num_columns = GetNumber("co", "COLUMNS");

    outs(t_cl);
    outs(t_vi);
    if (num_colors > 1) {
	if (!VALID_STRING(t_AF)
	    || !VALID_STRING(t_AB)
	    || (!VALID_STRING(t_oc) && !VALID_STRING(t_op)))
	    num_colors = -1;
    }

    r = (double) (num_lines - (2 * m_option));
    c = (double) (num_columns - (2 * m_option));
    started = time((time_t *) 0);

    while (!interrupted) {
	int x = (int) (c * ranf()) + m_option;
	int y = (int) (r * ranf()) + m_option;
	int p = (ranf() > 0.9) ? '*' : ' ';

	tputs(tgoto(t_cm, x, y), 1, outc);
	if (num_colors > 0) {
	    int z = (int) (ranf() * num_colors);
	    if (ranf() > 0.01) {
		tputs(tgoto(t_AF, 0, z), 1, outc);
	    } else {
		tputs(tgoto(t_AB, 0, z), 1, outc);
		if (s_option)
		    my_napms(s_option);
	    }
	} else if (VALID_STRING(t_me)
		   && VALID_STRING(t_mr)) {
	    if (ranf() <= 0.01) {
		outs((ranf() > 0.6)
		     ? t_mr
		     : t_me);
		if (s_option)
		    my_napms(s_option);
	    }
	}
	outc(p);
	fflush(stdout);
	++total_chars;
    }
    cleanup();
    ExitProgram(EXIT_SUCCESS);
}
#else
int
main(int argc GCC_UNUSED,
     char *argv[]GCC_UNUSED)
{
    fprintf(stderr, "This program requires termcap\n");
    exit(EXIT_FAILURE);
}
#endif
