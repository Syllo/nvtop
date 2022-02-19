/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 2015-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: test_sgr.c,v 1.17 2021/03/27 22:43:36 tom Exp $
 *
 * A simple demo of the sgr/sgr0 terminal capabilities.
 */
#define USE_TINFO
#include <test.priv.h>

#if !HAVE_TIGETSTR
static GCC_NORETURN void failed(const char *);

static void
failed(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    ExitProgram(EXIT_FAILURE);
}
#endif

#if HAVE_TIGETSTR

static bool no_init = FALSE;
static bool q_opt = FALSE;

static char *d_opt;
static char *e_opt;
static char **db_list;
static int db_item;

static long total_values;

static char *
make_dbitem(char *p, char *q)
{
    size_t need = strlen(e_opt) + 2 + (size_t) (p - q);
    char *result = malloc(need);
    _nc_SPRINTF(result, _nc_SLIMIT(need) "%s=%.*s", e_opt, (int) (p - q), q);
    return result;
}

static void
make_dblist(void)
{
    if (d_opt && e_opt) {
	int pass;

	for (pass = 0; pass < 2; ++pass) {
	    char *p, *q;
	    size_t count = 0;

	    for (p = q = d_opt; *p != '\0'; ++p) {
		if (*p == ':') {
		    if (p != q + 1) {
			if (pass) {
			    db_list[count] = make_dbitem(p, q);
			}
			count++;
		    }
		    q = p + 1;
		}
	    }
	    if (p != q + 1) {
		if (pass) {
		    db_list[count] = make_dbitem(p, q);
		}
		count++;
	    }
	    if (!pass) {
		db_list = typeCalloc(char *, count + 1);
	    }
	}
    }
}

static char *
next_dbitem(void)
{
    char *result = 0;

    if (db_list) {
	if ((result = db_list[db_item]) == 0) {
	    db_item = 0;
	    result = db_list[0];
	} else {
	    db_item++;
	}
    }
    printf("** %s\n", result ? result : "<null>");
    return result;
}

#if NO_LEAKS
static void
free_dblist(void)
{
    if (db_list) {
	int n;
	for (n = 0; db_list[n]; ++n)
	    free(db_list[n]);
	free(db_list);
	db_list = 0;
    }
}
#endif

#define MAXPAR    9
#define MAXSGR    (1 << MAXPAR)
#define BITS2P(n) (count & (1 << (n - 1)))
#define MASK_SMSO (1 << 0)
#define MASK_BOLD (1 << 5)
#define MASK_REV  (1 << 2)

static void
dumpit(unsigned bits, unsigned ignore, const char *sgr, const char *sgr0)
{
    static const char sample[] = "abcdefghijklm";
    static char params[] = "SURBDBIPA";
    unsigned n;

    printf("%4u ", bits);
    bits &= ~ignore;
    for (n = 0; n < MAXPAR; ++n) {
	putchar((int) ((bits & (unsigned) (1 << n)) ? params[n] : '-'));
    }
    putchar(' ');
    putp(sgr);
    putp(sample);
    putp(sgr0);
    putchar('\n');
}

static bool
one_bit(unsigned a, unsigned b)
{
    unsigned c = (a ^ b);
    bool result = FALSE;
    if (c) {
	while (!(c & 1)) {
	    c >>= 1;
	}
	result = (c == 1);
    }
    return result;
}

static void
brute_force(const char *name)
{
    unsigned count;
    char *my_sgr;
    char *my_sgr0;
    char *my_bold;
    char *my_revs;
    char *my_smso;
    char *my_name = strdup(name);

    if (db_list) {
	putenv(next_dbitem());
    }

    if (!q_opt)
	printf("Terminal type \"%s\"\n", my_name);

    if (no_init) {
	START_TRACE();
    } else {
	setupterm((NCURSES_CONST char *) my_name, 1, (int *) 0);
    }

    if (!q_opt) {
	if (strcmp(my_name, ttytype))
	    printf("... actual \"%s\"\n", ttytype);
    }

    my_sgr = tigetstr("sgr");
    my_sgr0 = tigetstr("sgr0");
    my_bold = tigetstr("bold");
    my_revs = tigetstr("rev");
    my_smso = tigetstr("smso");

    if (!VALID_STRING(my_sgr)) {
	fprintf(stderr, "no \"sgr\" capability found\n");
    } else if (!VALID_STRING(my_sgr0)) {
	fprintf(stderr, "no \"sgr0\" capability found\n");
    } else {
	char *values[MAXSGR + MAXPAR];
	unsigned j;
	unsigned ignore = 0;
	unsigned reason = 0;
	unsigned repeat = 0;
	for (count = 0; count < MAXSGR; ++count) {
	    values[count] = tparm(my_sgr,
				  BITS2P(1),
				  BITS2P(2),
				  BITS2P(3),
				  BITS2P(4),
				  BITS2P(5),
				  BITS2P(6),
				  BITS2P(7),
				  BITS2P(8),
				  BITS2P(9));
	    if (values[count] != 0) {
		values[count] = strdup(values[count]);
	    }
	}
	for (count = 0; count < MAXSGR; ++count) {
	    if (values[count] != 0) {
		for (j = count + 1; j < MAXSGR; ++j) {
		    if (values[j] == 0)
			continue;
		    if (strcmp(values[count], values[j]))
			continue;
		    if (one_bit(count, j)) {
			free(values[j]);
			values[j] = 0;
		    }
		}
	    }
	}
	for (j = 0; j < MAXPAR; ++j) {
	    unsigned mask = (unsigned) (1 << j);
	    for (count = 0; count < MAXSGR; ++count) {
		if ((count & mask) != 0)
		    continue;
		if (values[count] != 0 && values[count + mask] != 0) {
		    mask = 0;
		    break;
		}
	    }
	    ignore |= mask;
	}
	/* smso is tested first, but often duplicates bold or reverse. */
	if (VALID_STRING(my_smso)) {
	    if (VALID_STRING(my_bold) && !strcmp(my_bold, my_smso)) {
		repeat |= MASK_SMSO;
		reason = MASK_BOLD;
	    }
	    if (VALID_STRING(my_revs) && !strcmp(my_revs, my_smso)) {
		repeat |= MASK_SMSO;
		reason = MASK_REV;
	    }
	}
	for (count = 0; count < MAXSGR; ++count) {
	    if (values[count] != 0) {
		bool found = FALSE;
		if ((repeat & MASK_SMSO) != 0
		    && (count & MASK_SMSO) != 0) {
		    found = TRUE;
		} else {
		    for (j = 0; j < count; ++j) {
			if (values[j] != 0 && !strcmp(values[j], values[count])) {
			    if ((repeat & MASK_SMSO) != 0
				&& (j & MASK_SMSO) != 0
				&& (count & reason) != 0) {
				continue;
			    }
			    found = TRUE;
			    break;
			}
		    }
		}
		if (!found) {
		    dumpit(count, ignore, values[count], my_sgr0);
		    ++total_values;
		}
	    }
	}
	for (count = 0; count < MAXSGR; ++count) {
	    free(values[count]);
	}
    }
    free(my_name);
    del_curterm(cur_term);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: test_sgr [options] [terminal]",
	"",
	"Print all distinct combinations of sgr capability.",
	"",
	"Options:",
	" -d LIST  colon-separated list of databases to use",
	" -e NAME  environment variable to set with -d option",
	" -n       do not initialize terminal, to test error-checking",
	" -q       quiet (prints only counts)",
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
    char *name;

    while ((n = getopt(argc, argv, "d:e:nq")) != -1) {
	switch (n) {
	case 'd':
	    d_opt = optarg;
	    break;
	case 'e':
	    e_opt = optarg;
	    break;
	case 'n':
	    no_init = TRUE;
	    break;
	case 'q':
	    q_opt = TRUE;
	    break;
	default:
	    usage();
	    break;
	}
    }

    make_dblist();

    if (optind < argc) {
	for (n = optind; n < argc; ++n) {
	    brute_force(argv[n]);
	}
    } else if ((name = getenv("TERM")) != 0) {
	brute_force(name);
    } else {
	static char dumb[] = "dumb";
	brute_force(dumb);
    }

    printf("%ld distinct values\n", total_values);

#if NO_LEAKS
    free_dblist();
#endif

    ExitProgram(EXIT_SUCCESS);
}

#else /* !HAVE_TIGETSTR */
int
main(int argc GCC_UNUSED, char *argv[]GCC_UNUSED)
{
    failed("This program requires the terminfo functions such as tigetstr");
    ExitProgram(EXIT_FAILURE);
}
#endif /* HAVE_TIGETSTR */
