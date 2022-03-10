/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 2005-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: demo_termcap.c,v 1.60 2021/03/20 16:05:49 tom Exp $
 *
 * A simple demo of the termcap interface.
 */
#define USE_TINFO
#include <test.priv.h>
#include <sys/stat.h>

#if NCURSES_XNAMES
#if HAVE_TERM_ENTRY_H
#include <term_entry.h>
#else
#undef NCURSES_XNAMES
#define NCURSES_XNAMES 0
#endif
#endif

#if defined(NCURSES_VERSION)
#if HAVE_NCURSES_TERMCAP_H
#include <ncurses/termcap.h>
#elif HAVE_TERMCAP_H
#include <termcap.h>
#endif
#endif

static GCC_NORETURN void failed(const char *);

static void
failed(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    ExitProgram(EXIT_FAILURE);
}

#if HAVE_TGETENT

#if defined(HAVE_CURSES_DATA_BOOLNAMES) || defined(DECL_CURSES_DATA_BOOLNAMES)
#define USE_CODE_LISTS 1
#else
#define USE_CODE_LISTS 0
#endif

#define FCOLS 8
#define FNAME(type) "%s %-*s = ", #type, FCOLS

static bool b_opt = FALSE;
static bool n_opt = FALSE;
static bool s_opt = FALSE;
static bool q_opt = FALSE;
#ifdef NCURSES_VERSION
static bool x_opt = FALSE;
static bool y_opt = FALSE;
#endif

static char *d_opt;
static char *e_opt;
static char **db_list;
static int db_item;

static char *my_blob;
static char **my_boolcodes;
static char **my_numcodes;
static char **my_numvalues;
static char **my_strcodes;
static char **my_strvalues;

static long total_values;
static long total_b_values;
static long total_n_values;
static long total_s_values;

#define isCapName(c) (isgraph(c) && strchr("^=:\\", c) == 0)
#define EachCapName(n) n = 33; n < 127; ++n

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
    if (result != 0)
	printf("** %s\n", result);
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
#endif /* NO_LEAKS */

static void
show_string(const char *name, const char *value)
{
    printf(FNAME(str), name);
    if (value == ((char *) -1)) {
	printf("CANCELLED");
    } else if (value == ((char *) 0)) {
	printf("ABSENT");
    } else {
	while (*value != 0) {
	    int ch = UChar(*value++);
	    switch (ch) {
	    case '\177':
		fputs("^?", stdout);
		break;
	    case '\033':
		fputs("\\E", stdout);
		break;
	    case '\b':
		fputs("\\b", stdout);
		break;
	    case '\f':
		fputs("\\f", stdout);
		break;
	    case '\n':
		fputs("\\n", stdout);
		break;
	    case '\r':
		fputs("\\r", stdout);
		break;
	    case ' ':
		fputs("\\s", stdout);
		break;
	    case '\t':
		fputs("\\t", stdout);
		break;
	    case '^':
		fputs("\\^", stdout);
		break;
	    case ':':
		fputs("\\072", stdout);
		break;
	    case '\\':
		fputs("\\\\", stdout);
		break;
	    default:
		if (isgraph(ch))
		    fputc(ch, stdout);
		else if (ch < 32)
		    printf("^%c", ch + '@');
		else
		    printf("\\%03o", ch);
		break;
	    }
	}
    }
    printf("\n");
}

static void
show_number(const char *name, int value)
{
    printf(FNAME(num), name);
    printf(" %d\n", value);
}

static void
dumpit(NCURSES_CONST char *cap)
{
    /*
     * One of the limitations of the termcap interface is that the library
     * cannot determine the size of the buffer passed via tgetstr(), nor the
     * amount of space remaining.  This demo simply reuses the whole buffer
     * for each call; a normal termcap application would try to use the buffer
     * to hold all of the strings extracted from the terminal entry.
     */
    char area[1024], *ap = area;
    char *str;
    int num;

    if ((str = tgetstr(cap, &ap)) != 0) {
	total_values++;
	total_s_values++;
	if (!q_opt) {
	    /*
	     * Note that the strings returned are mostly terminfo format, since
	     * ncurses does not convert except for a handful of special cases.
	     */
	    show_string(cap, str);
	}
    } else if ((num = tgetnum(cap)) >= 0) {
	total_values++;
	total_n_values++;
	if (!q_opt) {
	    show_number(cap, num);
	}
    } else if (tgetflag(cap) > 0) {
	total_values++;
	total_b_values++;
	if (!q_opt) {
	    printf(FNAME(flg), cap);
	    printf("%s\n", "true");
	}
    }

    if (!q_opt)
	fflush(stdout);
}

static void
brute_force(const char *name)
{
    char buffer[1024];

    if (db_list) {
	putenv(next_dbitem());
    }
    if (!q_opt)
	printf("Terminal type \"%s\"\n", name);
    if (tgetent(buffer, name) >= 0) {
	char cap[3];
	int c1, c2;

	cap[2] = 0;
	for (EachCapName(c1)) {
	    cap[0] = (char) c1;
	    if (isCapName(c1)) {
		for (EachCapName(c2)) {
		    cap[1] = (char) c2;
		    if (isCapName(c2)) {
			dumpit(cap);
		    }
		}
	    }
	}
    }
}

#if NCURSES_XNAMES
static void
dump_xname(NCURSES_CONST char *cap)
{
    if (strlen(cap) == 2)
	dumpit(cap);
}
#endif

static void
demo_termcap(NCURSES_CONST char *name)
{
    char buffer[1024];

    if (db_list) {
	putenv(next_dbitem());
    }
    if (!q_opt)
	printf("Terminal type \"%s\"\n", name);
    if (tgetent(buffer, name) >= 0) {
	NCURSES_CONST char *cap;
	unsigned n;

	if (b_opt) {
	    for (n = 0;; ++n) {
		cap = my_boolcodes[n];
		if (cap == 0)
		    break;
		dumpit(cap);
	    }
	}

	if (n_opt) {
	    for (n = 0;; ++n) {
		cap = my_numcodes[n];
		if (cap == 0)
		    break;
		dumpit(cap);
	    }
	}

	if (s_opt) {
	    for (n = 0;; ++n) {
		cap = my_strcodes[n];
		if (cap == 0)
		    break;
		dumpit(cap);
	    }
	}
#ifdef NCURSES_VERSION
	if (x_opt && (my_blob == 0) && y_opt) {
#if NCURSES_XNAMES
	    TERMTYPE *term = (TERMTYPE *) cur_term;
	    if (term != 0
		&& ((NUM_BOOLEANS(term) != BOOLCOUNT)
		    || (NUM_NUMBERS(term) != NUMCOUNT)
		    || (NUM_STRINGS(term) != STRCOUNT))) {
		for (n = BOOLCOUNT; n < NUM_BOOLEANS(term); ++n) {
		    dump_xname(ExtBoolname(term, (int) n, boolnames));
		}
		for (n = NUMCOUNT; n < NUM_NUMBERS(term); ++n) {
		    dump_xname(ExtNumname(term, (int) n, numnames));
		}
		for (n = STRCOUNT; n < NUM_STRINGS(term); ++n) {
		    dump_xname(ExtStrname(term, (int) n, strnames));
		}
	    }
#endif
	}
#endif
    }
}

typedef enum {
    pDefault = 0
    ,pComment
    ,pDescription
    ,pEscaped
    ,pNewline
    ,pName
    ,pNumber
    ,pString
} STATE;

static void
parse_description(const char *input_name)
{
    static char empty[1];

    FILE *fp;
    struct stat sb;
    size_t count_bools = 0;
    size_t count_nums = 0;
    size_t count_strs = 0;
    size_t len;
    size_t j, k;
    STATE state;

    if (stat(input_name, &sb) != 0
	|| (sb.st_mode & S_IFMT) != S_IFREG) {
	failed("input is not a file");
    }

    if (sb.st_size == 0) {
	failed("input is empty");
    }

    /*
     * None of the arrays could be larger than the input-file, and since it
     * is small, just allocate the maximum for simplicity.
     */
    if ((my_blob = malloc((size_t) sb.st_size + 1)) == 0 ||
	(my_boolcodes = typeCalloc(char *, sb.st_size)) == 0 ||
	  (my_numcodes = typeCalloc(char *, sb.st_size)) == 0 ||
	  (my_numvalues = typeCalloc(char *, sb.st_size)) == 0 ||
	  (my_strcodes = typeCalloc(char *, sb.st_size)) == 0 ||
	  (my_strvalues = typeCalloc(char *, sb.st_size)) == 0) {
	failed("cannot allocate memory for input-file");
    }

    if ((fp = fopen(input_name, "r")) == 0)
	failed("cannot open input-file");
    len = fread(my_blob, sizeof(char), (size_t) sb.st_size, fp);
    my_blob[sb.st_size] = '\0';
    fclose(fp);

    /*
     * First, get rid of comments and escaped newlines, as well as repeated
     * colons to construct a canonical entry.
     *
     * FIXME: actually this should make an additional pass just to strip
     * comment-lines and escaped newlines.  But it is workable for infocmp
     * output.
     */
    state = pNewline;
    for (j = k = 0; j < len; ++j) {
	int ch = my_blob[j];
	if (ch == '\t') {
	    ch = ' ';
	}
	switch (state) {
	case pNewline:
	    if (ch == ' ') {
		continue;
	    }
	    if (ch == '#') {
		state = pComment;
		continue;
	    }
	    state = pDefault;
	    /* FALLTHRU */
	case pDefault:
	    switch (ch) {
	    case '|':
		state = pDescription;
		continue;
	    case '\\':
		state = pEscaped;
		continue;
	    case '\n':
		state = pNewline;
		continue;
	    case ' ':
	    case ':':
		break;
	    default:
		state = pName;
		break;
	    }
	    my_blob[k++] = (char) ch;
	    break;
	case pComment:
	    if (ch == '\n')
		state = pNewline;
	    break;
	case pDescription:
	    switch (ch) {
	    case ':':
		state = pDefault;
		break;
	    case '\n':
		state = pNewline;
		break;
	    }
	    break;
	case pEscaped:
	    if (ch != '\n') {
		my_blob[k++] = (char) ch;
		state = pDefault;
	    } else {
		state = pNewline;
	    }
	    break;
	case pName:
	    switch (ch) {
	    case '\n':
		state = pNewline;
		continue;
	    case ' ':
	    case ':':
		state = pDefault;
		break;
	    case '#':
		state = pNumber;
		break;
	    case '|':
		state = pDescription;
		continue;
	    }
	    my_blob[k++] = (char) ch;
	    break;
	case pNumber:
	    switch (ch) {
	    case '\n':
		state = pNewline;
		continue;
	    case ':':
		state = pDefault;
		break;
	    case ' ':
		state = pDefault;
		continue;
	    }
	    my_blob[k++] = (char) ch;
	    break;
	case pString:
	    switch (ch) {
	    case '\\':
		if (my_blob[j + 1] == '\0') {
		    state = pDefault;
		    continue;
		}
		break;
	    case '\n':
		state = pNewline;
		continue;
	    case ':':
		state = pDefault;
		break;
	    }
	    my_blob[k++] = (char) ch;
	    break;
	default:
	    /* not used */
	    break;
	}
    }
    my_blob[k] = '\0';

    /*
     * Then, parse what's left, making indexes of the names and values.
     */
    state = pDefault;
    for (j = 0; my_blob[j] != '\0'; ++j) {
	switch (state) {
	case pDefault:
	    switch (my_blob[j]) {
	    case '\\':
		state = pEscaped;
		break;
	    case ':':
		my_blob[j] = '\0';
		if (my_blob[j + 1] != '\0' && my_blob[j + 1] != ':')
		    state = pName;
		break;
	    case ' ':
		break;
	    default:
		break;
	    }
	case pEscaped:
	    break;
	case pName:
	    state = pDefault;
	    /*
	     * Commented-out capabilities might be accessible (they are in
	     * ncurses).
	     */
	    if (my_blob[j] == '.' && my_blob[j + 1] == '.') {
		j += 2;
	    }
	    if (my_blob[j + 1] != '\0') {
		switch (my_blob[j + 2]) {
		case '#':
		    my_numvalues[count_nums] = &my_blob[j + 3];
		    my_numcodes[count_nums++] = &my_blob[j];
		    my_blob[j + 2] = '\0';
		    state = pNumber;
		    j += 2;
		    break;
		case '=':
		    my_strvalues[count_strs] = &my_blob[j + 3];
		    my_strcodes[count_strs++] = &my_blob[j];
		    my_blob[j + 2] = '\0';
		    state = pString;
		    j += 2;
		    break;
		default:
		    if (my_blob[j + 2] == '@') {
			/*
			 * We cannot get the type for a cancelled item
			 * directly, but can infer it assuming the input
			 * came from infocmp, which puts the data in a
			 * known order.
			 */
			if (count_strs) {
			    my_strvalues[count_strs] = empty;
			    my_strcodes[count_strs++] = &my_blob[j];
			} else if (count_nums) {
			    my_numvalues[count_nums] = empty;
			    my_numcodes[count_nums++] = &my_blob[j];
			} else {
			    my_boolcodes[count_bools++] = &my_blob[j];
			}
		    } else {
			my_boolcodes[count_bools++] = &my_blob[j];
		    }
		    j++;
		    break;
		}
	    }
	    break;
	case pNumber:
	    if (!isdigit(UChar(my_blob[j]))) {
		--j;
		state = pDefault;
	    }
	    break;
	case pString:
	    switch (my_blob[j]) {
	    case '\\':
		if (my_blob[j + 1] == '\0') {
		    state = pDefault;
		    continue;
		} else {
		    ++j;
		}
		break;
	    case '\n':
		state = pNewline;
		continue;
	    case ':':
		--j;
		state = pDefault;
		break;
	    }
	    break;
	case pNewline:
	case pComment:
	case pDescription:
	default:
	    break;
	}
    }
    my_boolcodes[count_bools] = 0;
    my_numcodes[count_nums] = 0;
    my_numvalues[count_nums] = 0;
    my_strcodes[count_strs] = 0;
    my_strvalues[count_strs] = 0;

#if 0
    printf("bools:%d\n", (int) count_bools);
    for (j = 0; my_boolcodes[j]; ++j)
	printf("%5d:%s\n", (int) j, my_boolcodes[j]);

    printf("numbers:%d\n", (int) count_nums);
    for (j = 0; my_numcodes[j]; ++j)
	printf("%5d:%s(%s)\n", (int) j, my_numcodes[j], my_numvalues[j]);

    printf("strings:%d\n", (int) count_strs);
    for (j = 0; my_strcodes[j]; ++j)
	printf("%5d:%s(%s)\n", (int) j, my_strcodes[j], my_strvalues[j]);
#endif
}

#if USE_CODE_LISTS
static char **
copy_code_list(NCURSES_CONST char *const *list)
{
    int pass;
    size_t count;
    size_t length = 1;
    char **result = 0;
    char *unused = 0;

    for (pass = 0; pass < 2; ++pass) {
	for (count = 0; list[count] != 0; ++count) {
	    size_t chunk = strlen(list[count]) + 1;
	    if (pass == 0) {
		length += chunk;
	    } else {
		result[count] = unused;
		_nc_STRCPY(unused, list[count], length);
		unused += chunk;
	    }
	}
	if (pass == 0) {
	    char *blob = malloc(length);
	    result = typeCalloc(char *, count + 1);
	    unused = blob;
	    if (blob == 0 || result == 0)
		failed("copy_code_list failed");
	}
    }

    return result;
}

#if NO_LEAKS
static void
free_code_list(char **list)
{
    if (list) {
	free(list[0]);
	free(list);
    }
}
#endif /* NO_LEAKS */
#endif /* USE_CODE_LISTS */

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: demo_termcap [options] [terminal]",
	"",
	"If no options are given, print all (boolean, numeric, string)",
	"capabilities for the given terminal, using short names.",
	"",
	"Options:",
	" -a       try all names, print capabilities found",
	" -b       print boolean-capabilities",
	" -d LIST  colon-separated list of databases to use",
	" -e NAME  environment variable to set with -d option",
	" -i NAME  terminal description to use as names for \"-a\" option, etc.",
	" -n       print numeric-capabilities",
	" -q       quiet (prints only counts)",
	" -r COUNT repeat for given count",
	" -s       print string-capabilities",
	" -v       print termcap-variables",
#ifdef NCURSES_VERSION
	" -x       print extended capabilities",
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
    char *name;
    bool a_opt = FALSE;
#if defined(NCURSES_VERSION) || defined(HAVE_CURSES_DATA_OSPEED)
    bool v_opt = FALSE;
#endif
    char *input_name = 0;

    int repeat;
    int r_opt = 1;

    while ((n = getopt(argc, argv, "abd:e:i:nqr:svxy")) != -1) {
	switch (n) {
	case 'a':
	    a_opt = TRUE;
	    break;
	case 'b':
	    b_opt = TRUE;
	    break;
	case 'd':
	    d_opt = optarg;
	    break;
	case 'e':
	    e_opt = optarg;
	    break;
	case 'i':
	    input_name = optarg;
	    break;
	case 'n':
	    n_opt = TRUE;
	    break;
	case 'q':
	    q_opt = TRUE;
	    break;
	case 'r':
	    if ((r_opt = atoi(optarg)) <= 0)
		usage();
	    break;
	case 's':
	    s_opt = TRUE;
	    break;
#if defined(NCURSES_VERSION) || defined(HAVE_CURSES_DATA_OSPEED)
	case 'v':
	    v_opt = TRUE;
	    break;
#endif
#ifdef NCURSES_VERSION
#if NCURSES_XNAMES
	case 'x':
	    x_opt = TRUE;
	    break;
	case 'y':
	    y_opt = TRUE;
	    x_opt = TRUE;
	    break;
#endif
#endif
	default:
	    usage();
	    break;
	}
    }

#if HAVE_USE_EXTENDED_NAMES
    use_extended_names(x_opt);
#endif

    if (!(b_opt || n_opt || s_opt)) {
	b_opt = TRUE;
	n_opt = TRUE;
	s_opt = TRUE;
    }

    make_dblist();

    if (a_opt) {
	for (repeat = 0; repeat < r_opt; ++repeat) {
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
	}
    } else {
	if (input_name != 0) {
	    parse_description(input_name);
	}
#if USE_CODE_LISTS
	else {
	    my_boolcodes = copy_code_list(boolcodes);
	    my_numcodes = copy_code_list(numcodes);
	    my_strcodes = copy_code_list(strcodes);
	}
#else
	else {
	    failed("no capability-lists available (use -i option)");
	}
#endif /* USE_CODE_LISTS */
	for (repeat = 0; repeat < r_opt; ++repeat) {
	    if (optind < argc) {
		for (n = optind; n < argc; ++n) {
		    demo_termcap(argv[n]);
		}
	    } else if ((name = getenv("TERM")) != 0) {
		demo_termcap(name);
	    } else {
		static char dumb[] = "dumb";
		demo_termcap(dumb);
	    }
	}
    }

    printf("%ld values (%ld booleans, %ld numbers, %ld strings)\n",
	   total_values, total_b_values, total_n_values, total_s_values);

#if defined(NCURSES_VERSION) || defined(HAVE_CURSES_DATA_OSPEED)
    if (v_opt) {
	show_number("PC", PC);
	show_string("UP", UP);
	show_string("BC", BC);
	show_number("ospeed", (int) ospeed);
    }
#endif

#if NO_LEAKS
    free_dblist();
#if USE_CODE_LISTS
    free_code_list(my_boolcodes);
    free_code_list(my_numcodes);
    free_code_list(my_strcodes);
#endif
#endif /* NO_LEAKS */

    ExitProgram(EXIT_SUCCESS);
}

#else
int
main(int argc GCC_UNUSED,
     char *argv[]GCC_UNUSED)
{
    failed("This program requires termcap");
}
#endif
