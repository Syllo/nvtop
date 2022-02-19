/****************************************************************************
 * Copyright 2019-2020,2021 Thomas E. Dickey                                *
 * Copyright 2009-2016,2017 Free Software Foundation, Inc.                  *
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
 * $Id: demo_terminfo.c,v 1.52 2021/03/20 16:07:29 tom Exp $
 *
 * A simple demo of the terminfo interface.
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

static GCC_NORETURN void failed(const char *);

static void
failed(const char *msg)
{
    fprintf(stderr, "%s\n", msg);
    ExitProgram(EXIT_FAILURE);
}

#if HAVE_TIGETSTR

#if defined(HAVE_CURSES_DATA_BOOLNAMES) || defined(DECL_CURSES_DATA_BOOLNAMES)
#define USE_CODE_LISTS 1
#else
#define USE_CODE_LISTS 0
#endif

static bool a_opt = FALSE;
static bool b_opt = FALSE;
static bool f_opt = FALSE;
static bool n_opt = FALSE;
static bool q_opt = FALSE;
static bool s_opt = FALSE;
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

#define FCOLS 8
#define FNAME(type) "%s %-*s = ", #type, f_opt ? 24 : FCOLS

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
#endif

static void
dumpit(NCURSES_CONST char *cap, const char *show)
{
    const char *str;
    int num;

    if ((str = tigetstr(cap)) != 0 && (str != (char *) -1)) {
	total_values++;
	total_s_values++;
	if (!q_opt) {
	    printf(FNAME(str), show ? show : cap);
	    while (*str != 0) {
		int ch = UChar(*str++);
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
	    printf("\n");
	}
    } else if ((num = tigetnum(cap)) >= 0) {
	total_values++;
	total_n_values++;
	if (!q_opt) {
	    printf(FNAME(num), show ? show : cap);
	    printf(" %d\n", num);
	}
    } else if ((num = tigetflag(cap)) >= 0) {
	total_values++;
	total_b_values++;
	if (!q_opt) {
	    printf(FNAME(flg), show ? show : cap);
	    printf("%s\n", num ? "true" : "false");
	}
    }

    if (!q_opt)
	fflush(stdout);
}

#define isCapName(c) (isalnum(UChar(c)) || ((c) == '_'))
#define LegalItem(c,n) (n)

static void
brute_force(const char *name)
{
#define MAX_FORCE 5		/* omit "colors", since CPU-time is a problem */
    static const char legal[] = "\
0123456789\
ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz_";
    int length;
    int j, k;
    bool carry;
    bool changed;
    char cap[MAX_FORCE + 1];
    int item[MAX_FORCE + 1];

    if (db_list) {
	putenv(next_dbitem());
    }
    if (!q_opt)
	printf("Terminal type \"%s\"\n", name);
    setupterm((NCURSES_CONST char *) name, 1, (int *) 0);
    if (!q_opt) {
	if (strcmp(name, ttytype))
	    printf("... actual \"%s\"\n", ttytype);
    }

    for (length = 1; length <= MAX_FORCE; ++length) {
	/* set all digits to zeros */
	for (j = 0; j < length; ++j) {
	    item[j] = LegalItem(j, 0);
	}

	do {
	    changed = FALSE;
	    /* copy digits to cap-name */
	    for (j = 0; j < length; ++j) {
		cap[j] = legal[item[j]];
	    }
	    cap[length] = '\0';
	    dumpit(cap, NULL);

	    k = length - 1;
	    do {
		carry = FALSE;
		for (; k >= 0; --k) {
		    item[k] += 1;
		    if (legal[item[k]]) {
			changed = TRUE;
			break;
		    }
		    if (k > 0 &&
			legal[item[k - 1] + 1]) {
			for (j = k; j < length; ++j) {
			    item[j] = LegalItem(j, 0);
			}
			carry = TRUE;
			changed = TRUE;
		    }
		}
	    } while (carry);
	} while (changed);
    }
    del_curterm(cur_term);
}

#if USE_CODE_LISTS
#define fullname(type,n) f_opt ? type##fnames[n] : cap
#else
#define fullname(type,n) cap
#endif

static void
demo_terminfo(char *name)
{
    unsigned n;
    NCURSES_CONST char *cap;

    if (db_list) {
	putenv(next_dbitem());
    }
    if (!q_opt)
	printf("Terminal type \"%s\"\n", name);
    setupterm(name, 1, (int *) 0);

    if (b_opt) {
	for (n = 0;; ++n) {
	    cap = my_boolcodes[n];
	    if (cap == 0)
		break;
	    dumpit(cap, fullname(bool, n));
	}
    }

    if (n_opt) {
	for (n = 0;; ++n) {
	    cap = my_numcodes[n];
	    if (cap == 0)
		break;
	    dumpit(cap, fullname(num, n));
	}
    }

    if (s_opt) {
	for (n = 0;; ++n) {
	    cap = my_strcodes[n];
	    if (cap == 0)
		break;
	    dumpit(cap, fullname(str, n));
	}
    }
#ifdef NCURSES_VERSION
    if (x_opt && (my_blob == 0)) {
	if (y_opt) {
#if NCURSES_XNAMES
	    TERMTYPE *term = (TERMTYPE *) cur_term;
	    if (term != 0
		&& ((NUM_BOOLEANS(term) != BOOLCOUNT)
		    || (NUM_NUMBERS(term) != NUMCOUNT)
		    || (NUM_STRINGS(term) != STRCOUNT))) {
		for (n = BOOLCOUNT; n < NUM_BOOLEANS(term); ++n) {
		    dumpit(ExtBoolname(term, (int) n, boolnames), NULL);
		}
		for (n = NUMCOUNT; n < NUM_NUMBERS(term); ++n) {
		    dumpit(ExtNumname(term, (int) n, numnames), NULL);
		}
		for (n = STRCOUNT; n < NUM_STRINGS(term); ++n) {
		    dumpit(ExtStrname(term, (int) n, strnames), NULL);
		}
	    }
#endif
	} else {
	    char temp[80];
	    static const char *xterm_keys[] =
	    {
		"kDC", "kDN", "kEND", "kHOM", "kIC",
		"kLFT", "kNXT", "kPRV", "kRIT", "kUP",
	    };
	    for (n = 0; n < SIZEOF(xterm_keys); ++n) {
		int mod;
		for (mod = 0; mod < 8; ++mod) {
		    if (mod == 0) {
			/* these happen to be standard - avoid duplicates */
			if (!strcmp(xterm_keys[n], "kDC") ||
			    !strcmp(xterm_keys[n], "kEND") ||
			    !strcmp(xterm_keys[n], "kHOM") ||
			    !strcmp(xterm_keys[n], "kLFT") ||
			    !strcmp(xterm_keys[n], "kRIT")) {
			    continue;
			}
			_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
				    "%.*s", 8, xterm_keys[n]);
		    } else {
			_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp))
				    "%.*s%d", 8, xterm_keys[n], mod);
		    }
		    dumpit(temp, NULL);
		}
	    }
	}
    }
#endif
    del_curterm(cur_term);
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
    size_t j, k, jl;
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
		break;
	    case ',':
		my_blob[k++] = (char) ch;
		break;
	    default:
		if (isalpha(UChar(ch)))
		    state = pName;
		else
		    fprintf(stderr, "OOPS @%d:%.20s\n", __LINE__, my_blob + j);
		my_blob[k++] = (char) ch;
		break;
	    }
	    break;
	case pComment:
	    if (ch == '\n')
		state = pNewline;
	    break;
	case pDescription:
	    switch (ch) {
	    case ',':
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
	    case ',':
		state = pDefault;
		break;
	    case '#':
		state = pNumber;
		break;
	    case '=':
		state = pString;
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
	    case ',':
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
	    case '\n':
		state = pNewline;
		break;
	    case ',':
		state = pDefault;
		my_blob[k++] = (char) ch;
		break;
	    default:
		my_blob[k++] = (char) ch;
		break;
	    }
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
	    case ',':
		my_blob[j] = '\0';
		if (my_blob[j + 1] != '\0' && my_blob[j + 1] != ',')
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
	    if (isalpha(UChar(my_blob[j]))) {
		for (jl = 1; isalnum(UChar(my_blob[j + jl])); ++jl) {
		    ;
		}
	    } else {
		jl = 0;
	    }
	    if (jl != 0) {
		switch (my_blob[j + jl]) {
		case '#':
		    my_numvalues[count_nums] = &my_blob[j + jl + 1];
		    my_numcodes[count_nums++] = &my_blob[j];
		    my_blob[j + jl] = '\0';
		    state = pNumber;
		    j += jl;
		    break;
		case '=':
		    my_strvalues[count_strs] = &my_blob[j + jl + 1];
		    my_strcodes[count_strs++] = &my_blob[j];
		    my_blob[j + jl] = '\0';
		    state = pString;
		    j += jl;
		    break;
		default:
		    if (my_blob[j + jl] == '@') {
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
			my_blob[j + jl] = '\0';
			j += jl + 1;
		    } else {
			my_boolcodes[count_bools++] = &my_blob[j];
			my_blob[j + jl] = '\0';
			j += jl;
		    }
		    state = (isCapName(my_blob[j + 1])
			     ? pName
			     : pDefault);
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
		if (my_blob[j + 1] != '\0') {
		    ++j;
		} else {
		    --j;
		    state = pDefault;
		}
		break;
	    case ',':
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
    printf("# bools:%d\n", (int) count_bools);
    for (j = 0; my_boolcodes[j]; ++j)
	printf("\t%s,\n", my_boolcodes[j]);

    printf("# numbers:%d\n", (int) count_nums);
    for (j = 0; my_numcodes[j]; ++j)
	printf("\t%s#%s,\n", my_numcodes[j], my_numvalues[j]);

    printf("# strings:%d\n", (int) count_strs);
    for (j = 0; my_strcodes[j]; ++j)
	printf("\t%s=%s,\n", my_strcodes[j], my_strvalues[j]);
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
#endif
#endif /* USE_CODE_LISTS */

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: demo_terminfo [options] [terminal]",
	"",
	"If no options are given, print all (boolean, numeric, string)",
	"capabilities for the given terminal, using short names.",
	"",
	"Options:",
	" -a       try all names, print capabilities found",
	" -b       print boolean-capabilities",
	" -d LIST  colon-separated list of databases to use",
	" -e NAME  environment variable to set with -d option",
	" -f       print full names",
	" -i NAME  terminal description to use as names for \"-a\" option",
	" -n       print numeric-capabilities",
	" -q       quiet (prints only counts)",
	" -r COUNT repeat for given count",
	" -s       print string-capabilities",
#ifdef NCURSES_VERSION
	" -x       print extended capabilities",
	" -y       direct-lookup names of extended capabilities",
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
    int repeat;
    char *name;
    int r_opt = 1;
    char *input_name = 0;

    while ((n = getopt(argc, argv, "abd:e:fi:nqr:sxy")) != -1) {
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
	case 'f':
	    f_opt = TRUE;
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
	case 'x':
#ifdef NCURSES_VERSION
	    x_opt = TRUE;
#endif
	    break;
#ifdef NCURSES_VERSION
	case 'y':
	    y_opt = TRUE;
	    x_opt = TRUE;
	    break;
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
	    my_boolcodes = copy_code_list(boolnames);
	    my_numcodes = copy_code_list(numnames);
	    my_strcodes = copy_code_list(strnames);
	}
#else
	else {
	    failed("no capability-lists available (use -i option)");
	}
#endif /* USE_CODE_LISTS */
	for (repeat = 0; repeat < r_opt; ++repeat) {
	    if (optind < argc) {
		for (n = optind; n < argc; ++n) {
		    demo_terminfo(argv[n]);
		}
	    } else if ((name = getenv("TERM")) != 0) {
		demo_terminfo(name);
	    } else {
		static char dumb[] = "dumb";
		demo_terminfo(dumb);
	    }
	}
    }

#define PLURAL(n) n, (n != 1) ? "s" : ""
    printf("%ld value%s (%ld boolean%s, %ld number%s, %ld string%s)\n",
	   PLURAL(total_values),
	   PLURAL(total_b_values),
	   PLURAL(total_n_values),
	   PLURAL(total_s_values));

#if NO_LEAKS
    free_dblist();
    if (input_name != 0) {
	if (my_blob != 0) {
	    free(my_blob);
	    free(my_boolcodes);
	    free(my_numcodes);
	    free(my_numvalues);
	    free(my_strcodes);
	    free(my_strvalues);
	}
    }
#if USE_CODE_LISTS
    else {
	free_code_list(my_boolcodes);
	free_code_list(my_numcodes);
	free_code_list(my_strcodes);
    }
#endif
#endif /* NO_LEAKS */

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
