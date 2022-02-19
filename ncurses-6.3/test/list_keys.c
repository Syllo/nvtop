/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 2016,2017 Free Software Foundation, Inc.                       *
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
 * $Id: list_keys.c,v 1.27 2021/03/27 23:41:21 tom Exp $
 *
 * Author: Thomas E Dickey
 *
 * List function keys for one or more terminals.
 */

#define USE_TINFO
#include <test.priv.h>

#if NCURSES_XNAMES
#if HAVE_TERM_ENTRY_H
#include <term_entry.h>
#else
#undef NCURSES_XNAMES
#define NCURSES_XNAMES 0
#endif
#endif

#if HAVE_TIGETSTR
#if defined(HAVE_CURSES_DATA_BOOLNAMES) || defined(DECL_CURSES_DATA_BOOLNAMES)

static bool f_opt = FALSE;
static bool m_opt = FALSE;
static bool t_opt = FALSE;
static bool x_opt = FALSE;

typedef enum {
    ktCursor
    ,ktFunction
    ,ktOther
#if HAVE_USE_EXTENDED_NAMES
    ,ktExtended
#endif
} KEYTYPE;

typedef struct {
    KEYTYPE type;
    const char *name;
} KEYNAMES;

#define Type(n) list[n].type
#define Name(n) list[n].name

static const char *
full_name(const char *name)
{
    const char *result = name;
    int n;
    for (n = 0; strnames[n] != 0; ++n) {
	if (!strcmp(name, strnames[n])) {
	    result = strfnames[n];
	    break;
	}
    }
    return result;
}

static int
show_key(const char *name, bool show)
{
    int width = 0;
    NCURSES_CONST char *value = tigetstr((NCURSES_CONST char *) name);

    if (show && t_opt)
	fputc('"', stdout);

    if (value != 0 && value != (char *) -1) {
	while (*value != 0) {
	    char buffer[10];
	    int ch = UChar(*value++);
	    switch (ch) {
	    case '\177':
		_nc_STRCPY(buffer, "^?", sizeof(buffer));
		break;
	    case '\033':
		_nc_STRCPY(buffer, "\\E", sizeof(buffer));
		break;
	    case '\b':
		_nc_STRCPY(buffer, "\\b", sizeof(buffer));
		break;
	    case '\f':
		_nc_STRCPY(buffer, "\\f", sizeof(buffer));
		break;
	    case '\n':
		_nc_STRCPY(buffer, "\\n", sizeof(buffer));
		break;
	    case '\r':
		_nc_STRCPY(buffer, "\\r", sizeof(buffer));
		break;
	    case ' ':
		_nc_STRCPY(buffer, "\\s", sizeof(buffer));
		break;
	    case '\t':
		_nc_STRCPY(buffer, "\\t", sizeof(buffer));
		break;
	    case '^':
		_nc_STRCPY(buffer, "\\^", sizeof(buffer));
		break;
	    case ':':
		_nc_STRCPY(buffer, "\\072", sizeof(buffer));
		break;
	    case '\\':
		_nc_STRCPY(buffer, "\\\\", sizeof(buffer));
		break;
	    default:
		if (t_opt && ch == '"') {
		    _nc_STRCPY(buffer, "\"\"", sizeof(buffer));
		} else if (isgraph(ch)) {
		    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
				"%c", ch);
		} else if (ch < 32) {
		    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
				"^%c", ch + '@');
		} else {
		    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
				"\\%03o", ch);
		}
		break;
	    }
	    width += (int) strlen(buffer);
	    if (show)
		fputs(buffer, stdout);
	}
    }

    if (show && t_opt)
	fputc('"', stdout);

    return width;
}

static bool
valid_key(const char *name, TERMINAL **terms, int count)
{
    bool result = FALSE;
    if (*name == 'k') {
	int k;
	for (k = 0; k < count; ++k) {
	    set_curterm(terms[k]);
	    if (show_key(name, FALSE)) {
		result = TRUE;
		break;
	    }
	}
    }
    return result;
}

static int
compare_keys(const void *a, const void *b)
{
    const KEYNAMES *p = (const KEYNAMES *) a;
    const KEYNAMES *q = (const KEYNAMES *) b;
    int result = (int) (p->type - q->type);
    int pn, qn;
    if (result == 0) {
	if (p->type == ktFunction &&
	    sscanf(p->name, "kf%d", &pn) == 1 &&
	    sscanf(q->name, "kf%d", &qn) == 1) {
	    result = (pn - qn);
	} else {
	    result = strcmp(p->name, q->name);
	}
    }
    return result;
}

static void
draw_line(int width)
{
    if (!t_opt) {
	int j;
	for (j = 0; j < width; ++j) {
	    printf("-");
	}
	printf("\n");
    }
}

static const char *
modified_key(const char *name)
{
    static char result[100];
    char buffer[sizeof(result) - 10];
    int value;
    char chr;
    static const char *modifiers[][2] =
    {
	{"", ""},
	{"s-", "shift-"},
	{"a-", "alt-"},
	{"as-", "alt-shift-"},
	{"c-", "ctrl-"},
	{"sc-", "ctrl-shift-"},
	{"ac-", "alt-ctrl-"},
	{"acs-" "alt-ctrl-shift-"},
    };

    if (strlen(name) > (sizeof(result) - 3)) {
	*result = '\0';
    } else if (sscanf(name, "kf%d%c", &value, &chr) == 1 &&
	       value >= 1 &&
	       value <= 63) {
	/* map 1,2,3,4,5,6,7 to 1,2,5,... */
	int map = ((value - 1) / 12);
	int key = ((value - 1) % 12);
	int bit1 = (map & 2);
	int bit2 = (map & 4);
	map &= ~6;
	map |= (bit1 << 1) | (bit2 >> 1);
	_nc_SPRINTF(result, _nc_SLIMIT(sizeof(result))
		    "%sF%d", modifiers[map][(unsigned) f_opt], 1 + key);
    } else if (sscanf(name, "k%80[A-Z]%d%c", buffer, &value, &chr) == 2 &&
	       (value > 1 &&
		value <= 8) &&
	       (!strcmp(buffer, "UP") ||
		!strcmp(buffer, "DN") ||
		!strcmp(buffer, "LFT") ||
		!strcmp(buffer, "RIT") ||
		!strcmp(buffer, "IC") ||
		!strcmp(buffer, "DC") ||
		!strcmp(buffer, "HOM") ||
		!strcmp(buffer, "END") ||
		!strcmp(buffer, "NXT") ||
		!strcmp(buffer, "PRV"))) {
	_nc_SPRINTF(result, _nc_SLIMIT(sizeof(result))
		    "%sk%s", modifiers[value - 1][(unsigned) f_opt], buffer);
    } else if (sscanf(name, "k%80[A-Z]%c", buffer, &chr) == 1 &&
	       (!strcmp(buffer, "UP") ||
		!strcmp(buffer, "DN"))) {
	_nc_SPRINTF(result, _nc_SLIMIT(sizeof(result))
		    "%sk%s", modifiers[1][(unsigned) f_opt], buffer);
    } else {
	*result = '\0';
    }
    return result;
}

static void
list_keys(TERMINAL **terms, int count)
{
    int j, k;
    int widths0 = 0;
    int widths1 = 0;
    int widths2 = 0;
    int widthsx;
    int check;
    size_t total = 0;
    size_t actual = 0;
    const char *name = f_opt ? "strfname" : "strname";
    const char *modifier = "extended";
    KEYNAMES *list;

    for (total = 0; strnames[total]; ++total) {
	;
    }
#if NCURSES_XNAMES
    if (x_opt) {
	for (k = 0; k < count; ++k) {
	    TERMTYPE *term;
	    set_curterm(terms[k]);
	    term = (TERMTYPE *) cur_term;
	    total += (size_t) (NUM_STRINGS(term) - STRCOUNT);
	}
    }
#endif
    list = typeCalloc(KEYNAMES, total + 1);
    for (j = 0; strnames[j]; ++j) {
	Type(j) = ktOther;
	if (sscanf(strnames[j], "kf%d", &k) == 1) {
	    Type(j) = ktFunction;
	} else if (!(strncmp) (strnames[j], "kcu", 3)) {
	    Type(j) = ktCursor;
	}
	Name(j) = strnames[j];
    }
#if NCURSES_XNAMES
    if (x_opt) {
	int m, n;

	for (k = 0; k < count; ++k) {
	    TERMTYPE *term;

	    set_curterm(terms[k]);
	    term = (TERMTYPE *) cur_term;
	    for (n = STRCOUNT; n < NUM_STRINGS(term); ++n) {
		bool found = FALSE;
		const char *estr = ExtStrname(term, (int) n, strnames);
		for (m = STRCOUNT; m < j; ++m) {
		    if (!strcmp(estr, Name(m))) {
			found = TRUE;
			break;
		    }
		}
		if (!found) {
		    Type(j) = ktExtended;
		    Name(j++) = estr;
		}
	    }
	}
    }
#endif
    actual = (size_t) j;
    qsort(list, actual, sizeof(KEYNAMES), compare_keys);

    widths0 = (int) strlen(name);
    if (m_opt)
	widths1 = (int) strlen(modifier);

    for (k = 0; k < count; ++k) {
	set_curterm(terms[k]);
	check = (int) strlen(termname());
	if (widths2 < check)
	    widths2 = check;
    }
    for (j = 0; Name(j) != 0; ++j) {
	if (valid_key(Name(j), terms, count)) {
	    const char *label = f_opt ? full_name(Name(j)) : Name(j);
	    check = (int) strlen(label);
	    if (widths0 < check)
		widths0 = check;
	    for (k = 0; k < count; ++k) {
		set_curterm(terms[k]);
		check = show_key(Name(j), FALSE) + 1;
		if (widths2 < check)
		    widths2 = check;
		if (m_opt) {
		    check = (int) strlen(modified_key(Name(j)));
		    if (widths1 < check)
			widths1 = check;
		}
	    }
	}
    }

    if (t_opt) {
	printf("\"%s\"", name);
	if (m_opt)
	    printf(",\"%s\"", modifier);
    } else {
	printf("%-*s", widths0, name);
	if (m_opt)
	    printf(" %-*s", widths1, modifier);
    }
    for (k = 0; k < count; ++k) {
	set_curterm(terms[k]);
	if (t_opt) {
	    printf(",\"%s\"", termname());
	} else if (k + 1 >= count) {
	    printf(" %s", termname());
	} else {
	    printf(" %-*s", widths2, termname());
	}
    }
    printf("\n");

    widthsx = widths0 + ((count + 1) * widths2);

    for (j = 0; Name(j) != 0; ++j) {
	if (j == 0 || (Type(j) != Type(j - 1)))
	    draw_line(widthsx);
	if (valid_key(Name(j), terms, count)) {
	    const char *label = f_opt ? full_name(Name(j)) : Name(j);
	    if (t_opt) {
		printf("\"%s\"", label);
		if (m_opt)
		    printf(",\"%s\"", modified_key(Name(j)));
	    } else {
		printf("%-*s", widths0, label);
		if (m_opt)
		    printf(" %-*s", widths1, modified_key(Name(j)));
	    }
	    for (k = 0; k < count; ++k) {
		printf(t_opt ? "," : " ");
		set_curterm(terms[k]);
		check = show_key(Name(j), TRUE);
		if (!t_opt) {
		    if (k + 1 < count) {
			printf("%*s", widths2 - check, " ");
		    }
		}
	    }
	    printf("\n");
	}
    }
    free(list);
}

static void
usage(void)
{
    static const char *msg[] =
    {
	"Usage: list_keys [options] [terminal [terminal2 [...]]]",
	"",
	"Print capabilities for terminal special keys.",
	"",
	"Options:",
	" -f       print full names",
	" -m       print modifier-column for shift/control keys",
	" -t       print result as CSV table",
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
    TERMINAL **terms = typeCalloc(TERMINAL *, argc + 1);

    while ((n = getopt(argc, argv, "fmtx")) != -1) {
	switch (n) {
	case 'f':
	    f_opt = TRUE;
	    break;
	case 'm':
	    m_opt = TRUE;
	    break;
	case 't':
	    t_opt = TRUE;
	    break;
#ifdef NCURSES_VERSION
	case 'x':
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

    if (optind < argc) {
	int found = 0;
	int status;
	for (n = optind; n < argc; ++n) {
	    setupterm((NCURSES_CONST char *) argv[n], 1, &status);
	    if (status > 0 && cur_term != 0) {
		terms[found++] = cur_term;
	    }
	}
	if (found)
	    list_keys(terms, found);
    } else {
	setupterm(NULL, 1, (int *) 0);
	terms[0] = cur_term;
	list_keys(terms, 1);
    }

    free(terms);

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
