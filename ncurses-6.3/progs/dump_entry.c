/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
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
 *     and: Thomas E. Dickey                        1996 on                 *
 ****************************************************************************/

#define __INTERNAL_CAPS_VISIBLE
#include <progs.priv.h>

#include <dump_entry.h>
#include <termsort.h>		/* this C file is generated */
#include <parametrized.h>	/* so is this */

MODULE_ID("$Id: dump_entry.c,v 1.189 2021/09/04 10:29:59 tom Exp $")

#define DISCARD(string) string = ABSENT_STRING
#define PRINTF (void) printf
#define WRAPPED 32

#define OkIndex(index,array) ((int)(index) >= 0 && (int)(index) < (int) SIZEOF(array))
#define TcOutput() (outform == F_TERMCAP || outform == F_TCONVERR)

typedef struct {
    char *text;
    size_t used;
    size_t size;
} DYNBUF;

static int tversion;		/* terminfo version */
static int outform;		/* output format to use */
static int sortmode;		/* sort mode to use */
static int width = 60;		/* max line width for listings */
static int height = 65535;	/* max number of lines for listings */
static int column;		/* current column, limited by 'width' */
static int oldcol;		/* last value of column before wrap */
static bool pretty;		/* true if we format if-then-else strings */
static bool wrapped;		/* true if we wrap too-long strings */
static bool did_wrap;		/* true if last wrap_concat did wrapping */
static bool checking;		/* true if we are checking for tic */
static int quickdump;		/* true if we are dumping compiled data */

static char *save_sgr;

static DYNBUF outbuf;
static DYNBUF tmpbuf;

/* indirection pointers for implementing sort and display modes */
static const PredIdx *bool_indirect, *num_indirect, *str_indirect;
static NCURSES_CONST char *const *bool_names;
static NCURSES_CONST char *const *num_names;
static NCURSES_CONST char *const *str_names;

static const char *separator = "", *trailer = "";
static int indent = 8;

/* cover various ports and variants of terminfo */
#define V_ALLCAPS	0	/* all capabilities (SVr4, XSI, ncurses) */
#define V_SVR1		1	/* SVR1, Ultrix */
#define V_HPUX		2	/* HP-UX */
#define V_AIX		3	/* AIX */
#define V_BSD		4	/* BSD */

#if NCURSES_XNAMES
#define OBSOLETE(n) (!_nc_user_definable && (n[0] == 'O' && n[1] == 'T'))
#else
#define OBSOLETE(n) (n[0] == 'O' && n[1] == 'T')
#endif

#define isObsolete(f,n) ((f == F_TERMINFO || f == F_VARIABLE) && (sortmode != S_VARIABLE) && OBSOLETE(n))

#if NCURSES_XNAMES
#define BoolIndirect(j) ((j >= BOOLCOUNT) ? (j) : ((sortmode == S_NOSORT) ? j : bool_indirect[j]))
#define NumIndirect(j)  ((j >= NUMCOUNT)  ? (j) : ((sortmode == S_NOSORT) ? j : num_indirect[j]))
#define StrIndirect(j)  ((j >= STRCOUNT)  ? (j) : ((sortmode == S_NOSORT) ? j : str_indirect[j]))
#else
#define BoolIndirect(j) ((sortmode == S_NOSORT) ? (j) : bool_indirect[j])
#define NumIndirect(j)  ((sortmode == S_NOSORT) ? (j) : num_indirect[j])
#define StrIndirect(j)  ((sortmode == S_NOSORT) ? (j) : str_indirect[j])
#endif

static GCC_NORETURN void
failed(const char *s)
{
    perror(s);
    ExitProgram(EXIT_FAILURE);
}

static void
strncpy_DYN(DYNBUF * dst, const char *src, size_t need)
{
    size_t want = need + dst->used + 1;
    if (want > dst->size) {
	dst->size += (want + 1024);	/* be generous */
	dst->text = typeRealloc(char, dst->size, dst->text);
	if (dst->text == 0)
	    failed("strncpy_DYN");
    }
    _nc_STRNCPY(dst->text + dst->used, src, need + 1);
    dst->used += need;
    dst->text[dst->used] = 0;
}

static void
strcpy_DYN(DYNBUF * dst, const char *src)
{
    if (src == 0) {
	dst->used = 0;
	strcpy_DYN(dst, "");
    } else {
	strncpy_DYN(dst, src, strlen(src));
    }
}

#if NO_LEAKS
static void
free_DYN(DYNBUF * p)
{
    if (p->text != 0)
	free(p->text);
    p->text = 0;
    p->size = 0;
    p->used = 0;
}

void
_nc_leaks_dump_entry(void)
{
    free_DYN(&outbuf);
    free_DYN(&tmpbuf);
}
#endif

#define NameTrans(check,result) \
	    if ((np->nte_index <= OK_ ## check) \
		&& check[np->nte_index]) \
		return (result[np->nte_index])

NCURSES_CONST char *
nametrans(const char *name)
/* translate a capability name to termcap from terminfo */
{
    const struct name_table_entry *np;

    if ((np = _nc_find_entry(name, _nc_get_hash_table(0))) != 0) {
	switch (np->nte_type) {
	case BOOLEAN:
	    NameTrans(bool_from_termcap, boolcodes);
	    break;

	case NUMBER:
	    NameTrans(num_from_termcap, numcodes);
	    break;

	case STRING:
	    NameTrans(str_from_termcap, strcodes);
	    break;
	}
    }

    return (0);
}

void
dump_init(const char *version,
	  int mode,
	  int sort,
	  bool wrap_strings,
	  int twidth,
	  int theight,
	  unsigned traceval,
	  bool formatted,
	  bool check,
	  int quick)
/* set up for entry display */
{
    width = twidth;
    height = theight;
    pretty = formatted;
    wrapped = wrap_strings;
    checking = check;
    quickdump = (quick & 3);

    did_wrap = (width <= 0);

    /* versions */
    if (version == 0)
	tversion = V_ALLCAPS;
    else if (!strcmp(version, "SVr1") || !strcmp(version, "SVR1")
	     || !strcmp(version, "Ultrix"))
	tversion = V_SVR1;
    else if (!strcmp(version, "HP"))
	tversion = V_HPUX;
    else if (!strcmp(version, "AIX"))
	tversion = V_AIX;
    else if (!strcmp(version, "BSD"))
	tversion = V_BSD;
    else
	tversion = V_ALLCAPS;

    /* implement display modes */
    switch (outform = mode) {
    case F_LITERAL:
    case F_TERMINFO:
	bool_names = boolnames;
	num_names = numnames;
	str_names = strnames;
	separator = (twidth > 0 && theight > 1) ? ", " : ",";
	trailer = "\n\t";
	break;

    case F_VARIABLE:
	bool_names = boolfnames;
	num_names = numfnames;
	str_names = strfnames;
	separator = (twidth > 0 && theight > 1) ? ", " : ",";
	trailer = "\n\t";
	break;

    case F_TERMCAP:
    case F_TCONVERR:
	bool_names = boolcodes;
	num_names = numcodes;
	str_names = strcodes;
	separator = ":";
	trailer = "\\\n\t:";
	break;
    }
    indent = 8;

    /* implement sort modes */
    switch (sortmode = sort) {
    case S_NOSORT:
	if (traceval)
	    (void) fprintf(stderr,
			   "%s: sorting by term structure order\n", _nc_progname);
	break;

    case S_TERMINFO:
	if (traceval)
	    (void) fprintf(stderr,
			   "%s: sorting by terminfo name order\n", _nc_progname);
	bool_indirect = bool_terminfo_sort;
	num_indirect = num_terminfo_sort;
	str_indirect = str_terminfo_sort;
	break;

    case S_VARIABLE:
	if (traceval)
	    (void) fprintf(stderr,
			   "%s: sorting by C variable order\n", _nc_progname);
	bool_indirect = bool_variable_sort;
	num_indirect = num_variable_sort;
	str_indirect = str_variable_sort;
	break;

    case S_TERMCAP:
	if (traceval)
	    (void) fprintf(stderr,
			   "%s: sorting by termcap name order\n", _nc_progname);
	bool_indirect = bool_termcap_sort;
	num_indirect = num_termcap_sort;
	str_indirect = str_termcap_sort;
	break;
    }

    if (traceval)
	(void) fprintf(stderr,
		       "%s: width = %d, tversion = %d, outform = %d\n",
		       _nc_progname, width, tversion, outform);
}

static TERMTYPE2 *cur_type;

static int
dump_predicate(PredType type, PredIdx idx)
/* predicate function to use for ordinary decompilation */
{
    switch (type) {
    case BOOLEAN:
	return (cur_type->Booleans[idx] == FALSE)
	    ? FAIL : cur_type->Booleans[idx];

    case NUMBER:
	return (cur_type->Numbers[idx] == ABSENT_NUMERIC)
	    ? FAIL : cur_type->Numbers[idx];

    case STRING:
	return (cur_type->Strings[idx] != ABSENT_STRING)
	    ? (int) TRUE : FAIL;
    }

    return (FALSE);		/* pacify compiler */
}

static void set_obsolete_termcaps(TERMTYPE2 *tp);

/* is this the index of a function key string? */
#define FNKEY(i) \
    (((i) >= STR_IDX(key_f0) && \
      (i) <= STR_IDX(key_f9)) || \
     ((i) >= STR_IDX(key_f11) && \
      (i) <= STR_IDX(key_f63)))

/*
 * If we configure with a different Caps file, the offsets into the arrays
 * will change.  So we use an address expression.
 */
#define BOOL_IDX(name) (PredType) (&(name) - &(CUR Booleans[0]))
#define NUM_IDX(name)  (PredType) (&(name) - &(CUR Numbers[0]))
#define STR_IDX(name)  (PredType) (&(name) - &(CUR Strings[0]))

static bool
version_filter(PredType type, PredIdx idx)
/* filter out capabilities we may want to suppress */
{
    switch (tversion) {
    case V_ALLCAPS:		/* SVr4, XSI Curses */
	return (TRUE);

    case V_SVR1:		/* System V Release 1, Ultrix */
	switch (type) {
	case BOOLEAN:
	    return ((idx <= BOOL_IDX(xon_xoff)) ? TRUE : FALSE);
	case NUMBER:
	    return ((idx <= NUM_IDX(width_status_line)) ? TRUE : FALSE);
	case STRING:
	    return ((idx <= STR_IDX(prtr_non)) ? TRUE : FALSE);
	}
	break;

    case V_HPUX:		/* Hewlett-Packard */
	switch (type) {
	case BOOLEAN:
	    return ((idx <= BOOL_IDX(xon_xoff)) ? TRUE : FALSE);
	case NUMBER:
	    return ((idx <= NUM_IDX(label_width)) ? TRUE : FALSE);
	case STRING:
	    if (idx <= STR_IDX(prtr_non))
		return (TRUE);
	    else if (FNKEY(idx))	/* function keys */
		return (TRUE);
	    else if (idx == STR_IDX(plab_norm)
		     || idx == STR_IDX(label_on)
		     || idx == STR_IDX(label_off))
		return (TRUE);
	    else
		return (FALSE);
	}
	break;

    case V_AIX:		/* AIX */
	switch (type) {
	case BOOLEAN:
	    return ((idx <= BOOL_IDX(xon_xoff)) ? TRUE : FALSE);
	case NUMBER:
	    return ((idx <= NUM_IDX(width_status_line)) ? TRUE : FALSE);
	case STRING:
	    if (idx <= STR_IDX(prtr_non))
		return (TRUE);
	    else if (FNKEY(idx))	/* function keys */
		return (TRUE);
	    else
		return (FALSE);
	}
	break;

#define is_termcap(type) (OkIndex(idx, type##_from_termcap) && \
			  type##_from_termcap[idx])

    case V_BSD:		/* BSD */
	switch (type) {
	case BOOLEAN:
	    return is_termcap(bool);
	case NUMBER:
	    return is_termcap(num);
	case STRING:
	    return is_termcap(str);
	}
	break;
    }

    return (FALSE);		/* pacify the compiler */
}

static void
trim_trailing(void)
{
    while (outbuf.used > 0 && outbuf.text[outbuf.used - 1] == ' ')
	outbuf.text[--outbuf.used] = '\0';
}

static void
force_wrap(void)
{
    oldcol = column;
    trim_trailing();
    strcpy_DYN(&outbuf, trailer);
    column = indent;
}

static int
op_length(const char *src, int offset)
{
    int result = 0;

    if (offset > 0 && src[offset - 1] == '\\') {
	result = 0;
    } else {
	int ch;

	result++;		/* for '%' mark */
	ch = src[offset + result];
	if (TcOutput()) {
	    if (ch == '>') {
		result += 3;
	    } else if (ch == '+') {
		result += 2;
	    } else {
		result++;
	    }
	} else if (ch == '\'') {
	    result += 3;
	} else if (ch == L_CURL[0]) {
	    int n = result;
	    while ((ch = src[offset + n]) != '\0') {
		if (ch == R_CURL[0]) {
		    result = ++n;
		    break;
		}
		n++;
	    }
	} else if (strchr("pPg", ch) != 0) {
	    result += 2;
	} else {
	    result++;		/* ordinary operator */
	}
    }
    return result;
}

/*
 * When wrapping too-long strings, avoid splitting a backslash sequence, or
 * a terminfo '%' operator.  That will leave things a little ragged, but avoids
 * a stray backslash at the end of the line, as well as making the result a
 * little more readable.
 */
static int
find_split(const char *src, int step, int size)
{
    int result = size;

    if (size > 0) {
	/* check if that would split a backslash-sequence */
	int mark = size;
	int n;

	for (n = size - 1; n > 0; --n) {
	    int ch = UChar(src[step + n]);
	    if (ch == '\\') {
		if (n > 0 && src[step + n - 1] == ch)
		    --n;
		mark = n;
		break;
	    } else if (!isalnum(ch)) {
		break;
	    }
	}
	if (mark < size) {
	    result = mark;
	} else {
	    /* check if that would split a backslash-sequence */
	    for (n = size - 1; n > 0; --n) {
		int ch = UChar(src[step + n]);
		if (ch == '%') {
		    int need = op_length(src, step + n);
		    if ((n + need) > size) {
			mark = n;
		    }
		    break;
		}
	    }
	    if (mark < size) {
		result = mark;
	    }
	}
    }
    return result;
}

/*
 * If we are going to wrap lines, we cannot leave literal spaces because that
 * would be ambiguous if we split on that space.
 */
static char *
fill_spaces(const char *src)
{
    const char *fill = "\\s";
    size_t need = strlen(src);
    size_t size = strlen(fill);
    char *result = 0;
    int pass;
    size_t s, d;
    for (pass = 0; pass < 2; ++pass) {
	for (s = d = 0; src[s] != '\0'; ++s) {
	    if (src[s] == ' ') {
		if (pass) {
		    _nc_STRCPY(&result[d], fill, need + 1 - d);
		    d += size;
		} else {
		    need += size;
		}
	    } else {
		if (pass) {
		    result[d++] = src[s];
		} else {
		    ++d;
		}
	    }
	}
	if (pass) {
	    result[d] = '\0';
	} else {
	    result = calloc(need + 1, sizeof(char));
	    if (result == 0)
		failed("fill_spaces");
	}
    }
    return result;
}

typedef enum {
    wOFF = 0
    ,w1ST = 1
    ,w2ND = 2
    ,wEND = 4
    ,wERR = 8
} WRAPMODE;

#define wrap_1ST(mode) ((mode)&w1ST)
#define wrap_END(mode) ((mode)&wEND)
#define wrap_ERR(mode) ((mode)&wERR)

static void
wrap_concat(const char *src, int need, unsigned mode)
{
    int gaps = (int) strlen(separator);
    int want = gaps + need;

    did_wrap = (width <= 0);
    if (wrap_1ST(mode)
	&& column > indent
	&& column + want > width) {
	force_wrap();
    }
    if ((wrap_END(mode) && !wrap_ERR(mode)) &&
	wrapped &&
	(width >= 0) &&
	(column + want) > width) {
	int step = 0;
	int used = width > WRAPPED ? width : WRAPPED;
	int base = 0;
	char *p, align[9];
	const char *my_t = trailer;
	char *fill = fill_spaces(src);
	int last = (int) strlen(fill);

	need = last;

	if (TcOutput())
	    trailer = "\\\n\t ";

	if (!TcOutput() && (p = strchr(fill, '=')) != 0) {
	    base = (int) (p + 1 - fill);
	    if (base > 8)
		base = 8;
	    _nc_SPRINTF(align, _nc_SLIMIT(align) "%*s", base, " ");
	} else if (column > 8) {
	    base = column - 8;
	    if (base > 8)
		base = 8;
	    _nc_SPRINTF(align, _nc_SLIMIT(align) "%*s", base, " ");
	} else {
	    align[base] = '\0';
	}
	/* "pretty" overrides wrapping if it already split the line */
	if (!pretty || strchr(fill, '\n') == 0) {
	    int tag = 0;

	    if (TcOutput() && outbuf.used && !wrap_1ST(mode)) {
		tag = 3;
	    }

	    while ((column + (need + gaps)) > used) {
		int size = used - tag;
		if (step) {
		    strcpy_DYN(&outbuf, align);
		    size -= base;
		}
		if (size > (last - step)) {
		    size = (last - step);
		}
		size = find_split(fill, step, size);
		strncpy_DYN(&outbuf, fill + step, (size_t) size);
		step += size;
		need -= size;
		if (need > 0) {
		    force_wrap();
		    did_wrap = TRUE;
		    tag = 0;
		}
	    }
	}
	if (need > 0) {
	    if (step)
		strcpy_DYN(&outbuf, align);
	    strcpy_DYN(&outbuf, fill + step);
	}
	if (wrap_END(mode))
	    strcpy_DYN(&outbuf, separator);
	trailer = my_t;
	force_wrap();

	free(fill);
    } else {
	strcpy_DYN(&outbuf, src);
	if (wrap_END(mode))
	    strcpy_DYN(&outbuf, separator);
	column += (int) strlen(src);
    }
}

static void
wrap_concat1(const char *src)
{
    int need = (int) strlen(src);
    wrap_concat(src, need, w1ST | wEND);
}

static void
wrap_concat3(const char *name, const char *eqls, const char *value)
{
    int nlen = (int) strlen(name);
    int elen = (int) strlen(eqls);
    int vlen = (int) strlen(value);

    wrap_concat(name, nlen + elen + vlen, w1ST);
    wrap_concat(eqls, elen + vlen, w2ND);
    wrap_concat(value, vlen, wEND);
}

#define IGNORE_SEP_TRAIL(first,last,sep_trail) \
	if ((size_t)(last - first) > sizeof(sep_trail)-1 \
	 && !strncmp(first, sep_trail, sizeof(sep_trail)-1)) \
		first += sizeof(sep_trail)-2

/* Returns the nominal length of the buffer assuming it is termcap format,
 * i.e., the continuation sequence is treated as a single character ":".
 *
 * There are several implementations of termcap which read the text into a
 * fixed-size buffer.  Generally they strip the newlines from the text, but may
 * not do it until after the buffer is read.  Also, "tc=" resolution may be
 * expanded in the same buffer.  This function is useful for measuring the size
 * of the best fixed-buffer implementation; the worst case may be much worse.
 */
#ifdef TEST_TERMCAP_LENGTH
static int
termcap_length(const char *src)
{
    static const char pattern[] = ":\\\n\t:";

    int len = 0;
    const char *const t = src + strlen(src);

    while (*src != '\0') {
	IGNORE_SEP_TRAIL(src, t, pattern);
	src++;
	len++;
    }
    return len;
}
#else
#define termcap_length(src) strlen(src)
#endif

static void
indent_DYN(DYNBUF * buffer, int level)
{
    int n;

    for (n = 0; n < level; n++)
	strncpy_DYN(buffer, "\t", (size_t) 1);
}

/*
 * Check if the current line which was begun consists only of a tab and the
 * given leading text.
 */
static bool
leading_DYN(DYNBUF * buffer, const char *leading)
{
    bool result = FALSE;
    size_t need = strlen(leading);
    if (buffer->used > need) {
	need = buffer->used - need;
	if (!strcmp(buffer->text + need, leading)) {
	    result = TRUE;
	    while (--need != 0) {
		if (buffer->text[need] == '\n') {
		    break;
		}
		if (buffer->text[need] != '\t') {
		    result = FALSE;
		    break;
		}
	    }
	}
    }
    return result;
}

bool
has_params(const char *src, bool formatting)
{
    bool result = FALSE;
    int len = (int) strlen(src);
    int n;
    bool ifthen = FALSE;
    bool params = FALSE;

    for (n = 0; n < len - 1; ++n) {
	if (!strncmp(src + n, "%p", (size_t) 2)) {
	    params = TRUE;
	} else if (!strncmp(src + n, "%;", (size_t) 2)) {
	    ifthen = TRUE;
	    result = params;
	    break;
	}
    }
    if (!ifthen) {
	if (formatting) {
	    result = ((len > 50) && params);
	} else {
	    result = params;
	}
    }
    return result;
}

static char *
fmt_complex(TERMTYPE2 *tterm, const char *capability, char *src, int level)
{
    bool percent = FALSE;
    bool params = has_params(src, TRUE);

    while (*src != '\0') {
	switch (*src) {
	case '^':
	    percent = FALSE;
	    strncpy_DYN(&tmpbuf, src++, (size_t) 1);
	    break;
	case '\\':
	    percent = FALSE;
	    strncpy_DYN(&tmpbuf, src++, (size_t) 1);
	    break;
	case '%':
	    percent = TRUE;
	    break;
	case '?':		/* "if" */
	case 't':		/* "then" */
	case 'e':		/* "else" */
	    if (percent) {
		percent = FALSE;
		tmpbuf.text[tmpbuf.used - 1] = '\n';
		/* treat a "%e" as else-if, on the same level */
		if (*src == 'e') {
		    indent_DYN(&tmpbuf, level);
		    strncpy_DYN(&tmpbuf, "%", (size_t) 1);
		    strncpy_DYN(&tmpbuf, src, (size_t) 1);
		    src++;
		    params = has_params(src, TRUE);
		    if (!params && *src != '\0' && *src != '%') {
			strncpy_DYN(&tmpbuf, "\n", (size_t) 1);
			indent_DYN(&tmpbuf, level + 1);
		    }
		} else {
		    indent_DYN(&tmpbuf, level + 1);
		    strncpy_DYN(&tmpbuf, "%", (size_t) 1);
		    strncpy_DYN(&tmpbuf, src, (size_t) 1);
		    if (*src++ == '?') {
			src = fmt_complex(tterm, capability, src, level + 1);
			if (*src != '\0' && *src != '%') {
			    strncpy_DYN(&tmpbuf, "\n", (size_t) 1);
			    indent_DYN(&tmpbuf, level + 1);
			}
		    } else if (level == 1) {
			if (checking)
			    _nc_warning("%s: %%%c without %%? in %s",
					_nc_first_name(tterm->term_names),
					*src, capability);
		    }
		}
		continue;
	    }
	    break;
	case ';':		/* "endif" */
	    if (percent) {
		percent = FALSE;
		if (level > 1) {
		    tmpbuf.text[tmpbuf.used - 1] = '\n';
		    indent_DYN(&tmpbuf, level);
		    strncpy_DYN(&tmpbuf, "%", (size_t) 1);
		    strncpy_DYN(&tmpbuf, src++, (size_t) 1);
		    if (src[0] == '%'
			&& src[1] != '\0'
			&& (strchr("?e;", src[1])) == 0) {
			tmpbuf.text[tmpbuf.used++] = '\n';
			indent_DYN(&tmpbuf, level);
		    }
		    return src;
		}
		if (checking)
		    _nc_warning("%s: %%; without %%? in %s",
				_nc_first_name(tterm->term_names),
				capability);
	    }
	    break;
	case 'p':
	    if (percent && params && !leading_DYN(&tmpbuf, "%")) {
		tmpbuf.text[tmpbuf.used - 1] = '\n';
		indent_DYN(&tmpbuf, level + 1);
		strncpy_DYN(&tmpbuf, "%", (size_t) 1);
	    }
	    params = FALSE;
	    percent = FALSE;
	    break;
	case ' ':
	    strncpy_DYN(&tmpbuf, "\\s", (size_t) 2);
	    ++src;
	    continue;
	default:
	    percent = FALSE;
	    break;
	}
	strncpy_DYN(&tmpbuf, src++, (size_t) 1);
    }
    return src;
}

/*
 * Make "large" numbers a little easier to read by showing them in hexadecimal
 * if they are "close" to a power of two.
 */
static const char *
number_format(int value)
{
    const char *result = "%d";

    if ((outform != F_TERMCAP) && (value > 255)) {
	unsigned long lv = (unsigned long) value;
	int bits = sizeof(unsigned long) * 8;
	int nn;

	for (nn = 8; nn < bits; ++nn) {
	    unsigned long mm;

	    mm = 1UL << nn;
	    if ((mm - 16) <= lv && (mm + 16) > lv) {
		result = "%#x";
		break;
	    }
	}
    }
    return result;
}

#define SAME_CAP(n,cap) (&tterm->Strings[n] == &cap)
#define EXTRA_CAP 20

int
fmt_entry(TERMTYPE2 *tterm,
	  PredFunc pred,
	  int content_only,
	  int suppress_untranslatable,
	  int infodump,
	  int numbers)
{
    PredIdx i, j;
    char buffer[MAX_TERMINFO_LENGTH + EXTRA_CAP];
    NCURSES_CONST char *name;
    int predval, len;
    PredIdx num_bools = 0;
    PredIdx num_values = 0;
    PredIdx num_strings = 0;
    bool outcount = 0;

#define WRAP_CONCAT1(s)		wrap_concat1(s); outcount = TRUE
#define WRAP_CONCAT		WRAP_CONCAT1(buffer)

    len = 12;			/* terminfo file-header */

    if (pred == 0) {
	cur_type = tterm;
	pred = dump_predicate;
    }

    strcpy_DYN(&outbuf, 0);
    if (content_only) {
	column = indent;	/* FIXME: workaround to prevent empty lines */
    } else {
	strcpy_DYN(&outbuf, tterm->term_names);

	/*
	 * Colon is legal in terminfo descriptions, but not in termcap.
	 */
	if (!infodump) {
	    char *p = outbuf.text;
	    while (*p) {
		if (*p == ':') {
		    *p = '=';
		}
		++p;
	    }
	}
	strcpy_DYN(&outbuf, separator);
	column = (int) outbuf.used;
	if (height > 1)
	    force_wrap();
    }

    for_each_boolean(j, tterm) {
	i = BoolIndirect(j);
	name = ExtBoolname(tterm, (int) i, bool_names);
	assert(strlen(name) < sizeof(buffer) - EXTRA_CAP);

	if (!version_filter(BOOLEAN, i))
	    continue;
	else if (isObsolete(outform, name))
	    continue;

	predval = pred(BOOLEAN, i);
	if (predval != FAIL) {
	    _nc_STRCPY(buffer, name, sizeof(buffer));
	    if (predval <= 0)
		_nc_STRCAT(buffer, "@", sizeof(buffer));
	    else if (i + 1 > num_bools)
		num_bools = i + 1;
	    WRAP_CONCAT;
	}
    }

    if (column != indent && height > 1)
	force_wrap();

    for_each_number(j, tterm) {
	i = NumIndirect(j);
	name = ExtNumname(tterm, (int) i, num_names);
	assert(strlen(name) < sizeof(buffer) - EXTRA_CAP);

	if (!version_filter(NUMBER, i))
	    continue;
	else if (isObsolete(outform, name))
	    continue;

	predval = pred(NUMBER, i);
	if (predval != FAIL) {
	    if (tterm->Numbers[i] < 0) {
		_nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
			    "%s@", name);
	    } else {
		size_t nn;
		_nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
			    "%s#", name);
		nn = strlen(buffer);
		_nc_SPRINTF(buffer + nn, _nc_SLIMIT(sizeof(buffer) - nn)
			    number_format(tterm->Numbers[i]),
			    tterm->Numbers[i]);
		if (i + 1 > num_values)
		    num_values = i + 1;
	    }
	    WRAP_CONCAT;
	}
    }

    if (column != indent && height > 1)
	force_wrap();

    len += (int) (num_bools
		  + num_values * 2
		  + strlen(tterm->term_names) + 1);
    if (len & 1)
	len++;

#undef CUR
#define CUR tterm->
    if (outform == F_TERMCAP) {
	if (VALID_STRING(termcap_reset)) {
	    if (VALID_STRING(init_3string)
		&& !strcmp(init_3string, termcap_reset))
		DISCARD(init_3string);

	    if (VALID_STRING(reset_2string)
		&& !strcmp(reset_2string, termcap_reset))
		DISCARD(reset_2string);
	}
    }

    for_each_string(j, tterm) {
	char *capability;
	i = StrIndirect(j);
	name = ExtStrname(tterm, (int) i, str_names);
	assert(strlen(name) < sizeof(buffer) - EXTRA_CAP);

	capability = tterm->Strings[i];

	if (!version_filter(STRING, i))
	    continue;
	else if (isObsolete(outform, name))
	    continue;

#if NCURSES_XNAMES
	/*
	 * Extended names can be longer than 2 characters, but termcap programs
	 * cannot read those (filter them out).
	 */
	if (outform == F_TERMCAP && (strlen(name) > 2))
	    continue;
#endif

	if (outform == F_TERMCAP) {
	    /*
	     * Some older versions of vi want rmir/smir to be defined
	     * for ich/ich1 to work.  If they're not defined, force
	     * them to be output as defined and empty.
	     */
	    if (PRESENT(insert_character) || PRESENT(parm_ich)) {
		if (SAME_CAP(i, enter_insert_mode)
		    && enter_insert_mode == ABSENT_STRING) {
		    _nc_STRCPY(buffer, "im=", sizeof(buffer));
		    WRAP_CONCAT;
		    continue;
		}

		if (SAME_CAP(i, exit_insert_mode)
		    && exit_insert_mode == ABSENT_STRING) {
		    _nc_STRCPY(buffer, "ei=", sizeof(buffer));
		    WRAP_CONCAT;
		    continue;
		}
	    }
	    /*
	     * termcap applications such as screen will be confused if sgr0
	     * is translated to a string containing rmacs.  Filter that out.
	     */
	    if (PRESENT(exit_attribute_mode)) {
		if (SAME_CAP(i, exit_attribute_mode)) {
		    char *trimmed_sgr0;
		    char *my_sgr = set_attributes;

		    set_attributes = save_sgr;

		    trimmed_sgr0 = _nc_trim_sgr0(tterm);
		    if (strcmp(capability, trimmed_sgr0)) {
			capability = trimmed_sgr0;
		    } else {
			if (trimmed_sgr0 != exit_attribute_mode)
			    free(trimmed_sgr0);
		    }

		    set_attributes = my_sgr;
		}
	    }
	}

	predval = pred(STRING, i);
	buffer[0] = '\0';

	if (predval != FAIL) {
	    if (VALID_STRING(capability)
		&& i + 1 > num_strings)
		num_strings = i + 1;

	    if (!VALID_STRING(capability)) {
		_nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
			    "%s@", name);
		WRAP_CONCAT;
	    } else if (TcOutput()) {
		char *srccap = _nc_tic_expand(capability, TRUE, numbers);
		int params = ((i < (int) SIZEOF(parametrized))
			      ? parametrized[i]
			      : ((*srccap == 'k')
				 ? 0
				 : has_params(srccap, FALSE)));
		char *cv = _nc_infotocap(name, srccap, params);

		if (cv == 0) {
		    if (outform == F_TCONVERR) {
			_nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
				    "%s=!!! %s WILL NOT CONVERT !!!",
				    name, srccap);
			WRAP_CONCAT;
		    } else if (suppress_untranslatable) {
			continue;
		    } else {
			char *s = srccap, *d = buffer;
			int need = 3 + (int) strlen(name);
			while ((*d = *s++) != 0) {
			    if ((d - buffer + 2) >= (int) sizeof(buffer)) {
				fprintf(stderr,
					"%s: value for %s is too long\n",
					_nc_progname,
					name);
				*d = '\0';
				break;
			    }
			    if (*d == ':') {
				*d++ = '\\';
				*d = ':';
			    } else if (*d == '\\') {
				if ((*++d = *s++) == '\0')
				    break;
			    }
			    d++;
			    *d = '\0';
			}
			need += (int) (d - buffer);
			wrap_concat("..", need, w1ST | wERR);
			need -= 2;
			wrap_concat(name, need, wOFF | wERR);
			need -= (int) strlen(name);
			wrap_concat("=", need, w2ND | wERR);
			need -= 1;
			wrap_concat(buffer, need, wEND | wERR);
			outcount = TRUE;
		    }
		} else {
		    wrap_concat3(name, "=", cv);
		}
		len += (int) strlen(capability) + 1;
	    } else {
		char *src = _nc_tic_expand(capability,
					   outform == F_TERMINFO, numbers);

		strcpy_DYN(&tmpbuf, 0);
		strcpy_DYN(&tmpbuf, name);
		strcpy_DYN(&tmpbuf, "=");
		if (pretty
		    && (outform == F_TERMINFO
			|| outform == F_VARIABLE)) {
		    fmt_complex(tterm, name, src, 1);
		} else {
		    strcpy_DYN(&tmpbuf, src);
		}
		len += (int) strlen(capability) + 1;
		WRAP_CONCAT1(tmpbuf.text);
	    }
	}
	/* e.g., trimmed_sgr0 */
	if (VALID_STRING(capability) &&
	    capability != tterm->Strings[i])
	    free(capability);
    }
    len += (int) (num_strings * 2);

    /*
     * This piece of code should be an effective inverse of the functions
     * postprocess_terminfo() and postprocess_terminfo() in parse_entry.c.
     * Much more work should be done on this to support dumping termcaps.
     */
    if (tversion == V_HPUX) {
	if (VALID_STRING(memory_lock)) {
	    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
			"meml=%s", memory_lock);
	    WRAP_CONCAT;
	}
	if (VALID_STRING(memory_unlock)) {
	    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
			"memu=%s", memory_unlock);
	    WRAP_CONCAT;
	}
    } else if (tversion == V_AIX) {
	if (VALID_STRING(acs_chars)) {
	    bool box_ok = TRUE;
	    const char *acstrans = "lqkxjmwuvtn";
	    const char *cp;
	    char *tp, *sp, boxchars[11];

	    tp = boxchars;
	    for (cp = acstrans; *cp; cp++) {
		sp = (strchr) (acs_chars, *cp);
		if (sp)
		    *tp++ = sp[1];
		else {
		    box_ok = FALSE;
		    break;
		}
	    }
	    tp[0] = '\0';

	    if (box_ok) {
		char *tmp = _nc_tic_expand(boxchars,
					   (outform == F_TERMINFO),
					   numbers);
		_nc_STRCPY(buffer, "box1=", sizeof(buffer));
		while (*tmp != '\0') {
		    size_t have = strlen(buffer);
		    size_t next = strlen(tmp);
		    size_t want = have + next + 1;
		    size_t last = next;
		    char save = '\0';

		    /*
		     * If the expanded string is too long for the buffer,
		     * chop it off and save the location where we chopped it.
		     */
		    if (want >= sizeof(buffer)) {
			save = tmp[last];
			tmp[last] = '\0';
		    }
		    _nc_STRCAT(buffer, tmp, sizeof(buffer));

		    /*
		     * If we chopped the buffer, replace the missing piece and
		     * shift everything to append the remainder.
		     */
		    if (save != '\0') {
			next = 0;
			tmp[last] = save;
			while ((tmp[next] = tmp[last + next]) != '\0') {
			    ++next;
			}
		    } else {
			break;
		    }
		}
		WRAP_CONCAT;
	    }
	}
    }

    /*
     * kludge: trim off trailer to avoid an extra blank line
     * in infocmp -u output when there are no string differences
     */
    if (outcount) {
	bool trimmed = FALSE;
	j = (PredIdx) outbuf.used;
	if (wrapped && did_wrap) {
	    /* EMPTY */ ;
	} else if (j >= 2
		   && outbuf.text[j - 1] == '\t'
		   && outbuf.text[j - 2] == '\n') {
	    outbuf.used -= 2;
	    trimmed = TRUE;
	} else if (j >= 4
		   && outbuf.text[j - 1] == ':'
		   && outbuf.text[j - 2] == '\t'
		   && outbuf.text[j - 3] == '\n'
		   && outbuf.text[j - 4] == '\\') {
	    outbuf.used -= 4;
	    trimmed = TRUE;
	}
	if (trimmed) {
	    outbuf.text[outbuf.used] = '\0';
	    column = oldcol;
	    strcpy_DYN(&outbuf, " ");
	}
    }
#if 0
    fprintf(stderr, "num_bools = %d\n", num_bools);
    fprintf(stderr, "num_values = %d\n", num_values);
    fprintf(stderr, "num_strings = %d\n", num_strings);
    fprintf(stderr, "term_names=%s, len=%d, strlen(outbuf)=%d, outbuf=%s\n",
	    tterm->term_names, len, outbuf.used, outbuf.text);
#endif
    /*
     * Here's where we use infodump to trigger a more stringent length check
     * for termcap-translation purposes.
     * Return the length of the raw entry, without tc= expansions,
     * It gives an idea of which entries are deadly to even *scan past*,
     * as opposed to *use*.
     */
    return (infodump ? len : (int) termcap_length(outbuf.text));
}

static bool
kill_string(TERMTYPE2 *tterm, char *cap)
{
    unsigned n;
    for (n = 0; n < NUM_STRINGS(tterm); ++n) {
	if (cap == tterm->Strings[n]) {
	    tterm->Strings[n] = ABSENT_STRING;
	    return TRUE;
	}
    }
    return FALSE;
}

static char *
find_string(TERMTYPE2 *tterm, char *name)
{
    PredIdx n;
    for (n = 0; n < NUM_STRINGS(tterm); ++n) {
	if (version_filter(STRING, n)
	    && !strcmp(name, strnames[n])) {
	    char *cap = tterm->Strings[n];
	    if (VALID_STRING(cap)) {
		return cap;
	    }
	    break;
	}
    }
    return ABSENT_STRING;
}

/*
 * This is used to remove function-key labels from a termcap entry to
 * make it smaller.
 */
static int
kill_labels(TERMTYPE2 *tterm, int target)
{
    int n;
    int result = 0;
    char name[20];

    for (n = 0; n <= 10; ++n) {
	char *cap;

	_nc_SPRINTF(name, _nc_SLIMIT(sizeof(name)) "lf%d", n);
	cap = find_string(tterm, name);
	if (VALID_STRING(cap)
	    && kill_string(tterm, cap)) {
	    target -= (int) (strlen(cap) + 5);
	    ++result;
	    if (target < 0)
		break;
	}
    }
    return result;
}

/*
 * This is used to remove function-key definitions from a termcap entry to
 * make it smaller.
 */
static int
kill_fkeys(TERMTYPE2 *tterm, int target)
{
    int n;
    int result = 0;
    char name[20];

    for (n = 60; n >= 0; --n) {
	char *cap;

	_nc_SPRINTF(name, _nc_SLIMIT(sizeof(name)) "kf%d", n);
	cap = find_string(tterm, name);
	if (VALID_STRING(cap)
	    && kill_string(tterm, cap)) {
	    target -= (int) (strlen(cap) + 5);
	    ++result;
	    if (target < 0)
		break;
	}
    }
    return result;
}

/*
 * Check if the given acsc string is a 1-1 mapping, i.e., just-like-vt100.
 * Also, since this is for termcap, we only care about the line-drawing map.
 */
#define isLine(c) (strchr("lmkjtuvwqxn", c) != 0)

static bool
one_one_mapping(const char *mapping)
{
    bool result = TRUE;

    if (VALID_STRING(mapping)) {
	int n = 0;
	while (mapping[n] != '\0' && mapping[n + 1] != '\0') {
	    if (isLine(mapping[n]) &&
		mapping[n] != mapping[n + 1]) {
		result = FALSE;
		break;
	    }
	    n += 2;
	}
    }
    return result;
}

#define FMT_ENTRY() \
		fmt_entry(tterm, pred, \
			0, \
			suppress_untranslatable, \
			infodump, numbers)

#define SHOW_WHY PRINTF

static bool
purged_acs(TERMTYPE2 *tterm)
{
    bool result = FALSE;

    if (VALID_STRING(acs_chars)) {
	if (!one_one_mapping(acs_chars)) {
	    enter_alt_charset_mode = ABSENT_STRING;
	    exit_alt_charset_mode = ABSENT_STRING;
	    SHOW_WHY("# (rmacs/smacs removed for consistency)\n");
	}
	result = TRUE;
    }
    return result;
}

static void
encode_b64(char *target, char *source, unsigned state, int *saved)
{
    /* RFC-4648 */
    static const char data[] =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
    "abcdefghijklmnopqrstuvwxyz"
    "0123456789" "-_";
    int ch = UChar(source[state]);

    switch (state % 3) {
    case 0:
	*target++ = data[(ch >> 2) & 077];
	*saved = (ch << 4);
	break;
    case 1:
	*target++ = data[((ch >> 4) | *saved) & 077];
	*saved = (ch << 2);
	break;
    case 2:
	*target++ = data[((ch >> 6) | *saved) & 077];
	*target++ = data[ch & 077];
	*saved = 0;
	break;
    }
    *target = '\0';
}

/*
 * Dump a single entry.
 */
void
dump_entry(TERMTYPE2 *tterm,
	   int suppress_untranslatable,
	   int limited,
	   int numbers,
	   PredFunc pred)
{
    TERMTYPE2 save_tterm;
    int critlen;
    const char *legend;
    bool infodump;

    if (quickdump) {
	char bigbuf[65536];
	unsigned offset = 0;

	separator = "";
	trailer = "\n";
	indent = 0;

	if (_nc_write_object(tterm, bigbuf, &offset, sizeof(bigbuf)) == OK) {
	    char numbuf[80];
	    unsigned n;

	    if (quickdump & 1) {
		if (outbuf.used)
		    wrap_concat1("\n");
		wrap_concat1("hex:");
		for (n = 0; n < offset; ++n) {
		    _nc_SPRINTF(numbuf, _nc_SLIMIT(sizeof(numbuf))
				"%02X", UChar(bigbuf[n]));
		    wrap_concat1(numbuf);
		}
	    }
	    if (quickdump & 2) {
		static char padding[] =
		{0, 0};
		int value = 0;

		if (outbuf.used)
		    wrap_concat1("\n");
		wrap_concat1("b64:");
		for (n = 0; n < offset; ++n) {
		    encode_b64(numbuf, bigbuf, n, &value);
		    wrap_concat1(numbuf);
		}
		switch (n % 3) {
		case 0:
		    break;
		case 1:
		    encode_b64(numbuf, padding, 1, &value);
		    wrap_concat1(numbuf);
		    wrap_concat1("==");
		    break;
		case 2:
		    encode_b64(numbuf, padding, 1, &value);
		    wrap_concat1(numbuf);
		    wrap_concat1("=");
		    break;
		}
	    }
	}
	return;
    }

    if (TcOutput()) {
	critlen = MAX_TERMCAP_LENGTH;
	legend = "older termcap";
	infodump = FALSE;
	set_obsolete_termcaps(tterm);
    } else {
	critlen = MAX_TERMINFO_LENGTH;
	legend = "terminfo";
	infodump = TRUE;
    }

    save_sgr = set_attributes;

    if ((FMT_ENTRY() > critlen)
	&& limited) {

	save_tterm = *tterm;
	if (!suppress_untranslatable) {
	    SHOW_WHY("# (untranslatable capabilities removed to fit entry within %d bytes)\n",
		     critlen);
	    suppress_untranslatable = TRUE;
	}
	if (FMT_ENTRY() > critlen) {
	    /*
	     * We pick on sgr because it is a nice long string capability that
	     * is really just an optimization hack.  Another good candidate is
	     * acsc since it is both long and unused by BSD termcap.
	     */
	    bool changed = FALSE;

#if NCURSES_XNAMES
	    /*
	     * Extended names are most likely function-key definitions.  Drop
	     * those first.
	     */
	    unsigned n;
	    for (n = STRCOUNT; n < NUM_STRINGS(tterm); n++) {
		const char *name = ExtStrname(tterm, (int) n, strnames);

		if (VALID_STRING(tterm->Strings[n])) {
		    set_attributes = ABSENT_STRING;
		    /* we remove long names anyway - only report the short */
		    if (strlen(name) <= 2) {
			SHOW_WHY("# (%s removed to fit entry within %d bytes)\n",
				 name,
				 critlen);
		    }
		    changed = TRUE;
		    if (FMT_ENTRY() <= critlen)
			break;
		}
	    }
#endif
	    if (VALID_STRING(set_attributes)) {
		set_attributes = ABSENT_STRING;
		SHOW_WHY("# (sgr removed to fit entry within %d bytes)\n",
			 critlen);
		changed = TRUE;
	    }
	    if (!changed || (FMT_ENTRY() > critlen)) {
		if (purged_acs(tterm)) {
		    acs_chars = ABSENT_STRING;
		    SHOW_WHY("# (acsc removed to fit entry within %d bytes)\n",
			     critlen);
		    changed = TRUE;
		}
	    }
	    if (!changed || (FMT_ENTRY() > critlen)) {
		int oldversion = tversion;
		int len;

		tversion = V_BSD;
		SHOW_WHY("# (terminfo-only capabilities suppressed to fit entry within %d bytes)\n",
			 critlen);

		len = FMT_ENTRY();
		if (len > critlen
		    && kill_labels(tterm, len - critlen)) {
		    SHOW_WHY("# (some labels capabilities suppressed to fit entry within %d bytes)\n",
			     critlen);
		    len = FMT_ENTRY();
		}
		if (len > critlen
		    && kill_fkeys(tterm, len - critlen)) {
		    SHOW_WHY("# (some function-key capabilities suppressed to fit entry within %d bytes)\n",
			     critlen);
		    len = FMT_ENTRY();
		}
		if (len > critlen) {
		    (void) fprintf(stderr,
				   "%s: %s entry is %d bytes long\n",
				   _nc_progname,
				   _nc_first_name(tterm->term_names),
				   len);
		    SHOW_WHY("# WARNING: this entry, %d bytes long, may core-dump %s libraries!\n",
			     len, legend);
		}
		tversion = oldversion;
	    }
	    set_attributes = save_sgr;
	    *tterm = save_tterm;
	}
    } else if (!version_filter(STRING, STR_IDX(acs_chars))) {
	save_tterm = *tterm;
	if (purged_acs(tterm)) {
	    (void) FMT_ENTRY();
	}
	*tterm = save_tterm;
    }
}

void
dump_uses(const char *name, bool infodump)
/* dump "use=" clauses in the appropriate format */
{
    char buffer[MAX_TERMINFO_LENGTH];

    if (TcOutput())
	trim_trailing();
    _nc_SPRINTF(buffer, _nc_SLIMIT(sizeof(buffer))
		"%s%s", infodump ? "use=" : "tc=", name);
    wrap_concat1(buffer);
}

int
show_entry(void)
{
    /*
     * Trim any remaining whitespace.
     */
    if (outbuf.used != 0) {
	bool infodump = !TcOutput();
	char delim = (char) (infodump ? ',' : ':');
	int j;

	for (j = (int) outbuf.used - 1; j > 0; --j) {
	    char ch = outbuf.text[j];
	    if (ch == '\n') {
		;
	    } else if (isspace(UChar(ch))) {
		outbuf.used = (size_t) j;
	    } else if (!infodump && ch == '\\') {
		outbuf.used = (size_t) j;
	    } else if (ch == delim && (j == 0 || outbuf.text[j - 1] != '\\')) {
		outbuf.used = (size_t) (j + 1);
	    } else {
		break;
	    }
	}
	outbuf.text[outbuf.used] = '\0';
    }
    if (outbuf.text != 0) {
	(void) fputs(outbuf.text, stdout);
	putchar('\n');
    }
    return (int) outbuf.used;
}

void
compare_entry(PredHook hook,
	      TERMTYPE2 *tp GCC_UNUSED,
	      bool quiet)
/* compare two entries */
{
    PredIdx i, j;
    NCURSES_CONST char *name;

    if (!quiet)
	fputs("    comparing booleans.\n", stdout);
    for_each_boolean(j, tp) {
	i = BoolIndirect(j);
	name = ExtBoolname(tp, (int) i, bool_names);

	if (isObsolete(outform, name))
	    continue;

	(*hook) (CMP_BOOLEAN, i, name);
    }

    if (!quiet)
	fputs("    comparing numbers.\n", stdout);
    for_each_number(j, tp) {
	i = NumIndirect(j);
	name = ExtNumname(tp, (int) i, num_names);

	if (isObsolete(outform, name))
	    continue;

	(*hook) (CMP_NUMBER, i, name);
    }

    if (!quiet)
	fputs("    comparing strings.\n", stdout);
    for_each_string(j, tp) {
	i = StrIndirect(j);
	name = ExtStrname(tp, (int) i, str_names);

	if (isObsolete(outform, name))
	    continue;

	(*hook) (CMP_STRING, i, name);
    }

    /* (void) fputs("    comparing use entries.\n", stdout); */
    (*hook) (CMP_USE, 0, "use");

}

#define NOTSET(s)	((s) == 0)

/*
 * This bit of legerdemain turns all the terminfo variable names into
 * references to locations in the arrays Booleans, Numbers, and Strings ---
 * precisely what's needed.
 */
#undef CUR
#define CUR tp->

static void
set_obsolete_termcaps(TERMTYPE2 *tp)
{
#include "capdefaults.c"
}

/*
 * Convert an alternate-character-set string to canonical form: sorted and
 * unique.
 */
void
repair_acsc(TERMTYPE2 *tp)
{
    if (VALID_STRING(acs_chars)) {
	size_t n;
	char mapped[256];
	unsigned source;
	unsigned target;
	bool fix_needed = FALSE;

	for (n = 0, source = 0; acs_chars[n] != 0; n++) {
	    target = UChar(acs_chars[n]);
	    if (source >= target) {
		fix_needed = TRUE;
		break;
	    }
	    source = target;
	    if (acs_chars[n + 1])
		n++;
	}

	if (fix_needed) {
	    size_t m;
	    char extra = 0;

	    memset(mapped, 0, sizeof(mapped));
	    for (n = 0; acs_chars[n] != 0; n++) {
		source = UChar(acs_chars[n]);
		if ((target = (unsigned char) acs_chars[n + 1]) != 0) {
		    mapped[source] = (char) target;
		    n++;
		} else {
		    extra = (char) source;
		}
	    }
	    for (n = m = 0; n < sizeof(mapped); n++) {
		if (mapped[n]) {
		    acs_chars[m++] = (char) n;
		    acs_chars[m++] = mapped[n];
		}
	    }
	    if (extra)
		acs_chars[m++] = extra;		/* garbage in, garbage out */
	    acs_chars[m] = 0;
	}
    }
}
