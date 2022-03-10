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
 *     and: Thomas E. Dickey                        1996-on                 *
 ****************************************************************************/

/*
 * $Id: dump_entry.h,v 1.44 2021/04/18 19:56:09 tom Exp $
 *
 * Dump control definitions and variables
 */

#ifndef DUMP_ENTRY_H
#define DUMP_ENTRY_H 1

#define NCURSES_OPAQUE    0
#define NCURSES_INTERNALS 1
#include <curses.h>
#include <term.h>

/* capability output formats */
#define F_TERMINFO	0	/* use terminfo names */
#define F_VARIABLE	1	/* use C variable names */
#define F_TERMCAP	2	/* termcap names with capability conversion */
#define F_TCONVERR	3	/* as T_TERMCAP, no skip of untranslatables */
#define F_LITERAL	4	/* like F_TERMINFO, but no smart defaults */

/* capability sort modes */
#define S_DEFAULT	0	/* sort by terminfo name (implicit) */
#define S_NOSORT	1	/* don't sort */
#define S_TERMINFO	2	/* sort by terminfo names (explicit) */
#define S_VARIABLE	3	/* sort by C variable names */
#define S_TERMCAP	4	/* sort by termcap names */

/* capability types for the comparison hook */
#define CMP_BOOLEAN	0	/* comparison on booleans */
#define CMP_NUMBER	1	/* comparison on numerics */
#define CMP_STRING	2	/* comparison on strings */
#define CMP_USE		3	/* comparison on use capabilities */

#ifndef _TERMSORT_H
typedef unsigned PredType;
typedef unsigned PredIdx;
#endif

typedef int (*PredFunc) (PredType, PredIdx);
typedef void (*PredHook) (PredType, PredIdx, const char *);

extern NCURSES_CONST char *nametrans(const char *);
extern bool has_params(const char *, bool);
extern int fmt_entry(TERMTYPE2 *, PredFunc, int, int, int, int);
extern int show_entry(void);
extern void compare_entry(PredHook, TERMTYPE2 *, bool);
extern void dump_entry(TERMTYPE2 *, int, int, int, PredFunc);
extern void dump_init(const char *, int, int, bool, int, int, unsigned, bool,
		      bool, int);
extern void dump_uses(const char *, bool);
extern void repair_acsc(TERMTYPE2 *tp);

#define L_CURL "{"
#define R_CURL "}"

#define FAIL	-1

#endif /* DUMP_ENTRY_H */
