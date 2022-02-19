/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 1998-2017,2018 Free Software Foundation, Inc.                  *
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
 *  Author: Thomas E. Dickey                    1996-on                     *
 ****************************************************************************/
/* $Id: test.priv.h,v 1.197 2021/04/25 00:00:24 tom Exp $ */

#ifndef __TEST_PRIV_H
#define __TEST_PRIV_H 1

#include <ncurses_cfg.h>

/*
 * Fix ifdef's that look for the form/menu/panel libraries, if we are building
 * with wide-character ncurses.
 */
#ifdef  HAVE_LIBFORMW
#define HAVE_LIBFORMW 1
#define HAVE_LIBFORM 1
#endif

#ifdef  HAVE_LIBMENUW
#define HAVE_LIBMENUW 1
#define HAVE_LIBMENU 1
#endif

#ifdef  HAVE_LIBPANELW
#define HAVE_LIBPANELW 1
#define HAVE_LIBPANEL 1
#endif

/*
 * Fallback definitions to accommodate broken compilers.
 */
#ifndef HAVE_ALLOC_PAIR
#define HAVE_ALLOC_PAIR 0
#endif

#ifndef HAVE_ASSUME_DEFAULT_COLORS
#define HAVE_ASSUME_DEFAULT_COLORS 0
#endif

#ifndef HAVE_BSD_STRING_H
#define HAVE_BSD_STRING_H 0
#endif

#ifndef HAVE_CURSES_VERSION
#define HAVE_CURSES_VERSION 0
#endif

#ifndef HAVE_CURSCR
#define HAVE_CURSCR 0
#endif

#ifndef HAVE_CHGAT
#define HAVE_CHGAT 0
#endif

#ifndef HAVE_COLOR_CONTENT
#define HAVE_COLOR_CONTENT 0
#endif

#ifndef HAVE_COPYWIN
#define HAVE_COPYWIN 0
#endif

#ifndef HAVE_COLOR_SET
#define HAVE_COLOR_SET 0
#endif

#ifndef HAVE_DELSCREEN
#define HAVE_DELSCREEN 0
#endif

#ifndef HAVE_DUPWIN
#define HAVE_DUPWIN 0
#endif

#ifndef HAVE_FILTER
#define HAVE_FILTER 0
#endif

#ifndef HAVE_FORM_H
#define HAVE_FORM_H 0
#endif

#ifndef HAVE_GETBEGX
#define HAVE_GETBEGX 0
#endif

#ifndef HAVE_GETCURX
#define HAVE_GETCURX 0
#endif

#ifndef HAVE_GETMAXX
#define HAVE_GETMAXX 0
#endif

#ifndef HAVE_GETOPT_H
#define HAVE_GETOPT_H 0
#endif

#ifndef HAVE_GETPARX
#define HAVE_GETPARX 0
#endif

#ifndef HAVE_GETWIN
#define HAVE_GETWIN 0
#endif

#ifndef HAVE_HALFDELAY
#define HAVE_HALFDELAY 0
#endif

#ifndef HAVE_INIT_EXTENDED_COLOR
#define HAVE_INIT_EXTENDED_COLOR 0
#endif

#ifndef HAVE_LIBFORM
#define HAVE_LIBFORM 0
#endif

#ifndef HAVE_LIBMENU
#define HAVE_LIBMENU 0
#endif

#ifndef HAVE_LIBPANEL
#define HAVE_LIBPANEL 0
#endif

#ifndef HAVE_LANGINFO_CODESET
#define HAVE_LANGINFO_CODESET 0
#endif

#ifndef HAVE_LOCALE_H
#define HAVE_LOCALE_H 0
#endif

#ifndef HAVE_MATH_H
#define HAVE_MATH_H 0
#endif

#ifndef HAVE_MENU_H
#define HAVE_MENU_H 0
#endif

#ifndef HAVE_MVDERWIN
#define HAVE_MVDERWIN 0
#endif

#ifndef HAVE_MVVLINE
#define HAVE_MVVLINE 0
#endif

#ifndef HAVE_MVWIN
#define HAVE_MVWIN 0
#endif

#ifndef HAVE_MVWVLINE
#define HAVE_MVWVLINE 0
#endif

#ifndef HAVE_NAPMS
#define HAVE_NAPMS 1
#endif

#ifndef HAVE_NC_ALLOC_H
#define HAVE_NC_ALLOC_H 0
#endif

#ifndef HAVE_NEWPAD
#define HAVE_NEWPAD 0
#endif

#ifndef HAVE_PANEL_H
#define HAVE_PANEL_H 0
#endif

#ifndef HAVE_PUTWIN
#define HAVE_PUTWIN 0
#endif

#ifndef HAVE_RESET_COLOR_PAIRS
#define HAVE_RESET_COLOR_PAIRS 0
#endif

#ifndef HAVE_RESIZE_TERM
#define HAVE_RESIZE_TERM 0
#endif

#ifndef HAVE_RESTARTTERM
#define HAVE_RESTARTTERM 0
#endif

#ifndef HAVE_RIPOFFLINE
#define HAVE_RIPOFFLINE 0
#endif

#ifndef HAVE_SCR_DUMP
#define HAVE_SCR_DUMP 0
#endif

#ifndef HAVE_SETUPTERM
#define HAVE_SETUPTERM 0
#endif

#ifndef HAVE_SLK_COLOR
#define HAVE_SLK_COLOR 0
#endif

#ifndef HAVE_SLK_INIT
#define HAVE_SLK_INIT 0
#endif

#ifndef HAVE_STDINT_H
#define HAVE_STDINT_H 0
#endif

#ifndef HAVE_STDNORETURN_H
#define HAVE_STDNORETURN_H 0
#endif

#ifndef HAVE_STRSTR
#define HAVE_STRSTR 0
#endif

#ifndef HAVE_SYS_IOCTL_H
#define HAVE_SYS_IOCTL_H 0
#endif

#ifndef HAVE_SYS_SELECT_H
#define HAVE_SYS_SELECT_H 0
#endif

#ifndef HAVE_TERMATTRS
#define HAVE_TERMATTRS 0
#endif

#ifndef HAVE_TERMIOS_H
#define HAVE_TERMIOS_H 0
#endif

#ifndef HAVE_TERMNAME
#define HAVE_TERMNAME 0
#endif

#ifndef HAVE_TERM_ENTRY_H
#define HAVE_TERM_ENTRY_H 0
#endif

#ifndef HAVE_TGETENT
#define HAVE_TGETENT 0
#endif

#ifndef HAVE_TIGETNUM
#define HAVE_TIGETNUM 0
#endif

#ifndef HAVE_TIGETSTR
#define HAVE_TIGETSTR 0
#endif

#ifndef HAVE_TPUTS_SP
#define HAVE_TPUTS_SP 0
#endif

#ifndef HAVE_TSEARCH
#define HAVE_TSEARCH 0
#endif

#ifndef HAVE_TYPEAHEAD
#define HAVE_TYPEAHEAD 0
#endif

#ifndef HAVE_WINSSTR
#define HAVE_WINSSTR 0
#endif

#ifndef HAVE_USE_DEFAULT_COLORS
#define HAVE_USE_DEFAULT_COLORS 0
#endif

#ifndef HAVE_USE_ENV
#define HAVE_USE_ENV 0
#endif

#ifndef HAVE_USE_EXTENDED_NAMES
#define HAVE_USE_EXTENDED_NAMES 0
#endif

#ifndef HAVE_USE_SCREEN
#define HAVE_USE_SCREEN 0
#endif

#ifndef HAVE_USE_WINDOW
#define HAVE_USE_WINDOW 0
#endif

#ifndef HAVE_VIDPUTS
#define HAVE_VIDPUTS 0
#endif

#ifndef HAVE_VID_PUTS
#define HAVE_VID_PUTS 0
#endif

#ifndef HAVE_WINSDELLN
#define HAVE_WINSDELLN 0
#endif

#ifndef HAVE_WRESIZE
#define HAVE_WRESIZE 0
#endif

#ifndef HAVE__TRACEF
#define HAVE__TRACEF 0
#endif

#ifndef NCURSES_EXT_FUNCS
#define NCURSES_EXT_FUNCS 0
#endif

#ifndef NEED_PTEM_H
#define NEED_PTEM_H 0
#endif

#ifndef NEED_WCHAR_H
#define NEED_WCHAR_H 0
#endif

#ifndef NO_LEAKS
#define NO_LEAKS 0
#endif

/*
 * Workaround for HPUX
 */
#if defined(__hpux) && !defined(NCURSES_VERSION)
#define _ACS_COMPAT_CODE	/* needed for acs_map vs __acs_map */
#endif

#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/types.h>
#include <errno.h>

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#include <signal.h>		/* include before curses.h to work around glibc bug */

#if NEED_WCHAR_H
#include <wchar.h>
#ifdef HAVE_LIBUTF8_H
#include <libutf8.h>
#endif
#endif

#if defined(HAVE_XCURSES)
#include <xcurses.h>
#elif defined(HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/curses.h>
#elif defined(HAVE_NCURSES_NCURSES_H)
#include <ncurses/curses.h>
#else
#include <curses.h>
#endif

#if HAVE_STDNORETURN_H && !defined(NCURSES_VERSION)
#include <stdnoreturn.h>
#undef GCC_NORETURN
#define GCC_NORETURN _Noreturn
#endif

#if !(defined(NCURSES_WGETCH_EVENTS) && defined(NEED_KEY_EVENT))
#undef KEY_EVENT		/* reduce compiler-warnings with Visual C++ */
#endif

#if defined(HAVE_XCURSES) || defined(PDCURSES)
/* no other headers */
#undef  HAVE_SETUPTERM		/* nonfunctional */
#define HAVE_SETUPTERM 0
#undef  HAVE_TGETENT		/* nonfunctional */
#define HAVE_TGETENT 0
#undef  HAVE_TIGETSTR		/* nonfunctional */
#define HAVE_TIGETSTR 0
#elif defined(HAVE_NCURSESW_TERM_H)
#include <ncursesw/term.h>
#elif defined(HAVE_NCURSES_TERM_H)
#include <ncurses/term.h>
#elif defined(HAVE_TERM_H)
#include <term.h>
#endif

/*
 * Not all curses.h implementations include unctrl.h,
 */
#if defined(HAVE_NCURSESW_UNCTRL_H)
#include <ncursesw/unctrl.h>
#elif defined(HAVE_NCURSES_UNCTRL_H)
#include <ncurses/unctrl.h>
#elif defined(HAVE_UNCTRL_H)
#include <unctrl.h>
#endif

#if HAVE_GETOPT_H
#include <getopt.h>
#elif !defined(HAVE_GETOPT_HEADER)
/* 'getopt()' may be prototyped in <stdlib.h>, but declaring its variables
 * doesn't hurt.
 */
extern char *optarg;
extern int optind;
#endif /* HAVE_GETOPT_H */

#if HAVE_LOCALE_H
#include <locale.h>
#else
#define setlocale(name,string)	/* nothing */
#endif

#include <assert.h>
#include <ctype.h>

#if defined(_MSC_VER)
#undef popen
#define popen(s,n) _popen(s,n)
#undef pclose
#define pclose(s) _pclose(s)
#endif

#ifndef GCC_NORETURN
#define GCC_NORETURN		/* nothing */
#endif
#ifndef GCC_PRINTFLIKE
#define GCC_PRINTFLIKE(a,b)	/* nothing */
#endif
#ifndef GCC_SCANFLIKE
#define GCC_SCANFLIKE(a,b)	/* nothing */
#endif
#ifndef GCC_UNUSED
#define GCC_UNUSED		/* nothing */
#endif

#ifndef HAVE_GETNSTR
#define getnstr(s,n) getstr(s)
#endif

#if HAVE_INIT_EXTENDED_COLOR
#define USE_EXTENDED_COLOR 1
#else
#define USE_EXTENDED_COLOR 0
#endif

#ifndef USE_SOFTKEYS
#if HAVE_SLK_INIT
#define USE_SOFTKEYS 1
#else
#define USE_SOFTKEYS 0
#endif
#endif

#if !USE_SOFTKEYS
#define slk_init()		/* nothing */
#define slk_restore()		/* nothing */
#define slk_clear()		/* nothing */
#endif

#ifndef HAVE_CURSES_DATA_TABSIZE
#define HAVE_CURSES_DATA_TABSIZE 0
#endif

#if !NCURSES_EXT_FUNCS
#if HAVE_CURSES_DATA_TABSIZE
#define set_tabsize(n)	TABSIZE = (n)
#else
#define set_tabsize(n)		/* nothing */
#endif
#endif

#if HAVE_TPUTS_SP
#define USE_SP_FUNCS 1
#else
#define USE_SP_FUNCS 0
#endif

#ifndef HAVE_WSYNCDOWN
#define wsyncdown(win)		/* nothing */
#endif

#ifndef USE_WIDEC_SUPPORT
#if (defined(_XOPEN_SOURCE_EXTENDED) \
  || (defined(_XOPEN_SOURCE) && (_XOPEN_SOURCE - 0 >= 500)) \
  || (defined(NCURSES_WIDECHAR) && (NCURSES_WIDECHAR - 0 < 1))) \
  && defined(WACS_ULCORNER)
#define USE_WIDEC_SUPPORT 1
#else
#define USE_WIDEC_SUPPORT 0
#endif
#endif

#if HAVE_PANEL_H && HAVE_LIBPANEL
#define USE_LIBPANEL 1
#else
#define USE_LIBPANEL 0
#endif

#if HAVE_MENU_H && HAVE_LIBMENU
#define USE_LIBMENU 1
#else
#define USE_LIBMENU 0
#endif

#if HAVE_FORM_H && HAVE_LIBFORM
#define USE_LIBFORM 1
#else
#define USE_LIBFORM 0
#endif

/* workaround, to build against NetBSD's variant of the form library */
#ifdef HAVE_NETBSD_FORM_H
#define form_getyx(form, y, x) y = (int)current_field(form)->cursor_ypos, x = (int)current_field(form)->cursor_xpos
#else
#define form_getyx(form, y, x) y = (int)(form)->currow, x = (int)(form)->curcol
#endif

/* workaround, to build against NetBSD's variant of the form library */
#ifdef HAVE_NETBSD_MENU_H
#define menu_itemwidth(menu) (menu)->max_item_width
#else
#define menu_itemwidth(menu) (menu)->itemlen
#endif

#ifndef HAVE_TYPE_ATTR_T
#if !USE_WIDEC_SUPPORT && !defined(attr_t)
#define attr_t chtype
#endif
#endif

#undef NCURSES_CH_T
#if !USE_WIDEC_SUPPORT
#define NCURSES_CH_T chtype
#else
#define NCURSES_CH_T cchar_t
#endif

#ifndef NCURSES_COLOR_T
#define NCURSES_COLOR_T short
#endif

#ifndef NCURSES_PAIRS_T
#define NCURSES_PAIRS_T short
#endif

#ifndef CCHARW_MAX
#define CCHARW_MAX 5
#endif

#if defined(NCURSES_VERSION) && defined(CURSES_WACS_ARRAY) && !defined(CURSES_WACS_SYMBOLS)
#define CURSES_WACS_SYMBOLS
#endif

#if defined(CURSES_WACS_ARRAY) && !defined(CURSES_WACS_SYMBOLS)
/* NetBSD 5.1 defines these incorrectly */
#undef	WACS_RARROW
#undef	WACS_LARROW
#undef	WACS_UARROW
#undef	WACS_DARROW
#undef	WACS_BLOCK
#undef	WACS_DIAMOND
#undef	WACS_CKBOARD
#undef	WACS_DEGREE
#undef	WACS_PLMINUS
#undef	WACS_BOARD
#undef	WACS_LANTERN
#undef	WACS_LRCORNER
#undef	WACS_URCORNER
#undef	WACS_ULCORNER
#undef	WACS_LLCORNER
#undef	WACS_PLUS
#undef	WACS_HLINE
#undef	WACS_S1
#undef	WACS_S9
#undef	WACS_LTEE
#undef	WACS_RTEE
#undef	WACS_BTEE
#undef	WACS_TTEE
#undef	WACS_VLINE
#undef	WACS_BULLET
#undef	WACS_S3
#undef	WACS_S7
#undef	WACS_LEQUAL
#undef	WACS_GEQUAL
#undef	WACS_PI
#undef	WACS_NEQUAL
#undef	WACS_STERLING

#define WACS_RARROW     &(CURSES_WACS_ARRAY['+'])
#define WACS_LARROW     &(CURSES_WACS_ARRAY[','])
#define WACS_UARROW     &(CURSES_WACS_ARRAY['-'])
#define WACS_DARROW     &(CURSES_WACS_ARRAY['.'])
#define WACS_BLOCK      &(CURSES_WACS_ARRAY['0'])
#define WACS_DIAMOND    &(CURSES_WACS_ARRAY['`'])
#define WACS_CKBOARD    &(CURSES_WACS_ARRAY['a'])
#define WACS_DEGREE     &(CURSES_WACS_ARRAY['f'])
#define WACS_PLMINUS    &(CURSES_WACS_ARRAY['g'])
#define WACS_BOARD      &(CURSES_WACS_ARRAY['h'])
#define WACS_LANTERN    &(CURSES_WACS_ARRAY['i'])
#define WACS_LRCORNER   &(CURSES_WACS_ARRAY['j'])
#define WACS_URCORNER   &(CURSES_WACS_ARRAY['k'])
#define WACS_ULCORNER   &(CURSES_WACS_ARRAY['l'])
#define WACS_LLCORNER   &(CURSES_WACS_ARRAY['m'])
#define WACS_PLUS       &(CURSES_WACS_ARRAY['n'])
#define WACS_HLINE      &(CURSES_WACS_ARRAY['q'])
#define WACS_S1         &(CURSES_WACS_ARRAY['o'])
#define WACS_S9         &(CURSES_WACS_ARRAY['s'])
#define WACS_LTEE       &(CURSES_WACS_ARRAY['t'])
#define WACS_RTEE       &(CURSES_WACS_ARRAY['u'])
#define WACS_BTEE       &(CURSES_WACS_ARRAY['v'])
#define WACS_TTEE       &(CURSES_WACS_ARRAY['w'])
#define WACS_VLINE      &(CURSES_WACS_ARRAY['x'])
#define WACS_BULLET     &(CURSES_WACS_ARRAY['~'])
#define WACS_S3         &(CURSES_WACS_ARRAY['p'])
#define WACS_S7         &(CURSES_WACS_ARRAY['r'])
#define WACS_LEQUAL     &(CURSES_WACS_ARRAY['y'])
#define WACS_GEQUAL     &(CURSES_WACS_ARRAY['z'])
#define WACS_PI         &(CURSES_WACS_ARRAY['{'])
#define WACS_NEQUAL     &(CURSES_WACS_ARRAY['|'])
#define WACS_STERLING   &(CURSES_WACS_ARRAY['}'])
#endif

#ifndef WA_NORMAL
#define WA_NORMAL       A_NORMAL
#endif
#ifndef WA_BOLD
#define WA_BOLD         A_BOLD
#endif
#ifndef WA_REVERSE
#define WA_REVERSE      A_REVERSE
#endif
#ifndef WA_UNDERLINE
#define WA_UNDERLINE    A_UNDERLINE
#endif
#ifndef WA_BLINK
#define WA_BLINK        A_BLINK
#endif

#ifndef OK
#define OK (0)
#endif

#ifndef ERR
#define ERR (-1)
#endif

#undef CTRL
#define CTRL(x)	((x) & 0x1f)

#define QUIT		CTRL('Q')
#define ESCAPE		CTRL('[')

#ifndef KEY_MIN
#define KEY_MIN 256		/* not defined in Solaris 8 */
#endif

#define HELP_KEY_1	'?'
#define HELP_KEY_2	KEY_F(1)

/* from nc_string.h, to make this stand alone */
#if HAVE_BSD_STRING_H
#include <bsd/string.h>
#endif

#ifdef __cplusplus
#define NCURSES_VOID		/* nothing */
#else
#define NCURSES_VOID (void)
#endif

#ifndef HAVE_STRLCAT
#define HAVE_STRLCAT 0
#endif

#ifndef HAVE_STRLCPY
#define HAVE_STRLCPY 0
#endif

#ifndef HAVE_SNPRINTF
#define HAVE_SNPRINTF 0
#endif

#ifndef USE_STRING_HACKS
#define USE_STRING_HACKS 0
#endif

#ifndef NCURSES_CAST
#ifdef __cplusplus
extern "C" {
#define NCURSES_CAST(type,value) static_cast<type>(value)
#else
#define NCURSES_CAST(type,value) (type)(value)
#endif
#endif

#if USE_STRING_HACKS && HAVE_STRLCAT
#define _nc_STRCAT(d,s,n)	NCURSES_VOID strlcat((d),(s),NCURSES_CAST(size_t,n))
#define _nc_STRNCAT(d,s,m,n)	NCURSES_VOID strlcat((d),(s),NCURSES_CAST(size_t,m))
#else
#define _nc_STRCAT(d,s,n)	NCURSES_VOID strcat((d),(s))
#define _nc_STRNCAT(d,s,m,n)	NCURSES_VOID strncat((d),(s),(n))
#endif

#if USE_STRING_HACKS && HAVE_STRLCPY
#define _nc_STRCPY(d,s,n)	NCURSES_VOID strlcpy((d),(s),NCURSES_CAST(size_t,n))
#define _nc_STRNCPY(d,s,n)	NCURSES_VOID strlcpy((d),(s),NCURSES_CAST(size_t,n))
#else
#define _nc_STRCPY(d,s,n)	NCURSES_VOID strcpy((d),(s))
#define _nc_STRNCPY(d,s,n)	NCURSES_VOID strncpy((d),(s),(n))
#endif

#if USE_STRING_HACKS && HAVE_SNPRINTF
#define _nc_SPRINTF             NCURSES_VOID (snprintf)
#define _nc_SLIMIT(n)           NCURSES_CAST(size_t,n),
#else
#define _nc_SPRINTF             NCURSES_VOID (sprintf)
#define _nc_SLIMIT(n)		/* nothing */
#endif

/*
 * X/Open Curses does not define the arrays of terminfo/termcap names as SVr4
 * curses did, and some implementations provide them anyway, but undeclared.
 */
#ifdef DECL_CURSES_DATA_BOOLNAMES
extern char *boolnames[], *boolcodes[], *boolfnames[];
extern char *numnames[], *numcodes[], *numfnames[];
extern char *strnames[], *strcodes[], *strfnames[];
#endif

/*
 * Again, an SVr4 curses feature latent in the libraries but not in headers.
 */
#ifndef DECL_CURSES_DATA_TABSIZE
#define DECL_CURSES_DATA_TABSIZE 0
#endif

#if DECL_CURSES_DATA_TABSIZE
extern int TABSIZE;
#undef  HAVE_CURSES_DATA_TABSIZE
#define HAVE_CURSES_DATA_TABSIZE 1
#endif

#ifndef HAVE_CURSES_DATA_TABSIZE
#define HAVE_CURSES_DATA_TABSIZE 0
#endif

/*
 * X/Open Curses provides termname(), whose return value is analogous to the
 * SVr4 curses variable ttytype[].
 */
#ifndef HAVE_CURSES_DATA_TTYTYPE
#define HAVE_CURSES_DATA_TTYTYPE 0
#endif

#ifndef DECL_CURSES_DATA_TTYTYPE
#define DECL_CURSES_DATA_TTYTYPE 0
#endif

#if !defined(ttytype) && (!HAVE_CURSES_DATA_TTYTYPE || DECL_CURSES_DATA_TTYTYPE)
#define ttytype termname()
#endif

#define colored_chtype(ch, attr, pair) \
	((chtype) (ch) | (chtype) (attr) | (chtype) COLOR_PAIR(pair))

/*
 * Workaround for HPUX
 */
#if defined(__hpux) && !defined(NCURSES_VERSION)
#define getbegx(w) __getbegx(w)
#define getbegy(w) __getbegy(w)
#define getcurx(w) __getcurx(w)
#define getcury(w) __getcury(w)
#define getmaxx(w) __getmaxx(w)
#define getmaxy(w) __getmaxy(w)
#define getparx(w) __getparx(w)
#define getpary(w) __getpary(w)
#endif

/*
 * Workaround in case getcchar() returns a positive value when the source
 * string produces only a L'\0'.
 */
#define TEST_CCHAR(s, count, then_stmt, else_stmt) \
	if ((count = getcchar(s, NULL, NULL, NULL, NULL)) > 0) { \
	    wchar_t test_wch[CCHARW_MAX + 2]; \
	    attr_t test_attrs; \
	    NCURSES_PAIRS_T test_pair; \
	    \
	    if (getcchar( s, test_wch, &test_attrs, &test_pair, NULL) == OK \
		&& test_wch[0] != L'\0') { \
		then_stmt \
	    } else { \
		else_stmt \
	    } \
	} else { \
	    else_stmt \
	}
/*
 * These usually are implemented as macros, but may be functions.
 */
#if !defined(getcurx) && !HAVE_GETCURX
#define getcurx(win)            ((win) ? ((int)(win)->_curx) : ERR)
#define getcury(win)            ((win) ? ((int)(win)->_cury) : ERR)
#endif

#if !defined(getbegx) && !HAVE_GETBEGX
#define getbegx(win)            ((win) ? ((int)(win)->_begx) : ERR)
#define getbegy(win)            ((win) ? ((int)(win)->_begy) : ERR)
#endif

#if !defined(getmaxx) && !HAVE_GETMAXX
#define getmaxx(win)            ((win) ? ((int)(win)->_maxx + 1) : ERR)
#define getmaxy(win)            ((win) ? ((int)(win)->_maxy + 1) : ERR)
#endif

/*
 * Solaris 10 xpg4:
#define	__m_getparx(w)		((w)->_parent == (WINDOW *) 0 ? -1 \
				: (w)->_begx - (w)->_parent->_begx)
 */
#if !defined(getparx) && !HAVE_GETPARX
#ifdef __m_getparx
#define getparx(win)            __m_getparx(win)
#define getpary(win)            __m_getpary(win)
#else
#define getparx(win)            ((win)?((win)->_parx + 1):ERR)
#define getpary(win)            ((win)?((win)->_pary + 1):ERR)
#endif
#endif

#if !defined(mvwvline) && !HAVE_MVWVLINE
#define mvwvline(w,y,x,ch,n)    (move(y,x) == ERR ? ERR : wvline(w,ch,n))
#define mvwhline(w,y,x,ch,n)    (move(y,x) == ERR ? ERR : whline(w,ch,n))
#endif

#if !defined(mvvline) && !HAVE_MVVLINE
#define mvvline(y,x,ch,n)       (move(y,x) == ERR ? ERR : vline(ch,n))
#define mvhline(y,x,ch,n)       (move(y,x) == ERR ? ERR : hline(ch,n))
#endif

/*
 * Try to accommodate curses implementations that have no terminfo support.
 */
#if HAVE_TIGETNUM
#define TIGETNUM(ti,tc) tigetnum(ti)
#else
#define TIGETNUM(ti,tc) tgetnum(tc)
#endif

#if HAVE_TIGETSTR
#define TIGETSTR(ti,tc) tigetstr(ti)
#else
#define TIGETSTR(ti,tc) tgetstr(tc,&area_pointer)
#endif

/*
 * So far (2013 - more than ten years), only ncurses implements
 * use_extended_names().
 */
#if defined(NCURSES_XNAMES)
#elif defined(NCURSES_VERSION) && defined(HAVE_TERM_ENTRY_H) && HAVE_TERM_ENTRY_H
#define NCURSES_XNAMES 1
#else
#define NCURSES_XNAMES 0
#endif

/*
 * ncurses restores the cursor in endwin().  Other libraries may not.
 */
#ifdef NCURSES_VERSION
#define stop_curses() endwin()
#else
#define stop_curses() do { endwin(); curs_set(1); } while (0)
#endif

/* ncurses implements tparm() with varargs, X/Open with a fixed-parameter list
 * (which is incompatible with legacy usage, doesn't solve any problems).
 */
#define tparm3(a,b,c) tparm(a,b,c,0,0,0,0,0,0,0)
#define tparm2(a,b)   tparm(a,b,0,0,0,0,0,0,0,0)

#define UChar(c)    ((unsigned char)(c))

#define SIZEOF(table)	(sizeof(table)/sizeof(table[0]))

#if defined(NCURSES_VERSION) && HAVE_NC_ALLOC_H
#include <nc_alloc.h>
#if HAVE_EXIT_TERMINFO && (defined(USE_TERMINFO) || defined(USE_TINFO))
#undef ExitProgram
#define ExitProgram(code) exit_terminfo(code)
#elif HAVE_EXIT_CURSES
#undef ExitProgram
#define ExitProgram(code) exit_curses(code)
#endif
#else /* not ncurses-tree */
#define typeMalloc(type,n) (type *) malloc((size_t)(n) * sizeof(type))
#define typeCalloc(type,elts) (type *) calloc((size_t)(elts), sizeof(type))
#define typeRealloc(type,n,p) (type *) realloc(p, (size_t)(n) * sizeof(type))
#endif

#ifndef ExitProgram
#define ExitProgram(code) exit(code)
#endif

#ifndef EXIT_SUCCESS
#define EXIT_SUCCESS 0
#endif
#ifndef EXIT_FAILURE
#define EXIT_FAILURE 1
#endif

#undef _NC_WINDOWS
#if (defined(_WIN32) || defined(_WIN64))
#define _NC_WINDOWS 1
#endif

#if defined(_NC_WINDOWS) || defined(USE_WIN32CON_DRIVER)

#if defined(PDCURSES)
#ifdef WINVER
#  if WINVER < 0x0501
#    error WINVER must at least be 0x0501
#  endif
#else
#  define WINVER 0x0501
#endif
#include <windows.h>
#include <sys/time.h>		/* for struct timeval */
#undef sleep
#define sleep(n) Sleep((n) * 1000)
#define SIGHUP  1
#define SIGKILL 9
#define getlogin() "username"

#elif defined(EXP_WIN32_DRIVER)

#if defined(HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/nc_win32.h>
#elif defined(HAVE_NCURSES_NCURSES_H)
#include <ncurses/nc_win32.h>
#else
#include <nc_win32.h>
#endif

#else

#if defined(HAVE_NCURSESW_NCURSES_H)
#include <ncursesw/nc_mingw.h>
#elif defined(HAVE_NCURSES_NCURSES_H)
#include <ncurses/nc_mingw.h>
#else
#include <nc_mingw.h>
#endif

#endif

/* conflicts in test/firstlast.c */
#undef large
#undef small

#endif

#ifdef NEED_TIME_H
#if TIME_WITH_SYS_TIME
# include <sys/time.h>
# include <time.h>
#else
# if HAVE_SYS_TIME_H
#  include <sys/time.h>
# else
#  include <time.h>
# endif
#endif
#endif

/*
 * Ultrix 3.1
 */
#ifndef STDOUT_FILENO
#define STDOUT_FILENO 1
#endif

#if !HAVE_STRSTR
extern char *_nc_strstr(const char *, const char *);
#define strstr(a,b) _nc_strstr((a),(b))
#endif /* !HAVE_STRSTR */

/* Use this to quiet gcc's -Wwrite-strings warnings, but accommodate SVr4
 * curses which doesn't have const parameters declared (so far) in the places
 * that XSI shows.
 */
#ifndef NCURSES_CONST
#ifdef PDCURSES
#define NCURSES_CONST		const	/* close enough */
#else
#define NCURSES_CONST		/* nothing */
#endif
#endif

/* out-of-band values for representing absent capabilities */
#define ABSENT_BOOLEAN		((signed char)-1)	/* 255 */
#define ABSENT_NUMERIC		(-1)
#define ABSENT_STRING		(char *)0

/* out-of-band values for representing cancels */
#define CANCELLED_BOOLEAN	((signed char)-2)	/* 254 */
#define CANCELLED_NUMERIC	(-2)
#define CANCELLED_STRING	(char *)(-1)

#define VALID_BOOLEAN(s) ((unsigned char)(s) <= 1)	/* reject "-1" */
#define VALID_NUMERIC(s) ((s) >= 0)
#define VALID_STRING(s)  ((s) != CANCELLED_STRING && (s) != ABSENT_STRING)

#define VT_ACSC "``aaffggiijjkkllmmnnooppqqrrssttuuvvwwxxyyzz{{||}}~~"

#define CATCHALL(handler) do { \
		int nsig; \
		for (nsig = SIGHUP; nsig < SIGTERM; ++nsig) \
		    if (nsig != SIGKILL) \
			signal(nsig, handler); \
	    } while(0)

#ifdef NCURSES_VERSION
#define InitAndCatch(init,handler) do { CATCHALL(handler); init; } while (0)
#else
#define InitAndCatch(init,handler) do { init; CATCHALL(handler); } while (0)
#endif

#if defined(_NC_WINDOWS) || defined(USE_WIN32CON_DRIVER)
#define SetupAlarm(opt)	(void)opt
#else
#define SetupAlarm(opt)	if (opt) alarm((unsigned)opt)
#endif

/*
 * Workaround for clean(er) compile with Solaris's legacy curses.
 * The same would be needed for HPUX 10.20
 */
#ifndef TPUTS_ARG
#define TPUTS_ARG int
#endif

#if defined(sun) && !defined(_XOPEN_CURSES) && !defined(NCURSES_VERSION_PATCH)
#undef TPUTS_ARG
#define TPUTS_ARG char
extern char *tgoto(char *, int, int);	/* available, but not prototyped */
#endif

#ifndef TPUTS_PROTO
#define TPUTS_PROTO(func,value) int func(TPUTS_ARG value)
#endif

#ifndef TPUTS_RETURN
#define TPUTS_RETURN(value) return value
#endif

/*
 * Workarounds for Solaris's X/Open curses
 */
#if !defined(KEY_MIN) && defined(__KEY_MIN)
#define KEY_MIN __KEY_MIN
#endif
#if !defined(KEY_MAX) && defined(__KEY_MIN)
#define KEY_MAX __KEY_MAX
#endif

/*
 * Workaround to build with Sun's default SVr4 curses.
 */
#ifdef NCURSES_VERSION
#ifndef HAVE_VW_PRINTW
#define HAVE_VW_PRINTW 1
#endif
#endif

/*
 * ncurses provides arrays of capability names; X/Open discarded these SVr4
 * features.  Some implementations continue to provide them (see the test
 * configure script).
 */
#ifdef NCURSES_VERSION
#ifndef HAVE_CURSES_DATA_BOOLNAMES
#define HAVE_CURSES_DATA_BOOLNAMES 1
#endif
#endif

/*
 * ncurses provides a termcap interface; a few packagers replace or displace
 * its header file with an incompatible one.  The demo_termcap program uses
 * the ncurses file, if available.
 */
#ifdef NCURSES_VERSION
#ifndef HAVE_NCURSES_TERMCAP_H
#define HAVE_NCURSES_TERMCAP_H 0
#endif
#ifndef HAVE_TERMCAP_H
#define HAVE_TERMCAP_H 0
#endif
#endif

/*
 * ncurses uses const in some places where X/Open does (or did) not allow.
 */
#if defined(NCURSES_CONST)
#define CONST_MENUS NCURSES_CONST
#elif defined(PDCURSES)
#define CONST_MENUS const
#else
#define CONST_MENUS		/* nothing */
#endif

/*
 * Simplify setting up demo of threading with these macros.
 */

#if HAVE_USE_WINDOW
#define USING_WINDOW(w,func) use_window(w, (NCURSES_WINDOW_CB) func, w)
#define USING_WINDOW1(w,func,safe) use_window(w, (NCURSES_WINDOW_CB) safe, NULL)
#define USING_WINDOW2(w,func,data) use_window(w, (NCURSES_WINDOW_CB) func, data)
#define WANT_USE_WINDOW() extern void _nc_want_use_window(void)
#else
#define USING_WINDOW(w,func) func(w, NULL)
#define USING_WINDOW1(w,func,safe) func(w)
#define USING_WINDOW2(w,func,data) func(w,data)
#define WANT_USE_WINDOW() extern void _nc_want_use_window(void)
#endif

#if HAVE_USE_WINDOW
#define USING_SCREEN(s,func,data) use_screen(s, (NCURSES_SCREEN_CB) func, data)
#define WANT_USE_SCREEN() extern void _nc_want_use_screen(void)
#else
#define USING_SCREEN(s,func,data) func(data)
#define WANT_USE_SCREEN() extern void _nc_want_use_screen(void)
#endif

#if defined(TRACE) && HAVE__TRACEF
#define Trace(p) _tracef p
#define USE_TRACE 1
#define START_TRACE() \
	if ((_nc_tracing & TRACE_MAXIMUM) == 0) { \
	    int t = _nc_getenv_num("NCURSES_TRACE"); \
	    if (t >= 0) \
		curses_trace((unsigned) t); \
	}
extern unsigned _nc_tracing;
extern int _nc_getenv_num(const char *);
#else
#undef TRACE
#define Trace(p)		/* nothing */
#define USE_TRACE 0
#define START_TRACE()		/* nothing */
#endif

#define Trace2(p)		/* nothing */

#define AddCh(c)		(void) addch((chtype)(c))
#define WAddCh(w,c)		(void) waddch((w),(chtype)(c))
#define MvAddCh(y,x,c)		(void) mvaddch((y),(x),(chtype)(c))
#define MvWAddCh(w,y,x,c)	(void) mvwaddch((w),(y),(x),(chtype)(c))
#define MvAddStr(y,x,s)		(void) mvaddstr((y),(x),(s))
#define MvWAddStr(w,y,x,s)	(void) mvwaddstr((w),(y),(x),(s))
#define MvWAddChStr(w,y,x,s)	(void) mvwaddchstr((w),(y),(x),(s))
#define MvPrintw		(void) mvprintw
#define MvWPrintw		(void) mvwprintw
#define MvHLine			(void) mvhline
#define MvWHLine		(void) mvwhline
#define MvVLine			(void) mvvline
#define MvWVLine		(void) mvwvline

/*
 * The macro likely uses unsigned values, while X/Open prototype uses int.
 */
#if defined(wattrset) || defined(PDCURSES)
#define AttrArg(p,a)    (chtype) ((chtype)(p) | (chtype)(a))
#else
#define AttrArg(p,a)    (int) ((chtype)(p) | (chtype)(a))
#endif

/*
 * Workaround for defective implementation of gcc attribute warn_unused_result
 */
#if defined(__GNUC__) && defined(_FORTIFY_SOURCE)
#define IGNORE_RC(func) errno = func
#else
#define IGNORE_RC(func) (void) func
#endif /* gcc workarounds */

#define init_mb(state)	memset(&state, 0, sizeof(state))

#endif /* __TEST_PRIV_H */
