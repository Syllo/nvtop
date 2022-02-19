/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2011,2015 Free Software Foundation, Inc.                       *
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
 *   Author:  Thomas E. Dickey, 2011                                        *
 ****************************************************************************/

/*
    Version Control
    $Id: ncurses_compat.c,v 1.4 2020/02/02 23:34:34 tom Exp $
  --------------------------------------------------------------------------*/

/*
 * Provide compatibility with older versions of ncurses.
 */
#include <ncurses_cfg.h>

#if HAVE_INTTYPES_H
# include <inttypes.h>
#else
# if HAVE_STDINT_H
#  include <stdint.h>
# endif
#endif

#include <curses.h>

#if defined(NCURSES_VERSION_PATCH)

#if NCURSES_VERSION_PATCH < 20081122
extern bool has_mouse(void);
extern int _nc_has_mouse(void);

bool
has_mouse(void)
{
  return (bool)_nc_has_mouse();
}
#endif

/*
 * These are provided by lib_gen.c:
 */
#if NCURSES_VERSION_PATCH < 20070331
extern bool (is_keypad) (const WINDOW *);
extern bool (is_scrollok) (const WINDOW *);

bool
is_keypad(const WINDOW *win)
{
  return ((win)->_use_keypad);
}

bool
  (is_scrollok) (const WINDOW *win)
{
  return ((win)->_scroll);
}
#endif

#if NCURSES_VERSION_PATCH < 20060107
extern int (getbegx) (WINDOW *);
extern int (getbegy) (WINDOW *);
extern int (getcurx) (WINDOW *);
extern int (getcury) (WINDOW *);
extern int (getmaxx) (WINDOW *);
extern int (getmaxy) (WINDOW *);
extern int (getparx) (WINDOW *);
extern int (getpary) (WINDOW *);

int
  (getbegy) (WINDOW *win)
{
  return ((win) ? (win)->_begy : ERR);
}

int
  (getbegx) (WINDOW *win)
{
  return ((win) ? (win)->_begx : ERR);
}

int
  (getcury) (WINDOW *win)
{
  return ((win) ? (win)->_cury : ERR);
}

int
  (getcurx) (WINDOW *win)
{
  return ((win) ? (win)->_curx : ERR);
}

int
  (getmaxy) (WINDOW *win)
{
  return ((win) ? ((win)->_maxy + 1) : ERR);
}

int
  (getmaxx) (WINDOW *win)
{
  return ((win) ? ((win)->_maxx + 1) : ERR);
}

int
  (getpary) (WINDOW *win)
{
  return ((win) ? (win)->_pary : ERR);
}

int
  (getparx) (WINDOW *win)
{
  return ((win) ? (win)->_parx : ERR);
}
#endif

#endif
