// * This makes emacs happy -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
 * Copyright 1998-2008,2012 Free Software Foundation, Inc.                  *
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
 *   Author: Juergen Pfeifer, 1997                                          *
 ****************************************************************************/

// $Id: internal.h,v 1.22 2020/08/29 23:06:41 tom Exp $

#ifndef NCURSES_CPLUS_INTERNAL_H
#define NCURSES_CPLUS_INTERNAL_H 1

#include <ncurses_cfg.h>

#if USE_RCS_IDS
#define MODULE_ID(id) static const char Ident[] = id;
#else
#define MODULE_ID(id)		/*nothing */
#endif

#if (defined(_WIN32) || defined(_WIN64))
#if defined(EXP_WIN32_DRIVER)
#include <nc_win32.h>
#else
#include <nc_mingw.h>
#endif
#undef KEY_EVENT
#endif

#ifndef _QNX_SOURCE
#include <stdlib.h>
#include <string.h>
#endif

#ifndef CTRL
#define CTRL(x) ((x) & 0x1f)
#endif

#ifndef NULL
#define NULL 0
#endif

#include <nc_string.h>

#endif /* NCURSES_CPLUS_INTERNAL_H */
