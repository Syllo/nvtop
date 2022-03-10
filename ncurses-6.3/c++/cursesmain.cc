// * this is for making emacs happy: -*-Mode: C++;-*-
/****************************************************************************
 * Copyright 2019,2020 Thomas E. Dickey                                     *
 * Copyright 1998-2007,2013 Free Software Foundation, Inc.                  *
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

#include "internal.h"
#include "cursesapp.h"

#if CPP_HAS_TRY_CATCH && HAVE_IOSTREAM
#pragma GCC diagnostic ignored "-Weffc++"
#include <iostream>
#pragma GCC diagnostic warning "-Weffc++"
#else
#undef CPP_HAS_TRY_CATCH
#define CPP_HAS_TRY_CATCH 0
#endif

MODULE_ID("$Id: cursesmain.cc,v 1.20 2020/07/18 19:57:11 anonymous.maarten Exp $")

#if HAVE_LOCALE_H
#include <locale.h>
#else
#define setlocale(name,string) /* nothing */
#endif

#if NO_LEAKS
#include <nc_alloc.h>
#endif

/* This is the default implementation of main() for a NCursesApplication.
 * You only have to instantiate a static NCursesApplication object in your
 * main application source file and link this module with your application.
 */
int NCURSES_CXX_MAIN_NAME(int argc, char* argv[])
{
  setlocale(LC_ALL, "");

  NCursesApplication* A = NCursesApplication::getApplication();
  if (!A)
    return(1);
  else {
    int res;

    A->handleArgs(argc,argv);
    ::endwin();
#if CPP_HAS_TRY_CATCH
    try {
      res = (*A)();
      ::endwin();
    }
    catch(const NCursesException &e) {
      ::endwin();
      std::cerr << e.message << std::endl;
      res = e.errorno;
    }
#else
    res = (*A)();
    ::endwin();
#endif
#if NO_LEAKS
    delete A;
    exit_curses(res);
#else
    return(res);
#endif
  }
}
