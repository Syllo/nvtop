/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
 * Copyright 2012 Free Software Foundation, Inc.                            *
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

#ifndef __WIDECHARS_H
#define __WIDECHARS_H 1

#include <test.priv.h>

#if USE_WIDEC_SUPPORT

#if defined(_NC_WINDOWS) && !defined(_MSC_VER) && !defined(EXP_WIN32_DRIVER)
/*
 * MinGW has wide-character functions, but they do not work correctly.
 */

extern int _nc_mbtowc(wchar_t *pwc, const char *s, size_t n);
extern int __MINGW_NOTHROW _nc_mbtowc(wchar_t *pwc, const char *s, size_t n);
#define mbtowc(pwc,s,n) _nc_mbtowc(pwc,s,n)

extern int __MINGW_NOTHROW _nc_mblen(const char *, size_t);
#define mblen(s,n) _nc_mblen(s, n)

#endif /* _WIN32||_WIN64 */

#if HAVE_MBTOWC && HAVE_MBLEN
#define reset_mbytes(state) IGNORE_RC(mblen(NULL, 0)), IGNORE_RC(mbtowc(NULL, NULL, 0))
#define count_mbytes(buffer,length,state) mblen(buffer,length)
#define check_mbytes(wch,buffer,length,state) \
	(int) mbtowc(&wch, buffer, length)
#define state_unused
#elif HAVE_MBRTOWC && HAVE_MBRLEN
#define reset_mbytes(state) init_mb(state)
#define count_mbytes(buffer,length,state) mbrlen(buffer,length,&state)
#define check_mbytes(wch,buffer,length,state) \
	(int) mbrtowc(&wch, buffer, length, &state)
#else
make an error
#endif

#else

#endif /* USE_WIDEC_SUPPORT */

extern void widechars_stub(void);

#endif /* __WIDECHARS_H */
