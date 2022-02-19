/****************************************************************************
 * Copyright 2018-2020,2021 Thomas E. Dickey                                *
 * Copyright 2011-2012,2016 Free Software Foundation, Inc.                  *
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
 * $Id: color_name.h,v 1.9 2021/04/24 23:25:29 tom Exp $
 */

#ifndef __COLORNAME_H
#define __COLORNAME_H 1

#ifndef __TEST_PRIV_H
#include <test.priv.h>
#endif

static NCURSES_CONST char *const the_color_names[] =
{
    "black",
    "red",
    "green",
    "yellow",
    "blue",
    "magenta",
    "cyan",
    "white",
    "BLACK",
    "RED",
    "GREEN",
    "YELLOW",
    "BLUE",
    "MAGENTA",
    "CYAN",
    "WHITE"
};

#ifdef NEED_COLOR_CODE
static int
color_code(const char *color)
{
    int result = 0;
    char *endp = 0;
    size_t n;

    if ((result = (int) strtol(color, &endp, 0)) >= 0
	&& (endp == 0 || *endp == 0)) {
	;
    } else if (!strcmp(color, "default")) {
	result = -1;
    } else {
	for (n = 0; n < SIZEOF(the_color_names); ++n) {
	    if (!strcmp(the_color_names[n], color)) {
		result = (int) n;
		break;
	    }
	}
    }
    return result;
}
#endif /* NEED_COLOR_CODE */

#ifdef NEED_COLOR_NAME
static const char *
color_name(int color)
{
    static char temp[20];
    const char *result = 0;

    if (color >= (int) SIZEOF(the_color_names)) {
	_nc_SPRINTF(temp, _nc_SLIMIT(sizeof(temp)) "%d", color);
	result = temp;
    } else if (color < 0) {
	result = "default";
    } else {
	result = the_color_names[color];
    }
    return result;
}
#endif /* NEED_COLOR_NAME */

#endif /* __COLORNAME_H */
