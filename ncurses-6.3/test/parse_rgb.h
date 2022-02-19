/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
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
 * $Id: parse_rgb.h,v 1.5 2020/02/02 23:34:34 tom Exp $
 *
 * Sample implementation of ncurses RGB extension from user_caps(5).
 */
#ifndef PARSE_RBG_H_incl
#define PARSE_RBG_H_incl 1

#include <test.priv.h>

#if HAVE_TIGETSTR && USE_WIDEC_SUPPORT
static int
parse_rgb(int *r_max, int *g_max, int *b_max)
{
    int colors = tigetnum("colors");
    int result = ERR;

    *r_max = *g_max = *b_max = 0;

    if (colors > 0) {
	int max_bits;
	int bits;
	int pwr2;
	int r = 0, g = 0, b = 0;
	char *data;
	char ch;

	for (max_bits = 0, pwr2 = 1;
	     pwr2 < colors;
	     ++max_bits, pwr2 <<= 1) {
	    ;
	}

	if (tigetflag("RGB") > 0) {
	    result = OK;
	    r = g = b = (max_bits + 2) / 3;
	} else if ((bits = tigetnum("RGB")) > 0) {
	    result = OK;
	    r = g = b = bits;
	} else if ((data = tigetstr("RGB")) != ABSENT_STRING
		   && data != CANCELLED_STRING
		   && sscanf(data, "%d/%d/%d%c", &r, &g, &b, &ch) == 3) {
	    result = OK;
	}

	if ((r + g + b) < max_bits) {
	    result = ERR;
	} else if (result == 0) {
	    if (r > max_bits) {
		r = max_bits;
		g = b = 0;
	    }
	    *r_max = r;
	    if (g > (max_bits -= r)) {
		g = max_bits;
		b = 0;
	    }
	    *g_max = g;
	    if (b > (max_bits -= g)) {
		b = max_bits;
	    }
	    *b_max = b;
	}
    }
    return result;
}
#else
#define parse_rgb(r,g,b) (ERR)
#endif

#endif /* PARSE_RBG_H_incl */
