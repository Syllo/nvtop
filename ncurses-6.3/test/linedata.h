/****************************************************************************
 * Copyright 2018,2020 Thomas E. Dickey                                     *
 * Copyright 2009-2010,2012 Free Software Foundation, Inc.                  *
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
 * $Id: linedata.h,v 1.8 2020/02/02 23:34:34 tom Exp $
 *
 * Utility functions for reading a line of text from a file.
 */
#ifndef LINEDATA_H_incl
#define LINEDATA_H_incl 1

#include <test.priv.h>

#define isQUIT(c)     ((c) == QUIT || (c) == ESCAPE)

#define key_RECUR     CTRL('W')
#define key_NEWLINE   CTRL('N')
#define key_BACKSPACE '\b'

static FILE *linedata;

static void
failed(const char *s)
{
    perror(s);
    ExitProgram(EXIT_FAILURE);
}

static void
init_linedata(const char *name)
{
    if ((linedata = fopen(name, "r")) == 0) {
	failed(name);
    }
}

static int
read_linedata(WINDOW *work)
{
    int result;
    if (linedata != 0) {
	result = fgetc(linedata);
	if (result == EOF) {
	    fclose(linedata);
	    linedata = 0;
	    result = read_linedata(work);
	} else {
	    wrefresh(work);
	    if (result == '\n') {
		result = key_NEWLINE;
	    }
	}
    } else {
#ifdef WIDE_LINEDATA
	wint_t ch;
	int code;

	result = ERR;
	while ((code = wget_wch(work, &ch)) != ERR) {

	    if (code == KEY_CODE_YES) {
		switch (ch) {
		case KEY_DOWN:
		    result = key_NEWLINE;
		    break;
		case KEY_BACKSPACE:
		    result = key_BACKSPACE;
		    break;
		default:
		    beep();
		    continue;
		}
	    } else {
		result = (int) ch;
		break;
	    }
	}
#else
	result = wgetch(work);
#endif
    }
    return result;
}

#endif /* LINEDATA_H_incl */
