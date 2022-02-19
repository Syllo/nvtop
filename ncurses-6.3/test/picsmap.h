/****************************************************************************
 * Copyright 2020,2021 Thomas E. Dickey                                     *
 * Copyright 2017 Free Software Foundation, Inc.                            *
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
 *  Author: Thomas E. Dickey                                                *
 ****************************************************************************/
/* $Id: picsmap.h,v 1.5 2021/04/24 23:25:17 tom Exp $ */

#ifndef PICSMAP_H_INCL
#define PICSMAP_H_INCL 1

typedef int NUM_COLOR;
typedef unsigned short NUM_COUNT;

typedef struct {
    char ch;			/* nominal character to display */
    NUM_COLOR fg;		/* foreground color */
} PICS_CELL;

typedef struct {
    NUM_COLOR fgcol;
    NUM_COUNT count;
} FG_NODE;

typedef struct {
    char *name;
    short high;
    short wide;
    int colors;
    FG_NODE *fgcol;
    PICS_CELL *cells;
} PICS_HEAD;

typedef struct {
    const char *name;
    int value;
} RGB_NAME;

typedef struct {
    short red;
    short green;
    short blue;
} RGB_DATA;

#endif /* PICSMAP_H_INCL */
