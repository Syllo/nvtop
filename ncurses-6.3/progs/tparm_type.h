/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2014 Free Software Foundation, Inc.                            *
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

/*
 * $Id: tparm_type.h,v 1.3 2020/10/24 17:11:33 tom Exp $
 *
 * determine expected/actual number of parameters to setup for tparm
 */
#ifndef TPARM_TYPE_H
#define TPARM_TYPE_H 1

#define USE_LIBTINFO
#include <progs.priv.h>

typedef enum {
    Other = -1
    ,Numbers = 0
    ,Num_Str
    ,Num_Str_Str
} TParams;

extern TParams tparm_type(const char *name);
extern TParams guess_tparm_type(int nparam, char **p_is_s);

#endif /* TPARM_TYPE_H */
