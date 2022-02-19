/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2014,2015 Free Software Foundation, Inc.                       *
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

#include <tparm_type.h>

MODULE_ID("$Id: tparm_type.c,v 1.4 2020/10/24 17:30:32 tom Exp $")

/*
 * Lookup the type of call we should make to tparm().  This ignores the actual
 * terminfo capability (bad, because it is not extensible), but makes this
 * code portable to platforms where sizeof(int) != sizeof(char *).
 */
TParams
tparm_type(const char *name)
{
#define TD(code, longname, ti, tc) \
    	{code, {longname} }, \
	{code, {ti} }, \
	{code, {tc} }
    TParams result = Numbers;
    /* *INDENT-OFF* */
    static const struct {
	TParams code;
	const char name[12];
    } table[] = {
	TD(Num_Str,	"pkey_key",	"pfkey",	"pk"),
	TD(Num_Str,	"pkey_local",	"pfloc",	"pl"),
	TD(Num_Str,	"pkey_xmit",	"pfx",		"px"),
	TD(Num_Str,	"plab_norm",	"pln",		"pn"),
	TD(Num_Str_Str, "pkey_plab",	"pfxl",		"xl"),
    };
    /* *INDENT-ON* */

    unsigned n;
    for (n = 0; n < SIZEOF(table); n++) {
	if (!strcmp(name, table[n].name)) {
	    result = table[n].code;
	    break;
	}
    }
    return result;
}

TParams
guess_tparm_type(int nparam, char **p_is_s)
{
    TParams result = Other;
    switch (nparam) {
    case 0:
    case 1:
	if (!p_is_s[0])
	    result = Numbers;
	break;
    case 2:
	if (!p_is_s[0] && !p_is_s[1])
	    result = Numbers;
	if (!p_is_s[0] && p_is_s[1])
	    result = Num_Str;
	break;
    case 3:
	if (!p_is_s[0] && !p_is_s[1] && !p_is_s[2])
	    result = Numbers;
	if (!p_is_s[0] && p_is_s[1] && p_is_s[2])
	    result = Num_Str_Str;
	break;
    default:
	break;
    }
    return result;
}
