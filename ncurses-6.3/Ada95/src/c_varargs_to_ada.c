/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2011,2014 Free Software Foundation, Inc.                       *
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
 *   Author:  Nicolas Boulenguez, 2011                                      *
 ****************************************************************************/

/*
    Version Control
    $Id: c_varargs_to_ada.c,v 1.7 2020/02/02 23:34:34 tom Exp $
  --------------------------------------------------------------------------*/
/*
  */

#include "c_varargs_to_ada.h"

int
set_field_type_alnum(FIELD *field,
		     int minimum_width)
{
  return set_field_type(field, TYPE_ALNUM, minimum_width);
}

int
set_field_type_alpha(FIELD *field,
		     int minimum_width)
{
  return set_field_type(field, TYPE_ALPHA, minimum_width);
}

int
set_field_type_enum(FIELD *field,
		    char **value_list,
		    int case_sensitive,
		    int unique_match)
{
  return set_field_type(field, TYPE_ENUM, value_list, case_sensitive,
			unique_match);
}

int
set_field_type_integer(FIELD *field,
		       int precision,
		       long minimum,
		       long maximum)
{
  return set_field_type(field, TYPE_INTEGER, precision, minimum, maximum);
}

int
set_field_type_numeric(FIELD *field,
		       int precision,
		       double minimum,
		       double maximum)
{
  return set_field_type(field, TYPE_NUMERIC, precision, minimum, maximum);
}

int
set_field_type_regexp(FIELD *field,
		      char *regular_expression)
{
  return set_field_type(field, TYPE_REGEXP, regular_expression);
}

int
set_field_type_ipv4(FIELD *field)
{
  return set_field_type(field, TYPE_IPV4);
}

int
set_field_type_user(FIELD *field,
		    FIELDTYPE *fieldtype,
		    void *arg)
{
  return set_field_type(field, fieldtype, arg);
}

void *
void_star_make_arg(va_list *list)
{
  return va_arg(*list, void *);
}

#ifdef TRACE
void
_traces(const char *fmt, char *arg)
{
  _tracef(fmt, arg);
}
#endif
