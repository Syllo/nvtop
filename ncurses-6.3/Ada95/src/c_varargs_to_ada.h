/****************************************************************************
 * Copyright 2020 Thomas E. Dickey                                          *
 * Copyright 2011,2015 Free Software Foundation, Inc.                       *
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

/* $Id: c_varargs_to_ada.h,v 1.5 2020/02/02 23:34:34 tom Exp $ */

#ifndef __C_VARARGS_TO_ADA_H
#define __C_VARARGS_TO_ADA_H

#ifdef HAVE_CONFIG_H
#include <ncurses_cfg.h>
#else
#include <ncurses.h>
#endif

#include <stdlib.h>

#include <form.h>

extern int set_field_type_alnum(FIELD * /* field */ ,
				int /* minimum_width */ );

extern int set_field_type_alpha(FIELD * /* field */ ,
				int /* minimum_width */ );

extern int set_field_type_enum(FIELD * /* field */ ,
			       char ** /* value_list */ ,
			       int /* case_sensitive */ ,
			       int /* unique_match */ );

extern int set_field_type_integer(FIELD * /* field */ ,
				  int /* precision */ ,
				  long /* minimum */ ,
				  long /* maximum */ );

extern int set_field_type_numeric(FIELD * /* field */ ,
				  int /* precision */ ,
				  double /* minimum */ ,
				  double /* maximum */ );

extern int set_field_type_regexp(FIELD * /* field */ ,
				 char * /* regular_expression */ );

extern int set_field_type_ipv4(FIELD * /* field */ );

extern int set_field_type_user(FIELD * /* field */ ,
			       FIELDTYPE * /* fieldtype */ ,
			       void * /* arg */ );

extern void *void_star_make_arg(va_list * /* list */ );

#ifdef TRACE
extern void _traces(const char *	/* fmt */
		    ,char * /* arg */ );
#endif

#endif /* __C_VARARGS_TO_ADA_H */
