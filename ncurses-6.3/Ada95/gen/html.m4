dnl***************************************************************************
dnl Copyright 2019,2020 Thomas E. Dickey                                     *
dnl Copyright 2000-2006,2007 Free Software Foundation, Inc.                  *
dnl                                                                          *
dnl Permission is hereby granted, free of charge, to any person obtaining a  *
dnl copy of this software and associated documentation files (the            *
dnl "Software"), to deal in the Software without restriction, including      *
dnl without limitation the rights to use, copy, modify, merge, publish,      *
dnl distribute, distribute with modifications, sublicense, and/or sell       *
dnl copies of the Software, and to permit persons to whom the Software is    *
dnl furnished to do so, subject to the following conditions:                 *
dnl                                                                          *
dnl The above copyright notice and this permission notice shall be included  *
dnl in all copies or substantial portions of the Software.                   *
dnl                                                                          *
dnl THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  *
dnl OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               *
dnl MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   *
dnl IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   *
dnl DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    *
dnl OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    *
dnl THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               *
dnl                                                                          *
dnl Except as contained in this notice, the name(s) of the above copyright   *
dnl holders shall not be used in advertising or otherwise to promote the     *
dnl sale, use or other dealings in this Software without prior written       *
dnl authorization.                                                           *
dnl***************************************************************************
dnl
dnl $Id: html.m4,v 1.5 2020/02/02 23:34:34 tom Exp $
define(`ANCHORIDX',`0')dnl
define(`MANPAGE',`define(`MANPG',$1)dnl
|=====================================================================
   --  | Man page <A HREF="../man/MANPG.html">MANPG</A>
   --  |=====================================================================')dnl
define(`ANCHOR',`define(`ANCHORIDX',incr(ANCHORIDX))dnl
`#'1A NAME="AFU`_'ANCHORIDX"`#'2dnl
define(`CFUNAME',`$1')define(`AFUNAME',`$2')dnl
|#1/A#2')dnl
define(`AKA',``AKA': <A HREF="../man/MANPG.html">CFUNAME</A>')dnl
define(`ALIAS',``AKA': $1')dnl
