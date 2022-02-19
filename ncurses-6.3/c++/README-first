-------------------------------------------------------------------------------
-- Copyright 2020,2021 Thomas E. Dickey                                      --
-- Copyright 1998-2006,2007 Free Software Foundation, Inc.                   --
--                                                                           --
-- Permission is hereby granted, free of charge, to any person obtaining a   --
-- copy of this software and associated documentation files (the             --
-- "Software"), to deal in the Software without restriction, including       --
-- without limitation the rights to use, copy, modify, merge, publish,       --
-- distribute, distribute with modifications, sublicense, and/or sell copies --
-- of the Software, and to permit persons to whom the Software is furnished  --
-- to do so, subject to the following conditions:                            --
--                                                                           --
-- The above copyright notice and this permission notice shall be included   --
-- in all copies or substantial portions of the Software.                    --
--                                                                           --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS   --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF                --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN --
-- NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,       --
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR     --
-- OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE --
-- USE OR OTHER DEALINGS IN THE SOFTWARE.                                    --
--                                                                           --
-- Except as contained in this notice, the name(s) of the above copyright    --
-- holders shall not be used in advertising or otherwise to promote the      --
-- sale, use or other dealings in this Software without prior written        --
-- authorization.                                                            --
-------------------------------------------------------------------------------
-- $Id: README-first,v 1.11 2021/06/17 21:20:30 tom Exp $
-------------------------------------------------------------------------------
                  C++ interface to ncurses routines
-----------------------------------------------------------------------

This directory contains the source code for several C++ classes which
ease the use of writing ncurses-based programs.  The code was originally
derived from the libg++ CursesWindow class, but rewritten for ncurses.

The classes simplify the use of window specific functions by
encapsulating them in the window object.  Function overloading is
used in order to narrow the interface.  For example, you do not have the
distinction between `printw' and `mvprintw' anymore.

A second benefit is the removal of all #defines which are included in
the curses.h file.  This is a steady cause of trouble because many
common identifiers are used.  Instead now all #defines are inline
functions, which also allows strict type checking of arguments.

The next enhancement is color support. It was originally provided by a
derived class.  This caused some trouble if you think about Panels or
Menus and Forms with colors.  We decided to put color support into the
base class so that any derived class may use color support also.
The implementation chosen here is directed to unrestricted use
of mixes of color and monochrome windows.  The original NCursesColorWindow
class is maintained for compatibility reasons.

The last point to mention is the support of other packages that are
distributed with the ncurses package:  the panels library, the menu library
and the form library.  This support is provided by the NCursesPanel class,
which is also derived from the NCursesWindow class and the NCursesMenu
and NCursesForm classes which are derived from NCursesPanel.  This allows
building interfaces with windows.

Please see the example program for a quick introduction.

Note that at this point, there is no documentation for these classes.
Hopefully some will be written in the not too distant future.  For now,
to find out how to use the classes, read the code and the example program.

Suggestions for enhancements and contributions of code (and docs) are
welcome.  Please let us know which functionality you miss.

Original author:
     Eric Newton         <newton@rocky.oswego.edu> for FSF's libg++

Authors of first ncurses based release (NCursesWindow, NCursesPanel):
     Ulrich Drepper      <drepper@ira.uka.de>
 and Anatoly Ivasyuk     <anatoly@nick.csh.rit.edu>

Authors of this release:
     Juergen Pfeifer
     Thomas E. Dickey
