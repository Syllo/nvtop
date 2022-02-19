------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2006,2009 Free Software Foundation, Inc.                  --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining a  --
-- copy of this software and associated documentation files (the            --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, distribute with modifications, sublicense, and/or sell       --
-- copies of the Software, and to permit persons to whom the Software is    --
-- furnished to do so, subject to the following conditions:                 --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS  --
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF               --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE ABOVE COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,   --
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR    --
-- OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR    --
-- THE USE OR OTHER DEALINGS IN THE SOFTWARE.                               --
--                                                                          --
-- Except as contained in this notice, the name(s) of the above copyright   --
-- holders shall not be used in advertising or otherwise to promote the     --
-- sale, use or other dealings in this Software without prior written       --
-- authorization.                                                           --
------------------------------------------------------------------------------
--  Author: Eugene V. Melaragno <aldomel@ix.netcom.com> 2000
--  Version Control
--  $Revision: 1.4 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings.Bounded;
use Ada.Strings.Bounded;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Terminal_Interface.Curses;

generic
   Max : Natural;
   --   type mystring is private;
   --   type myint is
package ncurses2.genericPuts is
   package BS is new
     Ada.Strings.Bounded.Generic_Bounded_Length (Max);
   use BS;

   procedure myGet (Win : Terminal_Interface.Curses.Window
                       := Terminal_Interface.Curses.Standard_Window;
                    Str : out BS.Bounded_String;
                    Len :  Integer := -1);

   procedure myPut (Str  : out BS.Bounded_String;
                    i    : Integer;
                    Base : Number_Base := 10);
   --  the default should be Ada.Text_IO.Integer_IO.Default_Base
   --  but Default_Base is hidden in the generic so doesn't exist!
   procedure myAdd (Str : BS.Bounded_String);

   procedure Fill_String (Cp : chars_ptr; Str : out BS.Bounded_String);
end ncurses2.genericPuts;
