------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                              ncurses2.util                               --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000,2006 Free Software Foundation, Inc.                       --
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
--  $Revision: 1.3 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Text_IO;
package ncurses2.util is

   Blank : constant Character := ' ';
   Blank2 : constant Attributed_Character :=
     (Ch => Blank, Attr => Normal_Video, Color => Color_Pair'First);

   newl : constant Character :=  Character'Val (10);

   function CTRL (c : Character) return Key_Code;

   function CTRL (c : Character) return Character;

   function Getchar (win : Window := Standard_Window) return Key_Code;

   procedure Getchar (win : Window := Standard_Window);

   procedure Pause;

   procedure Cannot (s : String);

   procedure ShellOut (message : Boolean);

   package Int_IO is new Ada.Text_IO.Integer_IO (Integer);

   function Is_Digit (c : Key_Code) return Boolean;

   procedure P (s : String);

   function Code_To_Char (c : Key_Code) return Character;
   function ctoi (c : Character) return Integer;

end ncurses2.util;
