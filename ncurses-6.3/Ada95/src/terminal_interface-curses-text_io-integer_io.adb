------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Text_IO.Integer_IO               --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2003,2009 Free Software Foundation, Inc.                  --
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
--  Author:  Juergen Pfeifer, 1996
--  Version Control:
--  $Revision: 1.12 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Text_IO;
with Terminal_Interface.Curses.Text_IO.Aux;

package body Terminal_Interface.Curses.Text_IO.Integer_IO is

   package Aux renames Terminal_Interface.Curses.Text_IO.Aux;
   package IIO is new Ada.Text_IO.Integer_IO (Num);

   procedure Put
     (Win   : Window;
      Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
      Buf : String (1 .. Field'Last);
   begin
      IIO.Put (Buf, Item, Base);
      Aux.Put_Buf (Win, Buf, Width);
   end Put;

   procedure Put
     (Item  : Num;
      Width : Field := Default_Width;
      Base  : Number_Base := Default_Base)
   is
   begin
      Put (Get_Window, Item, Width, Base);
   end Put;

end Terminal_Interface.Curses.Text_IO.Integer_IO;
