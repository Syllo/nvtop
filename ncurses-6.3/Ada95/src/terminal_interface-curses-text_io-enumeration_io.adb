------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--             Terminal_Interface.Curses.Text_IO.Enumeration_IO             --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Terminal_Interface.Curses.Text_IO.Aux;

package body Terminal_Interface.Curses.Text_IO.Enumeration_IO is

   package Aux renames Terminal_Interface.Curses.Text_IO.Aux;
   package EIO is new Ada.Text_IO.Enumeration_IO (Enum);

   procedure Put
     (Win   : Window;
      Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting)
   is
      Buf  : String (1 .. Field'Last);
      Tset : Ada.Text_IO.Type_Set;
   begin
      if Set /= Mixed_Case then
         Tset := Ada.Text_IO.Type_Set'Val (Type_Set'Pos (Set));
      else
         Tset := Ada.Text_IO.Lower_Case;
      end if;
      EIO.Put (Buf, Item, Tset);
      if Set = Mixed_Case then
         Buf (Buf'First) := To_Upper (Buf (Buf'First));
      end if;
      Aux.Put_Buf (Win, Buf, Width, True, True);
   end Put;

   procedure Put
     (Item  : Enum;
      Width : Field := Default_Width;
      Set   : Type_Set := Default_Setting)
   is
   begin
      Put (Get_Window, Item, Width, Set);
   end Put;

end Terminal_Interface.Curses.Text_IO.Enumeration_IO;
