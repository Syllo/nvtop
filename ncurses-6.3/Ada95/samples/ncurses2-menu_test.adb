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
-- Copyright 2000-2006,2011 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.9 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Terminal_Interface.Curses.Mouse; use Terminal_Interface.Curses.Mouse;

procedure ncurses2.menu_test is
   function menu_virtualize (c : Key_Code) return Key_Code;
   procedure xAdd (l : Line_Position; c : Column_Position; s : String);

   function menu_virtualize (c : Key_Code) return Key_Code is
   begin
      case c is
         when Character'Pos (newl) | Key_Exit =>
            return Menu_Request_Code'Last + 1; --  MAX_COMMAND? TODO
         when  Character'Pos ('u')  =>
            return M_ScrollUp_Line;
         when  Character'Pos ('d') =>
            return M_ScrollDown_Line;
         when  Character'Pos ('b') |  Key_Next_Page =>
            return M_ScrollUp_Page;
         when  Character'Pos ('f') |  Key_Previous_Page =>
            return M_ScrollDown_Page;
         when  Character'Pos ('n') |  Key_Cursor_Down =>
            return M_Next_Item;
         when  Character'Pos ('p') |  Key_Cursor_Up =>
            return M_Previous_Item;
         when  Character'Pos (' ') =>
            return M_Toggle_Item;
         when  Key_Mouse =>
            return c;
         when others =>
            Beep;
            return c;
      end case;
   end menu_virtualize;

   MENU_Y : constant Line_Count := 8;
   MENU_X : constant Column_Count := 8;

   type String_Access is access String;

   animals : constant array (Positive range <>) of String_Access :=
     (new String'("Lions"),
      new String'("Tigers"),
      new String'("Bears"),
      new String'("(Oh my!)"),
      new String'("Newts"),
      new String'("Platypi"),
      new String'("Lemurs"));

   items_a : constant Item_Array_Access :=
      new Item_Array (1 .. animals'Last + 1);

   tmp : Event_Mask;

   procedure xAdd (l : Line_Position; c : Column_Position; s : String) is
   begin
      Add (Line => l, Column => c, Str => s);
   end xAdd;

   mrows : Line_Count;
   mcols : Column_Count;

   menuwin : Window;

   m : Menu;

   c1 : Key_Code;

   c : Driver_Result;
   r : Key_Code;
begin
   tmp := Start_Mouse;
   xAdd (0, 0, "This is the menu test:");
   xAdd (2, 0, "  Use up and down arrow to move the select bar.");
   xAdd (3, 0, "  'n' and 'p' act like arrows.");
   xAdd (4, 0, "  'b' and 'f' scroll up/down (page), 'u' and 'd' (line).");
   xAdd (5, 0, "  Press return to exit.");
   Refresh;

   for i in animals'Range loop
      items_a.all (i) := New_Item (animals (i).all);
   end loop;
   items_a.all (animals'Last + 1) := Null_Item;

   m := New_Menu (items_a);

   Set_Format (m, Line_Position (animals'Last + 1) / 2, 1);
   Scale (m, mrows, mcols);

   menuwin := Create (mrows + 2, mcols + 2, MENU_Y, MENU_X);
   Set_Window (m, menuwin);
   Set_KeyPad_Mode (menuwin, True);
   Box (menuwin); -- 0,0?

   Set_Sub_Window (m, Derived_Window (menuwin, mrows, mcols, 1, 1));

   Post (m);

   loop
      c1 := Getchar (menuwin);
      r := menu_virtualize (c1);
      c := Driver (m, r);
      exit when c = Unknown_Request; -- E_UNKNOWN_COMMAND?
      if c = Request_Denied then
         Beep;
      end if;
      --  continue ?
   end loop;

   Move_Cursor (Line => Lines - 2, Column => 0);
   Add (Str => "You chose: ");
   Add (Str => Name (Current (m)));
   Add (Ch => newl);
   Pause; -- the C version didn't use Pause, it spelled it out

   Post (m, False); --  unpost, not clear :-(
   declare begin
      Delete (menuwin);
   exception when Curses_Exception => null; end;
   --  menuwin has children so will raise the exception.

   Delete (m);

   End_Mouse (tmp);
end ncurses2.menu_test;
