------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020,2021 Thomas E. Dickey                                     --
-- Copyright 2000-2014,2015 Free Software Foundation, Inc.                  --
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
--  $Date: 2021/09/04 10:52:55 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

--  test effects of overlapping windows

procedure ncurses2.overlap_test is

   procedure fillwin (win : Window; ch : Character);
   procedure crosswin (win : Window; ch : Character);

   procedure fillwin (win : Window; ch : Character) is
      y1 : Line_Position;
      x1 : Column_Position;
   begin
      Get_Size (win, y1, x1);
      for y in 0 .. y1 - 1 loop
         Move_Cursor (win, y, 0);
         for x in 0 .. x1 - 1 loop
            Add (win, Ch => ch);
         end loop;
      end loop;
   exception
      when Curses_Exception => null;
         --  write to lower right corner
   end fillwin;

   procedure crosswin (win : Window; ch : Character) is
      y1 : Line_Position;
      x1 : Column_Position;
   begin
      Get_Size (win, y1, x1);
      for y in 0 .. y1 - 1 loop
         for x in 0 .. x1 - 1 loop
            if ((x > (x1 - 1) / 3) and (x <= (2 * (x1 - 1)) / 3)) or
               (((y > (y1 - 1) / 3) and (y <= (2 * (y1 - 1)) / 3)))
            then
               Move_Cursor (win, y, x);
               Add (win, Ch => ch);
            end if;
         end loop;
      end loop;
   end crosswin;

   --  In a 24x80 screen like some xterms are, the instructions will
   --  be overwritten.
   ch : Character;
   win1 : Window := New_Window (9, 20, 3, 3);
   win2 : Window := New_Window (9, 20, 9, 16);
begin
   Set_Raw_Mode (SwitchOn => True);
   Refresh;
   Move_Cursor (Line => 0, Column => 0);
   Add (Str => "This test shows the behavior of wnoutrefresh() with " &
        "respect to");
   Add (Ch => newl);
   Add (Str => "the shared region of two overlapping windows A and B. " &
        "The cross");
   Add (Ch => newl);
   Add (Str => "pattern in each window does not overlap the other.");
   Add (Ch => newl);

   Move_Cursor (Line => 18, Column => 0);
   Add (Str => "a = refresh A, then B, then doupdate. b = refresh B, " &
        "then A, then doupdate");
   Add (Ch => newl);
   Add (Str => "c = fill window A with letter A.      d = fill window B " &
        "with letter B.");
   Add (Ch => newl);
   Add (Str => "e = cross pattern in window A.        f = cross pattern " &
        "in window B.");
   Add (Ch => newl);
   Add (Str => "g = clear window A.                   h = clear window B.");
   Add (Ch => newl);
   Add (Str => "i = overwrite A onto B.               j = overwrite " &
        "B onto A.");
   Add (Ch => newl);
   Add (Str => "^Q/ESC = terminate test.");

   loop
      ch := Code_To_Char (Getchar);
      exit when ch = CTRL ('Q') or ch = CTRL ('['); --  QUIT or ESCAPE
      case ch is
         when 'a' => --  refresh window A first, then B
            Refresh_Without_Update (win1);
            Refresh_Without_Update (win2);
            Update_Screen;
         when 'b' => --  refresh window B first, then A
            Refresh_Without_Update (win2);
            Refresh_Without_Update (win1);
            Update_Screen;
         when 'c' => --  fill window A so it is visible
            fillwin (win1, 'A');
         when 'd' => --  fill window B so it is visible
            fillwin (win2, 'B');
         when 'e' => --  cross test pattern in window A
            crosswin (win1, 'A');
         when 'f' => --  cross test pattern in window B
            crosswin (win2, 'B');
         when 'g' => --  clear window A
            Clear (win1);
            Move_Cursor (win1, 0, 0);
         when 'h' => --  clear window B
            Clear (win2);
            Move_Cursor (win2, 0, 0);
         when 'i' => --  overwrite A onto B
            Overwrite (win1, win2);
         when 'j' => --  overwrite B onto A
            Overwrite (win2, win1);
         when others => null;
      end case;
   end loop;

   Delete (win2);
   Delete (win1);
   Erase;
   End_Windows;
end ncurses2.overlap_test;
