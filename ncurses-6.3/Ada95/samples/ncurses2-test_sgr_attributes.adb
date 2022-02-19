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
with ncurses2.util; use ncurses2.util;

--  Graphic-rendition test (adapted from vttest)

procedure ncurses2.test_sgr_attributes is

   procedure xAdd (l : Line_Position; c : Column_Position; s : String);

   procedure xAdd (l : Line_Position; c : Column_Position; s : String) is
   begin
      Add (Line => l, Column => c, Str => s);
   end xAdd;

   normal, current : Attributed_Character;
begin
   for pass in reverse Boolean loop
      if pass then
         normal := (Ch => ' ', Attr => Normal_Video, Color => 0);
      else
         normal := (Ch => ' ', Attr =>
                      (Reverse_Video => True, others => False), Color => 0);
      end if;

      --  Use non-default colors if possible to exercise bce a little
      if Has_Colors then
         Init_Pair (1, White, Blue);
         normal.Color := 1;
      end if;
      Set_Background (Ch => normal);
      Erase;
      xAdd (1, 20, "Graphic rendition test pattern:");

      xAdd (4, 1, "vanilla");

      current := normal;
      current.Attr.Bold_Character := not current.Attr.Bold_Character;
      Set_Background (Ch => current);
      xAdd (4, 40, "bold");

      current := normal;
      current.Attr.Under_Line := not current.Attr.Under_Line;
      Set_Background (Ch => current);
      xAdd (6, 6, "underline");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Under_Line := not current.Attr.Under_Line;
      Set_Background (Ch => current);
      xAdd (6, 45, "bold underline");

      current := normal;
      current.Attr.Blink := not current.Attr.Blink;
      Set_Background (Ch => current);
      xAdd (8, 1, "blink");

      current := normal;
      current.Attr.Blink  := not current.Attr.Blink;
      current.Attr.Bold_Character := not current.Attr.Bold_Character;
      Set_Background (Ch => current);
      xAdd (8, 40, "bold blink");

      current := normal;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Blink := not current.Attr.Blink;
      Set_Background (Ch => current);
      xAdd (10, 6, "underline blink");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Blink := not current.Attr.Blink;
      Set_Background (Ch => current);
      xAdd (10, 45, "bold underline blink");

      current := normal;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (12, 1, "negative");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (12, 40, "bold negative");

      current := normal;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (14, 6, "underline negative");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (14, 45, "bold underline negative");

      current := normal;
      current.Attr.Blink  := not current.Attr.Blink;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (16, 1, "blink negative");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Blink  := not current.Attr.Blink;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (16, 40, "bold blink negative");

      current := normal;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Blink  := not current.Attr.Blink;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (18, 6, "underline blink negative");

      current := normal;
      current.Attr.Bold_Character  := not current.Attr.Bold_Character;
      current.Attr.Under_Line  := not current.Attr.Under_Line;
      current.Attr.Blink  := not current.Attr.Blink;
      current.Attr.Reverse_Video := not current.Attr.Reverse_Video;
      Set_Background (Ch => current);
      xAdd (18, 45, "bold underline blink negative");

      Set_Background (Ch => normal);
      Move_Cursor (Line => Lines - 2, Column => 1);
      if pass then
         Add (Str => "Dark");
      else
         Add (Str => "Light");
      end if;
      Add (Str => " background. ");
      Clear_To_End_Of_Line;
      Pause;
   end loop;

   Set_Background (Ch => Blank2);
   Erase;
   End_Windows;

end ncurses2.test_sgr_attributes;
