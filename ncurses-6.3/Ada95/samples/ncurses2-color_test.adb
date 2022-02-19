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
-- Copyright 2000-2006,2008 Free Software Foundation, Inc.                  --
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
with ncurses2.util; use ncurses2.util;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Ada.Strings.Fixed;

procedure ncurses2.color_test is
   use Int_IO;

   procedure show_color_name (y, x : Integer; color : Integer);

   color_names : constant array (0 .. 15) of String (1 .. 7) :=
     (
      "black  ",
      "red    ",
      "green  ",
      "yellow ",
      "blue   ",
      "magenta",
      "cyan   ",
      "white  ",
      "BLACK  ",
      "RED    ",
      "GREEN  ",
      "YELLOW ",
      "BLUE   ",
      "MAGENTA",
      "CYAN   ",
      "WHITE  "
      );

   procedure show_color_name (y, x : Integer; color : Integer) is
      tmp5 : String (1 .. 5);
   begin
      if Number_Of_Colors > 8 then

         Put (tmp5, color);
         Add (Line => Line_Position (y), Column => Column_Position (x),
              Str => tmp5);
      else
         Add (Line => Line_Position (y), Column => Column_Position (x),
              Str => color_names (color));
      end if;
   end show_color_name;

   top, width : Integer;
   hello : String (1 .. 5);
   --  tmp3 : String (1 .. 3);
   --  tmp2 : String (1 .. 2);

begin
   Refresh;
   Add (Str => "There are ");
   --  Put(tmp3, Number_Of_Colors*Number_Of_Colors);
   Add (Str => Ada.Strings.Fixed.Trim (Integer'Image (Number_Of_Colors *
                                                      Number_Of_Colors),
                                       Ada.Strings.Left));
   Add (Str => " color pairs");
   Add (Ch => newl);

   if Number_Of_Colors > 8 then
      width := 4;
   else
      width := 8;
   end if;

   if Number_Of_Colors > 8 then
      hello := "Test ";
   else
      hello := "Hello";
   end if;

   for Bright in Boolean loop
      if Number_Of_Colors > 8 then
         top := 0;
      else
         top := Boolean'Pos (Bright) * (Number_Of_Colors + 3);
      end if;
      Clear_To_End_Of_Screen;
      Move_Cursor (Line => Line_Position (top) + 1, Column => 0);
      --  Put(tmp2, Number_Of_Colors);
      Add (Str => Ada.Strings.Fixed.Trim (Integer'Image (Number_Of_Colors),
                                          Ada.Strings.Left));
      Add (Ch => 'x');
      Add (Str => Ada.Strings.Fixed.Trim (Integer'Image (Number_Of_Colors),
                                          Ada.Strings.Left));
      Add (Str => "  matrix of foreground/background colors, bright *");
      if Bright then
         Add (Str => "on");
      else
         Add (Str => "off");
      end if;
      Add (Ch => '*');

      for i in 0 .. Number_Of_Colors - 1 loop
         show_color_name (top + 2, (i + 1) * width, i);
      end loop;
      for i in 0 .. Number_Of_Colors - 1 loop
         show_color_name (top + 3 + i, 0, i);
      end loop;
      for i in 1 .. Number_Of_Color_Pairs - 1 loop
         Init_Pair (Color_Pair (i), Color_Number (i mod Number_Of_Colors),
                    Color_Number (i / Number_Of_Colors));
         --  attron((attr_t) COLOR_PAIR(i)) -- Huh?
         Set_Color (Pair => Color_Pair (i));
         if Bright then
            Switch_Character_Attribute (Attr => (Bold_Character => True,
                                                 others => False));
         end if;
         Add (Line => Line_Position (top + 3 + (i / Number_Of_Colors)),
              Column => Column_Position ((i mod Number_Of_Colors + 1) *
                                         width),
              Str => hello);
         Set_Character_Attributes;
      end loop;
      if Number_Of_Colors > 8 or Bright then
         Pause;
      end if;
   end loop;

   Erase;
   End_Windows;
end ncurses2.color_test;
