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
--  $Revision: 1.7 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with ncurses2.genericPuts;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

procedure ncurses2.color_edit is
   use Int_IO;

   type RGB_Enum is (Redx, Greenx, Bluex);

   procedure change_color (current : Color_Number;
                           field   : RGB_Enum;
                           value   : RGB_Value;
                           usebase : Boolean);

   procedure change_color (current : Color_Number;
                           field   : RGB_Enum;
                           value   : RGB_Value;
                           usebase : Boolean)  is
      red, green, blue : RGB_Value;
   begin
      if usebase then
         Color_Content (current, red, green, blue);
      else
         red := 0;
         green := 0;
         blue := 0;
      end if;

      case field is
         when Redx => red :=  red + value;
         when Greenx => green := green + value;
         when Bluex => blue := blue + value;
      end case;

      declare
      begin
         Init_Color (current, red, green, blue);
      exception
         when Curses_Exception => Beep;
      end;

   end change_color;

   package x is new ncurses2.genericPuts (100); use x;

   tmpb : x.BS.Bounded_String;

   tmp4 : String (1 .. 4);
   tmp6 : String (1 .. 6);
   tmp8 : String (1 .. 8);
   --  This would be easier if Ada had a Bounded_String
   --  defined as a class instead of the inferior generic package,
   --  then I could define Put, Add, and Get for them. Blech.
   value : RGB_Value := 0;
   red, green, blue : RGB_Value;
   max_colors : constant Natural := Number_Of_Colors;
   current : Color_Number := 0;
   field : RGB_Enum := Redx;
   this_c : Key_Code := 0;
begin
   Refresh;

   for i in Color_Number'(0) .. Color_Number (Number_Of_Colors) loop
      Init_Pair (Color_Pair (i), White, i);
   end loop;

   Move_Cursor (Line => Lines - 2, Column => 0);
   Add (Str => "Number: ");
   myPut (tmpb, Integer (value));
   myAdd (Str => tmpb);

   loop

      Switch_Character_Attribute (On => False,
                                  Attr => (Bold_Character => True,
                                           others => False));
      Add (Line => 0, Column => 20, Str => "Color RGB Value Editing");

      Switch_Character_Attribute (On => False,
                                  Attr => (Bold_Character => True,
                                           others => False));

      for i in Color_Number'(0) .. Color_Number (Number_Of_Colors) loop
         Move_Cursor (Line => 2 + Line_Position (i), Column => 0);
         if current = i then
            Add (Ch => '>');
         else
            Add (Ch => ' ');
         end if;
         --  TODO if i <= color_names'Max  then
         Put (tmp8, Integer (i));
         Set_Character_Attributes (Color => Color_Pair (i));
         Add (Str => "        ");
         Set_Character_Attributes;

         Refresh;

         Color_Content (i, red, green, blue);
         Add (Str => "   R = ");
         if current = i and field = Redx then
            Switch_Character_Attribute (On => True,
                                        Attr => (Stand_Out => True,
                                                 others => False));
         end if;
         Put (tmp4, Integer (red));
         Add (Str => tmp4);
         if current = i and field = Redx then
            Set_Character_Attributes;
         end if;
         Add (Str => "   G = ");
         if current = i and field =  Greenx then
            Switch_Character_Attribute (On => True,
                                        Attr => (Stand_Out => True,
                                                 others => False));
         end if;
         Put (tmp4, Integer (green));
         Add (Str => tmp4);
         if current = i and field = Greenx then
            Set_Character_Attributes;
         end if;
         Add (Str => "   B = ");
         if current = i and field = Bluex then
            Switch_Character_Attribute (On => True,
                                        Attr => (Stand_Out => True,
                                                 others => False));
         end if;
         Put (tmp4, Integer (blue));
         Add (Str => tmp4);
         if current = i and field = Bluex then
            Set_Character_Attributes;
         end if;
         Set_Character_Attributes;
         Add (Ch => ')');
      end loop;
      Add (Line => Line_Position (Number_Of_Colors + 3), Column => 0,
           Str => "Use up/down to select a color, left/right to change " &
           "fields.");
      Add (Line => Line_Position (Number_Of_Colors + 4), Column => 0,
           Str => "Modify field by typing nnn=, nnn-, or nnn+.  ? for help.");

      Move_Cursor (Line => 2 + Line_Position (current), Column => 0);

      this_c := Getchar;
      if Is_Digit (this_c) then
         value := 0;
      end if;

      case this_c is
         when KEY_UP =>
            current := (current - 1) mod Color_Number (max_colors);
         when KEY_DOWN =>
            current := (current + 1) mod Color_Number (max_colors);
         when KEY_RIGHT =>
            field := RGB_Enum'Val ((RGB_Enum'Pos (field) + 1) mod 3);
         when KEY_LEFT =>
            field := RGB_Enum'Val ((RGB_Enum'Pos (field) - 1) mod 3);
         when
           Character'Pos ('0') |
           Character'Pos ('1') |
           Character'Pos ('2') |
           Character'Pos ('3') |
           Character'Pos ('4') |
           Character'Pos ('5') |
           Character'Pos ('6') |
           Character'Pos ('7') |
           Character'Pos ('8') |
           Character'Pos ('9')  =>
            value := value * 10 + RGB_Value (ctoi (Code_To_Char (this_c)));

         when Character'Pos ('+') =>
            change_color (current, field,  value, True);

         when Character'Pos ('-') =>
            change_color (current, field, -value, True);

         when Character'Pos ('=') =>
            change_color (current, field,  value, False);

         when Character'Pos ('?') =>
            Erase;
            P ("                      RGB Value Editing Help");
            P ("");
            P ("You are in the RGB value editor.  Use the arrow keys to " &
               "select one of");
            P ("the fields in one of the RGB triples of the current colors;" &
               " the one");
            P ("currently selected will be reverse-video highlighted.");
            P ("");
            P ("To change a field, enter the digits of the new value; they" &
               " are echoed");
            P ("as entered.  Finish by typing `='.  The change will take" &
               " effect instantly.");
            P ("To increment or decrement a value, use the same procedure," &
               " but finish");
            P ("with a `+' or `-'.");
            P ("");
            P ("To quit, do `x' or 'q'");

            Pause;
            Erase;
         when Character'Pos ('q') |
           Character'Pos ('x') =>
            null;
         when others =>
            Beep;
      end case;
      Move_Cursor (Line => Lines - 2, Column => 0);
      Put (tmp6, Integer (value));
      Add (Str => "Number: " & tmp6);

      Clear_To_End_Of_Line;
      exit when this_c = Character'Pos ('x') or
        this_c = Character'Pos ('q');
   end loop;

   Erase;
   End_Windows;
end ncurses2.color_edit;
