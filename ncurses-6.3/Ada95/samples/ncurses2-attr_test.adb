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
-- Copyright 2000-2007,2008 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.10 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Terminfo;
use Terminal_Interface.Curses.Terminfo;
with Ada.Characters.Handling;
with Ada.Strings.Fixed;

procedure ncurses2.attr_test is

   function  subset (super, sub : Character_Attribute_Set) return Boolean;
   function  intersect (b, a : Character_Attribute_Set) return Boolean;
   function  has_A_COLOR (attr : Attributed_Character) return Boolean;
   function  show_attr (row  : Line_Position;
                        skip : Natural;
                        attr : Character_Attribute_Set;
                        name : String;
                        once : Boolean) return Line_Position;
   procedure attr_getc (skip : in out Integer;
                        fg, bg : in out Color_Number;
                        result : out Boolean);

   function subset (super, sub : Character_Attribute_Set) return Boolean is
   begin
      if
        (super.Stand_Out or not sub.Stand_Out) and
        (super.Under_Line or not sub.Under_Line) and
        (super.Reverse_Video or not sub.Reverse_Video) and
        (super.Blink or not sub.Blink) and
        (super.Dim_Character or not sub.Dim_Character) and
        (super.Bold_Character or not sub.Bold_Character) and
        (super.Alternate_Character_Set or not sub.Alternate_Character_Set) and
        (super.Invisible_Character or not sub.Invisible_Character) -- and
--      (super.Protected_Character or not sub.Protected_Character) and
--      (super.Horizontal or not sub.Horizontal) and
--      (super.Left or not sub.Left) and
--      (super.Low or not sub.Low) and
--      (super.Right or not sub.Right) and
--      (super.Top or not sub.Top) and
--      (super.Vertical or not sub.Vertical)
      then
         return True;
      else
         return False;
      end if;
   end subset;

   function intersect (b, a : Character_Attribute_Set) return Boolean is
   begin
      if
        (a.Stand_Out and b.Stand_Out) or
        (a.Under_Line and b.Under_Line) or
        (a.Reverse_Video and b.Reverse_Video) or
        (a.Blink and b.Blink) or
        (a.Dim_Character and b.Dim_Character) or
        (a.Bold_Character and b.Bold_Character) or
        (a.Alternate_Character_Set and b.Alternate_Character_Set) or
        (a.Invisible_Character and b.Invisible_Character) -- or
--      (a.Protected_Character and b.Protected_Character) or
--      (a.Horizontal and b.Horizontal) or
--      (a.Left and b.Left) or
--      (a.Low and b.Low) or
--      (a.Right and b.Right) or
--      (a.Top and b.Top) or
--      (a.Vertical and b.Vertical)
      then
         return True;
      else
         return False;
      end if;
   end intersect;

   function has_A_COLOR (attr : Attributed_Character) return Boolean is
   begin
      if attr.Color /= Color_Pair (0) then
         return True;
      else
         return False;
      end if;
   end has_A_COLOR;

   --  Print some text with attributes.
   function show_attr (row  : Line_Position;
                       skip : Natural;
                       attr : Character_Attribute_Set;
                       name : String;
                       once : Boolean) return Line_Position is

      function make_record (n : Integer) return Character_Attribute_Set;
      function make_record (n : Integer) return Character_Attribute_Set is
         --  unsupported means true
         a : Character_Attribute_Set := (others => False);
         m : Integer;
         rest : Integer;
      begin
         --  ncv is a bitmap with these fields
         --              A_STANDOUT,
         --              A_UNDERLINE,
         --              A_REVERSE,
         --              A_BLINK,
         --              A_DIM,
         --              A_BOLD,
         --              A_INVIS,
         --              A_PROTECT,
         --              A_ALTCHARSET
         --  It means no_color_video,
         --  video attributes that can't be used with colors
         --  see man terminfo.5
         m := n mod 2;
         rest := n / 2;
         if 1 = m then
            a.Stand_Out := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Under_Line := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Reverse_Video := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Blink := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Bold_Character := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Invisible_Character := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Protected_Character := True;
         end if;
         m := rest mod 2;
         rest := rest / 2;
         if 1 = m then
            a.Alternate_Character_Set := True;
         end if;

         return a;
      end make_record;

      ncv : constant Integer := Get_Number ("ncv");

   begin
      Move_Cursor (Line => row, Column => 8);
      Add (Str => name & " mode:");
      Move_Cursor (Line => row, Column => 24);
      Add (Ch => '|');
      if skip /= 0 then
         --  printw("%*s", skip, " ")
         Add (Str => Ada.Strings.Fixed."*" (skip, ' '));
      end if;
      if once then
         Switch_Character_Attribute (Attr => attr);
      else
         Set_Character_Attributes (Attr => attr);
      end if;
      Add (Str => "abcde fghij klmno pqrst uvwxy z");
      if once then
         Switch_Character_Attribute (Attr => attr, On => False);
      end if;
      if skip /= 0 then
         Add (Str => Ada.Strings.Fixed."*" (skip, ' '));
      end if;
      Add (Ch => '|');
      if attr /= Normal_Video then
         declare begin
            if not subset (super => Supported_Attributes, sub => attr) then
               Add (Str => " (N/A)");
            elsif ncv > 0 and has_A_COLOR (Get_Background) then
               declare
                  Color_Supported_Attributes :
                    constant Character_Attribute_Set := make_record (ncv);
               begin
                  if intersect (Color_Supported_Attributes, attr) then
                     Add (Str => " (NCV) ");
                  end if;
               end;
            end if;
         end;
      end if;
      return row + 2;
   end show_attr;

   procedure attr_getc (skip : in out Integer;
                        fg, bg : in out Color_Number;
                        result : out Boolean) is
      ch : constant Key_Code := Getchar;
      nc : constant Color_Number := Color_Number (Number_Of_Colors);
   begin
      result := True;
      if Ada.Characters.Handling.Is_Digit (Character'Val (ch)) then
         skip := ctoi (Code_To_Char (ch));
      elsif ch = CTRL ('L') then
         Touch;
         Touch (Current_Window);
         Refresh;
      elsif Has_Colors then
         case ch is
            --  Note the mathematical elegance compared to the C version.
            when Character'Pos ('f') => fg := (fg + 1) mod nc;
            when Character'Pos ('F') => fg := (fg - 1) mod nc;
            when Character'Pos ('b') => bg := (bg + 1) mod nc;
            when Character'Pos ('B') => bg := (bg - 1) mod nc;
            when others =>
               result := False;
         end case;
      else
         result := False;
      end if;
   end attr_getc;

   --      pairs could be defined as array ( Color_Number(0) .. colors - 1) of
   --      array (Color_Number(0).. colors - 1) of Boolean;
   pairs : array (Color_Pair'Range) of Boolean := (others => False);
   fg, bg : Color_Number := Black; -- = 0;
   xmc : constant Integer := Get_Number ("xmc");
   skip : Integer := xmc;
   n : Integer;

   use Int_IO;

begin
   pairs (0) := True;

   if skip < 0 then
      skip := 0;
   end if;
   n := skip;

   loop
      declare
         row : Line_Position := 2;
         normal : Attributed_Character := Blank2;
         --  ???
      begin
         --  row := 2; -- weird, row is set to 0 without this.
         --  TODO delete the above line, it was a gdb quirk that confused me
         if Has_Colors then
            declare pair : constant Color_Pair :=
              Color_Pair (fg * Color_Number (Number_Of_Colors) + bg);
            begin
               --  Go though each color pair. Assume that the number of
               --  Redefinable_Color_Pairs is 8*8 with predefined Colors 0..7
               if not pairs (pair) then
                  Init_Pair (pair, fg, bg);
                  pairs (pair) := True;
               end if;
               normal.Color := pair;
            end;
         end if;
         Set_Background (Ch => normal);
         Erase;

         Add (Line => 0, Column => 20,
              Str => "Character attribute test display");

         row := show_attr (row, n, (Stand_Out => True, others => False),
                           "STANDOUT", True);
         row := show_attr (row, n, (Reverse_Video => True, others => False),
                           "REVERSE", True);
         row := show_attr (row, n, (Bold_Character => True, others => False),
                           "BOLD", True);
         row := show_attr (row, n, (Under_Line => True, others => False),
                           "UNDERLINE", True);
         row := show_attr (row, n, (Dim_Character => True, others => False),
                           "DIM", True);
         row := show_attr (row, n, (Blink => True, others => False),
                           "BLINK", True);
--       row := show_attr (row, n, (Protected_Character => True,
--                                  others => False), "PROTECT", True);
         row := show_attr (row, n, (Invisible_Character => True,
                                    others => False), "INVISIBLE", True);
         row := show_attr (row, n, Normal_Video, "NORMAL", False);

         Move_Cursor (Line => row, Column => 8);
         if xmc > -1 then
            Add (Str => "This terminal does have the magic-cookie glitch");
         else
            Add (Str => "This terminal does not have the magic-cookie glitch");
         end if;
         Move_Cursor (Line => row + 1, Column => 8);
         Add (Str => "Enter a digit to set gaps on each side of " &
              "displayed attributes");
         Move_Cursor (Line => row + 2, Column => 8);
         Add (Str => "^L = repaint");
         if Has_Colors then
            declare tmp1 : String (1 .. 1);
            begin
               Add (Str => ".  f/F/b/F toggle colors (");
               Put (tmp1, Integer (fg));
               Add (Str => tmp1);
               Add (Ch => '/');
               Put (tmp1, Integer (bg));
               Add (Str => tmp1);
               Add (Ch => ')');
            end;
         end if;
         Refresh;
      end;

      declare result : Boolean; begin
         attr_getc (n, fg, bg, result);
         exit when not result;
      end;
   end loop;

   Set_Background (Ch => Blank2);
   Erase;
   End_Windows;
end ncurses2.attr_test;
