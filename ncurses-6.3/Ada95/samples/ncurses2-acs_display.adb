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
--  $Revision: 1.7 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with ncurses2.genericPuts;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Strings.Unbounded;
with Ada.Strings.Fixed;

procedure ncurses2.acs_display is
   use Int_IO;

   procedure show_upper_chars (first : Integer);
   function  show_1_acs (N    : Integer;
                         name : String;
                         code :  Attributed_Character)
                        return Integer;
   procedure show_acs_chars;

   procedure show_upper_chars (first : Integer)  is
      C1 : constant Boolean := (first = 128);
      last : constant Integer := first + 31;
      package p is new ncurses2.genericPuts (200);
      use p;
      use p.BS;
      use Ada.Strings.Unbounded;

      tmpa : Unbounded_String;
      tmpb : BS.Bounded_String;
   begin
      Erase;
      Switch_Character_Attribute
        (Attr => (Bold_Character => True, others => False));
      Move_Cursor (Line => 0, Column => 20);
      tmpa := To_Unbounded_String ("Display of ");
      if C1 then
         tmpa := tmpa & "C1";
      else
         tmpa := tmpa & "GR";
      end if;
      tmpa := tmpa & " Character Codes ";
      myPut (tmpb, first);
      Append (tmpa, To_String (tmpb));
      Append (tmpa, " to ");
      myPut (tmpb, last);
      Append (tmpa, To_String (tmpb));
      Add (Str => To_String (tmpa));
      Switch_Character_Attribute
        (On => False,
         Attr => (Bold_Character => True, others => False));
      Refresh;

      for code in first .. last loop
         declare
            row : constant Line_Position
                := Line_Position (4 + ((code - first) mod 16));
            col : constant Column_Position
                := Column_Position (((code - first) / 16) *
                                    Integer (Columns) / 2);
            tmp3 : String (1 .. 3);
            tmpx : String (1 .. Integer (Columns / 4));
            reply : Key_Code;
         begin
            Put (tmp3, code);
            myPut (tmpb, code, 16);
            tmpa := To_Unbounded_String (tmp3 & " (" & To_String (tmpb) & ')');

            Ada.Strings.Fixed.Move (To_String (tmpa), tmpx,
                                    Justify => Ada.Strings.Right);
            Add (Line => row, Column => col,
                 Str => tmpx & ' ' & ':' & ' ');
            if C1 then
               Set_NoDelay_Mode (Mode => True);
            end if;
            Add_With_Immediate_Echo (Ch => Code_To_Char (Key_Code (code)));
            --  TODO check this
            if C1 then
               reply := Getchar;
               while reply /= Key_None loop
                  Add (Ch => Code_To_Char (reply));
                  Nap_Milli_Seconds (10);
                  reply := Getchar;
               end loop;
               Set_NoDelay_Mode (Mode => False);
            end if;
         end;
      end loop;
   end show_upper_chars;

   function show_1_acs (N    : Integer;
                        name : String;
                        code :  Attributed_Character)
                       return Integer is
      height : constant Integer := 16;
      row : constant Line_Position := Line_Position (4 + (N mod height));
      col : constant Column_Position := Column_Position ((N / height) *
                                                Integer (Columns) / 2);
      tmpx : String (1 .. Integer (Columns) / 3);
   begin
      Ada.Strings.Fixed.Move (name, tmpx,
                              Justify => Ada.Strings.Right,
                              Drop => Ada.Strings.Left);
      Add (Line => row, Column => col, Str => tmpx & ' ' & ':' & ' ');
      --  we need more room than C because our identifiers are longer
      --  22 chars actually
      Add (Ch => code);
      return N + 1;
   end show_1_acs;

   procedure show_acs_chars is
      n : Integer;
   begin
      Erase;
      Switch_Character_Attribute
        (Attr => (Bold_Character => True, others => False));
      Add (Line => 0, Column => 20,
           Str => "Display of the ACS Character Set");
      Switch_Character_Attribute (On => False,
                                  Attr => (Bold_Character => True,
                                           others => False));
      Refresh;

      --  the following is useful to generate the below
      --  grep '^[ ]*ACS_' ../src/terminal_interface-curses.ads |
      --  awk '{print  "n := show_1_acs(n, \""$1"\", ACS_Map("$1"));"}'

      n := show_1_acs (0, "ACS_Upper_Left_Corner",
                       ACS_Map (ACS_Upper_Left_Corner));
      n := show_1_acs (n, "ACS_Lower_Left_Corner",
                       ACS_Map (ACS_Lower_Left_Corner));
      n := show_1_acs (n, "ACS_Upper_Right_Corner",
                       ACS_Map (ACS_Upper_Right_Corner));
      n := show_1_acs (n, "ACS_Lower_Right_Corner",
                       ACS_Map (ACS_Lower_Right_Corner));
      n := show_1_acs (n, "ACS_Left_Tee", ACS_Map (ACS_Left_Tee));
      n := show_1_acs (n, "ACS_Right_Tee", ACS_Map (ACS_Right_Tee));
      n := show_1_acs (n, "ACS_Bottom_Tee", ACS_Map (ACS_Bottom_Tee));
      n := show_1_acs (n, "ACS_Top_Tee", ACS_Map (ACS_Top_Tee));
      n := show_1_acs (n, "ACS_Horizontal_Line",
                       ACS_Map (ACS_Horizontal_Line));
      n := show_1_acs (n, "ACS_Vertical_Line", ACS_Map (ACS_Vertical_Line));
      n := show_1_acs (n, "ACS_Plus_Symbol", ACS_Map (ACS_Plus_Symbol));
      n := show_1_acs (n, "ACS_Scan_Line_1", ACS_Map (ACS_Scan_Line_1));
      n := show_1_acs (n, "ACS_Scan_Line_9", ACS_Map (ACS_Scan_Line_9));
      n := show_1_acs (n, "ACS_Diamond", ACS_Map (ACS_Diamond));
      n := show_1_acs (n, "ACS_Checker_Board", ACS_Map (ACS_Checker_Board));
      n := show_1_acs (n, "ACS_Degree", ACS_Map (ACS_Degree));
      n := show_1_acs (n, "ACS_Plus_Minus", ACS_Map (ACS_Plus_Minus));
      n := show_1_acs (n, "ACS_Bullet", ACS_Map (ACS_Bullet));
      n := show_1_acs (n, "ACS_Left_Arrow", ACS_Map (ACS_Left_Arrow));
      n := show_1_acs (n, "ACS_Right_Arrow", ACS_Map (ACS_Right_Arrow));
      n := show_1_acs (n, "ACS_Down_Arrow", ACS_Map (ACS_Down_Arrow));
      n := show_1_acs (n, "ACS_Up_Arrow", ACS_Map (ACS_Up_Arrow));
      n := show_1_acs (n, "ACS_Board_Of_Squares",
                       ACS_Map (ACS_Board_Of_Squares));
      n := show_1_acs (n, "ACS_Lantern", ACS_Map (ACS_Lantern));
      n := show_1_acs (n, "ACS_Solid_Block", ACS_Map (ACS_Solid_Block));
      n := show_1_acs (n, "ACS_Scan_Line_3", ACS_Map (ACS_Scan_Line_3));
      n := show_1_acs (n, "ACS_Scan_Line_7", ACS_Map (ACS_Scan_Line_7));
      n := show_1_acs (n, "ACS_Less_Or_Equal", ACS_Map (ACS_Less_Or_Equal));
      n := show_1_acs (n, "ACS_Greater_Or_Equal",
                       ACS_Map (ACS_Greater_Or_Equal));
      n := show_1_acs (n, "ACS_PI", ACS_Map (ACS_PI));
      n := show_1_acs (n, "ACS_Not_Equal", ACS_Map (ACS_Not_Equal));
      n := show_1_acs (n, "ACS_Sterling", ACS_Map (ACS_Sterling));

      if n = 0 then
         raise Constraint_Error;
      end if;
   end show_acs_chars;

   c1 : Key_Code;
   c : Character := 'a';
begin
   loop
      case c is
         when 'a' =>
            show_acs_chars;
         when '0' | '1' | '2' | '3' =>
            show_upper_chars (ctoi (c) * 32 + 128);
         when others =>
            null;
      end case;
      Add (Line => Lines - 3, Column => 0,
           Str => "Note: ANSI terminals may not display C1 characters.");
      Add (Line => Lines - 2, Column => 0,
           Str => "Select: a=ACS, 0=C1, 1,2,3=GR characters, q=quit");
      Refresh;
      c1 := Getchar;
      c := Code_To_Char (c1);
      exit when c = 'q' or c = 'x';
   end loop;
   Pause;
   Erase;
   End_Windows;
end ncurses2.acs_display;
