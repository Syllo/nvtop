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
-- Copyright 2000-2009,2014 Free Software Foundation, Inc.                  --
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
--  Character input test
--  test the keypad feature

with ncurses2.util; use ncurses2.util;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Mouse; use Terminal_Interface.Curses.Mouse;
with Ada.Characters.Handling;
with Ada.Strings.Bounded;

with ncurses2.genericPuts;

procedure ncurses2.getch_test is
   use Int_IO;

   function mouse_decode (ep : Mouse_Event) return String;

   function mouse_decode (ep : Mouse_Event) return String is
      Y      : Line_Position;
      X      : Column_Position;
      Button : Mouse_Button;
      State  : Button_State;
      package BS is new Ada.Strings.Bounded.Generic_Bounded_Length (200);
      use BS;
      buf : Bounded_String := To_Bounded_String ("");
   begin
      --  Note that these bindings do not allow
      --  two button states,
      --  The C version can print {click-1, click-3} for example.
      --  They also don't have the 'id' or z coordinate.
      Get_Event (ep, Y, X, Button, State);

      --  TODO Append (buf, "id "); from C version
      Append (buf, "at (");
      Append (buf, Column_Position'Image (X));
      Append (buf, ", ");
      Append (buf, Line_Position'Image (Y));
      Append (buf, ") state");
      Append (buf, Mouse_Button'Image (Button));

      Append (buf, " = ");
      Append (buf, Button_State'Image (State));
      return To_String (buf);
   end mouse_decode;

   buf : String (1 .. 1024); --  TODO was BUFSIZE
   n : Integer;
   c : Key_Code;
   blockflag : Timeout_Mode := Blocking;
   firsttime : Boolean := True;
   tmp2  : Event_Mask;
   tmp6 : String (1 .. 6);
   tmp20 : String (1 .. 20);
   x : Column_Position;
   y : Line_Position;
   tmpx : Integer;
   incount : Integer := 0;

begin
   Refresh;
   tmp2 := Start_Mouse (All_Events);
   Add (Str => "Delay in 10ths of a second (<CR> for blocking input)? ");
   Set_Echo_Mode (SwitchOn => True);
   Get (Str => buf);

   Set_Echo_Mode (SwitchOn => False);
   Set_NL_Mode (SwitchOn => False);

   if Ada.Characters.Handling.Is_Digit (buf (1)) then
      Get (Item => n, From => buf, Last => tmpx);
      Set_Timeout_Mode (Mode => Delayed, Amount => n * 100);
      blockflag := Delayed;
   end if;

   c := Character'Pos ('?');
   Set_Raw_Mode (SwitchOn => True);
   loop
      if not firsttime then
         Add (Str => "Key pressed: ");
         Put (tmp6, Integer (c), 8);
         Add (Str => tmp6);
         Add (Ch => ' ');
         if c = Key_Mouse then
            declare
               event : Mouse_Event;
            begin
               event := Get_Mouse;
               Add (Str => "KEY_MOUSE, ");
               Add (Str => mouse_decode (event));
               Add (Ch => newl);
            end;
         elsif c >= Key_Min then
            Key_Name (c, tmp20);
            Add (Str => tmp20);
            --  I used tmp and got bitten by the length problem:->
            Add (Ch => newl);
         elsif c > 16#80# then --  TODO fix, use constant if possible
            declare
               c2 : constant Character := Character'Val (c mod 16#80#);
            begin
               if Ada.Characters.Handling.Is_Graphic (c2) then
                  Add (Str => "M-");
                  Add (Ch => c2);
               else
                  Add (Str => "M-");
                  Add (Str => Un_Control ((Ch => c2,
                                           Color => Color_Pair'First,
                                           Attr => Normal_Video)));
               end if;
               Add (Str => " (high-half character)");
               Add (Ch => newl);
            end;
         else
            declare
               c2 : constant Character := Character'Val (c mod 16#80#);
            begin
               if Ada.Characters.Handling.Is_Graphic (c2) then
                  Add (Ch => c2);
                  Add (Str => " (ASCII printable character)");
                  Add (Ch => newl);
               else
                  Add (Str => Un_Control ((Ch => c2,
                                          Color => Color_Pair'First,
                                          Attr => Normal_Video)));
                  Add (Str => " (ASCII control character)");
                  Add (Ch => newl);
               end if;
            end;
         end if;
         --  TODO I am not sure why this was in the C version
         --  the delay statement scroll anyway.
         Get_Cursor_Position (Line => y, Column => x);
         if y >= Lines - 1 then
            Move_Cursor (Line => 0, Column => 0);
         end if;
         Clear_To_End_Of_Line;
      end if;

      firsttime := False;
      if c = Character'Pos ('g') then
         declare
            package p is new ncurses2.genericPuts (1024);
            use p;
            use p.BS;
            timedout : Boolean := False;
            boundedbuf : Bounded_String;
         begin
            Add (Str => "getstr test: ");
            Set_Echo_Mode (SwitchOn => True);
            --  Note that if delay mode is set
            --  Get can raise an exception.
            --  The C version would print the string it had so far
            --  also TODO get longer length string, like the C version
            declare begin
               myGet (Str => boundedbuf);
            exception when Curses_Exception =>
               Add (Str => "Timed out.");
               Add (Ch => newl);
               timedout := True;
            end;
            --  note that the Ada Get will stop reading at 1024.
            if not timedout then
               Set_Echo_Mode (SwitchOn => False);
               Add (Str => " I saw '");
               myAdd (Str => boundedbuf);
               Add (Str => "'.");
               Add (Ch => newl);
            end if;
         end;
      elsif c = Character'Pos ('s') then
         ShellOut (True);
      elsif c = Character'Pos ('x') or
            c = Character'Pos ('q') or
           (c = Key_None and blockflag = Blocking)
      then
         exit;
      elsif c = Character'Pos ('?') then
         Add (Str => "Type any key to see its keypad value.  Also:");
         Add (Ch => newl);
         Add (Str => "g -- triggers a getstr test");
         Add (Ch => newl);
         Add (Str => "s -- shell out");
         Add (Ch => newl);
         Add (Str => "q -- quit");
         Add (Ch => newl);
         Add (Str => "? -- repeats this help message");
         Add (Ch => newl);
      end if;

      loop
         c := Getchar;
         exit when c /= Key_None;
         if blockflag /= Blocking then
            Put (tmp6, incount); --  argh string length!
            Add (Str => tmp6);
            Add (Str => ": input timed out");
            Add (Ch => newl);
         else
            Put (tmp6, incount);
            Add (Str => tmp6);
            Add (Str => ": input error");
            Add (Ch => newl);
            exit;
         end if;
         incount := incount + 1;
      end loop;
   end loop;

   End_Mouse (tmp2);
   Set_Timeout_Mode (Mode => Blocking, Amount => 0); --  amount is ignored
   Set_Raw_Mode (SwitchOn => False);
   Set_NL_Mode (SwitchOn => True);
   Erase;
   End_Windows;
end ncurses2.getch_test;
