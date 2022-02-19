------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                   Rain                                   --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2007,2008 Free Software Foundation, Inc.                  --
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
--  Author:  Laurent Pautet <pautet@gnat.com>
--  Modified by:  Juergen Pfeifer, 1997
--  Version Control
--  $Revision: 1.9 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
--                                                                          --
with ncurses2.util; use ncurses2.util;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Status; use Status;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

procedure Rain is

   Visibility : Cursor_Visibility;

   subtype X_Position is Line_Position;
   subtype Y_Position is Column_Position;

   Xpos    : array (1 .. 5) of X_Position;
   Ypos    : array (1 .. 5) of Y_Position;

   done : Boolean;

   c : Key_Code;

   N : Integer;

   G : Generator;

   Max_X, X : X_Position;
   Max_Y, Y : Y_Position;

   procedure Next (J : in out Integer);
   procedure Cursor (X : X_Position; Y : Y_Position);

   procedure Next (J : in out Integer) is
   begin
      if J = 5 then
         J := 1;
      else
         J := J + 1;
      end if;
   end Next;

   procedure Cursor (X : X_Position; Y : Y_Position) is
   begin
      Move_Cursor (Line => X, Column => Y);
   end Cursor;
   pragma Inline (Cursor);

begin

   Init_Screen;
   Set_NL_Mode;
   Set_Echo_Mode (False);

   Visibility := Invisible;
   Set_Cursor_Visibility (Visibility);
   Set_Timeout_Mode (Standard_Window, Non_Blocking, 0);

   Max_X := Lines - 5;
   Max_Y := Columns - 5;

   for I in Xpos'Range loop
      Xpos (I) := X_Position (Float (Max_X) * Random (G)) + 2;
      Ypos (I) := Y_Position (Float (Max_Y) * Random (G)) + 2;
   end loop;

   N := 1;
   done := False;
   while not done and Process.Continue loop

      X := X_Position (Float (Max_X) * Random (G)) + 2;
      Y := Y_Position (Float (Max_Y) * Random (G)) + 2;

      Cursor (X, Y);
      Add (Ch => '.');

      Cursor (Xpos (N), Ypos (N));
      Add (Ch => 'o');

      --
      Next (N);
      Cursor (Xpos (N), Ypos (N));
      Add (Ch => 'O');

      --
      Next (N);
      Cursor (Xpos (N) - 1, Ypos (N));
      Add (Ch => '-');
      Cursor (Xpos (N), Ypos (N) - 1);
      Add (Str => "|.|");
      Cursor (Xpos (N) + 1, Ypos (N));
      Add (Ch => '-');

      --
      Next (N);
      Cursor (Xpos (N) - 2, Ypos (N));
      Add (Ch => '-');
      Cursor (Xpos (N) - 1, Ypos (N) - 1);
      Add (Str => "/\\");
      Cursor (Xpos (N), Ypos (N) - 2);
      Add (Str => "| O |");
      Cursor (Xpos (N) + 1, Ypos (N) - 1);
      Add (Str => "\\/");
      Cursor (Xpos (N) + 2, Ypos (N));
      Add (Ch => '-');

      --
      Next (N);
      Cursor (Xpos (N) - 2, Ypos (N));
      Add (Ch => ' ');
      Cursor (Xpos (N) - 1, Ypos (N) - 1);
      Add (Str => "   ");
      Cursor (Xpos (N), Ypos (N) - 2);
      Add (Str => "     ");
      Cursor (Xpos (N) + 1, Ypos (N) - 1);
      Add (Str => "   ");
      Cursor (Xpos (N) + 2, Ypos (N));
      Add (Ch => ' ');

      Xpos (N) := X;
      Ypos (N) := Y;

      c := Getchar;
      case c is
      when Character'Pos ('q') => done := True;
      when Character'Pos ('Q') => done := True;
      when Character'Pos ('s') => Set_NoDelay_Mode (Standard_Window, False);
      when Character'Pos (' ') => Set_NoDelay_Mode (Standard_Window, True);
      when others => null;
      end case;

      Nap_Milli_Seconds (50);
   end loop;

   Visibility := Normal;
   Set_Cursor_Visibility (Visibility);
   End_Windows;
   Curses_Free_All;

end Rain;
