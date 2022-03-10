------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                               ncurses2.util                              --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2008,2014 Free Software Foundation, Inc.                  --
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
with Ada.Text_IO; use Ada.Text_IO;

with Terminal_Interface.Curses.Trace; use Terminal_Interface.Curses.Trace;

with Interfaces.C;
with Interfaces.C.Strings;

with Ada.Characters.Handling;

with ncurses2.genericPuts;

package body ncurses2.util is

   --  #defines from C
   --  #define CTRL(x)         ((x) & 0x1f)
   function CTRL (c : Character) return Key_Code is
   begin
      return Character'Pos (c) mod 16#20#;
      --  uses a property of ASCII
      --  A = 16#41#; a = 16#61#; ^A = 1 or 16#1#
   end CTRL;

   function CTRL (c : Character) return Character is
   begin
      return Character'Val (Character'Pos (c) mod 16#20#);
      --  uses a property of ASCII
      --  A = 16#41#; a = 16#61#; ^A = 1 or 16#1#
   end CTRL;

   save_trace : Trace_Attribute_Set;
   --  Common function to allow ^T to toggle trace-mode in the middle of a test
   --  so that trace-files can be made smaller.
   function Getchar (win : Window := Standard_Window) return Key_Code is
      c : Key_Code;
   begin
      --  #ifdef TRACE
      c := Get_Keystroke (win);
      while c = CTRL ('T') loop
         --  if _nc_tracing  in C
         if Current_Trace_Setting /= Trace_Disable then
            save_trace := Current_Trace_Setting;
            Trace_Put ("TOGGLE-TRACING OFF");
            Current_Trace_Setting := Trace_Disable;
         else
            Current_Trace_Setting := save_trace;
         end if;
         Trace_On (Current_Trace_Setting);
         if Current_Trace_Setting /= Trace_Disable then
            Trace_Put ("TOGGLE-TRACING ON");
         end if;
      end loop;
      --  #else c := Get_Keystroke;
      return c;
   end Getchar;

   procedure Getchar (win : Window := Standard_Window) is
   begin
      if Getchar (win) < 0 then
         Beep;
      end if;
   end Getchar;

   procedure Pause is
   begin
      Move_Cursor (Line => Lines - 1, Column => 0);
      Add (Str => "Press any key to continue... ");
      Getchar;
   end Pause;

   procedure Cannot (s : String) is
      use Interfaces.C;
      use Interfaces.C.Strings;
      function getenv (x : char_array)  return chars_ptr;
      pragma Import (C, getenv, "getenv");
      tmp1 : char_array (0 .. 10);
      package p is new ncurses2.genericPuts (1024);
      use p;
      use p.BS;

      tmpb : BS.Bounded_String;

      Length : size_t;
   begin
      To_C ("TERM", tmp1, Length);
      Fill_String (getenv (tmp1), tmpb);
      Add (Ch => newl);
      myAdd (Str => "This " & tmpb & " terminal " & s);
      Pause;
   end Cannot;

   procedure ShellOut (message : Boolean) is
      use Interfaces.C;
      Txt : char_array (0 .. 10);
      Length : size_t;
      procedure system (x : char_array);
      pragma Import (C, system, "system");
   begin
      To_C ("sh", Txt,  Length);
      if message then
         Add (Str => "Shelling out...");
      end if;
      Save_Curses_Mode (Mode => Curses);
      End_Windows;
      system (Txt);
      if message then
         Add (Str => "returned from shellout.");
         Add (Ch => newl);
      end if;
      Refresh;
   end ShellOut;

   function Is_Digit (c : Key_Code) return Boolean is
   begin
      if c >= 16#100# then
         return False;
      else
         return Ada.Characters.Handling.Is_Digit (Character'Val (c));
      end if;
   end Is_Digit;

   procedure P (s : String) is
   begin
      Add (Str => s);
      Add (Ch => newl);
   end P;

   function Code_To_Char (c : Key_Code) return Character is
   begin
      if c > Character'Pos (Character'Last) then
         return Character'Val (0);
         --  maybe raise exception?
      else
         return Character'Val (c);
      end if;
   end Code_To_Char;

   --  This was untestable due to a bug in GNAT (3.12p)
   --  Hmm, what bug? I don't remember.
   function ctoi (c : Character) return Integer is
   begin
      return Character'Pos (c) - Character'Pos ('0');
   end ctoi;

end ncurses2.util;
