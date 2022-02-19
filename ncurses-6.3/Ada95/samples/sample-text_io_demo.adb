------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Text_IO_Demo                           --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
-- Copyright 1998-2006,2011 Free Software Foundation, Inc.                  --
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
--  Author:  Juergen Pfeifer, 1996
--  Version Control
--  $Revision: 1.19 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Numerics.Generic_Elementary_Functions;

with Ada.Numerics.Complex_Types;
use  Ada.Numerics.Complex_Types;

with Terminal_Interface.Curses;
use  Terminal_Interface.Curses;

with Terminal_Interface.Curses.Panels;
use  Terminal_Interface.Curses.Panels;

with Terminal_Interface.Curses.Text_IO;
use  Terminal_Interface.Curses.Text_IO;

with Terminal_Interface.Curses.Text_IO.Integer_IO;
with Terminal_Interface.Curses.Text_IO.Float_IO;
with Terminal_Interface.Curses.Text_IO.Enumeration_IO;
with Terminal_Interface.Curses.Text_IO.Complex_IO;
with Terminal_Interface.Curses.Text_IO.Decimal_IO;
with Terminal_Interface.Curses.Text_IO.Modular_IO;

with Sample.Manifest; use Sample.Manifest;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Explanation; use Sample.Explanation;

pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Complex_IO);
pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Decimal_IO);
pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Enumeration_IO);
pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Float_IO);
pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Integer_IO);
pragma Elaborate_All (Terminal_Interface.Curses.Text_Io.Modular_IO);

package body Sample.Text_IO_Demo is

   type Weekday is (Sunday,
                    Monday,
                    Tuesday,
                    Wednesday,
                    Thursday,
                    Friday,
                    Saturday);

   type Dec is delta 0.01 digits 5 range 0.0 .. 4.0;
   type Md is mod 5;

   package Math is new
     Ada.Numerics.Generic_Elementary_Functions (Float);

   package Int_IO is new
     Terminal_Interface.Curses.Text_IO.Integer_IO (Integer);
   use Int_IO;

   package Real_IO is new
     Terminal_Interface.Curses.Text_IO.Float_IO (Float);
   use Real_IO;

   package Enum_IO is new
     Terminal_Interface.Curses.Text_IO.Enumeration_IO (Weekday);
   use Enum_IO;

   package C_IO is new
     Terminal_Interface.Curses.Text_IO.Complex_IO (Ada.Numerics.Complex_Types);
   use C_IO;

   package D_IO is new
     Terminal_Interface.Curses.Text_IO.Decimal_IO (Dec);
   use D_IO;

   package M_IO is new
     Terminal_Interface.Curses.Text_IO.Modular_IO (Md);
   use M_IO;

   procedure Demo
   is
      W : Window;
      P : Panel := Create (Standard_Window);
      K : Real_Key_Code;
      Im : constant Complex := (0.0, 1.0);
      Fx : constant Dec := 3.14;
      Dc : constant Dec := 2.72;
      L : Md;

   begin
      Push_Environment ("TEXTIO");
      Default_Labels;
      Notepad ("TEXTIO-PAD00");

      Set_Echo_Mode (False);
      Set_Meta_Mode;
      Set_KeyPad_Mode;
      W := Sub_Window (Standard_Window, Lines - 2, Columns - 2, 1, 1);
      Box;
      Refresh_Without_Update;
      Set_Meta_Mode (W);
      Set_KeyPad_Mode (W);
      Immediate_Update_Mode (W, True);

      Set_Window (W);

      for I in 1 .. 10 loop
         Put ("Square root of ");
         Put (Item => I, Width => 5);
         Put (" is ");
         Put (Item => Math.Sqrt (Float (I)), Exp => 0, Aft => 7);
         New_Line;
      end loop;

      for W in Weekday loop
         Put (Item => W); Put (' ');
      end loop;
      New_Line;

      L := Md'First;
      for I in 1 .. 2 loop
         for J in Md'Range loop
            Put (L); Put (' ');
            L := L + 1;
         end loop;
      end loop;
      New_Line;

      Put (Im); New_Line;
      Put (Fx); New_Line;
      Put (Dc); New_Line;

      loop
         K := Get_Key;
         if K in Special_Key_Code'Range then
            case K is
               when QUIT_CODE     => exit;
               when HELP_CODE     => Explain_Context;
               when EXPLAIN_CODE  => Explain ("TEXTIOKEYS");
               when others        => null;
            end case;
         end if;
      end loop;

      Set_Window (Null_Window);
      Erase; Refresh_Without_Update;
      Delete (P);
      Delete (W);

      Pop_Environment;
   end Demo;

end Sample.Text_IO_Demo;
