------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                       Sample.Curses_Demo.Attributes                      --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2002,2003 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.14 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels;  use Terminal_Interface.Curses.Panels;

with Sample.Manifest; use Sample.Manifest;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Curses_Demo.Attributes is

   procedure Demo
   is
      P : Panel := Create (Standard_Window);
      K : Real_Key_Code;
   begin
      Set_Meta_Mode;
      Set_KeyPad_Mode;

      Top (P);

      Push_Environment ("ATTRIBDEMO");
      Default_Labels;
      Notepad ("ATTRIB-PAD00");

      Set_Character_Attributes (Attr => (others => False));
      Add (Line => 1, Column => Columns / 2 - 10,
           Str => "This is NORMAL");

      Set_Character_Attributes (Attr => (Stand_Out => True,
                                          others => False));
      Add (Line => 2, Column => Columns / 2 - 10,
           Str => "This is Stand_Out");

      Set_Character_Attributes (Attr => (Under_Line => True,
                                          others => False));
      Add (Line => 3, Column => Columns / 2 - 10,
           Str => "This is Under_Line");

      Set_Character_Attributes (Attr => (Reverse_Video => True,
                                          others => False));
      Add (Line => 4, Column => Columns / 2 - 10,
           Str => "This is Reverse_Video");

      Set_Character_Attributes (Attr => (Blink => True,
                                          others => False));
      Add (Line => 5, Column => Columns / 2 - 10,
           Str => "This is Blink");

      Set_Character_Attributes (Attr => (Dim_Character => True,
                                          others => False));
      Add (Line => 6, Column => Columns / 2 - 10,
           Str => "This is Dim_Character");

      Set_Character_Attributes (Attr => (Bold_Character => True,
                                          others => False));
      Add (Line => 7, Column => Columns / 2 - 10,
           Str => "This is Bold_Character");

      Refresh_Without_Update;
      Update_Panels; Update_Screen;

      loop
         K := Get_Key;
         if K in Special_Key_Code'Range then
            case K is
               when QUIT_CODE     => exit;
               when HELP_CODE     => Explain_Context;
               when EXPLAIN_CODE  => Explain ("ATTRIBKEYS");
               when others        => null;
            end case;
         end if;
      end loop;

      Pop_Environment;
      Clear;
      Refresh_Without_Update;
      Delete (P);
      Update_Panels; Update_Screen;

   end Demo;

end Sample.Curses_Demo.Attributes;
