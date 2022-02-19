------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                         Sample.Curses_Demo.Mouse                         --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2006,2008 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.17 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Mouse; use Terminal_Interface.Curses.Mouse;
with Terminal_Interface.Curses.Text_IO; use Terminal_Interface.Curses.Text_IO;
with Terminal_Interface.Curses.Text_IO.Integer_IO;
with Terminal_Interface.Curses.Text_IO.Enumeration_IO;

with Sample.Helpers; use Sample.Helpers;
with Sample.Manifest; use Sample.Manifest;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Curses_Demo.Mouse is

   package Int_IO is new
     Terminal_Interface.Curses.Text_IO.Integer_IO (Integer);
   use Int_IO;

   package Button_IO is new
     Terminal_Interface.Curses.Text_IO.Enumeration_IO (Mouse_Button);
   use Button_IO;

   package State_IO is new
     Terminal_Interface.Curses.Text_IO.Enumeration_IO (Button_State);
   use State_IO;

   procedure Demo is

      type Controls is array (1 .. 3) of Panel;

      Frame : Window;
      Msg   : Window;
      Ctl   : Controls;
      Pan   : Panel;
      K     : Real_Key_Code;
      V     : Cursor_Visibility := Invisible;
      W     : Window;
      Note  : Window;
      Msg_L : constant Line_Count := 8;
      Lins  : Line_Position := Lines;
      Cols  : Column_Position;
      Mask  : Event_Mask;
      procedure Show_Mouse_Event;

      procedure Show_Mouse_Event
      is
         Evt    : constant Mouse_Event := Get_Mouse;
         Y      : Line_Position;
         X      : Column_Position;
         Button : Mouse_Button;
         State  : Button_State;
         W      : Window;
      begin
         Get_Event (Evt, Y, X, Button, State);
         Put (Msg, "Event at");
         Put (Msg, "  X=");    Put (Msg, Integer (X), 3);
         Put (Msg, ", Y=");    Put (Msg, Integer (Y), 3);
         Put (Msg, ", Btn=");  Put (Msg, Button, 10);
         Put (Msg, ", Stat="); Put (Msg, State, 15);
         for I in Ctl'Range loop
            W := Get_Window (Ctl (I));
            if Enclosed_In_Window (W, Evt) then
               Transform_Coordinates (W, Y, X, From_Screen);
               Put (Msg, ",Box(");
               Put (Msg, (I), 1); Put (Msg, ",");
               Put (Msg, Integer (Y), 1); Put (Msg, ",");
               Put (Msg, Integer (X), 1); Put (Msg, ")");
            end if;
         end loop;
         New_Line (Msg);
         Flush (Msg);
         Update_Panels; Update_Screen;
      end Show_Mouse_Event;

   begin
      Push_Environment ("MOUSE00");
      Notepad ("MOUSE-PAD00");
      Default_Labels;
      Set_Cursor_Visibility (V);

      Note  := Notepad_Window;
      if Note /= Null_Window then
         Get_Window_Position (Note, Lins, Cols);
      end if;
      Frame := Create (Msg_L, Columns, Lins - Msg_L, 0);
      if Has_Colors then
         Set_Background (Win => Frame,
                         Ch => (Color => Default_Colors,
                                Attr  => Normal_Video,
                                Ch    => ' '));
         Set_Character_Attributes (Win   => Frame,
                                   Attr  => Normal_Video,
                                   Color => Default_Colors);
         Erase (Frame);
      end if;
      Msg   := Derived_Window (Frame, Msg_L - 2, Columns - 2, 1, 1);
      Pan   := Create (Frame);

      Set_Meta_Mode;
      Set_KeyPad_Mode;
      Mask := Start_Mouse;

      Box (Frame);
      Window_Title (Frame, "Mouse Protocol");
      Refresh_Without_Update (Frame);
      Allow_Scrolling (Msg, True);

      declare
         Middle_Column : constant Integer := Integer (Columns) / 2;
         Middle_Index  : constant Natural := Ctl'First + (Ctl'Length / 2);
         Width         : constant Column_Count := 5;
         Height        : constant Line_Count   := 3;
         Half          : constant Column_Count := Width / 2;
         Space         : constant Column_Count := 3;
         Position      : Integer;
         W             : Window;
      begin
         for I in Ctl'Range loop
            Position := ((I) - Integer (Middle_Index)) *
              Integer (Half + Space + Width) + Middle_Column;
            W := Create (Height,
                         Width,
                         1,
                         Column_Position (Position));
            if Has_Colors then
               Set_Background (Win => W,
                               Ch => (Color => Menu_Back_Color,
                                      Attr  => Normal_Video,
                                      Ch    => ' '));
               Set_Character_Attributes (Win   => W,
                                         Attr  => Normal_Video,
                                         Color => Menu_Fore_Color);
               Erase (W);
            end if;
            Ctl (I) := Create (W);
            Box (W);
            Move_Cursor (W, 1, Half);
            Put (W, (I), 1);
            Refresh_Without_Update (W);
         end loop;
      end;

      Update_Panels; Update_Screen;

      loop
         K := Get_Key;
         if K in Special_Key_Code'Range then
            case K is
               when QUIT_CODE     => exit;
               when HELP_CODE     => Explain_Context;
               when EXPLAIN_CODE  => Explain ("MOUSEKEYS");
               when Key_Mouse     => Show_Mouse_Event;
               when others        => null;
            end case;
         end if;
      end loop;

      for I in Ctl'Range loop
         W := Get_Window (Ctl (I));
         Clear (W);
         Delete (Ctl (I));
         Delete (W);
      end loop;

      Clear (Frame);
      Delete (Pan);
      Delete (Msg);
      Delete (Frame);

      Set_Cursor_Visibility (V);
      End_Mouse (Mask);

      Pop_Environment;
      Update_Panels; Update_Screen;

   end Demo;

end Sample.Curses_Demo.Mouse;
