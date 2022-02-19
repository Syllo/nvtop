------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Mouse                      --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
-- Copyright 1999-2009,2014 Free Software Foundation, Inc.                  --
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
--  Version Control:
--  $Revision: 1.28 $
--  $Date: 2020/06/27 18:50:44 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces.C; use Interfaces.C;
use Interfaces;

package body Terminal_Interface.Curses.Mouse is

   function Has_Mouse return Boolean
   is
      function Mouse_Avail return C_Int;
      pragma Import (C, Mouse_Avail, "has_mouse");
   begin
      if Has_Key (Key_Mouse) or else Mouse_Avail /= 0 then
         return True;
      else
         return False;
      end if;
   end Has_Mouse;

   function Get_Mouse return Mouse_Event
   is
      type Event_Access is access all Mouse_Event;

      function Getmouse (Ev : Event_Access) return C_Int;
      pragma Import (C, Getmouse, "getmouse");

      Event : aliased Mouse_Event;
   begin
      if Getmouse (Event'Access) = Curses_Err then
         raise Curses_Exception;
      end if;
      return Event;
   end Get_Mouse;

   procedure Register_Reportable_Event (Button : Mouse_Button;
                                        State  : Button_State;
                                        Mask   : in out Event_Mask)
   is
      Button_Nr : constant Natural := Mouse_Button'Pos (Button);
      State_Nr  : constant Natural := Button_State'Pos (State);
   begin
      if Button in Modifier_Keys and then State /= Pressed then
         raise Curses_Exception;
      else
         if Button in Real_Buttons then
            Mask := Mask or ((2 ** (6 * Button_Nr)) ** State_Nr);
         else
            Mask := Mask or (BUTTON_CTRL ** (Button_Nr - 4));
         end if;
      end if;
   end Register_Reportable_Event;

   procedure Register_Reportable_Events (Button : Mouse_Button;
                                         State  : Button_States;
                                         Mask   : in out Event_Mask)
   is
   begin
      for S in Button_States'Range loop
         if State (S) then
            Register_Reportable_Event (Button, S, Mask);
         end if;
      end loop;
   end Register_Reportable_Events;

   function Start_Mouse (Mask : Event_Mask := All_Events)
                         return Event_Mask
   is
      function MMask (M : Event_Mask;
                      O : access Event_Mask) return Event_Mask;
      pragma Import (C, MMask, "mousemask");
      R   : Event_Mask;
      Old : aliased Event_Mask;
   begin
      R := MMask (Mask, Old'Access);
      if R = No_Events then
         Beep;
      end if;
      return Old;
   end Start_Mouse;

   procedure End_Mouse (Mask : Event_Mask := No_Events)
   is
   begin
      if Mask /= No_Events then
         Beep;
      end if;
   end End_Mouse;

   procedure Dispatch_Event (Mask   : Event_Mask;
                             Button : out Mouse_Button;
                             State  : out Button_State);

   procedure Dispatch_Event (Mask   : Event_Mask;
                             Button : out Mouse_Button;
                             State  : out Button_State) is
      L : Event_Mask;
   begin
      Button := Alt;  --  preset to non real button;
      if (Mask and BUTTON1_EVENTS) /= 0 then
         Button := Left;
      elsif (Mask and BUTTON2_EVENTS) /= 0 then
         Button := Middle;
      elsif (Mask and BUTTON3_EVENTS) /= 0 then
         Button := Right;
      elsif (Mask and BUTTON4_EVENTS) /= 0 then
         Button := Button4;
      end if;
      if Button in Real_Buttons then
         State := Released;  --  preset to non real button;
         L := 2 ** (6 * Mouse_Button'Pos (Button));
         for I in Button_State'Range loop
            if (Mask and L) /= 0 then
               State := I;
               exit;
            end if;
            L := 2 * L;
         end loop;
      else
         State := Pressed;
         if (Mask and BUTTON_CTRL) /= 0 then
            Button := Control;
         elsif (Mask and BUTTON_SHIFT) /= 0 then
            Button := Shift;
         elsif (Mask and BUTTON_ALT) /= 0 then
            Button := Alt;
         end if;
      end if;
   end Dispatch_Event;

   procedure Get_Event (Event  : Mouse_Event;
                        Y      : out Line_Position;
                        X      : out Column_Position;
                        Button : out Mouse_Button;
                        State  : out Button_State)
   is
      Mask  : constant Event_Mask := Event.Bstate;
   begin
      X := Column_Position (Event.X);
      Y := Line_Position   (Event.Y);
      Dispatch_Event (Mask, Button, State);
   end Get_Event;

   procedure Unget_Mouse (Event : Mouse_Event)
   is
      function Ungetmouse (Ev : Mouse_Event) return C_Int;
      pragma Import (C, Ungetmouse, "ungetmouse");
   begin
      if Ungetmouse (Event) = Curses_Err then
         raise Curses_Exception;
      end if;
   end Unget_Mouse;

   function Enclosed_In_Window (Win    : Window := Standard_Window;
                                Event  : Mouse_Event) return Boolean
   is
      function Wenclose (Win : Window; Y : C_Int; X : C_Int)
                         return Curses_Bool;
      pragma Import (C, Wenclose, "wenclose");
   begin
      if Wenclose (Win, C_Int (Event.Y), C_Int (Event.X))
        = Curses_Bool_False
      then
         return False;
      else
         return True;
      end if;
   end Enclosed_In_Window;

   function Mouse_Interval (Msec : Natural := 200) return Natural
   is
      function Mouseinterval (Msec : C_Int) return C_Int;
      pragma Import (C, Mouseinterval, "mouseinterval");
   begin
      return Natural (Mouseinterval (C_Int (Msec)));
   end Mouse_Interval;

end Terminal_Interface.Curses.Mouse;
