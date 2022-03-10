------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Keyboard_Handler                       --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
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
--  $Revision: 1.17 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;
with Ada.Characters.Handling; use Ada.Characters.Handling;

with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use  Terminal_Interface.Curses.Forms.Field_Types.Enumeration;

with Sample.Header_Handler; use Sample.Header_Handler;
with Sample.Form_Demo.Aux; use Sample.Form_Demo.Aux;
with Sample.Manifest; use Sample.Manifest;
with Sample.Form_Demo.Handler;

--  This package contains a centralized keyboard handler used throughout
--  this example. The handler establishes a timeout mechanism that provides
--  periodical updates of the common header lines used in this example.
--

package body Sample.Keyboard_Handler is

   In_Command : Boolean := False;

   function Get_Key (Win : Window := Standard_Window) return Real_Key_Code
   is
      K : Real_Key_Code;

      function Command return Real_Key_Code;

      function Command return Real_Key_Code
      is
         function My_Driver (F : Form;
                             C : Key_Code;
                             P : Panel) return Boolean;
         package Fh is new Sample.Form_Demo.Handler (My_Driver);

         type Label_Array is array (Label_Number) of String (1 .. 8);

         Labels : Label_Array;

         FA : Field_Array_Access := new Field_Array'
           (Make (0, 0, "Command:"),
            Make (Top => 0, Left => 9, Width => Columns - 11),
            Null_Field);

         K  : Real_Key_Code := Key_None;
         N  : Natural := 0;

         function My_Driver (F : Form;
                             C : Key_Code;
                             P : Panel) return Boolean
         is
            Ch : Character;
         begin
            if P = Null_Panel then
               raise Panel_Exception;
            end if;
            if C in User_Key_Code'Range and then C = QUIT then
               if Driver (F, F_Validate_Field) = Form_Ok  then
                  K := Key_None;
                  return True;
               end if;
            elsif C in Normal_Key_Code'Range then
               Ch := Character'Val (C);
               if Ch = LF or else Ch = CR then
                  if Driver (F, F_Validate_Field) = Form_Ok  then
                     declare
                        Buffer : String (1 .. Positive (Columns - 11));
                        Cmdc : String (1 .. 8);
                     begin
                        Get_Buffer (Fld => FA.all (2), Str => Buffer);
                        Trim (Buffer, Left);
                        if Buffer (1) /= ' ' then
                           Cmdc := To_Upper (Buffer (Cmdc'Range));
                           for I in Labels'Range loop
                              if Cmdc = Labels (I) then
                                 K := Function_Key_Code
                                   (Function_Key_Number (I));
                                 exit;
                              end if;
                           end loop;
                        end if;
                        return True;
                     end;
                  end if;
               end if;
            end if;
            return False;
         end My_Driver;

      begin
         In_Command := True;
         for I in Label_Number'Range loop
            Get_Soft_Label_Key (I, Labels (I));
            Trim (Labels (I), Left);
            Translate (Labels (I), Upper_Case_Map);
            if Labels (I) (1) /= ' ' then
               N := N + 1;
            end if;
         end loop;
         if N > 0 then --  some labels were really set
            declare
               Enum_Info    : Enumeration_Info (N);
               Enum_Field   : Enumeration_Field;
               J : Positive := Enum_Info.Names'First;

               Frm : Form := Create (FA);

            begin
               for I in Label_Number'Range loop
                  if Labels (I) (1) /= ' ' then
                     Enum_Info.Names (J) := new String'(Labels (I));
                     J := J + 1;
                  end if;
               end loop;
               Enum_Field := Create (Enum_Info, True);
               Set_Field_Type (FA.all (2), Enum_Field);
               Set_Background (FA.all (2), Normal_Video);

               Fh.Drive_Me (Frm, Lines - 3, 0);
               Delete (Frm);
               Update_Panels; Update_Screen;
            end;
         end if;
         Free (FA, True);
         In_Command := False;
         return K;
      end Command;

   begin
      Set_Timeout_Mode (Win, Delayed, 30000);
      loop
         K := Get_Keystroke (Win);
         if K = Key_None then  -- a timeout occurred
            Update_Header_Window;
         elsif K = 3 and then not In_Command  then  -- CTRL-C
            K := Command;
            exit when K /= Key_None;
         else
            exit;
         end if;
      end loop;
      return K;
   end Get_Key;

   procedure Init_Keyboard_Handler is
   begin
      null;
   end Init_Keyboard_Handler;

end Sample.Keyboard_Handler;
