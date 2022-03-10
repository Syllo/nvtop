------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 Sample                                   --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2008,2011 Free Software Foundation, Inc.                  --
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
with Text_IO;

with Ada.Exceptions; use Ada.Exceptions;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Terminal_Interface.Curses.Menus.Menu_User_Data;
with Terminal_Interface.Curses.Menus.Item_User_Data;

with Sample.Manifest; use Sample.Manifest;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Header_Handler; use Sample.Header_Handler;
with Sample.Explanation; use Sample.Explanation;

with Sample.Menu_Demo.Handler;
with Sample.Curses_Demo;
with Sample.Form_Demo;
with Sample.Menu_Demo;
with Sample.Text_IO_Demo;

with GNAT.OS_Lib;

package body Sample is

   type User_Data is
      record
         Data : Integer;
      end record;
   type User_Access is access User_Data;

   package Ud is new
     Terminal_Interface.Curses.Menus.Menu_User_Data
     (User_Data, User_Access);

   package Id is new
     Terminal_Interface.Curses.Menus.Item_User_Data
     (User_Data, User_Access);

   procedure Whow is
      procedure Main_Menu;
      procedure Main_Menu
      is
         function My_Driver (M : Menu;
                             K : Key_Code;
                             Pan : Panel) return Boolean;

         package Mh is new Sample.Menu_Demo.Handler (My_Driver);

         I : Item_Array_Access := new Item_Array'
           (New_Item ("Curses Core Demo"),
            New_Item ("Menu Demo"),
            New_Item ("Form Demo"),
            New_Item ("Text IO Demo"),
            Null_Item);

         M : Menu := New_Menu (I);

         D1, D2 : User_Access;
         I1, I2 : User_Access;

         function My_Driver (M : Menu;
                             K : Key_Code;
                             Pan : Panel) return Boolean
         is
            Idx : constant Positive := Get_Index (Current (M));
         begin
            if K in User_Key_Code'Range then
               if K = QUIT then
                  return True;
               elsif K = SELECT_ITEM then
                  if Idx <= 4 then
                     Hide (Pan);
                     Update_Panels;
                  end if;
                  case Idx is
                     when 1 => Sample.Curses_Demo.Demo;
                     when 2 => Sample.Menu_Demo.Demo;
                     when 3 => Sample.Form_Demo.Demo;
                     when 4 => Sample.Text_IO_Demo.Demo;
                     when others => null;
                  end case;
                  if Idx <= 4 then
                     Top (Pan);
                     Show (Pan);
                     Update_Panels;
                     Update_Screen;
                  end if;
               end if;
            end if;
            return False;
         end My_Driver;

      begin

         if (1 + Item_Count (M)) /= I'Length then
            raise Constraint_Error;
         end if;

         D1 := new User_Data'(Data => 4711);
         Ud.Set_User_Data (M, D1);

         I1 := new User_Data'(Data => 1174);
         Id.Set_User_Data (I.all (1), I1);

         Set_Spacing (Men => M, Row => 2);

         Default_Labels;
         Notepad ("MAINPAD");

         Mh.Drive_Me (M, " Demo ");

         Ud.Get_User_Data (M, D2);
         pragma Assert (D1 = D2);
         pragma Assert (D1.Data = D2.Data);

         Id.Get_User_Data (I.all (1), I2);
         pragma Assert (I1 = I2);
         pragma Assert (I1.Data = I2.Data);

         Delete (M);
         Free (I, True);
      end Main_Menu;

   begin
      Initialize (PC_Style_With_Index);
      Init_Header_Handler;
      Init_Screen;

      if Has_Colors then
         Start_Color;

         Init_Pair (Pair => Default_Colors,  Fore => Black,   Back => White);
         Init_Pair (Pair => Menu_Back_Color, Fore => Black,   Back => Cyan);
         Init_Pair (Pair => Menu_Fore_Color, Fore => Red,     Back => Cyan);
         Init_Pair (Pair => Menu_Grey_Color, Fore => White,   Back => Cyan);
         Init_Pair (Pair => Notepad_Color,   Fore => Black,   Back => Yellow);
         Init_Pair (Pair => Help_Color,      Fore => Blue,    Back => Cyan);
         Init_Pair (Pair => Form_Back_Color, Fore => Black,   Back => Cyan);
         Init_Pair (Pair => Form_Fore_Color, Fore => Red,     Back => Cyan);
         Init_Pair (Pair => Header_Color,    Fore => Black,   Back => Green);

         Set_Background (Ch => (Color => Default_Colors,
                                Attr  => Normal_Video,
                                Ch    => ' '));
         Set_Character_Attributes (Attr  => Normal_Video,
                                   Color => Default_Colors);
         Erase;

         Set_Soft_Label_Key_Attributes (Color => Header_Color);
         --  This propagates the attributes to the label window
         Refresh_Soft_Label_Keys;
      end if;

      Init_Keyboard_Handler;

      Set_Echo_Mode (False);
      Set_Raw_Mode;
      Set_Meta_Mode;
      Set_KeyPad_Mode;

      --  Initialize the Function Key Environment
      --  We have some fixed key throughout this sample
      Main_Menu;
      End_Windows;
      Curses_Free_All;

   exception
      when Event : others =>
         Terminal_Interface.Curses.End_Windows;
         Text_IO.Put ("Exception: ");
         Text_IO.Put (Exception_Name (Event));
         Text_IO.New_Line;
         GNAT.OS_Lib.OS_Exit (1);

   end Whow;

end Sample;
