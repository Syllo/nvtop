------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Curses_Demo                            --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2004,2011 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.18 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Terminal_Interface.Curses.Mouse;  use Terminal_Interface.Curses.Mouse;
with Terminal_Interface.Curses.Panels;  use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Panels.User_Data;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;

with Sample.Explanation; use Sample.Explanation;

with Sample.Menu_Demo.Handler;
with Sample.Curses_Demo.Mouse;
with Sample.Curses_Demo.Attributes;

package body Sample.Curses_Demo is

   type User_Data is new Integer;
   type User_Data_Access is access all User_Data;
   package PUD is new Panels.User_Data (User_Data, User_Data_Access);
   --  We use above instantiation of the generic User_Data package to
   --  demonstrate and test the use of the user data mechanism.

   procedure Demo
   is
      function My_Driver (M : Menu;
                          K : Key_Code;
                          Pan : Panel) return Boolean;
      package Mh is new Sample.Menu_Demo.Handler (My_Driver);

      Itm : Item_Array_Access := new Item_Array'
        (New_Item ("Attributes Demo"),
         New_Item ("Mouse Demo"),
         Null_Item);
      M  : Menu := New_Menu (Itm);
      U1 : constant User_Data_Access := new User_Data'(4711);
      U2 : User_Data_Access;

      function My_Driver (M : Menu;
                          K : Key_Code;
                          Pan : Panel) return Boolean
      is
         Idx : constant Positive := Get_Index (Current (M));
         Result : Boolean := False;
      begin
         PUD.Set_User_Data (Pan, U1); --  set some user data, just for fun
         if K in User_Key_Code'Range then
            if K = QUIT then
               Result := True;
            elsif K = SELECT_ITEM then
               if Idx in Itm'Range then
                  Hide (Pan);
                  Update_Panels;
               end if;
               case Idx is
                  when 1 => Sample.Curses_Demo.Attributes.Demo;
                  when 2 => Sample.Curses_Demo.Mouse.Demo;
                  when others => Not_Implemented;
               end case;
               if Idx in Itm'Range then
                  Top (Pan);
                  Show (Pan);
                  Update_Panels;
                  Update_Screen;
               end if;
            end if;
         end if;
         PUD.Get_User_Data (Pan, U2); --  get the user data
         pragma Assert (U1.all = U2.all and then U1 = U2);
         return Result;
      end My_Driver;

   begin

      if (1 + Item_Count (M)) /= Itm'Length then
         raise Constraint_Error;
      end if;

      if not Has_Mouse then
         declare
            O : Item_Option_Set;
         begin
            Get_Options (Itm.all (2), O);
            O.Selectable := False;
            Set_Options (Itm.all (2), O);
         end;
      end if;

      Push_Environment ("CURSES00");
      Notepad ("CURSES-PAD00");
      Default_Labels;
      Refresh_Soft_Label_Keys_Without_Update;

      Mh.Drive_Me (M, " Demo ");
      Pop_Environment;

      Delete (M);
      Free (Itm, True);
   end Demo;

end Sample.Curses_Demo;
