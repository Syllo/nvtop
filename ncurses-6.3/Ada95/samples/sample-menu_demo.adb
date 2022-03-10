------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                              Sample.Menu_Demo                            --
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
--  $Revision: 1.20 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;
with Terminal_Interface.Curses.Menus.Menu_User_Data;
with Terminal_Interface.Curses.Menus.Item_User_Data;

with Sample.Manifest; use Sample.Manifest;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Menu_Demo.Handler;
with Sample.Helpers; use Sample.Helpers;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Menu_Demo is

   package Spacing_Demo is
      procedure Spacing_Test;
   end Spacing_Demo;

   package body Spacing_Demo is

      procedure Spacing_Test
      is
         function My_Driver (M : Menu;
                             K : Key_Code;
                             P : Panel) return Boolean;

         procedure Set_Option_Key;
         procedure Set_Select_Key;
         procedure Set_Description_Key;
         procedure Set_Hide_Key;

         package Mh is new Sample.Menu_Demo.Handler (My_Driver);

         I : Item_Array_Access := new Item_Array'
           (New_Item ("January",   "31 Days"),
            New_Item ("February",  "28/29 Days"),
            New_Item ("March",     "31 Days"),
            New_Item ("April",     "30 Days"),
            New_Item ("May",       "31 Days"),
            New_Item ("June",      "30 Days"),
            New_Item ("July",      "31 Days"),
            New_Item ("August",    "31 Days"),
            New_Item ("September", "30 Days"),
            New_Item ("October",   "31 Days"),
            New_Item ("November",  "30 Days"),
            New_Item ("December",  "31 Days"),
            Null_Item);

         M : Menu   := New_Menu (I);
         Flip_State : Boolean := True;
         Hide_Long  : Boolean := False;

         type Format_Code is (Four_By_1, Four_By_2, Four_By_3);
         type Operations  is (Flip, Reorder, Reformat, Reselect, Describe);

         type Change is array (Operations) of Boolean;
         pragma Pack (Change);
         No_Change : constant Change := Change'(others => False);

         Current_Format : Format_Code := Four_By_1;
         To_Change : Change := No_Change;

         function My_Driver (M : Menu;
                             K : Key_Code;
                             P : Panel) return Boolean
         is
         begin
            if M = Null_Menu then
               raise Menu_Exception;
            end if;
            if P = Null_Panel then
               raise Panel_Exception;
            end if;
            To_Change := No_Change;
            if K in User_Key_Code'Range then
               if K = QUIT then
                  return True;
               end if;
            end if;
            if K in Special_Key_Code'Range then
               case K is
                  when Key_F4 =>
                     To_Change (Flip) := True;
                     return True;
                  when Key_F5 =>
                     To_Change (Reformat)  := True;
                     Current_Format := Four_By_1;
                     return True;
                  when Key_F6 =>
                     To_Change (Reformat)  := True;
                     Current_Format := Four_By_2;
                     return True;
                  when Key_F7 =>
                     To_Change (Reformat)  := True;
                     Current_Format := Four_By_3;
                     return True;
                  when Key_F8 =>
                     To_Change (Reorder) := True;
                     return True;
                  when Key_F9 =>
                     To_Change (Reselect) := True;
                     return True;
                  when Key_F10 =>
                     if Current_Format /= Four_By_3 then
                        To_Change (Describe) := True;
                        return True;
                     else
                        return False;
                     end if;
                  when Key_F11 =>
                     Hide_Long := not Hide_Long;
                     declare
                        O : Item_Option_Set;
                     begin
                        for J in I'Range loop
                           Get_Options (I.all (J), O);
                           O.Selectable := True;
                           if Hide_Long then
                              case J is
                                 when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
                                    O.Selectable := False;
                                 when others => null;
                              end case;
                           end if;
                           Set_Options (I.all (J), O);
                        end loop;
                     end;
                     return False;
                  when others => null;
               end case;
            end if;
            return False;
         end My_Driver;

         procedure Set_Option_Key
         is
            O : Menu_Option_Set;
         begin
            if Current_Format = Four_By_1 then
               Set_Soft_Label_Key (8, "");
            else
               Get_Options (M, O);
               if O.Row_Major_Order then
                  Set_Soft_Label_Key (8, "O-Col");
               else
                  Set_Soft_Label_Key (8, "O-Row");
               end if;
            end if;
            Refresh_Soft_Label_Keys_Without_Update;
         end Set_Option_Key;

         procedure Set_Select_Key
         is
            O : Menu_Option_Set;
         begin
            Get_Options (M, O);
            if O.One_Valued then
               Set_Soft_Label_Key (9, "Multi");
            else
               Set_Soft_Label_Key (9, "Singl");
            end if;
            Refresh_Soft_Label_Keys_Without_Update;
         end Set_Select_Key;

         procedure Set_Description_Key
         is
            O : Menu_Option_Set;
         begin
            if Current_Format = Four_By_3 then
               Set_Soft_Label_Key (10, "");
            else
               Get_Options (M, O);
               if O.Show_Descriptions then
                  Set_Soft_Label_Key (10, "-Desc");
               else
                  Set_Soft_Label_Key (10, "+Desc");
               end if;
            end if;
            Refresh_Soft_Label_Keys_Without_Update;
         end Set_Description_Key;

         procedure Set_Hide_Key
         is
         begin
            if Hide_Long then
               Set_Soft_Label_Key (11, "Enab");
            else
               Set_Soft_Label_Key (11, "Disab");
            end if;
            Refresh_Soft_Label_Keys_Without_Update;
         end Set_Hide_Key;

      begin
         Push_Environment ("MENU01");
         Notepad ("MENU-PAD01");
         Default_Labels;
         Set_Soft_Label_Key (4, "Flip");
         Set_Soft_Label_Key (5, "4x1");
         Set_Soft_Label_Key (6, "4x2");
         Set_Soft_Label_Key (7, "4x3");
         Set_Option_Key;
         Set_Select_Key;
         Set_Description_Key;
         Set_Hide_Key;

         Set_Format (M, 4, 1);
         loop
            Mh.Drive_Me (M);
            exit when To_Change = No_Change;
            if To_Change (Flip) then
               if Flip_State then
                  Flip_State := False;
                  Set_Spacing (M, 3, 2, 0);
               else
                  Flip_State := True;
                  Set_Spacing (M);
               end if;
            elsif To_Change (Reformat) then
               case Current_Format is
                  when Four_By_1 => Set_Format (M, 4, 1);
                  when Four_By_2 => Set_Format (M, 4, 2);
                  when Four_By_3 =>
                     declare
                        O : Menu_Option_Set;
                     begin
                        Get_Options (M, O);
                        O.Show_Descriptions := False;
                        Set_Options (M, O);
                        Set_Format (M, 4, 3);
                     end;
               end case;
               Set_Option_Key;
               Set_Description_Key;
            elsif To_Change (Reorder) then
               declare
                  O : Menu_Option_Set;
               begin
                  Get_Options (M, O);
                  O.Row_Major_Order := not O.Row_Major_Order;
                  Set_Options (M, O);
                  Set_Option_Key;
               end;
            elsif To_Change (Reselect) then
               declare
                  O : Menu_Option_Set;
               begin
                  Get_Options (M, O);
                  O.One_Valued := not O.One_Valued;
                  Set_Options (M, O);
                  Set_Select_Key;
               end;
            elsif To_Change (Describe) then
               declare
                  O : Menu_Option_Set;
               begin
                  Get_Options (M, O);
                  O.Show_Descriptions := not O.Show_Descriptions;
                  Set_Options (M, O);
                  Set_Description_Key;
               end;
            else
               null;
            end if;
         end loop;
         Set_Spacing (M);

         Pop_Environment;
         pragma Assert (Get_Index (Items (M, 1)) = Get_Index (I (1)));
         Delete (M);
         Free (I, True);
      end Spacing_Test;
   end Spacing_Demo;

   procedure Demo
   is
      --  We use this datatype only to test the instantiation of
      --  the Menu_User_Data generic package. No functionality
      --  behind it.
      type User_Data is new Integer;
      type User_Data_Access is access User_Data;

      --  Those packages are only instantiated to test the usability.
      --  No real functionality is shown in the demo.
      package MUD is new Menu_User_Data (User_Data, User_Data_Access);
      package IUD is new Item_User_Data (User_Data, User_Data_Access);

      function My_Driver (M : Menu;
                          K : Key_Code;
                          P : Panel) return Boolean;

      package Mh is new Sample.Menu_Demo.Handler (My_Driver);

      Itm : Item_Array_Access := new Item_Array'
        (New_Item ("Menu Layout Options"),
         New_Item ("Demo of Hook functions"),
         Null_Item);
      M : Menu := New_Menu (Itm);

      U1 : constant User_Data_Access := new User_Data'(4711);
      U2 : User_Data_Access;
      U3 : constant User_Data_Access := new User_Data'(4712);
      U4 : User_Data_Access;

      function My_Driver (M : Menu;
                          K : Key_Code;
                          P : Panel) return Boolean
      is
         Idx   : constant Positive := Get_Index (Current (M));
      begin
         if K in User_Key_Code'Range then
            if K = QUIT then
               return True;
            elsif K = SELECT_ITEM then
               if Idx in Itm'Range then
                  Hide (P);
                  Update_Panels;
               end if;
               case Idx is
                  when 1 => Spacing_Demo.Spacing_Test;
                  when others => Not_Implemented;
               end case;
               if Idx in Itm'Range then
                  Top (P);
                  Show (P);
                  Update_Panels;
                  Update_Screen;
               end if;
            end if;
         end if;
         return False;
      end My_Driver;
   begin
      Push_Environment ("MENU00");
      Notepad ("MENU-PAD00");
      Default_Labels;
      Refresh_Soft_Label_Keys_Without_Update;
      Set_Pad_Character (M, '|');

      MUD.Set_User_Data (M, U1);
      IUD.Set_User_Data (Itm.all (1), U3);

      Mh.Drive_Me (M);

      MUD.Get_User_Data (M, U2);
      pragma Assert (U1 = U2 and U1.all = 4711);

      IUD.Get_User_Data (Itm.all (1), U4);
      pragma Assert (U3 = U4 and U3.all = 4712);

      Pop_Environment;
      Delete (M);
      Free (Itm, True);
   end Demo;

end Sample.Menu_Demo;
