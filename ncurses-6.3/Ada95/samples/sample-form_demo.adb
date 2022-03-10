------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                             Sample.Form_Demo                             --
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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_User_Data;
with Sample.My_Field_Type; use Sample.My_Field_Type;
with Sample.Explanation; use Sample.Explanation;
with Sample.Form_Demo.Aux; use Sample.Form_Demo.Aux;
with Sample.Function_Key_Setting; use Sample.Function_Key_Setting;
with Sample.Form_Demo.Handler;

with Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada;
with Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
use  Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
with Terminal_Interface.Curses.Forms.Field_Types.IntField;
use  Terminal_Interface.Curses.Forms.Field_Types.IntField;

package body Sample.Form_Demo is

   type User_Data is
      record
         Data : Integer;
      end record;
   type User_Access is access User_Data;

   package Fld_U is new
     Terminal_Interface.Curses.Forms.Field_User_Data (User_Data,
                                                      User_Access);

   type Weekday is (Sunday, Monday, Tuesday, Wednesday, Thursday,
                    Friday, Saturday);

   package Weekday_Enum is new
     Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada (Weekday);

   Enum_Field : constant Enumeration_Field :=
     Weekday_Enum.Create;

   procedure Demo
   is

      Mft : constant My_Data := (Ch => 'X');

      FA : Field_Array_Access := new Field_Array'
        (Make (0, 14, "Sample Entry Form"),
         Make (2, 0,  "WeekdayEnumeration"),
         Make (2, 20, "Numeric 1-10"),
         Make (2, 34, "Only 'X'"),
         Make (5, 0, "Multiple Lines offscreen(Scroll)"),
         Make (Width => 18, Top => 3, Left =>  0),
         Make (Width => 12, Top => 3, Left => 20),
         Make (Width => 12, Top => 3, Left => 34),
         Make (Width => 46, Top => 6, Left => 0, Height => 4, Off_Screen => 2),
         Null_Field
         );

      Frm : Terminal_Interface.Curses.Forms.Form := Create (FA);

      I_F : constant Integer_Field := (Precision   => 0,
                                       Lower_Limit => 1,
                                       Upper_Limit => 10);

      F1, F2 : User_Access;

      package Fh is new Sample.Form_Demo.Handler (Default_Driver);

   begin
      Push_Environment ("FORM00");
      Notepad ("FORM-PAD00");
      Default_Labels;

      Set_Field_Type (FA.all (6), Enum_Field);
      Set_Field_Type (FA.all (7), I_F);
      Set_Field_Type (FA.all (8), Mft);

      F1 := new User_Data'(Data => 4711);
      Fld_U.Set_User_Data (FA.all (1), F1);

      Fh.Drive_Me (Frm);

      Fld_U.Get_User_Data (FA.all (1), F2);
      pragma Assert (F1 = F2);
      pragma Assert (F1.Data = F2.Data);

      Pop_Environment;
      Delete (Frm);

      Free (FA, True);
   end Demo;

end Sample.Form_Demo;
