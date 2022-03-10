------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--               Terminal_Interface.Curses.Forms.Field_User_Data            --
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
--  $Revision: 1.17 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use  Terminal_Interface.Curses.Aux;

--  |
--  |=====================================================================
--  | man page form_field_userptr.3x
--  |=====================================================================
--  |
package body Terminal_Interface.Curses.Forms.Field_User_Data is
   --  |
   --  |
   --  |
   procedure Set_User_Data (Fld  : Field;
                            Data : User_Access)
   is
      function Set_Field_Userptr (Fld : Field;
                                  Usr : User_Access) return Eti_Error;
      pragma Import (C, Set_Field_Userptr, "set_field_userptr");

   begin
      Eti_Exception (Set_Field_Userptr (Fld, Data));
   end Set_User_Data;
   --  |
   --  |
   --  |
   function Get_User_Data (Fld  : Field) return User_Access
   is
      function Field_Userptr (Fld : Field) return User_Access;
      pragma Import (C, Field_Userptr, "field_userptr");
   begin
      return Field_Userptr (Fld);
   end Get_User_Data;

   procedure Get_User_Data (Fld  : Field;
                            Data : out User_Access)
   is
   begin
      Data := Get_User_Data (Fld);
   end Get_User_Data;

end Terminal_Interface.Curses.Forms.Field_User_Data;
