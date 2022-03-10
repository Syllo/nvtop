------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--              Terminal_Interface.Curses.Forms.Field_Types.User            --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2011,2014 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.24 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System.Address_To_Access_Conversions;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;

package body Terminal_Interface.Curses.Forms.Field_Types.User is

   procedure Set_Field_Type (Fld : Field;
                             Typ : User_Defined_Field_Type)
   is
      function Allocate_Arg (T : User_Defined_Field_Type'Class)
                             return Argument_Access;

      function Set_Fld_Type (F    : Field := Fld;
                             Cft  : C_Field_Type := C_Generic_Type;
                             Arg1 : Argument_Access)
                             return Eti_Error;
      pragma Import (C, Set_Fld_Type, "set_field_type_user");

      function Allocate_Arg (T : User_Defined_Field_Type'Class)
                             return Argument_Access
      is
         Ptr : constant Field_Type_Access
             := new User_Defined_Field_Type'Class'(T);
      begin
         return new Argument'(Usr => System.Null_Address,
                              Typ => Ptr,
                              Cft => Null_Field_Type);
      end Allocate_Arg;

   begin
      Eti_Exception (Set_Fld_Type (Arg1 => Allocate_Arg (Typ)));
   end Set_Field_Type;

   package Argument_Conversions is
      new System.Address_To_Access_Conversions (Argument);

   function Generic_Field_Check (Fld : Field;
                                 Usr : System.Address) return Curses_Bool
   is
      Result : Boolean;
      Udf    : constant User_Defined_Field_Type_Access :=
        User_Defined_Field_Type_Access
          (Argument_Access (Argument_Conversions.To_Pointer (Usr)).all.Typ);
   begin
      Result := Field_Check (Fld, Udf.all);
      return Curses_Bool (Boolean'Pos (Result));
   end Generic_Field_Check;

   function Generic_Char_Check (Ch  : C_Int;
                                Usr : System.Address) return Curses_Bool
   is
      Result : Boolean;
      Udf    : constant User_Defined_Field_Type_Access :=
        User_Defined_Field_Type_Access
          (Argument_Access (Argument_Conversions.To_Pointer (Usr)).all.Typ);
   begin
      Result := Character_Check (Character'Val (Ch), Udf.all);
      return Curses_Bool (Boolean'Pos (Result));
   end Generic_Char_Check;

   --  -----------------------------------------------------------------------
   --
   function C_Generic_Type return C_Field_Type
   is
      Res : Eti_Error;
      T   : C_Field_Type;
   begin
      if M_Generic_Type = Null_Field_Type then
         T := New_Fieldtype (Generic_Field_Check'Access,
                             Generic_Char_Check'Access);
         if T = Null_Field_Type then
            raise Form_Exception;
         else
            Res := Set_Fieldtype_Arg (T,
                                      Make_Arg'Access,
                                      Copy_Arg'Access,
                                      Free_Arg'Access);
            Eti_Exception (Res);
         end if;
         M_Generic_Type := T;
      end if;
      pragma Assert (M_Generic_Type /= Null_Field_Type);
      return M_Generic_Type;
   end C_Generic_Type;

end Terminal_Interface.Curses.Forms.Field_Types.User;
