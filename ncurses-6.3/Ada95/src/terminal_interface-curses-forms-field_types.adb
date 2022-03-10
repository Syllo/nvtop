------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Forms.Field_Types              --
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
--  $Revision: 1.29 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

--  |
--  |=====================================================================
--  | man page form_fieldtype.3x
--  |=====================================================================
--  |
package body Terminal_Interface.Curses.Forms.Field_Types is

   use type System.Address;

   package Argument_Conversions is
      new System.Address_To_Access_Conversions (Argument);

   function Get_Fieldtype (F : Field) return C_Field_Type;
   pragma Import (C, Get_Fieldtype, "field_type");

   function Get_Arg (F : Field) return System.Address;
   pragma Import (C, Get_Arg, "field_arg");
   --  |
   --  |=====================================================================
   --  | man page form_field_validation.3x
   --  |=====================================================================
   --  |
   --  |
   --  |
   function Get_Type (Fld : Field) return Field_Type_Access
   is
      Low_Level : constant C_Field_Type := Get_Fieldtype (Fld);
      Arg : Argument_Access;
   begin
      if Low_Level = Null_Field_Type then
         return null;
      else
         if Low_Level = M_Builtin_Router or else
            Low_Level = M_Generic_Type or else
            Low_Level = M_Choice_Router or else
            Low_Level = M_Generic_Choice
         then
            Arg := Argument_Access
         (Argument_Conversions.To_Pointer (Get_Arg (Fld)));
            if Arg = null then
               raise Form_Exception;
            else
               return Arg.all.Typ;
            end if;
         else
            raise Form_Exception;
         end if;
      end if;
   end Get_Type;

   function Copy_Arg (Usr : System.Address) return System.Address
   is
   begin
      return Usr;
   end Copy_Arg;

   procedure Free_Arg (Usr : System.Address)
   is
      procedure Free_Type is new Ada.Unchecked_Deallocation
        (Field_Type'Class, Field_Type_Access);
      procedure Freeargs is new Ada.Unchecked_Deallocation
        (Argument, Argument_Access);

      To_Be_Free : Argument_Access
   := Argument_Access (Argument_Conversions.To_Pointer (Usr));
      Low_Level  : C_Field_Type;
   begin
      if To_Be_Free /= null then
         if To_Be_Free.all.Usr /= System.Null_Address then
            Low_Level := To_Be_Free.all.Cft;
            if Low_Level.all.Freearg /= null then
               Low_Level.all.Freearg (To_Be_Free.all.Usr);
            end if;
         end if;
         if To_Be_Free.all.Typ /= null then
            Free_Type (To_Be_Free.all.Typ);
         end if;
         Freeargs (To_Be_Free);
      end if;
   end Free_Arg;

   procedure Wrap_Builtin (Fld : Field;
                           Typ : Field_Type'Class;
                           Cft : C_Field_Type := C_Builtin_Router)
   is
      Usr_Arg   : constant System.Address := Get_Arg (Fld);
      Low_Level : constant C_Field_Type := Get_Fieldtype (Fld);
      Arg : Argument_Access;
      function Set_Fld_Type (F    : Field := Fld;
                             Cf   : C_Field_Type := Cft;
                             Arg1 : Argument_Access) return Eti_Error;
      pragma Import (C, Set_Fld_Type, "set_field_type_user");

   begin
      pragma Assert (Low_Level /= Null_Field_Type);
      if Cft /= C_Builtin_Router and then Cft /= C_Choice_Router then
         raise Form_Exception;
      else
         Arg := new Argument'(Usr => System.Null_Address,
                              Typ => new Field_Type'Class'(Typ),
                              Cft => Get_Fieldtype (Fld));
         if Usr_Arg /= System.Null_Address then
            if Low_Level.all.Copyarg /= null then
               Arg.all.Usr := Low_Level.all.Copyarg (Usr_Arg);
            else
               Arg.all.Usr := Usr_Arg;
            end if;
         end if;

         Eti_Exception (Set_Fld_Type (Arg1 => Arg));
      end if;
   end Wrap_Builtin;

   function Field_Check_Router (Fld : Field;
                                Usr : System.Address) return Curses_Bool
   is
      Arg  : constant Argument_Access
   := Argument_Access (Argument_Conversions.To_Pointer (Usr));
   begin
      pragma Assert (Arg /= null and then Arg.all.Cft /= Null_Field_Type
                     and then Arg.all.Typ /= null);
      if Arg.all.Cft.all.Fcheck /= null then
         return Arg.all.Cft.all.Fcheck (Fld, Arg.all.Usr);
      else
         return 1;
      end if;
   end Field_Check_Router;

   function Char_Check_Router (Ch  : C_Int;
                               Usr : System.Address) return Curses_Bool
   is
      Arg  : constant Argument_Access
   := Argument_Access (Argument_Conversions.To_Pointer (Usr));
   begin
      pragma Assert (Arg /= null and then Arg.all.Cft /= Null_Field_Type
                     and then Arg.all.Typ /= null);
      if Arg.all.Cft.all.Ccheck /= null then
         return Arg.all.Cft.all.Ccheck (Ch, Arg.all.Usr);
      else
         return 1;
      end if;
   end Char_Check_Router;

   function Next_Router (Fld : Field;
                         Usr : System.Address) return Curses_Bool
   is
      Arg  : constant Argument_Access
   := Argument_Access (Argument_Conversions.To_Pointer (Usr));
   begin
      pragma Assert (Arg /= null and then Arg.all.Cft /= Null_Field_Type
                     and then Arg.all.Typ /= null);
      if Arg.all.Cft.all.Next /= null then
         return Arg.all.Cft.all.Next (Fld, Arg.all.Usr);
      else
         return 1;
      end if;
   end Next_Router;

   function Prev_Router (Fld : Field;
                         Usr : System.Address) return Curses_Bool
   is
      Arg  : constant Argument_Access :=
               Argument_Access (Argument_Conversions.To_Pointer (Usr));
   begin
      pragma Assert (Arg /= null and then Arg.all.Cft /= Null_Field_Type
                     and then Arg.all.Typ /= null);
      if Arg.all.Cft.all.Prev /= null then
         return Arg.all.Cft.all.Prev (Fld, Arg.all.Usr);
      else
         return 1;
      end if;
   end Prev_Router;

   --  -----------------------------------------------------------------------
   --
   function C_Builtin_Router return C_Field_Type
   is
      T   : C_Field_Type;
   begin
      if M_Builtin_Router = Null_Field_Type then
         T := New_Fieldtype (Field_Check_Router'Access,
                             Char_Check_Router'Access);
         if T = Null_Field_Type then
            raise Form_Exception;
         else
            Eti_Exception (Set_Fieldtype_Arg (T,
                                              Make_Arg'Access,
                                              Copy_Arg'Access,
                                              Free_Arg'Access));
         end if;
         M_Builtin_Router := T;
      end if;
      pragma Assert (M_Builtin_Router /= Null_Field_Type);
      return M_Builtin_Router;
   end C_Builtin_Router;

   --  -----------------------------------------------------------------------
   --
   function C_Choice_Router return C_Field_Type
   is
      T   : C_Field_Type;
   begin
      if M_Choice_Router = Null_Field_Type then
         T := New_Fieldtype (Field_Check_Router'Access,
                             Char_Check_Router'Access);
         if T = Null_Field_Type then
            raise Form_Exception;
         else
            Eti_Exception (Set_Fieldtype_Arg (T,
                                              Make_Arg'Access,
                                              Copy_Arg'Access,
                                              Free_Arg'Access));

            Eti_Exception (Set_Fieldtype_Choice (T,
                                                 Next_Router'Access,
                                                 Prev_Router'Access));
         end if;
         M_Choice_Router := T;
      end if;
      pragma Assert (M_Choice_Router /= Null_Field_Type);
      return M_Choice_Router;
   end C_Choice_Router;

end Terminal_Interface.Curses.Forms.Field_Types;
