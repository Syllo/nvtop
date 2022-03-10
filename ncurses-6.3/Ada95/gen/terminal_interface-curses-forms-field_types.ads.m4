--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-forms-field_user_data__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Forms.Field_Types              --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
-- Copyright 1998-2011,2014 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.21 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Interfaces.C;
with Terminal_Interface.Curses.Aux;

package Terminal_Interface.Curses.Forms.Field_Types is
   pragma Preelaborate (Terminal_Interface.Curses.Forms.Field_Types);
   subtype C_Int is Interfaces.C.int;

   --  MANPAGE(`form_fieldtype.3x')

   type Field_Type is abstract tagged null record;
   --  Abstract base type for all field types. A concrete field type
   --  is an extension that adds some data elements describing formats or
   --  boundary values for the type and validation routines.
   --  For the builtin low-level fieldtypes, the validation routines are
   --  already defined by the low-level C library.
   --  The builtin types like Alpha or AlphaNumeric etc. are defined in
   --  child packages of this package. You may use one of them as example
   --  how to create you own child packages for low-level field types that
   --  you may have already written in C.

   type Field_Type_Access is access all Field_Type'Class;

   --  ANCHOR(`set_field_type()',`Set_Type')
   procedure Set_Field_Type (Fld      : Field;
                             Fld_Type : Field_Type) is abstract;
   --  AKA
   --  But: we hide the vararg mechanism of the C interface. You always
   --       have to pass a single Field_Type parameter.

   --  ---------------------------------------------------------------------

   --  MANPAGE(`form_field_validation.3x')

   --  ANCHOR(`field_type()',`Get_Type')
   function Get_Type (Fld : Field) return Field_Type_Access;
   --  AKA
   --  ALIAS(`field_arg()')
   --  In Ada95 we can combine these. If you try to retrieve the field type
   --  that is not defined as extension of the abstract tagged type above,
   --  you will raise a Form_Exception.
   --  This is not inlined

   --  +----------------------------------------------------------------------
   --  | Private Part.
   --  | Most of this is used by the implementations of the child packages.
   --  |
private
   type Makearg_Function is access
     function (Args : System.Address) return System.Address;
   pragma Convention (C, Makearg_Function);

   type Copyarg_Function is access
     function (Usr : System.Address) return System.Address;
   pragma Convention (C, Copyarg_Function);

   type Freearg_Function is access
     procedure (Usr : System.Address);
   pragma Convention (C, Freearg_Function);

   type Field_Check_Function is access
     function (Fld : Field; Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Field_Check_Function);

   type Char_Check_Function is access
     function (Ch : C_Int; Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Char_Check_Function);

   type Choice_Function is access
     function (Fld : Field; Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Choice_Function);

   --  +----------------------------------------------------------------------
   --  | This must be in sync with the FIELDTYPE structure in form.h
   --  |
   type Low_Level_Field_Type is
      record
         Status :              Interfaces.C.unsigned_short;
         Ref_Count :           Interfaces.C.long;
         Left, Right :         System.Address;
         Makearg :             Makearg_Function;
         Copyarg :             Copyarg_Function;
         Freearg :             Freearg_Function;
         Fcheck :              Field_Check_Function;
         Ccheck :              Char_Check_Function;
         Next, Prev :          Choice_Function;
      end record;
   pragma Convention (C, Low_Level_Field_Type);
   type C_Field_Type is access all Low_Level_Field_Type;

   Null_Field_Type   : constant C_Field_Type := null;

   --  +----------------------------------------------------------------------
   --  | This four low-level fieldtypes are the ones associated with
   --  | fieldtypes handled by this binding. Any other low-level fieldtype
   --  | will result in a Form_Exception is function Get_Type.
   --  |
   M_Generic_Type   : C_Field_Type := null;
   M_Generic_Choice : C_Field_Type := null;
   M_Builtin_Router : C_Field_Type := null;
   M_Choice_Router  : C_Field_Type := null;

   --  Two wrapper functions to access those low-level fieldtypes defined
   --  in this package.
   function C_Builtin_Router return C_Field_Type;
   function C_Choice_Router  return C_Field_Type;

   procedure Wrap_Builtin (Fld : Field;
                           Typ : Field_Type'Class;
                           Cft : C_Field_Type := C_Builtin_Router);
   --  This procedure has to be called by the Set_Field_Type implementation
   --  for builtin low-level fieldtypes to replace it by an Ada95
   --  conformant Field_Type object.
   --  The parameter Cft must be C_Builtin_Router for regular low-level
   --  fieldtypes (like TYP_ALPHA or TYP_ALNUM) and C_Choice_Router for
   --  low-level fieldtypes witch choice functions (like TYP_ENUM).
   --  Any other value will raise a Form_Exception.

   function Make_Arg (Args : System.Address) return System.Address;
   pragma Import (C, Make_Arg, "void_star_make_arg");
   --  This is the Makearg_Function for the internal low-level types
   --  introduced by this binding.

   function Copy_Arg (Usr : System.Address) return System.Address;
   pragma Convention (C, Copy_Arg);
   --  This is the Copyarg_Function for the internal low-level types
   --  introduced by this binding.

   procedure Free_Arg (Usr : System.Address);
   pragma Convention (C, Free_Arg);
   --  This is the Freearg_Function for the internal low-level types
   --  introduced by this binding.

   function Field_Check_Router (Fld : Field;
                                Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Field_Check_Router);
   --  This is the Field_Check_Function for the internal low-level types
   --  introduced to wrap the low-level types by a Field_Type derived
   --  type. It routes the call to the corresponding low-level validation
   --  function.

   function Char_Check_Router (Ch : C_Int;
                               Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Char_Check_Router);
   --  This is the Char_Check_Function for the internal low-level types
   --  introduced to wrap the low-level types by a Field_Type derived
   --  type. It routes the call to the corresponding low-level validation
   --  function.

   function Next_Router (Fld : Field;
                         Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Next_Router);
   --  This is the Choice_Function for the internal low-level types
   --  introduced to wrap the low-level types by a Field_Type derived
   --  type. It routes the call to the corresponding low-level next_choice
   --  function.

   function Prev_Router (Fld : Field;
                         Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Prev_Router);
   --  This is the Choice_Function for the internal low-level types
   --  introduced to wrap the low-level types by a Field_Type derived
   --  type. It routes the call to the corresponding low-level prev_choice
   --  function.

   --  This is the Argument structure maintained by all low-level field types
   --  introduced by this binding.
   type Argument is record
      Typ : Field_Type_Access;   --  the Field_Type creating this record
      Usr : System.Address;      --  original arg for builtin low-level types
      Cft : C_Field_Type;        --  the original low-level type
   end record;
   type Argument_Access is access all Argument;

   --  +----------------------------------------------------------------------
   --  |
   --  | Some Imports of libform routines to deal with low-level fieldtypes.
   --  |
   function New_Fieldtype (Fcheck : Field_Check_Function;
                           Ccheck : Char_Check_Function)
     return C_Field_Type;
   pragma Import (C, New_Fieldtype, "new_fieldtype");

   function Set_Fieldtype_Arg (Cft : C_Field_Type;
                               Mak : Makearg_Function := Make_Arg'Access;
                               Cop : Copyarg_Function := Copy_Arg'Access;
                               Fre : Freearg_Function := Free_Arg'Access)
     return Aux.Eti_Error;
   pragma Import (C, Set_Fieldtype_Arg, "set_fieldtype_arg");

   function Set_Fieldtype_Choice (Cft : C_Field_Type;
                                  Next, Prev : Choice_Function)
     return Aux.Eti_Error;
   pragma Import (C, Set_Fieldtype_Choice, "set_fieldtype_choice");

end Terminal_Interface.Curses.Forms.Field_Types;
