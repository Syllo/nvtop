------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--           Terminal_Interface.Curses.Forms.Field_Types.User.Choice        --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2008,2011 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.15 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Interfaces.C;

package Terminal_Interface.Curses.Forms.Field_Types.User.Choice is
   pragma Preelaborate
     (Terminal_Interface.Curses.Forms.Field_Types.User.Choice);

   subtype C_Int is Interfaces.C.int;

   type User_Defined_Field_Type_With_Choice is abstract new
     User_Defined_Field_Type with null record;
   --  This is the root of the mechanism we use to create field types in
   --  Ada95 that allow the prev/next mechanism. You should your own type
   --  derive from this one and implement the Field_Check, Character_Check
   --  Next and Previous functions for your own type.

   type User_Defined_Field_Type_With_Choice_Access is access all
     User_Defined_Field_Type_With_Choice'Class;

   function Next
     (Fld : Field;
      Typ : User_Defined_Field_Type_With_Choice) return Boolean
      is abstract;
   --  If True is returned, the function successfully generated a next
   --  value into the fields buffer.

   function Previous
     (Fld : Field;
      Typ : User_Defined_Field_Type_With_Choice) return Boolean
      is abstract;
   --  If True is returned, the function successfully generated a previous
   --  value into the fields buffer.

   --  +----------------------------------------------------------------------
   --  | Private Part.
   --  |
private
   function C_Generic_Choice return C_Field_Type;

   function Generic_Next (Fld : Field;
                          Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Generic_Next);
   --  This is the generic next Choice_Function for the low-level fieldtype
   --  representing all the User_Defined_Field_Type derivatives. It routes
   --  the call to the Next implementation for the type.

   function Generic_Prev (Fld : Field;
                          Usr : System.Address) return Curses_Bool;
   pragma Convention (C, Generic_Prev);
   --  This is the generic prev Choice_Function for the low-level fieldtype
   --  representing all the User_Defined_Field_Type derivatives. It routes
   --  the call to the Previous implementation for the type.

end Terminal_Interface.Curses.Forms.Field_Types.User.Choice;
