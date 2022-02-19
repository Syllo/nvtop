------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--           Terminal_Interface.Curses.Forms.Field_Types.Enumeration        --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
-- Copyright 1999-2003,2009 Free Software Foundation, Inc.                  --
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
--  Binding Version 01.00
------------------------------------------------------------------------------
with Interfaces.C.Strings;

package Terminal_Interface.Curses.Forms.Field_Types.Enumeration is
   pragma Preelaborate
     (Terminal_Interface.Curses.Forms.Field_Types.Enumeration);

   type String_Access is access String;

   --  Type_Set is used by the child package Ada
   type Type_Set is (Lower_Case, Upper_Case, Mixed_Case);

   type Enum_Array is array (Positive range <>)
     of String_Access;

   type Enumeration_Info (C : Positive) is
      record
         Case_Sensitive       : Boolean := False;
         Match_Must_Be_Unique : Boolean := False;
         Names                : Enum_Array (1 .. C);
      end record;

   type Enumeration_Field is new Field_Type with private;

   function Create (Info : Enumeration_Info;
                    Auto_Release_Names : Boolean := False)
                    return Enumeration_Field;
   --  Make an fieldtype from the info. Enumerations are special, because
   --  they normally don't copy the enum values into a private store, so
   --  we have to care for the lifetime of the info we provide.
   --  The Auto_Release_Names flag may be used to automatically releases
   --  the strings in the Names array of the Enumeration_Info.

   function Make_Enumeration_Type (Info : Enumeration_Info;
                                   Auto_Release_Names : Boolean := False)
                                   return Enumeration_Field renames Create;

   procedure Release (Enum : in out Enumeration_Field);
   --  But we may want to release the field to release the memory allocated
   --  by it internally. After that the Enumeration field is no longer usable.

   --  The next type definitions are all ncurses extensions. They are typically
   --  not available in other curses implementations.

   procedure Set_Field_Type (Fld : Field;
                             Typ : Enumeration_Field);
   pragma Inline (Set_Field_Type);

private
   type CPA_Access is access Interfaces.C.Strings.chars_ptr_array;

   type Enumeration_Field is new Field_Type with
      record
         Case_Sensitive       : Boolean := False;
         Match_Must_Be_Unique : Boolean := False;
         Arr                  : CPA_Access := null;
      end record;

end Terminal_Interface.Curses.Forms.Field_Types.Enumeration;
