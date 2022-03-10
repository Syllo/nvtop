------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--         Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada      --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2002,2003 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.12 $
--  Binding Version 01.00
------------------------------------------------------------------------------
generic
   type T is (<>);

package Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada is
   pragma Preelaborate
     (Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada);

   function Create (Set            : Type_Set := Mixed_Case;
                    Case_Sensitive : Boolean  := False;
                    Must_Be_Unique : Boolean  := False)
                    return Enumeration_Field;

   function Value (Fld : Field;
                   Buf : Buffer_Number := Buffer_Number'First) return T;
   --  Translate the content of the fields buffer - indicated by the
   --  buffer number - into an enumeration value. If the buffer is empty
   --  or the content is invalid, a Constraint_Error is raises.

end Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada;
