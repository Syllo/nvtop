------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--         Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada      --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2004,2011 Free Software Foundation, Inc.                  --
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
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada is

   function Create (Set            : Type_Set := Mixed_Case;
                    Case_Sensitive : Boolean  := False;
                    Must_Be_Unique : Boolean  := False)
                    return Enumeration_Field
   is
      I : Enumeration_Info (T'Pos (T'Last) - T'Pos (T'First) + 1);
      J : Positive := 1;
   begin
      I.Case_Sensitive := Case_Sensitive;
      I.Match_Must_Be_Unique := Must_Be_Unique;

      for E in T'Range loop
         I.Names (J) := new String'(T'Image (E));
         --  The Image attribute defaults to upper case, so we have to handle
         --  only the other ones...
         if Set /= Upper_Case then
            I.Names (J).all := To_Lower (I.Names (J).all);
            if Set = Mixed_Case then
               I.Names (J).all (I.Names (J).all'First) :=
                 To_Upper (I.Names (J).all (I.Names (J).all'First));
            end if;
         end if;
         J := J + 1;
      end loop;

      return Create (I, True);
   end Create;

   function Value (Fld : Field;
                   Buf : Buffer_Number := Buffer_Number'First) return T
   is
   begin
      return T'Value (Get_Buffer (Fld, Buf));
   end Value;

end Terminal_Interface.Curses.Forms.Field_Types.Enumeration.Ada;
