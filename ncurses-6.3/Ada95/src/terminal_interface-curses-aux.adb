------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Aux                       --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
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
--  $Revision: 1.12 $
--  Binding Version 01.00
------------------------------------------------------------------------------
package body Terminal_Interface.Curses.Aux is
   --
   --  Some helpers
   procedure Fill_String (Cp  : chars_ptr;
                          Str : out String)
   is
      --  Fill the string with the characters referenced by the
      --  chars_ptr.
      --
      Len : Natural;
   begin
      if Cp /= Null_Ptr then
         Len := Natural (Strlen (Cp));
         if Str'Length < Len then
            raise Constraint_Error;
         end if;
         declare
            S : String (1 .. Len);
         begin
            S := Value (Cp);
            Str (Str'First .. (Str'First + Len - 1)) := S (S'Range);
         end;
      else
         Len := 0;
      end if;

      if Len < Str'Length then
         Str ((Str'First + Len) .. Str'Last) := (others => ' ');
      end if;

   end Fill_String;

   function Fill_String (Cp : chars_ptr) return String
   is
      Len : Natural;
   begin
      if Cp /= Null_Ptr then
         Len := Natural (Strlen (Cp));
         if Len = 0 then
            return "";
         else
            declare
               S : String (1 .. Len);
            begin
               Fill_String (Cp, S);
               return S;
            end;
         end if;
      else
         return "";
      end if;
   end Fill_String;

   procedure Eti_Exception (Code : Eti_Error)
   is
   begin
      case Code is
         when E_Ok              => null;
         when E_System_Error    => raise Eti_System_Error;
         when E_Bad_Argument    => raise Eti_Bad_Argument;
         when E_Posted          => raise Eti_Posted;
         when E_Connected       => raise Eti_Connected;
         when E_Bad_State       => raise Eti_Bad_State;
         when E_No_Room         => raise Eti_No_Room;
         when E_Not_Posted      => raise Eti_Not_Posted;
         when E_Unknown_Command => raise Eti_Unknown_Command;
         when E_No_Match        => raise Eti_No_Match;
         when E_Not_Selectable  => raise Eti_Not_Selectable;
         when E_Not_Connected   => raise Eti_Not_Connected;
         when E_Request_Denied  => raise Eti_Request_Denied;
         when E_Invalid_Field   => raise Eti_Invalid_Field;
         when E_Current         => raise Eti_Current;
      end case;
   end Eti_Exception;

end Terminal_Interface.Curses.Aux;
