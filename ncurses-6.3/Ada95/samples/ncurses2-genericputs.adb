------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2008,2009 Free Software Foundation, Inc.                  --
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
--  Author: Eugene V. Melaragno <aldomel@ix.netcom.com> 2000
--  Version Control
--  $Revision: 1.5 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;

package body ncurses2.genericPuts is

   procedure myGet (Win : Window := Standard_Window;
                    Str : out BS.Bounded_String;
                    Len : Integer := -1)
   is
      function Wgetnstr (Win : Window;
                         Str : char_array;
                         Len : int) return int;
      pragma Import (C, Wgetnstr, "wgetnstr");

      N        : Integer := Len;
      Txt : char_array (0 .. size_t (Max_Length));
      xStr : String (1 .. Max_Length);
      Cnt : Natural;
   begin
      if N < 0 then
         N := Max_Length;
      end if;
      if N > Max_Length then
         raise Constraint_Error;
      end if;
      Txt (0) := Interfaces.C.char'First;
      if Wgetnstr (Win, Txt, C_Int (N)) = Curses_Err then
         raise Curses_Exception;
      end if;
      To_Ada (Txt, xStr, Cnt, True);
      Str := To_Bounded_String (xStr (1 .. Cnt));
   end myGet;

   procedure myPut (Str  : out BS.Bounded_String;
                    i    : Integer;
                    Base : Number_Base := 10) is
      package Int_IO is new Integer_IO (Integer); use Int_IO;
      tmp : String (1 .. BS.Max_Length);
   begin
      Put (tmp, i, Base);
      Str := To_Bounded_String (tmp);
      Trim (Str, Ada.Strings.Trim_End'(Ada.Strings.Left));
   end myPut;

   procedure myAdd (Str : BS.Bounded_String) is
   begin
      Add (Str => To_String (Str));
   end myAdd;

   --  from ncurses-aux
   procedure Fill_String (Cp  : chars_ptr;
                          Str : out BS.Bounded_String)
   is
      --  Fill the string with the characters referenced by the
      --  chars_ptr.
      --
      Len : Natural;
   begin
      if Cp /= Null_Ptr then
         Len := Natural (Strlen (Cp));
         if Max_Length < Len then
            raise Constraint_Error;
         end if;
         declare
            S : String (1 .. Len);
         begin
            S := Value (Cp);
            Str := To_Bounded_String (S);
         end;
      else
         Str := Null_Bounded_String;
      end if;

   end Fill_String;

end ncurses2.genericPuts;
