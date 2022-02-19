------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                    Terminal_Interface.Curses.Terminfo                    --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2006,2009 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.7 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------

with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Ada.Unchecked_Conversion;

package body Terminal_Interface.Curses.Terminfo is

   function Is_MinusOne_Pointer (P : chars_ptr) return Boolean;

   function Is_MinusOne_Pointer (P : chars_ptr) return Boolean is
      type Weird_Address is new System.Storage_Elements.Integer_Address;
      Invalid_Pointer : constant Weird_Address := -1;
      function To_Weird is new Ada.Unchecked_Conversion
        (Source => chars_ptr, Target => Weird_Address);
   begin
      if To_Weird (P) = Invalid_Pointer then
         return True;
      else
         return False;
      end if;
   end Is_MinusOne_Pointer;
   pragma Inline (Is_MinusOne_Pointer);

------------------------------------------------------------------------------
   function Get_Flag (Name : String) return Boolean
   is
      function tigetflag (id : char_array) return Curses_Bool;
      pragma Import (C, tigetflag);
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
   begin
      To_C (Name, Txt, Length);
      if tigetflag (Txt) = Curses_Bool (Curses_True) then
         return True;
      else
         return False;
      end if;
   end Get_Flag;

------------------------------------------------------------------------------
   procedure Get_String (Name   : String;
                         Value  : out Terminfo_String;
                         Result : out Boolean)
   is
      function tigetstr (id : char_array) return chars_ptr;
      pragma Import (C, tigetstr, "tigetstr");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
      Txt2 : chars_ptr;
   begin
      To_C (Name, Txt, Length);
      Txt2 := tigetstr (Txt);
      if Txt2 = Null_Ptr then
         Result := False;
      elsif Is_MinusOne_Pointer (Txt2) then
         raise Curses_Exception;
      else
         Value  := Terminfo_String (Fill_String (Txt2));
         Result := True;
      end if;
   end Get_String;

------------------------------------------------------------------------------
   function Has_String (Name : String) return Boolean
   is
      function tigetstr (id : char_array) return chars_ptr;
      pragma Import (C, tigetstr, "tigetstr");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
      Txt2 : chars_ptr;
   begin
      To_C (Name, Txt, Length);
      Txt2 := tigetstr (Txt);
      if Txt2 = Null_Ptr then
         return False;
      elsif Is_MinusOne_Pointer (Txt2) then
         raise Curses_Exception;
      else
         return True;
      end if;
   end Has_String;

------------------------------------------------------------------------------
   function Get_Number (Name : String) return Integer is
      function tigetstr (s : char_array) return C_Int;
      pragma Import (C, tigetstr);
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
   begin
      To_C (Name, Txt, Length);
      return Integer (tigetstr (Txt));
   end Get_Number;

------------------------------------------------------------------------------
   procedure Put_String (Str    : Terminfo_String;
                         affcnt : Natural := 1;
                         putc   : putctype := null) is
      function tputs (str    : char_array;
                      affcnt : C_Int;
                      putc   : putctype) return C_Int;
      function putp (str : char_array) return C_Int;
      pragma Import (C, tputs);
      pragma Import (C, putp);
      Txt    : char_array (0 .. Str'Length);
      Length : size_t;
      Err : C_Int;
   begin
      To_C (String (Str), Txt, Length);
      if putc = null then
         Err := putp (Txt);
      else
         Err := tputs (Txt, C_Int (affcnt), putc);
      end if;
      if Err = Curses_Err then
         raise Curses_Exception;
      end if;
   end Put_String;

end Terminal_Interface.Curses.Terminfo;
