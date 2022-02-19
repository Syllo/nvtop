------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                    Terminal_Interface.Curses.Termcap                     --
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
--  $Revision: 1.13 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------

with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body Terminal_Interface.Curses.Termcap is

   function Get_Entry (Name : String) return Boolean
   is
      function tgetent (name : char_array; val : char_array)
                        return C_Int;
      pragma Import (C, tgetent, "tgetent");
      NameTxt : char_array (0 .. Name'Length);
      Length  : size_t;
      ignored : constant char_array (0 .. 0) := (0 => nul);
      result  : C_Int;
   begin
      To_C (Name, NameTxt, Length);
      result := tgetent (char_array (ignored), NameTxt);
      if result = -1 then
         raise Curses_Exception;
      else
         return Boolean'Val (result);
      end if;
   end Get_Entry;

------------------------------------------------------------------------------
   function Get_Flag (Name : String) return Boolean
   is
      function tgetflag (id : char_array) return C_Int;
      pragma Import (C, tgetflag, "tgetflag");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
   begin
      To_C (Name, Txt, Length);
      if tgetflag (Txt) = 0 then
         return False;
      else
         return True;
      end if;
   end Get_Flag;

------------------------------------------------------------------------------
   procedure Get_Number (Name   : String;
                         Value  : out Integer;
                         Result : out Boolean)
   is
      function tgetnum (id : char_array) return C_Int;
      pragma Import (C, tgetnum, "tgetnum");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
   begin
      To_C (Name, Txt, Length);
      Value := Integer (tgetnum (Txt));
      if Value = -1 then
         Result := False;
      else
         Result :=  True;
      end if;
   end Get_Number;

------------------------------------------------------------------------------
   procedure Get_String (Name   : String;
                         Value  : out String;
                         Result : out Boolean)
   is
      function tgetstr (id  : char_array;
                        buf : char_array) return chars_ptr;
      pragma Import (C, tgetstr, "tgetstr");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
      Txt2   : chars_ptr;
      type t is new char_array (0 .. 1024); --  does it need to be 1024?
      Return_Buffer : constant t := (others => nul);
   begin
      To_C (Name, Txt, Length);
      Txt2 := tgetstr (Txt, char_array (Return_Buffer));
      if Txt2 = Null_Ptr then
         Result := False;
      else
         Value := Fill_String (Txt2);
         Result := True;
      end if;
   end Get_String;

   function Get_String (Name : String) return Boolean
   is
      function tgetstr (Id  : char_array;
                        buf : char_array) return chars_ptr;
      pragma Import (C, tgetstr, "tgetstr");
      Txt    : char_array (0 .. Name'Length);
      Length : size_t;
      Txt2   : chars_ptr;
      type t is new char_array (0 .. 1024); --  does it need to be 1024?
      Phony_Txt : constant t := (others => nul);
   begin
      To_C (Name, Txt, Length);
      Txt2 := tgetstr (Txt, char_array (Phony_Txt));
      if Txt2 = Null_Ptr then
         return False;
      else
         return True;
      end if;
   end Get_String;

------------------------------------------------------------------------------
   function TGoto (Cap : String;
                   Col : Column_Position;
                   Row : Line_Position) return Termcap_String is
      function tgoto (cap : char_array;
                      col : C_Int;
                      row : C_Int) return chars_ptr;
      pragma Import (C, tgoto);
      Txt    : char_array (0 .. Cap'Length);
      Length : size_t;
   begin
      To_C (Cap, Txt, Length);
      return Termcap_String (Fill_String
                             (tgoto (Txt, C_Int (Col), C_Int (Row))));
   end TGoto;

end Terminal_Interface.Curses.Termcap;
