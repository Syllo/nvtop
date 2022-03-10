------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                    Terminal_Interface.Curses.PutWin                      --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2002,2003 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.5 $
--  Binding Version 01.00

with Ada.Streams.Stream_IO.C_Streams;
with Interfaces.C_Streams;
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;

package body Terminal_Interface.Curses.PutWin is

   package ICS renames Interfaces.C_Streams;
   package ACS renames Ada.Streams.Stream_IO.C_Streams;
   use type C_Int;

   procedure Put_Window (Win  : Window;
                         File : Ada.Streams.Stream_IO.File_Type) is
      function putwin (Win : Window; f : ICS.FILEs) return C_Int;
      pragma Import (C, putwin, "putwin");

      R : constant C_Int := putwin (Win, ACS.C_Stream (File));
   begin
      if R /= Curses_Ok then
         raise Curses_Exception;
      end if;
   end Put_Window;

   function Get_Window (File : Ada.Streams.Stream_IO.File_Type)
                        return Window is
      function getwin (f : ICS.FILEs) return Window;
      pragma Import (C, getwin, "getwin");

      W : constant Window := getwin (ACS.C_Stream (File));
   begin
      if W = Null_Window then
         raise Curses_Exception;
      else
         return W;
      end if;
   end Get_Window;

end Terminal_Interface.Curses.PutWin;
