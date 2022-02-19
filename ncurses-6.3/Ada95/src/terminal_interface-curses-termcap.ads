------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Termcap                    --
--                                                                          --
--                                 S P E C                                  --
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
------------------------------------------------------------------------------

package Terminal_Interface.Curses.Termcap is
   pragma Preelaborate (Terminal_Interface.Curses.Termcap);

   --  |=====================================================================
   --  | Man page curs_termcap.3x
   --  |=====================================================================
   --  Not implemented:  tputs (see curs_terminfo)

   type Termcap_String is new String;

   --  |
   function TGoto (Cap : String;
                   Col : Column_Position;
                   Row : Line_Position) return Termcap_String;
   --  AKA: tgoto()

   --  |
   function Get_Entry (Name : String) return Boolean;
   --  AKA: tgetent()

   --  |
   function Get_Flag (Name : String) return Boolean;
   --  AKA: tgetflag()

   --  |
   procedure Get_Number (Name   : String;
                         Value  : out Integer;
                         Result : out Boolean);
   --  AKA: tgetnum()

   --  |
   procedure Get_String (Name   : String;
                         Value  : out String;
                         Result : out Boolean);
   function Get_String (Name : String) return Boolean;
   --  Returns True if the string is found.
   --  AKA: tgetstr()

end Terminal_Interface.Curses.Termcap;
