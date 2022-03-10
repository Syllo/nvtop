--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-panels-user_data__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Panels.User_Data               --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2003,2009 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.16 $
--  Binding Version 01.00
------------------------------------------------------------------------------

generic
   type User is limited private;
   type User_Access is access all User;
package Terminal_Interface.Curses.Panels.User_Data is
   pragma Preelaborate (Terminal_Interface.Curses.Panels.User_Data);

   --  MANPAGE(`panel.3x')

   --  ANCHOR(`set_panel_userptr',`Set_User_Data')
   procedure Set_User_Data (Pan  : Panel;
                            Data : User_Access);
   --  AKA
   pragma Inline (Set_User_Data);

   --  ANCHOR(`panel_userptr',`Get_User_Data')
   procedure Get_User_Data (Pan  : Panel;
                            Data : out User_Access);
   --  AKA

   --  ANCHOR(`panel_userptr',`Get_User_Data')
   function Get_User_Data (Pan  : Panel) return User_Access;
   --  AKA
   --  Same as function
   pragma Inline (Get_User_Data);

end Terminal_Interface.Curses.Panels.User_Data;
