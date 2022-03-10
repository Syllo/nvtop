------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                 Terminal_Interface.Curses.Panels.User_Data               --
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
--  $Revision: 1.13 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Interfaces.C;
with Terminal_Interface.Curses.Aux;
use  Terminal_Interface.Curses.Aux;
with Terminal_Interface.Curses.Panels;
use  Terminal_Interface.Curses.Panels;

package body Terminal_Interface.Curses.Panels.User_Data is

   use type Interfaces.C.int;

   procedure Set_User_Data (Pan  : Panel;
                            Data : User_Access)
   is
      function Set_Panel_Userptr (Pan  : Panel;
                                  Addr : User_Access) return C_Int;
      pragma Import (C, Set_Panel_Userptr, "set_panel_userptr");
   begin
      if Set_Panel_Userptr (Pan, Data) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Set_User_Data;

   function Get_User_Data (Pan  : Panel) return User_Access
   is
      function Panel_Userptr (Pan : Panel) return User_Access;
      pragma Import (C, Panel_Userptr, "panel_userptr");
   begin
      return Panel_Userptr (Pan);
   end Get_User_Data;

   procedure Get_User_Data (Pan  : Panel;
                            Data : out User_Access)
   is
   begin
      Data := Get_User_Data (Pan);
   end Get_User_Data;

end Terminal_Interface.Curses.Panels.User_Data;
