------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Panels                    --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2004,2009 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.15 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses.Aux; use Terminal_Interface.Curses.Aux;
with Interfaces.C;

package body Terminal_Interface.Curses.Panels is

   use type Interfaces.C.int;

   function Create (Win : Window) return Panel
   is
      function Newpanel (Win : Window) return Panel;
      pragma Import (C, Newpanel, "new_panel");

      Pan : Panel;
   begin
      Pan := Newpanel (Win);
      if Pan = Null_Panel then
         raise Panel_Exception;
      end if;
      return Pan;
   end Create;

   procedure Bottom (Pan : Panel)
   is
      function Bottompanel (Pan : Panel) return C_Int;
      pragma Import (C, Bottompanel, "bottom_panel");
   begin
      if Bottompanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Bottom;

   procedure Top (Pan : Panel)
   is
      function Toppanel (Pan : Panel) return C_Int;
      pragma Import (C, Toppanel, "top_panel");
   begin
      if Toppanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Top;

   procedure Show (Pan : Panel)
   is
      function Showpanel (Pan : Panel) return C_Int;
      pragma Import (C, Showpanel, "show_panel");
   begin
      if Showpanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Show;

   procedure Hide (Pan : Panel)
   is
      function Hidepanel (Pan : Panel) return C_Int;
      pragma Import (C, Hidepanel, "hide_panel");
   begin
      if Hidepanel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Hide;

   function Get_Window (Pan : Panel) return Window
   is
      function Panel_Win (Pan : Panel) return Window;
      pragma Import (C, Panel_Win, "panel_window");

      Win : constant Window := Panel_Win (Pan);
   begin
      if Win = Null_Window then
         raise Panel_Exception;
      end if;
      return Win;
   end Get_Window;

   procedure Replace (Pan : Panel;
                      Win : Window)
   is
      function Replace_Pan (Pan : Panel;
                            Win : Window) return C_Int;
      pragma Import (C, Replace_Pan, "replace_panel");
   begin
      if Replace_Pan (Pan, Win) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Replace;

   procedure Move (Pan    : Panel;
                   Line   : Line_Position;
                   Column : Column_Position)
   is
      function Move (Pan    : Panel;
                     Line   : C_Int;
                     Column : C_Int) return C_Int;
      pragma Import (C, Move, "move_panel");
   begin
      if Move (Pan, C_Int (Line), C_Int (Column)) = Curses_Err then
         raise Panel_Exception;
      end if;
   end Move;

   function Is_Hidden (Pan : Panel) return Boolean
   is
      function Panel_Hidden (Pan : Panel) return C_Int;
      pragma Import (C, Panel_Hidden, "panel_hidden");
   begin
      if Panel_Hidden (Pan) = Curses_False then
         return False;
      else
         return True;
      end if;
   end Is_Hidden;

   procedure Delete (Pan : in out Panel)
   is
      function Del_Panel (Pan : Panel) return C_Int;
      pragma Import (C, Del_Panel, "del_panel");
   begin
      if Del_Panel (Pan) = Curses_Err then
         raise Panel_Exception;
      end if;
      Pan := Null_Panel;
   end Delete;

end Terminal_Interface.Curses.Panels;
