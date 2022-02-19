--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-panels__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Panels                    --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2009,2014 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.23 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System;

package Terminal_Interface.Curses.Panels is
   pragma Preelaborate (Terminal_Interface.Curses.Panels);
   pragma Linker_Options ("-lpanel" & Curses_Constants.DFT_ARG_SUFFIX);

   type Panel is private;

   ---------------------------
   --  Interface constants  --
   ---------------------------
   Null_Panel : constant Panel;

   -------------------
   --  Exceptions   --
   -------------------

   Panel_Exception : exception;

   --  MANPAGE(`panel.3x')

   --  ANCHOR(`new_panel()',`Create')
   function Create (Win : Window) return Panel;
   --  AKA
   pragma Inline (Create);

   --  ANCHOR(`new_panel()',`New_Panel')
   function New_Panel (Win : Window) return Panel renames Create;
   --  AKA
   --  pragma Inline (New_Panel);

   --  ANCHOR(`bottom_panel()',`Bottom')
   procedure Bottom (Pan : Panel);
   --  AKA
   pragma Inline (Bottom);

   --  ANCHOR(`top_panel()',`Top')
   procedure Top (Pan : Panel);
   --  AKA
   pragma Inline (Top);

   --  ANCHOR(`show_panel()',`Show')
   procedure Show (Pan : Panel);
   --  AKA
   pragma Inline (Show);

   --  ANCHOR(`update_panels()',`Update_Panels')
   procedure Update_Panels;
   --  AKA
   pragma Import (C, Update_Panels, "update_panels");

   --  ANCHOR(`hide_panel()',`Hide')
   procedure Hide (Pan : Panel);
   --  AKA
   pragma Inline (Hide);

   --  ANCHOR(`panel_window()',`Get_Window')
   function Get_Window (Pan : Panel) return Window;
   --  AKA
   pragma Inline (Get_Window);

   --  ANCHOR(`panel_window()',`Panel_Window')
   function Panel_Window (Pan : Panel) return Window renames Get_Window;
   --  pragma Inline (Panel_Window);

   --  ANCHOR(`replace_panel()',`Replace')
   procedure Replace (Pan : Panel;
                      Win : Window);
   --  AKA
   pragma Inline (Replace);

   --  ANCHOR(`move_panel()',`Move')
   procedure Move (Pan    : Panel;
                   Line   : Line_Position;
                   Column : Column_Position);
   --  AKA
   pragma Inline (Move);

   --  ANCHOR(`panel_hidden()',`Is_Hidden')
   function Is_Hidden (Pan : Panel) return Boolean;
   --  AKA
   pragma Inline (Is_Hidden);

   --  ANCHOR(`panel_above()',`Above')
   function Above (Pan : Panel) return Panel;
   --  AKA
   pragma Import (C, Above, "panel_above");

   --  ANCHOR(`panel_below()',`Below')
   function Below (Pan : Panel) return Panel;
   --  AKA
   pragma Import (C, Below, "panel_below");

   --  ANCHOR(`del_panel()',`Delete')
   procedure Delete (Pan : in out Panel);
   --  AKA
   pragma Inline (Delete);

private
      type Panel is new System.Storage_Elements.Integer_Address;
      Null_Panel : constant Panel := 0;

end Terminal_Interface.Curses.Panels;
