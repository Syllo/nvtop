------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Form_Demo.Aux                          --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2009,2011 Free Software Foundation, Inc.                  --
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
--  Version Control
--  $Revision: 1.12 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use  Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use  Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Forms;  use  Terminal_Interface.Curses.Forms;

package Sample.Form_Demo.Aux is

   procedure Geometry (F  : Form;
                       L  : out Line_Count;
                       C  : out Column_Count;
                       Y  : out Line_Position;
                       X  : out Column_Position);
   --  Calculate the geometry for a panel being able to be used to display
   --  the menu.

   function Create (F     : Form;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel;
   --  Create a panel decorated with a frame and the title at the specified
   --  position. The dimension of the panel is derived from the menus layout.

   procedure Destroy (F : Form;
                      P : in out Panel);
   --  Destroy all the windowing structures associated with this menu and
   --  panel.

   function Get_Request (F           : Form;
                         P           : Panel;
                         Handle_CRLF : Boolean := True) return Key_Code;
   --  Centralized request driver for all menus in this sample. This
   --  gives us a common key binding for all menus.

   function Make (Top         : Line_Position;
                  Left        : Column_Position;
                  Text        : String) return Field;
   --  create a label

   function Make  (Height      : Line_Count := 1;
                   Width       : Column_Count;
                   Top         : Line_Position;
                   Left        : Column_Position;
                   Off_Screen  : Natural := 0) return Field;
   --  create a editable field

   function Default_Driver (F : Form;
                            K : Key_Code;
                            P : Panel) return Boolean;

   function Count_Active (F : Form) return Natural;
   --  Count the number of active fields in the form

end Sample.Form_Demo.Aux;
