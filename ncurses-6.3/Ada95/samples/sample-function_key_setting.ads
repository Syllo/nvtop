------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                         Sample.Function_Key_Setting                      --
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
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;

--  This package implements a simple stack of function key label environments.
--
package Sample.Function_Key_Setting is

   procedure Push_Environment (Key   : String;
                               Reset : Boolean := True);
   --  Push the definition of the current function keys on an internal
   --  stack. If the reset flag is true, all labels are reset while
   --  pushed, so the new environment can assume a tabula rasa.
   --  The Key defines the new Help Context associated with the new
   --  Environment. This saves also the currently active Notepad.

   procedure Pop_Environment;
   --  Pop the Definitions from the stack and make them the current ones.
   --  This also restores the Help context and the previous Notepad.

   procedure Initialize (Mode : Soft_Label_Key_Format := PC_Style;
                         Just : Label_Justification := Left);
   --  Initialize the environment

   function Context return String;
   --  Return the current context identifier

   function Find_Context (Key : String) return Boolean;
   --  Look for a context, return true if it is in the stack,
   --  false otherwise.

   procedure Notepad_To_Context (Pan : Panel);
   --  Add a panel representing a notepad to the current context.

   Function_Key_Stack_Error : exception;

   procedure Default_Labels;
   --  Set the default labels used in all environments

   function Notepad_Window return Window;
   --  Return the current notepad window or Null_Window if there is none.

end Sample.Function_Key_Setting;
