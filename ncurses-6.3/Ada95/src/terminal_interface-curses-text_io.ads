------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Text_IO                    --
--                                                                          --
--                                 S P E C                                  --
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
--  $Revision: 1.15 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Text_IO;
with Ada.IO_Exceptions;

package Terminal_Interface.Curses.Text_IO is

   use type Ada.Text_IO.Count;
   subtype Count is Ada.Text_IO.Count;
   subtype Positive_Count is Count range 1 .. Count'Last;

   subtype Field is Ada.Text_IO.Field;
   subtype Number_Base is Integer range 2 .. 16;

   type Type_Set is (Lower_Case, Upper_Case, Mixed_Case);

   --  For most of the routines you will see a version without a Window
   --  type parameter. They will operate on a default window, which can
   --  be set by the user. It is initially equal to Standard_Window.

   procedure Set_Window (Win : Window);
   --  Set Win as the default window

   function Get_Window return Window;
   --  Get the current default window

   procedure Flush (Win : Window);
   procedure Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   --  There are no set routines in this package. I assume, that you allocate
   --  the window with an appropriate size.
   --  A scroll-window is interpreted as an page with unbounded page length,
   --  i.e. it returns the conventional 0 as page length.

   function Line_Length (Win : Window) return Count;
   function Line_Length return Count;

   function Page_Length (Win : Window) return Count;
   function Page_Length return Count;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------
   procedure New_Line (Win : Window; Spacing : Positive_Count := 1);
   procedure New_Line (Spacing : Positive_Count := 1);

   procedure New_Page (Win : Window);
   procedure New_Page;

   procedure Set_Col (Win : Window;  To : Positive_Count);
   procedure Set_Col (To : Positive_Count);

   procedure Set_Line (Win : Window; To : Positive_Count);
   procedure Set_Line (To : Positive_Count);

   function Col (Win : Window) return Positive_Count;
   function Col return Positive_Count;

   function Line (Win : Window) return Positive_Count;
   function Line return Positive_Count;

   -----------------------
   -- Characters-Output --
   -----------------------

   procedure Put (Win  : Window; Item : Character);
   procedure Put (Item : Character);

   --------------------
   -- Strings-Output --
   --------------------

   procedure Put (Win  : Window; Item : String);
   procedure Put (Item : String);

   procedure Put_Line
     (Win  : Window;
      Item : String);

   procedure Put_Line
     (Item : String);

   --  Exceptions

   Status_Error : exception renames Ada.IO_Exceptions.Status_Error;
   Mode_Error   : exception renames Ada.IO_Exceptions.Mode_Error;
   Name_Error   : exception renames Ada.IO_Exceptions.Name_Error;
   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   Device_Error : exception renames Ada.IO_Exceptions.Device_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;
   Data_Error   : exception renames Ada.IO_Exceptions.Data_Error;
   Layout_Error : exception renames Ada.IO_Exceptions.Layout_Error;

end Terminal_Interface.Curses.Text_IO;
