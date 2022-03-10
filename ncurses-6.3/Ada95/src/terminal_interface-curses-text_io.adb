------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                     Terminal_Interface.Curses.Text_IO                    --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2011,2014 Free Software Foundation, Inc.                  --
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
package body Terminal_Interface.Curses.Text_IO is

   Default_Window : Window := Null_Window;

   procedure Set_Window (Win : Window)
   is
   begin
      Default_Window := Win;
   end Set_Window;

   function Get_Window return Window
   is
   begin
      if Default_Window = Null_Window then
         return Standard_Window;
      else
         return Default_Window;
      end if;
   end Get_Window;
   pragma Inline (Get_Window);

   procedure Flush (Win : Window)
   is
   begin
      Refresh (Win);
   end Flush;

   procedure Flush
   is
   begin
      Flush (Get_Window);
   end Flush;

   --------------------------------------------
   -- Specification of line and page lengths --
   --------------------------------------------

   --  There are no set routines in this package. I assume, that you allocate
   --  the window with an appropriate size.
   --  A scroll-window is interpreted as an page with unbounded page length,
   --  i.e. it returns the conventional 0 as page length.

   function Line_Length (Win : Window) return Count
   is
      N_Lines : Line_Count;
      N_Cols  : Column_Count;
   begin
      Get_Size (Win, N_Lines, N_Cols);
      --  if Natural (N_Cols) > Natural (Count'Last) then
      --     raise Layout_Error;
      --  end if;
      return Count (N_Cols);
   end Line_Length;

   function Line_Length return Count
   is
   begin
      return Line_Length (Get_Window);
   end Line_Length;

   function Page_Length (Win : Window) return Count
   is
      N_Lines : Line_Count;
      N_Cols  : Column_Count;
   begin
      if Scrolling_Allowed (Win) then
         return 0;
      else
         Get_Size (Win, N_Lines, N_Cols);
         --  if Natural (N_Lines) > Natural (Count'Last) then
         --     raise Layout_Error;
         --  end if;
         return Count (N_Lines);
      end if;
   end Page_Length;

   function Page_Length return Count
   is
   begin
      return Page_Length (Get_Window);
   end Page_Length;

   ------------------------------------
   -- Column, Line, and Page Control --
   ------------------------------------
   procedure New_Line (Win : Window; Spacing : Positive_Count := 1)
   is
      P_Size : constant Count := Page_Length (Win);
   begin
      if not Spacing'Valid then
         raise Constraint_Error;
      end if;

      for I in 1 .. Spacing loop
         if P_Size > 0 and then Line (Win) >= P_Size then
            New_Page (Win);
         else
            Add (Win, ASCII.LF);
         end if;
      end loop;
   end New_Line;

   procedure New_Line (Spacing : Positive_Count := 1)
   is
   begin
      New_Line (Get_Window, Spacing);
   end New_Line;

   procedure New_Page (Win : Window)
   is
   begin
      Clear (Win);
   end New_Page;

   procedure New_Page
   is
   begin
      New_Page (Get_Window);
   end New_Page;

   procedure Set_Col (Win : Window;  To : Positive_Count)
   is
      Y  : Line_Position;
      X1 : Column_Position;
      X2 : Column_Position;
      N  : Natural;
   begin
      if not To'Valid then
         raise Constraint_Error;
      end if;

      Get_Cursor_Position (Win, Y, X1);
      N  := Natural (To); N := N - 1;
      X2 := Column_Position (N);
      if X1 > X2 then
         New_Line (Win, 1);
         X1 := 0;
      end if;
      if X1 < X2 then
         declare
            Filler : constant String (Integer (X1) .. (Integer (X2) - 1))
              := (others => ' ');
         begin
            Put (Win, Filler);
         end;
      end if;
   end Set_Col;

   procedure Set_Col (To : Positive_Count)
   is
   begin
      Set_Col (Get_Window, To);
   end Set_Col;

   procedure Set_Line (Win : Window; To : Positive_Count)
   is
      Y1 : Line_Position;
      Y2 : Line_Position;
      X  : Column_Position;
      N  : Natural;
   begin
      if not To'Valid then
         raise Constraint_Error;
      end if;

      Get_Cursor_Position (Win, Y1, X);
      pragma Warnings (Off, X);         --  unreferenced
      N  := Natural (To); N := N - 1;
      Y2 := Line_Position (N);
      if Y2 < Y1 then
         New_Page (Win);
         Y1 := 0;
      end if;
      if Y1 < Y2 then
         New_Line (Win, Positive_Count (Y2 - Y1));
      end if;
   end Set_Line;

   procedure Set_Line (To : Positive_Count)
   is
   begin
      Set_Line (Get_Window, To);
   end Set_Line;

   function Col (Win : Window) return Positive_Count
   is
      Y : Line_Position;
      X : Column_Position;
      N : Natural;
   begin
      Get_Cursor_Position (Win, Y, X);
      N := Natural (X); N := N + 1;
      --  if N > Natural (Count'Last) then
      --     raise Layout_Error;
      --  end if;
      return Positive_Count (N);
   end Col;

   function Col return Positive_Count
   is
   begin
      return Col (Get_Window);
   end Col;

   function Line (Win : Window) return Positive_Count
   is
      Y : Line_Position;
      X : Column_Position;
      N : Natural;
   begin
      Get_Cursor_Position (Win, Y, X);
      N := Natural (Y); N := N + 1;
      --  if N > Natural (Count'Last) then
      --     raise Layout_Error;
      --  end if;
      return Positive_Count (N);
   end Line;

   function Line return Positive_Count
   is
   begin
      return Line (Get_Window);
   end Line;

   -----------------------
   -- Characters Output --
   -----------------------

   procedure Put (Win  : Window; Item : Character)
   is
      P_Size : constant Count := Page_Length (Win);
      Y : Line_Position;
      X : Column_Position;
      L : Line_Count;
      C : Column_Count;
   begin
      if P_Size > 0 then
         Get_Cursor_Position (Win, Y, X);
         Get_Size (Win, L, C);
         if (Y + 1) = L and then (X + 1) = C then
            New_Page (Win);
         end if;
      end if;
      Add (Win, Item);
   end Put;

   procedure Put (Item : Character)
   is
   begin
      Put (Get_Window, Item);
   end Put;

   --------------------
   -- Strings-Output --
   --------------------

   procedure Put (Win  : Window; Item : String)
   is
      P_Size : constant Count := Page_Length (Win);
      Y : Line_Position;
      X : Column_Position;
      L : Line_Count;
      C : Column_Count;
   begin
      if P_Size > 0 then
         Get_Cursor_Position (Win, Y, X);
         Get_Size (Win, L, C);
         if (Y + 1) = L and then (X + 1 + Item'Length) >= C then
            New_Page (Win);
         end if;
      end if;
      Add (Win, Item);
   end Put;

   procedure Put (Item : String)
   is
   begin
      Put (Get_Window, Item);
   end Put;

   procedure Put_Line
     (Win  : Window;
      Item : String)
   is
   begin
      Put (Win, Item);
      New_Line (Win, 1);
   end Put_Line;

   procedure Put_Line
     (Item : String)
   is
   begin
      Put_Line (Get_Window, Item);
   end Put_Line;

end Terminal_Interface.Curses.Text_IO;
