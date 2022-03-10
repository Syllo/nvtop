------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
-- Copyright 2000-2009,2011 Free Software Foundation, Inc.                  --
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
--  Author: Eugene V. Melaragno <aldomel@ix.netcom.com> 2000
--  Version Control
--  $Revision: 1.11 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;

with Ada.Strings.Unbounded;
with Interfaces.C;
with Terminal_Interface.Curses.Aux;

procedure ncurses2.slk_test is
   procedure myGet (Win : Window := Standard_Window;
                    Str : out Ada.Strings.Unbounded.Unbounded_String;
                    Len : Integer := -1);

   procedure myGet (Win : Window := Standard_Window;
                    Str : out Ada.Strings.Unbounded.Unbounded_String;
                    Len : Integer := -1)
   is
      use Ada.Strings.Unbounded;
      use Interfaces.C;
      use Terminal_Interface.Curses.Aux;

      function Wgetnstr (Win : Window;
                         Str : char_array;
                         Len : int) return int;
      pragma Import (C, Wgetnstr, "wgetnstr");

      --  FIXME: how to construct "(Len > 0) ? Len : 80"?
      Ask : constant Interfaces.C.size_t := Interfaces.C.size_t'Val (Len + 80);
      Txt : char_array (0 .. Ask);

   begin
      Txt (0) := Interfaces.C.char'First;
      if Wgetnstr (Win, Txt, Txt'Length) = Curses_Err then
         raise Curses_Exception;
      end if;
      Str := To_Unbounded_String (To_Ada (Txt, True));
   end myGet;

   use Ada.Strings.Unbounded;

   c : Key_Code;
   buf : Unbounded_String;
   c2 : Character;
   fmt : Label_Justification := Centered;
   tmp : Integer;

begin
   c := CTRL ('l');
   loop
      Move_Cursor (Line => 0, Column => 0);
      c2 := Code_To_Char (c);
      case c2 is
         when Character'Val (Character'Pos ('l') mod 16#20#) => --  CTRL('l')
            Erase;
            Switch_Character_Attribute (Attr => (Bold_Character => True,
                                                 others => False));
            Add (Line => 0, Column => 20,
                 Str => "Soft Key Exerciser");
            Switch_Character_Attribute (On => False,
                                        Attr => (Bold_Character => True,
                                                 others => False));

            Move_Cursor (Line => 2, Column => 0);
            P ("Available commands are:");
            P ("");
            P ("^L         -- refresh screen");
            P ("a          -- activate or restore soft keys");
            P ("d          -- disable soft keys");
            P ("c          -- set centered format for labels");
            P ("l          -- set left-justified format for labels");
            P ("r          -- set right-justified format for labels");
            P ("[12345678] -- set label; labels are numbered 1 through 8");
            P ("e          -- erase stdscr (should not erase labels)");
            P ("s          -- test scrolling of shortened screen");
            P ("x, q       -- return to main menu");
            P ("");
            P ("Note: if activating the soft keys causes your terminal to");
            P ("scroll up one line, your terminal auto-scrolls when anything");
            P ("is written to the last screen position.  The ncurses code");
            P ("does not yet handle this gracefully.");
            Refresh;
            Restore_Soft_Label_Keys;

         when 'a' =>
            Restore_Soft_Label_Keys;
         when 'e' =>
            Clear;
         when 's' =>
            Add (Line => 20, Column => 0,
                Str => "Press Q to stop the scrolling-test: ");
            loop
               c := Getchar;
               c2 := Code_To_Char (c);
               exit when c2 = 'Q';
               --  c = ERR?
               --  TODO when c is not a character (arrow key)
               --  the behavior is different from the C version.
               Add (Ch => c2);
            end loop;
         when 'd' =>
            Clear_Soft_Label_Keys;
         when 'l' =>
            fmt := Left;
         when 'c' =>
            fmt := Centered;
         when 'r' =>
            fmt := Right;
         when '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8'  =>
            Add (Line => 20, Column => 0,
                 Str => "Please enter the label value: ");
            Set_Echo_Mode (SwitchOn => True);
            myGet (Str => buf);
            Set_Echo_Mode (SwitchOn => False);
            tmp := ctoi (c2);
            Set_Soft_Label_Key (Label_Number (tmp), To_String (buf), fmt);
            Refresh_Soft_Label_Keys;
            Move_Cursor (Line => 20, Column => 0);
            Clear_To_End_Of_Line;
         when 'x' | 'q' =>
            exit;
            --  the C version needed a goto, ha ha
            --  breaks exit the case not the loop because fall-through
            --  happens in C!
         when others =>
            Beep;
      end case;
      c := Getchar;
      --  TODO exit when c = EOF
   end loop;
   Erase;
   End_Windows;
end ncurses2.slk_test;
