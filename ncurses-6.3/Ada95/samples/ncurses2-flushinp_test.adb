------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000 Free Software Foundation, Inc.                            --
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
--  $Revision: 1.2 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with ncurses2.util; use ncurses2.util;

procedure ncurses2.flushinp_test (win : Window) is

   procedure Continue (win : Window);

   procedure Continue (win : Window) is
   begin
      Set_Echo_Mode (False);
      Move_Cursor (win, 10, 1);
      Add (win, 10, 1, " Press any key to continue");
      Refresh (win);
      Getchar (win);
   end Continue;

   h, by, sh : Line_Position;
   w, bx, sw : Column_Position;

   subWin : Window;

begin
   Clear (win);
   Get_Size (win, h, w);
   Get_Window_Position (win, by, bx);
   sw := w / 3;
   sh := h / 3;
   subWin := Sub_Window (win, sh, sw, by + h - sh - 2, bx + w - sw - 2);

   if Has_Colors then
      Init_Pair (2, Cyan, Blue);
      Change_Background (subWin,
                         Attributed_Character'(Ch => ' ', Color => 2,
                                               Attr => Normal_Video));
   end if;

   Set_Character_Attributes (subWin,
                             (Bold_Character => True, others => False));
   Box (subWin);
   Add (subWin, 2, 1, "This is a subwindow");
   Refresh (win);

   Set_Cbreak_Mode (True);
   Add (win, 0, 1, "This is a test of the flushinp() call.");

   Add (win, 2, 1, "Type random keys for 5 seconds.");
   Add (win, 3, 1,
        "These should be discarded (not echoed) after the subwindow " &
        "goes away.");
   Refresh (win);

   for i in 0 .. 4 loop
      Move_Cursor (subWin, 1, 1);
      Add (subWin, Str => "Time = ");
      Add (subWin, Str => Integer'Image (i));
      Refresh (subWin);
      Nap_Milli_Seconds (1000);
      Flush_Input;
   end loop;

   Delete (subWin);
   Erase (win);
   Flash_Screen;
   Refresh (win);
   Nap_Milli_Seconds (1000);

   Add (win, 2, 1,
        Str => "If you were still typing when the window timer expired,");
   Add (win, 3, 1,
        "or else you typed nothing at all while it was running,");
   Add (win, 4, 1,
        "test was invalid.  You'll see garbage or nothing at all. ");
   Add (win, 6, 1, "Press a key");
   Move_Cursor (win, 9, 10);
   Refresh (win);
   Set_Echo_Mode (True);
   Getchar (win);
   Flush_Input;
   Add (win, 12, 0,
        "If you see any key other than what you typed, flushinp() is broken.");
   Continue (win);

   Move_Cursor (win, 9, 10);
   Delete_Character (win);
   Refresh (win);
   Move_Cursor (win, 12, 0);
   Clear_To_End_Of_Line;
   Add (win,
        "What you typed should now have been deleted; if not, wdelch() " &
        "failed.");
   Continue (win);

   Set_Cbreak_Mode (True);

end ncurses2.flushinp_test;
