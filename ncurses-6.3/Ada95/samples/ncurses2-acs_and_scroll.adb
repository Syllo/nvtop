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
--  $Revision: 1.13 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
--  Windows and scrolling tester.
--  Demonstrate windows

with Ada.Strings.Fixed;
with Ada.Strings;

with ncurses2.util; use ncurses2.util;
with ncurses2.genericPuts;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Mouse; use Terminal_Interface.Curses.Mouse;
with Terminal_Interface.Curses.PutWin; use Terminal_Interface.Curses.PutWin;

with Ada.Streams.Stream_IO; use Ada.Streams.Stream_IO;
with Ada.Streams; use Ada.Streams;

procedure ncurses2.acs_and_scroll is

   Macro_Quit   : constant Key_Code := Character'Pos ('Q') mod 16#20#;
   Macro_Escape : constant Key_Code := Character'Pos ('[') mod 16#20#;

   Quit : constant Key_Code := CTRL ('Q');
   Escape : constant Key_Code := CTRL ('[');

   Botlines : constant Line_Position := 4;

   type pair is record
      y : Line_Position;
      x : Column_Position;
   end record;

   type Frame;
   type FrameA is access Frame;

   f : File_Type;
   dumpfile : constant String := "screendump";

   procedure Outerbox (ul, lr : pair; onoff : Boolean);
   function  HaveKeyPad (w : Window) return Boolean;
   function  HaveScroll (w : Window) return Boolean;
   procedure newwin_legend (curpw : Window);
   procedure transient (curpw : Window; msg : String);
   procedure newwin_report (win : Window := Standard_Window);
   procedure selectcell (uli : Line_Position;
                         ulj : Column_Position;
                         lri : Line_Position;
                         lrj : Column_Position;
                         p   : out pair;
                         b   : out Boolean);
   function  getwindow return Window;
   procedure newwin_move (win : Window;
                          dy  : Line_Position;
                          dx  : Column_Position);
   function delete_framed (fp : FrameA; showit : Boolean) return FrameA;

   --  A linked list
   --  I  wish there was a standard library linked list. Oh well.
   type Frame is record
      next, last : FrameA;
      do_scroll : Boolean;
      do_keypad : Boolean;
      wind : Window;
   end record;

   current : FrameA;

   c : Key_Code;

   procedure Outerbox (ul, lr : pair; onoff : Boolean) is
   begin
      if onoff then
         --  Note the fix of an obscure bug
         --  try making a 1x1 box then enlarging it, the is a blank
         --  upper left corner!
         Add (Line => ul.y - 1, Column => ul.x - 1,
             Ch => ACS_Map (ACS_Upper_Left_Corner));
         Add (Line => ul.y - 1, Column => lr.x + 1,
             Ch => ACS_Map (ACS_Upper_Right_Corner));
         Add (Line => lr.y + 1, Column => lr.x + 1,
             Ch => ACS_Map (ACS_Lower_Right_Corner));
         Add (Line => lr.y + 1, Column => ul.x - 1,
             Ch => ACS_Map (ACS_Lower_Left_Corner));

         Move_Cursor (Line => ul.y - 1, Column => ul.x);
         Horizontal_Line (Line_Symbol => ACS_Map (ACS_Horizontal_Line),
                          Line_Size => Integer (lr.x - ul.x) + 1);
         Move_Cursor (Line => ul.y, Column => ul.x - 1);
         Vertical_Line (Line_Symbol => ACS_Map (ACS_Vertical_Line),
                        Line_Size => Integer (lr.y - ul.y) + 1);
         Move_Cursor (Line => lr.y + 1, Column => ul.x);
         Horizontal_Line (Line_Symbol => ACS_Map (ACS_Horizontal_Line),
                          Line_Size => Integer (lr.x - ul.x) + 1);
         Move_Cursor (Line => ul.y, Column => lr.x + 1);
         Vertical_Line (Line_Symbol => ACS_Map (ACS_Vertical_Line),
                        Line_Size => Integer (lr.y - ul.y) + 1);
      else
         Add (Line => ul.y - 1, Column => ul.x - 1, Ch => ' ');
         Add (Line => ul.y - 1, Column => lr.x + 1, Ch => ' ');
         Add (Line => lr.y + 1, Column => lr.x + 1, Ch => ' ');
         Add (Line => lr.y + 1, Column => ul.x - 1, Ch => ' ');

         Move_Cursor (Line => ul.y - 1, Column => ul.x);
         Horizontal_Line (Line_Symbol => Blank2,
                          Line_Size => Integer (lr.x - ul.x) + 1);
         Move_Cursor (Line => ul.y, Column => ul.x - 1);
         Vertical_Line (Line_Symbol => Blank2,
                        Line_Size => Integer (lr.y - ul.y) + 1);
         Move_Cursor (Line => lr.y + 1, Column => ul.x);
         Horizontal_Line (Line_Symbol => Blank2,
                          Line_Size => Integer (lr.x - ul.x) + 1);
         Move_Cursor (Line => ul.y, Column => lr.x + 1);
         Vertical_Line (Line_Symbol => Blank2,
                        Line_Size => Integer (lr.y - ul.y) + 1);
      end if;
   end Outerbox;

   function HaveKeyPad (w : Window) return Boolean is
   begin
      return Get_KeyPad_Mode (w);
   exception
      when Curses_Exception => return False;
   end HaveKeyPad;

   function HaveScroll (w : Window) return Boolean is
   begin
      return Scrolling_Allowed (w);
   exception
      when Curses_Exception => return False;
   end HaveScroll;

   procedure newwin_legend (curpw : Window) is

      package p is new genericPuts (200);
      use p;
      use p.BS;

      type string_a is access String;

      type rrr is record
         msg : string_a;
         code : Integer range 0 .. 3;
      end record;

      legend : constant array (Positive range <>) of rrr :=
        (
         (
          new String'("^C = create window"), 0
          ),
         (
          new String'("^N = next window"), 0
          ),
         (
          new String'("^P = previous window"), 0
          ),
         (
          new String'("^F = scroll forward"), 0
          ),
         (
          new String'("^B = scroll backward"), 0
          ),
         (
          new String'("^K = keypad(%s)"), 1
          ),
         (
          new String'("^S = scrollok(%s)"), 2
          ),
         (
          new String'("^W = save window to file"), 0
          ),
         (
          new String'("^R = restore window"), 0
          ),
         (
          new String'("^X = resize"), 0
          ),
         (
          new String'("^Q%s = exit"), 3
          )
         );

      buf : Bounded_String;
      do_keypad : constant Boolean := HaveKeyPad (curpw);
      do_scroll : constant Boolean := HaveScroll (curpw);

      pos : Natural;

      mypair : pair;

   begin
      Move_Cursor (Line => Lines - 4, Column => 0);
      for n in legend'Range loop
         pos := Ada.Strings.Fixed.Index (Source => legend (n).msg.all,
                                         Pattern => "%s");
         buf := To_Bounded_String (legend (n).msg.all);
         case legend (n).code is
            when 0 => null;
            when 1 =>
               if do_keypad then
                  Replace_Slice (buf, pos, pos + 1, "yes");
               else
                  Replace_Slice (buf, pos, pos + 1, "no");
               end if;
            when 2 =>
               if do_scroll then
                  Replace_Slice (buf, pos, pos + 1, "yes");
               else
                  Replace_Slice (buf, pos, pos + 1, "no");
               end if;
            when 3 =>
               if do_keypad then
                  Replace_Slice (buf, pos, pos + 1, "/ESC");
               else
                  Replace_Slice (buf, pos, pos + 1, "");
               end if;
         end case;
         Get_Cursor_Position (Line => mypair.y, Column => mypair.x);
         if Columns < mypair.x + 3 + Column_Position (Length (buf)) then
            Add (Ch => newl);
         elsif n /= 1 then -- n /= legen'First
            Add (Str => ", ");
         end if;
         myAdd (Str => buf);
      end loop;
      Clear_To_End_Of_Line;
   end newwin_legend;

   procedure transient (curpw : Window; msg : String) is
   begin
      newwin_legend (curpw);
      if msg /= "" then
         Add (Line => Lines - 1, Column => 0, Str => msg);
         Refresh;
         Nap_Milli_Seconds (1000);
      end if;

      Move_Cursor (Line => Lines - 1, Column => 0);

      if HaveKeyPad (curpw) then
         Add (Str => "Non-arrow");
      else
         Add (Str => "All other");
      end if;
      Add (Str => " characters are echoed, window should ");
      if not HaveScroll (curpw) then
         Add (Str => "not ");
      end if;
      Add (Str => "scroll");

      Clear_To_End_Of_Line;
   end transient;

   procedure newwin_report (win : Window := Standard_Window) is
      y : Line_Position;
      x : Column_Position;
      use Int_IO;
      tmp2a : String (1 .. 2);
      tmp2b : String (1 .. 2);
   begin
      if win /= Standard_Window then
         transient (win, "");
      end if;
      Get_Cursor_Position (win, y, x);
      Move_Cursor (Line => Lines - 1, Column => Columns - 17);
      Put (tmp2a, Integer (y));
      Put (tmp2b, Integer (x));
      Add (Str => "Y = " & tmp2a & " X = " & tmp2b);
      if win /= Standard_Window then
         Refresh;
      else
         Move_Cursor (win, y, x);
      end if;
   end newwin_report;

   procedure selectcell (uli : Line_Position;
                         ulj : Column_Position;
                         lri : Line_Position;
                         lrj : Column_Position;
                         p   : out pair;
                         b   : out Boolean) is
      c : Key_Code;
      res : pair;
      i : Line_Position := 0;
      j : Column_Position := 0;
      si : constant Line_Position := lri - uli + 1;
      sj : constant Column_Position := lrj - ulj + 1;
   begin
      res.y := uli;
      res.x := ulj;
      loop
         Move_Cursor (Line => uli + i, Column => ulj + j);
         newwin_report;

         c := Getchar;
         case c is
            when
              Macro_Quit   |
              Macro_Escape =>
               --  on the same line macro calls interfere due to the # comment
               --  this is needed because keypad off affects all windows.
               --  try removing the ESCAPE and see what happens.
               b := False;
               return;
            when KEY_UP =>
               i := i + si - 1;
               --  same as  i := i - 1 because of Modulus arithmetic,
               --  on Line_Position, which is a Natural
               --  the C version uses this form too, interestingly.
            when KEY_DOWN =>
               i := i + 1;
            when KEY_LEFT =>
               j := j + sj - 1;
            when KEY_RIGHT =>
               j := j + 1;
            when Key_Mouse =>
               declare
                  event : Mouse_Event;
                  y : Line_Position;
                  x : Column_Position;
                  Button : Mouse_Button;
                  State : Button_State;

               begin
                  event := Get_Mouse;
                  Get_Event (Event => event,
                             Y => y,
                             X => x,
                             Button => Button,
                             State  => State);
                  if y > uli and x > ulj then
                     i := y - uli;
                     j := x - ulj;
                     --  same as when others =>
                     res.y := uli + i;
                     res.x := ulj + j;
                     p := res;
                     b := True;
                     return;
                  else
                     Beep;
                  end if;
               end;
            when others =>
               res.y := uli + i;
               res.x := ulj + j;
               p := res;
               b := True;
               return;
         end case;
         i := i mod si;
         j := j mod sj;
      end loop;
   end selectcell;

   function getwindow return Window is
      rwindow : Window;
      ul, lr : pair;
      result : Boolean;
   begin
      Move_Cursor (Line => 0, Column => 0);
      Clear_To_End_Of_Line;
      Add (Str => "Use arrows to move cursor, anything else to mark corner 1");
      Refresh;
      selectcell (2, 1, Lines - Botlines - 2, Columns - 2, ul, result);
      if not result then
         return Null_Window;
      end if;
      Add (Line => ul.y - 1, Column => ul.x - 1,
           Ch => ACS_Map (ACS_Upper_Left_Corner));
      Move_Cursor (Line => 0, Column => 0);
      Clear_To_End_Of_Line;
      Add (Str => "Use arrows to move cursor, anything else to mark corner 2");
      Refresh;
      selectcell (ul.y, ul.x, Lines - Botlines - 2, Columns - 2, lr, result);
      if not result then
         return Null_Window;
      end if;

      rwindow := Sub_Window (Number_Of_Lines => lr.y - ul.y + 1,
                             Number_Of_Columns => lr.x - ul.x + 1,
                             First_Line_Position => ul.y,
                             First_Column_Position => ul.x);

      Outerbox (ul, lr, True);
      Refresh;

      Refresh (rwindow);

      Move_Cursor (Line => 0, Column => 0);
      Clear_To_End_Of_Line;
      return rwindow;
   end getwindow;

   procedure newwin_move (win : Window;
                          dy  : Line_Position;
                          dx  : Column_Position) is
      cur_y, max_y : Line_Position;
      cur_x, max_x : Column_Position;
   begin
      Get_Cursor_Position (win, cur_y, cur_x);
      Get_Size (win, max_y, max_x);
      cur_x := Column_Position'Min (Column_Position'Max (cur_x + dx, 0),
                                    max_x - 1);
      cur_y := Line_Position'Min (Line_Position'Max (cur_y + dy, 0),
                                  max_y - 1);

      Move_Cursor (win, Line => cur_y, Column => cur_x);
   end newwin_move;

   function delete_framed (fp : FrameA; showit : Boolean) return FrameA is
      np : FrameA;
   begin
      fp.all.last.all.next := fp.all.next;
      fp.all.next.all.last := fp.all.last;

      if showit then
         Erase (fp.all.wind);
         Refresh (fp.all.wind);
      end if;
      Delete (fp.all.wind);

      if fp = fp.all.next then
         np := null;
      else
         np := fp.all.next;
      end if;
      --  TODO free(fp);
      return np;
   end delete_framed;

   Mask : Event_Mask := No_Events;
   Mask2 : Event_Mask;

   usescr : Window;

begin
   if Has_Mouse then
      Register_Reportable_Event (
                                 Button => Left,
                                 State => Clicked,
                                 Mask => Mask);
      Mask2 := Start_Mouse (Mask);
   end if;
   c := CTRL ('C');
   Set_Raw_Mode (SwitchOn => True);
   loop
      transient (Standard_Window, "");
      case c is
         when Character'Pos ('c') mod 16#20# => --  Ctrl('c')
            declare
               neww : constant FrameA := new Frame'(null, null,
                                                    False, False,
                                                    Null_Window);
            begin
               neww.all.wind := getwindow;
               if neww.all.wind = Null_Window  then
                  exit;
                  --  was goto breakout; ha ha ha
               else

                  if current = null  then
                     neww.all.next := neww;
                     neww.all.last := neww;
                  else
                     neww.all.next := current.all.next;
                     neww.all.last := current;
                     neww.all.last.all.next := neww;
                     neww.all.next.all.last := neww;
                  end if;
                  current := neww;

                  Set_KeyPad_Mode (current.all.wind, True);
                  current.all.do_keypad := HaveKeyPad (current.all.wind);
                  current.all.do_scroll := HaveScroll (current.all.wind);
               end if;
            end;
         when Character'Pos ('N') mod 16#20#  => --  Ctrl('N')
            if current /= null then
               current := current.all.next;
            end if;
         when Character'Pos ('P') mod 16#20#  => --  Ctrl('P')
            if current /= null then
               current := current.all.last;
            end if;
         when Character'Pos ('F') mod 16#20#  => --  Ctrl('F')
            if current /= null and then HaveScroll (current.all.wind) then
               Scroll (current.all.wind, 1);
            end if;
         when Character'Pos ('B') mod 16#20#  => --  Ctrl('B')
            if current /= null and then HaveScroll (current.all.wind) then
            --  The C version of Scroll may return ERR which is ignored
            --  we need to avoid the exception
            --  with the 'and HaveScroll(current.wind)'
               Scroll (current.all.wind, -1);
            end if;
         when Character'Pos ('K') mod 16#20#  => --  Ctrl('K')
            if current /= null then
               current.all.do_keypad := not current.all.do_keypad;
               Set_KeyPad_Mode (current.all.wind, current.all.do_keypad);
            end if;
         when Character'Pos ('S') mod 16#20#  => --  Ctrl('S')
            if current /= null then
               current.all.do_scroll := not current.all.do_scroll;
               Allow_Scrolling (current.all.wind, current.all.do_scroll);
            end if;
         when Character'Pos ('W') mod 16#20#  => --  Ctrl('W')
            if current /= current.all.next then
               Create (f, Name => dumpfile); -- TODO error checking
               if not Is_Open (f) then
                  raise Curses_Exception;
               end if;
               Put_Window (current.all.wind, f);
               Close (f);
               current := delete_framed (current, True);
            end if;
         when Character'Pos ('R') mod 16#20#  => --  Ctrl('R')
            declare
               neww : FrameA := new Frame'(null, null, False, False,
                                           Null_Window);
            begin
               Open (f, Mode => In_File, Name => dumpfile);
               neww := new Frame'(null, null, False, False, Null_Window);

               neww.all.next := current.all.next;
               neww.all.last := current;
               neww.all.last.all.next := neww;
               neww.all.next.all.last := neww;

               neww.all.wind := Get_Window (f);
               Close (f);

               Refresh (neww.all.wind);
            end;
         when Character'Pos ('X') mod 16#20# => --  Ctrl('X')
            if current /= null then
               declare
                  tmp, ul, lr : pair;
                  mx : Column_Position;
                  my : Line_Position;
                  tmpbool : Boolean;
               begin
                  Move_Cursor (Line => 0, Column => 0);
                  Clear_To_End_Of_Line;
                  Add (Str => "Use arrows to move cursor, anything else " &
                       "to mark new corner");
                  Refresh;

                  Get_Window_Position (current.all.wind, ul.y, ul.x);

                  selectcell (ul.y, ul.x, Lines - Botlines - 2, Columns - 2,
                              tmp, tmpbool);
                  if not tmpbool then
                     --  the C version had a goto. I refuse gotos.
                     Beep;
                  else
                     Get_Size (current.all.wind, lr.y, lr.x);
                     lr.y := lr.y + ul.y - 1;
                     lr.x := lr.x + ul.x - 1;
                     Outerbox (ul, lr, False);
                     Refresh_Without_Update;

                     Get_Size (current.all.wind, my, mx);
                     if my > tmp.y - ul.y then
                        Get_Cursor_Position (current.all.wind, lr.y, lr.x);
                        Move_Cursor (current.all.wind, tmp.y - ul.y + 1, 0);
                        Clear_To_End_Of_Screen (current.all.wind);
                        Move_Cursor (current.all.wind, lr.y, lr.x);
                     end if;
                     if mx > tmp.x - ul.x then
                        for i in 0 .. my - 1 loop
                           Move_Cursor (current.all.wind, i, tmp.x - ul.x + 1);
                           Clear_To_End_Of_Line (current.all.wind);
                        end loop;
                     end if;
                     Refresh_Without_Update (current.all.wind);

                     lr := tmp;
                     --  The C version passes invalid args to resize
                     --  which returns an ERR. For Ada we avoid the exception.
                     if lr.y /= ul.y and lr.x /= ul.x then
                        Resize (current.all.wind, lr.y - ul.y + 0,
                                lr.x - ul.x + 0);
                     end if;

                     Get_Window_Position (current.all.wind, ul.y, ul.x);
                     Get_Size (current.all.wind, lr.y, lr.x);
                     lr.y := lr.y + ul.y - 1;
                     lr.x := lr.x + ul.x - 1;
                     Outerbox (ul, lr, True);
                     Refresh_Without_Update;

                     Refresh_Without_Update (current.all.wind);
                     Move_Cursor (Line => 0, Column => 0);
                     Clear_To_End_Of_Line;
                     Update_Screen;
                  end if;
               end;
            end if;
         when Key_F10  =>
            declare tmp : pair; tmpbool : Boolean;
            begin
               --  undocumented --- use this to test area clears
               selectcell (0, 0, Lines - 1, Columns - 1, tmp, tmpbool);
               Clear_To_End_Of_Screen;
               Refresh;
            end;
         when Key_Cursor_Up =>
            newwin_move (current.all.wind, -1, 0);
         when Key_Cursor_Down  =>
            newwin_move (current.all.wind, 1, 0);
         when Key_Cursor_Left  =>
            newwin_move (current.all.wind, 0, -1);
         when Key_Cursor_Right  =>
            newwin_move (current.all.wind, 0, 1);
         when Key_Backspace | Key_Delete_Char  =>
            declare
               y : Line_Position;
               x : Column_Position;
               tmp : Line_Position;
            begin
               Get_Cursor_Position (current.all.wind, y, x);
               --  x := x - 1;
               --  I got tricked by the -1 = Max_Natural - 1 result
               --  y := y - 1;
               if not (x = 0 and y = 0) then
                  if x = 0 then
                     y := y - 1;
                     Get_Size (current.all.wind, tmp, x);
                  end if;
                  x := x - 1;
                  Delete_Character (current.all.wind, y, x);
               end if;
            end;
         when others =>
            --  TODO c = '\r' ?
            if current /= null then
               declare
               begin
                  Add (current.all.wind, Ch => Code_To_Char (c));
               exception
                  when Curses_Exception => null;
                     --  this happens if we are at the
                     --  lower right of a window and add a character.
               end;
            else
               Beep;
            end if;
      end case;
      newwin_report (current.all.wind);
      if current /= null then
         usescr := current.all.wind;
      else
         usescr := Standard_Window;
      end if;
      Refresh (usescr);
      c := Getchar (usescr);
      exit when c = Quit or (c = Escape and HaveKeyPad (usescr));
      --  TODO when does c = ERR happen?
   end loop;

   --  TODO while current /= null loop
   --  current := delete_framed(current, False);
   --  end loop;

   Allow_Scrolling (Mode => True);

   End_Mouse (Mask2);
   Set_Raw_Mode (SwitchOn => True);
   Erase;
   End_Windows;

end ncurses2.acs_and_scroll;
