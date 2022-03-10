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
-- Copyright 2000-2011,2014 Free Software Foundation, Inc.                  --
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

with Interfaces.C;
with System.Storage_Elements;
with System.Address_To_Access_Conversions;

with Ada.Text_IO;
--  with Ada.Real_Time; use Ada.Real_Time;
--  TODO is there a way to use Real_Time or Ada.Calendar in place of
--  gettimeofday?

--  Demonstrate pads.
procedure ncurses2.demo_pad is

   type timestruct is record
      seconds : Integer;
      microseconds : Integer;
   end record;

   type myfunc is access function (w : Window) return Key_Code;

   function  gettime return timestruct;
   procedure do_h_line (y  : Line_Position;
                        x  : Column_Position;
                        c  : Attributed_Character;
                        to : Column_Position);
   procedure do_v_line (y  : Line_Position;
                        x  : Column_Position;
                        c  : Attributed_Character;
                        to : Line_Position);
   function  padgetch (win : Window) return Key_Code;
   function  panner_legend (line : Line_Position) return Boolean;
   procedure panner_legend (line : Line_Position);
   procedure panner_h_cleanup (from_y : Line_Position;
                               from_x : Column_Position;
                               to_x   : Column_Position);
   procedure panner_v_cleanup (from_y : Line_Position;
                               from_x : Column_Position;
                               to_y   : Line_Position);
   procedure panner (pad    : Window;
                     top_xp : Column_Position;
                     top_yp : Line_Position;
                     portyp : Line_Position;
                     portxp : Column_Position;
                     pgetc  : myfunc);

   function gettime return timestruct is

      retval : timestruct;

      use Interfaces.C;
      type timeval is record
         tv_sec : long;
         tv_usec : long;
      end record;
      pragma Convention (C, timeval);

      --      TODO    function from_timeval is new Ada.Unchecked_Conversion(
      --                  timeval_a, System.Storage_Elements.Integer_Address);
      --  should Interfaces.C.Pointers be used here?

      package myP is new System.Address_To_Access_Conversions (timeval);
      use myP;

      t : constant Object_Pointer := new timeval;

      function gettimeofday
        (TP : System.Storage_Elements.Integer_Address;
         TZP : System.Storage_Elements.Integer_Address) return int;
      pragma Import (C, gettimeofday, "gettimeofday");
      tmp : int;
   begin
      tmp := gettimeofday (System.Storage_Elements.To_Integer
                           (myP.To_Address (t)),
                           System.Storage_Elements.To_Integer
                           (myP.To_Address (null)));
      if tmp < 0 then
         retval.seconds := 0;
         retval.microseconds := 0;
      else
         retval.seconds := Integer (t.all.tv_sec);
         retval.microseconds := Integer (t.all.tv_usec);
      end if;
      return retval;
   end gettime;

   --  in C, The behavior of mvhline, mvvline for negative/zero length is
   --  unspecified, though we can rely on negative x/y values to stop the
   --  macro. Except Ada makes Line_Position(-1) = Natural - 1 so forget it.
   procedure do_h_line (y  : Line_Position;
                        x  : Column_Position;
                        c  : Attributed_Character;
                        to : Column_Position) is
   begin
      if to > x then
         Move_Cursor (Line => y, Column => x);
         Horizontal_Line (Line_Size => Natural (to - x), Line_Symbol => c);
      end if;
   end do_h_line;

   procedure do_v_line (y  : Line_Position;
                        x  : Column_Position;
                        c  : Attributed_Character;
                        to : Line_Position) is
   begin
      if to > y then
         Move_Cursor (Line => y, Column => x);
         Vertical_Line (Line_Size => Natural (to - y), Line_Symbol => c);
      end if;
   end do_v_line;

   function padgetch (win : Window) return Key_Code is
      c : Key_Code;
      c2 : Character;
   begin
      c := Getchar (win);
      c2 := Code_To_Char (c);

      case c2 is
         when '!' =>
            ShellOut (False);
            return Key_Refresh;
         when Character'Val (Character'Pos ('r') mod 16#20#) => --  CTRL('r')
            End_Windows;
            Refresh;
            return Key_Refresh;
         when Character'Val (Character'Pos ('l') mod 16#20#) => --  CTRL('l')
            return Key_Refresh;
         when 'U' =>
            return Key_Cursor_Up;
         when 'D' =>
            return Key_Cursor_Down;
         when 'R' =>
            return Key_Cursor_Right;
         when 'L' =>
            return Key_Cursor_Left;
         when '+' =>
            return Key_Insert_Line;
         when '-' =>
            return Key_Delete_Line;
         when '>' =>
            return Key_Insert_Char;
         when '<' =>
            return Key_Delete_Char;
            --  when ERR=>                   /* FALLTHRU */
         when 'q' =>
            return (Key_Exit);
         when others =>
            return (c);
      end case;
   end padgetch;

   show_panner_legend : Boolean := True;

   function panner_legend (line : Line_Position) return Boolean is
      legend : constant array (0 .. 3) of String (1 .. 61) :=
        (
         "Use arrow keys (or U,D,L,R) to pan, q to quit (?,t,s flags)  ",
         "Use ! to shell-out.  Toggle legend:?, timer:t, scroll mark:s.",
         "Use +,- (or j,k) to grow/shrink the panner vertically.       ",
         "Use <,> (or h,l) to grow/shrink the panner horizontally.     ");
      legendsize : constant := 4;

      n : constant Integer := legendsize - Integer (Lines - line);
   begin
      if line < Lines and n >= 0 then
         Move_Cursor (Line => line, Column => 0);
         if show_panner_legend then
            Add (Str => legend (n));
         end if;
         Clear_To_End_Of_Line;
         return show_panner_legend;
      end if;
      return False;
   end panner_legend;

   procedure panner_legend (line : Line_Position) is
   begin
      if not panner_legend (line) then
         Beep;
      end if;
   end panner_legend;

   procedure panner_h_cleanup (from_y : Line_Position;
                               from_x : Column_Position;
                               to_x   : Column_Position) is
   begin
      if not panner_legend (from_y) then
         do_h_line (from_y, from_x, Blank2, to_x);
      end if;
   end panner_h_cleanup;

   procedure panner_v_cleanup (from_y : Line_Position;
                               from_x : Column_Position;
                               to_y   : Line_Position) is
   begin
      if not panner_legend (from_y) then
         do_v_line (from_y, from_x, Blank2, to_y);
      end if;
   end panner_v_cleanup;

   procedure panner (pad    : Window;
                     top_xp : Column_Position;
                     top_yp : Line_Position;
                     portyp : Line_Position;
                     portxp : Column_Position;
                     pgetc  : myfunc) is

      function f (y : Line_Position) return Line_Position;
      function f (x : Column_Position) return Column_Position;
      function greater (y1, y2 : Line_Position) return Integer;
      function greater (x1, x2 : Column_Position) return Integer;

      top_x : Column_Position := top_xp;
      top_y : Line_Position := top_yp;
      porty : Line_Position := portyp;
      portx : Column_Position := portxp;

      --  f[x] returns max[x - 1, 0]
      function f (y : Line_Position) return Line_Position is
      begin
         if y > 0 then
            return y - 1;
         else
            return y; -- 0
         end if;
      end f;

      function f (x : Column_Position) return Column_Position is
      begin
         if x > 0 then
            return x - 1;
         else
            return x; -- 0
         end if;
      end f;

      function greater (y1, y2 : Line_Position) return Integer is
      begin
         if y1 > y2 then
            return 1;
         else
            return 0;
         end if;
      end greater;

      function greater (x1, x2 : Column_Position) return Integer is
      begin
         if x1 > x2 then
            return 1;
         else
            return 0;
         end if;
      end greater;

      pymax : Line_Position;
      basey : Line_Position := 0;
      pxmax : Column_Position;
      basex : Column_Position := 0;
      c : Key_Code;
      scrollers : Boolean := True;
      before, after : timestruct;
      timing : Boolean := True;

      package floatio is new Ada.Text_IO.Float_IO (Long_Float);
   begin
      Get_Size (pad, pymax, pxmax);
      Allow_Scrolling (Mode => False); -- we don't want stdscr to scroll!

      c := Key_Refresh;
      loop
         --  During shell-out, the user may have resized the window.  Adjust
         --  the port size of the pad to accommodate this.  Ncurses
         --  automatically resizes all of the normal windows to fit on the
         --  new screen.
         if top_x > Columns then
            top_x := Columns;
         end if;
         if portx > Columns then
            portx := Columns;
         end if;
         if top_y > Lines then
            top_y := Lines;
         end if;
         if porty > Lines then
            porty := Lines;
         end if;

         case c is
            when Key_Refresh | Character'Pos ('?') =>
               if c = Key_Refresh then
                  Erase;
               else -- '?'
                  show_panner_legend := not show_panner_legend;
               end if;
               panner_legend (Lines - 4);
               panner_legend (Lines - 3);
               panner_legend (Lines - 2);
               panner_legend (Lines - 1);
            when Character'Pos ('t') =>
               timing := not timing;
               if not timing then
                  panner_legend (Lines - 1);
               end if;
            when Character'Pos ('s') =>
               scrollers := not scrollers;

               --  Move the top-left corner of the pad, keeping the
               --  bottom-right corner fixed.
            when Character'Pos ('h') =>
               --  increase-columns: move left edge to left
               if top_x = 0 then
                  Beep;
               else
                  panner_v_cleanup (top_y, top_x, porty);
                  top_x := top_x - 1;
               end if;

            when Character'Pos ('j') =>
               --  decrease-lines: move top-edge down
               if top_y >= porty then
                  Beep;
               else
                  if top_y /= 0 then
                     panner_h_cleanup (top_y - 1, f (top_x), portx);
                  end if;
                  top_y := top_y + 1;
               end if;
            when Character'Pos ('k') =>
               --  increase-lines: move top-edge up
               if top_y = 0 then
                  Beep;
               else
                  top_y := top_y - 1;
                  panner_h_cleanup (top_y, top_x, portx);
               end if;

            when Character'Pos ('l') =>
               --  decrease-columns: move left-edge to right
               if top_x >= portx then
                  Beep;
               else
                  if top_x /= 0 then
                     panner_v_cleanup (f (top_y), top_x - 1, porty);
                  end if;
                  top_x := top_x + 1;
               end if;

               --  Move the bottom-right corner of the pad, keeping the
               --  top-left corner fixed.
            when Key_Insert_Char =>
               --  increase-columns: move right-edge to right
               if portx >= pxmax or portx >= Columns then
                  Beep;
               else
                  panner_v_cleanup (f (top_y), portx - 1, porty);
                  portx := portx + 1;
                  --  C had ++portx instead of portx++, weird.
               end if;
            when Key_Insert_Line =>
               --  increase-lines: move bottom-edge down
               if porty >= pymax or porty >= Lines then
                  Beep;
               else
                  panner_h_cleanup (porty - 1, f (top_x), portx);
                  porty := porty + 1;
               end if;

            when Key_Delete_Char =>
               --  decrease-columns: move bottom edge up
               if portx <= top_x then
                  Beep;
               else
                  portx := portx - 1;
                  panner_v_cleanup (f (top_y), portx, porty);
               end if;

            when Key_Delete_Line =>
               --  decrease-lines
               if porty <= top_y then
                  Beep;
               else
                  porty := porty - 1;
                  panner_h_cleanup (porty, f (top_x), portx);
               end if;
            when Key_Cursor_Left =>
               --  pan leftwards
               if basex > 0 then
                  basex := basex - 1;
               else
                  Beep;
               end if;
            when Key_Cursor_Right =>
               --  pan rightwards
               --  if (basex + portx - (pymax > porty) < pxmax)
               if basex + portx -
                   Column_Position (greater (pymax, porty)) < pxmax
               then
                  --  if basex + portx  < pxmax or
                  --      (pymax > porty and basex + portx - 1 < pxmax) then
                  basex := basex + 1;
               else
                  Beep;
               end if;

            when Key_Cursor_Up =>
               --  pan upwards
               if basey > 0 then
                  basey := basey - 1;
               else
                  Beep;
               end if;

            when Key_Cursor_Down =>
               --  pan downwards
               --  same as if (basey + porty - (pxmax > portx) < pymax)
               if basey + porty -
                   Line_Position (greater (pxmax, portx)) < pymax
               then
                  --  if (basey + porty  < pymax) or
                  --      (pxmax > portx and basey + porty - 1 < pymax) then
                  basey := basey + 1;
               else
                  Beep;
               end if;

            when  Character'Pos ('H') |
              Key_Home |
              Key_Find =>
               basey := 0;

            when   Character'Pos ('E') |
              Key_End |
              Key_Select =>
               if pymax < porty then
                  basey := 0;
               else
                  basey := pymax - porty;
               end if;

            when others =>
               Beep;
         end case;

         --  more writing off the screen.
         --  Interestingly, the exception is not handled if
         --  we put a block around this.
         --  declare --begin
         if top_y /= 0 and top_x /= 0 then
            Add (Line => top_y - 1, Column => top_x - 1,
                 Ch => ACS_Map (ACS_Upper_Left_Corner));
         end if;
         if top_x /= 0 then
            do_v_line (top_y, top_x - 1, ACS_Map (ACS_Vertical_Line), porty);
         end if;
         if top_y /= 0 then
            do_h_line (top_y - 1, top_x, ACS_Map (ACS_Horizontal_Line), portx);
         end if;
         --  exception when Curses_Exception => null; end;

         --  in C was ... pxmax > portx - 1
         if scrollers and pxmax >= portx then
            declare
               length : constant Column_Position := portx - top_x - 1;
               lowend, highend : Column_Position;
            begin
               --  Instead of using floats, I'll use integers only.
               lowend := top_x + (basex * length) / pxmax;
               highend := top_x + ((basex + length) * length) / pxmax;

               do_h_line (porty - 1, top_x, ACS_Map (ACS_Horizontal_Line),
                          lowend);
               if highend < portx then
                  Switch_Character_Attribute
                    (Attr => (Reverse_Video => True, others => False),
                     On => True);
                  do_h_line (porty - 1, lowend, Blank2, highend + 1);
                  Switch_Character_Attribute
                    (Attr => (Reverse_Video => True, others => False),
                     On => False);
                  do_h_line (porty - 1, highend + 1,
                             ACS_Map (ACS_Horizontal_Line), portx);
               end if;
            end;
         else
            do_h_line (porty - 1, top_x, ACS_Map (ACS_Horizontal_Line), portx);
         end if;

         if scrollers and pymax >= porty then
            declare
               length : constant Line_Position := porty - top_y - 1;
               lowend, highend : Line_Position;
            begin
               lowend := top_y + (basey * length) / pymax;
               highend := top_y + ((basey + length) * length) / pymax;

               do_v_line (top_y, portx - 1, ACS_Map (ACS_Vertical_Line),
                          lowend);
               if highend < porty then
                  Switch_Character_Attribute
                    (Attr => (Reverse_Video => True, others => False),
                     On => True);
                  do_v_line (lowend, portx - 1, Blank2, highend + 1);
                  Switch_Character_Attribute
                    (Attr => (Reverse_Video => True, others => False),
                     On => False);
                  do_v_line (highend + 1, portx - 1,
                             ACS_Map (ACS_Vertical_Line), porty);
               end if;
            end;
         else
            do_v_line (top_y, portx - 1, ACS_Map (ACS_Vertical_Line), porty);
         end if;

         if top_y /= 0 then
            Add (Line => top_y - 1, Column => portx - 1,
                 Ch => ACS_Map (ACS_Upper_Right_Corner));
         end if;
         if top_x /= 0 then
            Add (Line => porty - 1, Column => top_x - 1,
                 Ch => ACS_Map (ACS_Lower_Left_Corner));
         end if;
         declare
         begin
            --  Here is another place where it is possible
            --  to write to the corner of the screen.
            Add (Line => porty - 1, Column => portx - 1,
                 Ch => ACS_Map (ACS_Lower_Right_Corner));
            exception
            when Curses_Exception => null;
         end;

         before := gettime;

         Refresh_Without_Update;

         declare
            --  the C version allows the panel to have a zero height
            --  which raise the exception
         begin
            Refresh_Without_Update
              (
               pad,
               basey, basex,
               top_y, top_x,
               porty - Line_Position (greater (pxmax, portx)) - 1,
               portx - Column_Position (greater (pymax, porty)) - 1);
            exception
            when Curses_Exception => null;
         end;

         Update_Screen;

         if timing then
            declare
               s : String (1 .. 7);
               elapsed : Long_Float;
            begin
               after := gettime;
               elapsed := (Long_Float (after.seconds - before.seconds) +
                           Long_Float (after.microseconds
                                     - before.microseconds)
                           / 1.0e6);
               Move_Cursor (Line => Lines - 1, Column => Columns - 20);
               floatio.Put (s, elapsed, Aft => 3, Exp => 0);
               Add (Str => s);
               Refresh;
            end;
         end if;

         c := pgetc (pad);
         exit when c = Key_Exit;

      end loop;

      Allow_Scrolling (Mode => True);

   end panner;

   Gridsize : constant := 3;
   Gridcount : Integer := 0;

   Pad_High : constant Line_Count :=  200;
   Pad_Wide : constant Column_Count := 200;
   panpad : Window := New_Pad (Pad_High, Pad_Wide);
begin
   if panpad = Null_Window then
      Cannot ("cannot create requested pad");
      return;
   end if;

   for i in 0 .. Pad_High - 1 loop
      for j in 0 .. Pad_Wide - 1  loop
         if i mod Gridsize = 0 and j mod Gridsize = 0 then
            if i = 0 or j = 0 then
               Add (panpad, '+');
            else
               --  depends on ASCII?
               Add (panpad,
                    Ch => Character'Val (Character'Pos ('A') +
                                         Gridcount mod 26));
               Gridcount := Gridcount + 1;
            end if;
         elsif i mod Gridsize = 0 then
            Add (panpad, '-');
         elsif j mod Gridsize = 0 then
            Add (panpad, '|');
         else
            declare
               --  handle the write to the lower right corner error
            begin
               Add (panpad, ' ');
               exception
               when Curses_Exception => null;
            end;
         end if;
      end loop;
   end loop;
   panner_legend (Lines - 4);
   panner_legend (Lines - 3);
   panner_legend (Lines - 2);
   panner_legend (Lines - 1);

   Set_KeyPad_Mode (panpad, True);
   --  Make the pad (initially) narrow enough that a trace file won't wrap.
   --  We'll still be able to widen it during a test, since that's required
   --  for testing boundaries.

   panner (panpad, 2, 2, Lines - 5, Columns - 15, padgetch'Access);

   Delete (panpad);
   End_Windows; --  Hmm, Erase after End_Windows
   Erase;
end ncurses2.demo_pad;
