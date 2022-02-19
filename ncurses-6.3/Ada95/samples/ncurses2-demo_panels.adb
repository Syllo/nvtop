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
-- Copyright 2000-2008,2011 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.9 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Panels; use Terminal_Interface.Curses.Panels;
with Terminal_Interface.Curses.Panels.User_Data;

with ncurses2.genericPuts;

procedure ncurses2.demo_panels (nap_mseci : Integer) is

   function  mkpanel (color : Color_Number;
                      rows  : Line_Count;
                      cols  : Column_Count;
                      tly   : Line_Position;
                      tlx   : Column_Position) return Panel;
   procedure rmpanel (pan : in out Panel);
   procedure pflush;
   procedure wait_a_while (msec : Integer);
   procedure saywhat (text : String);
   procedure fill_panel (pan : Panel);

   nap_msec : Integer := nap_mseci;

   function mkpanel (color : Color_Number;
                     rows  : Line_Count;
                     cols  : Column_Count;
                     tly   : Line_Position;
                     tlx   : Column_Position) return Panel is
      win : Window;
      pan : Panel := Null_Panel;
   begin
      win := New_Window (rows, cols, tly, tlx);
      if Null_Window /= win then
         pan := New_Panel (win);
         if pan = Null_Panel then
            Delete (win);
         elsif Has_Colors then
            declare
               fg, bg : Color_Number;
            begin
               if color = Blue then
                  fg := White;
               else
                  fg := Black;
               end if;
               bg := color;
               Init_Pair (Color_Pair (color), fg, bg);
               Set_Background (win, (Ch => ' ',
                                     Attr => Normal_Video,
                                     Color => Color_Pair (color)));
            end;
         else
            Set_Background (win, (Ch => ' ',
                                  Attr => (Bold_Character => True,
                                           others => False),
                                  Color => Color_Pair (color)));
         end if;
      end if;
      return pan;
   end mkpanel;

   procedure rmpanel (pan : in out Panel) is
      win : Window := Panel_Window (pan);
   begin
      Delete (pan);
      Delete (win);
   end rmpanel;

   procedure pflush is
   begin
      Update_Panels;
      Update_Screen;
   end pflush;

   procedure wait_a_while (msec : Integer) is
   begin
      --  The C version had some #ifdef blocks here
      if msec = 1 then
         Getchar;
      else
         Nap_Milli_Seconds (msec);
      end if;
   end wait_a_while;

   procedure saywhat (text : String) is
   begin
      Move_Cursor (Line => Lines - 1, Column => 0);
      Clear_To_End_Of_Line;
      Add (Str => text);
   end saywhat;

   --  from sample-curses_demo.adb
   type User_Data is new String (1 .. 2);
   type User_Data_Access is access all User_Data;
   package PUD is new Panels.User_Data (User_Data, User_Data_Access);

   use PUD;

   procedure fill_panel (pan : Panel) is
      win : constant Window := Panel_Window (pan);
      num : constant Character := Get_User_Data (pan).all (2);
      tmp6 : String (1 .. 6) := "-panx-";
      maxy : Line_Count;
      maxx : Column_Count;

   begin
      Move_Cursor (win, 1, 1);
      tmp6 (5) := num;
      Add (win, Str => tmp6);
      Clear_To_End_Of_Line (win);
      Box (win);
      Get_Size (win, maxy, maxx);
      for y in 2 .. maxy - 3 loop
         for x in 1 .. maxx - 3 loop
            Move_Cursor (win, y, x);
            Add (win, num);
         end loop;
      end loop;
   exception
   when Curses_Exception => null;
   end fill_panel;

   modstr : constant array (0 .. 5) of String (1 .. 5) :=
     ("test ",
      "TEST ",
      "(**) ",
      "*()* ",
      "<--> ",
      "LAST "
      );

   package p is new ncurses2.genericPuts (1024);
   use p;
   use p.BS;
   --  the C version said register int y, x;
   tmpb : BS.Bounded_String;

begin
   Refresh;

   for y in 0 .. Integer (Lines - 2) loop
      for x in 0 .. Integer (Columns - 1) loop
         myPut (tmpb, (y + x) mod 10);
         myAdd (Str => tmpb);
      end loop;
   end loop;
   for y in 0 .. 4 loop
      declare
         p1, p2, p3, p4, p5 : Panel;
         U1 : constant User_Data_Access := new User_Data'("p1");
         U2 : constant User_Data_Access := new User_Data'("p2");
         U3 : constant User_Data_Access := new User_Data'("p3");
         U4 : constant User_Data_Access := new User_Data'("p4");
         U5 : constant User_Data_Access := new User_Data'("p5");

      begin
         p1 := mkpanel (Red, Lines / 2 - 2, Columns / 8 + 1, 0, 0);
         Set_User_Data (p1, U1);
         p2 := mkpanel (Green, Lines / 2 + 1, Columns / 7, Lines / 4,
                        Columns / 10);
         Set_User_Data (p2, U2);
         p3 := mkpanel (Yellow, Lines / 4, Columns / 10, Lines / 2,
                        Columns / 9);
         Set_User_Data (p3, U3);
         p4 := mkpanel (Blue, Lines / 2 - 2, Columns / 8,  Lines / 2 - 2,
                        Columns / 3);
         Set_User_Data (p4, U4);
         p5 := mkpanel (Magenta, Lines / 2 - 2, Columns / 8,  Lines / 2,
                        Columns / 2 - 2);
         Set_User_Data (p5, U5);

         fill_panel (p1);
         fill_panel (p2);
         fill_panel (p3);
         fill_panel (p4);
         fill_panel (p5);
         Hide (p4);
         Hide (p5);
         pflush;
         saywhat ("press any key to continue");
         wait_a_while (nap_msec);

         saywhat ("h3 s1 s2 s4 s5; press any key to continue");
         Move (p1, 0, 0);
         Hide (p3);
         Show (p1);
         Show (p2);
         Show (p4);
         Show (p5);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("s1; press any key to continue");
         Show (p1);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("s2; press any key to continue");
         Show (p2);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("m2; press any key to continue");
         Move (p2, Lines / 3 + 1, Columns / 8);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("s3;");
         Show (p3);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("m3; press any key to continue");
         Move (p3, Lines / 4 + 1, Columns / 15);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("b3; press any key to continue");
         Bottom (p3);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("s4; press any key to continue");
         Show (p4);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("s5; press any key to continue");
         Show (p5);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t3; press any key to continue");
         Top (p3);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t1; press any key to continue");
         Top (p1);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t2; press any key to continue");
         Top (p2);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t3; press any key to continue");
         Top (p3);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t4; press any key to continue");
         Top (p4);
         pflush;
         wait_a_while (nap_msec);

         for itmp in  0 ..  5 loop
            declare
               w4 : constant Window := Panel_Window (p4);
               w5 : constant Window := Panel_Window (p5);
            begin

               saywhat ("m4; press any key to continue");
               Move_Cursor (w4, Lines / 8, 1);
               Add (w4, modstr (itmp));
               Move (p4, Lines / 6, Column_Position (itmp) * (Columns / 8));
               Move_Cursor (w5, Lines / 6, 1);
               Add (w5, modstr (itmp));
               pflush;
               wait_a_while (nap_msec);

               saywhat ("m5; press any key to continue");
               Move_Cursor (w4, Lines / 6, 1);
               Add (w4, modstr (itmp));
               Move (p5, Lines / 3 - 1, (Column_Position (itmp) * 10) + 6);
               Move_Cursor (w5, Lines / 8, 1);
               Add (w5, modstr (itmp));
               pflush;
               wait_a_while (nap_msec);
            end;
         end loop;

         saywhat ("m4; press any key to continue");
         Move (p4, Lines / 6, 6 * (Columns / 8));
         --  Move(p4, Lines / 6, itmp * (Columns / 8));
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t5; press any key to continue");
         Top (p5);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t2; press any key to continue");
         Top (p2);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("t1; press any key to continue");
         Top (p1);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("d2; press any key to continue");
         rmpanel (p2);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("h3; press any key to continue");
         Hide (p3);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("d1; press any key to continue");
         rmpanel (p1);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("d4; press any key to continue");
         rmpanel (p4);
         pflush;
         wait_a_while (nap_msec);

         saywhat ("d5; press any key to continue");
         rmpanel (p5);
         pflush;
         wait_a_while (nap_msec);
         if nap_msec = 1 then
            exit;
         else
            nap_msec := 100;
         end if;

      end;
   end loop;

   Erase;
   End_Windows;

end ncurses2.demo_panels;
