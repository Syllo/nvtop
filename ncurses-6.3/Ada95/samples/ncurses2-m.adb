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
-- Copyright 2000-2007,2008 Free Software Foundation, Inc.                  --
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
--  TODO use Default_Character where appropriate

--  This is an Ada version of ncurses
--  I translated this because it tests the most features.

with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Trace; use Terminal_Interface.Curses.Trace;

with Ada.Text_IO; use Ada.Text_IO;

with Ada.Characters.Latin_1;

with Ada.Command_Line; use Ada.Command_Line;

with Ada.Strings.Unbounded;

with ncurses2.util; use ncurses2.util;
with ncurses2.getch_test;
with ncurses2.attr_test;
with ncurses2.color_test;
with ncurses2.demo_panels;
with ncurses2.color_edit;
with ncurses2.slk_test;
with ncurses2.acs_display;
with ncurses2.acs_and_scroll;
with ncurses2.flushinp_test;
with ncurses2.test_sgr_attributes;
with ncurses2.menu_test;
with ncurses2.demo_pad;
with ncurses2.demo_forms;
with ncurses2.overlap_test;
with ncurses2.trace_set;

with ncurses2.getopt; use ncurses2.getopt;

package body ncurses2.m is

   function To_trace (n : Integer) return Trace_Attribute_Set;
   procedure usage;
   procedure Set_Terminal_Modes;
   function Do_Single_Test (c : Character) return Boolean;

   function To_trace (n : Integer) return Trace_Attribute_Set is
      a : Trace_Attribute_Set := (others => False);
      m : Integer;
      rest : Integer;
   begin
      m := n  mod 2;
      if 1 = m then
         a.Times := True;
      end if;
      rest := n / 2;

      m := rest mod 2;
      if 1 = m then
         a.Tputs := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Update := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Cursor_Move := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Character_Output := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Calls := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Virtual_Puts := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Input_Events := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.TTY_State := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Internal_Calls := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Character_Calls := True;
      end if;
      rest := rest / 2;
      m := rest mod 2;
      if 1 = m then
         a.Termcap_TermInfo := True;
      end if;

      return a;
   end To_trace;

   --   these are type Stdscr_Init_Proc;

   function rip_footer (
                        Win : Window;
                        Columns : Column_Count) return Integer;
   pragma Convention (C, rip_footer);

   function rip_footer (
                        Win : Window;
                        Columns : Column_Count) return Integer is
   begin
      Set_Background (Win, (Ch => ' ',
                            Attr => (Reverse_Video => True, others => False),
                            Color => 0));
      Erase (Win);
      Move_Cursor (Win, 0, 0);
      Add (Win, "footer:"  & Columns'Img & " columns");
      Refresh_Without_Update (Win);
      return 0; -- Curses_OK;
   end rip_footer;

   function rip_header (
                        Win : Window;
                        Columns : Column_Count) return Integer;
   pragma Convention (C, rip_header);

   function rip_header (
                        Win : Window;
                        Columns : Column_Count) return Integer is
   begin
      Set_Background (Win, (Ch => ' ',
                            Attr => (Reverse_Video => True, others => False),
                            Color => 0));
      Erase (Win);
      Move_Cursor (Win, 0, 0);
      Add (Win, "header:"  & Columns'Img & " columns");
      --  'Img is a GNAT extension
      Refresh_Without_Update (Win);
      return 0; -- Curses_OK;
   end rip_header;

   procedure usage is
      --  type Stringa is access String;
      use Ada.Strings.Unbounded;
      --  tbl : constant array (Positive range <>) of Stringa := (
      tbl : constant array (Positive range <>) of Unbounded_String
        := (
            To_Unbounded_String ("Usage: ncurses [options]"),
            To_Unbounded_String (""),
            To_Unbounded_String ("Options:"),
            To_Unbounded_String ("  -a f,b   set default-colors " &
                                 "(assumed white-on-black)"),
            To_Unbounded_String ("  -d       use default-colors if terminal " &
                                 "supports them"),
            To_Unbounded_String ("  -e fmt   specify format for soft-keys " &
                                 "test (e)"),
            To_Unbounded_String ("  -f       rip-off footer line " &
                                 "(can repeat)"),
            To_Unbounded_String ("  -h       rip-off header line " &
                                 "(can repeat)"),
            To_Unbounded_String ("  -s msec  specify nominal time for " &
                                 "panel-demo (default: 1, to hold)"),
            To_Unbounded_String ("  -t mask  specify default trace-level " &
                                 "(may toggle with ^T)")
            );
   begin
      for n in tbl'Range loop
         Put_Line (Standard_Error, To_String (tbl (n)));
      end loop;
      --     exit(EXIT_FAILURE);
      --  TODO should we use Set_Exit_Status and throw and exception?
   end usage;

   procedure Set_Terminal_Modes is begin
      Set_Raw_Mode (SwitchOn => False);
      Set_Cbreak_Mode (SwitchOn => True);
      Set_Echo_Mode (SwitchOn => False);
      Allow_Scrolling (Mode => True);
      Use_Insert_Delete_Line (Do_Idl => True);
      Set_KeyPad_Mode (SwitchOn => True);
   end Set_Terminal_Modes;

   nap_msec : Integer := 1;

   function Do_Single_Test (c : Character) return Boolean is
   begin
      case c is
         when 'a' =>
            getch_test;
         when 'b' =>
            attr_test;
         when 'c' =>
            if not Has_Colors then
               Cannot ("does not support color.");
            else
               color_test;
            end if;
         when 'd' =>
            if not Has_Colors then
               Cannot ("does not support color.");
            elsif not Can_Change_Color then
               Cannot ("has hardwired color values.");
            else
               color_edit;
            end if;
         when 'e' =>
            slk_test;
         when 'f' =>
            acs_display;
         when 'o' =>
            demo_panels (nap_msec);
         when 'g' =>
            acs_and_scroll;
         when 'i' =>
            flushinp_test (Standard_Window);
         when 'k' =>
            test_sgr_attributes;
         when 'm' =>
            menu_test;
         when 'p' =>
            demo_pad;
         when 'r' =>
            demo_forms;
         when 's' =>
            overlap_test;
         when 't' =>
            trace_set;
         when '?' =>
            null;
         when others => return False;
      end case;
      return True;
   end Do_Single_Test;

   command : Character;
   my_e_param : Soft_Label_Key_Format := Four_Four;
   assumed_colors : Boolean := False;
   default_colors : Boolean := False;
   default_fg : Color_Number := White;
   default_bg : Color_Number := Black;
   --  nap_msec was an unsigned long integer in the C version,
   --  yet napms only takes an int!

   c : Integer;
   c2 : Character;
   optind : Integer := 1; -- must be initialized to one.
   optarg : getopt.stringa;

   length : Integer;
   tmpi : Integer;

   package myio is new Ada.Text_IO.Integer_IO (Integer);

   save_trace : Integer := 0;
   save_trace_set : Trace_Attribute_Set;

   function main return Integer is
   begin
      loop
         Qgetopt (c, Argument_Count, Argument'Access,
                  "a:de:fhs:t:", optind, optarg);
         exit when c = -1;
         c2 := Character'Val (c);
         case c2 is
            when 'a' =>
               --  Ada doesn't have scanf, it doesn't even have a
               --  regular expression library.
               assumed_colors := True;
               myio.Get (optarg.all, Integer (default_fg), length);
               myio.Get (optarg.all (length + 2 .. optarg.all'Length),
                         Integer (default_bg), length);
            when 'd' =>
               default_colors := True;
            when 'e' =>
               myio.Get (optarg.all, tmpi, length);
               if tmpi > 3 then
                  usage;
                  return 1;
               end if;
               my_e_param := Soft_Label_Key_Format'Val (tmpi);
            when 'f' =>
               Rip_Off_Lines (-1, rip_footer'Access);
            when 'h' =>
               Rip_Off_Lines (1, rip_header'Access);
            when 's' =>
               myio.Get (optarg.all, nap_msec, length);
            when 't' =>
               myio.Get (optarg.all, save_trace, length);
            when others =>
               usage;
               return 1;
         end case;
      end loop;

      --  the C version had a bunch of macros here.

      --   if (!isatty(fileno(stdin)))
      --   isatty is not available in the standard Ada so skip it.
      save_trace_set := To_trace (save_trace);
      Trace_On (save_trace_set);

      Init_Soft_Label_Keys (my_e_param);

      Init_Screen;
      Set_Background (Ch => (Ch    => Blank,
                             Attr  => Normal_Video,
                             Color => Color_Pair'First));

      if Has_Colors then
         Start_Color;
         if default_colors then
            Use_Default_Colors;
         elsif assumed_colors then
            Assume_Default_Colors (default_fg, default_bg);
         end if;
      end if;

      Set_Terminal_Modes;
      Save_Curses_Mode (Curses);

      End_Windows;

      --  TODO add macro #if blocks.
      Put_Line ("Welcome to " & Curses_Version & ".  Press ? for help.");

      loop
         Put_Line ("This is the ncurses main menu");
         Put_Line ("a = keyboard and mouse input test");
         Put_Line ("b = character attribute test");
         Put_Line ("c = color test pattern");
         Put_Line ("d = edit RGB color values");
         Put_Line ("e = exercise soft keys");
         Put_Line ("f = display ACS characters");
         Put_Line ("g = display windows and scrolling");
         Put_Line ("i = test of flushinp()");
         Put_Line ("k = display character attributes");
         Put_Line ("m = menu code test");
         Put_Line ("o = exercise panels library");
         Put_Line ("p = exercise pad features");
         Put_Line ("q = quit");
         Put_Line ("r = exercise forms code");
         Put_Line ("s = overlapping-refresh test");
         Put_Line ("t = set trace level");
         Put_Line ("? = repeat this command summary");

         Put ("> ");
         Flush;

         command := Ada.Characters.Latin_1.NUL;
         --              get_input:
         --              loop
         declare
            Ch : Character;
         begin
            Get (Ch);
            --  TODO if read(ch) <= 0
            --  TODO ada doesn't have an Is_Space function
            command := Ch;
            --  TODO if ch = '\n' or '\r' are these in Ada?
         end;
         --              end loop get_input;

         declare
         begin
            if Do_Single_Test (command) then
               Flush_Input;
               Set_Terminal_Modes;
               Reset_Curses_Mode (Curses);
               Clear;
               Refresh;
               End_Windows;
               if command = '?' then
                  Put_Line ("This is the ncurses capability tester.");
                  Put_Line ("You may select a test from the main menu by " &
                            "typing the");
                  Put_Line ("key letter of the choice (the letter to left " &
                            "of the =)");
                  Put_Line ("at the > prompt.  The commands `x' or `q' will " &
                            "exit.");
               end if;
               --  continue; --why continue in the C version?
            end if;
         exception
            when Curses_Exception => End_Windows;
         end;

         exit when command = 'q';
      end loop;
      Curses_Free_All;
      return 0; -- TODO ExitProgram(EXIT_SUCCESS);
   end main;

end ncurses2.m;
