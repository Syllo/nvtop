------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            ncurses2.trace_set                            --
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
--  $Revision: 1.7 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Trace; use Terminal_Interface.Curses.Trace;
with Terminal_Interface.Curses.Menus; use Terminal_Interface.Curses.Menus;

with Ada.Strings.Bounded;

--  interactively set the trace level

procedure ncurses2.trace_set is

   function menu_virtualize (c : Key_Code) return Key_Code;
   function subset (super, sub : Trace_Attribute_Set) return Boolean;
   function trace_or (a, b : Trace_Attribute_Set) return Trace_Attribute_Set;
   function trace_num (tlevel : Trace_Attribute_Set) return String;
   function tracetrace (tlevel : Trace_Attribute_Set) return String;
   function run_trace_menu (m : Menu; count : Integer) return Boolean;

   function menu_virtualize (c : Key_Code) return Key_Code is
   begin
      case c is
         when Character'Pos (newl) | Key_Exit =>
            return Menu_Request_Code'Last + 1; --  MAX_COMMAND? TODO
         when Character'Pos ('u') =>
            return M_ScrollUp_Line;
         when Character'Pos ('d') =>
            return M_ScrollDown_Line;
         when Character'Pos ('b') | Key_Next_Page =>
            return M_ScrollUp_Page;
         when Character'Pos ('f') | Key_Previous_Page =>
            return M_ScrollDown_Page;
         when Character'Pos ('n') | Key_Cursor_Down =>
            return M_Next_Item;
         when Character'Pos ('p') | Key_Cursor_Up =>
            return M_Previous_Item;
         when Character'Pos (' ') =>
            return M_Toggle_Item;
         when Key_Mouse =>
            return c;
         when others =>
            Beep;
            return c;
      end case;
   end menu_virtualize;

   type string_a is access String;
   type tbl_entry is record
      name : string_a;
      mask : Trace_Attribute_Set;
   end record;

   t_tbl : constant array (Positive range <>) of tbl_entry :=
     (
      (new String'("Disable"),
       Trace_Disable),
      (new String'("Times"),
       Trace_Attribute_Set'(Times => True, others => False)),
      (new String'("Tputs"),
       Trace_Attribute_Set'(Tputs => True, others => False)),
      (new String'("Update"),
       Trace_Attribute_Set'(Update => True, others => False)),
      (new String'("Cursor_Move"),
       Trace_Attribute_Set'(Cursor_Move => True, others => False)),
      (new String'("Character_Output"),
       Trace_Attribute_Set'(Character_Output => True, others => False)),
      (new String'("Ordinary"),
       Trace_Ordinary),
      (new String'("Calls"),
       Trace_Attribute_Set'(Calls => True, others => False)),
      (new String'("Virtual_Puts"),
       Trace_Attribute_Set'(Virtual_Puts => True, others => False)),
      (new String'("Input_Events"),
       Trace_Attribute_Set'(Input_Events => True, others => False)),
      (new String'("TTY_State"),
       Trace_Attribute_Set'(TTY_State => True, others => False)),
      (new String'("Internal_Calls"),
       Trace_Attribute_Set'(Internal_Calls => True, others => False)),
      (new String'("Character_Calls"),
       Trace_Attribute_Set'(Character_Calls => True, others => False)),
      (new String'("Termcap_TermInfo"),
       Trace_Attribute_Set'(Termcap_TermInfo => True, others => False)),
      (new String'("Maximium"),
       Trace_Maximum)
      );

   package BS is new Ada.Strings.Bounded.Generic_Bounded_Length (300);

   function subset (super, sub : Trace_Attribute_Set) return Boolean is
   begin
      if
        (super.Times or not sub.Times) and
        (super.Tputs or not sub.Tputs) and
        (super.Update or not sub.Update) and
        (super.Cursor_Move or not sub.Cursor_Move) and
        (super.Character_Output or not sub.Character_Output) and
        (super.Calls or not sub.Calls) and
        (super.Virtual_Puts or not sub.Virtual_Puts) and
        (super.Input_Events or not sub.Input_Events) and
        (super.TTY_State or not sub.TTY_State) and
        (super.Internal_Calls or not sub.Internal_Calls) and
        (super.Character_Calls or not sub.Character_Calls) and
        (super.Termcap_TermInfo or not sub.Termcap_TermInfo) and
        True
      then
         return True;
      else
         return False;
      end if;
   end subset;

   function trace_or (a, b : Trace_Attribute_Set) return Trace_Attribute_Set is
      retval : Trace_Attribute_Set := Trace_Disable;
   begin
      retval.Times := (a.Times or b.Times);
      retval.Tputs := (a.Tputs or b.Tputs);
      retval.Update := (a.Update or b.Update);
      retval.Cursor_Move := (a.Cursor_Move or b.Cursor_Move);
      retval.Character_Output := (a.Character_Output or b.Character_Output);
      retval.Calls := (a.Calls or b.Calls);
      retval.Virtual_Puts := (a.Virtual_Puts or b.Virtual_Puts);
      retval.Input_Events := (a.Input_Events or b.Input_Events);
      retval.TTY_State := (a.TTY_State or b.TTY_State);
      retval.Internal_Calls := (a.Internal_Calls or b.Internal_Calls);
      retval.Character_Calls := (a.Character_Calls or b.Character_Calls);
      retval.Termcap_TermInfo := (a.Termcap_TermInfo or b.Termcap_TermInfo);

      return retval;
   end trace_or;

   --  Print the hexadecimal value of the mask so
   --  users can set it from the command line.

   function trace_num (tlevel : Trace_Attribute_Set) return String is
      result : Integer := 0;
      m : Integer := 1;
   begin

      if tlevel.Times then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Tputs then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Update then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Cursor_Move then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Character_Output then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Calls then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Virtual_Puts then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Input_Events then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.TTY_State then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Internal_Calls then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Character_Calls then
         result := result + m;
      end if;
      m := m * 2;

      if tlevel.Termcap_TermInfo then
         result := result + m;
      end if;
      m := m * 2;
      return result'Img;
   end trace_num;

   function tracetrace (tlevel : Trace_Attribute_Set) return String is

      use BS;
      buf : Bounded_String := To_Bounded_String ("");
   begin
      --  The C version prints the hexadecimal value of the mask, we
      --  won't do that here because this is Ada.

      if tlevel = Trace_Disable then
         Append (buf, "Trace_Disable");
      else

         if subset (tlevel,
                    Trace_Attribute_Set'(Times => True, others => False))
         then
            Append (buf, "Times");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Tputs => True, others => False))
         then
            Append (buf, "Tputs");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Update => True, others => False))
         then
            Append (buf, "Update");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Cursor_Move => True,
                                         others => False))
         then
            Append (buf, "Cursor_Move");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Character_Output => True,
                                         others => False))
         then
            Append (buf, "Character_Output");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Ordinary)
         then
            Append (buf, "Ordinary");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Calls => True, others => False))
         then
            Append (buf, "Calls");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Virtual_Puts => True,
                                         others => False))
         then
            Append (buf, "Virtual_Puts");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Input_Events => True,
                                         others => False))
         then
            Append (buf, "Input_Events");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(TTY_State => True,
                                         others => False))
         then
            Append (buf, "TTY_State");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Internal_Calls => True,
                                         others => False))
         then
            Append (buf, "Internal_Calls");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Character_Calls => True,
                                         others => False))
         then
            Append (buf, "Character_Calls");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Attribute_Set'(Termcap_TermInfo => True,
                                         others => False))
         then
            Append (buf, "Termcap_TermInfo");
            Append (buf, ", ");
         end if;

         if subset (tlevel,
                    Trace_Maximum)
         then
            Append (buf, "Maximium");
            Append (buf, ", ");
         end if;
      end if;

      if To_String (buf) (Length (buf) - 1) = ',' then
         Delete (buf, Length (buf) - 1, Length (buf));
      end if;

      return To_String (buf);
   end tracetrace;

   function run_trace_menu (m : Menu; count : Integer) return Boolean is
      i, p : Item;
      changed : Boolean;
      c, v : Key_Code;
   begin
      loop
         changed := (count /= 0);
         c := Getchar (Get_Window (m));
         v := menu_virtualize (c);
         case Driver (m, v) is
            when Unknown_Request =>
               return False;
            when others =>
               i := Current (m);
               if i = Menus.Items (m, 1) then -- the first item
                  for n in t_tbl'First + 1 .. t_tbl'Last loop
                     if Value (i) then
                        Set_Value (i, False);
                        changed := True;
                     end if;
                  end loop;
               else
                  for n in t_tbl'First + 1 .. t_tbl'Last loop
                     p := Menus.Items (m, n);
                     if Value (p) then
                        Set_Value (Menus.Items (m, 1), False);
                        changed := True;
                        exit;
                     end if;
                  end loop;
               end if;
               if not changed then
                  return True;
               end if;
         end case;
      end loop;
   end run_trace_menu;

   nc_tracing, mask : Trace_Attribute_Set;
   pragma Import (C, nc_tracing, "_nc_tracing");
   items_a : constant Item_Array_Access :=
     new Item_Array (t_tbl'First .. t_tbl'Last + 1);
   mrows : Line_Count;
   mcols : Column_Count;
   menuwin : Window;
   menu_y : constant Line_Position := 8;
   menu_x : constant Column_Position := 8;
   ip : Item;
   m : Menu;
   count : Integer;
   newtrace : Trace_Attribute_Set;
begin
   Add (Line => 0, Column => 0, Str => "Interactively set trace level:");
   Add (Line => 2, Column => 0,
        Str => "  Press space bar to toggle a selection.");
   Add (Line => 3, Column => 0,
        Str => "  Use up and down arrow to move the select bar.");
   Add (Line => 4, Column => 0,
        Str => "  Press return to set the trace level.");
   Add (Line => 6, Column => 0, Str => "(Current trace level is ");
   Add (Str => tracetrace (nc_tracing) & " numerically: " &
        trace_num (nc_tracing));
   Add (Ch => ')');

   Refresh;

   for n in t_tbl'Range loop
      items_a.all (n) := New_Item (t_tbl (n).name.all);
   end loop;
   items_a.all (t_tbl'Last + 1) := Null_Item;

   m := New_Menu (items_a);

   Set_Format (m, 16, 2);
   Scale (m, mrows, mcols);

   Switch_Options (m, (One_Valued => True, others => False), On => False);
   menuwin := New_Window (mrows + 2, mcols + 2, menu_y, menu_x);
   Set_Window (m, menuwin);
   Set_KeyPad_Mode (menuwin, SwitchOn => True);
   Box (menuwin);

   Set_Sub_Window (m, Derived_Window (menuwin, mrows, mcols, 1, 1));

   Post (m);

   for n in t_tbl'Range loop
      ip := Items (m, n);
      mask := t_tbl (n).mask;
      if mask = Trace_Disable then
         Set_Value (ip, nc_tracing = Trace_Disable);
      elsif subset (sub => mask, super => nc_tracing) then
         Set_Value (ip, True);
      end if;
   end loop;

   count := 1;
   while run_trace_menu (m, count) loop
      count := count + 1;
   end loop;

   newtrace := Trace_Disable;
   for n in t_tbl'Range loop
      ip := Items (m, n);
      if Value (ip) then
         mask := t_tbl (n).mask;
         newtrace := trace_or (newtrace, mask);
      end if;
   end loop;

   Trace_On (newtrace);
   Trace_Put ("trace level interactively set to " &
              tracetrace (nc_tracing));

   Move_Cursor (Line => Lines - 4, Column => 0);
   Add (Str => "Trace level is ");
   Add (Str => tracetrace (nc_tracing));
   Add (Ch => newl);
   Pause; -- was just Add(); Getchar

   Post (m, False);
   --  menuwin has subwindows I think, which makes an error.
   declare begin
      Delete (menuwin);
   exception when Curses_Exception => null; end;

   --  free_menu(m);
   --  free_item()
end ncurses2.trace_set;
