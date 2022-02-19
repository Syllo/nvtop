------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020,2021 Thomas E. Dickey                                     --
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
--  $Revision: 1.9 $
--  $Date: 2021/09/04 10:52:55 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with ncurses2.util; use ncurses2.util;
with Terminal_Interface.Curses; use Terminal_Interface.Curses;
with Terminal_Interface.Curses.Forms; use Terminal_Interface.Curses.Forms;
with Terminal_Interface.Curses.Forms.Field_User_Data;
with Ada.Characters.Handling;
with Ada.Strings;
with Ada.Strings.Bounded;

procedure ncurses2.demo_forms is
   package BS is new Ada.Strings.Bounded.Generic_Bounded_Length (80);

   type myptr is access Integer;

   --  The C version stores a pointer in the userptr and
   --  converts it into a long integer.
   --  The correct, but inconvenient  way to do it is to use a
   --  pointer to long and keep the pointer constant.
   --  It just adds one memory piece to allocate and deallocate (not done here)

   package StringData is new
     Terminal_Interface.Curses.Forms.Field_User_Data (Integer, myptr);

   function edit_secure (me : Field; c_in : Key_Code) return Key_Code;
   function form_virtualize (f : Form; w : Window) return Key_Code;
   function my_form_driver (f : Form; c : Key_Code) return Boolean;
   function make_label (frow  : Line_Position;
                        fcol  : Column_Position;
                        label : String) return Field;
   function make_field (frow   : Line_Position;
                        fcol   : Column_Position;
                        rows   : Line_Count;
                        cols   : Column_Count;
                        secure : Boolean) return Field;
   procedure display_form (f : Form);
   procedure erase_form (f : Form);

   --  prints '*' instead of characters.
   --  Not that this keeps a bug from the C version:
   --  type in the psasword field then move off and back.
   --  the cursor is at position one, but
   --  this assumes it as at the end so text gets appended instead
   --  of overwtitting.
   function edit_secure (me : Field; c_in : Key_Code) return Key_Code is
      rows, frow : Line_Position;
      nrow : Natural;
      cols, fcol : Column_Position;
      nbuf : Buffer_Number;
      c : Key_Code := c_in;
      c2 :  Character;

      use StringData;
   begin
      Info (me, rows, cols, frow, fcol, nrow, nbuf);
      --  TODO         if result = Form_Ok and nbuf > 0 then
      --  C version checked the return value
      --  of Info, the Ada binding throws an exception I think.
      if nbuf > 0 then
         declare
            temp : BS.Bounded_String;
            temps : String (1 .. 10);
            --  TODO Get_Buffer povides no information on the field length?
            len : myptr;
         begin
            Get_Buffer (me, 1, Str => temps);
            --  strcpy(temp, field_buffer(me, 1));
            Get_User_Data (me, len);
            temp := BS.To_Bounded_String (temps (1 .. len.all));
            if c <= Key_Max then
               c2 := Code_To_Char (c);
               if Ada.Characters.Handling.Is_Graphic (c2) then
                  BS.Append (temp, c2);
                  len.all := len.all + 1;
                  Set_Buffer (me, 1, BS.To_String (temp));
                  c := Character'Pos ('*');
               else
                  c := 0;
               end if;
            else
               case c is
                  when  REQ_BEG_FIELD |
                    REQ_CLR_EOF |
                    REQ_CLR_EOL |
                    REQ_DEL_LINE |
                    REQ_DEL_WORD |
                    REQ_DOWN_CHAR |
                    REQ_END_FIELD |
                    REQ_INS_CHAR |
                    REQ_INS_LINE |
                    REQ_LEFT_CHAR |
                    REQ_NEW_LINE |
                    REQ_NEXT_WORD |
                    REQ_PREV_WORD |
                    REQ_RIGHT_CHAR |
                    REQ_UP_CHAR =>
                     c := 0;         -- we don't want to do inline editing
                  when REQ_CLR_FIELD =>
                     if len.all /= 0 then
                        temp := BS.To_Bounded_String ("");
                        Set_Buffer (me, 1, BS.To_String (temp));
                        len.all := 0;
                     end if;

                  when REQ_DEL_CHAR |
                    REQ_DEL_PREV =>
                     if len.all /= 0 then
                        BS.Delete (temp, BS.Length (temp), BS.Length (temp));
                        Set_Buffer (me, 1, BS.To_String (temp));
                        len.all := len.all - 1;
                     end if;
                  when others => null;
               end case;
            end if;
         end;
      end if;
      return c;
   end edit_secure;

   mode : Key_Code := REQ_INS_MODE;

   function form_virtualize (f : Form; w : Window) return Key_Code is
      type lookup_t is record
         code : Key_Code;
         result : Key_Code;
         --  should be Form_Request_Code, but we need MAX_COMMAND + 1
      end record;

      lookup : constant array (Positive range <>) of lookup_t :=
        (
         (
          Character'Pos ('A') mod 16#20#, REQ_NEXT_CHOICE
          ),
         (
          Character'Pos ('B') mod 16#20#, REQ_PREV_WORD
          ),
         (
          Character'Pos ('C') mod 16#20#, REQ_CLR_EOL
          ),
         (
          Character'Pos ('D') mod 16#20#, REQ_DOWN_FIELD
          ),
         (
          Character'Pos ('E') mod 16#20#, REQ_END_FIELD
          ),
         (
          Character'Pos ('F') mod 16#20#, REQ_NEXT_PAGE
          ),
         (
          Character'Pos ('G') mod 16#20#, REQ_DEL_WORD
          ),
         (
          Character'Pos ('H') mod 16#20#, REQ_DEL_PREV
          ),
         (
          Character'Pos ('I') mod 16#20#, REQ_INS_CHAR
          ),
         (
          Character'Pos ('K') mod 16#20#, REQ_CLR_EOF
          ),
         (
          Character'Pos ('L') mod 16#20#, REQ_LEFT_FIELD
          ),
         (
          Character'Pos ('M') mod 16#20#, REQ_NEW_LINE
          ),
         (
          Character'Pos ('N') mod 16#20#, REQ_NEXT_FIELD
          ),
         (
          Character'Pos ('O') mod 16#20#, REQ_INS_LINE
          ),
         (
          Character'Pos ('P') mod 16#20#, REQ_PREV_FIELD
          ),
         (
          Character'Pos ('R') mod 16#20#, REQ_RIGHT_FIELD
          ),
         (
          Character'Pos ('S') mod 16#20#, REQ_BEG_FIELD
          ),
         (
          Character'Pos ('U') mod 16#20#, REQ_UP_FIELD
          ),
         (
          Character'Pos ('V') mod 16#20#, REQ_DEL_CHAR
          ),
         (
          Character'Pos ('W') mod 16#20#, REQ_NEXT_WORD
          ),
         (
          Character'Pos ('X') mod 16#20#, REQ_CLR_FIELD
          ),
         (
          Character'Pos ('Y') mod 16#20#, REQ_DEL_LINE
          ),
         (
          Character'Pos ('Z') mod 16#20#, REQ_PREV_CHOICE
          ),
         (
          Character'Pos ('[') mod 16#20#, --  ESCAPE
          Form_Request_Code'Last + 1
          ),
         (
          Key_Backspace, REQ_DEL_PREV
          ),
         (
          KEY_DOWN, REQ_DOWN_CHAR
          ),
         (
          Key_End, REQ_LAST_FIELD
          ),
         (
          Key_Home, REQ_FIRST_FIELD
          ),
         (
          KEY_LEFT, REQ_LEFT_CHAR
          ),
         (
          KEY_LL, REQ_LAST_FIELD
          ),
         (
          Key_Next, REQ_NEXT_FIELD
          ),
         (
          KEY_NPAGE, REQ_NEXT_PAGE
          ),
         (
          KEY_PPAGE, REQ_PREV_PAGE
          ),
         (
          Key_Previous, REQ_PREV_FIELD
          ),
         (
          KEY_RIGHT, REQ_RIGHT_CHAR
          ),
         (
          KEY_UP, REQ_UP_CHAR
          ),
         (
          Character'Pos ('Q') mod 16#20#, --  QUIT
          Form_Request_Code'Last + 1      --  TODO MAX_FORM_COMMAND + 1
          )
         );

      c : Key_Code := Getchar (w);
      me : constant Field := Current (f);

   begin
      if c = Character'Pos (']') mod 16#20# then
         if mode = REQ_INS_MODE then
            mode := REQ_OVL_MODE;
         else
            mode := REQ_INS_MODE;
         end if;
         c := mode;
      else
         for n in lookup'Range loop
            if lookup (n).code = c then
               c := lookup (n).result;
               exit;
            end if;
         end loop;
      end if;

      --  Force the field that the user is typing into to be in reverse video,
      --  while the other fields are shown underlined.
      if c <= Key_Max then
         c := edit_secure (me, c);
         Set_Background (me, (Reverse_Video => True, others => False));
      elsif c <= Form_Request_Code'Last then
         c := edit_secure (me, c);
         Set_Background (me, (Under_Line => True, others => False));
      end if;
      return c;
   end form_virtualize;

   function my_form_driver (f : Form; c : Key_Code) return Boolean is
      flag : constant Driver_Result := Driver (f, F_Validate_Field);
   begin
      if c = Form_Request_Code'Last + 1 and
         flag = Form_Ok
      then
         return True;
      else
         Beep;
         return False;
      end if;
   end my_form_driver;

   function make_label (frow  : Line_Position;
                        fcol  : Column_Position;
                        label : String) return Field is
      f : constant Field := Create (1, label'Length, frow, fcol, 0, 0);
      o : Field_Option_Set := Get_Options (f);
   begin
      if f /= Null_Field then
         Set_Buffer (f, 0, label);
         o.Active := False;
         Set_Options (f, o);
      end if;
      return f;
   end make_label;

   function make_field (frow   : Line_Position;
                        fcol   : Column_Position;
                        rows   : Line_Count;
                        cols   : Column_Count;
                        secure : Boolean) return Field is
      f : Field;
      use StringData;
      len : myptr;
   begin
      if secure then
         f := Create (rows, cols, frow, fcol, 0, 1);
      else
         f := Create (rows, cols, frow, fcol, 0, 0);
      end if;

      if f /= Null_Field then
         Set_Background (f, (Under_Line => True, others => False));
         len := new Integer;
         len.all := 0;
         Set_User_Data (f, len);
      end if;
      return f;
   end make_field;

   procedure display_form (f : Form) is
      w : Window;
      rows : Line_Count;
      cols : Column_Count;
   begin
      Scale (f, rows, cols);

      w := New_Window (rows + 2, cols + 4, 0, 0);
      if w /= Null_Window then
         Set_Window (f, w);
         Set_Sub_Window (f, Derived_Window (w, rows, cols, 1, 2));
         Box (w); -- 0,0
         Set_KeyPad_Mode (w, True);
      end if;

      --  TODO if Post(f) /= Form_Ok then it is a procedure
      declare
      begin
         Post (f);
      exception
         when
           Eti_System_Error    |
           Eti_Bad_Argument    |
           Eti_Posted          |
           Eti_Connected       |
           Eti_Bad_State       |
           Eti_No_Room         |
           Eti_Not_Posted      |
           Eti_Unknown_Command |
           Eti_No_Match        |
           Eti_Not_Selectable  |
           Eti_Not_Connected   |
           Eti_Request_Denied  |
           Eti_Invalid_Field   |
           Eti_Current         =>
            Refresh (w);
      end;
      --  end if;
   end display_form;

   procedure erase_form (f : Form) is
      w : Window := Get_Window (f);
      s : Window := Get_Sub_Window (f);
   begin
      Post (f, False);
      Erase (w);
      Refresh (w);
      Delete (s);
      Delete (w);
   end erase_form;

   finished : Boolean := False;
   f : constant Field_Array_Access := new Field_Array (1 .. 12);
   secure : Field;
   myform : Form;
   w : Window;
   c : Key_Code;
   result : Driver_Result;
begin
   Move_Cursor (Line => 18, Column => 0);
   Add (Str => "Defined form-traversal keys:   ^Q/ESC- exit form");
   Add (Ch => newl);
   Add (Str => "^N   -- go to next field       ^P  -- go to previous field");
   Add (Ch => newl);
   Add (Str => "Home -- go to first field      End -- go to last field");
   Add (Ch => newl);
   Add (Str => "^L   -- go to field to left    ^R  -- go to field to right");
   Add (Ch => newl);
   Add (Str => "^U   -- move upward to field   ^D  -- move downward to field");
   Add (Ch => newl);
   Add (Str => "^W   -- go to next word        ^B  -- go to previous word");
   Add (Ch => newl);
   Add (Str => "^S   -- go to start of field   ^E  -- go to end of field");
   Add (Ch => newl);
   Add (Str => "^H   -- delete previous char   ^Y  -- delete line");
   Add (Ch => newl);
   Add (Str => "^G   -- delete current word    ^C  -- clear to end of line");
   Add (Ch => newl);
   Add (Str => "^K   -- clear to end of field  ^X  -- clear field");
   Add (Ch => newl);
   Add (Str => "Arrow keys move within a field as you would expect.");

   Add (Line => 4, Column => 57, Str => "Forms Entry Test");

   Refresh;

   --  describe the form
   f.all (1) := make_label (0, 15, "Sample Form");
   f.all (2) := make_label (2, 0, "Last Name");
   f.all (3) := make_field (3, 0, 1, 18, False);
   f.all (4) := make_label (2, 20, "First Name");
   f.all (5) := make_field (3, 20, 1, 12, False);
   f.all (6) := make_label (2, 34, "Middle Name");
   f.all (7) := make_field (3, 34, 1, 12, False);
   f.all (8) := make_label (5, 0, "Comments");
   f.all (9) := make_field (6, 0, 4, 46, False);
   f.all (10) := make_label (5, 20, "Password:");
   f.all (11) := make_field (5, 30, 1, 9, True);
   secure := f.all (11);
   f.all (12) := Null_Field;

   myform := New_Form (f);

   display_form (myform);

   w := Get_Window (myform);
   Set_Raw_Mode (SwitchOn => True);
   Set_NL_Mode (SwitchOn => True);     --  lets us read ^M's
   while not finished loop
      c := form_virtualize (myform, w);
      result := Driver (myform, c);
      case result is
         when Form_Ok =>
            Add (Line => 5, Column => 57, Str => Get_Buffer (secure, 1));
            Clear_To_End_Of_Line;
            Refresh;
         when Unknown_Request =>
            finished := my_form_driver (myform, c);
         when others =>
            Beep;
      end case;
   end loop;

   erase_form (myform);

   --  TODO Free_Form(myform);
   --     for (c = 0; f[c] != 0; c++) free_field(f[c]);
   Set_Raw_Mode (SwitchOn => False);
   Set_NL_Mode (SwitchOn => True);

end ncurses2.demo_forms;
