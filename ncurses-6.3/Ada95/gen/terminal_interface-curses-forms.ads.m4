--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-forms__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Form                      --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2009,2014 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.34 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System;
with Ada.Characters.Latin_1;

package Terminal_Interface.Curses.Forms is
   pragma Preelaborate (Terminal_Interface.Curses.Forms);
   pragma Linker_Options ("-lform" & Curses_Constants.DFT_ARG_SUFFIX);

   Space : Character renames Ada.Characters.Latin_1.Space;

   type Field        is private;
   type Form         is private;

   Null_Field        : constant Field;
   Null_Form         : constant Form;

   type Field_Justification is (None,
                                Left,
                                Center,
                                Right);

   type Field_Option_Set is
      record
         Visible   : Boolean;
         Active    : Boolean;
         Public    : Boolean;
         Edit      : Boolean;
         Wrap      : Boolean;
         Blank     : Boolean;
         Auto_Skip : Boolean;
         Null_Ok   : Boolean;
         Pass_Ok   : Boolean;
         Static    : Boolean;
      end record;
   pragma Convention (C_Pass_By_Copy, Field_Option_Set);

   for Field_Option_Set use
      record
         Visible   at 0 range Curses_Constants.O_VISIBLE_First
           .. Curses_Constants.O_VISIBLE_Last;
         Active    at 0 range Curses_Constants.O_ACTIVE_First
           .. Curses_Constants.O_ACTIVE_Last;
         Public    at 0 range Curses_Constants.O_PUBLIC_First
           .. Curses_Constants.O_PUBLIC_Last;
         Edit      at 0 range Curses_Constants.O_EDIT_First
           .. Curses_Constants.O_EDIT_Last;
         Wrap      at 0 range Curses_Constants.O_WRAP_First
           .. Curses_Constants.O_WRAP_Last;
         Blank     at 0 range Curses_Constants.O_BLANK_First
           .. Curses_Constants.O_BLANK_Last;
         Auto_Skip at 0 range Curses_Constants.O_AUTOSKIP_First
           .. Curses_Constants.O_AUTOSKIP_Last;
         Null_Ok   at 0 range Curses_Constants.O_NULLOK_First
           .. Curses_Constants.O_NULLOK_Last;
         Pass_Ok   at 0 range Curses_Constants.O_PASSOK_First
           .. Curses_Constants.O_PASSOK_Last;
         Static    at 0 range Curses_Constants.O_STATIC_First
           .. Curses_Constants.O_STATIC_Last;
      end record;
   pragma Warnings (Off);
   for Field_Option_Set'Size use Curses_Constants.Field_Options_Size;
   pragma Warnings (On);

   function Default_Field_Options return Field_Option_Set;
   --  The initial defaults for the field options.
   pragma Inline (Default_Field_Options);

   type Form_Option_Set is
      record
         NL_Overload : Boolean;
         BS_Overload : Boolean;
      end record;
   pragma Convention (C_Pass_By_Copy, Form_Option_Set);

   for Form_Option_Set use
      record
         NL_Overload at 0 range Curses_Constants.O_NL_OVERLOAD_First
           .. Curses_Constants.O_NL_OVERLOAD_Last;
         BS_Overload at 0 range Curses_Constants.O_BS_OVERLOAD_First
           .. Curses_Constants.O_BS_OVERLOAD_Last;
      end record;
   pragma Warnings (Off);
   for Form_Option_Set'Size use Curses_Constants.Field_Options_Size;
   pragma Warnings (On);

   function Default_Form_Options return Form_Option_Set;
   --  The initial defaults for the form options.
   pragma Inline (Default_Form_Options);

   type Buffer_Number is new Natural;

   type Field_Array is array (Positive range <>) of aliased Field;
   pragma Convention (C, Field_Array);

   type Field_Array_Access is access Field_Array;

   procedure Free (FA          : in out Field_Array_Access;
                   Free_Fields : Boolean := False);
   --  Release the memory for an allocated field array
   --  If Free_Fields is True, call Delete() for all the fields in
   --  the array.

   subtype Form_Request_Code is Key_Code range (Key_Max + 1) .. (Key_Max + 57);

   --  The prefix F_ stands for "Form Request"
   F_Next_Page                : constant Form_Request_Code := Key_Max + 1;
   F_Previous_Page            : constant Form_Request_Code := Key_Max + 2;
   F_First_Page               : constant Form_Request_Code := Key_Max + 3;
   F_Last_Page                : constant Form_Request_Code := Key_Max + 4;

   F_Next_Field               : constant Form_Request_Code := Key_Max + 5;
   F_Previous_Field           : constant Form_Request_Code := Key_Max + 6;
   F_First_Field              : constant Form_Request_Code := Key_Max + 7;
   F_Last_Field               : constant Form_Request_Code := Key_Max + 8;
   F_Sorted_Next_Field        : constant Form_Request_Code := Key_Max + 9;
   F_Sorted_Previous_Field    : constant Form_Request_Code := Key_Max + 10;
   F_Sorted_First_Field       : constant Form_Request_Code := Key_Max + 11;
   F_Sorted_Last_Field        : constant Form_Request_Code := Key_Max + 12;
   F_Left_Field               : constant Form_Request_Code := Key_Max + 13;
   F_Right_Field              : constant Form_Request_Code := Key_Max + 14;
   F_Up_Field                 : constant Form_Request_Code := Key_Max + 15;
   F_Down_Field               : constant Form_Request_Code := Key_Max + 16;

   F_Next_Char                : constant Form_Request_Code := Key_Max + 17;
   F_Previous_Char            : constant Form_Request_Code := Key_Max + 18;
   F_Next_Line                : constant Form_Request_Code := Key_Max + 19;
   F_Previous_Line            : constant Form_Request_Code := Key_Max + 20;
   F_Next_Word                : constant Form_Request_Code := Key_Max + 21;
   F_Previous_Word            : constant Form_Request_Code := Key_Max + 22;
   F_Begin_Field              : constant Form_Request_Code := Key_Max + 23;
   F_End_Field                : constant Form_Request_Code := Key_Max + 24;
   F_Begin_Line               : constant Form_Request_Code := Key_Max + 25;
   F_End_Line                 : constant Form_Request_Code := Key_Max + 26;
   F_Left_Char                : constant Form_Request_Code := Key_Max + 27;
   F_Right_Char               : constant Form_Request_Code := Key_Max + 28;
   F_Up_Char                  : constant Form_Request_Code := Key_Max + 29;
   F_Down_Char                : constant Form_Request_Code := Key_Max + 30;

   F_New_Line                 : constant Form_Request_Code := Key_Max + 31;
   F_Insert_Char              : constant Form_Request_Code := Key_Max + 32;
   F_Insert_Line              : constant Form_Request_Code := Key_Max + 33;
   F_Delete_Char              : constant Form_Request_Code := Key_Max + 34;
   F_Delete_Previous          : constant Form_Request_Code := Key_Max + 35;
   F_Delete_Line              : constant Form_Request_Code := Key_Max + 36;
   F_Delete_Word              : constant Form_Request_Code := Key_Max + 37;
   F_Clear_EOL                : constant Form_Request_Code := Key_Max + 38;
   F_Clear_EOF                : constant Form_Request_Code := Key_Max + 39;
   F_Clear_Field              : constant Form_Request_Code := Key_Max + 40;
   F_Overlay_Mode             : constant Form_Request_Code := Key_Max + 41;
   F_Insert_Mode              : constant Form_Request_Code := Key_Max + 42;

   --  Vertical Scrolling
   F_ScrollForward_Line       : constant Form_Request_Code := Key_Max + 43;
   F_ScrollBackward_Line      : constant Form_Request_Code := Key_Max + 44;
   F_ScrollForward_Page       : constant Form_Request_Code := Key_Max + 45;
   F_ScrollBackward_Page      : constant Form_Request_Code := Key_Max + 46;
   F_ScrollForward_HalfPage   : constant Form_Request_Code := Key_Max + 47;
   F_ScrollBackward_HalfPage  : constant Form_Request_Code := Key_Max + 48;

   --  Horizontal Scrolling
   F_HScrollForward_Char      : constant Form_Request_Code := Key_Max + 49;
   F_HScrollBackward_Char     : constant Form_Request_Code := Key_Max + 50;
   F_HScrollForward_Line      : constant Form_Request_Code := Key_Max + 51;
   F_HScrollBackward_Line     : constant Form_Request_Code := Key_Max + 52;
   F_HScrollForward_HalfLine  : constant Form_Request_Code := Key_Max + 53;
   F_HScrollBackward_HalfLine : constant Form_Request_Code := Key_Max + 54;

   F_Validate_Field           : constant Form_Request_Code := Key_Max + 55;
   F_Next_Choice              : constant Form_Request_Code := Key_Max + 56;
   F_Previous_Choice          : constant Form_Request_Code := Key_Max + 57;

   --  For those who like the old 'C' style request names
   REQ_NEXT_PAGE    : Form_Request_Code renames F_Next_Page;
   REQ_PREV_PAGE    : Form_Request_Code renames F_Previous_Page;
   REQ_FIRST_PAGE   : Form_Request_Code renames F_First_Page;
   REQ_LAST_PAGE    : Form_Request_Code renames F_Last_Page;

   REQ_NEXT_FIELD   : Form_Request_Code renames F_Next_Field;
   REQ_PREV_FIELD   : Form_Request_Code renames F_Previous_Field;
   REQ_FIRST_FIELD  : Form_Request_Code renames F_First_Field;
   REQ_LAST_FIELD   : Form_Request_Code renames F_Last_Field;
   REQ_SNEXT_FIELD  : Form_Request_Code renames F_Sorted_Next_Field;
   REQ_SPREV_FIELD  : Form_Request_Code renames F_Sorted_Previous_Field;
   REQ_SFIRST_FIELD : Form_Request_Code renames F_Sorted_First_Field;
   REQ_SLAST_FIELD  : Form_Request_Code renames F_Sorted_Last_Field;
   REQ_LEFT_FIELD   : Form_Request_Code renames F_Left_Field;
   REQ_RIGHT_FIELD  : Form_Request_Code renames F_Right_Field;
   REQ_UP_FIELD     : Form_Request_Code renames F_Up_Field;
   REQ_DOWN_FIELD   : Form_Request_Code renames F_Down_Field;

   REQ_NEXT_CHAR    : Form_Request_Code renames F_Next_Char;
   REQ_PREV_CHAR    : Form_Request_Code renames F_Previous_Char;
   REQ_NEXT_LINE    : Form_Request_Code renames F_Next_Line;
   REQ_PREV_LINE    : Form_Request_Code renames F_Previous_Line;
   REQ_NEXT_WORD    : Form_Request_Code renames F_Next_Word;
   REQ_PREV_WORD    : Form_Request_Code renames F_Previous_Word;
   REQ_BEG_FIELD    : Form_Request_Code renames F_Begin_Field;
   REQ_END_FIELD    : Form_Request_Code renames F_End_Field;
   REQ_BEG_LINE     : Form_Request_Code renames F_Begin_Line;
   REQ_END_LINE     : Form_Request_Code renames F_End_Line;
   REQ_LEFT_CHAR    : Form_Request_Code renames F_Left_Char;
   REQ_RIGHT_CHAR   : Form_Request_Code renames F_Right_Char;
   REQ_UP_CHAR      : Form_Request_Code renames F_Up_Char;
   REQ_DOWN_CHAR    : Form_Request_Code renames F_Down_Char;

   REQ_NEW_LINE     : Form_Request_Code renames F_New_Line;
   REQ_INS_CHAR     : Form_Request_Code renames F_Insert_Char;
   REQ_INS_LINE     : Form_Request_Code renames F_Insert_Line;
   REQ_DEL_CHAR     : Form_Request_Code renames F_Delete_Char;
   REQ_DEL_PREV     : Form_Request_Code renames F_Delete_Previous;
   REQ_DEL_LINE     : Form_Request_Code renames F_Delete_Line;
   REQ_DEL_WORD     : Form_Request_Code renames F_Delete_Word;
   REQ_CLR_EOL      : Form_Request_Code renames F_Clear_EOL;
   REQ_CLR_EOF      : Form_Request_Code renames F_Clear_EOF;
   REQ_CLR_FIELD    : Form_Request_Code renames F_Clear_Field;
   REQ_OVL_MODE     : Form_Request_Code renames F_Overlay_Mode;
   REQ_INS_MODE     : Form_Request_Code renames F_Insert_Mode;

   REQ_SCR_FLINE    : Form_Request_Code renames F_ScrollForward_Line;
   REQ_SCR_BLINE    : Form_Request_Code renames F_ScrollBackward_Line;
   REQ_SCR_FPAGE    : Form_Request_Code renames F_ScrollForward_Page;
   REQ_SCR_BPAGE    : Form_Request_Code renames F_ScrollBackward_Page;
   REQ_SCR_FHPAGE   : Form_Request_Code renames F_ScrollForward_HalfPage;
   REQ_SCR_BHPAGE   : Form_Request_Code renames F_ScrollBackward_HalfPage;

   REQ_SCR_FCHAR    : Form_Request_Code renames F_HScrollForward_Char;
   REQ_SCR_BCHAR    : Form_Request_Code renames F_HScrollBackward_Char;
   REQ_SCR_HFLINE   : Form_Request_Code renames F_HScrollForward_Line;
   REQ_SCR_HBLINE   : Form_Request_Code renames F_HScrollBackward_Line;
   REQ_SCR_HFHALF   : Form_Request_Code renames F_HScrollForward_HalfLine;
   REQ_SCR_HBHALF   : Form_Request_Code renames F_HScrollBackward_HalfLine;

   REQ_VALIDATION   : Form_Request_Code renames F_Validate_Field;
   REQ_NEXT_CHOICE  : Form_Request_Code renames F_Next_Choice;
   REQ_PREV_CHOICE  : Form_Request_Code renames F_Previous_Choice;

   procedure Request_Name (Key  : Form_Request_Code;
                           Name : out String);

   function  Request_Name (Key : Form_Request_Code) return String;
   --  Same as function
   pragma Inline (Request_Name);

   ------------------
   --  Exceptions  --
   ------------------
   Form_Exception : exception;

   --  MANPAGE(`form_field_new.3x')

   --  ANCHOR(`new_field()',`Create')
   function Create (Height       : Line_Count;
                    Width        : Column_Count;
                    Top          : Line_Position;
                    Left         : Column_Position;
                    Off_Screen   : Natural := 0;
                    More_Buffers : Buffer_Number := Buffer_Number'First)
                    return Field;
   --  AKA
   --  An overloaded Create is defined later. Pragma Inline appears there.

   --  ANCHOR(`new_field()',`New_Field')
   function New_Field (Height       : Line_Count;
                       Width        : Column_Count;
                       Top          : Line_Position;
                       Left         : Column_Position;
                       Off_Screen   : Natural := 0;
                       More_Buffers : Buffer_Number := Buffer_Number'First)
                       return Field renames Create;
   --  AKA
   pragma Inline (New_Field);

   --  ANCHOR(`free_field()',`Delete')
   procedure Delete (Fld : in out Field);
   --  AKA
   --  Reset Fld to Null_Field
   --  An overloaded Delete is defined later. Pragma Inline appears there.

   --  ANCHOR(`dup_field()',`Duplicate')
   function Duplicate (Fld  : Field;
                       Top  : Line_Position;
                       Left : Column_Position) return Field;
   --  AKA
   pragma Inline (Duplicate);

   --  ANCHOR(`link_field()',`Link')
   function Link (Fld  : Field;
                  Top  : Line_Position;
                  Left : Column_Position) return Field;
   --  AKA
   pragma Inline (Link);

   --  MANPAGE(`form_field_just.3x')

   --  ANCHOR(`set_field_just()',`Set_Justification')
   procedure Set_Justification (Fld  : Field;
                                Just : Field_Justification := None);
   --  AKA
   pragma Inline (Set_Justification);

   --  ANCHOR(`field_just()',`Get_Justification')
   function Get_Justification (Fld : Field) return Field_Justification;
   --  AKA
   pragma Inline (Get_Justification);

   --  MANPAGE(`form_field_buffer.3x')

   --  ANCHOR(`set_field_buffer()',`Set_Buffer')
   procedure Set_Buffer
     (Fld    : Field;
      Buffer : Buffer_Number := Buffer_Number'First;
      Str    : String);
   --  AKA
   --  Not inlined

   --  ANCHOR(`field_buffer()',`Get_Buffer')
   procedure Get_Buffer
     (Fld    : Field;
      Buffer : Buffer_Number := Buffer_Number'First;
      Str    : out String);
   --  AKA

   function Get_Buffer
     (Fld    : Field;
      Buffer : Buffer_Number := Buffer_Number'First) return String;
   --  AKA
   --  Same but as function
   pragma Inline (Get_Buffer);

   --  ANCHOR(`set_field_status()',`Set_Status')
   procedure Set_Status (Fld    : Field;
                         Status : Boolean := True);
   --  AKA
   pragma Inline (Set_Status);

   --  ANCHOR(`field_status()',`Changed')
   function Changed (Fld : Field) return Boolean;
   --  AKA
   pragma Inline (Changed);

   --  ANCHOR(`set_field_max()',`Set_Maximum_Size')
   procedure Set_Maximum_Size (Fld : Field;
                               Max : Natural := 0);
   --  AKA
   pragma Inline (Set_Maximum_Size);

   --  MANPAGE(`form_field_opts.3x')

   --  ANCHOR(`set_field_opts()',`Set_Options')
   procedure Set_Options (Fld     : Field;
                          Options : Field_Option_Set);
   --  AKA
   --  An overloaded version is defined later. Pragma Inline appears there

   --  ANCHOR(`field_opts_on()',`Switch_Options')
   procedure Switch_Options (Fld     : Field;
                             Options : Field_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`field_opts_off()')
   --  An overloaded version is defined later. Pragma Inline appears there

   --  ANCHOR(`field_opts()',`Get_Options')
   procedure Get_Options (Fld     : Field;
                          Options : out Field_Option_Set);
   --  AKA

   --  ANCHOR(`field_opts()',`Get_Options')
   function Get_Options (Fld : Field := Null_Field)
                         return Field_Option_Set;
   --  AKA
   --  An overloaded version is defined later. Pragma Inline appears there

   --  MANPAGE(`form_field_attributes.3x')

   --  ANCHOR(`set_field_fore()',`Set_Foreground')
   procedure Set_Foreground
     (Fld   : Field;
      Fore  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Foreground);

   --  ANCHOR(`field_fore()',`Foreground')
   procedure Foreground (Fld  : Field;
                         Fore : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`field_fore()',`Foreground')
   procedure Foreground (Fld   : Field;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA
   pragma Inline (Foreground);

   --  ANCHOR(`set_field_back()',`Set_Background')
   procedure Set_Background
     (Fld   : Field;
      Back  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Background);

   --  ANCHOR(`field_back()',`Background')
   procedure Background (Fld  : Field;
                         Back : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`field_back()',`Background')
   procedure Background (Fld   : Field;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA
   pragma Inline (Background);

   --  ANCHOR(`set_field_pad()',`Set_Pad_Character')
   procedure Set_Pad_Character (Fld : Field;
                                Pad : Character := Space);
   --  AKA
   pragma Inline (Set_Pad_Character);

   --  ANCHOR(`field_pad()',`Pad_Character')
   procedure Pad_Character (Fld : Field;
                            Pad : out Character);
   --  AKA
   pragma Inline (Pad_Character);

   --  MANPAGE(`form_field_info.3x')

   --  ANCHOR(`field_info()',`Info')
   procedure Info (Fld                : Field;
                   Lines              : out Line_Count;
                   Columns            : out Column_Count;
                   First_Row          : out Line_Position;
                   First_Column       : out Column_Position;
                   Off_Screen         : out Natural;
                   Additional_Buffers : out Buffer_Number);
   --  AKA
   pragma Inline (Info);

   --  ANCHOR(`dynamic_field_info()',`Dynamic_Info')
   procedure Dynamic_Info (Fld     : Field;
                           Lines   : out Line_Count;
                           Columns : out Column_Count;
                           Max     : out Natural);
   --  AKA
   pragma Inline (Dynamic_Info);

   --  MANPAGE(`form_win.3x')

   --  ANCHOR(`set_form_win()',`Set_Window')
   procedure Set_Window (Frm : Form;
                         Win : Window);
   --  AKA
   pragma Inline (Set_Window);

   --  ANCHOR(`form_win()',`Get_Window')
   function Get_Window (Frm : Form) return Window;
   --  AKA
   pragma Inline (Get_Window);

   --  ANCHOR(`set_form_sub()',`Set_Sub_Window')
   procedure Set_Sub_Window (Frm : Form;
                             Win : Window);
   --  AKA
   pragma Inline (Set_Sub_Window);

   --  ANCHOR(`form_sub()',`Get_Sub_Window')
   function Get_Sub_Window (Frm : Form) return Window;
   --  AKA
   pragma Inline (Get_Sub_Window);

   --  ANCHOR(`scale_form()',`Scale')
   procedure Scale (Frm     : Form;
                    Lines   : out Line_Count;
                    Columns : out Column_Count);
   --  AKA
   pragma Inline (Scale);

   --  MANPAGE(`form_hook.3x')

   type Form_Hook_Function is access procedure (Frm : Form);
   pragma Convention (C, Form_Hook_Function);

   --  ANCHOR(`set_field_init()',`Set_Field_Init_Hook')
   procedure Set_Field_Init_Hook (Frm  : Form;
                                  Proc : Form_Hook_Function);
   --  AKA
   pragma Inline (Set_Field_Init_Hook);

   --  ANCHOR(`set_field_term()',`Set_Field_Term_Hook')
   procedure Set_Field_Term_Hook (Frm  : Form;
                                  Proc : Form_Hook_Function);
   --  AKA
   pragma Inline (Set_Field_Term_Hook);

   --  ANCHOR(`set_form_init()',`Set_Form_Init_Hook')
   procedure Set_Form_Init_Hook (Frm  : Form;
                                 Proc : Form_Hook_Function);
   --  AKA
   pragma Inline (Set_Form_Init_Hook);

   --  ANCHOR(`set_form_term()',`Set_Form_Term_Hook')
   procedure Set_Form_Term_Hook (Frm  : Form;
                                 Proc : Form_Hook_Function);
   --  AKA
   pragma Inline (Set_Form_Term_Hook);

   --  ANCHOR(`field_init()',`Get_Field_Init_Hook')
   function Get_Field_Init_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Field_Init_Hook, "field_init");

   --  ANCHOR(`field_term()',`Get_Field_Term_Hook')
   function Get_Field_Term_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Field_Term_Hook, "field_term");

   --  ANCHOR(`form_init()',`Get_Form_Init_Hook')
   function Get_Form_Init_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Form_Init_Hook, "form_init");

   --  ANCHOR(`form_term()',`Get_Form_Term_Hook')
   function Get_Form_Term_Hook (Frm : Form) return Form_Hook_Function;
   --  AKA
   pragma Import (C, Get_Form_Term_Hook, "form_term");

   --  MANPAGE(`form_field.3x')

   --  ANCHOR(`set_form_fields()',`Redefine')
   procedure Redefine (Frm  : Form;
                       Flds : Field_Array_Access);
   --  AKA
   pragma Inline (Redefine);

   --  ANCHOR(`set_form_fields()',`Set_Fields')
   procedure Set_Fields (Frm  : Form;
                         Flds : Field_Array_Access) renames Redefine;
   --  AKA
   --  pragma Inline (Set_Fields);

   --  ANCHOR(`form_fields()',`Fields')
   function Fields (Frm   : Form;
                    Index : Positive) return Field;
   --  AKA
   pragma Inline (Fields);

   --  ANCHOR(`field_count()',`Field_Count')
   function Field_Count (Frm : Form) return Natural;
   --  AKA
   pragma Inline (Field_Count);

   --  ANCHOR(`move_field()',`Move')
   procedure Move (Fld    : Field;
                   Line   : Line_Position;
                   Column : Column_Position);
   --  AKA
   pragma Inline (Move);

   --  MANPAGE(`form_new.3x')

   --  ANCHOR(`new_form()',`Create')
   function Create (Fields : Field_Array_Access) return Form;
   --  AKA
   pragma Inline (Create);

   --  ANCHOR(`new_form()',`New_Form')
   function New_Form (Fields : Field_Array_Access) return Form
     renames Create;
   --  AKA
   --  pragma Inline (New_Form);

   --  ANCHOR(`free_form()',`Delete')
   procedure Delete (Frm : in out Form);
   --  AKA
   --  Reset Frm to Null_Form
   pragma Inline (Delete);

   --  MANPAGE(`form_opts.3x')

   --  ANCHOR(`set_form_opts()',`Set_Options')
   procedure Set_Options (Frm     : Form;
                          Options : Form_Option_Set);
   --  AKA
   pragma Inline (Set_Options);

   --  ANCHOR(`form_opts_on()',`Switch_Options')
   procedure Switch_Options (Frm     : Form;
                             Options : Form_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`form_opts_off()')
   pragma Inline (Switch_Options);

   --  ANCHOR(`form_opts()',`Get_Options')
   procedure Get_Options (Frm     : Form;
                          Options : out Form_Option_Set);
   --  AKA

   --  ANCHOR(`form_opts()',`Get_Options')
   function Get_Options (Frm : Form := Null_Form) return Form_Option_Set;
   --  AKA
   pragma Inline (Get_Options);

   --  MANPAGE(`form_post.3x')

   --  ANCHOR(`post_form()',`Post')
   procedure Post (Frm  : Form;
                   Post : Boolean := True);
   --  AKA
   --  ALIAS(`unpost_form()')
   pragma Inline (Post);

   --  MANPAGE(`form_cursor.3x')

   --  ANCHOR(`pos_form_cursor()',`Position_Cursor')
   procedure Position_Cursor (Frm : Form);
   --  AKA
   pragma Inline (Position_Cursor);

   --  MANPAGE(`form_data.3x')

   --  ANCHOR(`data_ahead()',`Data_Ahead')
   function Data_Ahead (Frm : Form) return Boolean;
   --  AKA
   pragma Inline (Data_Ahead);

   --  ANCHOR(`data_behind()',`Data_Behind')
   function Data_Behind (Frm : Form) return Boolean;
   --  AKA
   pragma Inline (Data_Behind);

   --  MANPAGE(`form_driver.3x')

   type Driver_Result is (Form_Ok,
                          Request_Denied,
                          Unknown_Request,
                          Invalid_Field);

   --  ANCHOR(`form_driver()',`Driver')
   function Driver (Frm : Form;
                    Key : Key_Code) return Driver_Result;
   --  AKA
   --  Driver not inlined

   --  MANPAGE(`form_page.3x')

   type Page_Number is new Natural;

   --  ANCHOR(`set_current_field()',`Set_Current')
   procedure Set_Current (Frm : Form;
                          Fld : Field);
   --  AKA
   pragma Inline (Set_Current);

   --  ANCHOR(`current_field()',`Current')
   function Current (Frm : Form) return Field;
   --  AKA
   pragma Inline (Current);

   --  ANCHOR(`set_form_page()',`Set_Page')
   procedure Set_Page (Frm  : Form;
                       Page : Page_Number := Page_Number'First);
   --  AKA
   pragma Inline (Set_Page);

   --  ANCHOR(`form_page()',`Page')
   function Page (Frm : Form) return Page_Number;
   --  AKA
   pragma Inline (Page);

   --  ANCHOR(`field_index()',`Get_Index')
   function Get_Index (Fld : Field) return Positive;
   --  AKA
   --  Please note that in this binding we start the numbering of fields
   --  with 1. So this is number is one more than you get from the low
   --  level call.
   pragma Inline (Get_Index);

   --  MANPAGE(`form_new_page.3x')

   --  ANCHOR(`set_new_page()',`Set_New_Page')
   procedure Set_New_Page (Fld      : Field;
                           New_Page : Boolean := True);
   --  AKA
   pragma Inline (Set_New_Page);

   --  ANCHOR(`new_page()',`Is_New_Page')
   function Is_New_Page (Fld : Field) return Boolean;
   --  AKA
   pragma Inline (Is_New_Page);

   --  MANPAGE(`form_requestname.3x')
   --  Not Implemented: form_request_name, form_request_by_name

------------------------------------------------------------------------------
private
   type Field is new System.Storage_Elements.Integer_Address;
   type Form  is new System.Storage_Elements.Integer_Address;

   Null_Field : constant Field := 0;
   Null_Form  : constant Form  := 0;

end Terminal_Interface.Curses.Forms;
