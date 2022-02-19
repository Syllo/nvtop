--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses__ads.htm')dnl
include(M4MACRO)------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                         Terminal_Interface.Curses                        --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2011,2014 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.48 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System.Storage_Elements;
with Interfaces.C;   --  We need this for some assertions.

with Terminal_Interface.Curses_Constants;

package Terminal_Interface.Curses is
   pragma Preelaborate (Terminal_Interface.Curses);
   pragma Linker_Options ("-lncurses" & Curses_Constants.DFT_ARG_SUFFIX);

   Major_Version : constant := Curses_Constants.NCURSES_VERSION_MAJOR;
   Minor_Version : constant := Curses_Constants.NCURSES_VERSION_MINOR;
   NC_Version : String renames Curses_Constants.Version;

   type Window is private;
   Null_Window : constant Window;

   type Line_Position   is new Integer; --  line coordinate
   type Column_Position is new Integer; --  column coordinate

   subtype Line_Count   is Line_Position   range 1 .. Line_Position'Last;
   --  Type to count lines. We do not allow null windows, so must be positive
   subtype Column_Count is Column_Position range 1 .. Column_Position'Last;
   --  Type to count columns. We do not allow null windows, so must be positive

   type Key_Code is new Integer;
   --  That is anything including real characters, special keys and logical
   --  request codes.

   --  FIXME: The "-1" should be Curses_Err
   subtype Real_Key_Code is Key_Code range -1 .. Curses_Constants.KEY_MAX;
   --  This are the codes that potentially represent a real keystroke.
   --  Not all codes may be possible on a specific terminal. To check the
   --  availability of a special key, the Has_Key function is provided.

   subtype Special_Key_Code is Real_Key_Code
     range Curses_Constants. KEY_MIN - 1 .. Real_Key_Code'Last;
   --  Type for a function- or special key number

   subtype Normal_Key_Code is Real_Key_Code range
     Character'Pos (Character'First) .. Character'Pos (Character'Last);
   --  This are the codes for regular (incl. non-graphical) characters.

   --  For those who like to use the original key names we produce them were
   --  they differ from the original.

   --  Constants for function- and special keys
   Key_None                    : constant Special_Key_Code
     := Curses_Constants.KEY_MIN - 1;
   Key_Min                     : constant Special_Key_Code
     := Curses_Constants.KEY_MIN;
   Key_Break                   : constant Special_Key_Code
     := Curses_Constants.KEY_BREAK;
   KEY_DOWN                    : constant Special_Key_Code
     := Curses_Constants.KEY_DOWN;
   Key_Cursor_Down             : Special_Key_Code renames KEY_DOWN;
   KEY_UP                      : constant Special_Key_Code
     := Curses_Constants.KEY_UP;
   Key_Cursor_Up               : Special_Key_Code renames KEY_UP;
   KEY_LEFT                    : constant Special_Key_Code
     := Curses_Constants.KEY_LEFT;
   Key_Cursor_Left             : Special_Key_Code renames KEY_LEFT;
   KEY_RIGHT                   : constant Special_Key_Code
     := Curses_Constants.KEY_RIGHT;
   Key_Cursor_Right            : Special_Key_Code renames KEY_RIGHT;
   Key_Home                    : constant Special_Key_Code
     := Curses_Constants.KEY_HOME;
   Key_Backspace               : constant Special_Key_Code
     := Curses_Constants.KEY_BACKSPACE;
   Key_F0                      : constant Special_Key_Code
     := Curses_Constants.KEY_F0;
   Key_F1                      : constant Special_Key_Code
     := Curses_Constants.KEY_F1;
   Key_F2                      : constant Special_Key_Code
     := Curses_Constants.KEY_F2;
   Key_F3                      : constant Special_Key_Code
     := Curses_Constants.KEY_F3;
   Key_F4                      : constant Special_Key_Code
     := Curses_Constants.KEY_F4;
   Key_F5                      : constant Special_Key_Code
     := Curses_Constants.KEY_F5;
   Key_F6                      : constant Special_Key_Code
     := Curses_Constants.KEY_F6;
   Key_F7                      : constant Special_Key_Code
     := Curses_Constants.KEY_F7;
   Key_F8                      : constant Special_Key_Code
     := Curses_Constants.KEY_F8;
   Key_F9                      : constant Special_Key_Code
     := Curses_Constants.KEY_F9;
   Key_F10                     : constant Special_Key_Code
     := Curses_Constants.KEY_F10;
   Key_F11                     : constant Special_Key_Code
     := Curses_Constants.KEY_F11;
   Key_F12                     : constant Special_Key_Code
     := Curses_Constants.KEY_F12;
   Key_F13                     : constant Special_Key_Code
     := Curses_Constants.KEY_F13;
   Key_F14                     : constant Special_Key_Code
     := Curses_Constants.KEY_F14;
   Key_F15                     : constant Special_Key_Code
     := Curses_Constants.KEY_F15;
   Key_F16                     : constant Special_Key_Code
     := Curses_Constants.KEY_F16;
   Key_F17                     : constant Special_Key_Code
     := Curses_Constants.KEY_F17;
   Key_F18                     : constant Special_Key_Code
     := Curses_Constants.KEY_F18;
   Key_F19                     : constant Special_Key_Code
     := Curses_Constants.KEY_F19;
   Key_F20                     : constant Special_Key_Code
     := Curses_Constants.KEY_F20;
   Key_F21                     : constant Special_Key_Code
     := Curses_Constants.KEY_F21;
   Key_F22                     : constant Special_Key_Code
     := Curses_Constants.KEY_F22;
   Key_F23                     : constant Special_Key_Code
     := Curses_Constants.KEY_F23;
   Key_F24                     : constant Special_Key_Code
     := Curses_Constants.KEY_F24;
   KEY_DL                      : constant Special_Key_Code
     := Curses_Constants.KEY_DL;
   Key_Delete_Line             : Special_Key_Code renames KEY_DL;
   KEY_IL                      : constant Special_Key_Code
     := Curses_Constants.KEY_IL;
   Key_Insert_Line             : Special_Key_Code renames KEY_IL;
   KEY_DC                      : constant Special_Key_Code
     := Curses_Constants.KEY_DC;
   Key_Delete_Char             : Special_Key_Code renames KEY_DC;
   KEY_IC                      : constant Special_Key_Code
     := Curses_Constants.KEY_IC;
   Key_Insert_Char             : Special_Key_Code renames KEY_IC;
   KEY_EIC                     : constant Special_Key_Code
     := Curses_Constants.KEY_EIC;
   Key_Exit_Insert_Mode        : Special_Key_Code renames KEY_EIC;
   KEY_CLEAR                   : constant Special_Key_Code
     := Curses_Constants.KEY_CLEAR;
   Key_Clear_Screen            : Special_Key_Code renames KEY_CLEAR;
   KEY_EOS                     : constant Special_Key_Code
     := Curses_Constants.KEY_EOS;
   Key_Clear_End_Of_Screen     : Special_Key_Code renames KEY_EOS;
   KEY_EOL                     : constant Special_Key_Code
     := Curses_Constants.KEY_EOL;
   Key_Clear_End_Of_Line       : Special_Key_Code renames KEY_EOL;
   KEY_SF                      : constant Special_Key_Code
     := Curses_Constants.KEY_SF;
   Key_Scroll_1_Forward        : Special_Key_Code renames KEY_SF;
   KEY_SR                      : constant Special_Key_Code
     := Curses_Constants.KEY_SR;
   Key_Scroll_1_Backward       : Special_Key_Code renames KEY_SR;
   KEY_NPAGE                   : constant Special_Key_Code
     := Curses_Constants.KEY_NPAGE;
   Key_Next_Page               : Special_Key_Code renames KEY_NPAGE;
   KEY_PPAGE                   : constant Special_Key_Code
     := Curses_Constants.KEY_PPAGE;
   Key_Previous_Page           : Special_Key_Code renames KEY_PPAGE;
   KEY_STAB                    : constant Special_Key_Code
     := Curses_Constants.KEY_STAB;
   Key_Set_Tab                 : Special_Key_Code renames KEY_STAB;
   KEY_CTAB                    : constant Special_Key_Code
     := Curses_Constants.KEY_CTAB;
   Key_Clear_Tab               : Special_Key_Code renames KEY_CTAB;
   KEY_CATAB                   : constant Special_Key_Code
     := Curses_Constants.KEY_CATAB;
   Key_Clear_All_Tabs          : Special_Key_Code renames KEY_CATAB;
   KEY_ENTER                   : constant Special_Key_Code
     := Curses_Constants.KEY_ENTER;
   Key_Enter_Or_Send           : Special_Key_Code renames KEY_ENTER;
   KEY_SRESET                  : constant Special_Key_Code
     := Curses_Constants.KEY_SRESET;
   Key_Soft_Reset              : Special_Key_Code renames KEY_SRESET;
   Key_Reset                   : constant Special_Key_Code
     := Curses_Constants.KEY_RESET;
   Key_Print                   : constant Special_Key_Code
     := Curses_Constants.KEY_PRINT;
   KEY_LL                      : constant Special_Key_Code
     := Curses_Constants.KEY_LL;
   Key_Bottom                  : Special_Key_Code renames KEY_LL;
   KEY_A1                      : constant Special_Key_Code
     := Curses_Constants.KEY_A1;
   Key_Upper_Left_Of_Keypad    : Special_Key_Code renames KEY_A1;
   KEY_A3                      : constant Special_Key_Code
     := Curses_Constants.KEY_A3;
   Key_Upper_Right_Of_Keypad   : Special_Key_Code renames KEY_A3;
   KEY_B2                      : constant Special_Key_Code
     := Curses_Constants.KEY_B2;
   Key_Center_Of_Keypad        : Special_Key_Code renames KEY_B2;
   KEY_C1                      : constant Special_Key_Code
     := Curses_Constants.KEY_C1;
   Key_Lower_Left_Of_Keypad    : Special_Key_Code renames KEY_C1;
   KEY_C3                      : constant Special_Key_Code
     := Curses_Constants.KEY_C3;
   Key_Lower_Right_Of_Keypad   : Special_Key_Code renames KEY_C3;
   KEY_BTAB                    : constant Special_Key_Code
     := Curses_Constants.KEY_BTAB;
   Key_Back_Tab                : Special_Key_Code renames KEY_BTAB;
   KEY_BEG                     : constant Special_Key_Code
     := Curses_Constants.KEY_BEG;
   Key_Beginning               : Special_Key_Code renames KEY_BEG;
   Key_Cancel                  : constant Special_Key_Code
     := Curses_Constants.KEY_CANCEL;
   Key_Close                   : constant Special_Key_Code
     := Curses_Constants.KEY_CLOSE;
   Key_Command                 : constant Special_Key_Code
     := Curses_Constants.KEY_COMMAND;
   Key_Copy                    : constant Special_Key_Code
     := Curses_Constants.KEY_COPY;
   Key_Create                  : constant Special_Key_Code
     := Curses_Constants.KEY_CREATE;
   Key_End                     : constant Special_Key_Code
     := Curses_Constants.KEY_END;
   Key_Exit                    : constant Special_Key_Code
     := Curses_Constants.KEY_EXIT;
   Key_Find                    : constant Special_Key_Code
     := Curses_Constants.KEY_FIND;
   Key_Help                    : constant Special_Key_Code
     := Curses_Constants.KEY_HELP;
   Key_Mark                    : constant Special_Key_Code
     := Curses_Constants.KEY_MARK;
   Key_Message                 : constant Special_Key_Code
     := Curses_Constants.KEY_MESSAGE;
   Key_Move                    : constant Special_Key_Code
     := Curses_Constants.KEY_MOVE;
   Key_Next                    : constant Special_Key_Code
     := Curses_Constants.KEY_NEXT;
   Key_Open                    : constant Special_Key_Code
     := Curses_Constants.KEY_OPEN;
   Key_Options                 : constant Special_Key_Code
     := Curses_Constants.KEY_OPTIONS;
   Key_Previous                : constant Special_Key_Code
     := Curses_Constants.KEY_PREVIOUS;
   Key_Redo                    : constant Special_Key_Code
     := Curses_Constants.KEY_REDO;
   Key_Reference               : constant Special_Key_Code
     := Curses_Constants.KEY_REFERENCE;
   Key_Refresh                 : constant Special_Key_Code
     := Curses_Constants.KEY_REFRESH;
   Key_Replace                 : constant Special_Key_Code
     := Curses_Constants.KEY_REPLACE;
   Key_Restart                 : constant Special_Key_Code
     := Curses_Constants.KEY_RESTART;
   Key_Resume                  : constant Special_Key_Code
     := Curses_Constants.KEY_RESUME;
   Key_Save                    : constant Special_Key_Code
     := Curses_Constants.KEY_SAVE;
   KEY_SBEG                    : constant Special_Key_Code
     := Curses_Constants.KEY_SBEG;
   Key_Shift_Begin             : Special_Key_Code renames KEY_SBEG;
   KEY_SCANCEL                 : constant Special_Key_Code
     := Curses_Constants.KEY_SCANCEL;
   Key_Shift_Cancel            : Special_Key_Code renames KEY_SCANCEL;
   KEY_SCOMMAND                : constant Special_Key_Code
     := Curses_Constants.KEY_SCOMMAND;
   Key_Shift_Command           : Special_Key_Code renames KEY_SCOMMAND;
   KEY_SCOPY                   : constant Special_Key_Code
     := Curses_Constants.KEY_SCOPY;
   Key_Shift_Copy              : Special_Key_Code renames KEY_SCOPY;
   KEY_SCREATE                 : constant Special_Key_Code
     := Curses_Constants.KEY_SCREATE;
   Key_Shift_Create            : Special_Key_Code renames KEY_SCREATE;
   KEY_SDC                     : constant Special_Key_Code
     := Curses_Constants.KEY_SDC;
   Key_Shift_Delete_Char       : Special_Key_Code renames KEY_SDC;
   KEY_SDL                     : constant Special_Key_Code
     := Curses_Constants.KEY_SDL;
   Key_Shift_Delete_Line       : Special_Key_Code renames KEY_SDL;
   Key_Select                  : constant Special_Key_Code
     := Curses_Constants.KEY_SELECT;
   KEY_SEND                    : constant Special_Key_Code
     := Curses_Constants.KEY_SEND;
   Key_Shift_End               : Special_Key_Code renames KEY_SEND;
   KEY_SEOL                    : constant Special_Key_Code
     := Curses_Constants.KEY_SEOL;
   Key_Shift_Clear_End_Of_Line : Special_Key_Code renames KEY_SEOL;
   KEY_SEXIT                   : constant Special_Key_Code
     := Curses_Constants.KEY_SEXIT;
   Key_Shift_Exit              : Special_Key_Code renames KEY_SEXIT;
   KEY_SFIND                   : constant Special_Key_Code
     := Curses_Constants.KEY_SFIND;
   Key_Shift_Find              : Special_Key_Code renames KEY_SFIND;
   KEY_SHELP                   : constant Special_Key_Code
     := Curses_Constants.KEY_SHELP;
   Key_Shift_Help              : Special_Key_Code renames KEY_SHELP;
   KEY_SHOME                   : constant Special_Key_Code
     := Curses_Constants.KEY_SHOME;
   Key_Shift_Home              : Special_Key_Code renames KEY_SHOME;
   KEY_SIC                     : constant Special_Key_Code
     := Curses_Constants.KEY_SIC;
   Key_Shift_Insert_Char       : Special_Key_Code renames KEY_SIC;
   KEY_SLEFT                   : constant Special_Key_Code
     := Curses_Constants.KEY_SLEFT;
   Key_Shift_Cursor_Left       : Special_Key_Code renames KEY_SLEFT;
   KEY_SMESSAGE                : constant Special_Key_Code
     := Curses_Constants.KEY_SMESSAGE;
   Key_Shift_Message           : Special_Key_Code renames KEY_SMESSAGE;
   KEY_SMOVE                   : constant Special_Key_Code
     := Curses_Constants.KEY_SMOVE;
   Key_Shift_Move              : Special_Key_Code renames KEY_SMOVE;
   KEY_SNEXT                   : constant Special_Key_Code
     := Curses_Constants.KEY_SNEXT;
   Key_Shift_Next_Page         : Special_Key_Code renames KEY_SNEXT;
   KEY_SOPTIONS                : constant Special_Key_Code
     := Curses_Constants.KEY_SOPTIONS;
   Key_Shift_Options           : Special_Key_Code renames KEY_SOPTIONS;
   KEY_SPREVIOUS               : constant Special_Key_Code
     := Curses_Constants.KEY_SPREVIOUS;
   Key_Shift_Previous_Page     : Special_Key_Code renames KEY_SPREVIOUS;
   KEY_SPRINT                  : constant Special_Key_Code
     := Curses_Constants.KEY_SPRINT;
   Key_Shift_Print             : Special_Key_Code renames KEY_SPRINT;
   KEY_SREDO                   : constant Special_Key_Code
     := Curses_Constants.KEY_SREDO;
   Key_Shift_Redo              : Special_Key_Code renames KEY_SREDO;
   KEY_SREPLACE                : constant Special_Key_Code
     := Curses_Constants.KEY_SREPLACE;
   Key_Shift_Replace           : Special_Key_Code renames KEY_SREPLACE;
   KEY_SRIGHT                  : constant Special_Key_Code
     := Curses_Constants.KEY_SRIGHT;
   Key_Shift_Cursor_Right      : Special_Key_Code renames KEY_SRIGHT;
   KEY_SRSUME                  : constant Special_Key_Code
     := Curses_Constants.KEY_SRSUME;
   Key_Shift_Resume            : Special_Key_Code renames KEY_SRSUME;
   KEY_SSAVE                   : constant Special_Key_Code
     := Curses_Constants.KEY_SSAVE;
   Key_Shift_Save              : Special_Key_Code renames KEY_SSAVE;
   KEY_SSUSPEND                : constant Special_Key_Code
     := Curses_Constants.KEY_SSUSPEND;
   Key_Shift_Suspend           : Special_Key_Code renames KEY_SSUSPEND;
   KEY_SUNDO                   : constant Special_Key_Code
     := Curses_Constants.KEY_SUNDO;
   Key_Shift_Undo              : Special_Key_Code renames KEY_SUNDO;
   Key_Suspend                 : constant Special_Key_Code
     := Curses_Constants.KEY_SUSPEND;
   Key_Undo                    : constant Special_Key_Code
     := Curses_Constants.KEY_UNDO;
   Key_Mouse                   : constant Special_Key_Code
     := Curses_Constants.KEY_MOUSE;
   Key_Resize                  : constant Special_Key_Code
     := Curses_Constants.KEY_RESIZE;
   Key_Max                     : constant Special_Key_Code
     := Special_Key_Code'Last;

   subtype User_Key_Code is Key_Code
     range (Key_Max + 129) .. Key_Code'Last;
   --  This is reserved for user defined key codes. The range between Key_Max
   --  and the first user code is reserved for subsystems like menu and forms.

   --------------------------------------------------------------------------

   type Color_Number is range -1 .. Integer (Interfaces.C.short'Last);
   for Color_Number'Size use Interfaces.C.short'Size;
   --  (n)curses uses a short for the color index
   --  The model is, that a Color_Number is an index into an array of
   --  (potentially) definable colors. Some of those indices are
   --  predefined (see below), although they may not really exist.

   Black   : constant Color_Number := Curses_Constants.COLOR_BLACK;
   Red     : constant Color_Number := Curses_Constants.COLOR_RED;
   Green   : constant Color_Number := Curses_Constants.COLOR_GREEN;
   Yellow  : constant Color_Number := Curses_Constants.COLOR_YELLOW;
   Blue    : constant Color_Number := Curses_Constants.COLOR_BLUE;
   Magenta : constant Color_Number := Curses_Constants.COLOR_MAGENTA;
   Cyan    : constant Color_Number := Curses_Constants.COLOR_CYAN;
   White   : constant Color_Number := Curses_Constants.COLOR_WHITE;

   type RGB_Value is range 0 .. Integer (Interfaces.C.short'Last);
   for RGB_Value'Size use Interfaces.C.short'Size;
   --  Some system may allow to redefine a color by setting RGB values.

   type Color_Pair is range 0 .. 255;
   for Color_Pair'Size use 8;
   subtype Redefinable_Color_Pair is Color_Pair range 1 .. 255;
   --  (n)curses reserves 1 Byte for the color-pair number. Color Pair 0
   --  is fixed (Black & White). A color pair is simply a combination of
   --  two colors described by Color_Numbers, one for the foreground and
   --  the other for the background

   type Character_Attribute_Set is
      record
         Stand_Out               : Boolean;
         Under_Line              : Boolean;
         Reverse_Video           : Boolean;
         Blink                   : Boolean;
         Dim_Character           : Boolean;
         Bold_Character          : Boolean;
         Protected_Character     : Boolean;
         Invisible_Character     : Boolean;
         Alternate_Character_Set : Boolean;
         Horizontal              : Boolean;
         Left                    : Boolean;
         Low                     : Boolean;
         Right                   : Boolean;
         Top                     : Boolean;
         Vertical                : Boolean;
      end record;

   for Character_Attribute_Set use
      record
         Stand_Out at 0 range
           Curses_Constants.A_STANDOUT_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_STANDOUT_Last - Curses_Constants.Attr_First;
         Under_Line at 0 range
           Curses_Constants.A_UNDERLINE_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_UNDERLINE_Last - Curses_Constants.Attr_First;
         Reverse_Video at 0 range
           Curses_Constants.A_REVERSE_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_REVERSE_Last - Curses_Constants.Attr_First;
         Blink at 0 range
           Curses_Constants.A_BLINK_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_BLINK_Last - Curses_Constants.Attr_First;
         Dim_Character at 0 range
           Curses_Constants.A_DIM_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_DIM_Last - Curses_Constants.Attr_First;
         Bold_Character at 0 range
           Curses_Constants.A_BOLD_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_BOLD_Last - Curses_Constants.Attr_First;
         Protected_Character at 0 range
           Curses_Constants.A_PROTECT_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_PROTECT_Last - Curses_Constants.Attr_First;
         Invisible_Character at 0 range
           Curses_Constants.A_INVIS_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_INVIS_Last - Curses_Constants.Attr_First;
         Alternate_Character_Set at 0 range
           Curses_Constants.A_ALTCHARSET_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_ALTCHARSET_Last - Curses_Constants.Attr_First;
         Horizontal at 0 range
           Curses_Constants.A_HORIZONTAL_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_HORIZONTAL_Last - Curses_Constants.Attr_First;
         Left at 0 range
           Curses_Constants.A_LEFT_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_LEFT_Last - Curses_Constants.Attr_First;
         Low at 0 range
           Curses_Constants.A_LOW_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_LOW_Last - Curses_Constants.Attr_First;
         Right at 0 range
           Curses_Constants.A_RIGHT_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_RIGHT_Last - Curses_Constants.Attr_First;
         Top at 0 range
           Curses_Constants.A_TOP_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_TOP_Last - Curses_Constants.Attr_First;
         Vertical at 0 range
           Curses_Constants.A_VERTICAL_First - Curses_Constants.Attr_First
           .. Curses_Constants.A_VERTICAL_Last - Curses_Constants.Attr_First;
      end record;

   Normal_Video : constant Character_Attribute_Set := (others => False);

   type Attributed_Character is
      record
         Attr  : Character_Attribute_Set;
         Color : Color_Pair;
         Ch    : Character;
      end record;
   pragma Convention (C_Pass_By_Copy, Attributed_Character);
   --  This is the counterpart for the chtype in C.

   for Attributed_Character use
      record
         Ch    at 0 range Curses_Constants.A_CHARTEXT_First
           .. Curses_Constants.A_CHARTEXT_Last;
         Color at 0 range Curses_Constants.A_COLOR_First
           .. Curses_Constants.A_COLOR_Last;
         pragma Warnings (Off);
         Attr  at 0 range Curses_Constants.Attr_First
           .. Curses_Constants.Attr_Last;
         pragma Warnings (On);
      end record;
   for Attributed_Character'Size use Curses_Constants.chtype_Size;

   Default_Character : constant Attributed_Character
     := (Ch    => Character'First,
         Color => Color_Pair'First,
         Attr  => (others => False));  --  preelaboratable Normal_Video

   type Attributed_String is array (Positive range <>) of Attributed_Character;
   pragma Convention (C, Attributed_String);
   --  In this binding we allow strings of attributed characters.

   ------------------
   --  Exceptions  --
   ------------------
   Curses_Exception     : exception;
   Wrong_Curses_Version : exception;

   --  Those exceptions are raised by the ETI (Extended Terminal Interface)
   --  subpackets for Menu and Forms handling.
   --
   Eti_System_Error    : exception;
   Eti_Bad_Argument    : exception;
   Eti_Posted          : exception;
   Eti_Connected       : exception;
   Eti_Bad_State       : exception;
   Eti_No_Room         : exception;
   Eti_Not_Posted      : exception;
   Eti_Unknown_Command : exception;
   Eti_No_Match        : exception;
   Eti_Not_Selectable  : exception;
   Eti_Not_Connected   : exception;
   Eti_Request_Denied  : exception;
   Eti_Invalid_Field   : exception;
   Eti_Current         : exception;

   --------------------------------------------------------------------------
   --  External C variables
   --  Conceptually even in C this are kind of constants, but they are
   --  initialized and sometimes changed by the library routines at runtime
   --  depending on the type of terminal. I believe the best way to model
   --  this is to use functions.
   --------------------------------------------------------------------------

   function Lines            return Line_Count;
   pragma Inline (Lines);

   function Columns          return Column_Count;
   pragma Inline (Columns);

   function Tab_Size         return Natural;
   pragma Inline (Tab_Size);

   function Number_Of_Colors return Natural;
   pragma Inline (Number_Of_Colors);

   function Number_Of_Color_Pairs return Natural;
   pragma Inline (Number_Of_Color_Pairs);

   subtype ACS_Index is Character range
     Character'Val (0) .. Character'Val (127);
   function ACS_Map (Index : ACS_Index) return Attributed_Character;
   pragma Import (C, ACS_Map, "acs_map_as_function");

   --  Constants for several characters from the Alternate Character Set
   --  You must use these constants as indices into the ACS_Map function
   --  to get the corresponding attributed character at runtime
   ACS_Upper_Left_Corner  : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_ULCORNER);
   ACS_Lower_Left_Corner  : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LLCORNER);
   ACS_Upper_Right_Corner : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_URCORNER);
   ACS_Lower_Right_Corner : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LRCORNER);
   ACS_Left_Tee           : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LTEE);
   ACS_Right_Tee          : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_RTEE);
   ACS_Bottom_Tee         : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_BTEE);
   ACS_Top_Tee            : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_TTEE);
   ACS_Horizontal_Line    : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_HLINE);
   ACS_Vertical_Line      : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_VLINE);
   ACS_Plus_Symbol        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_PLUS);
   ACS_Scan_Line_1        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_S1);
   ACS_Scan_Line_9        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_S9);
   ACS_Diamond            : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_DIAMOND);
   ACS_Checker_Board      : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_CKBOARD);
   ACS_Degree             : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_DEGREE);
   ACS_Plus_Minus         : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_PLMINUS);
   ACS_Bullet             : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_BULLET);
   ACS_Left_Arrow         : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LARROW);
   ACS_Right_Arrow        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_RARROW);
   ACS_Down_Arrow         : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_DARROW);
   ACS_Up_Arrow           : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_UARROW);
   ACS_Board_Of_Squares   : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_BOARD);
   ACS_Lantern            : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LANTERN);
   ACS_Solid_Block        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_BLOCK);
   ACS_Scan_Line_3        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_S3);
   ACS_Scan_Line_7        : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_S7);
   ACS_Less_Or_Equal      : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_LEQUAL);
   ACS_Greater_Or_Equal   : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_GEQUAL);
   ACS_PI                 : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_PI);
   ACS_Not_Equal          : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_NEQUAL);
   ACS_Sterling           : constant ACS_Index
      := Character'Val (Curses_Constants.ACS_STERLING);

   --  MANPAGE(`curs_initscr.3x')
   --  | Not implemented: newterm, set_term, delscreen

   --  ANCHOR(`stdscr',`Standard_Window')
   function Standard_Window return Window;
   --  AKA
   pragma Import (C, Standard_Window, "stdscr_as_function");
   pragma Inline (Standard_Window);

   --  ANCHOR(`curscr',`Current_Window')
   function Current_Window return Window;
   --  AKA
   pragma Import (C, Current_Window, "curscr_as_function");
   pragma Inline (Current_Window);

   --  ANCHOR(`initscr()',`Init_Screen')
   procedure Init_Screen;

   --  ANCHOR(`initscr()',`Init_Windows')
   procedure Init_Windows renames Init_Screen;
   --  AKA
   pragma Inline (Init_Screen);
   --  pragma Inline (Init_Windows);

   --  ANCHOR(`endwin()',`End_Windows')
   procedure End_Windows;
   --  AKA
   procedure End_Screen renames End_Windows;
   pragma Inline (End_Windows);
   --  pragma Inline (End_Screen);

   --  ANCHOR(`isendwin()',`Is_End_Window')
   function Is_End_Window return Boolean;
   --  AKA
   pragma Inline (Is_End_Window);

   --  MANPAGE(`curs_move.3x')

   --  ANCHOR(`wmove()',`Move_Cursor')
   procedure Move_Cursor (Win    : Window := Standard_Window;
                          Line   : Line_Position;
                          Column : Column_Position);
   --  AKA
   --  ALIAS(`move()')
   pragma Inline (Move_Cursor);

   --  MANPAGE(`curs_addch.3x')

   --  ANCHOR(`waddch()',`Add')
   procedure Add (Win : Window := Standard_Window;
                  Ch  : Attributed_Character);
   --  AKA
   --  ALIAS(`addch()')

   procedure Add (Win : Window := Standard_Window;
                  Ch  : Character);
   --  Add a single character at the current logical cursor position to
   --  the window. Use the current windows attributes.

   --  ANCHOR(`mvwaddch()',`Add')
   procedure Add
     (Win    : Window := Standard_Window;
      Line   : Line_Position;
      Column : Column_Position;
      Ch     : Attributed_Character);
   --  AKA
   --  ALIAS(`mvaddch()')

   procedure Add
     (Win    : Window := Standard_Window;
      Line   : Line_Position;
      Column : Column_Position;
      Ch     : Character);
   --  Move to the position and add a single character into the window
   --  There are more Add routines, so the Inline pragma follows later

   --  ANCHOR(`wechochar()',`Add_With_Immediate_Echo')
   procedure Add_With_Immediate_Echo
     (Win : Window := Standard_Window;
      Ch  : Attributed_Character);
   --  AKA
   --  ALIAS(`echochar()')

   procedure Add_With_Immediate_Echo
     (Win : Window := Standard_Window;
      Ch  : Character);
   --  Add a character and do an immediate refresh of the screen.
   pragma Inline (Add_With_Immediate_Echo);

   --  MANPAGE(`curs_window.3x')
   --  Not Implemented: wcursyncup

   --  ANCHOR(`newwin()',`Create')
   function Create
     (Number_Of_Lines       : Line_Count;
      Number_Of_Columns     : Column_Count;
      First_Line_Position   : Line_Position;
      First_Column_Position : Column_Position) return Window;
   --  Not Implemented: Default Number_Of_Lines, Number_Of_Columns
   --  the C version lets them be 0, see the man page.
   --  AKA
   pragma Inline (Create);

   function New_Window
     (Number_Of_Lines       : Line_Count;
      Number_Of_Columns     : Column_Count;
      First_Line_Position   : Line_Position;
      First_Column_Position : Column_Position) return Window
     renames Create;
   --  pragma Inline (New_Window);

   --  ANCHOR(`delwin()',`Delete')
   procedure Delete (Win : in out Window);
   --  AKA
   --  Reset Win to Null_Window
   pragma Inline (Delete);

   --  ANCHOR(`subwin()',`Sub_Window')
   function Sub_Window
     (Win                   : Window := Standard_Window;
      Number_Of_Lines       : Line_Count;
      Number_Of_Columns     : Column_Count;
      First_Line_Position   : Line_Position;
      First_Column_Position : Column_Position) return Window;
   --  AKA
   pragma Inline (Sub_Window);

   --  ANCHOR(`derwin()',`Derived_Window')
   function Derived_Window
     (Win                   : Window := Standard_Window;
      Number_Of_Lines       : Line_Count;
      Number_Of_Columns     : Column_Count;
      First_Line_Position   : Line_Position;
      First_Column_Position : Column_Position) return Window;
   --  AKA
   pragma Inline (Derived_Window);

   --  ANCHOR(`dupwin()',`Duplicate')
   function Duplicate (Win : Window) return Window;
   --  AKA
   pragma Inline (Duplicate);

   --  ANCHOR(`mvwin()',`Move_Window')
   procedure Move_Window (Win    : Window;
                          Line   : Line_Position;
                          Column : Column_Position);
   --  AKA
   pragma Inline (Move_Window);

   --  ANCHOR(`mvderwin()',`Move_Derived_Window')
   procedure Move_Derived_Window (Win    : Window;
                                  Line   : Line_Position;
                                  Column : Column_Position);
   --  AKA
   pragma Inline (Move_Derived_Window);

   --  ANCHOR(`wsyncup()',`Synchronize_Upwards')
   procedure Synchronize_Upwards (Win : Window);
   --  AKA
   pragma Import (C, Synchronize_Upwards, "wsyncup");

   --  ANCHOR(`wsyncdown()',`Synchronize_Downwards')
   procedure Synchronize_Downwards (Win : Window);
   --  AKA
   pragma Import (C, Synchronize_Downwards, "wsyncdown");

   --  ANCHOR(`syncok()',`Set_Synch_Mode')
   procedure Set_Synch_Mode (Win  : Window := Standard_Window;
                             Mode : Boolean := False);
   --  AKA
   pragma Inline (Set_Synch_Mode);

   --  MANPAGE(`curs_addstr.3x')

   --  ANCHOR(`waddnstr()',`Add')
   procedure Add (Win : Window := Standard_Window;
                  Str : String;
                  Len : Integer := -1);
   --  AKA
   --  ALIAS(`waddstr()')
   --  ALIAS(`addnstr()')
   --  ALIAS(`addstr()')

   --  ANCHOR(`mvwaddnstr()',`Add')
   procedure Add (Win    : Window := Standard_Window;
                  Line   : Line_Position;
                  Column : Column_Position;
                  Str    : String;
                  Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwaddstr()')
   --  ALIAS(`mvaddnstr()')
   --  ALIAS(`mvaddstr()')

   --  MANPAGE(`curs_addchstr.3x')

   --  ANCHOR(`waddchnstr()',`Add')
   procedure Add (Win : Window := Standard_Window;
                  Str : Attributed_String;
                  Len : Integer := -1);
   --  AKA
   --  ALIAS(`waddchstr()')
   --  ALIAS(`addchnstr()')
   --  ALIAS(`addchstr()')

   --  ANCHOR(`mvwaddchnstr()',`Add')
   procedure Add (Win    : Window := Standard_Window;
                  Line   : Line_Position;
                  Column : Column_Position;
                  Str    : Attributed_String;
                  Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwaddchstr()')
   --  ALIAS(`mvaddchnstr()')
   --  ALIAS(`mvaddchstr()')
   pragma Inline (Add);

   --  MANPAGE(`curs_border.3x')
   --  | Not implemented: mvhline,  mvwhline, mvvline, mvwvline
   --  | use Move_Cursor then Horizontal_Line or Vertical_Line

   --  ANCHOR(`wborder()',`Border')
   procedure Border
     (Win                       : Window := Standard_Window;
      Left_Side_Symbol          : Attributed_Character := Default_Character;
      Right_Side_Symbol         : Attributed_Character := Default_Character;
      Top_Side_Symbol           : Attributed_Character := Default_Character;
      Bottom_Side_Symbol        : Attributed_Character := Default_Character;
      Upper_Left_Corner_Symbol  : Attributed_Character := Default_Character;
      Upper_Right_Corner_Symbol : Attributed_Character := Default_Character;
      Lower_Left_Corner_Symbol  : Attributed_Character := Default_Character;
      Lower_Right_Corner_Symbol : Attributed_Character := Default_Character
     );
   --  AKA
   --  ALIAS(`border()')
   pragma Inline (Border);

   --  ANCHOR(`box()',`Box')
   procedure Box
     (Win               : Window := Standard_Window;
      Vertical_Symbol   : Attributed_Character := Default_Character;
      Horizontal_Symbol : Attributed_Character := Default_Character);
   --  AKA
   pragma Inline (Box);

   --  ANCHOR(`whline()',`Horizontal_Line')
   procedure Horizontal_Line
     (Win         : Window := Standard_Window;
      Line_Size   : Natural;
      Line_Symbol : Attributed_Character := Default_Character);
   --  AKA
   --  ALIAS(`hline()')
   pragma Inline (Horizontal_Line);

   --  ANCHOR(`wvline()',`Vertical_Line')
   procedure Vertical_Line
     (Win         : Window := Standard_Window;
      Line_Size   : Natural;
      Line_Symbol : Attributed_Character := Default_Character);
   --  AKA
   --  ALIAS(`vline()')
   pragma Inline (Vertical_Line);

   --  MANPAGE(`curs_getch.3x')
   --  Not implemented: mvgetch, mvwgetch

   --  ANCHOR(`wgetch()',`Get_Keystroke')
   function Get_Keystroke (Win : Window := Standard_Window)
                           return Real_Key_Code;
   --  AKA
   --  ALIAS(`getch()')
   --  Get a character from the keyboard and echo it - if enabled - to the
   --  window.
   --  If for any reason (i.e. a timeout) we could not get a character the
   --  returned keycode is Key_None.
   pragma Inline (Get_Keystroke);

   --  ANCHOR(`ungetch()',`Undo_Keystroke')
   procedure Undo_Keystroke (Key : Real_Key_Code);
   --  AKA
   pragma Inline (Undo_Keystroke);

   --  ANCHOR(`has_key()',`Has_Key')
   function Has_Key (Key : Special_Key_Code) return Boolean;
   --  AKA
   pragma Inline (Has_Key);

   --  |
   --  | Some helper functions
   --  |
   function Is_Function_Key (Key : Special_Key_Code) return Boolean;
   --  Return True if the Key is a function key (i.e. one of F0 .. F63)
   pragma Inline (Is_Function_Key);

   subtype Function_Key_Number is Integer range 0 .. 63;
   --  (n)curses allows for 64 function keys.

   function Function_Key (Key : Real_Key_Code) return Function_Key_Number;
   --  Return the number of the function key. If the code is not a
   --  function key, a CONSTRAINT_ERROR will be raised.
   pragma Inline (Function_Key);

   function Function_Key_Code (Key : Function_Key_Number) return Real_Key_Code;
   --  Return the key code for a given function-key number.
   pragma Inline (Function_Key_Code);

   --  MANPAGE(`curs_attr.3x')
   --  | Not implemented attr_off,  wattr_off,
   --  |  attr_on, wattr_on, attr_set, wattr_set

   --  PAIR_NUMBER
   --  PAIR_NUMBER(c) is the same as c.Color

   --  ANCHOR(`standout()',`Standout')
   procedure Standout (Win : Window  := Standard_Window;
                       On  : Boolean := True);
   --  ALIAS(`wstandout()')
   --  ALIAS(`wstandend()')

   --  ANCHOR(`wattron()',`Switch_Character_Attribute')
   procedure Switch_Character_Attribute
     (Win  : Window := Standard_Window;
      Attr : Character_Attribute_Set := Normal_Video;
      On   : Boolean := True); --  if False we switch Off.
   --  Switches those Attributes set to true in the list.
   --  AKA
   --  ALIAS(`wattroff()')
   --  ALIAS(`attron()')
   --  ALIAS(`attroff()')

   --  ANCHOR(`wattrset()',`Set_Character_Attributes')
   procedure Set_Character_Attributes
     (Win   : Window := Standard_Window;
      Attr  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   --  ALIAS(`attrset()')
   pragma Inline (Set_Character_Attributes);

   --  ANCHOR(`wattr_get()',`Get_Character_Attributes')
   function Get_Character_Attribute
     (Win : Window := Standard_Window) return Character_Attribute_Set;
   --  AKA
   --  ALIAS(`attr_get()')

   --  ANCHOR(`wattr_get()',`Get_Character_Attribute')
   function Get_Character_Attribute
     (Win : Window := Standard_Window) return Color_Pair;
   --  AKA
   pragma Inline (Get_Character_Attribute);

   --  ANCHOR(`wcolor_set()',`Set_Color')
   procedure Set_Color (Win  : Window := Standard_Window;
                        Pair : Color_Pair);
   --  AKA
   --  ALIAS(`color_set()')
   pragma Inline (Set_Color);

   --  ANCHOR(`wchgat()',`Change_Attributes')
   procedure Change_Attributes
     (Win   : Window := Standard_Window;
      Count : Integer := -1;
      Attr  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   --  ALIAS(`chgat()')

   --  ANCHOR(`mvwchgat()',`Change_Attributes')
   procedure Change_Attributes
     (Win    : Window := Standard_Window;
      Line   : Line_Position := Line_Position'First;
      Column : Column_Position := Column_Position'First;
      Count  : Integer := -1;
      Attr   : Character_Attribute_Set := Normal_Video;
      Color  : Color_Pair := Color_Pair'First);
   --  AKA
   --  ALIAS(`mvchgat()')
   pragma Inline (Change_Attributes);

   --  MANPAGE(`curs_beep.3x')

   --  ANCHOR(`beep()',`Beep')
   procedure Beep;
   --  AKA
   pragma Inline (Beep);

   --  ANCHOR(`flash()',`Flash_Screen')
   procedure Flash_Screen;
   --  AKA
   pragma Inline (Flash_Screen);

   --  MANPAGE(`curs_inopts.3x')

   --  | Not implemented : typeahead
   --
   --  ANCHOR(`cbreak()',`Set_Cbreak_Mode')
   procedure Set_Cbreak_Mode (SwitchOn : Boolean := True);
   --  AKA
   --  ALIAS(`nocbreak()')
   pragma Inline (Set_Cbreak_Mode);

   --  ANCHOR(`raw()',`Set_Raw_Mode')
   procedure Set_Raw_Mode (SwitchOn : Boolean := True);
   --  AKA
   --  ALIAS(`noraw()')
   pragma Inline (Set_Raw_Mode);

   --  ANCHOR(`echo()',`Set_Echo_Mode')
   procedure Set_Echo_Mode (SwitchOn : Boolean := True);
   --  AKA
   --  ALIAS(`noecho()')
   pragma Inline (Set_Echo_Mode);

   --  ANCHOR(`meta()',`Set_Meta_Mode')
   procedure Set_Meta_Mode (Win      : Window := Standard_Window;
                            SwitchOn : Boolean := True);
   --  AKA
   pragma Inline (Set_Meta_Mode);

   --  ANCHOR(`keypad()',`Set_KeyPad_Mode')
   procedure Set_KeyPad_Mode (Win      : Window := Standard_Window;
                              SwitchOn : Boolean := True);
   --  AKA
   pragma Inline (Set_KeyPad_Mode);

   function Get_KeyPad_Mode (Win : Window := Standard_Window)
                             return Boolean;
   --  This has no pendant in C. There you've to look into the WINDOWS
   --  structure to get the value. Bad practice, not repeated in Ada.

   type Half_Delay_Amount is range 1 .. 255;

   --  ANCHOR(`halfdelay()',`Half_Delay')
   procedure Half_Delay (Amount : Half_Delay_Amount);
   --  AKA
   pragma Inline (Half_Delay);

   --  ANCHOR(`intrflush()',`Set_Flush_On_Interrupt_Mode')
   procedure Set_Flush_On_Interrupt_Mode
     (Win  : Window := Standard_Window;
      Mode : Boolean := True);
   --  AKA
   pragma Inline (Set_Flush_On_Interrupt_Mode);

   --  ANCHOR(`qiflush()',`Set_Queue_Interrupt_Mode')
   procedure Set_Queue_Interrupt_Mode
     (Win   : Window := Standard_Window;
      Flush : Boolean := True);
   --  AKA
   --  ALIAS(`noqiflush()')
   pragma Inline (Set_Queue_Interrupt_Mode);

   --  ANCHOR(`nodelay()',`Set_NoDelay_Mode')
   procedure Set_NoDelay_Mode
     (Win  : Window := Standard_Window;
      Mode : Boolean := False);
   --  AKA
   pragma Inline (Set_NoDelay_Mode);

   type Timeout_Mode is (Blocking, Non_Blocking, Delayed);

   --  ANCHOR(`wtimeout()',`Set_Timeout_Mode')
   procedure Set_Timeout_Mode (Win    : Window := Standard_Window;
                               Mode   : Timeout_Mode;
                               Amount : Natural); --  in Milliseconds
   --  AKA
   --  ALIAS(`timeout()')
   --  Instead of overloading the semantic of the sign of amount, we
   --  introduce the Timeout_Mode parameter. This should improve
   --  readability. For Blocking and Non_Blocking, the Amount is not
   --  evaluated.
   --  We do not inline this procedure.

   --  ANCHOR(`notimeout()',`Set_Escape_Time_Mode')
   procedure Set_Escape_Timer_Mode
     (Win       : Window := Standard_Window;
      Timer_Off : Boolean := False);
   --  AKA
   pragma Inline (Set_Escape_Timer_Mode);

   --  MANPAGE(`curs_outopts.3x')

   --  ANCHOR(`nl()',`Set_NL_Mode')
   procedure Set_NL_Mode (SwitchOn : Boolean := True);
   --  AKA
   --  ALIAS(`nonl()')
   pragma Inline (Set_NL_Mode);

   --  ANCHOR(`clearok()',`Clear_On_Next_Update')
   procedure Clear_On_Next_Update
     (Win      : Window := Standard_Window;
      Do_Clear : Boolean := True);
   --  AKA
   pragma Inline (Clear_On_Next_Update);

   --  ANCHOR(`idlok()',`Use_Insert_Delete_Line')
   procedure Use_Insert_Delete_Line
     (Win    : Window := Standard_Window;
      Do_Idl : Boolean := True);
   --  AKA
   pragma Inline (Use_Insert_Delete_Line);

   --  ANCHOR(`idcok()',`Use_Insert_Delete_Character')
   procedure Use_Insert_Delete_Character
     (Win    : Window := Standard_Window;
      Do_Idc : Boolean := True);
   --  AKA
   pragma Inline (Use_Insert_Delete_Character);

   --  ANCHOR(`leaveok()',`Leave_Cursor_After_Update')
   procedure Leave_Cursor_After_Update
     (Win      : Window := Standard_Window;
      Do_Leave : Boolean := True);
   --  AKA
   pragma Inline (Leave_Cursor_After_Update);

   --  ANCHOR(`immedok()',`Immediate_Update_Mode')
   procedure Immediate_Update_Mode
     (Win  : Window := Standard_Window;
      Mode : Boolean := False);
   --  AKA
   pragma Inline (Immediate_Update_Mode);

   --  ANCHOR(`scrollok()',`Allow_Scrolling')
   procedure Allow_Scrolling
     (Win  : Window := Standard_Window;
      Mode : Boolean := False);
   --  AKA
   pragma Inline (Allow_Scrolling);

   function Scrolling_Allowed (Win : Window := Standard_Window) return Boolean;
   --  There is no such function in the C interface.
   pragma Inline (Scrolling_Allowed);

   --  ANCHOR(`wsetscrreg()',`Set_Scroll_Region')
   procedure Set_Scroll_Region
     (Win         : Window := Standard_Window;
      Top_Line    : Line_Position;
      Bottom_Line : Line_Position);
   --  AKA
   --  ALIAS(`setscrreg()')
   pragma Inline (Set_Scroll_Region);

   --  MANPAGE(`curs_refresh.3x')

   --  ANCHOR(`doupdate()',`Update_Screen')
   procedure Update_Screen;
   --  AKA
   pragma Inline (Update_Screen);

   --  ANCHOR(`wrefresh()',`Refresh')
   procedure Refresh (Win : Window := Standard_Window);
   --  AKA
   --  There is an overloaded Refresh for Pads.
   --  The Inline pragma appears there
   --  ALIAS(`refresh()')

   --  ANCHOR(`wnoutrefresh()',`Refresh_Without_Update')
   procedure Refresh_Without_Update
     (Win : Window := Standard_Window);
   --  AKA
   --  There is an overloaded Refresh_Without_Update for Pads.
   --  The Inline pragma appears there

   --  ANCHOR(`redrawwin()',`Redraw')
   procedure Redraw (Win : Window := Standard_Window);
   --  AKA

   --  ANCHOR(`wredrawln()',`Redraw')
   procedure Redraw (Win        : Window := Standard_Window;
                     Begin_Line : Line_Position;
                     Line_Count : Positive);
   --  AKA
   pragma Inline (Redraw);

   --  MANPAGE(`curs_clear.3x')

   --  ANCHOR(`werase()',`Erase')
   procedure Erase (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`erase()')
   pragma Inline (Erase);

   --  ANCHOR(`wclear()',`Clear')
   procedure Clear
     (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`clear()')
   pragma Inline (Clear);

   --  ANCHOR(`wclrtobot()',`Clear_To_End_Of_Screen')
   procedure Clear_To_End_Of_Screen
     (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`clrtobot()')
   pragma Inline (Clear_To_End_Of_Screen);

   --  ANCHOR(`wclrtoeol()',`Clear_To_End_Of_Line')
   procedure Clear_To_End_Of_Line
     (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`clrtoeol()')
   pragma Inline (Clear_To_End_Of_Line);

   --  MANPAGE(`curs_bkgd.3x')

   --  ANCHOR(`wbkgdset()',`Set_Background')
   --  TODO: we could have Set_Background(Window; Character_Attribute_Set)
   --  because in C it is common to see bkgdset(A_BOLD) or
   --  bkgdset(COLOR_PAIR(n))
   procedure Set_Background
     (Win : Window := Standard_Window;
      Ch  : Attributed_Character);
   --  AKA
   --  ALIAS(`bkgdset()')
   pragma Inline (Set_Background);

   --  ANCHOR(`wbkgd()',`Change_Background')
   procedure Change_Background
     (Win : Window := Standard_Window;
      Ch  : Attributed_Character);
   --  AKA
   --  ALIAS(`bkgd()')
   pragma Inline (Change_Background);

   --  ANCHOR(`wbkgdget()',`Get_Background')
   --  ? wbkgdget is not listed in curs_bkgd, getbkgd is thpough.
   function Get_Background (Win : Window := Standard_Window)
     return Attributed_Character;
   --  AKA
   --  ALIAS(`bkgdget()')
   pragma Inline (Get_Background);

   --  MANPAGE(`curs_touch.3x')

   --  ANCHOR(`untouchwin()',`Untouch')
   procedure Untouch (Win : Window := Standard_Window);
   --  AKA
   pragma Inline (Untouch);

   --  ANCHOR(`touchwin()',`Touch')
   procedure Touch (Win : Window := Standard_Window);
   --  AKA

   --  ANCHOR(`touchline()',`Touch')
   procedure Touch (Win   : Window := Standard_Window;
                    Start : Line_Position;
                    Count : Positive);
   --  AKA
   pragma Inline (Touch);

   --  ANCHOR(`wtouchln()',`Change_Line_Status')
   procedure Change_Lines_Status (Win   : Window := Standard_Window;
                                  Start : Line_Position;
                                  Count : Positive;
                                  State : Boolean);
   --  AKA
   pragma Inline (Change_Lines_Status);

   --  ANCHOR(`is_linetouched()',`Is_Touched')
   function Is_Touched (Win  : Window := Standard_Window;
                        Line : Line_Position) return Boolean;
   --  AKA

   --  ANCHOR(`is_wintouched()',`Is_Touched')
   function Is_Touched (Win : Window := Standard_Window) return Boolean;
   --  AKA
   pragma Inline (Is_Touched);

   --  MANPAGE(`curs_overlay.3x')

   --  ANCHOR(`copywin()',`Copy')
   procedure Copy
     (Source_Window            : Window;
      Destination_Window       : Window;
      Source_Top_Row           : Line_Position;
      Source_Left_Column       : Column_Position;
      Destination_Top_Row      : Line_Position;
      Destination_Left_Column  : Column_Position;
      Destination_Bottom_Row   : Line_Position;
      Destination_Right_Column : Column_Position;
      Non_Destructive_Mode     : Boolean := True);
   --  AKA
   pragma Inline (Copy);

   --  ANCHOR(`overwrite()',`Overwrite')
   procedure Overwrite (Source_Window      : Window;
                        Destination_Window : Window);
   --  AKA
   pragma Inline (Overwrite);

   --  ANCHOR(`overlay()',`Overlay')
   procedure Overlay (Source_Window      : Window;
                      Destination_Window : Window);
   --  AKA
   pragma Inline (Overlay);

   --  MANPAGE(`curs_deleteln.3x')

   --  ANCHOR(`winsdelln()',`Insert_Delete_Lines')
   procedure Insert_Delete_Lines
     (Win   : Window  := Standard_Window;
      Lines : Integer := 1); --  default is to insert one line above
   --  AKA
   --  ALIAS(`insdelln()')
   pragma Inline (Insert_Delete_Lines);

   --  ANCHOR(`wdeleteln()',`Delete_Line')
   procedure Delete_Line (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`deleteln()')
   pragma Inline (Delete_Line);

   --  ANCHOR(`winsertln()',`Insert_Line')
   procedure Insert_Line (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`insertln()')
   pragma Inline (Insert_Line);

   --  MANPAGE(`curs_getyx.3x')

   --  ANCHOR(`getmaxyx()',`Get_Size')
   procedure Get_Size
     (Win               : Window := Standard_Window;
      Number_Of_Lines   : out Line_Count;
      Number_Of_Columns : out Column_Count);
   --  AKA
   pragma Inline (Get_Size);

   --  ANCHOR(`getbegyx()',`Get_Window_Position')
   procedure Get_Window_Position
     (Win             : Window := Standard_Window;
      Top_Left_Line   : out Line_Position;
      Top_Left_Column : out Column_Position);
   --  AKA
   pragma Inline (Get_Window_Position);

   --  ANCHOR(`getyx()',`Get_Cursor_Position')
   procedure Get_Cursor_Position
     (Win    : Window := Standard_Window;
      Line   : out Line_Position;
      Column : out Column_Position);
   --  AKA
   pragma Inline (Get_Cursor_Position);

   --  ANCHOR(`getparyx()',`Get_Origin_Relative_To_Parent')
   procedure Get_Origin_Relative_To_Parent
     (Win                : Window;
      Top_Left_Line      : out Line_Position;
      Top_Left_Column    : out Column_Position;
      Is_Not_A_Subwindow : out Boolean);
   --  AKA
   --  Instead of placing -1 in the coordinates as return, we use a Boolean
   --  to return the info that the window has no parent.
   pragma Inline (Get_Origin_Relative_To_Parent);

   --  MANPAGE(`curs_pad.3x')

   --  ANCHOR(`newpad()',`New_Pad')
   function New_Pad (Lines   : Line_Count;
                     Columns : Column_Count) return Window;
   --  AKA
   pragma Inline (New_Pad);

   --  ANCHOR(`subpad()',`Sub_Pad')
   function Sub_Pad
     (Pad                   : Window;
      Number_Of_Lines       : Line_Count;
      Number_Of_Columns     : Column_Count;
      First_Line_Position   : Line_Position;
      First_Column_Position : Column_Position) return Window;
   --  AKA
   pragma Inline (Sub_Pad);

   --  ANCHOR(`prefresh()',`Refresh')
   procedure Refresh
     (Pad                      : Window;
      Source_Top_Row           : Line_Position;
      Source_Left_Column       : Column_Position;
      Destination_Top_Row      : Line_Position;
      Destination_Left_Column  : Column_Position;
      Destination_Bottom_Row   : Line_Position;
      Destination_Right_Column : Column_Position);
   --  AKA
   pragma Inline (Refresh);

   --  ANCHOR(`pnoutrefresh()',`Refresh_Without_Update')
   procedure Refresh_Without_Update
     (Pad                      : Window;
      Source_Top_Row           : Line_Position;
      Source_Left_Column       : Column_Position;
      Destination_Top_Row      : Line_Position;
      Destination_Left_Column  : Column_Position;
      Destination_Bottom_Row   : Line_Position;
      Destination_Right_Column : Column_Position);
   --  AKA
   pragma Inline (Refresh_Without_Update);

   --  ANCHOR(`pechochar()',`Add_Character_To_Pad_And_Echo_It')
   procedure Add_Character_To_Pad_And_Echo_It
     (Pad : Window;
      Ch  : Attributed_Character);
   --  AKA

   procedure Add_Character_To_Pad_And_Echo_It
     (Pad : Window;
      Ch  : Character);
   pragma Inline (Add_Character_To_Pad_And_Echo_It);

   --  MANPAGE(`curs_scroll.3x')

   --  ANCHOR(`wscrl()',`Scroll')
   procedure Scroll (Win    : Window  := Standard_Window;
                     Amount : Integer := 1);
   --  AKA
   --  ALIAS(`scroll()')
   --  ALIAS(`scrl()')
   pragma Inline (Scroll);

   --  MANPAGE(`curs_delch.3x')

   --  ANCHOR(`wdelch()',`Delete_Character')
   procedure Delete_Character (Win : Window := Standard_Window);
   --  AKA
   --  ALIAS(`delch()')

   --  ANCHOR(`mvwdelch()',`Delete_Character')
   procedure Delete_Character
     (Win    : Window := Standard_Window;
      Line   : Line_Position;
      Column : Column_Position);
   --  AKA
   --  ALIAS(`mvdelch()')
   pragma Inline (Delete_Character);

   --  MANPAGE(`curs_inch.3x')

   --  ANCHOR(`winch()',`Peek')
   function Peek (Win : Window := Standard_Window)
     return Attributed_Character;
   --  ALIAS(`inch()')
   --  AKA

   --  ANCHOR(`mvwinch()',`Peek')
   function Peek
     (Win    : Window := Standard_Window;
      Line   : Line_Position;
      Column : Column_Position) return Attributed_Character;
   --  AKA
   --  ALIAS(`mvinch()')
   --  More Peek's follow, pragma Inline appears later.

   --  MANPAGE(`curs_insch.3x')

   --  ANCHOR(`winsch()',`Insert')
   procedure Insert (Win : Window := Standard_Window;
                     Ch  : Attributed_Character);
   --  AKA
   --  ALIAS(`insch()')

   --  ANCHOR(`mvwinsch()',`Insert')
   procedure Insert (Win    : Window := Standard_Window;
                     Line   : Line_Position;
                     Column : Column_Position;
                     Ch     : Attributed_Character);
   --  AKA
   --  ALIAS(`mvinsch()')

   --  MANPAGE(`curs_insstr.3x')

   --  ANCHOR(`winsnstr()',`Insert')
   procedure Insert (Win : Window := Standard_Window;
                     Str : String;
                     Len : Integer := -1);
   --  AKA
   --  ALIAS(`winsstr()')
   --  ALIAS(`insnstr()')
   --  ALIAS(`insstr()')

   --  ANCHOR(`mvwinsnstr()',`Insert')
   procedure Insert (Win    : Window := Standard_Window;
                     Line   : Line_Position;
                     Column : Column_Position;
                     Str    : String;
                     Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwinsstr()')
   --  ALIAS(`mvinsnstr()')
   --  ALIAS(`mvinsstr()')
   pragma Inline (Insert);

   --  MANPAGE(`curs_instr.3x')

   --  ANCHOR(`winnstr()',`Peek')
   procedure Peek (Win : Window := Standard_Window;
                   Str : out String;
                   Len : Integer := -1);
   --  AKA
   --  ALIAS(`winstr()')
   --  ALIAS(`innstr()')
   --  ALIAS(`instr()')

   --  ANCHOR(`mvwinnstr()',`Peek')
   procedure Peek (Win    : Window := Standard_Window;
                   Line   : Line_Position;
                   Column : Column_Position;
                   Str    : out String;
                   Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwinstr()')
   --  ALIAS(`mvinnstr()')
   --  ALIAS(`mvinstr()')

   --  MANPAGE(`curs_inchstr.3x')

   --  ANCHOR(`winchnstr()',`Peek')
   procedure Peek (Win : Window := Standard_Window;
                   Str : out Attributed_String;
                   Len : Integer := -1);
   --  AKA
   --  ALIAS(`winchstr()')
   --  ALIAS(`inchnstr()')
   --  ALIAS(`inchstr()')

   --  ANCHOR(`mvwinchnstr()',`Peek')
   procedure Peek (Win    : Window := Standard_Window;
                   Line   : Line_Position;
                   Column : Column_Position;
                   Str    : out Attributed_String;
                   Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwinchstr()')
   --  ALIAS(`mvinchnstr()')
   --  ALIAS(`mvinchstr()')
   --  We do not inline the Peek procedures

   --  MANPAGE(`curs_getstr.3x')

   --  ANCHOR(`wgetnstr()',`Get')
   procedure Get (Win : Window := Standard_Window;
                  Str : out String;
                  Len : Integer := -1);
   --  AKA
   --  ALIAS(`wgetstr()')
   --  ALIAS(`getnstr()')
   --  ALIAS(`getstr()')
   --  actually getstr is not supported because that results in buffer
   --  overflows.

   --  ANCHOR(`mvwgetnstr()',`Get')
   procedure Get (Win    : Window := Standard_Window;
                  Line   : Line_Position;
                  Column : Column_Position;
                  Str    : out String;
                  Len    : Integer := -1);
   --  AKA
   --  ALIAS(`mvwgetstr()')
   --  ALIAS(`mvgetnstr()')
   --  ALIAS(`mvgetstr()')
   --  Get is not inlined

   --  MANPAGE(`curs_slk.3x')

   --  Not Implemented: slk_attr_on, slk_attr_off, slk_attr_set

   type Soft_Label_Key_Format is (Three_Two_Three,
                                  Four_Four,
                                  PC_Style,              --  ncurses specific
                                  PC_Style_With_Index);  --  "
   type Label_Number is new Positive range 1 .. 12;
   type Label_Justification is (Left, Centered, Right);

   --  ANCHOR(`slk_init()',`Init_Soft_Label_Keys')
   procedure Init_Soft_Label_Keys
     (Format : Soft_Label_Key_Format := Three_Two_Three);
   --  AKA
   pragma Inline (Init_Soft_Label_Keys);

   --  ANCHOR(`slk_set()',`Set_Soft_Label_Key')
   procedure Set_Soft_Label_Key (Label : Label_Number;
                                 Text  : String;
                                 Fmt   : Label_Justification := Left);
   --  AKA
   --  We do not inline this procedure

   --  ANCHOR(`slk_refresh()',`Refresh_Soft_Label_Key')
   procedure Refresh_Soft_Label_Keys;
   --  AKA
   pragma Inline (Refresh_Soft_Label_Keys);

   --  ANCHOR(`slk_noutrefresh()',`Refresh_Soft_Label_Keys_Without_Update')
   procedure Refresh_Soft_Label_Keys_Without_Update;
   --  AKA
   pragma Inline (Refresh_Soft_Label_Keys_Without_Update);

   --  ANCHOR(`slk_label()',`Get_Soft_Label_Key')
   procedure Get_Soft_Label_Key (Label : Label_Number;
                                 Text  : out String);
   --  AKA

   --  ANCHOR(`slk_label()',`Get_Soft_Label_Key')
   function Get_Soft_Label_Key (Label : Label_Number) return String;
   --  AKA
   --  Same as function
   pragma Inline (Get_Soft_Label_Key);

   --  ANCHOR(`slk_clear()',`Clear_Soft_Label_Keys')
   procedure Clear_Soft_Label_Keys;
   --  AKA
   pragma Inline (Clear_Soft_Label_Keys);

   --  ANCHOR(`slk_restore()',`Restore_Soft_Label_Keys')
   procedure Restore_Soft_Label_Keys;
   --  AKA
   pragma Inline (Restore_Soft_Label_Keys);

   --  ANCHOR(`slk_touch()',`Touch_Soft_Label_Keys')
   procedure Touch_Soft_Label_Keys;
   --  AKA
   pragma Inline (Touch_Soft_Label_Keys);

   --  ANCHOR(`slk_attron()',`Switch_Soft_Label_Key_Attributes')
   procedure Switch_Soft_Label_Key_Attributes
     (Attr : Character_Attribute_Set;
      On   : Boolean := True);
   --  AKA
   --  ALIAS(`slk_attroff()')
   pragma Inline (Switch_Soft_Label_Key_Attributes);

   --  ANCHOR(`slk_attrset()',`Set_Soft_Label_Key_Attributes')
   procedure Set_Soft_Label_Key_Attributes
     (Attr  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Soft_Label_Key_Attributes);

   --  ANCHOR(`slk_attr()',`Get_Soft_Label_Key_Attributes')
   function Get_Soft_Label_Key_Attributes return Character_Attribute_Set;
   --  AKA

   --  ANCHOR(`slk_attr()',`Get_Soft_Label_Key_Attributes')
   function Get_Soft_Label_Key_Attributes return Color_Pair;
   --  AKA
   pragma Inline (Get_Soft_Label_Key_Attributes);

   --  ANCHOR(`slk_color()',`Set_Soft_Label_Key_Color')
   procedure Set_Soft_Label_Key_Color (Pair : Color_Pair);
   --  AKA
   pragma Inline (Set_Soft_Label_Key_Color);

   --  MANPAGE(`keybound.3x')
   --  Not Implemented: keybound

   --  MANPAGE(`keyok.3x')

   --  ANCHOR(`keyok()',`Enable_Key')
   procedure Enable_Key (Key    : Special_Key_Code;
                         Enable : Boolean := True);
   --  AKA
   pragma Inline (Enable_Key);

   --  MANPAGE(`define_key.3x')

   --  ANCHOR(`define_key()',`Define_Key')
   procedure Define_Key (Definition : String;
                         Key        : Special_Key_Code);
   --  AKA
   pragma Inline (Define_Key);

   --  MANPAGE(`curs_util.3x')

   --  | Not implemented : filter, use_env
   --  | putwin, getwin are in the child package PutWin
   --

   --  ANCHOR(`keyname()',`Key_Name')
   procedure Key_Name (Key  : Real_Key_Code;
                       Name : out String);
   --  AKA
   --  The external name for a real keystroke.

   --  ANCHOR(`keyname()',`Key_Name')
   function Key_Name (Key  : Real_Key_Code) return String;
   --  AKA
   --  Same as function
   --  We do not inline this routine

   --  ANCHOR(`unctrl()',`Un_Control')
   procedure Un_Control (Ch  : Attributed_Character;
                         Str : out String);
   --  AKA

   --  ANCHOR(`unctrl()',`Un_Control')
   function Un_Control (Ch  : Attributed_Character) return String;
   --  AKA
   --  Same as function
   pragma Inline (Un_Control);

   --  ANCHOR(`delay_output()',`Delay_Output')
   procedure Delay_Output (Msecs : Natural);
   --  AKA
   pragma Inline (Delay_Output);

   --  ANCHOR(`flushinp()',`Flush_Input')
   procedure Flush_Input;
   --  AKA
   pragma Inline (Flush_Input);

   --  MANPAGE(`curs_termattrs.3x')

   --  ANCHOR(`baudrate()',`Baudrate')
   function Baudrate return Natural;
   --  AKA
   pragma Inline (Baudrate);

   --  ANCHOR(`erasechar()',`Erase_Character')
   function Erase_Character return Character;
   --  AKA
   pragma Inline (Erase_Character);

   --  ANCHOR(`killchar()',`Kill_Character')
   function Kill_Character return Character;
   --  AKA
   pragma Inline (Kill_Character);

   --  ANCHOR(`has_ic()',`Has_Insert_Character')
   function Has_Insert_Character return Boolean;
   --  AKA
   pragma Inline (Has_Insert_Character);

   --  ANCHOR(`has_il()',`Has_Insert_Line')
   function Has_Insert_Line return Boolean;
   --  AKA
   pragma Inline (Has_Insert_Line);

   --  ANCHOR(`termattrs()',`Supported_Attributes')
   function Supported_Attributes return Character_Attribute_Set;
   --  AKA
   pragma Inline (Supported_Attributes);

   --  ANCHOR(`longname()',`Long_Name')
   procedure Long_Name (Name : out String);
   --  AKA

   --  ANCHOR(`longname()',`Long_Name')
   function Long_Name return String;
   --  AKA
   --  Same as function
   pragma Inline (Long_Name);

   --  ANCHOR(`termname()',`Terminal_Name')
   procedure Terminal_Name (Name : out String);
   --  AKA

   --  ANCHOR(`termname()',`Terminal_Name')
   function Terminal_Name return String;
   --  AKA
   --  Same as function
   pragma Inline (Terminal_Name);

   --  MANPAGE(`curs_color.3x')

   --  COLOR_PAIR
   --  COLOR_PAIR(n) in C is the same as
   --  Attributed_Character(Ch => Nul, Color => n, Attr => Normal_Video)
   --  In C you often see something like c = c | COLOR_PAIR(n);
   --  This is equivalent to c.Color := n;

   --  ANCHOR(`start_color()',`Start_Color')
   procedure Start_Color;
   --  AKA
   pragma Import (C, Start_Color, "start_color");

   --  ANCHOR(`init_pair()',`Init_Pair')
   procedure Init_Pair (Pair : Redefinable_Color_Pair;
                        Fore : Color_Number;
                        Back : Color_Number);
   --  AKA
   pragma Inline (Init_Pair);

   --  ANCHOR(`pair_content()',`Pair_Content')
   procedure Pair_Content (Pair : Color_Pair;
                           Fore : out Color_Number;
                           Back : out Color_Number);
   --  AKA
   pragma Inline (Pair_Content);

   --  ANCHOR(`has_colors()',`Has_Colors')
   function Has_Colors return Boolean;
   --  AKA
   pragma Inline (Has_Colors);

   --  ANCHOR(`init_color()',`Init_Color')
   procedure Init_Color (Color : Color_Number;
                         Red   : RGB_Value;
                         Green : RGB_Value;
                         Blue  : RGB_Value);
   --  AKA
   pragma Inline (Init_Color);

   --  ANCHOR(`can_change_color()',`Can_Change_Color')
   function Can_Change_Color return Boolean;
   --  AKA
   pragma Inline (Can_Change_Color);

   --  ANCHOR(`color_content()',`Color_Content')
   procedure Color_Content (Color : Color_Number;
                            Red   : out RGB_Value;
                            Green : out RGB_Value;
                            Blue  : out RGB_Value);
   --  AKA
   pragma Inline (Color_Content);

   --  MANPAGE(`curs_kernel.3x')
   --  | Not implemented: getsyx, setsyx
   --
   type Curses_Mode is (Curses, Shell);

   --  ANCHOR(`def_prog_mode()',`Save_Curses_Mode')
   procedure Save_Curses_Mode (Mode : Curses_Mode);
   --  AKA
   --  ALIAS(`def_shell_mode()')
   pragma Inline (Save_Curses_Mode);

   --  ANCHOR(`reset_prog_mode()',`Reset_Curses_Mode')
   procedure Reset_Curses_Mode (Mode : Curses_Mode);
   --  AKA
   --  ALIAS(`reset_shell_mode()')
   pragma Inline (Reset_Curses_Mode);

   --  ANCHOR(`savetty()',`Save_Terminal_State')
   procedure Save_Terminal_State;
   --  AKA
   pragma Inline (Save_Terminal_State);

   --  ANCHOR(`resetty();',`Reset_Terminal_State')
   procedure Reset_Terminal_State;
   --  AKA
   pragma Inline (Reset_Terminal_State);

   type Stdscr_Init_Proc is access
      function (Win     : Window;
                Columns : Column_Count) return Integer;
   pragma Convention (C, Stdscr_Init_Proc);
   --  N.B.: the return value is actually ignored, but it seems to be
   --        a good practice to return 0 if you think all went fine
   --        and -1 otherwise.

   --  ANCHOR(`ripoffline()',`Rip_Off_Lines')
   procedure Rip_Off_Lines (Lines : Integer;
                            Proc  : Stdscr_Init_Proc);
   --  AKA
   --  N.B.: to be more precise, this uses a ncurses specific enhancement of
   --        ripoffline(), in which the Lines argument absolute value is the
   --        number of lines to be ripped of. The official ripoffline() only
   --        uses the sign of Lines to remove a single line from bottom or top.
   pragma Inline (Rip_Off_Lines);

   type Cursor_Visibility is (Invisible, Normal, Very_Visible);

   --  ANCHOR(`curs_set()',`Set_Cursor_Visibility')
   procedure Set_Cursor_Visibility (Visibility : in out Cursor_Visibility);
   --  AKA
   pragma Inline (Set_Cursor_Visibility);

   --  ANCHOR(`napms()',`Nap_Milli_Seconds')
   procedure Nap_Milli_Seconds (Ms : Natural);
   --  AKA
   pragma Inline (Nap_Milli_Seconds);

   --  |=====================================================================
   --  | Some useful helpers.
   --  |=====================================================================
   type Transform_Direction is (From_Screen, To_Screen);
   procedure Transform_Coordinates
     (W      : Window := Standard_Window;
      Line   : in out Line_Position;
      Column : in out Column_Position;
      Dir    : Transform_Direction := From_Screen);
   --  This procedure transforms screen coordinates into coordinates relative
   --  to the window and vice versa, depending on the Dir parameter.
   --  Screen coordinates are the position information for the physical device.
   --  An Curses_Exception will be raised if Line and Column are not in the
   --  Window or if you pass the Null_Window as argument.
   --  We do not inline this procedure

   --  MANPAGE(`default_colors.3x')

   Default_Color : constant Color_Number := -1;

   --  ANCHOR(`use_default_colors()',`Use_Default_Colors')
   procedure Use_Default_Colors;
   --  AKA
   pragma Inline (Use_Default_Colors);

   --  ANCHOR(`assume_default_colors()',`Assume_Default_Colors')
   procedure Assume_Default_Colors (Fore : Color_Number := Default_Color;
                                    Back : Color_Number := Default_Color);
   --  AKA
   pragma Inline (Assume_Default_Colors);

   --  MANPAGE(`curs_extend.3x')

   --  ANCHOR(`curses_version()',`Curses_Version')
   function Curses_Version return String;
   --  AKA

   --  ANCHOR(`use_extended_names()',`Use_Extended_Names')
   --  The returnvalue is the previous setting of the flag
   function Use_Extended_Names (Enable : Boolean) return Boolean;
   --  AKA

   --  MANPAGE(`curs_trace.3x')

   --  ANCHOR(`_nc_freeall()',`Curses_Free_All')
   procedure Curses_Free_All;
   --  AKA

   --  MANPAGE(`curs_scr_dump.3x')

   --  ANCHOR(`scr_dump()',`Screen_Dump_To_File')
   procedure Screen_Dump_To_File (Filename : String);
   --  AKA

   --  ANCHOR(`scr_restore()',`Screen_Restore_From_File')
   procedure Screen_Restore_From_File (Filename : String);
   --  AKA

   --  ANCHOR(`scr_init()',`Screen_Init_From_File')
   procedure Screen_Init_From_File (Filename : String);
   --  AKA

   --  ANCHOR(`scr_set()',`Screen_Set_File')
   procedure Screen_Set_File (Filename : String);
   --  AKA

   --  MANPAGE(`curs_print.3x')
   --  Not implemented: mcprint

   --  MANPAGE(`curs_printw.3x')
   --  Not implemented: printw,  wprintw, mvprintw, mvwprintw, vwprintw,
   --                   vw_printw
   --  Please use the Ada style Text_IO child packages for formatted
   --  printing. It does not make a lot of sense to map the printf style
   --  C functions to Ada.

   --  MANPAGE(`curs_scanw.3x')
   --  Not implemented: scanw, wscanw, mvscanw, mvwscanw, vwscanw, vw_scanw

   --  MANPAGE(`resizeterm.3x')
   --  Not Implemented: resizeterm

   --  MANPAGE(`wresize.3x')

   --  ANCHOR(`wresize()',`Resize')
   procedure Resize (Win               : Window := Standard_Window;
                     Number_Of_Lines   : Line_Count;
                     Number_Of_Columns : Column_Count);
   --  AKA

private
   type Window is new System.Storage_Elements.Integer_Address;
   Null_Window : constant Window := 0;

   --  The next constants are generated and may be different on your
   --  architecture.
   --

   Sizeof_Bool : constant := Curses_Constants.Sizeof_Bool;

   type Curses_Bool is mod 2 ** Sizeof_Bool;

   Curses_Bool_False : constant Curses_Bool := 0;

end Terminal_Interface.Curses;
