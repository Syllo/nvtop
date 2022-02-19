--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-menus__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                      Terminal_Interface.Curses.Menu                      --
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
--  $Revision: 1.32 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System;
with Ada.Characters.Latin_1;

package Terminal_Interface.Curses.Menus is
   pragma Preelaborate (Terminal_Interface.Curses.Menus);
   pragma Linker_Options ("-lmenu" & Curses_Constants.DFT_ARG_SUFFIX);

   Space : Character renames Ada.Characters.Latin_1.Space;

   type Item is private;
   type Menu is private;

   ---------------------------
   --  Interface constants  --
   ---------------------------
   Null_Item : constant Item;
   Null_Menu : constant Menu;

   subtype Menu_Request_Code is Key_Code
     range (Key_Max + 1) .. (Key_Max + 17);

   --  The prefix M_ stands for "Menu Request"
   M_Left_Item       : constant Menu_Request_Code := Key_Max + 1;
   M_Right_Item      : constant Menu_Request_Code := Key_Max + 2;
   M_Up_Item         : constant Menu_Request_Code := Key_Max + 3;
   M_Down_Item       : constant Menu_Request_Code := Key_Max + 4;
   M_ScrollUp_Line   : constant Menu_Request_Code := Key_Max + 5;
   M_ScrollDown_Line : constant Menu_Request_Code := Key_Max + 6;
   M_ScrollDown_Page : constant Menu_Request_Code := Key_Max + 7;
   M_ScrollUp_Page   : constant Menu_Request_Code := Key_Max + 8;
   M_First_Item      : constant Menu_Request_Code := Key_Max + 9;
   M_Last_Item       : constant Menu_Request_Code := Key_Max + 10;
   M_Next_Item       : constant Menu_Request_Code := Key_Max + 11;
   M_Previous_Item   : constant Menu_Request_Code := Key_Max + 12;
   M_Toggle_Item     : constant Menu_Request_Code := Key_Max + 13;
   M_Clear_Pattern   : constant Menu_Request_Code := Key_Max + 14;
   M_Back_Pattern    : constant Menu_Request_Code := Key_Max + 15;
   M_Next_Match      : constant Menu_Request_Code := Key_Max + 16;
   M_Previous_Match  : constant Menu_Request_Code := Key_Max + 17;

   --  For those who like the old 'C' names for the request codes
   REQ_LEFT_ITEM     : Menu_Request_Code renames M_Left_Item;
   REQ_RIGHT_ITEM    : Menu_Request_Code renames M_Right_Item;
   REQ_UP_ITEM       : Menu_Request_Code renames M_Up_Item;
   REQ_DOWN_ITEM     : Menu_Request_Code renames M_Down_Item;
   REQ_SCR_ULINE     : Menu_Request_Code renames M_ScrollUp_Line;
   REQ_SCR_DLINE     : Menu_Request_Code renames M_ScrollDown_Line;
   REQ_SCR_DPAGE     : Menu_Request_Code renames M_ScrollDown_Page;
   REQ_SCR_UPAGE     : Menu_Request_Code renames M_ScrollUp_Page;
   REQ_FIRST_ITEM    : Menu_Request_Code renames M_First_Item;
   REQ_LAST_ITEM     : Menu_Request_Code renames M_Last_Item;
   REQ_NEXT_ITEM     : Menu_Request_Code renames M_Next_Item;
   REQ_PREV_ITEM     : Menu_Request_Code renames M_Previous_Item;
   REQ_TOGGLE_ITEM   : Menu_Request_Code renames M_Toggle_Item;
   REQ_CLEAR_PATTERN : Menu_Request_Code renames M_Clear_Pattern;
   REQ_BACK_PATTERN  : Menu_Request_Code renames M_Back_Pattern;
   REQ_NEXT_MATCH    : Menu_Request_Code renames M_Next_Match;
   REQ_PREV_MATCH    : Menu_Request_Code renames M_Previous_Match;

   procedure Request_Name (Key  : Menu_Request_Code;
                           Name : out String);

   function  Request_Name (Key : Menu_Request_Code) return String;
   --  Same as function

   ------------------
   --  Exceptions  --
   ------------------

   Menu_Exception : exception;
   --
   --  Menu options
   --
   type Menu_Option_Set is
      record
         One_Valued        : Boolean;
         Show_Descriptions : Boolean;
         Row_Major_Order   : Boolean;
         Ignore_Case       : Boolean;
         Show_Matches      : Boolean;
         Non_Cyclic        : Boolean;
      end record;
   pragma Convention (C_Pass_By_Copy, Menu_Option_Set);

   for Menu_Option_Set use
      record
         One_Valued        at 0 range Curses_Constants.O_ONEVALUE_First
           .. Curses_Constants.O_ONEVALUE_Last;
         Show_Descriptions at 0 range Curses_Constants.O_SHOWDESC_First
           .. Curses_Constants.O_SHOWDESC_Last;
         Row_Major_Order   at 0 range Curses_Constants.O_ROWMAJOR_First
           .. Curses_Constants.O_ROWMAJOR_Last;
         Ignore_Case       at 0 range Curses_Constants.O_IGNORECASE_First
           .. Curses_Constants.O_IGNORECASE_Last;
         Show_Matches      at 0 range Curses_Constants.O_SHOWMATCH_First
           .. Curses_Constants.O_SHOWMATCH_Last;
         Non_Cyclic        at 0 range Curses_Constants.O_NONCYCLIC_First
           .. Curses_Constants.O_NONCYCLIC_Last;
      end record;
   pragma Warnings (Off);
   for Menu_Option_Set'Size use Curses_Constants.Menu_Options_Size;
   pragma Warnings (On);

   function Default_Menu_Options return Menu_Option_Set;
   --  Initial default options for a menu.
   pragma Inline (Default_Menu_Options);
   --
   --  Item options
   --
   type Item_Option_Set is
      record
         Selectable : Boolean;
      end record;
   pragma Convention (C_Pass_By_Copy, Item_Option_Set);

   for Item_Option_Set use
      record
         Selectable at 0 range Curses_Constants.O_SELECTABLE_First
           ..  Curses_Constants.O_SELECTABLE_Last;
      end record;
   pragma Warnings (Off);
   for Item_Option_Set'Size use Curses_Constants.Item_Options_Size;
   pragma Warnings (On);

   function Default_Item_Options return Item_Option_Set;
   --  Initial default options for an item.
   pragma Inline (Default_Item_Options);

   --
   --  Item Array
   --
   type Item_Array is array (Positive range <>) of aliased Item;
   pragma Convention (C, Item_Array);

   type Item_Array_Access is access Item_Array;

   procedure Free (IA         : in out Item_Array_Access;
                   Free_Items : Boolean := False);
   --  Release the memory for an allocated item array
   --  If Free_Items is True, call Delete() for all the items in
   --  the array.

   --  MANPAGE(`mitem_new.3x')

   --  ANCHOR(`new_item()',`Create')
   function Create (Name        : String;
                    Description : String := "") return Item;
   --  AKA
   --  Not inlined.

   --  ANCHOR(`new_item()',`New_Item')
   function New_Item (Name        : String;
                      Description : String := "") return Item
     renames Create;
   --  AKA

   --  ANCHOR(`free_item()',`Delete')
   procedure Delete (Itm : in out Item);
   --  AKA
   --  Resets Itm to Null_Item

   --  MANPAGE(`mitem_value.3x')

   --  ANCHOR(`set_item_value()',`Set_Value')
   procedure Set_Value (Itm   : Item;
                        Value : Boolean := True);
   --  AKA
   pragma Inline (Set_Value);

   --  ANCHOR(`item_value()',`Value')
   function Value (Itm : Item) return Boolean;
   --  AKA
   pragma Inline (Value);

   --  MANPAGE(`mitem_visible.3x')

   --  ANCHOR(`item_visible()',`Visible')
   function Visible (Itm : Item) return Boolean;
   --  AKA
   pragma Inline (Visible);

   --  MANPAGE(`mitem_opts.3x')

   --  ANCHOR(`set_item_opts()',`Set_Options')
   procedure Set_Options (Itm     : Item;
                          Options : Item_Option_Set);
   --  AKA
   --  An overloaded Set_Options is defined later. Pragma Inline appears there

   --  ANCHOR(`item_opts_on()',`Switch_Options')
   procedure Switch_Options (Itm     : Item;
                             Options : Item_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`item_opts_off()')
   --  An overloaded Switch_Options is defined later.
   --  Pragma Inline appears there

   --  ANCHOR(`item_opts()',`Get_Options')
   procedure Get_Options (Itm     : Item;
                          Options : out Item_Option_Set);
   --  AKA

   --  ANCHOR(`item_opts()',`Get_Options')
   function Get_Options (Itm : Item := Null_Item) return Item_Option_Set;
   --  AKA
   --  An overloaded Get_Options is defined later. Pragma Inline appears there

   --  MANPAGE(`mitem_name.3x')

   --  ANCHOR(`item_name()',`Name')
   procedure Name (Itm  : Item;
                   Name : out String);
   --  AKA
   function  Name (Itm : Item) return String;
   --  AKA
   --  Implemented as function
   pragma Inline (Name);

   --  ANCHOR(`item_description();',`Description')
   procedure Description (Itm         : Item;
                          Description : out String);
   --  AKA

   function  Description (Itm : Item) return String;
   --  AKA
   --  Implemented as function
   pragma Inline (Description);

   --  MANPAGE(`mitem_current.3x')

   --  ANCHOR(`set_current_item()',`Set_Current')
   procedure Set_Current (Men : Menu;
                          Itm : Item);
   --  AKA
   pragma Inline (Set_Current);

   --  ANCHOR(`current_item()',`Current')
   function Current (Men : Menu) return Item;
   --  AKA
   pragma Inline (Current);

   --  ANCHOR(`set_top_row()',`Set_Top_Row')
   procedure Set_Top_Row (Men  : Menu;
                          Line : Line_Position);
   --  AKA
   pragma Inline (Set_Top_Row);

   --  ANCHOR(`top_row()',`Top_Row')
   function Top_Row (Men : Menu) return Line_Position;
   --  AKA
   pragma Inline (Top_Row);

   --  ANCHOR(`item_index()',`Get_Index')
   function Get_Index (Itm : Item) return Positive;
   --  AKA
   --  Please note that in this binding we start the numbering of items
   --  with 1. So this is number is one more than you get from the low
   --  level call.
   pragma Inline (Get_Index);

   --  MANPAGE(`menu_post.3x')

   --  ANCHOR(`post_menu()',`Post')
   procedure Post (Men  : Menu;
                   Post : Boolean := True);
   --  AKA
   --  ALIAS(`unpost_menu()')
   pragma Inline (Post);

   --  MANPAGE(`menu_opts.3x')

   --  ANCHOR(`set_menu_opts()',`Set_Options')
   procedure Set_Options (Men     : Menu;
                          Options : Menu_Option_Set);
   --  AKA
   pragma Inline (Set_Options);

   --  ANCHOR(`menu_opts_on()',`Switch_Options')
   procedure Switch_Options (Men     : Menu;
                             Options : Menu_Option_Set;
                             On      : Boolean := True);
   --  AKA
   --  ALIAS(`menu_opts_off()')
   pragma Inline (Switch_Options);

   --  ANCHOR(`menu_opts()',`Get_Options')
   procedure Get_Options (Men     : Menu;
                          Options : out Menu_Option_Set);
   --  AKA

   --  ANCHOR(`menu_opts()',`Get_Options')
   function Get_Options (Men : Menu := Null_Menu) return Menu_Option_Set;
   --  AKA
   pragma Inline (Get_Options);

   --  MANPAGE(`menu_win.3x')

   --  ANCHOR(`set_menu_win()',`Set_Window')
   procedure Set_Window (Men : Menu;
                         Win : Window);
   --  AKA
   pragma Inline (Set_Window);

   --  ANCHOR(`menu_win()',`Get_Window')
   function Get_Window (Men : Menu) return Window;
   --  AKA
   pragma Inline (Get_Window);

   --  ANCHOR(`set_menu_sub()',`Set_Sub_Window')
   procedure Set_Sub_Window (Men : Menu;
                             Win : Window);
   --  AKA
   pragma Inline (Set_Sub_Window);

   --  ANCHOR(`menu_sub()',`Get_Sub_Window')
   function Get_Sub_Window (Men : Menu) return Window;
   --  AKA
   pragma Inline (Get_Sub_Window);

   --  ANCHOR(`scale_menu()',`Scale')
   procedure Scale (Men     : Menu;
                    Lines   : out Line_Count;
                    Columns : out Column_Count);
   --  AKA
   pragma Inline (Scale);

   --  MANPAGE(`menu_cursor.3x')

   --  ANCHOR(`pos_menu_cursor()',`Position_Cursor')
   procedure Position_Cursor (Men : Menu);
   --  AKA
   pragma Inline (Position_Cursor);

   --  MANPAGE(`menu_mark.3x')

   --  ANCHOR(`set_menu_mark()',`Set_Mark')
   procedure Set_Mark (Men  : Menu;
                       Mark : String);
   --  AKA
   pragma Inline (Set_Mark);

   --  ANCHOR(`menu_mark()',`Mark')
   procedure Mark (Men  : Menu;
                   Mark : out String);
   --  AKA

   function  Mark (Men : Menu) return String;
   --  AKA
   --  Implemented as function
   pragma Inline (Mark);

   --  MANPAGE(`menu_attributes.3x')

   --  ANCHOR(`set_menu_fore()',`Set_Foreground')
   procedure Set_Foreground
     (Men   : Menu;
      Fore  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Foreground);

   --  ANCHOR(`menu_fore()',`Foreground')
   procedure Foreground (Men   : Menu;
                         Fore  : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`menu_fore()',`Foreground')
   procedure Foreground (Men   : Menu;
                         Fore  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA
   pragma Inline (Foreground);

   --  ANCHOR(`set_menu_back()',`Set_Background')
   procedure Set_Background
     (Men   : Menu;
      Back  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Background);

   --  ANCHOR(`menu_back()',`Background')
   procedure Background (Men  : Menu;
                         Back : out Character_Attribute_Set);
   --  AKA
   --  ANCHOR(`menu_back()',`Background')

   procedure Background (Men   : Menu;
                         Back  : out Character_Attribute_Set;
                         Color : out Color_Pair);
   --  AKA
   pragma Inline (Background);

   --  ANCHOR(`set_menu_grey()',`Set_Grey')
   procedure Set_Grey
     (Men   : Menu;
      Grey  : Character_Attribute_Set := Normal_Video;
      Color : Color_Pair := Color_Pair'First);
   --  AKA
   pragma Inline (Set_Grey);

   --  ANCHOR(`menu_grey()',`Grey')
   procedure Grey (Men  : Menu;
                   Grey : out Character_Attribute_Set);
   --  AKA

   --  ANCHOR(`menu_grey()',`Grey')
   procedure Grey
     (Men   : Menu;
      Grey  : out Character_Attribute_Set;
      Color : out Color_Pair);
   --  AKA
   pragma Inline (Grey);

   --  ANCHOR(`set_menu_pad()',`Set_Pad_Character')
   procedure Set_Pad_Character (Men : Menu;
                                Pad : Character := Space);
   --  AKA
   pragma Inline (Set_Pad_Character);

   --  ANCHOR(`menu_pad()',`Pad_Character')
   procedure Pad_Character (Men : Menu;
                            Pad : out Character);
   --  AKA
   pragma Inline (Pad_Character);

   --  MANPAGE(`menu_spacing.3x')

   --  ANCHOR(`set_menu_spacing()',`Set_Spacing')
   procedure Set_Spacing (Men   : Menu;
                          Descr : Column_Position := 0;
                          Row   : Line_Position   := 0;
                          Col   : Column_Position := 0);
   --  AKA
   pragma Inline (Set_Spacing);

   --  ANCHOR(`menu_spacing()',`Spacing')
   procedure Spacing (Men   : Menu;
                      Descr : out Column_Position;
                      Row   : out Line_Position;
                      Col   : out Column_Position);
   --  AKA
   pragma Inline (Spacing);

   --  MANPAGE(`menu_pattern.3x')

   --  ANCHOR(`set_menu_pattern()',`Set_Pattern')
   function Set_Pattern (Men  : Menu;
                         Text : String) return Boolean;
   --  AKA
   --  Return TRUE if the pattern matches, FALSE otherwise
   pragma Inline (Set_Pattern);

   --  ANCHOR(`menu_pattern()',`Pattern')
   procedure Pattern (Men  : Menu;
                      Text : out String);
   --  AKA
   pragma Inline (Pattern);

   --  MANPAGE(`menu_format.3x')

   --  ANCHOR(`set_menu_format()',`Set_Format')
   procedure Set_Format (Men     : Menu;
                         Lines   : Line_Count;
                         Columns : Column_Count);
   --  Not implemented: 0 argument for Lines or Columns;
   --  instead use Format to get the current sizes
   --      The  default  format  is  16  rows,  1  column.    Calling
   --      set_menu_format  with a null menu pointer will change this
   --      default.  A zero row or column argument to set_menu_format
   --      is  interpreted  as  a  request  not to change the current
   --      value.
   --  AKA
   pragma Inline (Set_Format);

   --  ANCHOR(`menu_format()',`Format')
   procedure Format (Men     : Menu;
                     Lines   : out Line_Count;
                     Columns : out Column_Count);
   --  AKA
   pragma Inline (Format);

   --  MANPAGE(`menu_hook.3x')

   type Menu_Hook_Function is access procedure (Men : Menu);
   pragma Convention (C, Menu_Hook_Function);

   --  ANCHOR(`set_item_init()',`Set_Item_Init_Hook')
   procedure Set_Item_Init_Hook (Men  : Menu;
                                 Proc : Menu_Hook_Function);
   --  AKA
   pragma Inline (Set_Item_Init_Hook);

   --  ANCHOR(`set_item_term()',`Set_Item_Term_Hook')
   procedure Set_Item_Term_Hook (Men  : Menu;
                                 Proc : Menu_Hook_Function);
   --  AKA
   pragma Inline (Set_Item_Term_Hook);

   --  ANCHOR(`set_menu_init()',`Set_Menu_Init_Hook')
   procedure Set_Menu_Init_Hook (Men  : Menu;
                                 Proc : Menu_Hook_Function);
   --  AKA
   pragma Inline (Set_Menu_Init_Hook);

   --  ANCHOR(`set_menu_term()',`Set_Menu_Term_Hook')
   procedure Set_Menu_Term_Hook (Men  : Menu;
                                 Proc : Menu_Hook_Function);
   --  AKA
   pragma Inline (Set_Menu_Term_Hook);

   --  ANCHOR(`item_init()',`Get_Item_Init_Hook')
   function Get_Item_Init_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA
   pragma Inline (Get_Item_Init_Hook);

   --  ANCHOR(`item_term()',`Get_Item_Term_Hook')
   function Get_Item_Term_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA
   pragma Inline (Get_Item_Term_Hook);

   --  ANCHOR(`menu_init()',`Get_Menu_Init_Hook')
   function Get_Menu_Init_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA
   pragma Inline (Get_Menu_Init_Hook);

   --  ANCHOR(`menu_term()',`Get_Menu_Term_Hook')
   function Get_Menu_Term_Hook (Men : Menu) return Menu_Hook_Function;
   --  AKA
   pragma Inline (Get_Menu_Term_Hook);

   --  MANPAGE(`menu_items.3x')

   --  ANCHOR(`set_menu_items()',`Redefine')
   procedure Redefine (Men   : Menu;
                       Items : Item_Array_Access);
   --  AKA
   pragma Inline (Redefine);

   procedure Set_Items (Men   : Menu;
                        Items : Item_Array_Access) renames Redefine;
   --  pragma Inline (Set_Items);

   --  ANCHOR(`menu_items()',`Items')
   function Items (Men   : Menu;
                   Index : Positive) return Item;
   --  AKA
   pragma Inline (Items);

   --  ANCHOR(`item_count()',`Item_Count')
   function Item_Count (Men : Menu) return Natural;
   --  AKA
   pragma Inline (Item_Count);

   --  MANPAGE(`menu_new.3x')

   --  ANCHOR(`new_menu()',`Create')
   function Create (Items : Item_Array_Access) return Menu;
   --  AKA
   --  Not inlined

   function New_Menu (Items : Item_Array_Access) return Menu renames Create;

   --  ANCHOR(`free_menu()',`Delete')
   procedure Delete (Men : in out Menu);
   --  AKA
   --  Reset Men to Null_Menu
   --  Not inlined

   --  MANPAGE(`menu_driver.3x')

   type Driver_Result is (Menu_Ok,
                          Request_Denied,
                          Unknown_Request,
                          No_Match);

   --  ANCHOR(`menu_driver()',`Driver')
   function Driver (Men : Menu;
                    Key : Key_Code) return Driver_Result;
   --  AKA
   --  Driver is not inlined

   --  ANCHOR(`menu_requestname.3x')
   --  Not Implemented: menu_request_name, menu_request_by_name
-------------------------------------------------------------------------------
private
   type Item   is new System.Storage_Elements.Integer_Address;
   type Menu   is new System.Storage_Elements.Integer_Address;

   Null_Item : constant Item := 0;
   Null_Menu : constant Menu := 0;

end Terminal_Interface.Curses.Menus;
