------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                            Sample.Menu_Demo.Aux                          --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2006,2009 Free Software Foundation, Inc.                  --
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
--  Version Control
--  $Revision: 1.15 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with Ada.Characters.Latin_1; use Ada.Characters.Latin_1;

with Sample.Manifest; use Sample.Manifest;
with Sample.Helpers; use Sample.Helpers;
with Sample.Keyboard_Handler; use Sample.Keyboard_Handler;
with Sample.Explanation; use Sample.Explanation;

package body Sample.Menu_Demo.Aux is

   procedure Geometry (M  : Menu;
                       L  : out Line_Count;
                       C  : out Column_Count;
                       Y  : out Line_Position;
                       X  : out Column_Position;
                       Fy : out Line_Position;
                       Fx : out Column_Position);

   procedure Geometry (M  : Menu;
                       L  : out Line_Count;        -- Lines used for menu
                       C  : out Column_Count;      -- Columns used for menu
                       Y  : out Line_Position;     -- Proposed Line for menu
                       X  : out Column_Position;   -- Proposed Column for menu
                       Fy : out Line_Position;     -- Vertical inner frame
                       Fx : out Column_Position)   -- Horiz. inner frame
   is
      Spc_Desc : Column_Position; -- spaces between description and item
   begin
      Set_Mark (M, Menu_Marker);

      Spacing (M, Spc_Desc, Fy, Fx);
      Scale (M, L, C);

      Fx := Fx + Column_Position (Fy - 1); -- looks a bit nicer

      L := L + 2 * Fy;  -- count for frame at top and bottom
      C := C + 2 * Fx;  -- "

      --  Calculate horizontal coordinate at the screen center
      X := (Columns - C) / 2;
      Y := 1;  -- always startin line 1

   end Geometry;

   procedure Geometry (M : Menu;
                       L : out Line_Count;        -- Lines used for menu
                       C : out Column_Count;      -- Columns used for menu
                       Y : out Line_Position;     -- Proposed Line for menu
                       X : out Column_Position)   -- Proposed Column for menu
   is
      Fy : Line_Position;
      Fx : Column_Position;
   begin
      Geometry (M, L, C, Y, X, Fy, Fx);
   end Geometry;

   function Create (M     : Menu;
                    Title : String;
                    Lin   : Line_Position;
                    Col   : Column_Position) return Panel
   is
      W, S : Window;
      L : Line_Count;
      C : Column_Count;
      Y, Fy : Line_Position;
      X, Fx : Column_Position;
      Pan : Panel;
   begin
      Geometry (M, L, C, Y, X, Fy, Fx);
      W := New_Window (L, C, Lin, Col);
      Set_Meta_Mode (W);
      Set_KeyPad_Mode (W);
      if Has_Colors then
         Set_Background (Win => W,
                         Ch  => (Ch    => ' ',
                                 Color => Menu_Back_Color,
                                 Attr  => Normal_Video));
         Set_Foreground (Men => M, Color => Menu_Fore_Color);
         Set_Background (Men => M, Color => Menu_Back_Color);
         Set_Grey (Men => M, Color => Menu_Grey_Color);
         Erase (W);
      end if;
      S := Derived_Window (W, L - Fy, C - Fx, Fy, Fx);
      Set_Meta_Mode (S);
      Set_KeyPad_Mode (S);
      Box (W);
      Set_Window (M, W);
      Set_Sub_Window (M, S);
      if Title'Length > 0 then
         Window_Title (W, Title);
      end if;
      Pan := New_Panel (W);
      Post (M);
      return Pan;
   end Create;

   procedure Destroy (M : Menu;
                      P : in out Panel)
   is
      W, S : Window;
   begin
      W := Get_Window (M);
      S := Get_Sub_Window (M);
      Post (M, False);
      Erase (W);
      Delete (P);
      Set_Window (M, Null_Window);
      Set_Sub_Window (M, Null_Window);
      Delete (S);
      Delete (W);
      Update_Panels;
   end Destroy;

   function Get_Request (M : Menu; P : Panel) return Key_Code
   is
      W  : constant Window := Get_Window (M);
      K  : Real_Key_Code;
      Ch : Character;
   begin
      Top (P);
      loop
         K := Get_Key (W);
         if K in Special_Key_Code'Range then
            case K is
               when HELP_CODE           => Explain_Context;
               when EXPLAIN_CODE        => Explain ("MENUKEYS");
               when Key_Home            => return REQ_FIRST_ITEM;
               when QUIT_CODE           => return QUIT;
               when Key_Cursor_Down     => return REQ_DOWN_ITEM;
               when Key_Cursor_Up       => return REQ_UP_ITEM;
               when Key_Cursor_Left     => return REQ_LEFT_ITEM;
               when Key_Cursor_Right    => return REQ_RIGHT_ITEM;
               when Key_End             => return REQ_LAST_ITEM;
               when Key_Backspace       => return REQ_BACK_PATTERN;
               when Key_Next_Page       => return REQ_SCR_DPAGE;
               when Key_Previous_Page   => return REQ_SCR_UPAGE;
               when others              => return K;
            end case;
         elsif K in Normal_Key_Code'Range then
            Ch := Character'Val (K);
            case Ch is
               when CAN => return QUIT;                  --  CTRL-X
               when SO  => return REQ_NEXT_ITEM;         --  CTRL-N
               when DLE => return REQ_PREV_ITEM;         --  CTRL-P
               when NAK => return REQ_SCR_ULINE;         --  CTRL-U
               when EOT => return REQ_SCR_DLINE;         --  CTRL-D
               when ACK => return REQ_SCR_DPAGE;         --  CTRL-F
               when STX => return REQ_SCR_UPAGE;         --  CTRL-B
               when EM  => return REQ_CLEAR_PATTERN;     --  CTRL-Y
               when BS  => return REQ_BACK_PATTERN;      --  CTRL-H
               when SOH => return REQ_NEXT_MATCH;        --  CTRL-A
               when ENQ => return REQ_PREV_MATCH;        --  CTRL-E
               when DC4 => return REQ_TOGGLE_ITEM;       --  CTRL-T

               when CR | LF  => return SELECT_ITEM;
               when others   => return K;
            end case;
         else
            return K;
         end if;
      end loop;
   end Get_Request;

end Sample.Menu_Demo.Aux;
