------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                          Sample.Form_Demo.Handler                        --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1998-2004,2009 Free Software Foundation, Inc.                  --
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
with Sample.Form_Demo.Aux;

package body Sample.Form_Demo.Handler is

   package Aux renames Sample.Form_Demo.Aux;

   procedure Drive_Me (F     : Form;
                       Title : String := "")
   is
      L : Line_Count;
      C : Column_Count;
      Y : Line_Position;
      X : Column_Position;
   begin
      Aux.Geometry (F, L, C, Y, X);
      Drive_Me (F, Y, X, Title);
   end Drive_Me;

   procedure Drive_Me (F     : Form;
                       Lin   : Line_Position;
                       Col   : Column_Position;
                       Title : String := "")
   is
      Pan : Panel := Aux.Create (F, Title, Lin, Col);
      V   : Cursor_Visibility := Normal;
      Handle_CRLF : Boolean := True;

   begin
      Set_Cursor_Visibility (V);
      if Aux.Count_Active (F) = 1 then
         Handle_CRLF := False;
      end if;
      loop
         declare
            K : constant Key_Code := Aux.Get_Request (F, Pan, Handle_CRLF);
            R : Driver_Result;
         begin
            if (K = 13 or else K = 10) and then not Handle_CRLF then
               R := Unknown_Request;
            else
               R := Driver (F, K);
            end if;
            case R is
               when Form_Ok => null;
               when Unknown_Request =>
                  if My_Driver (F, K, Pan) then
                     exit;
                  end if;
               when others => Beep;
            end case;
         end;
      end loop;
      Set_Cursor_Visibility (V);
      Aux.Destroy (F, Pan);
   end Drive_Me;

end Sample.Form_Demo.Handler;
