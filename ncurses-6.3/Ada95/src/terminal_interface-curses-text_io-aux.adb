------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                   Terminal_Interface.Curses.Text_IO.Aux                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 1999-2006,2009 Free Software Foundation, Inc.                  --
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
--  $Revision: 1.14 $
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
package body Terminal_Interface.Curses.Text_IO.Aux is

   procedure Put_Buf
     (Win    : Window;
      Buf    : String;
      Width  : Field;
      Signal : Boolean := True;
      Ljust  : Boolean := False)
   is
      L   : Field;
      Len : Field;
      W   : Field := Width;
      LC  : Line_Count;
      CC  : Column_Count;
      Y   : Line_Position;
      X   : Column_Position;

      procedure Output (From, To : Field);

      procedure Output (From, To : Field)
      is
      begin
         if Len > 0 then
            if W = 0 then
               W := Len;
            end if;
            if Len > W then
               --  LRM A10.6 (7) says this
               W := Len;
            end if;

            pragma Assert (Len <= W);
            Get_Size (Win, LC, CC);
            if Column_Count (Len) > CC then
               if Signal then
                  raise Layout_Error;
               else
                  return;
               end if;
            else
               if Len < W and then not Ljust then
                  declare
                     Filler : constant String (1 .. (W - Len))
                       := (others => ' ');
                  begin
                     Put (Win, Filler);
                  end;
               end if;
               Get_Cursor_Position (Win, Y, X);
               if (X + Column_Position (Len)) > CC then
                  New_Line (Win);
               end if;
               Put (Win, Buf (From .. To));
               if Len < W and then Ljust then
                  declare
                     Filler : constant String (1 .. (W - Len))
                       := (others => ' ');
                  begin
                     Put (Win, Filler);
                  end;
               end if;
            end if;
         end if;
      end Output;

   begin
      pragma Assert (Win /= Null_Window);
      if Ljust then
         L := 1;
         for I in 1 .. Buf'Length loop
            exit when Buf (L) = ' ';
            L := L + 1;
         end loop;
         Len := L - 1;
         Output (1, Len);
      else  -- input buffer is not left justified
         L := Buf'Length;
         for I in 1 .. Buf'Length loop
            exit when Buf (L) = ' ';
            L := L - 1;
         end loop;
         Len := Buf'Length - L;
         Output (L + 1, Buf'Length);
      end if;
   end Put_Buf;

end Terminal_Interface.Curses.Text_IO.Aux;
