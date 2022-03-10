--  -*- ada -*-
define(`HTMLNAME',`terminal_interface-curses-aux__ads.htm')dnl
include(M4MACRO)dnl
------------------------------------------------------------------------------
--                                                                          --
--                           GNAT ncurses Binding                           --
--                                                                          --
--                       Terminal_Interface.Curses.Aux                      --
--                                                                          --
--                                 S P E C                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2018,2020 Thomas E. Dickey                                     --
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
--  $Revision: 1.25 $
--  Binding Version 01.00
------------------------------------------------------------------------------
with System;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

package Terminal_Interface.Curses.Aux is
   pragma Preelaborate (Terminal_Interface.Curses.Aux);

   subtype C_Int      is Interfaces.C.int;
   subtype C_Short    is Interfaces.C.short;
   subtype C_Long_Int is Interfaces.C.long;
   subtype C_Size_T   is Interfaces.C.size_t;
   subtype C_UInt     is Interfaces.C.unsigned;
   subtype C_ULong    is Interfaces.C.unsigned_long;
   subtype C_Char_Ptr is Interfaces.C.Strings.chars_ptr;
   type    C_Void_Ptr is new System.Address;

   --  This is how those constants are defined in ncurses. I see them also
   --  exactly like this in all ETI implementations I ever tested. So it
   --  could be that this is quite general, but please check with your curses.
   --  This is critical, because curses sometime mixes Boolean returns with
   --  returning an error status.
   Curses_Ok    : constant C_Int := Curses_Constants.OK;
   Curses_Err   : constant C_Int := Curses_Constants.ERR;

   Curses_True  : constant C_Int := Curses_Constants.TRUE;
   Curses_False : constant C_Int := Curses_Constants.FALSE;

   --  Eti_Error: type for error codes returned by the menu and form subsystem
   type Eti_Error is
     (E_Current,
      E_Invalid_Field,
      E_Request_Denied,
      E_Not_Connected,
      E_Not_Selectable,
      E_No_Match,
      E_Unknown_Command,
      E_Not_Posted,
      E_No_Room,
      E_Bad_State,
      E_Connected,
      E_Posted,
      E_Bad_Argument,
      E_System_Error,
      E_Ok);

   procedure Eti_Exception (Code : Eti_Error);
   --  Do nothing if Code = E_Ok.
   --  Else dispatch the error code and raise the appropriate exception.

   procedure Fill_String (Cp  : chars_ptr;
                          Str : out String);
   --  Fill the Str parameter with the string denoted by the chars_ptr
   --  C-Style string.

   function Fill_String (Cp : chars_ptr) return String;
   --  Same but as function.

private
   for Eti_Error'Size use C_Int'Size;
   pragma Convention (C, Eti_Error);
   for Eti_Error use
     (E_Current         => Curses_Constants.E_CURRENT,
      E_Invalid_Field   => Curses_Constants.E_INVALID_FIELD,
      E_Request_Denied  => Curses_Constants.E_REQUEST_DENIED,
      E_Not_Connected   => Curses_Constants.E_NOT_CONNECTED,
      E_Not_Selectable  => Curses_Constants.E_NOT_SELECTABLE,
      E_No_Match        => Curses_Constants.E_NO_MATCH,
      E_Unknown_Command => Curses_Constants.E_UNKNOWN_COMMAND,
      E_Not_Posted      => Curses_Constants.E_NOT_POSTED,
      E_No_Room         => Curses_Constants.E_NO_ROOM,
      E_Bad_State       => Curses_Constants.E_BAD_STATE,
      E_Connected       => Curses_Constants.E_CONNECTED,
      E_Posted          => Curses_Constants.E_POSTED,
      E_Bad_Argument    => Curses_Constants.E_BAD_ARGUMENT,
      E_System_Error    => Curses_Constants.E_SYSTEM_ERROR,
      E_Ok              => Curses_Constants.E_OK);
end Terminal_Interface.Curses.Aux;
