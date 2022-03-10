------------------------------------------------------------------------------
--                                                                          --
--                       GNAT ncurses Binding Samples                       --
--                                                                          --
--                                 ncurses                                  --
--                                                                          --
--                                 B O D Y                                  --
--                                                                          --
------------------------------------------------------------------------------
-- Copyright 2020 Thomas E. Dickey                                          --
-- Copyright 2000-2008,2011 Free Software Foundation, Inc.                  --
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
--  $Date: 2020/02/02 23:34:34 $
--  Binding Version 01.00
------------------------------------------------------------------------------
--  A simplified version of the  GNU getopt function
--  copyright Free Software Foundtion

with Ada.Strings.Fixed;
with Ada.Strings.Bounded;
with Ada.Text_IO; use Ada.Text_IO;

package body ncurses2.getopt is

   nextchar : Natural := 0;

   --  Ncurses doesn't use the non option elements so we are spared
   --  the job of computing those.

   --  also the user is not allowed to modify argv or argc
   --  Doing so is Erroneous execution.

   --  long options are not handled.

   procedure Qgetopt (retval : out Integer;
                      argc : Integer;
                      argv : stringfunc;
                        --  argv will be the Argument function.
                      optstring : String;
                      optind : in out Integer;
                        --  ignored for ncurses, must be initialized to 1 by
                        --  the caller
                      Optarg : out stringa
                        --  a garbage collector would be useful here.
                     ) is

      package BS is new Ada.Strings.Bounded.Generic_Bounded_Length (200);
      use BS;
      optargx : Bounded_String;
   begin

      if argc < optind then
         retval := -1;
         return;
      end if;

      optargx := To_Bounded_String ("");

      if nextchar = 0 then

         if argv (optind) = "--" then
                           --  the rest are non-options, we ignore them
            retval := -1;
            return;
         end if;

         if argv (optind)(1) /= '-' or argv (optind)'Length = 1 then
            optind := optind + 1;
            Optarg := new String'(argv (optind));
            retval := 1;
            return;
         end if;

         nextchar := 2; -- skip the one hyphen.
      end if;

      --  Look at and handle the next short option-character.
      declare
         c : Character := argv (optind) (nextchar);
         temp : constant Natural :=
           Ada.Strings.Fixed.Index (optstring, String'(1 => c));
      begin
         if temp = 0 or c = ':' then
            Put_Line (Standard_Error,
                      argv (optind) & ": invalid option -- " & c);
            c := '?';
            return;
         end if;

         if optstring (temp + 1) = ':' then
            if optstring (temp + 2) = ':' then
               --  This is an option that accepts an argument optionally.
               if nextchar /= argv (optind)'Length then
                  optargx := To_Bounded_String
                    (argv (optind) (nextchar .. argv (optind)'Length));
               else
                  Optarg := null;
               end if;
            else
               --  This is an option that requires an argument.
               if nextchar /= argv (optind)'Length then
                  optargx := To_Bounded_String
                    (argv (optind) (nextchar .. argv (optind)'Length));
                  optind := optind + 1;
               elsif optind = argc then
                  Put_Line (Standard_Error,
                            argv (optind) &
                            ": option requires an argument -- " & c);
                  if optstring (optstring'First) = ':'  then
                     c := ':';
                  else
                     c := '?';
                  end if;
               else
                  --  increment it again when taking next ARGV-elt as argument.
                  optind := optind + 1;
                  optargx := To_Bounded_String (argv (optind));
                  optind := optind + 1;
               end if;
            end if;
            nextchar := 0;
         else -- no argument for the option
            if nextchar = argv (optind)'Length then
               optind := optind + 1;
               nextchar := 0;
            else
               nextchar := nextchar + 1;
            end if;
         end if;

         retval := Character'Pos (c);
         Optarg := new String'(To_String (optargx));
         return;
      end;
   end Qgetopt;

end ncurses2.getopt;
