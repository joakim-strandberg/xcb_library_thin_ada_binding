--  Copyright 2003-2005 Simon Wright <simon@pushface.org>

--  This package is free software; you can redistribute it and/or
--  modify it under terms of the GNU General Public License as
--  published by the Free Software Foundation; either version 2, or
--  (at your option) any later version. This package is distributed in
--  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
--  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
--  PARTICULAR PURPOSE. See the GNU General Public License for more
--  details. You should have received a copy of the GNU General Public
--  License distributed with this package; see file COPYING.  If not,
--  write to the Free Software Foundation, 59 Temple Place - Suite
--  330, Boston, MA 02111-1307, USA.

--  As a special exception, if other files instantiate generics from
--  this unit, or you link this unit with other files to produce an
--  executable, this unit does not by itself cause the resulting
--  executable to be covered by the GNU General Public License.  This
--  exception does not however invalidate any other reasons why the
--  executable file might be covered by the GNU Public License.

--  $Revision: 1434 $
--  $Date: 2010-02-28 11:59:36 +0000 (Sun, 28 Feb 2010) $
--  $Author: simonjwright $

--  This common implementation devolves processor-specific high
--  resolution clock code to the separate body of Clock.

with Ada.Calendar;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;

with Ada.Text_IO; use Ada.Text_IO;

package body BC.Support.High_Resolution_Time is


   pragma Suppress (All_Checks);


   subtype GigaHertz is Duration range 0.0 .. Duration'Last;
   Clock_Rate_GigaHertz : GigaHertz := 800.0;


   function To_Duration
   is new Ada.Unchecked_Conversion (Interfaces.Integer_64, Duration);


   ---------
   -- "-" --
   ---------

   function "-" (L, R : Time) return Duration is
      use type Interfaces.Integer_64;
   begin
      --  Need to be a little careful here, I think, because with GHz
      --  processors the tick time is running close to Duration'Small.
      return
        To_Duration (Interfaces.Integer_64 (L) - Interfaces.Integer_64 (R))
        / Clock_Rate_GigaHertz;
   end "-";


   -----------
   -- Clock --
   -----------

   function Clock return Time is separate;


   procedure Initialize_Clock_Rate;
   procedure Initialize_Clock_Rate is
      use GNAT.OS_Lib;
      Rate : constant String_Access := Getenv ("CLOCK_RATE_GHZ");
   begin
      if Rate.all'Length > 0 then
         Clock_Rate_GigaHertz := GigaHertz'Value (Rate.all);
      else
         declare
            C : Time;
            T : Ada.Calendar.Time;
            D : Duration;
            use type Ada.Calendar.Time;
            use type Interfaces.Integer_64;
         begin
            delay 0.01;
            T := Ada.Calendar.Clock;
            C := Clock;
            delay 1.0;
            C := Time
              (Interfaces.Integer_64 (Clock) - Interfaces.Integer_64 (C));
            D := Ada.Calendar.Clock - T;
            --  A little careful here, we don't want to multiply D by
            --  1e9 in one go
            Clock_Rate_GigaHertz := Duration (C) / (D * 1e6);
            Clock_Rate_GigaHertz := Clock_Rate_GigaHertz / 1e3;
         end;
      end if;
      Put_Line ("Clock rate is" & Clock_Rate_GigaHertz'Img & " GHz");
   end Initialize_Clock_Rate;


begin
   Initialize_Clock_Rate;
end BC.Support.High_Resolution_Time;

--  vim: textwidth=0 nowrap tabstop=8 shiftwidth=3 softtabstop=3 expandtab
--  vim: filetype=ada encoding=latin1 fileformat=unix
