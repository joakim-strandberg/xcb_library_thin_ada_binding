--  Copyright 2003 Simon Wright <simon@pushface.org>

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

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $

--  This package provides interval measurements using the Pentium
--  timestamp clock. This clock counts the number of processor cycles
--  since the processor was started (modulo 2**64).
--
--  By default, the package initializes itself by measuring a "delay
--  1.0"; you can prevent this by setting the environment variable
--  CLOCK_RATE_GHZ to the correct value (eg, 1.123 for a 1123 MHz
--  machine).

with Interfaces;

package BC.Support.High_Resolution_Time is

   pragma Elaborate_Body;

   type Time is private;

   function Clock return Time;
   pragma Inline (Clock);

   function "-" (L, R : Time) return Duration;

private

   type Time is new Interfaces.Integer_64;

end BC.Support.High_Resolution_Time;
