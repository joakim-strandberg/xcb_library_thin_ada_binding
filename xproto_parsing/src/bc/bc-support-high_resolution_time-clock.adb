--  Copyright 2010-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1476 $
--  $Date: 2011-08-06 20:56:57 +0100 (Sat, 06 Aug 2011) $
--  $Author: simonjwright $

--  This implementation is for operating systems where the compiler
--  can provide adequate resolution (for example, Mac OS X, at least
--  on Intel hardware).

with Ada.Unchecked_Conversion;

separate (BC.Support.High_Resolution_Time)
function Clock return Time is
   pragma Warnings (Off, "representation of Time values may change*");
   function To_Time is new Ada.Unchecked_Conversion (Ada.Calendar.Time, Time);
   pragma Warnings (On, "representation of Time values may change*");
begin
   return To_Time (Ada.Calendar.Clock);
end Clock;
