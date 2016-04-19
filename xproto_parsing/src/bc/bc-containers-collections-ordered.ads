--  Copyright 1994 Grady Booch
--  Copyright 1998-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1488 $
--  $Date: 2011-09-01 16:47:02 +0100 (Thu, 01 Sep 2011) $
--  $Author: simonjwright $

generic

   with function "<" (L, R : Item) return Boolean is <>;
   --  Must define a strict ordering; if A "<" B and B "<" C, A must
   --  be "<" C. If A is not "<" B and B is not "<" A, A and B are
   --  said to be equivalent (they need not be "=").

package BC.Containers.Collections.Ordered is

   pragma Preelaborate;

   type Abstract_Ordered_Collection
      is abstract new Abstract_Collection with private;

   --  An ordered collection denotes a sorted indexed collection of
   --  items, drawn from some well-defined universe. An ordered
   --  collection may contain duplicate (equivalent) items; it owns a
   --  copy of each item.

private

   --  Suppress "unreferenced" warnings here (GNAT 5.02). Can't use
   --  pragma Unreferenced, because then we get warnings in child
   --  packages.
   pragma Warnings (Off, "<");

   type Abstract_Ordered_Collection
      is abstract new Abstract_Collection with null record;

end BC.Containers.Collections.Ordered;
