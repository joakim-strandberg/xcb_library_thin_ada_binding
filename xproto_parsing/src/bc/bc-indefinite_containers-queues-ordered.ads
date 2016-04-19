--  Copyright 1994 Grady Booch
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>
--  Copyright 2005 Martin Krischik

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

--  $Revision: 1420 $
--  $Date: 2009-09-26 18:42:21 +0100 (Sat, 26 Sep 2009) $
--  $Author: simonjwright $

generic
   with function "<" (L, R : Item) return Boolean is <>;
package BC.Indefinite_Containers.Queues.Ordered is

   pragma Preelaborate;

   type Abstract_Ordered_Queue is abstract new Abstract_Queue with private;

   --  A ordered queue denotes a sequence of items, in which items are
   --  stored and removed in order.

private

   --  Suppress "unreferenced" warnings here (GNAT 5.02). Can't use
   --  pragma Unreferenced, because then we get warnings in child
   --  packages.
   pragma Warnings (Off, "<");

   type Abstract_Ordered_Queue is abstract new Abstract_Queue with null record;

end BC.Indefinite_Containers.Queues.Ordered;
