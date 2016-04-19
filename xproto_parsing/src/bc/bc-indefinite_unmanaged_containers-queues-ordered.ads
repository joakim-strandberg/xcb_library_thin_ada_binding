--  Copyright 1994 Grady Booch
--  Copyright 2003-2012 Simon Wright <simon@pushface.org>
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

--  $Revision: 1508 $
--  $Date: 2012-06-03 19:01:11 +0100 (Sun, 03 Jun 2012) $
--  $Author: simonjwright $

generic

   with function "<" (L, R : Item) return Boolean is <>;
   --  Must define a strict ordering; if A "<" B and B "<" C, A must
   --  be "<" C. If A is not "<" B and B is not "<" A, A and B are
   --  said to be equivalent (they need not be "=").

package BC.Indefinite_Unmanaged_Containers.Queues.Ordered is

   pragma Preelaborate;

   type Queue is new Queues.Queue with private;
   --  This Queue exhibits unlimited growth and collapsing, limited
   --  only by available memory.

   function Null_Container return Queue;

   procedure Append (Q : in out Queue; Elem : Item);
   --  Add the item to the queue at the appropriate position; if any
   --  equivalent items are found, the new item is inserted after the
   --  last equivalent item.

   function New_Iterator (For_The_Queue : Queue) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Queue.

private

   type Queue is new Queues.Queue with null record;

end BC.Indefinite_Unmanaged_Containers.Queues.Ordered;
