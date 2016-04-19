--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2007 Simon Wright <simon@pushface.org>

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

generic package BC.Containers.Lists is

   -------------------------------------------------------------------
   --  WARNING: If  you just want a standard  container to support  --
   --  iteration, filtering and sorting, use Collections. The List  --
   --  components are much more complex than you'll need.           --
   -------------------------------------------------------------------

   pragma Preelaborate;

   --  Lists are polylithic structures, and hence the semantics of
   --  copying, assignment, and equality involve structural
   --  sharing. Care must be taken in manipulating the same list named
   --  by more than one alias.

   --  A single list is a rooted sequence of zero or more items, with
   --  a link from one item to its following item. A double list is a
   --  rooted sequence of zero or more items, with links from one item
   --  to its previous and next items.

   --  These classes are not intended to be subclassed.

   --  These abstractions have been carefully constructed to eliminate
   --  all storage leaks, except in the case of intentional
   --  abuses. When a list is manipulated, all items that become
   --  unreachable are automatically reclaimed. Furthermore, this
   --  design protects against dangling references: an item is never
   --  reclaimed if there exists a reference to it.

   --  Unreachable items are those that belong to a list or a sublist
   --  whose head is not designated by any alias. For example,
   --  consider the list (A B C), with the head of the list designated
   --  by L1. L1 initially points to the head of the list, at item
   --  A. Invoking the member function Tail on L1 now causes L1 to
   --  point to item B. Because A is now considered unreachable, the
   --  storage associated with item A is reclaimed; the predecessor of
   --  B is now null. Similarly, consider the list (D E F), with the
   --  head of the list designated by both L1 and L2. Both L1 and L2
   --  are aliases that initially point to the head of the list at
   --  item D. Invoking the member function Tail on L1 now causes L1
   --  to point to item E; L2 is unaffected. Suppose we now invoke the
   --  member function clear on L2. The semantics of this operation
   --  are such that only unreachable items are reclaimed. Thus, the
   --  storage associated with item D is reclaimed, because it is no
   --  longer reachable; L2 is now null, and the predecessor of E is
   --  now null. Items E and F are not reclaimed, because they are
   --  reachable through L1.

   --  An alternate abstraction would have been to consider items A
   --  and D reachable for doubly-linked lists in these two
   --  circumstances. We explicitly rejected this design, in order to
   --  maintain consistency with the semantics of the singly-linked
   --  list.

   --  It is possible, but not generally desirable, to produce
   --  multi-headed lists. In such cases, the predecessor of the item
   --  at the neck of a multi-headed list points to the most recently
   --  attached head. If doubly-linked lists share items, then the
   --  predecessor if an item is the one to which it was most recently
   --  attached as a successor.

   --  The singly-linked and doubly-linked lists have a similar
   --  protocol, except that the doubly-linked list adds two
   --  operations, Predecessor and Is_Head. The space semantics of
   --  these two classes are different (the doubly-linked list has one
   --  extra pointer per item) and additionally, their time semantics
   --  are slightly different, because of the optimizations possible
   --  with having a previous pointer in the doubly-linked list class.

end BC.Containers.Lists;
