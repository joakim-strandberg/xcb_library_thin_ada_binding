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

--  $Revision: 1495 $
--  $Date: 2011-12-04 16:40:36 +0000 (Sun, 04 Dec 2011) $
--  $Author: simonjwright $

with Ada.Finalization;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   pragma Warnings (Off, "=");
package BC.Lists is

   -------------------------------------------------------------------
   --  WARNING: If  you just want a standard  container to support  --
   --  iteration, filtering and sorting, use Collections. The List  --
   --  components  are   much  more  complex   than  you'll  need.  --
   -------------------------------------------------------------------

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
   --  attached head.

   --  The singly-linked and doubly-linked lists have a similar
   --  protocol, except that the doubly-linked list adds two
   --  operations, Predecessor and Is_Head. The space semantics of
   --  these two classes are different (the doubly-linked list has one
   --  extra pointer per item) and additionally, their time semantics
   --  are slightly different, because of the optimizations possible
   --  with having a previous pointer in the doubly-linked list class.

   pragma Preelaborate;

   --  This package specifies the common protocol of all Container
   --  classes. This common protocol consists of Iterators.

   type List_Base is abstract tagged private;

   --  Active iteration

   type Iterator (<>) is abstract tagged private;

   function New_Iterator (For_The_List : List_Base) return Iterator'Class
      is abstract;
   --  Return a reset Iterator bound to the specific List.

   procedure Reset (It : in out Iterator) is abstract;
   --  Reset the Iterator to the beginning.

   procedure Next (It : in out Iterator) is abstract;
   --  Advance the Iterator to the next Item in the List.

   function Is_Done (It : Iterator) return Boolean is abstract;
   --  Return True if there are no more Items in the List.

   function Current_Item (It : Iterator) return Item is abstract;
   --  Return a copy of the current Item.

   generic
      with procedure Apply (Elem : in out Item);
   procedure Access_Current_Item (In_The_Iterator : Iterator'Class);
   --  Call Apply for the Iterator's current Item.

   procedure Delete_Item_At (It : in out Iterator) is abstract;
   --  Remove the current item.

   --  Passive iteration

   generic
      with procedure Apply (Elem : in Item; OK : out Boolean);
   procedure Visit (Using : in out Iterator'Class);
   --  Call Apply with a copy of each Item in the List to which the
   --  iterator Using is bound. The iteration will terminate early if
   --  Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in Item;
                            Param : in Param_Type;
                            OK : out Boolean);
   procedure Visit_With_In_Param (Using : in out Iterator'Class;
                                  Param : in Param_Type);
   --  Call Apply with a Parameter for each Item in the List to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in Item;
                            Param : in out Param_Type;
                            OK : out Boolean);
   procedure Visit_With_In_Out_Param (Using : in out Iterator'Class;
                                      Param : in out Param_Type);
   --  Call Apply with a Parameter for each Item in the List to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   generic
      with procedure Apply (Elem : in out Item; OK : out Boolean);
   procedure Modify (Using : in out Iterator'Class);
   --  Call Apply with a copy of each Item in the List to which the
   --  iterator Using is bound. The iteration will terminate early if
   --  Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in out Item;
                            Param : in Param_Type;
                            OK : out Boolean);
   procedure Modify_With_In_Param (Using : in out Iterator'Class;
                                   Param : in Param_Type);
   --  Call Apply with a Parameter each Item in the List to which the
   --  iterator Using is bound. The iteration will terminate early if
   --  Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in out Item;
                            Param : in out Param_Type;
                            OK : out Boolean);
   procedure Modify_With_In_Out_Param (Using : in out Iterator'Class;
                                       Param : in out Param_Type);
   --  Call Apply with a copy of each Item in the List to which the
   --  iterator Using is bound. The iteration will terminate early if
   --  Apply sets OK to False.

private

   --  We need access to Items; but we must make sure that no actual
   --  allocations occur using this type.

   type Item_Ptr is access all Item;
   for Item_Ptr'Storage_Size use 0;

   type List_Base is abstract new Ada.Finalization.Controlled with null record;

   --  Private primitive operations of List_Base.  These should
   --  ideally be abstract; instead, we provide implementations, but
   --  they raise Should_Have_Been_Overridden.

   function Item_At (C : List_Base; Index : Positive) return Item_Ptr;

   --  Iteration

   type List_Base_Ptr is access all List_Base'Class;
   for List_Base_Ptr'Storage_Size use 0;

   type Iterator is abstract tagged record
      For_The_List : List_Base_Ptr;
   end record;

   --  Private primitive operations of Iterator.  These should ideally
   --  be abstract; instead, we provide implementations, but they
   --  raise Should_Have_Been_Overridden.
   function Current_Item_Ptr (It : Iterator) return Item_Ptr;

end BC.Lists;
