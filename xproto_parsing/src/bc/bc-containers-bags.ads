--  Copyright 1994 Grady Booch
--  Copyright 1998-2002 Simon Wright <simon@pushface.org>

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
package BC.Containers.Bags is

   pragma Preelaborate;

   --  A bag denotes a collection of items, drawn from some
   --  well-defined universe. A bag may contain duplicate items. A bag
   --  actually owns only one copy of each unique item: duplicates are
   --  counted, but are not stored with the bag.

   --  The parameter Item denotes the universe from which the bag
   --  draws its items. Items may be a primitive type or user-defined.

   type Abstract_Bag is abstract new Container with private;

   function Are_Equal (L, R : Abstract_Bag'Class) return Boolean;
   --  Return True if and only if both bags have the same number of
   --  distinct items, and the same items themselves, each with the
   --  same count; return False otherwise.
   --  Can't call this "=" because of the standard one for Bag.

   procedure Clear (B : in out Abstract_Bag) is abstract;
   --  Empty the bag of all items.

   procedure Add
     (B : in out Abstract_Bag; I : Item; Added : out Boolean) is abstract;
   --  Add the item to the bag. If the item is not already a distinct
   --  member of the bag, copy the item and add it to the bag and set
   --  Added to True. If the item already exists, then increment the
   --  number of that item and set Added to False.

   procedure Add (B : in out Abstract_Bag'Class; I : Item);
   --  Add the item to the bag. If the item is not already a distinct
   --  member of the bag, copy the item and add it to the bag; if it
   --  is, increment the number of that item.

   procedure Remove (B : in out Abstract_Bag; I : Item) is abstract;
   --  If the item is not a member of the bag, raise
   --  BC.Not_Found. Otherwise, if there is exactly one of the item in
   --  the bag, remove the item in the bag; if there is more than one
   --  of the item in the bag, simply decrement its number.

   procedure Union (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class);
   --  Perform a logical bag union; at the completion of this
   --  operation, the bag B contains the items and counts found in its
   --  original state combined with the bag O. For each item in the
   --  bag O, if the item is not already a distinct member of the bag
   --  B, copy the item and add it and its count to the bag B. If the
   --  item already is a member, increment its count in B.

   procedure Intersection (B : in out Abstract_Bag'Class;
                           O : Abstract_Bag'Class);
   --  Perform a logical bag intersection; at the completion of this
   --  operation, the bag B contains the items found both in its
   --  original state and in the bag O. For each item in the bag O, if
   --  the item is not already a distinct member of the bag B, do
   --  nothing. If the item already is a member of B, set its count to
   --  the lower of the two counts. Items in the bag B but not in the
   --  bag O are also removed.

   procedure Difference (B : in out Abstract_Bag'Class;
                         O : Abstract_Bag'Class);
   --  Perform a logical bag difference; at the completion of this
   --  operation, the bag B contains the items found in its original
   --  state, less those found in the bag O. For each item in the bag
   --  O, if the item is not already a distinct member of the bag B,
   --  do nothing. If the item is a member of the bag B with a count
   --  less than that in the bag O, remove the item from the bag B. If
   --  the item is a member of the bag B with a count more than that
   --  in the bag O, decrement the count in the bag B by the count in
   --  the bag O.

   function Available (B : Abstract_Bag) return Natural;
   --  Return the number of unused slots in the bag.

   function Extent (B : Abstract_Bag) return Natural is abstract;
   --  Return the number of distinct items in the bag.

   function Total_Size (B : Abstract_Bag'Class) return Natural;
   --  Return the total number of items in the bag.

   function Count (B : Abstract_Bag; I : Item) return Natural is abstract;
   --  Return the number of times the item occurs in the bag.

   function Is_Empty (B : Abstract_Bag) return Boolean is abstract;
   --  Return True if and only if there are no items in the bag.

   function Is_Member (B : Abstract_Bag; I : Item) return Boolean is abstract;
   --  Return True if and only if the item exists in the bag.

   function Is_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean;
   --  Return True if and only if the bag B has the same or fewer
   --  distinct items than in the bag O and equal or less numbers of
   --  each such item than in the bag O.

   function Is_Proper_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean;
   --  Return True if and only if all the distinct items in the bag B
   --  are also in the bag O, and either at least one of the items in
   --  the bag B has a lower number than the number in the bag O, or
   --  there is at least one distinct item in the bag O that is not in
   --  the bag B.

private

   type Abstract_Bag is abstract new Container with null record;

   --  These subprograms are used in the implementation of the Bag
   --  operations above. They would be abstract but that the ALRM
   --  forbids that for private operations.
   --
   --  These implementatiosm raise Should_Have_Been_Overridden.

   procedure Attach (B : in out Abstract_Bag; I : Item; C : Positive);

   procedure Detach (B : in out Abstract_Bag; I : Item);

   procedure Set_Value (B : in out Abstract_Bag; I : Item; C : Positive);

   type Bag_Iterator is abstract new Iterator with record
      Bucket_Index : Natural := 0;
      Index : Natural := 0;
   end record;

end BC.Containers.Bags;
