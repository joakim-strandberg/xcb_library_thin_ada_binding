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

with Ada.Finalization;
with System.Storage_Pools;

generic
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Containers.Lists.Double is

   -------------------------------------------------------------------
   --  WARNING: If  you just want a standard  container to support  --
   --  iteration, filtering and sorting, use Collections. The List  --
   --  components are much more complex than you'll need.           --
   -------------------------------------------------------------------

   pragma Preelaborate;

   --  Doubly-linked list

   type List is new Container with private;

   function Null_Container return List;

   function "=" (L, R : List) return Boolean;
   --  Return True if and only if both lists are null or structurally
   --  share the same list.

   procedure Clear (L : in out List);
   --  If the list is not null, destroy this alias to the list, make
   --  the list null, and reclaim the storage associated with any
   --  unreachable items.

   procedure Insert (L : in out List; Elem : Item);
   --  Add the item to the head of the list.

   procedure Insert (L : in out List; From_List : in out List);
   --  Add From_List to the head of the list.

   procedure Insert (L : in out List; Elem : Item; Before : Positive);
   --  Add the item before the given index item in the list; if Before
   --  is 1, the item is added to the head of the list.

   procedure Insert (L : in out List;
                     From_List : in out List;
                     Before : Positive);
   --  Add the list before the given index item in the list; if Before
   --  is 1, the list is added to the head of the list.

   procedure Append (L : in out List; Elem : Item);
   --  Add the item at the end of the list.

   procedure Append (L : in out List; From_List : in out List);
   --  Add From_List at the end of the list.

   procedure Append (L : in out List; Elem : Item; After : Positive);
   --  Add the item after the given index item in the list.

   procedure Append (L : in out List;
                     From_List : in out List;
                     After : Positive);
   --  Add From_List after the given index item in the list.

   procedure Remove (L : in out List; From : Positive);
   --  Remove the item at the given index in the list.

   procedure Purge (L : in out List; From : Positive);
   --  Remove all the items in the list starting at the given index,
   --  inclusive.

   procedure Purge (L : in out List; From : Positive; Count : Positive);
   --  Remove all the items in the list starting at the given index,
   --  inclusive, for a total of count items.

   procedure Preserve (L : in out List; From : Positive);
   --  Remove all the items in the list except those starting at the
   --  given index, inclusive.

   procedure Preserve (L : in out List; From : Positive; Count : Positive);
   --  Remove all the items in the list except those starting at the
   --  given index, inclusive, for a total of count items.

   procedure Share (L : in out List; With_List : List; Starting_At : Positive);
   --  Clear L, then, if With_List is not null, set L to structurally
   --  share with the head of With_List, starting at the given index.

   procedure Share_Head (L : in out List; With_List : in List);
   --  Clear L, then, if With_List is not null, set L to structurally
   --  share with the head of With_List.

   procedure Share_Foot (L : in out List; With_List : in List);
   --  Clear L, then, if With_List is not null, set L to structurally
   --  share with the end of With_List.

   procedure Swap_Tail (L : in out List; With_List : in out List);
   --  With_List must represent the head of a list, which may be
   --  null. Set the tail of L (which must not be null) to denote
   --  With_List (which may be null), and set With_List to the
   --  original tail of L. If it is not null, the predecessor of the
   --  new tail of L is set to be the head of L. If it is not null,
   --  the predecessor of the new head of With_List is set to be null.

   procedure Tail (L : in out List);
   --  The list must not be null. Set the list to now denote its tail
   --  (which may be null), and reclaim the storage associated with
   --  any unreachable items.

   procedure Predecessor (L : in out List);
   --  Set the list to now denote its predecessor (if any)

   procedure Set_Head (L : in out List; Elem : Item);
   --  Set the item at the head of the list.

   procedure Set_Item (L : in out List; Elem : Item; At_Loc : Positive);
   --  Set the item at the given index.

   function Length (L : List) return Natural;
   --  Return the number of items in the list.

   function Is_Null (L : List) return Boolean;
   --  Return True if and only there are no items in the list.

   function Is_Shared (L : List) return Boolean;
   --  Return True if and only if the list has an alias.

   function Is_Head (L : List) return Boolean;
   --  Return True if and only if the list is at the head.

   function Head (L : List) return Item;
   --  Return a copy of the item at the head of the list.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Head (L : in out List);
   --  Access the item at the head of the list.

   function Foot (L : List) return Item;
   --  Return a copy of the item at the end of the list.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Foot (L : in out List);
   --  Access the item at the end of the list.

   function Item_At (L : List; Index : Positive) return Item;
   --  Return a copy of the item at the given index.

   function New_Iterator (For_The_List : List) return Iterator'Class;
   --  Return a reset Iterator bound to the specific List.

private

   function Item_At (L : List; Index : Positive) return Item_Ptr;

   --  Type denoting a simple node consisting of an item, pointers to
   --  the previous and next items, and a reference count

   type Double_Node;
   type Double_Node_Ref is access Double_Node;
   for Double_Node_Ref'Storage_Pool use Storage;
   type Double_Node is record
      Element : Item;
      Previous : Double_Node_Ref;
      Next : Double_Node_Ref;
      Count : Natural := 1;
   end record;

   type Header is new Ada.Finalization.Controlled with record
      Rep : Double_Node_Ref;
   end record;

   procedure Adjust (H : in out Header);
   procedure Finalize (H : in out Header);

   type List is new Container with record
      Head : Header;
   end record;

   type List_Iterator is new Iterator with record
      Index : Double_Node_Ref;
   end record;

   --  Overriding primitive supbrograms of the concrete actual
   --  Iterator.

   procedure Reset (It : in out List_Iterator);

   procedure Next (It : in out List_Iterator);

   function Is_Done (It : List_Iterator) return Boolean;

   function Current_Item (It : List_Iterator) return Item;

   function Current_Item_Ptr (It : List_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out List_Iterator);

end BC.Containers.Lists.Double;
