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

with BC.Support.Dynamic;
with System.Storage_Pools;

generic
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Initial_Size : Positive := 10;
package BC.Containers.Collections.Ordered.Dynamic is

   pragma Preelaborate;

   type Collection is new Abstract_Ordered_Collection with private;

   function Null_Container return Collection;

   function "=" (Left, Right : in Collection) return Boolean;

   procedure Clear (C : in out Collection);
   --  Empty the collection of all items.

   procedure Insert (C : in out Collection; Elem : Item);
   --  Add the item to the collection, inserting the new item at the
   --  appropriate position; if an equivalent item is found, the new
   --  item is inserted before it.

   procedure Insert (C : in out Collection;
                     Elem : Item;
                     Before : Positive);
   --  If the indicated item is equivalent to the new item, the new
   --  item is inserted before the indicated item; otherwise, the
   --  behaviour is as above.

   procedure Append (C : in out Collection; Elem : Item);
   --  Add the item to the collection, inserting the new item at the
   --  appropriate position; if any equivalent items are found, the
   --  new item is inserted after all of them.

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive);
   --  If the indicated item is equivalent to the new item, the new
   --  item is inserted after the indicated item; otherwise, the
   --  behaviour is as above.

   procedure Remove (C : in out Collection; At_Index : Positive);
   --  Remove the item at the given index in the collection.

   procedure Replace (C : in out Collection;
                      At_Index : Positive;
                      Elem : Item);
   --  If the indicated item is equivalent to the new item, it is
   --  replaced directly.
   --
   --  If the new item is "<" the indicated item, the indicated item
   --  is removed and the new item is Appended, as above. If the
   --  indicated item is "<" the new item, the indicated item is
   --  removed and the new item is Inserted, as above. The effect is
   --  that the new item moves toward the appropriate end of the
   --  Collection but not beyond any equivalent items.

   function Length (C : Collection) return Natural;
   --  Return the number of items in the collection.

   function Is_Empty (C : Collection) return Boolean;
   --  Return True if and only if there are no items in the
   --  collection.

   function First (C : Collection) return Item;
   --  Return a copy of the item at the front of the collection.

   function Last (C : Collection) return Item;
   --  Return a copy of the item at the end of the collection.

   function Item_At (C : Collection; At_Index : Positive) return Item;
   --  Return a copy of the item at the indicated position in the
   --  collection.

   function Location (C : Collection; Elem : Item) return Natural;
   --  Return the first index at which the item is found (0 if the
   --  item desn't exist in the collecton).

   procedure Preallocate (C : in out Collection; Size : Natural);
   --  Allocates 'Size' additional storage elements for the Collection

   procedure Set_Chunk_Size
     (C : in out Collection; Size : Natural);
   --  Establishes the Size the Collection will grow if the Collection
   --  exhausts its current size.

   function Chunk_Size (C : Collection) return Natural;
   --  Returns the Chunk_Size

   function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Collection.

private

   function Item_At
     (C : Collection; Index : Positive) return Item_Ptr;

   package Collection_Nodes
   is new BC.Support.Dynamic (Item => Item,
                              Item_Ptr => Item_Ptr,
                              Storage => Storage,
                              Initial_Size => Initial_Size);

   type Collection is new Abstract_Ordered_Collection with record
      Rep : Collection_Nodes.Dyn_Node;
   end record;

end BC.Containers.Collections.Ordered.Dynamic;
