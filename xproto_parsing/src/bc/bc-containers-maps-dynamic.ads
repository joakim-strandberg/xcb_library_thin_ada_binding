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

--  $Revision: 1462 $
--  $Date: 2011-05-04 14:23:23 +0100 (Wed, 04 May 2011) $
--  $Author: simonjwright $

with BC.Support.Dynamic;
with BC.Support.Hash_Tables;
with System.Storage_Pools;

generic
   with function Hash (K : Key) return Natural is <>;
   Buckets : Positive;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
   Initial_Size : Positive := 10;
package BC.Containers.Maps.Dynamic is

   pragma Preelaborate;

   --  A map denotes a collection forming a dictionary of domain/range
   --  pairs.

   --  The hash function (the generic parameter Hash) determines the
   --  allocation of pairs to hash buckets. The value returned must
   --  not change during the lifetime of a given Item. The range of
   --  hash values need not be constrained to the number of buckets in
   --  the map.

   --  The hash function must satisfy the condition that, for objects
   --  A and B, if A = B, then Hash (A) must equal Hash (B). The hash
   --  function should attempt to spread the set of possible items
   --  uniformly across the number of buckets. The quality of the hash
   --  function has a significant impact upon performance.

   type Map is new Abstract_Map with private;

   function Null_Container return Map;

   function "=" (L, R : Map) return Boolean;
   --  Return True if the two Maps contain the same items bound to the
   --  same values.

   procedure Clear (M : in out Map);
   --  Empty the map of all key/item pairs.

   procedure Bind (M : in out Map; K : Key; I : Item);
   --  If the key already exists in the map, raise
   --  BC.Duplicate. Otherwise, add the key/item pair to the map.

   procedure Rebind (M : in out Map; K : Key; I : Item);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, change the key's binding to the given
   --  value.

   procedure Unbind (M : in out Map; K : Key);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, remove the key/item binding.

   function Extent (M : Map) return Natural;
   --  Return the number of key/item bindings in the map.

   function Is_Empty (M : Map) return Boolean;
   --  Return True if and only if there are no key/item bindings in
   --  the map; otherwise, return False.

   function Is_Bound (M : Map; K : Key) return Boolean;
   --  Return True if and only if there is a binding for the given key
   --  in the map; otherwise, return False.

   function Item_Of (M : Map; K : Key) return Item;
   --  If the key does not exist in the map, raises
   --  BC.Not_Found. Otherwise, return a copy of the item bound to the
   --  given key.

   procedure Preallocate (M : in out Map; Size : Positive);
   --  Allocates 'Size' additional storage elements for each bucket of
   --  the Map

   procedure Set_Chunk_Size (M : in out Map; Size : Positive);
   --  Establishes the Size each bucket of the Map will grow if the
   --  Map exhausts its current size.

   function Chunk_Size (M : Map) return Positive;
   --  Returns the Chunk_Size.

   function New_Iterator (For_The_Map : Map) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Map.

private

   package KC is new BC.Support.Dynamic (Item => Key,
                                         "=" => Maps."=",
                                         Item_Ptr => Key_Ptr,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   package Keys is new BC.Support.Hash_Tables.Item_Signature
     (Item => Key,
      Item_Ptr => Key_Ptr,
      Eq => Maps."=",
      Hash => Hash,
      Item_Container => KC.Dyn_Node,
      Clear => KC.Clear,
      Insert => KC.Insert,
      Append => KC.Append,
      Remove => KC.Remove,
      Replace => KC.Replace,
      Length => KC.Length,
      Item_At => KC.Item_At,
      Access_Item_At => KC.Item_At,
      Location => KC.Location);

   package IC is new BC.Support.Dynamic (Item => Item,
                                         Item_Ptr => Item_Ptr,
                                         Storage => Storage,
                                         Initial_Size => Initial_Size);
   package Items is new BC.Support.Hash_Tables.Value_Signature
     (Value => Item,
      Value_Ptr => Item_Ptr,
      Eq => Containers."=",
      Value_Container => IC.Dyn_Node,
      Clear => IC.Clear,
      Insert => IC.Insert,
      Append => IC.Append,
      Remove => IC.Remove,
      Replace => IC.Replace,
      Length => IC.Length,
      Item_At => IC.Item_At,
      Access_Item_At => IC.Item_At,
      Location => IC.Location);

   package Tables is new BC.Support.Hash_Tables.Tables
     (Items => Keys,
      Values => Items);

   type Map is new Abstract_Map with record
      Rep : Tables.Table (Number_Of_Buckets => Buckets);
   end record;

   --  Iterators

   type Dynamic_Map_Iterator is new Map_Iterator with null record;

   procedure Reset (It : in out Dynamic_Map_Iterator);

   procedure Next (It : in out Dynamic_Map_Iterator);

   function Is_Done (It : Dynamic_Map_Iterator) return Boolean;

   function Current_Key (It : Dynamic_Map_Iterator) return Key;

   function Current_Item (It : Dynamic_Map_Iterator) return Item;

   function Current_Item_Ptr (It : Dynamic_Map_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Dynamic_Map_Iterator);

end BC.Containers.Maps.Dynamic;
