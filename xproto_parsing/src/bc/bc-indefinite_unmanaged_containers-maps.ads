--  Copyright 1994 Grady Booch
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>

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

with BC.Support.Indefinite_Unmanaged;
with BC.Support.Indefinite_Hash_Tables;

generic
   type Key (<>) is private;
   with function "=" (L, R : Key) return Boolean is <>;
   with function Hash (K : Key) return Natural is <>;
   Buckets : Positive;
package BC.Indefinite_Unmanaged_Containers.Maps is

   pragma Preelaborate;

   --  A map denotes a collection forming a dictionary of domain/range
   --  pairs.

   --  The parameter Key denotes the universe from which the map draws
   --  its domain; the parameter Item denotes the universe from which
   --  the map draws its range. The parameters Key and Item typically
   --  represent different types, although they may may represent the
   --  same type. Either may be a primitive type or user-defined.

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

   type Unconstrained_Map
     (Number_Of_Buckets : Positive) is new Container with private;

   subtype Map is Unconstrained_Map (Number_Of_Buckets => Buckets);

   function Null_Container return Unconstrained_Map;
   --  Note, this function has to be provided but the object returned
   --  is in fact a Map (ie, it is constrained).

   function "=" (L, R : Unconstrained_Map) return Boolean;
   --  Return True if the two Maps contain the same items bound to the
   --  same values.

   procedure Clear (M : in out Unconstrained_Map);
   --  Empty the map of all key/item pairs.

   procedure Bind (M : in out Unconstrained_Map; K : Key; I : Item);
   --  If the key already exists in the map, raise
   --  BC.Duplicate. Otherwise, add the key/item pair to the map.

   procedure Rebind (M : in out Unconstrained_Map; K : Key; I : Item);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, change the key's binding to the given
   --  value.

   procedure Unbind (M : in out Unconstrained_Map; K : Key);
   --  If the key does not exist in the map, raise
   --  BC.Not_Found. Otherwise, remove the key/item binding.

   function Extent (M : Unconstrained_Map) return Natural;
   --  Return the number of key/item bindings in the map.

   function Is_Empty (M : Unconstrained_Map) return Boolean;
   --  Return True if and only if there are no key/item bindings in
   --  the map; otherwise, return False.

   function Is_Bound (M : Unconstrained_Map; K : Key) return Boolean;
   --  Return True if and only if there is a binding for the given key
   --  in the map; otherwise, return False.

   function Item_Of (M : Unconstrained_Map; K : Key) return Item;
   --  If the key does not exist in the map, raises
   --  BC.Not_Found. Otherwise, return a copy of the item bound to the
   --  given key.

   function New_Iterator
     (For_The_Map : Unconstrained_Map) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Map.

   --  Additional Iterator support

   type Map_Iterator is new Iterator with private;

   function Current_Key (It : Map_Iterator) return Key;
   --  Returns a copy of the current Key.

   generic
      with procedure Apply (K : Key; I : Item; OK : out Boolean);
   procedure Visit (Using : in out Map_Iterator'Class);
   --  Call Apply with a copy of each Key/Item pair in the Container
   --  to which the iterator Using is bound. The iteration will
   --  terminate early if Apply sets OK to False.

   generic
      with procedure Apply (K : Key; I : in out Item; OK : out Boolean);
   procedure Modify (Using : in out Map_Iterator'Class);
   --  Call Apply for each Key/Item pair in the Container to which the
   --  iterator Using is bound. The Item is a copy, the Item is the
   --  actual content. The iteration will terminate early if Apply
   --  sets OK to False.

private

   --  Suppress "unreferenced" warnings here (GNAT 5.02). Can't use
   --  pragma Unreferenced, because then we get warnings in child
   --  packages.
   pragma Warnings (Off, "=");

   type Key_Ptr is access Key;

   package KC is new BC.Support.Indefinite_Unmanaged
     (Item => Key,
      "=" => Maps."=",
      Item_Ptr => Key_Ptr);
   package Keys is new BC.Support.Indefinite_Hash_Tables.Item_Signature
     (Item => Key,
      Item_Ptr => Key_Ptr,
      Hash => Hash,
      Item_Container => KC.Unm_Node,
      Clear => KC.Clear,
      Insert => KC.Insert,
      Append => KC.Append,
      Remove => KC.Remove,
      Replace => KC.Replace,
      Length => KC.Length,
      Item_At => KC.Item_At,
      Access_Item_At => KC.Item_At,
      Location => KC.Location);

   package IC is new BC.Support.Indefinite_Unmanaged
     (Item => Item,
      "=" => Indefinite_Unmanaged_Containers."=",
      Item_Ptr => Item_Ptr);
   package Items is new BC.Support.Indefinite_Hash_Tables.Value_Signature
     (Value => Item,
      Value_Ptr => Item_Ptr,
      Eq => Indefinite_Unmanaged_Containers."=",
      Value_Container => IC.Unm_Node,
      Clear => IC.Clear,
      Insert => IC.Insert,
      Append => IC.Append,
      Remove => IC.Remove,
      Replace => IC.Replace,
      Length => IC.Length,
      Item_At => IC.Item_At,
      Access_Item_At => IC.Item_At,
      Location => IC.Location);

   package Tables is new BC.Support.Indefinite_Hash_Tables.Tables
     (Items => Keys,
      Values => Items);

   type Unconstrained_Map
     (Number_Of_Buckets : Positive)
   is new Container with record
      Rep : Tables.Table (Number_Of_Buckets => Number_Of_Buckets);
   end record;

   --  Iterators

--     --  The new subprograms for Map iteration (which allow access to
--     --  the Key as well as the Item) require the inherited
--     --  For_The_Container to in fact be in Map'Class. This must be the
--     --  case since the only way of getting a Map_Iterator is by using
--     --  one of the concrete forms' New_Iterator using eg
--     --
--     --   Iter : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (M));
--     --
--     --  which fails at compilation time if M isn't actually a Map.
   type Map_Iterator is new Iterator with record
      Bucket_Index : Natural := 0;
      Index : Natural := 0;
   end record;

   procedure Reset (It : in out Map_Iterator);

   procedure Next (It : in out Map_Iterator);

   function Is_Done (It : Map_Iterator) return Boolean;

   function Current_Item (It : Map_Iterator) return Item;

   function Current_Item_Ptr (It : Map_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Map_Iterator);

end BC.Indefinite_Unmanaged_Containers.Maps;
