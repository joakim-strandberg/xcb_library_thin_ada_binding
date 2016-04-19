--  Copyright 1994 Grady Booch
--  Copyright 1998-2005 Simon Wright <simon@pushface.org>

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

with BC.Support.Bounded_Hash_Tables;

generic
   with function Hash (K : Key) return Natural is <>;
   Buckets : Positive;
   Maximum_Size : Positive;
package BC.Containers.Maps.Bounded is

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

   type Unconstrained_Map
     (Number_Of_Buckets : Positive;
      Maximum_Size : Positive) is new Abstract_Map with private;

   subtype Map is Unconstrained_Map (Number_Of_Buckets => Buckets,
                                     Maximum_Size => Maximum_Size);

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

   function Available (M : Unconstrained_Map) return Natural;
   --  Return the number of unused slots in the map.

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

private

   package Keys is new BC.Support.Bounded_Hash_Tables.Item_Signature
     (Item => Key,
      Item_Ptr => Key_Ptr,
      Eq => Maps."=",
      Hash => Hash);

   package Items is new BC.Support.Bounded_Hash_Tables.Value_Signature
     (Value => Item,
      Value_Ptr => Item_Ptr,
      Eq => Containers."=");

   package Tables is new BC.Support.Bounded_Hash_Tables.Tables
     (Items => Keys,
      Values => Items);

   type Unconstrained_Map
     (Number_Of_Buckets : Positive;
      Maximum_Size : Positive)
   is new Abstract_Map with record
      Rep : Tables.Table (Number_Of_Buckets => Number_Of_Buckets,
                          Maximum_Size => Maximum_Size);
   end record;

   type Bounded_Map_Iterator is new Map_Iterator with null record;

   procedure Reset (It : in out Bounded_Map_Iterator);

   procedure Next (It : in out Bounded_Map_Iterator);

   function Is_Done (It : Bounded_Map_Iterator) return Boolean;

   function Current_Key (It : Bounded_Map_Iterator) return Key;

   function Current_Item (It : Bounded_Map_Iterator) return Item;

   function Current_Item_Ptr (It : Bounded_Map_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Bounded_Map_Iterator);

end BC.Containers.Maps.Bounded;
