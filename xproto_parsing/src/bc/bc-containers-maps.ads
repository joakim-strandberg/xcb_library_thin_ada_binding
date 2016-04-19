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
   type Key is private;
   with function "=" (L, R : Key) return Boolean is <>;
package BC.Containers.Maps is

   pragma Preelaborate;

   --  A map denotes a collection forming a dictionary of domain/range
   --  pairs.

   --  The parameter Key denotes the universe from which the map draws
   --  its domain; the parameter Item denotes the universe from which
   --  the map draws its range. The parameters Key and Item typically
   --  represent different types, although they may may represent the
   --  same type. Either may be a primitive type or user-defined.

   type Abstract_Map is abstract new Container with private;

   function Are_Equal (L, R : Abstract_Map'Class) return Boolean;
   --  Return True if and only if both maps have the same extent and
   --  the same key/item pairs; return False otherwise.
   --  Can't call this "=" because of the standard one for Map.

   procedure Clear (M : in out Abstract_Map)
      is abstract;
   --  Empty the map of all key/item pairs.

   procedure Bind (M : in out Abstract_Map; K : Key; I : Item)
      is abstract;
   --  If the key already exists in the map, raise BC.Duplicate.
   --  Otherwise, add the key/item pair to the map.

   procedure Rebind (M : in out Abstract_Map; K : Key; I : Item)
      is abstract;
   --  If the key does not exist in the map, raise BC.Not_Found.
   --  Otherwise, change the key's binding to the given value.

   procedure Unbind (M : in out Abstract_Map; K : Key)
      is abstract;
   --  If the key does not exist in the map, raise BC.Not_Found.
   --  Otherwise, remove the key/item binding.

   function Available (M : Abstract_Map) return Natural;
   --  Return the number of unused slots in the map.

   function Extent (M : Abstract_Map) return Natural
      is abstract;
   --  Return the number of key/item bindings in the map.

   function Is_Empty (M : Abstract_Map) return Boolean
      is abstract;
   --  Return True if and only if there are no key/item bindings in
   --  the map; otherwise, return False.

   function Is_Bound (M : Abstract_Map; K : Key) return Boolean
      is abstract;
   --  Return True if and only if there is a binding for the given key
   --  in the map; otherwise, return False.

   function Item_Of (M : Abstract_Map; K : Key) return Item
      is abstract;
   --  If the key does not exist in the map, raises BC.Not_Found.
   --  Otherwise, returns a copy of the item bound to the given key.

   --  Additional Iterator support

   type Map_Iterator is abstract new Iterator with private;

   function Current_Key (It : Map_Iterator) return Key is abstract;
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

   type Abstract_Map is abstract new Container with null record;

   type Key_Ptr is access all Key;
   for Key_Ptr'Storage_Size use 0;

   --  The new subprograms for Map iteration (which allow access to
   --  the Key as well as the Item) require the inherited
   --  For_The_Container to in fact be in Map'Class. This must be the
   --  case since the only way of getting a Map_Iterator is by using
   --  one of the concrete forms' New_Iterator using eg
   --
   --   Iter : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (M));
   --
   --  which fails at compilation time if M isn't actually a Map.
   type Map_Iterator is abstract new Iterator with record
      Bucket_Index : Natural := 0;
      Index : Natural := 0;
   end record;

end BC.Containers.Maps;
