--  Copyright 1994 Grady Booch
--  Copyright 1998-2003 Simon Wright <simon@pushface.org>

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
package BC.Containers.Collections is

   pragma Preelaborate;

   type Abstract_Collection is abstract new Container with private;

   --  A collection denotes an indexed collection of items, drawn from
   --  some well-defined universe. A collection may contain duplicate
   --  items; a collection owns a copy of each item.

   function Are_Equal (Left, Right : Abstract_Collection'Class) return Boolean;
   --  Return True if and only if both collections have the same
   --  extent and the same items in the same order; return False
   --  otherwise.  Can't call this "=" because of the standard one for
   --  Abstract_Collection.

   procedure Copy (From : Abstract_Collection'Class;
                   To : in out Abstract_Collection'Class);
   --  This operation MUST be called for dissimilar Collections in
   --  place of assignment.

   procedure Clear (C : in out Abstract_Collection) is abstract;
   --  Empty the collection of all items.

   procedure Insert (C : in out Abstract_Collection; Elem : Item) is abstract;
   --  Add the item to the front of the collection.

   procedure Insert (C : in out Abstract_Collection;
                     Elem : Item;
                     Before : Positive) is abstract;
   --  Add the item before the given index item in the collection; if
   --  before is 1, the item is added to the front of the collection.

   procedure Append (C : in out Abstract_Collection; Elem : Item) is abstract;
   --  Add the item at the end of the collection.

   procedure Append (C : in out Abstract_Collection;
                     Elem : Item;
                     After : Positive) is abstract;
   --  Add the item after the given index item in the collection.

   procedure Remove (C : in out Abstract_Collection;
                     At_Index : Positive) is abstract;
   --  Remove the item at the given index in the collection.

   procedure Replace (C : in out Abstract_Collection;
                      At_Index : Positive; Elem : Item) is abstract;
   --  Replace the item at the given index with the given item.

   function Available (C : Abstract_Collection) return Natural;
   --  Indicated number of empty "Item slots" left in the collection.

   function Length (C : Abstract_Collection) return Natural is abstract;
   --  Return the number of items in the collection.

   function Is_Empty (C : Abstract_Collection) return Boolean is abstract;
   --  Return True if and only if there are no items in the
   --  collection.

   function First (C : Abstract_Collection) return Item is abstract;
   --  Return a copy of the item at the front of the collection.

   function Last (C : Abstract_Collection) return Item is abstract;
   --  Return a copy of the item at the end of the collection.

   function Item_At (C : Abstract_Collection;
                     At_Index : Positive) return Item is abstract;
   --  Return a copy of the item at the indicated position in the
   --  collection.

   function Location (C : Abstract_Collection;
                      Elem : Item) return Natural is abstract;
   --  Return the first index at which the item is found (0 if the
   --  item desn't exist in the collecton).

private

   type Abstract_Collection is abstract new Container with null record;

   type Collection_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Collection_Iterator);

   procedure Next (It : in out Collection_Iterator);

   function Is_Done (It : Collection_Iterator) return Boolean;

   function Current_Item (It : Collection_Iterator) return Item;

   function Current_Item_Ptr (It : Collection_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Collection_Iterator);

end BC.Containers.Collections;
