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
package BC.Containers.Deques is

   pragma Preelaborate;

   type Deque_End is (Front, Back);

   type Abstract_Deque is abstract new Container with private;

   --  A deque denotes a sequence of items, in which items may be
   --  added and removed from either end of the sequence.

   procedure Clear (D : in out Abstract_Deque) is abstract;
   --  Empty the deque of all items.

   procedure Append (D : in out Abstract_Deque;
                     Elem : Item;
                     Location : Deque_End := Back) is abstract;
   --  Add the item to the deque at the given location; the item
   --  itself is copied.

   procedure Pop (D : in out Abstract_Deque;
                  Location : Deque_End := Front) is abstract;
   --  Remove the item from the deque at the given location.

   procedure Remove (D : in out Abstract_Deque; From : Positive) is abstract;
   --  Remove the item at the given index (may be a balking operation).

   function Available (D : in Abstract_Deque) return Natural;
   --  Indicates number of empty "Item slots" left in Deque

   function Length (D : in Abstract_Deque) return Natural is abstract;
   --  Return the number of items in the deque.

   function Is_Empty (D : in Abstract_Deque) return Boolean is abstract;
   --  Return True if and only if there are no items in the deque.

   function Front (D : in Abstract_Deque) return Item is abstract;
   --  Return a copy of the item at the front of the deque.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Front (D : in out Abstract_Deque'Class);
   --  Access the item at the front of the deque.

   function Back (D : in Abstract_Deque) return Item is abstract;
   --  Return a copy of the item at the back of the deque.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Back (D : in out Abstract_Deque'Class);
   --  Access the item at the back of the deque.

   function Location (D : in Abstract_Deque; Elem : in Item) return Natural
      is abstract;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the deque.

   function Are_Equal (Left, Right : Abstract_Deque'Class) return Boolean;
   --  Return True if and only if both deques have the same length and
   --  the same items in the same order; return False otherwise.

   procedure Copy (From : Abstract_Deque'Class;
                   To : in out Abstract_Deque'Class);
   --  This operation MUST be called for dissimilar Deques in place of
   --  assignment.

private

   type Abstract_Deque is abstract new Container with null record;

   type Deque_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Deque_Iterator);

   procedure Next (It : in out Deque_Iterator);

   function Is_Done (It : Deque_Iterator) return Boolean;

   function Current_Item (It : Deque_Iterator) return Item;

   function Current_Item_Ptr (It : Deque_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Deque_Iterator);

end BC.Containers.Deques;
