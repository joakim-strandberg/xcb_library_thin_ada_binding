--  Copyright 1994 Grady Booch
--  Copyright 2003 Simon Wright <simon@pushface.org>

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

with BC.Support.Unmanaged;

generic
package BC.Containers.Deques.Unmanaged is

   pragma Preelaborate;

   type Deque is new Abstract_Deque with private;
   --  This Deque exhibits unlimited growth and collapsing, limited
   --  only by available memory.  Assignment is "deep".

   function Null_Container return Deque;

   procedure Clear (D : in out Deque);
   --  Empty the deque of all items.

   procedure Append (D : in out Deque;
                     Elem : Item;
                     Location : Deque_End := Back);
   --  Add the item to the deque at the given location; the item
   --  itself is copied.

   procedure Pop (D : in out Deque; Location : Deque_End := Front);
   --  Remove the item from the deque at the given location.

   procedure Remove (D : in out Deque; From : Positive);
   --  Remove the item at the given index.

   function Length (D : in Deque) return Natural;
   --  Return the number of items in the deque.

   function Is_Empty (D : in Deque) return Boolean;
   --  Return True if and only if there are no items in the deque.

   function Front (D : in Deque) return Item;
   --  Return a copy of the item at the front of the deque.

   function Back (D : in Deque) return Item;
   --  Return a copy of the item at the back of the deque.

   function Location (D : in Deque; Elem : Item) return Natural;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the deque.

   function "=" (Left, Right : in Deque) return Boolean;
   --  Return True if and only if both deques have the same length and
   --  the same items in the same order; return False otherwise.

   function New_Iterator (For_The_Deque : Deque) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Deque.

private

   package Deque_Nodes
   is new BC.Support.Unmanaged (Item => Item,
                                Item_Ptr => Item_Ptr);

   type Deque is new Abstract_Deque with record
      Rep : Deque_Nodes.Unm_Node;
   end record;

   function Item_At (D : Deque; Index : Positive) return Item_Ptr;

end BC.Containers.Deques.Unmanaged;
