--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2010 Simon Wright <simon@pushface.org>

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

--  $Revision: 1435 $
--  $Date: 2010-02-28 12:09:10 +0000 (Sun, 28 Feb 2010) $
--  $Author: simonjwright $

with BC.Support.Bounded;

generic
   Maximum_Size : Positive;
package BC.Containers.Queues.Bounded is

   pragma Preelaborate;

   type Unconstrained_Queue
     (Maximum_Size : Positive) is new Abstract_Queue with private;

   subtype Queue
      is Unconstrained_Queue (Maximum_Size => Maximum_Size);

   function Null_Container return Unconstrained_Queue;

   procedure Clear (Q : in out Unconstrained_Queue);
   --  Empty the queue of all items.

   procedure Append (Q : in out Unconstrained_Queue; Elem : Item);
   --  Add the item to the back of the queue; the item itself is
   --  copied.

   procedure Pop (Q : in out Unconstrained_Queue);
   --  Remove the item from the front of the queue.

   procedure Remove (Q : in out Unconstrained_Queue; From : Positive);
   --  Remove the item at the given index (may be a balking
   --  operation).

   function Available (Q : in Unconstrained_Queue) return Natural;
   --  Indicates number of empty "Item slots" left in Queue

   function Length (Q : in Unconstrained_Queue) return Natural;
   --  Return the number of items in the queue.

   function Is_Empty (Q : in Unconstrained_Queue) return Boolean;
   --  Return True if and only if there are no items in the queue.

   function Front (Q : in Unconstrained_Queue) return Item;
   --  Return a copy of the item at the front of the queue.

   function Location (Q : in Unconstrained_Queue; Elem : Item) return Natural;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the queue.

   function "=" (Left, Right : in Unconstrained_Queue) return Boolean;
   --  Return True if and only if both queues have the same length and
   --  the same items in the same order; return False otherwise.

   function New_Iterator
     (For_The_Queue : Unconstrained_Queue) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Queue.

private

   function Item_At
     (Q : Unconstrained_Queue; Index : Positive) return Item_Ptr;

   package Queue_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr);

   type Unconstrained_Queue
     (Maximum_Size : Positive)
   is new Abstract_Queue with record
      Rep : Queue_Nodes.Bnd_Node (Maximum_Size => Maximum_Size);
   end record;

end BC.Containers.Queues.Bounded;
