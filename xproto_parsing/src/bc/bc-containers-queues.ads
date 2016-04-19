--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
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

--  $Revision: 1490 $
--  $Date: 2011-09-01 17:23:20 +0100 (Thu, 01 Sep 2011) $
--  $Author: simonjwright $

generic
package BC.Containers.Queues is

   pragma Preelaborate;

   type Abstract_Queue is abstract new Container with private;

   --  A queue denotes a sequence of items, in which items may be
   --  added from one end and removed from the opposite end of the
   --  sequence.

   procedure Clear (Q : in out Abstract_Queue) is abstract;
   --  Empty the queue of all items.

   procedure Append (Q : in out Abstract_Queue; Elem : Item) is abstract;
   --  Add the item to the back of the queue; the item itself is
   --  copied.

   procedure Pop (Q : in out Abstract_Queue) is abstract;
   --  Remove the item from the front of the queue.

   procedure Pop_Value (Q : in out Abstract_Queue; Elem : out Item);
   --  Remove and return the item from the front of the queue.

   procedure Remove (Q : in out Abstract_Queue; From : Positive) is abstract;
   --  Remove the item at the given index.

   function Available (Q : in Abstract_Queue) return Natural;
   --  Indicates number of empty "Item slots" left in Queue

   function Length (Q : in Abstract_Queue) return Natural is abstract;
   --  Return the number of items in the queue.

   function Is_Empty (Q : in Abstract_Queue) return Boolean is abstract;
   --  Return True if and only if there are no items in the queue.

   function Front (Q : in Abstract_Queue) return Item is abstract;
   --  Return a copy of the item at the front of the queue.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Front (Q : in out Abstract_Queue'Class);
   --  Allows modification of the item at the front of the queue.

   function Location (Q : in Abstract_Queue; Elem : in Item) return Natural
      is abstract;
   --  Return the first index at which the item is found; return 0 if
   --  the item does not exist in the queue.

   function Are_Equal (Left, Right : Abstract_Queue'Class) return Boolean;
   --  Return True if and only if both queues have the same length and
   --  the same items in the same order; return False otherwise.

   procedure Copy (From : Abstract_Queue'Class;
                   To : in out Abstract_Queue'Class);
   --  This operation MUST be called for dissimilar Queues in place of
   --  assignment.

private

   type Abstract_Queue is abstract new Container with null record;

   type Queue_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Queue_Iterator);

   procedure Next (It : in out Queue_Iterator);

   function Is_Done (It : Queue_Iterator) return Boolean;

   function Current_Item (It : Queue_Iterator) return Item;

   function Current_Item_Ptr (It : Queue_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Queue_Iterator);

end BC.Containers.Queues;
