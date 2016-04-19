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
package BC.Containers.Stacks.Bounded is

   pragma Preelaborate;

   type Unconstrained_Stack
     (Maximum_Size : Positive) is new Abstract_Stack with private;

   subtype Stack
      is Unconstrained_Stack (Maximum_Size => Maximum_Size);

   function Null_Container return Unconstrained_Stack;

   procedure Clear (S : in out Unconstrained_Stack);
   --  Empty the Stack of all items.

   procedure Push (S : in out Unconstrained_Stack; Elem : Item);
   --  Add a copy of the item to the top of the Stack.

   procedure Pop (S : in out Unconstrained_Stack);
   --  Remove the item from the top of the Stack.

   function Available (S : in Unconstrained_Stack) return Natural;
   --  Returns a count of the number of empty "Item slots" left.

   function Depth (S : in Unconstrained_Stack) return Natural;
   --  Returns the number of items in the Stack

   function Is_Empty (S : in Unconstrained_Stack) return Boolean;
   --  Returns True if and only if no items are in the stack

   function Top (S : in Unconstrained_Stack) return Item;
   --  Return a copy of the item at the top of the Stack.

   function "=" (Left, Right : in Unconstrained_Stack) return Boolean;
   --  Return True if and only if both stacks have the same depth and
   --  the same items in the same order; return False otherwise.

   function New_Iterator
     (For_The_Stack : Unconstrained_Stack) return Iterator'Class;
   --  Return a reset Iterator bound to the specific Stack.

private

   function Item_At
     (S : Unconstrained_Stack; Index : Positive) return Item_Ptr;

   procedure Add (S : in out Unconstrained_Stack; Elem : Item);
   procedure Remove (S : in out Unconstrained_Stack; From : Positive);

   package Stack_Nodes
   is new BC.Support.Bounded (Item => Item,
                              Item_Ptr => Item_Ptr);
   use Stack_Nodes;

   type Unconstrained_Stack
     (Maximum_Size : Positive)
   is new Abstract_Stack with record
      Rep : Stack_Nodes.Bnd_Node (Maximum_Size => Maximum_Size);
   end record;

end BC.Containers.Stacks.Bounded;
