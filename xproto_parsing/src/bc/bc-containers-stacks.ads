--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
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
package BC.Containers.Stacks is

   pragma Preelaborate;

   type Abstract_Stack is abstract new Container with private;

   --  A sequence in which items may be added and removed from one end
   --  only.  This class is abstract and serves only to enforce the
   --  interfaces among classes.

   --  Operations of equality, inequality, and assignment are "deep"
   --  for all Stack forms

   procedure Clear (S : in out Abstract_Stack) is abstract;
   --  Empty the Stack of all items.

   procedure Push (S : in out Abstract_Stack; Elem : Item) is abstract;
   --  Add a copy of the item to the top of the Stack.

   procedure Pop (S : in out Abstract_Stack) is abstract;
   --  Remove the item from the top of the Stack.

   function Available (S : in Abstract_Stack) return Natural;
   --  Returns a count of the number of empty "Item slots" left.

   function Depth (S : in Abstract_Stack) return Natural is abstract;
   --  Returns the number of items in the Stack

   function Is_Empty (S : in Abstract_Stack) return Boolean is abstract;
   --  Returns True if and only if no items are in the stack

   function Top (S : in Abstract_Stack) return Item is abstract;
   --  Return a copy of the item at the top of the Stack.

   generic
      with procedure Process (Elem : in out Item);
   procedure Process_Top (S : in out Abstract_Stack'Class);
   --  Access the item at the top of the Stack.

   function Are_Equal (Left, Right : Abstract_Stack'Class) return Boolean;
   --  Return True if and only if both stacks have the same depth and
   --  the same items in the same order; return False otherwise.

   procedure Copy (From : Abstract_Stack'Class;
                   To : in out Abstract_Stack'Class);
   --  This operation MUST be called for dissimilar Stacks in place of
   --  assignment.

private

   type Abstract_Stack is abstract new Container with null record;

   procedure Add (S : in out Abstract_Stack; Elem : Item);
   procedure Remove (S : in out Abstract_Stack; From : Positive);

   type Stack_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual Iterator.

   procedure Reset (It : in out Stack_Iterator);

   procedure Next (It : in out Stack_Iterator);

   function Is_Done (It : Stack_Iterator) return Boolean;

   function Current_Item (It : Stack_Iterator) return Item;

   function Current_Item_Ptr (It : Stack_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Stack_Iterator);

end BC.Containers.Stacks;
