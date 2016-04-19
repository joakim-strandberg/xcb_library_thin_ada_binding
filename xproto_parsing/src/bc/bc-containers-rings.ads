--  Copyright 1994 Grady Booch
--  Copyright 1998-2006 Simon Wright <simon@pushface.org>

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
package BC.Containers.Rings is

   pragma Preelaborate;

   type Abstract_Ring is abstract new Container with private;

   --  A ring denotes a sequence in which items may be added and
   --  removed from the top of a circular structure. Since this
   --  structure has no beginning or ending, a client can mark one
   --  particular item to designate a point of reference in the
   --  structure.

   type Direction is (Forward, Backward);

   function Are_Equal (Left, Right : Abstract_Ring'Class) return Boolean;
   --  Return True if and only if both rings have the same extent and
   --  the same items in the same order; return False otherwise. The
   --  identity of the top and mark of both rings does not participate
   --  in this test of equality.
   --  Can't call this "=" because of the standard one for Ring.

   procedure Copy (From : Abstract_Ring'Class;
                   To : in out Abstract_Ring'Class);
   --  This operation MUST be called for dissimilar Rings in place of
   --  assignment.

   procedure Clear (R : in out Abstract_Ring) is abstract;
   --  Empty the ring of all items. The mark is cleared.

   procedure Insert (R : in out Abstract_Ring; Elem : Item) is abstract;
   --  If the ring was empty, set the ring's mark and top to designate
   --  this item.
   --  Otherwise,
   --    this item becomes the new top;
   --    the previous top is located one place forward of the new top;
   --    the mark remains on the previously marked item.

   procedure Pop (R : in out Abstract_Ring) is abstract;
   --  Remove the top item from the ring.
   --  If the ring is still not empty, the new top is the item that was
   --  previously one place forward from the top.
   --  If the removed item was the marked item, the mark now designates
   --  the new top.

   procedure Rotate (R : in out Abstract_Ring; Dir : Direction := Forward);
   --  Rotate the top of the ring in the given direction. The ring's
   --  mark is unaffected. If there is exactly one item in the ring,
   --  rotating either direction always returns to the same item.

   procedure Mark (R : in out Abstract_Ring);
   --  Designate the item at the top of the ring (if not empty) as
   --  marked.

   procedure Rotate_To_Mark (R : in out Abstract_Ring);
   --  Rotate the ring so that the ring's mark is at the top.

   function Available (R : in Abstract_Ring) return Natural;
   --  Indicates number of empty "Item slots" left in Ring

   function Extent (R : Abstract_Ring) return Natural is abstract;
   --  Return the number of items in the ring.

   function Is_Empty (R : Abstract_Ring) return Boolean is abstract;
   --  Return True if and only if there are no items in the ring.

   function Top (R : Abstract_Ring) return Item is abstract;
   --  Return a copy of the item at the top of the ring.

   function At_Mark (R : Abstract_Ring) return Boolean;
   --  Return True if and only if the item at the top of the ring is
   --  marked; otherwise, return False. This member function will
   --  return True if the ring is empty, since the ring's top and mark
   --  both do not designate any item.

private

   type Abstract_Ring is abstract new Container with record
      Top : Natural := 0;      --  0 implies not set
      Mark : Natural := 0;     --  0 implies not set
   end record;

   procedure Add (R : in out Abstract_Ring; Elem : Item);

   type Ring_Iterator is new Iterator with record
      Index : Natural;
   end record;

   --  Overriding primitive supbrograms of the concrete actual
   --  Iterator.

   procedure Reset (It : in out Ring_Iterator);

   procedure Next (It : in out Ring_Iterator);

   function Is_Done (It : Ring_Iterator) return Boolean;

   function Current_Item (It : Ring_Iterator) return Item;

   function Current_Item_Ptr (It : Ring_Iterator) return Item_Ptr;

   procedure Delete_Item_At (It : in out Ring_Iterator);

end BC.Containers.Rings;
