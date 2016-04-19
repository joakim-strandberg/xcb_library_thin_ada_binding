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

with Ada.Streams;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   type Item_Ptr is access all Item;
package BC.Support.Bounded is

   pragma Preelaborate;

   type Bnd_Node (Maximum_Size : Positive) is private;
   --  An optimally-packed static container whose items are stored on
   --  the stack.  Items are indexable starting at 1.  This is a
   --  supporting type.  As such, it is not intended to be used
   --  directly by the end-user.

   procedure Clear (Obj : in out Bnd_Node);
   --  Empty the container of all Items

   function "=" (Left, Right : Bnd_Node) return Boolean;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item);
   --  Add an item to the front of the container

   procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Positive);
   --  Add an item to the container, before the given index

   procedure Append (Obj : in out Bnd_Node; Elem : Item);
   --  Add an item to the end of the container

   procedure Append (Obj : in out Bnd_Node; Elem : Item; After : Positive);
   --  Add an item to the end of the container, after the given index

   procedure Remove (Obj : in out Bnd_Node; From : Positive);
   --  Remove the item at a given index

   procedure Replace (Obj : in out Bnd_Node; Index : Positive; Elem : Item);
   --  Replace the Item at Index with the new Elem

   function Available (Obj : Bnd_Node) return Natural;
   --  Returns available storage elements

   function Length (Obj : Bnd_Node) return Natural;
   --  Returns the number of items in the container

   function First (Obj : Bnd_Node) return Item;
   --  Returns the Item at the front of the container

   function Last (Obj : Bnd_Node) return Item;
   --  Returns the item at the end of the container

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item;
   function Item_At (Obj : Bnd_Node; Index : Positive) return Item_Ptr;
   --  Returns the item at the given index

   function Location (Obj : Bnd_Node;
                      Elem : in Item;
                      Start : in Positive := 1) return Natural;
   --  Returns the first index in which the given item is
   --  found. Returns 0 if unsuccessful.

   pragma Inline (Insert);
   pragma Inline (Append);
   pragma Inline (Remove);
   pragma Inline (Replace);
   pragma Inline (Available);
   pragma Inline (Length);
   pragma Inline (First);
   pragma Inline (Last);
   pragma Inline (Item_At);
   pragma Inline (Location);

private

   subtype Elem_Range is Positive;
   type Elem_Array is array (Elem_Range range <>) of Item;

   subtype Size_Range is Natural;

   type Bnd_Node (Maximum_Size : Positive) is record
      Elems : Elem_Array (1 .. Maximum_Size);
      Start : Elem_Range := 1;
      Size : Size_Range := 0;
   end record;

   procedure Write_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Bnd_Node);

   procedure Read_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Bnd_Node);

   for Bnd_Node'Write use Write_Bnd_Node;
   for Bnd_Node'Read use Read_Bnd_Node;

end BC.Support.Bounded;
