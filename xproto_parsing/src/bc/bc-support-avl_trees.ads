--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2007 Simon Wright <simon@pushface.org>

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

with Ada.Finalization;
with System.Storage_Pools;

generic
   type Item is private;
   with function "=" (L, R : Item) return Boolean is <>;
   with function "<" (L, R : Item) return Boolean is <>;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Support.AVL_Trees is

   pragma Preelaborate;

   type AVL_Node;
   type AVL_Node_Ref is access AVL_Node;
   for AVL_Node_Ref'Storage_Pool use Storage;

   type Node_Balance is (Left, Middle, Right);

   type AVL_Node is record
      Element : Item;
      Left, Right : AVL_Node_Ref;
      Balance : Node_Balance := Middle;
   end record;

   type AVL_Tree is new Ada.Finalization.Controlled with record
      Rep : AVL_Node_Ref;
      Size : Natural := 0;
   end record;

   function "=" (L, R : AVL_Tree) return Boolean;
   --  return True if both trees contain the same Elements.

   procedure Clear (T : in out AVL_Tree);
   --  Make the tree null and reclaim the storage associated with its items.

   procedure Insert (T : in out AVL_Tree;
                     Element : Item;
                     Not_Found : out Boolean);
   --  Add the item to the tree, preserving the tree's
   --  balance. Not_Found is set to True if the item had not
   --  previously existed in the tree, and to False otherwise.

   procedure Delete
     (T : in out AVL_Tree; Element : Item; Found : out Boolean);
   --  Remove the item from the tree, preserving the tree's
   --  balance. Found is set to True if the item was in fact found in
   --  the tree and removed, and to False otherwise.

   function Extent (T : AVL_Tree) return Natural;
   --  Return the number of items in the tree.

   function Is_Null (T : AVL_Tree) return Boolean;
   --  Return True if and only if the tree has no items.

   function Is_Member (T : AVL_Tree; Element : Item) return Boolean;
   --  Return True if and only if the item exists in the tree.

   procedure Adjust (T : in out AVL_Tree);

   procedure Finalize (T : in out AVL_Tree);

end BC.Support.AVL_Trees;
