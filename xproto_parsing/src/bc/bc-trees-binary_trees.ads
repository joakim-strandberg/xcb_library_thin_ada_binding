--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
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

with Ada.Finalization;
with System.Storage_Pools;

generic
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Trees.Binary_Trees is

   pragma Preelaborate;

   type Binary_Tree is private;

   type Child_Branch is (Left, Right);

   function Create (From : Binary_Tree) return Binary_Tree;
   --  If the given tree is null; construct a null tree. Otherwise,
   --  construct a tree that structurally shares the root of the given
   --  tree.

   function "=" (Left, Right : Binary_Tree) return Boolean;
   --  Return True if and only if both trees are null or structurally
   --  share the same tree.

   procedure Clear (T : in out Binary_Tree);
   --  If the tree is not null, destroy this alias to the tree, make
   --  the tree null, and reclaim the storage associated with any
   --  unreachable items.

   procedure Insert (T : in out Binary_Tree;
                     Elem : in Item;
                     Child : in Child_Branch);
   --  Add the item to the root of the tree, and make the original
   --  root the given child of this new tree.

   procedure Append (T : in out Binary_Tree;
                     Elem : in Item;
                     Child : in Child_Branch;
                     After : in Child_Branch);
   --  Add the item as the given child of the tree, and take the
   --  original child and add it as the child after this new item.

   procedure Remove (T : in out Binary_Tree; Child : in Child_Branch);
   --  Remove the given child and destroy it if it is no longer
   --  reachable.

   procedure Share (T : in out Binary_Tree;
                    Share_With : in Binary_Tree;
                    Child : in Child_Branch);
   --  Clear the tree, then, if the given tree is not null, set the
   --  tree to structurally share with the given child of the tree.

   procedure Swap_Child (T : in out Binary_Tree;
                         Swap_With : in out Binary_Tree;
                         Child : in Child_Branch);
   --  The given tree must represent the root of a tree, which may be
   --  null. Set the child of the tree (which may be null) to denote
   --  the given tree (which may be null), and set the given tree to
   --  the original child of the tree. If it is not null, the parent
   --  of the new child of the tree is set to be the root of the tree.
   --  If it is not null, the parent of the new root of the given tree
   --  is set to be null.

   procedure Child (T : in out Binary_Tree; Child : in Child_Branch);
   --  The tree must not be null. Set the tree to now denote the given
   --  child (which may be null) and reclaim the storage associated
   --  with any unreachable items.

   procedure Left_Child (T : in out Binary_Tree);
   --  The tree must not be null. Set the tree to now denote the left
   --  child (which may be null) and reclaim the storage associated
   --  with any unreachable items.

   function Left_Child (T : Binary_Tree) return Binary_Tree;
   --  The tree must not be null. Return the left child (which may be
   --  null).

   procedure Right_Child (T : in out Binary_Tree);
   --  The tree must not be null. Set the tree to now denote the right
   --  child (which may be null) and reclaim the storage associated
   --  with any unreachable items.

   function Right_Child (T : Binary_Tree) return Binary_Tree;
   --  The tree must not be null. Return the right child (which may be
   --  null).

   procedure Parent (T : in out Binary_Tree);
   --  Set the tree to now denote its parent (if any).

   procedure Set_Item (T : in out Binary_Tree; Elem : in Item);
   --  Set the item at the root of the tree.

   function Has_Children (T : in Binary_Tree) return Boolean;
   --  Return True if and only if the tree has any non-null children.

   function Is_Null (T : in Binary_Tree) return Boolean;
   --  Return True if and only if the tree has no items.

   function Is_Shared (T : in Binary_Tree) return Boolean;
   --  Return True if and only if the tree has an alias.

   function Is_Root (T : in Binary_Tree) return Boolean;
   --  Return True if and only if the tree is at the root of a tree.

   function Item_At (T : in Binary_Tree) return Item;
   --  Return the item at the root of the tree.

private

   --  Type denoting a simple node consisting of an item, a pointer to
   --  the parent, pointers to the left and right items, and a
   --  reference count

   type Binary_Node;
   type Binary_Node_Ref is access Binary_Node;
   for Binary_Node_Ref'Storage_Pool use Storage;

   type Binary_Node is record
      Element : Item;
      Parent, Left, Right : Binary_Node_Ref;
      Count : Natural := 1;
   end record;

   procedure Purge (Node : in out Binary_Node_Ref);

   type Binary_Tree is new Ada.Finalization.Controlled with record
      Rep : Binary_Node_Ref;
   end record;

   procedure Initialize (T : in out Binary_Tree);
   procedure Adjust (T : in out Binary_Tree);
   procedure Finalize (T : in out Binary_Tree);

end BC.Trees.Binary_Trees;
