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

with BC.Support.AVL_Trees;
with System.Storage_Pools;

generic
   with function "<" (L, R : Item) return Boolean is <>;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Containers.Trees.AVL is

   pragma Preelaborate;

   type AVL_Tree is new BC.Containers.Container with private;

   function "=" (L, R : AVL_Tree) return Boolean;
   --  return True if both trees contain the same Elements.

   function Null_Container return AVL_Tree;

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

   function New_Iterator (For_The_Tree : AVL_Tree) return Iterator'Class;
   --  Return a reset Iterator bound to the specific tree.

   generic
      with procedure Apply (Elem : in out Item);
   procedure Access_Actual_Item (In_The_Tree : AVL_Tree;
                                 Elem : Item;
                                 Found : out Boolean);
   --  If an Item "=" to Elem is present in the Tree, call Apply for
   --  it and set Found to True; otherwise, set Found to False.
   --  Apply MUST NOT alter the result of the ordering operation "<".

   -------------------------------------------------------------------
   --  The  functionality of  Visit and  Modify is  also available  --
   --  using the  standard Container generic. Note  that, as here,  --
   --  the  Apply used  there MUST  NOT  alter the  result of  the  --
   --  ordering operation "<".                                      --
   -------------------------------------------------------------------

   generic
      with procedure Apply (Elem : in Item; OK : out Boolean);
   procedure Visit (Over_The_Tree : AVL_Tree);
   --  Call Apply with a copy of each Item in the Tree, in order. The
   --  iteration will terminate early if Apply sets OK to False.

   generic
      with procedure Apply (Elem : in out Item; OK : out Boolean);
   procedure Modify (Over_The_Tree : AVL_Tree);
   --  Call Apply for each Item in the Tree, in order. The iteration will
   --  terminate early if Apply sets OK to False.
   --  Apply MUST NOT alter the result of the ordering operation "<".

private

   package Support is new BC.Support.AVL_Trees
     (Item => Item,
      "=" => "=",
      "<" => "<",
      Storage => Storage);

   type AVL_Tree is new BC.Containers.Container with record
      Rep : Support.AVL_Tree;
   end record;

   --  Iterator implementations.

   type AVL_Tree_Iterator is new Iterator with record
      Previous, Current : Support.AVL_Node_Ref;
   end record;

   procedure Reset (It : in out AVL_Tree_Iterator);

   procedure Next (It : in out AVL_Tree_Iterator);

   function Is_Done (It : AVL_Tree_Iterator) return Boolean;

   function Current_Item (It : AVL_Tree_Iterator) return Item;

   procedure Delete_Item_At (It : in out AVL_Tree_Iterator);

   function Current_Item_Ptr (It : AVL_Tree_Iterator) return Item_Ptr;

end BC.Containers.Trees.AVL;
