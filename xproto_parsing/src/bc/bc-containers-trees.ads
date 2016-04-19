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

generic package BC.Containers.Trees is

   pragma Preelaborate;

   --  A binary tree is a rooted collection of nodes and arcs, where
   --  each node has two children and where arcs may not have cycles
   --  or cross-references (as in graphs). A multiway tree is a rooted
   --  collection of nodes and arcs, where each node may have an
   --  arbitrary number of children and where arcs may not have cycles
   --  or cross-references. AVL trees are a form of balance tree
   --  (following the algorithm of Adelson-Velskii and Landis) whose
   --  behavior only exposes operations to insert, delete, and search
   --  for items.

   --  Binary and multiway trees are polylithic structures, and hence
   --  the semantics of copying, assignment, and equality involve
   --  structural sharing. Care must be taken in manipulating the same
   --  tree named by more than one alias. AVL trees are monolithic.

   --  These classes are not intended to be subclassed, and so provide
   --  no virtual members.

   --  These abstractions have been carefully constructed to eliminate
   --  all storage leaks, except in the case of intentional
   --  abuses. When a tree is manipulated, all items that become
   --  unreachable are automatically reclaimed. Furthermore, this
   --  design protects against dangling references: an item is never
   --  reclaimed if there exists a reference to it.

   --  Unreachable items are those that belong to a tree or a subtree
   --  whose root is not designated by any alias. For example,
   --  consider the tree (A (B C (D E))), with the root of the tree
   --  designated by T1. T1 initially points to the root of the tree,
   --  at item A. Invoking the operation Right_Child on T1 now causes
   --  T1 to point to item C. Because A is now considered unreachable,
   --  the storage associated with item A is reclaimed; the parent of
   --  C is now null. Additionally, the sibling subtree rooted at B is
   --  also now unreachable, and so is reclaimed (along with its
   --  children, and recursively so). Similarly, consider the same
   --  tree, with the root of the tree designated by both T1 and
   --  T2. Both T1 and T2 are aliases that initially point to the root
   --  of the tree at item A. Invoking the operation Right_Child on T1
   --  now causes T1 to point to item C; T2 is unaffected. No storage
   --  is reclaimed, since every element of the tree is still
   --  reachable. Suppose we now invoke the member function Clear on
   --  T2. The semantics of this operation are such that only
   --  unreachable items are reclaimed. Thus, the storage associated
   --  with item A is reclaimed, because it is no longer reachable;
   --  additionally, the sibling B (and recursively so, its children)
   --  is reclaimed, because it is also now unreachable; the subtree
   --  denoted by T1 is unaffected. T2 is now null, and the parent of
   --  C is now null.

   --  It is possible, but not generally desirable, to produce
   --  multi-headed trees. In such cases, the parent of the item at
   --  the neck of a multi-headed tree points to the most recently
   --  attached root.

   --  The binary and multiway trees have a similar protocol, except
   --  that the binary tree adds two operations, Left_Child and
   --  Right_Child, and the multiway tree overloads the Append
   --  operation and adds the operation Arity. The AVL tree has a
   --  completely different protocol, with a much more limited set of
   --  operations.

end BC.Containers.Trees;
