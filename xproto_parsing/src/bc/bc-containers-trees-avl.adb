--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
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

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Containers.Trees.AVL is

   function "=" (L, R : AVL_Tree) return Boolean is
   begin
      return Support."=" (L.Rep, R.Rep);
   end "=";

   procedure Clear (T : in out AVL_Tree) is
   begin
      Support.Clear (T.Rep);
   end Clear;

   procedure Insert (T : in out AVL_Tree;
                     Element : Item;
                     Not_Found : out Boolean) is
   begin
      Support.Insert (T.Rep, Element, Not_Found);
   end Insert;

   procedure Delete
     (T : in out AVL_Tree; Element : Item; Found : out Boolean) is
   begin
      Support.Delete (T.Rep, Element, Found);
   end Delete;

   function Extent (T : in AVL_Tree) return Natural is
   begin
      return T.Rep.Size;
   end Extent;

   function Is_Null (T : in AVL_Tree) return Boolean is
   begin
      return Support.Is_Null (T.Rep);
   end Is_Null;

   function Is_Member (T : in AVL_Tree; Element : Item) return Boolean is
   begin
      return Support.Is_Member (T.Rep, Element);
   end Is_Member;

   procedure Access_Actual_Item (In_The_Tree : AVL_Tree;
                                 Elem : Item;
                                 Found : out Boolean) is
      procedure Access_Actual_Item (Node : Support.AVL_Node_Ref);
      procedure Access_Actual_Item (Node : Support.AVL_Node_Ref) is
         use type Support.AVL_Node_Ref;
      begin
         if Node /= null then
            if Node.Element = Elem then
               Found := True;
               Apply (Node.Element);
            elsif Elem < Node.Element then
               Access_Actual_Item (Node.Left);
            else
               Access_Actual_Item (Node.Right);
            end if;
         end if;
      end Access_Actual_Item;
   begin
      Found := False;
      Access_Actual_Item (In_The_Tree.Rep.Rep);
   end Access_Actual_Item;

   procedure Visit (Over_The_Tree : AVL_Tree) is
      Continue : Boolean := True;
      procedure Visit (Node : Support.AVL_Node_Ref);
      procedure Visit (Node : Support.AVL_Node_Ref) is
         use type Support.AVL_Node_Ref;
      begin
         if Node /= null then
            Visit (Node.Left);
            if not Continue then
               return;
            end if;
            Apply (Node.Element, Continue);
            if not Continue then
               return;
            end if;
            Visit (Node.Right);
         end if;
      end Visit;
   begin
      Visit (Over_The_Tree.Rep.Rep);
   end Visit;

   procedure Modify (Over_The_Tree : AVL_Tree) is
      Continue : Boolean := True;
      procedure Modify (Node : Support.AVL_Node_Ref);
      procedure Modify (Node : Support.AVL_Node_Ref) is
         use type Support.AVL_Node_Ref;
      begin
         if Node /= null then
            Modify (Node.Left);
            if not Continue then
               return;
            end if;
            Apply (Node.Element, Continue);
            if not Continue then
               return;
            end if;
            Modify (Node.Right);
         end if;
      end Modify;
   begin
      Modify (Over_The_Tree.Rep.Rep);
   end Modify;

   function Null_Container return AVL_Tree is
      Empty_Container : AVL_Tree;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   --  Iteration

   package Address_Conversions
   is new System.Address_To_Access_Conversions (AVL_Tree);

   function New_Iterator (For_The_Tree : AVL_Tree) return Iterator'Class is
      Result : AVL_Tree_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer
                         (For_The_Tree'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   procedure Reset (It : in out AVL_Tree_Iterator) is
      This : Support.AVL_Node_Ref
        := AVL_Tree (It.For_The_Container.all).Rep.Rep;
      use type Support.AVL_Node_Ref;
   begin
      It.Previous := null;
      It.Current := null;
      while This /= null loop
         It.Current := This;
         This := This.Left;
      end loop;
   end Reset;

   function Is_Done (It : AVL_Tree_Iterator) return Boolean is
      use type Support.AVL_Node_Ref;
   begin
      return It.Current = null;
   end Is_Done;

   function Current_Item (It : AVL_Tree_Iterator) return Item is
   begin
      return It.Current.Element;
   end Current_Item;

   procedure Next (It : in out AVL_Tree_Iterator) is
      procedure Visit (Node : Support.AVL_Node_Ref);
      Found_Previous : Boolean := False;
      Continue : Boolean := True;
      procedure Visit (Node : Support.AVL_Node_Ref) is
         use type Support.AVL_Node_Ref;
      begin
         if Node /= null then
            Visit (Node.Left);
            if not Continue then
               return;
            elsif Found_Previous then
               It.Current := Node;
               Continue := False;
               return;
            elsif Node = It.Previous then
               Found_Previous := True;
            end if;
            Visit (Node.Right);
         end if;
      end Visit;
   begin
      It.Previous := It.Current;
      It.Current := null;
      Visit (AVL_Tree (It.For_The_Container.all).Rep.Rep);
   end Next;

   procedure Delete_Item_At (It : in out AVL_Tree_Iterator) is
   begin
      raise Not_Yet_Implemented;
   end Delete_Item_At;

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   package Allow_Element_Access
   is new System.Address_To_Access_Conversions (Item);

   function Current_Item_Ptr (It : AVL_Tree_Iterator) return Item_Ptr is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Item_Ptr
        (Allow_Element_Access.To_Pointer (It.Current.Element'Address));
   end Current_Item_Ptr;

end BC.Containers.Trees.AVL;
