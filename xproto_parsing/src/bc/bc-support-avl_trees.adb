--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2004 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;

package body BC.Support.AVL_Trees is

   procedure Delete is
      new Ada.Unchecked_Deallocation (AVL_Node, AVL_Node_Ref);

   --  Supporting subprograms
   procedure Purge (Node : in out AVL_Node_Ref);

   procedure Search_Insert (T : in out AVL_Tree'Class;
                            Element : Item;
                            Node : in out AVL_Node_Ref;
                            Increased : in out Boolean;
                            Inserted : out Boolean);

   procedure Search_Delete (T : in out AVL_Tree'Class;
                            Element : Item;
                            Node : in out AVL_Node_Ref;
                            Decreased : in out Boolean;
                            Deleted : out Boolean);

   procedure Balance_Left (Node : in out AVL_Node_Ref;
                           Decreased : in out Boolean);

   procedure Balance_Right (Node : in out AVL_Node_Ref;
                            Decreased : in out Boolean);

   procedure Delete
     (To_Be_Deleted, Candidate_Replacement : in out AVL_Node_Ref;
      Decreased : in out Boolean);

   function Search (T : AVL_Tree'Class;
                    Element : Item;
                    Node : AVL_Node_Ref) return Boolean;

   generic
      with procedure Apply (Elem : in Item; OK : out Boolean);
   procedure Visit (Over_The_Tree : AVL_Tree);


   procedure Purge (Node : in out AVL_Node_Ref) is
   begin
      if Node /= null then
         Purge (Node.Left);
         Purge (Node.Right);
         Delete (Node);
      end if;
   end Purge;

   procedure Search_Insert (T : in out AVL_Tree'Class;
                            Element : Item;
                            Node : in out AVL_Node_Ref;
                            Increased : in out Boolean;
                            Inserted : out Boolean) is
      P1, P2 : AVL_Node_Ref;
   begin
      Inserted := True;
      if Node = null then
         Node := new AVL_Node'(Element => Element,
                               Left => null,
                               Right => null,
                               Balance => Middle);
         Increased := True;
      elsif Element < Node.Element then
         Search_Insert (T, Element, Node.Left, Increased, Inserted);
         if Increased then
            case Node.Balance is
               when Right =>
                  Node.Balance := Middle;
                  Increased := False;
               when Middle =>
                  Node.Balance := Left;
               when Left =>
                  P1 := Node.Left;
                  if P1.Balance = Left then
                     Node.Left := P1.Right;
                     P1.Right := Node;
                     Node.Balance := Middle;
                     Node := P1;
                  else
                     P2 := P1.Right;
                     P1.Right := P2.Left;
                     P2.Left := P1;
                     Node.Left := P2.Right;
                     P2.Right := Node;
                     if P2.Balance = Left then
                        Node.Balance := Right;
                     else
                        Node.Balance := Middle;
                     end if;
                     if P2.Balance = Right then
                        P1.Balance := Left;
                     else
                        P1.Balance := Middle;
                     end if;
                     Node := P2;
                  end if;
                  Node.Balance := Middle;
                  Increased := False;
            end case;
         end if;
      elsif Node.Element < Element then
         Search_Insert (T, Element, Node.Right, Increased, Inserted);
         if Increased then
            case Node.Balance is
               when Left =>
                  Node.Balance := Middle;
                  Increased := False;
               when Middle =>
                  Node.Balance := Right;
               when Right =>
                  P1 := Node.Right;
                  if P1.Balance = Right then
                     Node.Right := P1.Left;
                     P1.Left := Node;
                     Node.Balance := Middle;
                     Node := P1;
                  else
                     P2 := P1.Left;
                     P1.Left := P2.Right;
                     P2.Right := P1;
                     Node.Right := P2.Left;
                     P2.Left := Node;
                     if P2.Balance = Right then
                        Node.Balance := Left;
                     else
                        Node.Balance := Middle;
                     end if;
                     if P2.Balance = Left then
                        P1.Balance := Right;
                     else
                        P1.Balance := Middle;
                     end if;
                     Node := P2;
                  end if;
                  Node.Balance := Middle;
                  Increased := False;
            end case;
         end if;
      else
         --  We need to cope with the case where elements _compare_
         --  equal but their non-key data content has changed.
         Node.Element := Element;
         Inserted := False;
      end if;
   end Search_Insert;

   procedure Balance_Left (Node : in out AVL_Node_Ref;
                           Decreased : in out Boolean) is
      P1, P2 : AVL_Node_Ref;
      Balance1, Balance2 : Node_Balance;
   begin
      case Node.Balance is
         when Left =>
            Node.Balance := Middle;
         when Middle =>
            Node.Balance := Right;
            Decreased := False;
         when Right =>
            P1 := Node.Right;
            Balance1 := P1.Balance;
            if Balance1 >= Middle then
               Node.Right := P1.Left;
               P1.Left := Node;
               if Balance1 = Middle then
                  Node.Balance := Right;
                  P1.Balance := Left;
                  Decreased := False;
               else
                  Node.Balance := Middle;
                  P1.Balance := Middle;
               end if;
               Node := P1;
            else
               P2 := P1.Left;
               Balance2 := P2.Balance;
               P1.Left := P2.Right;
               P2.Right := P1;
               Node.Right := P2.Left;
               P2.Left := Node;
               if Balance2 = Right then
                  Node.Balance := Left;
               else
                  Node.Balance := Middle;
               end if;
               if Balance2 = Left then
                  P1.Balance := Right;
               else
                  P1.Balance := Middle;
               end if;
               Node := P2;
               P2.Balance := Middle;
            end if;
      end case;
   end Balance_Left;

   procedure Balance_Right (Node : in out AVL_Node_Ref;
                            Decreased : in out Boolean)  is
      P1, P2 : AVL_Node_Ref;
      Balance1, Balance2 : Node_Balance;
   begin
      case Node.Balance is
         when Right =>
            Node.Balance := Middle;
         when Middle =>
            Node.Balance := Left;
            Decreased := False;
         when Left =>
            P1 := Node.Left;
            Balance1 := P1.Balance;
            if Balance1 <= Middle then
               Node.Left := P1.Right;
               P1.Right := Node;
               if Balance1 = Middle then
                  Node.Balance := Left;
                  P1.Balance := Right;
                  Decreased := False;
               else
                  Node.Balance := Middle;
                  P1.Balance := Middle;
               end if;
               Node := P1;
            else
               P2 := P1.Right;
               Balance2 := P2.Balance;
               P1.Right := P2.Left;
               P2.Left := P1;
               Node.Left := P2.Right;
               P2.Right := Node;
               if Balance2 = Left then
                  Node.Balance := Right;
               else
                  Node.Balance := Middle;
               end if;
               if Balance2 = Right then
                  P1.Balance := Left;
               else
                  P1.Balance := Middle;
               end if;
               Node := P2;
               P2.Balance := Middle;
            end if;
      end case;
   end Balance_Right;

   --  On entry, To_Be_Deleted is the node which contains the value
   --  that is to be deleted. Candidate_Replacement starts off as the
   --  left child of To_Be_Deleted, but the procedure recurses until
   --  Candidate_Replacement is the rightmost (largest) child of the
   --  left subtree of To_Be_Deleted.
   --
   --  The value at Candidate_Replacement is then transferred to the
   --  node To_Be_Deleted, and the pointer To_Be_Deleted is made to
   --  point to the rightmost child (so that that what eventually gets
   --  deleted is that rightmost child).
   --
   --  The tree is rebalanced as the recursion unwinds.
   procedure Delete
     (To_Be_Deleted, Candidate_Replacement : in out AVL_Node_Ref;
      Decreased : in out Boolean) is
   begin
      if Candidate_Replacement.Right /= null then
         --  Recurse down the right branch
         Delete (To_Be_Deleted, Candidate_Replacement.Right, Decreased);
         if Candidate_Replacement.Left = null
           and then Candidate_Replacement.Right = null then
            Candidate_Replacement.Balance := Middle;
         elsif Decreased then
            Balance_Right (Candidate_Replacement, Decreased);
         end if;
      else
         --  We've found the rightmost child.
         --  Copy the value there to the node that contained the value
         --  to be deleted.
         To_Be_Deleted.Element := Candidate_Replacement.Element;
         --  Replace the pointer to the node that contained the value
         --  to be deleted with a pointer to the rightmost child of
         --  the left subtree (no longer needed, and to be deleted by
         --  the caller).
         To_Be_Deleted := Candidate_Replacement;
         --  Candidate_Replacement is the actual pointer in the parent
         --  node; it needs to point to the left subtree, if any, of
         --  the node that was the rightmost child and which we are
         --  about to delete.
         Candidate_Replacement := Candidate_Replacement.Left;
         --  We've definitely reduced the depth.
         Decreased := True;
      end if;
   end Delete;

   procedure Search_Delete (T : in out AVL_Tree'Class;
                            Element : Item;
                            Node : in out AVL_Node_Ref;
                            Decreased : in out Boolean;
                            Deleted : out Boolean) is
      Q : AVL_Node_Ref;
   begin
      Deleted := False;
      if Node /= null then
         if Element < Node.Element then
            Search_Delete (T, Element, Node.Left, Decreased, Deleted);
            if Decreased then
               Balance_Left (Node, Decreased);
            end if;
         elsif Node.Element < Element then
            Search_Delete (T, Element, Node.Right, Decreased, Deleted);
            if Decreased then
               Balance_Right (Node, Decreased);
            end if;
         else
            Q := Node;
            Deleted := True;
            if Q.Right = null then
               Node := Q.Left;
               Decreased := True;
            elsif Q.Left = null then
               Node := Q.Right;
               Decreased := True;
            else
               Delete (Q, Q.Left, Decreased);
               if Decreased then
                  Balance_Left (Node, Decreased);
               end if;
            end if;
            Delete (Q);
         end if;
      end if;
   end Search_Delete;

   function Search (T : AVL_Tree'Class;
                    Element : Item;
                    Node : AVL_Node_Ref) return Boolean is
   begin
      if Node /= null then
         if Node.Element = Element then
            return True;
         elsif Element < Node.Element then
            return Search (T, Element, Node.Left);
         else
            return Search (T, Element, Node.Right);
         end if;
      else
         return False;
      end if;
   end Search;

   --  end supporting functions

   function "=" (L, R : AVL_Tree) return Boolean is
      --  Once we know that the sizes are the same, we only need to
      --  check that all members of L are in R, because we don't allow
      --  duplicate members.
      procedure Check_In_Right (Elem : in Item; Found : out Boolean);
      procedure Compare is new Visit (Apply => Check_In_Right);
      Are_Equal : Boolean := True;
      procedure Check_In_Right (Elem : in Item; Found : out Boolean) is
      begin
         Found := Is_Member (R, Elem); -- to terminate early
         if not Found then
            Are_Equal := False;
         end if;
      end Check_In_Right;
   begin
      if L.Size /= R.Size then
         return False;
      end if;
      Compare (Over_The_Tree => L);
      return Are_Equal;
   end "=";

   procedure Clear (T : in out AVL_Tree) is
   begin
      Purge (T.Rep);
      T.Size := 0;
   end Clear;

   procedure Insert (T : in out AVL_Tree;
                     Element : Item;
                     Not_Found : out Boolean) is
      Increased : Boolean := False;
      Result : Boolean;
   begin
      Search_Insert (T, Element, T.Rep, Increased, Result);
      if Result then
         T.Size := T.Size + 1;
         Not_Found := True;
      else
         Not_Found := False;
      end if;
   end Insert;

   procedure Delete
     (T : in out AVL_Tree; Element : Item; Found : out Boolean) is
      Decreased : Boolean := False;
      Result : Boolean;
   begin
      Search_Delete (T, Element, T.Rep, Decreased, Result);
      if Result then
         T.Size := T.Size - 1;
         Found := True;
      else
         Found := False;
      end if;
   end Delete;

   function Extent (T : in AVL_Tree) return Natural is
   begin
      return T.Size;
   end Extent;

   function Is_Null (T : in AVL_Tree) return Boolean is
   begin
      return T.Rep = null;
   end Is_Null;

   function Is_Member (T : in AVL_Tree; Element : Item) return Boolean is
   begin
      return Search (T, Element, T.Rep);
   end Is_Member;

   procedure Visit (Over_The_Tree : AVL_Tree) is
      Continue : Boolean := True;
      procedure Visit (Node : AVL_Node_Ref);
      procedure Visit (Node : AVL_Node_Ref) is
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
      Visit (Over_The_Tree.Rep);
   end Visit;

   procedure Adjust (T : in out AVL_Tree) is
      New_Tree : AVL_Tree;
      procedure Add (Elem : in Item; OK : out Boolean);
      procedure Copy is new Visit (Apply => Add);
      procedure Add (Elem : in Item; OK : out Boolean) is
         Inserted : Boolean;
      begin
         Insert (T => New_Tree, Element => Elem, Not_Found => Inserted);
         --  XXX should test Inserted?
         OK := True;
      end Add;
   begin
      --  Create a deep copy of the representation
      Copy (Over_The_Tree => T);
      --  Replace the original representation with the copy
      T.Rep := New_Tree.Rep;
      --  Null out the spare reference to the copy (so that when
      --  New_Tree gets finalized on exit from this procedure, we
      --  don't Clear it down). NB, mustn't do a whole-record
      --  assignment here or we'll end up with a recursive disaster).
      New_Tree.Rep := null;
      New_Tree.Size := 0;
   end Adjust;

   procedure Finalize (T : in out AVL_Tree) is
   begin
      Clear (T);
   end Finalize;

end BC.Support.AVL_Trees;
