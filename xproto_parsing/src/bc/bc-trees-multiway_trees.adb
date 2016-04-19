--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;

package body BC.Trees.Multiway_Trees is

   function Create
     (I : Item; Parent, Child, Sibling : Multiway_Node_Ref)
     return Multiway_Node_Ref;
   pragma Inline (Create);

   function Create
     (I : Item; Parent, Child, Sibling : Multiway_Node_Ref)
     return Multiway_Node_Ref is
      Result : Multiway_Node_Ref;
   begin
      Result := new Multiway_Node'(Element => I,
                                   Parent => Parent,
                                   Child => Child,
                                   Sibling => Sibling,
                                   Count => 1);
      if Child /= null then
         Child.Parent := Result;
      end if;
      return Result;
   end Create;

   procedure Delete
   is new Ada.Unchecked_Deallocation (Multiway_Node, Multiway_Node_Ref);

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   --  package Allow_Element_Access
   --  is new System.Address_To_Access_Conversions (Item);

   procedure Purge (Curr : in out Multiway_Node_Ref);
   procedure Purge (Curr : in out Multiway_Node_Ref) is
   begin
      if Curr /= null then
         if Curr.Count > 1 then
            Curr.Count := Curr.Count - 1;
         else
            declare
               Ptr : Multiway_Node_Ref := Curr.Child;
               Next : Multiway_Node_Ref;
            begin
               while Ptr /= null loop
                  Next := Ptr.Sibling;
                  Ptr.Sibling := null;
                  Purge (Ptr);
                  if Ptr /= null then
                     Ptr.Parent := null;
                  end if;
                  Ptr := Next;
               end loop;
               Delete (Curr);
            end;
         end if;
      end if;
   end Purge;

   function Create (From : Multiway_Tree) return Multiway_Tree is
      Temp : Multiway_Tree
        := (Ada.Finalization.Controlled with Rep => From.Rep);
   begin
      if From.Rep /= null then
         Temp.Rep.Count := Temp.Rep.Count + 1;
      end if;
      return Temp;
   end Create;

   function "=" (Left, Right : Multiway_Tree) return Boolean is
   begin
      return Left.Rep = Right.Rep;
   end "=";

   procedure Clear (T : in out Multiway_Tree) is
   begin
      Purge (T.Rep);
      T.Rep := null;
   end Clear;

   procedure Insert (T : in out Multiway_Tree; Elem : in Item) is
   begin
      if T.Rep /= null and then T.Rep.Parent /= null then
         raise BC.Not_Root;
      end if;
      T.Rep := Create (Elem,
                       Parent => null,
                       Child => T.Rep,
                       Sibling => null);
   end Insert;

   procedure Append (T : in out Multiway_Tree; Elem : in Item) is
   begin
      if T.Rep = null then
         T.Rep := Create (Elem,
                          Parent => null,
                          Child => T.Rep,
                          Sibling => null);
      else
         T.Rep.Child := Create (Elem,
                                Parent => T.Rep,
                                Child => null,
                                Sibling => T.Rep.Child);
      end if;
   end Append;

   procedure Append (T : in out Multiway_Tree;
                     Elem : in Item;
                     After : Positive) is
   begin
      if T.Rep = null then
         T.Rep := Create (Elem,
                          Parent => null,
                          Child => T.Rep,
                          Sibling => null);
      else
         declare
            Curr : Multiway_Node_Ref := T.Rep.Child;
         begin
            if Curr = null then
               T.Rep.Child := Create (Elem,
                                      Parent => T.Rep,
                                      Child => null,
                                      Sibling => T.Rep.Child);
            else
               declare
                  I : Positive := 1;
               begin
                  while Curr /= null and then I < After loop
                     Curr := Curr.Sibling;
                     I := I + 1;
                  end loop;
                  if Curr = null then
                     raise BC.Range_Error;
                  end if;
                  Curr.Sibling := Create (Elem,
                                          Parent => T.Rep,
                                          Child => null,
                                          Sibling => Curr.Sibling);
               end;
            end if;
         end;
      end if;
   end Append;

   procedure Append (T : in out Multiway_Tree;
                     From_Tree : in out Multiway_Tree) is
   begin
      if From_Tree.Rep = null then
         return;
      end if;
      if From_Tree.Rep.Parent /= null then
         raise BC.Not_Root;
      end if;
      if T.Rep = null then
         T.Rep := From_Tree.Rep;
         T.Rep.Count := T.Rep.Count + 1;
      else
         From_Tree.Rep.Sibling := T.Rep.Child;
         From_Tree.Rep.Parent := T.Rep;
         From_Tree.Rep.Count := From_Tree.Rep.Count + 1;
         T.Rep.Child := From_Tree.Rep;
      end if;
   end Append;

   procedure Remove (T : in out Multiway_Tree; Index : Positive) is
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      declare
         I : Positive := 1;
         Prev : Multiway_Node_Ref;
         Curr : Multiway_Node_Ref := T.Rep.Child;
      begin
         while Curr /= null and then I < Index loop
            Prev := Curr;
            Curr := Curr.Sibling;
            I := I + 1;
         end loop;
         if Curr = null then
            raise BC.Range_Error;
         end if;
         if Prev = null then
            T.Rep.Child := Curr.Sibling;
         else
            Prev.Sibling := Curr.Sibling;
         end if;
         Curr.Parent := null;
         Curr.Sibling := null;
         Purge (Curr);
      end;
   end Remove;

   procedure Share (T : in out Multiway_Tree;
                    Share_With : in Multiway_Tree;
                    Child : Positive) is
      Ptr : Multiway_Node_Ref := Share_With.Rep;
      I : Positive := 1;
   begin
      if Ptr = null then
         raise BC.Is_Null;
      end if;
      Ptr := Ptr.Child;
      while Ptr /= null and then I < Child loop
         Ptr := Ptr.Sibling;
         I := I + 1;
      end loop;
      if Ptr = null then
         raise BC.Range_Error;
      end if;
      Clear (T);
      T.Rep := Ptr;
      T.Rep.Count := T.Rep.Count + 1;
   end Share;

   procedure Swap_Child (T : in out Multiway_Tree;
                         Swap_With : in out Multiway_Tree;
                         Child : in Positive) is
      Prev : Multiway_Node_Ref;
      Curr : Multiway_Node_Ref := T.Rep;
      I : Positive := 1;
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      if Swap_With.Rep /= null and then Swap_With.Rep.Parent /= null then
         raise BC.Not_Root;
      end if;
      Curr := Curr.Child;
      while Curr /= null and then I < Child loop
         Prev := Curr;
         Curr := Curr.Sibling;
         I := I + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      Swap_With.Rep.Sibling := Curr.Sibling;
      if Prev = null then
         T.Rep.Child := Swap_With.Rep;
      else
         Prev.Sibling := Swap_With.Rep;
      end if;
      if Swap_With.Rep /= null then
         Swap_With.Rep.Parent := T.Rep;
      end if;
      Swap_With.Rep := Curr;
      Swap_With.Rep.Sibling := null;
      Swap_With.Rep.Parent := null;
   end Swap_Child;


   procedure Child (T : in out Multiway_Tree; Child : in Positive) is
      Curr : Multiway_Node_Ref := T.Rep;
      I : Positive := 1;
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      Curr := Curr.Child;
      while Curr /= null and then I < Child loop
         Curr := Curr.Sibling;
         I := I + 1;
      end loop;
      if Curr = null then
         raise BC.Range_Error;
      end if;
      Curr.Count := Curr.Count + 1;
      Purge (T.Rep);
      T.Rep := Curr;
   end Child;

   procedure Parent (T : in out Multiway_Tree) is
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      if T.Rep.Parent = null then
         Clear (T);
      else
         T.Rep.Count := T.Rep.Count - 1;
         T.Rep := T.Rep.Parent;
         T.Rep.Count := T.Rep.Count + 1;
      end if;
   end Parent;

   procedure Set_Item (T : in out Multiway_Tree; Elem : in Item) is
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      T.Rep.Element := Elem;
   end Set_Item;

   function Arity (T : Multiway_Tree) return Natural is
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      declare
         Count : Natural := 0;
         Ptr : Multiway_Node_Ref := T.Rep.Child;
      begin
         while Ptr /= null loop
            Count := Count + 1;
            Ptr := Ptr.Sibling;
         end loop;
         return Count;
      end;
   end Arity;

   function Has_Children (T : in Multiway_Tree) return Boolean is
   begin
      return T.Rep /= null and then T.Rep.Child /= null;
   end Has_Children;

   function Is_Null (T : in Multiway_Tree) return Boolean is
   begin
      return T.Rep = null;
   end Is_Null;

   function Is_Shared (T : in Multiway_Tree) return Boolean is
   begin
      return T.Rep /= null and then T.Rep.Count > 1;
   end Is_Shared;

   function Is_Root (T : in Multiway_Tree) return Boolean is
   begin
      return T.Rep = null or else T.Rep.Parent = null;
   end Is_Root;

   function Item_At (T : in Multiway_Tree) return Item is
   begin
      if T.Rep = null then
         raise BC.Is_Null;
      end if;
      return T.Rep.Element;
   end Item_At;

   --     function Item_At (T : in Multiway_Tree) return Item_Ptr is
   --     begin
   --        if T.Rep = null then
   --           raise BC.Is_Null;
   --        end if;
   --        return Item_Ptr
   --      (Allow_Element_Access.To_Pointer (T.Rep.Element'Address));
   --     end Item_At;

   procedure Initialize (T : in out Multiway_Tree) is
      pragma Warnings (Off, T);
   begin
      null;
   end Initialize;

   procedure Adjust (T : in out Multiway_Tree) is
   begin
      if T.Rep /= null then
         T.Rep.Count := T.Rep.Count + 1;
      end if;
   end Adjust;

   procedure Finalize (T : in out Multiway_Tree) is
   begin
      Clear (T);
   end Finalize;

end BC.Trees.Multiway_Trees;
