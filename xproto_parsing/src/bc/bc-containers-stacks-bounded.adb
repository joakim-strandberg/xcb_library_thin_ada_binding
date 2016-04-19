--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1473 $
--  $Date: 2011-06-25 21:02:07 +0100 (Sat, 25 Jun 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Containers.Stacks.Bounded is

   procedure Clear (S : in out Unconstrained_Stack) is
   begin
      Stack_Nodes.Clear (S.Rep);
   end Clear;

   procedure Push (S : in out Unconstrained_Stack; Elem : Item) is
   begin
      Stack_Nodes.Insert (S.Rep, Elem);
   end Push;

   procedure Pop (S : in out Unconstrained_Stack) is
   begin
      Stack_Nodes.Remove (S.Rep, 1);
   end Pop;

   function Available (S : in Unconstrained_Stack) return Natural is
   begin
      return Stack_Nodes.Available (S.Rep);
   end Available;

   function Depth (S : Unconstrained_Stack) return Natural is
   begin
      return Stack_Nodes.Length (S.Rep);
   end Depth;

   function Is_Empty (S : Unconstrained_Stack) return Boolean is
   begin
      return Stack_Nodes.Length (S.Rep) = 0;
   end Is_Empty;

   function Top (S : Unconstrained_Stack) return Item is
   begin
      return Stack_Nodes.First (S.Rep);
   end Top;

   function "=" (Left, Right : Unconstrained_Stack) return Boolean is
   begin
      return Left.Rep = Right.Rep;
   end "=";

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Stack);

   function New_Iterator
     (For_The_Stack : Unconstrained_Stack) return Iterator'Class is
      Result : Stack_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Stack'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Item_At
     (S : Unconstrained_Stack; Index : Positive) return Item_Ptr is
   begin
      return Stack_Nodes.Item_At (S.Rep, Index);
   end Item_At;

   procedure Add (S : in out Unconstrained_Stack; Elem : Item) is
   begin
      Stack_Nodes.Append (S.Rep, Elem);
   end Add;

   procedure Remove (S : in out Unconstrained_Stack; From : Positive) is
   begin
      Stack_Nodes.Remove (S.Rep, From);
   end Remove;

   function Null_Container return Unconstrained_Stack is
      Empty_Container : Stack;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Stacks.Bounded;
