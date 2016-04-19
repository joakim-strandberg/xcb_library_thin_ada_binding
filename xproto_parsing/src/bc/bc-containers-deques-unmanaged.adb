--  Copyright 1994 Grady Booch
--  Copyright 2003-2011 Simon Wright <simon@pushface.org>

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

package body BC.Containers.Deques.Unmanaged is

   procedure Clear (D : in out Deque) is
   begin
      Deque_Nodes.Clear (D.Rep);
   end Clear;

   procedure Append (D : in out Deque;
                     Elem : Item;
                     Location : Deque_End := Back) is
   begin
      if Location = Back then
         Deque_Nodes.Append (D.Rep, Elem);
      else
         Deque_Nodes.Insert (D.Rep, Elem);
      end if;
   end Append;

   procedure Pop (D : in out Deque; Location : Deque_End := Front) is
   begin
      if Location = Front then
         Deque_Nodes.Remove (D.Rep, 1);
      else
         Deque_Nodes.Remove (D.Rep,
                             Deque_Nodes.Length (D.Rep));
      end if;
   end Pop;

   procedure Remove (D : in out Deque; From : Positive) is
   begin
      Deque_Nodes.Remove (D.Rep, From);
   end Remove;

   function Length (D : Deque) return Natural is
   begin
      return Deque_Nodes.Length (D.Rep);
   end Length;

   function Is_Empty (D : Deque) return Boolean is
   begin
      return Deque_Nodes.Length (D.Rep) = 0;
   end Is_Empty;

   function Front (D : Deque) return Item is
   begin
      return Deque_Nodes.First (D.Rep);
   end Front;

   function Back (D : Deque) return Item is
   begin
      return Deque_Nodes.Last (D.Rep);
   end Back;

   function Location (D : Deque; Elem : Item) return Natural is
   begin
      return Deque_Nodes.Location (D.Rep, Elem);
   end Location;

   function "=" (Left, Right : Deque) return Boolean is
      use Deque_Nodes;
   begin
      return Left.Rep = Right.Rep;
   end "=";

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Deque);

   function New_Iterator
     (For_The_Deque : Deque) return Iterator'Class is
      Result : Deque_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Deque'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Item_At (D : Deque; Index : Positive) return Item_Ptr is
   begin
      return Deque_Nodes.Item_At (D.Rep, Index);
   end Item_At;

   function Null_Container return Deque is
      Empty_Container : Deque;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Deques.Unmanaged;
