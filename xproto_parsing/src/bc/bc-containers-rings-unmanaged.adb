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

package body BC.Containers.Rings.Unmanaged is

   function "=" (Left, Right : in Ring) return Boolean is
      use Ring_Nodes;
   begin
      return Left.Top = Right.Top and then Left.Rep = Right.Rep;
   end "=";

   procedure Clear (R : in out Ring) is
   begin
      Ring_Nodes.Clear (R.Rep);
      R.Top := 0;
      R.Mark := 0;
   end Clear;

   procedure Insert (R : in out Ring; Elem : Item) is
   begin
      if R.Top = 0 then
         R.Top := 1;
         R.Mark := 1;
         Ring_Nodes.Insert (R.Rep, Elem);
      else
         if R.Mark >= R.Top then
            R.Mark := R.Mark + 1;
         end if;
         Ring_Nodes.Insert (R.Rep, Elem, Before => R.Top);
      end if;
   end Insert;

   procedure Pop (R : in out Ring) is
      Size : Natural;
   begin
      Ring_Nodes.Remove (R.Rep, R.Top);
      Size := Extent (R);
      if Size = 0 then
         R.Top := 0;
         R.Mark := 0;
      else
         if R.Mark > R.Top then
            R.Mark := R.Mark - 1;
         elsif R.Mark = R.Top and then R.Mark > Size then
            R.Mark := 1;
         end if;
         if R.Top > Size then
            R.Top := 1;
         end if;
      end if;
   end Pop;

   function Extent (R : Ring) return Natural is
   begin
      return Ring_Nodes.Length (R.Rep);
   end Extent;

   function Is_Empty (R : Ring) return Boolean is
   begin
      return Ring_Nodes.Length (R.Rep) = 0;
   end Is_Empty;

   function Top (R : Ring) return Item is
   begin
      if R.Top = 0 then
         raise BC.Underflow;
      end if;
      return Ring_Nodes.Item_At (R.Rep, R.Top);
   end Top;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Ring);

   function New_Iterator (For_The_Ring : Ring) return Iterator'Class is
      Result : Ring_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Ring'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   procedure Add (R : in out Ring; Elem : Item) is
   begin
      Ring_Nodes.Append (R.Rep, Elem);
   end Add;

   function Item_At (R : Ring; Index : Positive) return Item_Ptr is
   begin
      return Ring_Nodes.Item_At (R.Rep, Index);
   end Item_At;

   function Null_Container return Ring is
      Empty_Container : Ring;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

end BC.Containers.Rings.Unmanaged;
