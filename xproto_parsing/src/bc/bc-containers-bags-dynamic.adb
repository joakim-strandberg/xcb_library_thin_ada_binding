--  Copyright 1994 Grady Booch
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

package body BC.Containers.Bags.Dynamic is

   procedure Clear (B : in out Bag) is
   begin
      Tables.Clear (B.Rep);
   end Clear;

   procedure Add (B : in out Bag; I : Item; Added : out Boolean) is
   begin
      if Tables.Is_Bound (B.Rep, I) then
         Tables.Rebind (B.Rep, I, Tables.Value_Of (B.Rep, I) + 1);
         Added := False;
      else
         Tables.Bind (B.Rep, I, 1);
         Added := True;
      end if;
   end Add;

   procedure Remove (B : in out Bag; I : Item) is
      Count : Positive;
   begin
      Count := Tables.Value_Of (B.Rep, I);
      if Count = 1 then
         Tables.Unbind (B.Rep, I);
      else
         Tables.Rebind (B.Rep, I, Count - 1);
      end if;
   end Remove;

   function Extent (B : Bag) return Natural is
   begin
      return Tables.Extent (B.Rep);
   end Extent;

   function Count (B : Bag; I : Item) return Natural is
   begin
      if not Tables.Is_Bound (B.Rep, I) then
         return 0;
      else
         return Tables.Value_Of (B.Rep, I);
      end if;
   end  Count;

   function Is_Empty (B : Bag) return Boolean is
   begin
      return Tables.Extent (B.Rep) = 0;
   end Is_Empty;

   function Is_Member (B : Bag; I : Item) return Boolean is
   begin
      return Tables.Is_Bound (B.Rep, I);
   end Is_Member;

   procedure Preallocate (B : in out Bag; Size : Positive) is
   begin
      for Bucket in 1 .. Buckets loop
         IC.Preallocate (B.Rep.Items (Bucket), Size);
         VC.Preallocate (B.Rep.Values (Bucket), Size);
      end loop;
   end Preallocate;

   procedure Set_Chunk_Size (B : in out Bag; Size : Positive) is
   begin
      for Bucket in 1 .. Buckets loop
         IC.Set_Chunk_Size (B.Rep.Items (Bucket), Size);
         VC.Set_Chunk_Size (B.Rep.Values (Bucket), Size);
      end loop;
   end Set_Chunk_Size;

   function Chunk_Size (B : Bag) return Positive is
   begin
      return IC.Chunk_Size (B.Rep.Items (1));
   end Chunk_Size;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Bag);

   function New_Iterator (For_The_Bag : Bag) return Iterator'Class is
      Result : Dynamic_Bag_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Bag'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Private implementations

   procedure Attach (B : in out Bag; I : Item; C : Positive) is
   begin
      Tables.Bind (B.Rep, I, C);
   end Attach;

   procedure Detach (B : in out Bag; I : Item) is
   begin
      Tables.Unbind (B.Rep, I);
   end Detach;

   procedure Set_Value (B : in out Bag; I : Item; C : Positive) is
   begin
      Tables.Rebind (B.Rep, I, C);
   end Set_Value;

   --  Null containers

   function Null_Container return Bag is
      Empty_Container : Bag;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   --  Bodge to make it easier to convert to the real
   --  Unconstrained_Bag later.
   subtype Unconstrained_Bag is Bag;

   procedure Reset (It : in out Dynamic_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Reset (S.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Dynamic_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Next (S.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Dynamic_Bag_Iterator) return Boolean is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (S.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Item (It : Dynamic_Bag_Iterator) return Item is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Dynamic_Bag_Iterator) return Item_Ptr is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Dynamic_Bag_Iterator) is
      S : Unconstrained_Bag'Class
        renames Unconstrained_Bag'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (S.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Bags.Dynamic;
