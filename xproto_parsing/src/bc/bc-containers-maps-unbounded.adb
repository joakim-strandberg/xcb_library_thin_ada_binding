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

package body BC.Containers.Maps.Unbounded is

   function "=" (L, R : Unconstrained_Map) return Boolean is
   begin
      return Tables."=" (L.Rep, R.Rep);
   end "=";

   procedure Clear (M : in out Unconstrained_Map) is
   begin
      Tables.Clear (M.Rep);
   end Clear;

   procedure Bind
     (M : in out Unconstrained_Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Bind;

   procedure Rebind
     (M : in out Unconstrained_Map; K : Key; I : Item) is
   begin
      Tables.Rebind (M.Rep, K, I);
   end Rebind;

   procedure Unbind (M : in out Unconstrained_Map; K : Key) is
   begin
      Tables.Unbind (M.Rep, K);
   end Unbind;

   function Extent (M : Unconstrained_Map) return Natural is
   begin
      return Tables.Extent (M.Rep);
   end Extent;

   function Is_Empty (M : Unconstrained_Map) return Boolean is
   begin
      return Tables.Extent (M.Rep) = 0;
   end Is_Empty;

   function Is_Bound (M : Unconstrained_Map; K : Key) return Boolean is
   begin
      return Tables.Is_Bound (M.Rep, K);
   end Is_Bound;

   function Item_Of (M : Unconstrained_Map; K : Key) return Item is
   begin
      return Tables.Value_Of (M.Rep, K);
   end Item_Of;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Unconstrained_Map);

   function New_Iterator
     (For_The_Map : Unconstrained_Map) return Iterator'Class is
      Result : Unbounded_Map_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Map'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Null containers

   function Null_Container return Unconstrained_Map is
      Empty_Container : Map;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   procedure Reset (It : in out Unbounded_Map_Iterator) is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Reset (S.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Unbounded_Map_Iterator) is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Next (S.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Unbounded_Map_Iterator) return Boolean is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (S.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Key (It : Unbounded_Map_Iterator) return Key is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Key;

   function Current_Item (It : Unbounded_Map_Iterator) return Item is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Value_Ptr (S.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Unbounded_Map_Iterator) return Item_Ptr is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Value_Ptr (S.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Unbounded_Map_Iterator) is
      S : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (S.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Maps.Unbounded;
