--  Copyright 1994 Grady Booch
--  Copyright 1998-20 Simon Wright <simon@pushface.org>

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

package body BC.Containers.Maps.Dynamic is

   function "=" (L, R : Map) return Boolean is
   begin
      return Tables."=" (L.Rep, R.Rep);
   end "=";

   procedure Clear (M : in out Map) is
   begin
      Tables.Clear (M.Rep);
   end Clear;

   procedure Bind
     (M : in out Map; K : Key; I : Item) is
   begin
      Tables.Bind (M.Rep, K, I);
   end Bind;

   procedure Rebind
     (M : in out Map; K : Key; I : Item) is
   begin
      Tables.Rebind (M.Rep, K, I);
   end Rebind;

   procedure Unbind (M : in out Map; K : Key) is
   begin
      Tables.Unbind (M.Rep, K);
   end Unbind;

   function Extent (M : Map) return Natural is
   begin
      return Tables.Extent (M.Rep);
   end Extent;

   function Is_Empty (M : Map) return Boolean is
   begin
      return Tables.Extent (M.Rep) = 0;
   end Is_Empty;

   function Is_Bound (M : Map; K : Key) return Boolean is
   begin
      return Tables.Is_Bound (M.Rep, K);
   end Is_Bound;

   function Item_Of (M : Map; K : Key) return Item is
   begin
      return Tables.Value_Of (M.Rep, K);
   end Item_Of;

   procedure Preallocate (M : in out Map; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         KC.Preallocate (M.Rep.Items (B), Size);
         IC.Preallocate (M.Rep.Values (B), Size);
      end loop;
   end Preallocate;

   procedure Set_Chunk_Size (M : in out Map; Size : Positive) is
   begin
      for B in 1 .. Buckets loop
         KC.Set_Chunk_Size (M.Rep.Items (B), Size);
         IC.Set_Chunk_Size (M.Rep.Values (B), Size);
      end loop;
   end Set_Chunk_Size;

   function Chunk_Size (M : Map) return Positive is
   begin
      return KC.Chunk_Size (M.Rep.Items (1));
   end Chunk_Size;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Map);

   function New_Iterator (For_The_Map : Map) return Iterator'Class is
      Result : Dynamic_Map_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Map'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   --  Null containers

   function Null_Container return Map is
      Empty_Container : Map;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   --  Iterators

   --  XXX bodge to make it easier to convert to the real
   --  Unconstrained_Map later.
   subtype Unconstrained_Map is Map;

   procedure Reset (It : in out Dynamic_Map_Iterator) is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Reset (M.Rep, It.Bucket_Index, It.Index);
   end Reset;

   procedure Next (It : in out Dynamic_Map_Iterator) is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Next (M.Rep, It.Bucket_Index, It.Index);
   end Next;

   function Is_Done (It : Dynamic_Map_Iterator) return Boolean is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Is_Done (M.Rep, It.Bucket_Index, It.Index);
   end Is_Done;

   function Current_Key (It : Dynamic_Map_Iterator) return Key is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Item_Ptr (M.Rep, It.Bucket_Index, It.Index).all;
   end Current_Key;

   function Current_Item (It : Dynamic_Map_Iterator) return Item is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Value_Ptr (M.Rep, It.Bucket_Index, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Dynamic_Map_Iterator) return Item_Ptr is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      return Tables.Current_Value_Ptr (M.Rep, It.Bucket_Index, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Dynamic_Map_Iterator) is
      M : Unconstrained_Map'Class
        renames Unconstrained_Map'Class (It.For_The_Container.all);
   begin
      Tables.Delete_Item_At (M.Rep, It.Bucket_Index, It.Index);
   end Delete_Item_At;

end BC.Containers.Maps.Dynamic;
