--  Copyright 1994 Grady Booch
--  Copyright 2005 Martin Krischik
--  Copyright 1994-1997 David Weller
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

--  $Revision: 1490 $
--  $Date: 2011-09-01 17:23:20 +0100 (Thu, 01 Sep 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Indefinite_Unmanaged_Containers.Queues is

   procedure Clear (Q : in out Queue) is
   begin
      Queue_Nodes.Clear (Q.Rep);
   end Clear;

   procedure Append (Q : in out Queue; Elem : Item) is
   begin
      Queue_Nodes.Append (Q.Rep, Elem);
   end Append;

   procedure Pop (Q : in out Queue) is
   begin
      Queue_Nodes.Remove (Q.Rep, 1);
   end Pop;

   procedure Remove (Q : in out Queue; From : Positive) is
   begin
      Queue_Nodes.Remove (Q.Rep, From);
   end Remove;

   function Length (Q : Queue) return Natural is
   begin
      return Queue_Nodes.Length (Q.Rep);
   end Length;

   function Is_Empty (Q : Queue) return Boolean is
   begin
      return Queue_Nodes.Length (Q.Rep) = 0;
   end Is_Empty;

   function Front (Q : Queue) return Item is
   begin
      return Queue_Nodes.First (Q.Rep);
   end Front;

   procedure Process_Front (Q : in out Queue'Class) is
   begin
      Process (Item_At (Q, 1).all);
   end Process_Front;

   function Location (Q : Queue; Elem : Item) return Natural is
   begin
      return Queue_Nodes.Location (Q.Rep, Elem);
   end Location;

   function "=" (Left, Right : Queue) return Boolean is
      use Queue_Nodes;
   begin
      return Left.Rep = Right.Rep;
   end "=";

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Queue);

   function New_Iterator (For_The_Queue : Queue) return Iterator'Class is
      Result : Queue_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Queue'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Item_At (Q : Queue; Index : Positive) return Item_Ptr is
   begin
      return Queue_Nodes.Item_At (Q.Rep, Index);
   end Item_At;

   function Null_Container return Queue is
      Empty_Container : Queue;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   procedure Reset (It : in out Queue_Iterator) is
      Q : Queue'Class
        renames Queue'Class (It.For_The_Container.all);
   begin
      if Length (Q) = 0 then
         It.Index := 0;
      else
         It.Index := 1;
      end if;
   end Reset;

   procedure Next (It : in out Queue_Iterator) is
   begin
      It.Index := It.Index + 1;
   end Next;

   function Is_Done (It : Queue_Iterator) return Boolean is
      Q : Queue'Class
     renames Queue'Class (It.For_The_Container.all);
   begin
      return It.Index = 0 or else It.Index > Length (Q);
   end Is_Done;

   function Current_Item (It : Queue_Iterator) return Item is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Item_At (It.For_The_Container.all, It.Index).all;
   end Current_Item;

   function Current_Item_Ptr (It : Queue_Iterator) return Item_Ptr is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      return Item_At (It.For_The_Container.all, It.Index);
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Queue_Iterator) is
      Q : Queue'Class
        renames Queue'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      Remove (Q, It.Index);
   end Delete_Item_At;

end BC.Indefinite_Unmanaged_Containers.Queues;
