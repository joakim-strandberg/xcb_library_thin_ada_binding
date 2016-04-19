--  Copyright 2001-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1451 $
--  $Date: 2011-02-13 21:34:58 +0000 (Sun, 13 Feb 2011) $
--  $Author: simonjwright $

package body BC.Simple_Collections is

   function Null_Container return Collection is
      Empty_Container : Collection;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

   function Null_Collection return Collection is
   begin
      return Null_Container;
   end Null_Collection;

   function "=" (Left, Right : in Collection) return Boolean is
   begin
      return Collections."=" (Collections.Collection (Left),
                              Collections.Collection (Right));
   end "=";

   procedure Clear (C : in out Collection) is
   begin
      Collections.Clear (Collections.Collection (C));
   end Clear;

   procedure Insert (C : in out Collection; Elem : Item) is
   begin
      Collections.Insert (Collections.Collection (C), Elem);
   end Insert;

   procedure Insert (C : in out Collection;
                     Elem : Item; Before : Positive) is
   begin
      Collections.Insert (Collections.Collection (C), Elem, Before);
   end Insert;

   procedure Append (C : in out Collection; Elem : Item) is
   begin
      Collections.Append (Collections.Collection (C), Elem);
   end Append;

   procedure Append (C : in out Collection;
                     Elem : Item;
                     After : Positive) is
   begin
      Collections.Append (Collections.Collection (C), Elem, After);
   end Append;

   procedure Remove (C : in out Collection;
                     At_Index : Positive) is
   begin
      Collections.Remove (Collections.Collection (C), At_Index);
   end Remove;

   procedure Replace (C : in out Collection;
                      At_Index : Positive; Elem : Item) is
   begin
      Collections.Replace (Collections.Collection (C), At_Index, Elem);
   end Replace;

   function Length (C : Collection) return Natural is
   begin
      return Collections.Length (Collections.Collection (C));
   end Length;

   function Is_Empty (C : Collection) return Boolean is
   begin
      return Collections.Is_Empty (Collections.Collection (C));
   end Is_Empty;

   function First (C : Collection) return Item is
   begin
      return Collections.First (Collections.Collection (C));
   end First;

   function Last (C : Collection) return Item is
   begin
      return Collections.Last (Collections.Collection (C));
   end Last;

   function Item_At (C : Collection;
                     At_Index : Positive) return Item is
   begin
      return Collections.Item_At (Collections.Collection (C), At_Index);
   end Item_At;

   function Location (C : Collection;
                      Elem : Item) return Natural is
   begin
      return Collections.Location (Collections.Collection (C), Elem);
   end Location;

   function New_Iterator
     (For_The_Collection : Collection) return Iterator'Class is
   begin
      return Collections.New_Iterator
        (Collections.Collection (For_The_Collection));
   end New_Iterator;

   procedure Reset (It : in out Iterator) is
   begin
      Abstract_Containers.Reset (It);
   end Reset;

   procedure Next (It : in out Iterator) is
   begin
      Abstract_Containers.Next (It);
   end Next;

   function Is_Done (It : Iterator) return Boolean is
   begin
      return Abstract_Containers.Is_Done (It);
   end Is_Done;

   function Current_Item (It : Iterator) return Item is
   begin
      return Abstract_Containers.Current_Item (It);
   end Current_Item;

end BC.Simple_Collections;
