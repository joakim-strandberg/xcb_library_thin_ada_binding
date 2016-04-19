--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 2005 Martin Krischik
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

--  $Revision: 1490 $
--  $Date: 2011-09-01 17:23:20 +0100 (Thu, 01 Sep 2011) $
--  $Author: simonjwright $

with System;

package body BC.Indefinite_Containers.Queues is

   procedure Process_Front (Q : in out Abstract_Queue'Class) is
   begin
      Process (Item_At (Q, 1).all);
   end Process_Front;

   procedure Copy (From : Abstract_Queue'Class;
                   To : in out Abstract_Queue'Class) is
      Iter : Iterator'Class := New_Iterator (From);
   begin
      if System."/=" (From'Address, To'Address) then
         Clear (To);
         Reset (Iter);
         while not Is_Done (Iter) loop
            Append (To, Current_Item (Iter));
            Next (Iter);
         end loop;
      end if;
   end Copy;

   function Are_Equal (Left, Right : Abstract_Queue'Class) return Boolean is
   begin
      if System."=" (Left'Address, Right'Address) then
         return True;
      end if;
      if Length (Left) /= Length (Right) then
         return False;
      end if;
      declare
         Left_Iter : Iterator'Class := New_Iterator (Left);
         Right_Iter : Iterator'Class := New_Iterator (Right);
      begin
         while not Is_Done (Left_Iter) and then
           not Is_Done (Right_Iter) loop
            if Current_Item (Left_Iter) /= Current_Item (Right_Iter) then
               return False;
            end if;
            Next (Left_Iter);
            Next (Right_Iter);
         end loop;
         return True;
      end;
   end Are_Equal;

   procedure Reset (It : in out Queue_Iterator) is
      Q : Abstract_Queue'Class
        renames Abstract_Queue'Class (It.For_The_Container.all);
   begin
      if Length (Q) = 0 then
         It.Index := 0;
      else
         It.Index := 1;
      end if;
   end Reset;

   function Available (Q : in Abstract_Queue) return Natural is
      pragma Warnings (Off, Q);
   begin
      return Natural'Last;
   end Available;

   procedure Next (It : in out Queue_Iterator) is
   begin
      It.Index := It.Index + 1;
   end Next;

   function Is_Done (It : Queue_Iterator) return Boolean is
      Q : Abstract_Queue'Class
     renames Abstract_Queue'Class (It.For_The_Container.all);
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
      Q : Abstract_Queue'Class
        renames Abstract_Queue'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      Remove (Q, It.Index);
   end Delete_Item_At;

end BC.Indefinite_Containers.Queues;
