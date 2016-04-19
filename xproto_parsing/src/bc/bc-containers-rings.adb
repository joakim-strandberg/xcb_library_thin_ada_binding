--  Copyright 1994 Grady Booch
--  Copyright 1998-2006 Simon Wright <simon@pushface.org>
--  Included Chris Henrich patch of 20061209.

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

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $

with System;

package body BC.Containers.Rings is

   function Are_Equal (Left, Right : Abstract_Ring'Class) return Boolean is
   begin
      if System."=" (Left'Address, Right'Address) then
         return True;
      end if;
      if Extent (Left) /= Extent (Right) then
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

   procedure Copy (From : Abstract_Ring'Class;
                   To : in out Abstract_Ring'Class) is
      Iter : Iterator'Class := New_Iterator (From);
   begin
      if System."/=" (From'Address, To'Address) then
         Clear (To);
         Reset (Iter);
         while not Is_Done (Iter) loop
            Add (To, Current_Item (Iter));
            Next (Iter);
         end loop;
         To.Top := From.Top;
         To.Mark := From.Mark;
         if Extent (To) > 0 then
            --  cjh patch
            if To.Mark >= To.Top then
               To.Mark := To.Mark - To.Top + 1;
            else
               To.Mark := To.Mark + Extent (To) - To.Top + 1;
            end if;
            To.Top := 1;
         end if;
      end if;
   end Copy;

   procedure Mark (R : in out Abstract_Ring) is
   begin
      R.Mark := R.Top;
   end Mark;

   procedure Rotate (R : in out Abstract_Ring; Dir : Direction := Forward) is
   begin
      if Dir = Forward then
         R.Top := R.Top + 1;
         if R.Top > Extent (Abstract_Ring'Class (R)) then
            R.Top := 1;
         end if;
      else
         if R.Top = 1 then
            R.Top := Extent (Abstract_Ring'Class (R));
         else
            R.Top := R.Top - 1;
         end if;
      end if;
   end Rotate;

   procedure Rotate_To_Mark (R : in out Abstract_Ring) is
   begin
      R.Top := R.Mark;
   end Rotate_To_Mark;

   function Available (R : in Abstract_Ring) return Natural is
      pragma Warnings (Off, R);
   begin
      return Natural'Last;
   end Available;

   function At_Mark (R : Abstract_Ring) return Boolean is
   begin
      return R.Mark = R.Top;
   end At_Mark;

   procedure Add (R : in out Abstract_Ring; Elem : Item) is
   begin
      raise Should_Have_Been_Overridden;
   end Add;

   procedure Reset (It : in out Ring_Iterator) is
      R : Abstract_Ring'Class
        renames Abstract_Ring'Class (It.For_The_Container.all);
   begin
      if Extent (R) = 0 then
         It.Index := 0;
      else
         It.Index := 1;
      end if;
   end Reset;

   function Is_Done (It : Ring_Iterator) return Boolean is
      R : Abstract_Ring'Class
     renames Abstract_Ring'Class (It.For_The_Container.all);
   begin
      return It.Index = 0 or else It.Index > Extent (R);
   end Is_Done;

   procedure Next (It : in out Ring_Iterator) is
   begin
      It.Index := It.Index + 1;
   end Next;

   function Current_Item (It : Ring_Iterator) return Item is
      R : Abstract_Ring'Class
     renames Abstract_Ring'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      declare
         Size : constant Positive := Extent (R);
         I : Positive;
      begin
         I := It.Index + R.Top - 1;
         if I > Size then
            I := I - Size;
         end if;
         return Item_At (R, I).all;
      end;
   end Current_Item;

   function Current_Item_Ptr (It : Ring_Iterator) return Item_Ptr is
      R : Abstract_Ring'Class
     renames Abstract_Ring'Class (It.For_The_Container.all);
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      declare
         Size : constant Positive := Extent (R);
         I : Positive;
      begin
         I := It.Index + R.Top - 1;
         if I > Size then
            I := I - Size;
         end if;
         return Item_At (R, I);
      end;
   end Current_Item_Ptr;

   procedure Delete_Item_At (It : in out Ring_Iterator) is
   begin
      if Is_Done (It) then
         raise BC.Not_Found;
      end if;
      raise BC.Not_Yet_Implemented;
   end Delete_Item_At;

end BC.Containers.Rings;
