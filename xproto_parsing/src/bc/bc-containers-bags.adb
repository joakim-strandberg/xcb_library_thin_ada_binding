--  Copyright 1994 Grady Booch
--  Copyright 1998-2002 Simon Wright <simon@pushface.org>

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

package body BC.Containers.Bags is

   function Are_Equal (L, R : Abstract_Bag'Class) return Boolean is
      It : Iterator'Class := New_Iterator (L);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (L) /= Extent (R) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (R, Current_Item (It)) then
            return False;
         end if;
         if Count (L, Current_Item (It)) /= Count (R, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Are_Equal;

   procedure Add (B : in out Abstract_Bag'Class; I : Item) is
      Dummy : Boolean;
   begin
      Add (B, I, Added => Dummy);
   end Add;

   procedure Union (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
            This_Count : constant Positive := Count (O, This_Item);
         begin
            if not Is_Member (B, This_Item) then
               Attach (B, This_Item, This_Count);
            else
               Set_Value (B, This_Item, Count (B, This_Item) + This_Count);
            end if;
         end;
         Next (It);
      end loop;
   end Union;

   procedure Intersection
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
      Tmp : constant Abstract_Bag'Class := B;
      It : Iterator'Class := New_Iterator (Tmp);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
            B_Count : constant Positive := Count (B, This_Item);
         begin
            if not Is_Member (O, This_Item) then
               Detach (B, This_Item);
            else
               declare
                  O_Count : constant Positive := Count (O, This_Item);
               begin
                  if B_Count > O_Count then
                     Set_Value (B, This_Item, O_Count);
                  end if;
               end;
            end if;
            Next (It);
         end;
      end loop;
   end Intersection;

   procedure Difference
     (B : in out Abstract_Bag'Class; O : Abstract_Bag'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if Is_Member (B, This_Item) then
               declare
                  B_Count : constant Positive := Count (B, This_Item);
                  O_Count : constant Positive := Count (O, This_Item);
               begin
                  if B_Count <= O_Count then
                     Detach (B, This_Item);
                  else
                     Set_Value (B, This_Item, B_Count - O_Count);
                  end if;
               end;
            end if;
         end;
         Next (It);
      end loop;
   end Difference;

   function Total_Size (B : Abstract_Bag'Class) return Natural is
      It : Iterator'Class := New_Iterator (B);
      Result : Natural := 0;
   begin
      while not Is_Done (It) loop
         Result := Result + Count (B, Current_Item (It));
         Next (It);
      end loop;
      return Result;
   end Total_Size;

   function Is_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean is
      It : Iterator'Class := New_Iterator (B);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (B) > Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         declare
            This_Item : constant Item := Current_Item (It);
         begin
            --  why don't I just do "or else Count (B, This_Item) >
            --  Count (O, This_Item)"? .. because it triggered a
            --  compiler bug in GNAT 3.11p (or was it 3.11b2?)
            if not Is_Member (O, This_Item) then
               return False;
            else
               declare
                  B_Count : constant Positive := Count (B, This_Item);
                  O_Count : constant Positive := Count (O, This_Item);
               begin
                  if B_Count > O_Count then
                     return False;
                  end if;
               end;
            end if;
         end;
         Next (It);
      end loop;
      return True;
   end Is_Subset;

   function Is_Proper_Subset
     (B : Abstract_Bag'Class; O : Abstract_Bag'Class) return Boolean is
      It : Iterator'Class := New_Iterator (B);
      Is_Proper : Boolean := False;
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (B) > Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if not Is_Member (O, This_Item) then
               return False;
            else
               declare
                  B_Count : constant Positive := Count (B, This_Item);
                  O_Count : constant Positive := Count (O, This_Item);
               begin
                  if B_Count > O_Count then
                     return False;
                  elsif B_Count < O_Count then
                     Is_Proper := True;
                  end if;
               end;
            end if;
         end;
         Next (It);
      end loop;
      return Is_Proper or else Extent (B) < Extent (O);
   end Is_Proper_Subset;

   function Available (B : Abstract_Bag) return Natural is
      pragma Warnings (Off, B);
   begin
      return Natural'Last;
   end Available;


   --  Subprograms to be overridden

   procedure Attach (B : in out Abstract_Bag; I : Item; C : Positive) is
   begin
      raise Should_Have_Been_Overridden;
   end Attach;

   procedure Detach (B : in out Abstract_Bag; I : Item) is
   begin
      raise Should_Have_Been_Overridden;
   end Detach;

   procedure Set_Value (B : in out Abstract_Bag; I : Item; C : Positive) is
   begin
      raise Should_Have_Been_Overridden;
   end Set_Value;

end BC.Containers.Bags;
