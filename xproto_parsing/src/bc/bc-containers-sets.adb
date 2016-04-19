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

package body BC.Containers.Sets is

   function Are_Equal (L, R : Abstract_Set'Class) return Boolean is
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
         Next (It);
      end loop;
      return True;
   end Are_Equal;

   procedure Union (S : in out Abstract_Set'Class; O : Abstract_Set'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if not Is_Member (S, This_Item) then
               Add (S, This_Item);
            end if;
         end;
         Next (It);
      end loop;
   end Union;

   procedure Intersection (S : in out Abstract_Set'Class;
                           O : Abstract_Set'Class) is
      Tmp : constant Abstract_Set'Class := S;
      It : Iterator'Class := New_Iterator (Tmp);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if not Is_Member (O, This_Item) then
               Remove (S, This_Item);
            end if;
            Next (It);
         end;
      end loop;
   end Intersection;

   procedure Difference (S : in out Abstract_Set'Class;
                         O : Abstract_Set'Class) is
      It : Iterator'Class := New_Iterator (O);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      while not Is_Done (It) loop
         declare
            This_Item : Item renames Current_Item (It);
         begin
            if Is_Member (S, This_Item) then
               Remove (S, This_Item);
            end if;
         end;
         Next (It);
      end loop;
   end Difference;

   function Is_Subset (S : Abstract_Set'Class;
                       O : Abstract_Set'Class) return Boolean is
      It : Iterator'Class := New_Iterator (S);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (S) > Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (O, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Is_Subset;

   function Is_Proper_Subset (S : Abstract_Set'Class;
                              O : Abstract_Set'Class) return Boolean is
      It : Iterator'Class := New_Iterator (S);
   begin
      --  XXX left out the optimisation which checks whether L, R are
      --  identical.
      if Extent (S) >= Extent (O) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Member (O, Current_Item (It)) then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Is_Proper_Subset;

   function Available (S : in Abstract_Set) return Natural is
      pragma Warnings (Off, S);
   begin
      return Natural'Last;
   end Available;

end BC.Containers.Sets;
