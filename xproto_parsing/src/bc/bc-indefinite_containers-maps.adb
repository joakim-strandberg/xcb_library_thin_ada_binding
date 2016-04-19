--  Copyright 1994 Grady Booch
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>

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

--  $Revision: 1449 $
--  $Date: 2011-01-17 22:52:47 +0000 (Mon, 17 Jan 2011) $
--  $Author: simonjwright $

with System;

package body BC.Indefinite_Containers.Maps is


   function Are_Equal (L, R : Abstract_Map'Class) return Boolean is
      It : Map_Iterator'Class := Map_Iterator'Class (New_Iterator (L));
   begin
      if System."=" (L'Address, R'Address) then
         return True;
      end if;
      if Extent (L) /= Extent (R) then
         return False;
      end if;
      while not Is_Done (It) loop
         if not Is_Bound (R, Current_Key (It))
           or else Item_Of (L, Current_Key (It))
                      /= Item_Of (R, Current_Key (It))
         then
            return False;
         end if;
         Next (It);
      end loop;
      return True;
   end Are_Equal;


   function Available (M : Abstract_Map) return Natural is
      pragma Warnings (Off, M);
   begin
      return Natural'Last;
   end Available;


   procedure Visit (Using : in out Map_Iterator'Class) is
      Status : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Key (Using),
                Current_Item (Using),
                Status);
         exit when not Status;
         Next (Using);
      end loop;
   end Visit;


   procedure Modify (Using : in out Map_Iterator'Class) is
      Status : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Key (Using),
                Current_Item_Ptr (Using).all,
                Status);
         exit when not Status;
         Next (Using);
      end loop;
   end Modify;


end BC.Indefinite_Containers.Maps;
