--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2008 Simon Wright <simon@pushface.org>
--  Copyright 2005 Martin Krischik

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

package body BC.Indefinite_Unmanaged_Containers is

   --  Iteration support

   procedure Access_Current_Item (In_The_Iterator : Iterator'Class) is
   begin
      Apply (Current_Item_Ptr (In_The_Iterator).all);
   end Access_Current_Item;

   procedure Visit (Using : in out Iterator'Class) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Visit;

   procedure Visit_With_In_Param (Using : in out Iterator'Class;
                                  Param : Param_Type) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Param, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Visit_With_In_Param;

   procedure Visit_With_In_Out_Param (Using : in out Iterator'Class;
                                      Param : in out Param_Type) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Param, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Visit_With_In_Out_Param;

   procedure Modify (Using : in out Iterator'Class) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Modify;

   procedure Modify_With_In_Param (Using : in out Iterator'Class;
                                   Param : in Param_Type) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Param, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Modify_With_In_Param;

   procedure Modify_With_In_Out_Param (Using : in out Iterator'Class;
                                       Param : in out Param_Type) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Item_Ptr (Using).all, Param, Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Modify_With_In_Out_Param;

   --  Primitive implementations

   function Item_At (C : Container; Index : Positive) return Item_Ptr is
   begin
      raise Should_Have_Been_Overridden;
      return null;
   end Item_At;

   function Current_Item_Ptr (It : Iterator) return Item_Ptr is
   begin
      raise Should_Have_Been_Overridden;
      return null;
   end Current_Item_Ptr;

end BC.Indefinite_Unmanaged_Containers;
