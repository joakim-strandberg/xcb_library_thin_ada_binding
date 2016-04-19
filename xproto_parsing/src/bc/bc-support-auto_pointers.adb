--  Copyright 2006 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;

package body BC.Support.Auto_Pointers is


   function Create (Value : P) return Pointer is
      Result : Pointer;
   begin
      Result.The_Owner.The_Object :=
        new Object'(The_Owner => Result.The_Owner'Unchecked_Access,
                    The_P => Value);
      return Result;
   end Create;


   function Value (Ptr : Pointer) return P is
   begin
      if Ptr.The_Owner.The_Object = null then
         return null;
      else
         return Ptr.The_Owner.The_Object.The_P;
      end if;
   end Value;


   procedure Adjust (Obj : in out Pointer) is
   begin
      if Obj.The_Owner.The_Object /= null then
         declare
            The_Object : Object_P renames Obj.The_Owner.The_Object;
            The_Objects_Owner : Owner_P renames The_Object.The_Owner;
         begin
            The_Objects_Owner.The_Object := null;
            The_Objects_Owner := Obj.The_Owner'Unchecked_Access;
         end;
      end if;
   end Adjust;


   procedure Delete is new Ada.Unchecked_Deallocation (T, P);
   procedure Delete is new Ada.Unchecked_Deallocation (Object, Object_P);

   procedure Finalize (Obj : in out Pointer) is
   begin
      if Obj.The_Owner.The_Object /= null then
         Delete (Obj.The_Owner.The_Object.The_P);
         Delete (Obj.The_Owner.The_Object);
      end if;
   end Finalize;


end BC.Support.Auto_Pointers;
