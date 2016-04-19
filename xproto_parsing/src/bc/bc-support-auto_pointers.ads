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

--  $Revision: 1420 $
--  $Date: 2009-09-26 18:42:21 +0100 (Sat, 26 Sep 2009) $
--  $Author: simonjwright $

with Ada.Finalization;

generic
   type T (<>) is limited private;
   type P is access T;
package BC.Support.Auto_Pointers is

   pragma Preelaborate;

   type Pointer is private;
   --  A Pointer variable encapsulates an accessor of type P (to a T).
   --
   --  When a Pointer is assigned, ownership of the accessor and of
   --  the T it designates is transferred.
   --
   --  When a Pointer that owns an accessor is finalized, the accessor
   --  is deallocated.

   function Create (Value : P) return Pointer;
   --  Returns a new encapsulation. You must NOT deallocate the Value
   --  passed; it will be deallocated when an owning Pointer is
   --  finalized.

   function Value (Ptr : Pointer) return P;
   pragma Inline (Value);
   --  Returns the encapsulated pointer, which will be null if
   --  ownership has been assigned away.

private

   type Owner;
   type Owner_P is access all Owner;

   type Object is record
      The_Owner : Owner_P;
      The_P : P;
   end record;
   type Object_P is access Object;

   type Owner is record
      The_Object : Object_P;
   end record;

   type Pointer is new Ada.Finalization.Controlled with record
      The_Owner : aliased Owner;
   end record;

   procedure Adjust (Obj : in out Pointer);
   procedure Finalize (Obj : in out Pointer);

end BC.Support.Auto_Pointers;
