--  Copyright (C) 2003 Martin Krischik
--  Copyright 2008-2009 Simon Wright <simon@pushface.org>

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

--  Provides storage for indefinite values.
--
--  Note, this facility differs from Smart_Pointers in that
--
--  (a) copying an Indefinite Reference produces a completely new
--  encapsulated value;
--
--  (b) the encaspulated value is accessible for modification;
--
--  (c) streaming support is provided.

with Ada.Finalization;
with Ada.Streams;

generic
   type T (<>) is private;
   type P is access T;
   with function "=" (L, R : T) return Boolean is <>;
package BC.Support.Indefinite_Reference is

   --  The Indefinite_Reference stores a single object 'Value' of the
   --  indefinite type 'T'. 'Value' is allocated on the heap
   --  P'Storage_Pool.  When the 'Pointer' is copied, a new copy of
   --  'Value' is created as well. When the 'Pointer' is destroyed
   --  'Value' is deallocated.

   pragma Preelaborate;

   --  A Pointer variable encapsulates a single instance of the
   --  indefinite type T.
   type Pointer is private;

   --  Returns a new encapsulation.
   --  'Value' is to be stored - a copy is created.
   function Create (Value : T) return Pointer;

   --  Returns a copy of the encapsulated value.
   function Value (Ptr : Pointer) return T;

   --  Returns a copy of the encapsulated pointer.
   function Value_Access (Ptr : Pointer) return P;

   --  Compare encapsulated value with supplied value.
   function "=" (Ptr : Pointer; Value : T) return Boolean;

   --  Compare two encapsulated values.
   function "=" (Left : Pointer; Right : Pointer) return Boolean;

private

   type Pointer is new Ada.Finalization.Controlled with record
      Value : P := null;
   end record;

   procedure Adjust (Obj : in out Pointer);

   procedure Finalize (Obj : in out Pointer);

   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Item   : in Pointer);

   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Pointer);

   for Pointer'Write use Write;
   for Pointer'Read use Read;

   pragma Inline (Create);
   pragma Inline (Value);
   pragma Inline (Value_Access);

end BC.Support.Indefinite_Reference;
