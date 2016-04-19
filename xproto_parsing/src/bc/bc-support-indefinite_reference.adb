--  Copyright (C) 2003 Martin Krischik
--  Copyright 2009 Simon Wright <simon@pushface.org>

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

--  $Revision: 1467 $
--  $Date: 2011-06-12 19:55:51 +0100 (Sun, 12 Jun 2011) $
--  $Author: simonjwright $

with Ada.Unchecked_Deallocation;

package body BC.Support.Indefinite_Reference is


   procedure Deallocate is new Ada.Unchecked_Deallocation (Object => T,
                                                           Name => P);


   function "=" (Ptr : Pointer; Value : T) return Boolean
   is
   begin
      return Ptr.Value.all = Value;
   end "=";


   function "=" (Left : Pointer; Right : Pointer) return Boolean
   is
   begin
      return Left.Value.all = Right.Value.all;
   end "=";


   procedure Adjust (Obj : in out Pointer)
   is
   begin
      if Obj.Value /= null then
         Obj.Value := new T'(Obj.Value.all);
      end if;
   end Adjust;


   function Create (Value : T) return Pointer
   is
   begin
      return Pointer'(Ada.Finalization.Controlled with Value => new T'(Value));
   end Create;


   procedure Finalize (Obj : in out Pointer)
   is
   begin
      Deallocate (Obj.Value);
   end Finalize;


   procedure Read (Stream : access Ada.Streams.Root_Stream_Type'Class;
                   Item   : out Pointer)
   is
   begin
      Deallocate (Item.Value);
      Item.Value := new T'(T'Input (Stream));
   end Read;


   function Value (Ptr : Pointer) return T
   is
   begin
      return Ptr.Value.all;
   end Value;


   function Value_Access (Ptr : Pointer) return P
   is
   begin
      return Ptr.Value;
   end Value_Access;


   procedure Write (Stream : access Ada.Streams.Root_Stream_Type'Class;
                    Item   : in Pointer)
   is
   begin
      T'Output (Stream, Item.Value.all);
   end Write;


end BC.Support.Indefinite_Reference;
