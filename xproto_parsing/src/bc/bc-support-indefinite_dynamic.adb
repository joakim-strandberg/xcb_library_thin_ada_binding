--  Copyright 1994 Grady Booch
--  Copyright 2005 Martin Krischik
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2005 Simon Wright <simon@pushface.org>

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

package body BC.Support.Indefinite_Dynamic is

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.

   procedure Delete_Arr is
      new Ada.Unchecked_Deallocation (Dyn_Arr, Dyn_Arr_Ref);

   function "=" (Left, Right : Dyn_Node) return Boolean is
   begin
      if Left.Size /= Right.Size then
         return False;
      else
         --  We have to compare element-by-element; LRM 4.5.2(24)
         for I in 1 .. Left.Size loop
            if Smart.Value (Left.Ref (I)) /= Smart.Value (Right.Ref (I)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   procedure Clear (Obj : in out Dyn_Node) is
   begin
      Delete_Arr (Obj.Ref);
      Preallocate (Obj);
      Obj.Size := 0;
   end Clear;

   procedure Extend (Obj : in out Dyn_Node) is
      Temp : Dyn_Arr_Ref;
   begin
      Temp := new Dyn_Arr (1 .. Obj.Ref'Last + Obj.Chunk_Size);
      Temp (1 .. Obj.Size) := Obj.Ref (1 .. Obj.Size);
      Delete_Arr (Obj.Ref);
      Obj.Ref := Temp;
   end Extend;

   procedure Insert (Obj : in out Dyn_Node; Elem : Item) is
   begin
      if Obj.Size = Obj.Ref'Last then
         Extend (Obj);
      end if;
      Obj.Ref (2 .. Obj.Size + 1) := Obj.Ref (1 .. Obj.Size);
      Obj.Ref (1) := Smart.Create (Value => Elem);
      Obj.Size := Obj.Size + 1;
   end Insert;

   procedure Insert (Obj : in out Dyn_Node; Elem : Item; Before : Positive) is
   begin
      if Before > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 or else Before = 1 then
         Insert (Obj, Elem);
      else
         if Obj.Size = Obj.Ref'Last then
            Extend (Obj);
         end if;
         Obj.Ref (Before + 1 .. Obj.Size + 1) := Obj.Ref (Before .. Obj.Size);
         Obj.Ref (Before) := Smart.Create (Value => Elem);
         Obj.Size := Obj.Size + 1;
      end if;
   end Insert;

   procedure Append (Obj : in out Dyn_Node; Elem : Item) is
   begin
      if Obj.Size >= Obj.Ref'Last then
         Extend (Obj);
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Ref (Obj.Size) := Smart.Create (Value => Elem);
   end Append;

   procedure Append (Obj : in out Dyn_Node; Elem : Item; After : Positive) is
   begin
      if After > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = Obj.Ref'Last then
         Extend (Obj);
      end if;
      if After = Obj.Size then
         Obj.Size := Obj.Size + 1;
         Obj.Ref (Obj.Size) := Smart.Create (Value => Elem);
      else
         Obj.Ref (After + 2 .. Obj.Size + 1) :=
           Obj.Ref (After + 1 .. Obj.Size);
         Obj.Size := Obj.Size + 1;
         Obj.Ref (After + 1) := Smart.Create (Value => Elem);
      end if;
   end Append;

   procedure Remove (Obj : in out Dyn_Node; From : Positive) is
   begin
      if From > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      if Obj.Size = 1 then
         Clear (Obj);
      else
         Obj.Ref (From .. Obj.Size - 1) := Obj.Ref (From + 1 .. Obj.Size);
         Obj.Size := Obj.Size - 1;
      end if;
   end Remove;

   procedure Replace (Obj : in out Dyn_Node; Index : Positive; Elem : Item) is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      Obj.Ref (Index) := Smart.Create (Value => Elem);
   end Replace;

   function Length (Obj : Dyn_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First (Obj : Dyn_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Smart.Value (Obj.Ref (1));
   end First;

   function Last (Obj : Dyn_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Smart.Value (Obj.Ref (Obj.Size));
   end Last;

   function Item_At (Obj : Dyn_Node; Index : Positive) return Item is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      return Smart.Value (Obj.Ref (Index));
   end Item_At;

   function Item_At (Obj : Dyn_Node; Index : Positive) return Item_Ptr is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      return Smart.Value_Access (Obj.Ref (Index));
   end Item_At;

   function Location (Obj : Dyn_Node; Elem : Item; Start : Positive := 1)
                     return Natural is
   begin
      --  XXX the C++ (which indexes from 0) nevertheless checks
      --  "start <= count". We have to special-case the empty Node; the
      --  C++ indexes from 0, so it can legally start with index 0
      --  when the Node is empty.
      if Obj.Size = 0 then
         return 0;
      end if;
      if Start > Obj.Size then
         raise BC.Range_Error;
      end if;
      for I in Start .. Obj.Size loop
         if Smart.Value (Obj.Ref (I)) = Elem then
            return I;
         end if;
      end loop;
      return 0;  -- Not located
   end Location;

   procedure Preallocate (Obj : in out Dyn_Node;
                          New_Length : Natural := Initial_Size) is
      Temp : Dyn_Arr_Ref;
      Last : Natural;
   begin
      --  XXX I don't think this algorithm is very clever! we really
      --  shouldn't have to allocate a temporary and then delete it ..
      if Obj.Ref /= null then
         Temp := new Dyn_Arr (1 .. Obj.Ref'Last);
         Temp (1 .. Obj.Ref'Last) := Obj.Ref.all;
         Last := Obj.Ref'Last;
         Delete_Arr (Obj.Ref);
      else
         Last := 0;
      end if;
      Obj.Ref := new Dyn_Arr (1 .. Last + New_Length);
      if Last /= 0 then -- something was in the array already
         Obj.Ref (1 .. Obj.Size) := Temp (1 .. Obj.Size);
         Delete_Arr (Temp);
      end if;
   end Preallocate;

   procedure Set_Chunk_Size (Obj : in out Dyn_Node; Size : Natural) is
   begin
      Obj.Chunk_Size := Size;
   end Set_Chunk_Size;

   function Chunk_Size (Obj : in Dyn_Node) return Natural is
   begin
      return Obj.Chunk_Size;
   end Chunk_Size;

   procedure Initialize (D : in out Dyn_Node) is
   begin
      D.Ref := new Dyn_Arr (1 .. Initial_Size);
      D.Size := 0;
      D.Chunk_Size := Initial_Size;
   end Initialize;

   procedure Adjust (D : in out Dyn_Node) is
      Tmp : constant Dyn_Arr_Ref := new Dyn_Arr (1 .. D.Ref'Last);
   begin
      Tmp (1 .. D.Size) := D.Ref (1 .. D.Size);
      D.Ref := Tmp;
   end Adjust;

   procedure Finalize (D : in out Dyn_Node) is
   begin
      if D.Ref /= null then
         Delete_Arr (D.Ref);
         D.Ref := null;
      end if;
   end Finalize;

   procedure Write_Dyn_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Dyn_Node) is
   begin
      Integer'Write (Stream, Obj.Size);
      for I in 1 .. Obj.Size loop
         Item'Output (Stream, Smart.Value (Obj.Ref (I)));
      end loop;
   end Write_Dyn_Node;

   procedure Read_Dyn_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Dyn_Node) is
      Count : Integer;
   begin
      Clear (Obj);
      Integer'Read (Stream, Count);
      for I in 1 .. Count loop
         declare
            Elem : constant Item := Item'Input (Stream);
         begin
            Append (Obj, Elem);
         end;
      end loop;
   end Read_Dyn_Node;

end BC.Support.Indefinite_Dynamic;
