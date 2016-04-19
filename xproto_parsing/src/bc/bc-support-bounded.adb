--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2010 Simon Wright <simon@pushface.org>

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

--  $Revision: 1447 $
--  $Date: 2011-01-17 22:46:37 +0000 (Mon, 17 Jan 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Support.Bounded is

   --  We can't take 'Access of non-aliased components. But if we
   --  alias discriminated objects they become constrained - even if
   --  the discriminant has a default.
   package Allow_Element_Access
   is new System.Address_To_Access_Conversions (Item);

   procedure Clear (Obj : in out Bnd_Node) is
   begin
      Obj.Start := 1;
      Obj.Size := 0;
   end Clear;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item) is
   begin
      if Obj.Size >= Obj.Maximum_Size then
         raise BC.Overflow;
      end if;
      Obj.Start := ((Obj.Start - 2) mod Obj.Maximum_Size) + 1;
      Obj.Size := Obj.Size + 1;
      Obj.Elems (Obj.Start) := Elem;
   end Insert;

   procedure Insert (Obj : in out Bnd_Node; Elem : Item; Before : Positive) is
   begin
      if Before > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size >= Obj.Maximum_Size then
         raise BC.Overflow;
      end if;
      if Obj.Size = 0 or else Before = 1 then
         Insert (Obj, Elem);
      else
         --  We are inserting in the middle.
         --
         --  In the comments below, 'left' means the part of Elems
         --  before the element which the new entry is to be inserted
         --  before (indexed by Actual), 'right' means the part after.
         declare
            Start : Elem_Range renames Obj.Start;
            Actual : constant Elem_Range
              := ((Start - 1 + Before - 1) mod Obj.Maximum_Size) + 1;
            Last : constant Elem_Range
              := ((Start - 1 + Obj.Size - 1) mod Obj.Maximum_Size) + 1;
         begin
            if Start = 1 or else Start > Actual then
               --  the left part is wedged, shift the right part up
               Obj.Elems (Actual + 1 .. Last + 1)
                 := Obj.Elems (Actual .. Last);
               Obj.Elems (Actual) := Elem;
            elsif Last = Obj.Elems'Last or else Last < Actual then
               --  the right part is wedged, shift the left part down
               Obj.Elems (Start - 1 .. Actual - 2)
                 := Obj.Elems (Start .. Actual - 1);
               Start := Start - 1;
               Obj.Elems (Actual - 1) := Elem;
            elsif Before < Obj.Size / 2 then
               --  the left part is shorter, shift it down
               Obj.Elems (Start - 1 .. Actual - 2)
                 := Obj.Elems (Start .. Actual - 1);
               Start := Start - 1;
               Obj.Elems (Actual - 1) := Elem;
            else
               --  the right part is shorter, shift it up
               Obj.Elems (Actual + 1 .. Last + 1)
                 := Obj.Elems (Actual .. Last);
               Obj.Elems (Actual) := Elem;
            end if;
            Obj.Size := Obj.Size + 1;
         end;
      end if;
   end Insert;

   procedure Append (Obj : in out Bnd_Node; Elem : Item) is
   begin
      if Obj.Size >= Obj.Maximum_Size then
         raise BC.Overflow;
      end if;
      Obj.Size := Obj.Size + 1;
      Obj.Elems (((Obj.Start - 1 + Obj.Size - 1) mod Obj.Maximum_Size) + 1)
        := Elem;
   end Append;

   procedure Append (Obj : in out Bnd_Node; Elem : Item; After : Positive) is
   begin
      if After > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size >= Obj.Maximum_Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 0 or else After = Obj.Size then
         Append (Obj, Elem);
      else
         Insert (Obj, Elem, Before => After + 1);
      end if;
   end Append;

   procedure Remove (Obj : in out Bnd_Node; From : Positive) is
   begin
      if From > Obj.Size then
         raise BC.Range_Error;
      end if;
      if Obj.Size = 1 then
         Clear (Obj);
      elsif From = 1 then
         Obj.Start := (Obj.Start mod Obj.Maximum_Size) + 1;
         Obj.Size := Obj.Size - 1;
      elsif From = Obj.Size then
         Obj.Size := Obj.Size - 1;
      else
         --  We are removing from the middle.
         --
         --  In the comments below, 'left' means the part of Elems
         --  before the element to be removed (indexed by Actual),
         --  'right' means the part after.
         declare
            Start : Elem_Range renames Obj.Start;
            Actual : constant Elem_Range
              := ((Start - 1 + From - 1) mod Obj.Maximum_Size) + 1;
            Last : constant Elem_Range
              := ((Start - 1 + Obj.Size - 1) mod Obj.Maximum_Size) + 1;
         begin
            if Start > Actual then
               --  the left part wraps round; shift the right part down
               Obj.Elems (Actual .. Last - 1)
                 := Obj.Elems (Actual + 1 .. Last);
            elsif Actual > Last then
               --  the right part wraps round; shift the left part up
               Obj.Elems (Start + 1 .. Actual)
                 := Obj.Elems (Start .. Actual - 1);
               Start := Start + 1;
            elsif Obj.Maximum_Size > 1 and then From < Obj.Size / 2 then
               --  the left part is shorter
               Obj.Elems (Start + 1 .. Actual)
                 := Obj.Elems (Start .. Actual - 1);
               Start := Start + 1;
            else
               --  the right part is shorter
               Obj.Elems (Actual .. Last - 1)
                 := Obj.Elems (Actual + 1 .. Last);
            end if;
            Obj.Size := Obj.Size - 1;
         end;
      end if;
   end Remove;

   procedure Replace (Obj : in out Bnd_Node; Index : Positive; Elem : Item) is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      Obj.Elems (((Obj.Start - 1 + Index - 1) mod Obj.Maximum_Size) + 1)
        := Elem;
   end Replace;

   function Available (Obj : Bnd_Node) return Natural is
   begin
      return Obj.Maximum_Size - Obj.Size;
   end Available;

   function Length (Obj : Bnd_Node) return Natural is
   begin
      return Obj.Size;
   end Length;

   function First (Obj : Bnd_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Obj.Elems (Obj.Start);
   end First;

   function Last (Obj : Bnd_Node) return Item is
   begin
      if Obj.Size = 0 then
         raise BC.Underflow;
      end if;
      return Obj.Elems
        (((Obj.Start - 1 + Obj.Size - 1) mod Obj.Maximum_Size) + 1);
   end Last;

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item is
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      return Obj.Elems
        (((Obj.Start - 1 + Index - 1) mod Obj.Maximum_Size) + 1);
   end Item_At;

   function Item_At (Obj : Bnd_Node; Index : Positive) return Item_Ptr is
      --  We can't take 'Access of components of constant (in parameter)
      --  objects; but we need to be able to do this so that we can
      --  return pointers to individual elements. This technique is due
      --  to Matthew Heaney.
   begin
      if Index > Obj.Size then
         raise BC.Range_Error;
      end if;
      return Item_Ptr
        (Allow_Element_Access.To_Pointer
           (Obj.Elems (((Obj.Start - 1 + Index - 1) mod Obj.Maximum_Size) + 1)
            'Address));
   end Item_At;

   function Location (Obj : Bnd_Node;
                      Elem : Item;
                      Start : Positive := 1) return Natural is
   begin
      if Obj.Size = 0 then
         return 0;
      end if;
      if Start > Obj.Size then
         raise BC.Range_Error;
      end if;
      for I in Start .. Obj.Size loop
         if Obj.Elems (((Obj.Start - 1 + I - 1) mod Obj.Maximum_Size) + 1)
           = Elem then
            return I;
         end if;
      end loop;
      return 0;
   end Location;

   --  OA wants this to be after Item_At, so it can inline it.
   function "=" (Left, Right : Bnd_Node) return Boolean is
   begin
      if Left.Size /= Right.Size then
         return False;
      else
         for I in 1 .. Left.Size loop
            if Item'(Item_At (Left, I)) /= Item'(Item_At (Right, I)) then
               return False;
            end if;
         end loop;
         return True;
      end if;
   end "=";

   procedure Write_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : Bnd_Node) is
   begin
      Integer'Write (Stream, Obj.Maximum_Size);
      Integer'Write (Stream, Obj.Size);
      for I in 1 .. Obj.Size  loop
         Item'Output (Stream, Item_At (Obj, I));
      end loop;
   end Write_Bnd_Node;

   procedure Read_Bnd_Node
     (Stream : access Ada.Streams.Root_Stream_Type'Class;
      Obj : out Bnd_Node) is
      Count : Integer;
   begin
      Integer'Read (Stream, Count);
      Clear (Obj);
      Integer'Read (Stream, Count);
      for I in 1 .. Count loop
         Append (Obj, Item'Input (Stream));
      end loop;
   end Read_Bnd_Node;

end BC.Support.Bounded;
