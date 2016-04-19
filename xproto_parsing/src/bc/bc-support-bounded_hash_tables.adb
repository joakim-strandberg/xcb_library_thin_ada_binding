--  Copyright 2001-2003 Simon Wright <simon@pushface.org>

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

--  $Revision: 1456 $
--  $Date: 2011-03-11 22:04:19 +0000 (Fri, 11 Mar 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Support.Bounded_Hash_Tables is


   package body Tables is


      pragma Warnings (Off);
      --  for GNAT: style checks require a specification, but the
      --  operation can't be dispatching.
      function Location
        (T : Table; Start : Index; I : Items.Item) return Index;
      pragma Warnings (On);

      function Location
        (T : Table; Start : Index; I : Items.Item) return Index is
         Result : Index := Start;
      begin
         while Result /= 0 loop
            if Items.Eq (T.Contents (Result).Item, I) then
               return Result;
            end if;
            Result := T.Contents (Result).Next;
         end loop;
         return 0;
      end Location;


      procedure Initialize (T : in out Table) is
      begin
         Clear (T);
      end Initialize;


      function "=" (L, R : Table) return Boolean is
      begin
         if System."=" (L'Address, R'Address) then
            return True;
         end if;
         if L.Size = R.Size then
            for B in L.Buckets'Range loop
               declare
                  I : Index := L.Buckets (B);
               begin
                  while I > 0 loop
                     declare
                        C : constant Cell := L.Contents (I);
                     begin
                        if not Is_Bound (R, C.Item)
                          or else not Values.Eq (C.Value, Value_Of (R, C.Item))
                        then
                           return False;
                        end if;
                        I := C.Next;
                     end;
                  end loop;
               end;
            end loop;
            return True;
         else
            return False;
         end if;
      end "=";


      procedure Clear (T : in out Table) is
      begin
         T.Buckets := (others => 0);
         for C in T.Contents'First .. T.Contents'Last - 1 loop
            T.Contents (C).Next := C + 1;
         end loop;
         T.Contents (T.Contents'Last).Next := 0;
         T.Free := T.Contents'First;
         T.Size := 0;
      end Clear;


      procedure Bind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         B : Index renames T.Buckets (Bucket);
      begin
         if Location (T, B, I) /= 0 then
            raise BC.Duplicate;
         end if;
         if T.Size >= T.Maximum_Size then
            raise BC.Overflow;
         end if;
         declare
            C : Cell renames T.Contents (T.Free);
            Next : constant Index := C.Next;
         begin
            C := (Item => I,
                  Value => V,
                  Next => B);
            B := T.Free;
            T.Free := Next;
         end;
         T.Size := T.Size + 1;
      end Bind;


      procedure Rebind (T : in out Table; I : Items.Item; V : Values.Value) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         C : constant Index := Location (T, T.Buckets (Bucket), I);
      begin
         if C = 0 then
            raise BC.Not_Found;
         end if;
         T.Contents (C).Value := V;
      end Rebind;


      procedure Unbind (T : in out Table; I : Items.Item) is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         Current : Index := T.Buckets (Bucket);
         Previous : Index := 0;
      begin
         loop
            exit when Current = 0;
            exit when Items.Eq (T.Contents (Current).Item, I);
            Previous := Current;
            Current := T.Contents (Current).Next;
         end loop;
         if Current = 0 then
            raise BC.Not_Found;
         end if;
         if Previous = 0 then
            T.Buckets (Bucket) := T.Contents (Current).Next;
         else
            T.Contents (Previous).Next := T.Contents (Current).Next;
         end if;
         T.Contents (Current).Next := T.Free;
         T.Free := Current;
         T.Size := T.Size - 1;
      end Unbind;


      function Extent (T : Table) return Natural is
      begin
         return T.Size;
      end Extent;


      function Bucket_Extent
        (T : Table; Bucket : Bucket_Index) return Natural is
         Current : Index := T.Buckets (Bucket);
         Result : Natural := 0;
      begin
         while Current /= 0 loop
            Result := Result + 1;
            Current := T.Contents (Current).Next;
         end loop;
         return Result;
      end Bucket_Extent;


      function Is_Bound (T : Table; I : Items.Item) return Boolean is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
      begin
         return Location (T, T.Buckets (Bucket), I) /= 0;
      end Is_Bound;


      function Value_Of (T : Table; I : Items.Item) return Values.Value is
         Bucket : constant Positive
           := (Items.Hash (I) mod T.Number_Of_Buckets) + 1;
         C : constant Index := Location (T, T.Buckets (Bucket), I);
      begin
         if C = 0 then
            raise BC.Not_Found;
         end if;
         return T.Contents (C).Value;
      end Value_Of;


      --  We can't take 'Access of non-aliased components. But if we
      --  alias discriminated objects they become constrained - even
      --  if the discriminant has a default.
      package Allow_Item_Access
      is new System.Address_To_Access_Conversions (Items.Item);

      function Access_Item_At (T : Table; Position : Cell_Index)
                              return Items.Item_Ptr is
      begin
         return Items.Item_Ptr
           (Allow_Item_Access.To_Pointer
              (T.Contents (Position).Item'Address));
      end Access_Item_At;


      --  We can't take 'Access of non-aliased components. But if we
      --  alias discriminated objects they become constrained - even
      --  if the discriminant has a default.
      package Allow_Value_Access
      is new System.Address_To_Access_Conversions (Values.Value);

      function Access_Value_At (T : Table; Position : Cell_Index)
                               return Values.Value_Ptr is
      begin
         return Values.Value_Ptr
           (Allow_Value_Access.To_Pointer
              (T.Contents (Position).Value'Address));
      end Access_Value_At;


      procedure Reset (T : Table;
                       Bucket : out Positive;
                       Index : out Positive) is
      begin
         if T.Size = 0 then
            Bucket := T.Number_Of_Buckets + 1;
            Index := Positive'Last;         --  we have to ensure it's > 0
         else
            Bucket := 1;
            loop
               exit when Bucket > T.Number_Of_Buckets;
               if T.Buckets (Bucket) > 0 then
                  Index := T.Buckets (Bucket);
                  return;
               end if;
               Bucket := Bucket + 1;
            end loop;
            raise Hash_Table_Error;
         end if;
      end Reset;


      function Is_Done (T : Table;
                        Bucket : Positive;
                        Index : Positive) return Boolean is
         pragma Warnings (Off, Index);
      begin
         return Bucket > T.Number_Of_Buckets;
      end Is_Done;


      function Current_Item_Ptr (T : Table;
                                 Bucket : Positive;
                                 Index : Positive) return Items.Item_Ptr is
      begin
         if Bucket > T.Number_Of_Buckets then
            raise BC.Not_Found;
         end if;
         return Items.Item_Ptr
           (Allow_Item_Access.To_Pointer
            (T.Contents (Index).Item'Address));
      end Current_Item_Ptr;


      function Current_Value_Ptr (T : Table;
                                  Bucket : Positive;
                                  Index : Positive) return Values.Value_Ptr is
      begin
         if Bucket > T.Number_Of_Buckets then
            raise BC.Not_Found;
         end if;
         return Values.Value_Ptr
           (Allow_Value_Access.To_Pointer
            (T.Contents (Index).Value'Address));
      end Current_Value_Ptr;


      procedure Delete_Item_At (T : in out Table;
                                Bucket : in out Positive;
                                Index : in out  Positive) is
         Next : Tables.Index;
         Previous : Tables.Index;
      begin
         if Bucket > T.Number_Of_Buckets then
            raise BC.Not_Found;
         end if;
         Next := T.Contents (Index).Next;
         Previous := T.Buckets (Bucket);
         if Previous = Index then
            --  This is the first cell
            T.Buckets (Bucket) := Next;
         else
            --  We have to find the previous Contents cell
            while T.Contents (Previous).Next /= Index loop
               Previous := T.Contents (Previous).Next;
            end loop;
            T.Contents (Previous).Next := Next;
         end if;
         --  Put the released cell on the free list
         T.Contents (Index).Next := T.Free;
         T.Free := Index;
         T.Size := T.Size - 1;
         --  Adjust Index
         if Next = 0 then
            --  That was the last cell in this bucket, on to the next
            loop
               Bucket := Bucket + 1;
               exit when Bucket > T.Number_Of_Buckets;  --  we've done
               if T.Buckets (Bucket) > 0 then
                  Index := T.Buckets (Bucket);
                  exit;
               end if;
            end loop;
         else
            Index := Next;
         end if;
      end Delete_Item_At;


      procedure Next (T : Table;
                      Bucket : in out Positive;
                      Index : in out  Positive) is
      begin
         if Bucket > T.Number_Of_Buckets then
            raise BC.Not_Found;
         end if;
         if T.Contents (Index).Next > 0 then
            Index := T.Contents (Index).Next;
         else
            loop
               Bucket := Bucket + 1;
               exit when Bucket > T.Number_Of_Buckets;
               if T.Buckets (Bucket) > 0 then
                  Index := T.Buckets (Bucket);
                  exit;
               end if;
            end loop;
         end if;
      end Next;


   end Tables;


end BC.Support.Bounded_Hash_Tables;
