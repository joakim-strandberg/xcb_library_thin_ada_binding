--  Copyright 1994 Grady Booch
--  Copyright 1999 Pat Rogers
--  Copyright 1999-2009 Simon Wright <simon@pushface.org>
--  Modifications November 2006 by Christopher J. Henrich

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

--  $Revision: 1419 $
--  $Date: 2009-09-26 18:24:57 +0100 (Sat, 26 Sep 2009) $
--  $Author: simonjwright $

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

package body BC.Support.Managed_Storage is


   --  Manage chaining through the allocated elements in chunks.

   function Value_At (Location : System.Address) return System.Address;
   pragma Inline (Value_At);

   procedure Put (This : System.Address; At_Location : System.Address);
   pragma Inline (Put);


   --  Utilities.

   procedure Get_Chunk (Result : out Chunk_Pointer;
                        From : in out Pool;
                        Requested_Element_Size : SSE.Storage_Count;
                        Requested_Alignment : SSE.Storage_Count);

   function Within_Range (Target : System.Address;
                          Base : Chunk_Pointer) return Boolean;
   pragma Inline (Within_Range);

   procedure Usable_Size_And_Alignment
     (For_Size : SSE.Storage_Count;
      For_Alignment : SSE.Storage_Count;
      Is_Size : out SSE.Storage_Count;
      Is_Alignment : out SSE.Storage_Count);


   --  Constants.

   use type SSE.Storage_Count;

   Address_Size_I : constant Integer
     := System.Address'Max_Size_In_Storage_Elements;
   Address_Size_SC : constant SSE.Storage_Count
     := System.Address'Max_Size_In_Storage_Elements;


   --  Instantiations.

   procedure Dispose is
      new Ada.Unchecked_Deallocation (Chunk_List, Chunk_List_Pointer);
   procedure Dispose is
      new Ada.Unchecked_Deallocation (Chunk, Chunk_Pointer);

   package PeekPoke is
      new System.Address_To_Access_Conversions (System.Address);


   --  Bodies.

   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count)
   is

      Usable_Size : SSE.Storage_Count;
      Usable_Alignment : SSE.Storage_Count;

      List : Chunk_List_Pointer;
      Previous_List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;

      use type System.Address;

   begin

      --  Calculate the usable size and alignment.
      Usable_Size_And_Alignment (Size_In_Storage_Elements,
                                 Alignment,
                                 Usable_Size,
                                 Usable_Alignment);

      --  Look for a chunk list with the right element size and
      --  alignment, stopping when no point in continuing
      List := The_Pool.Head;
      while List /= null and then
        (List.Element_Size > Usable_Size
           or else List.Alignment > Usable_Alignment)
      loop
         Previous_List := List;
         List := List.Next_List;
      end loop;

      if List = null
        or else List.Element_Size /= Usable_Size
        or else List.Alignment /= Usable_Alignment then

         --  Need to create a new list.
         --
         --  The new list is inserted before the next list of the
         --  previous list, if any, and may become the new head.

         List := new Chunk_List;

         --  Chain the new list in
         if Previous_List /= null then

            --  There is a previous member, insert
            List.Next_List := Previous_List.Next_List;
            Previous_List.Next_List := List;

         else

            --  There was no previous member, add as head (before
            --  previous head)
            List.Next_List := The_Pool.Head;
            The_Pool.Head := List;

         end if;

         --  Store the sizing attributes
         List.Element_Size := Usable_Size;
         List.Alignment := Usable_Alignment;

      end if;

      --  List designates the correct chunk list.
      --  Find a chunk with a free element.
      Chunk := List.Head;
      while Chunk /= null
        and then Chunk.Next_Element = System.Null_Address loop
         Chunk := Chunk.Next_Chunk;
      end loop;

      if Chunk = null then

         --  There was no chunk with free elements; allocate a new one
         --  (at the head, for efficiency in future allocations).
         --
         --  Note that if Get_Chunk fails (alignment > alignment of
         --  System.Address => this request just fails to fit) we may
         --  be left with an empty List.
         begin
            Chunk := List.Head;
            Get_Chunk (List.Head, The_Pool, Usable_Size, Usable_Alignment);
            List.Head.Next_Chunk := Chunk;
            Chunk := List.Head;
            Chunk.Parent := List;
         exception
            when BC.Storage_Error =>
               if List.Head = null then
                  if The_Pool.Head = List then
                     The_Pool.Head := List.Next_List;
                  else
                     Previous_List.Next_List := List.Next_List;
                  end if;
                  Dispose (List);
               end if;
               raise;
         end;

      end if;

      Storage_Address := Chunk.Next_Element;
      Chunk.Next_Element := Value_At (Chunk.Next_Element);

   end Allocate;


   procedure Deallocate
     (The_Pool : in out Pool;
      Storage_Address : System.Address;
      Size_In_Storage_Elements : SSE.Storage_Count;
      Alignment : SSE.Storage_Count)
   is

      Usable_Size : SSE.Storage_Count;
      Usable_Alignment : SSE.Storage_Count;

      List : Chunk_List_Pointer;

   begin

      --  Calculate the usable size and alignment.
      Usable_Size_And_Alignment (Size_In_Storage_Elements,
                                 Alignment,
                                 Usable_Size,
                                 Usable_Alignment);

      --  Look for the right list
      List := The_Pool.Head;
      while List /= null and then
        (List.Element_Size /= Usable_Size
           or List.Alignment /= Usable_Alignment)
      loop
         List := List.Next_List;
      end loop;
      if List = null then
         raise Pool_Error;
      end if;

      Put (List.Head.Next_Element, At_Location => Storage_Address);
      List.Head.Next_Element := Storage_Address;
      --  Note that the effect of the above is that the "linked list"
      --  of elements will span chunks. This is necessary since
      --  Deallocate is given an address of the element, not a pointer
      --  to the containing chunk, and we don't want the overhead of
      --  the search at this time. The user should call
      --  Reclaim_Unused_Chunks at an appropriate moment.

   end Deallocate;


   function Dirty_Chunks (This : Pool) return Natural
   is
      Result : Natural := 0;
      List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;
   begin
      List := This.Head;
      while List /= null loop
         Chunk := List.Head;
         while Chunk /= null loop
            Result := Result + 1;
            Chunk := Chunk.Next_Chunk;
         end loop;
         List := List.Next_List;
      end loop;
      return Result;
   end Dirty_Chunks;


   procedure Finalize (This : in out Pool)
   is
      List, Previous_List : Chunk_List_Pointer;
      Chunk, Previous_Chunk : Chunk_Pointer;
   begin
      List := This.Head;
      while List /= null loop
         Chunk := List.Head;
         while Chunk /= null loop
            Previous_Chunk := Chunk;
            Chunk := Chunk.Next_Chunk;
            Dispose (Previous_Chunk);
         end loop;
         Previous_List := List;
         List := List.Next_List;
         Dispose (Previous_List);
      end loop;
   end Finalize;


   procedure Get_Chunk (Result : out Chunk_Pointer;
                        From : in out Pool;
                        Requested_Element_Size : SSE.Storage_Count;
                        Requested_Alignment : SSE.Storage_Count)
   is

      --  There are some tricky problems around the question of
      --  alignment, especially when the requested alignment is
      --  sufficiently large to impact the number of elements that can
      --  live in a chunk (the chunk's payload's alignment is the
      --  alignment of a System.Address).
      --
      --  This is normally not of any great significance: on i386
      --  hardware, the maximum alignment is 8, while on PowerPC it is
      --  4 (sometimes 8, depending on OS).
      --
      --  However, we can't calculate the number of elements that can
      --  be held in the chunk until we've got the chunk.

      --  The maximum that can be held if we turn out to be aligned
      --  correctly; there may in fact turn out to be room for one less
      --  element.
      Usable_Chunk_Size : constant SSE.Storage_Count :=
        (SSE.Storage_Count (From.Address_Array_Size) * Address_Size_SC
           / Requested_Alignment)
        * Requested_Alignment;

      Next, Start, Stop : System.Address;

      use type System.Address;
      use type SSE.Integer_Address;

   begin

      if Requested_Element_Size > Usable_Chunk_Size then
         --  We have no chance of meeting the requirement.
         raise BC.Storage_Error;
      end if;

      if From.Unused /= null then
         Result := From.Unused;
         From.Unused := From.Unused.Next_Chunk;
      else
         Result := new Chunk (Address_Array_Size => From.Address_Array_Size);
      end if;

      declare
         First : Positive := Result.Payload'First;
      begin
         --  Probably should be able to do this without a loop!
         loop
            exit when SSE.To_Integer (Result.Payload (First)'Address)
              mod SSE.Integer_Address (Requested_Alignment) = 0;
            First := First + 1;
         end loop;
         Start := Result.Payload (First)'Address;
         Result.Usable_Chunk_Size :=
           Usable_Chunk_Size
           - Address_Size_SC * SSE.Storage_Count (First
                                                    - Result.Payload'First);
      end;

      Result.Number_Elements :=
        Result.Usable_Chunk_Size / Requested_Element_Size;

      if Result.Number_Elements < 1 then
         --  We have failed. Put the chunk back.
         Result.Next_Chunk := From.Unused;
         From.Unused := Result;
         raise BC.Storage_Error;
      end if;

      Stop := Start + ((Result.Number_Elements - 1) * Requested_Element_Size);
      Next := Start;
      while Next < Stop loop
         Put (Next + Requested_Element_Size, At_Location => Next);
         Next := Next + Requested_Element_Size;
      end loop;
      Put (System.Null_Address, At_Location => Stop);
      Result.Next_Element := Start;

   end Get_Chunk;


   procedure Initialize (This : in out Pool)
   is
   begin
      This.Address_Array_Size :=
        (Integer (This.Chunk_Size) + Address_Size_I - 1) / Address_Size_I;
   end Initialize;


   procedure Preallocate_Chunks (This : in out Pool; Count : Positive)
   is
      Ch : Chunk_Pointer;
   begin
      for K in 1 .. Count loop
         Ch := new Chunk (Address_Array_Size => This.Address_Array_Size);
         Ch.Next_Chunk := This.Unused;
         This.Unused := Ch;
      end loop;
   end Preallocate_Chunks;


   procedure Purge_Unused_Chunks (This : in out Pool)
   is
      Chunk : Chunk_Pointer;
   begin
      while This.Unused /= null loop
         Chunk := This.Unused;
         This.Unused := This.Unused.Next_Chunk;
         Dispose (Chunk);
      end loop;
   end Purge_Unused_Chunks;


   procedure Put (This : System.Address;
                  At_Location : System.Address)
   is
   begin
      PeekPoke.To_Pointer (At_Location).all := This;
   end Put;


   procedure Reclaim_Unused_Chunks (This : in out Pool)
   is

      List : Chunk_List_Pointer;
      Previous_List : Chunk_List_Pointer;
      Chunk : Chunk_Pointer;
      Previous_Chunk : Chunk_Pointer;
      Element : System.Address;
      Previous_Element : System.Address; -- cjh

      use SSE;
      use type System.Address;

   begin

      pragma Style_Checks (Off); -- GNAT and GLIDE disagree about layout here

      List := This.Head;
      Previous_List := null;

      while List /= null loop

         Chunk := List.Head;

         --  Compute the maximum number of elements possible, per chunk,
         --  within this sized sublist.
     Compute_Max :
         while Chunk /= null loop
            Chunk.Number_Elements :=
              Chunk.Usable_Chunk_Size / Chunk.Parent.Element_Size;
            Chunk := Chunk.Next_Chunk;
         end loop Compute_Max;

         --  Now we traverse the "linked list" of free elements that
         --  span chunks, determining the containing chunk per element
         --  and decrementing the corresponding count (computed as the
         --  max, above).
         Element := List.Head.Next_Element;

     Decrement_Counts :
         while Element /= System.Null_Address loop
            Chunk := List.Head;

        This_Chunk :
            while Chunk /= null loop
               if Within_Range (Element, Chunk) then

                  Chunk.Number_Elements := Chunk.Number_Elements - 1;
                  exit This_Chunk;

               end if;
               Chunk := Chunk.Next_Chunk;
            end loop This_Chunk;
            if Chunk = null then
               raise Pool_Error;
            end if;

            Element := Value_At (Element); -- get next element

         end loop Decrement_Counts;

         --  Now walk each sized sublist and remove those chunks no
         --  longer used.
         Previous_Chunk := null;
         Chunk := List.Head;

     Reclaiming :
         while Chunk /= null loop

            if Chunk.Number_Elements = 0 then

               --  Remove this chunk to the Unused list.

               -- cjh: Elements on the "Next_Element" list and lying
               --  within this chunk must be removed from the list.
               Element := List.Head.Next_Element;
               Previous_Element := System.Null_Address;

               while Element /= System.Null_Address loop
                  if Within_Range (Element, Chunk) then
                     if Previous_Element = System.Null_Address then
                        List.Head.Next_Element := Value_At (Element);
                     else
                        Put (Value_At (Element),
                             At_Location => Previous_Element);
                     end if;
                  else
                     Previous_Element := Element;
                  end if;
                  Element := Value_At (Element); -- get next element
               end loop;
               -- end cjh

               if Previous_Chunk /= null then

                  --  This isn't the first chunk in this list.
                  Previous_Chunk.Next_Chunk := Chunk.Next_Chunk;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := Previous_Chunk.Next_Chunk;

               else

                  --  This is the first chunk in this list.
                  List.Head := Chunk.Next_Chunk;
                  Chunk.Next_Chunk := This.Unused;
                  This.Unused := Chunk;
                  Chunk := List.Head;

               end if;

            else

               --  Chunk isn't empty.
               Previous_Chunk := Chunk;
               Chunk := Chunk.Next_Chunk;

            end if;

         end loop Reclaiming;

         --  If this list has no chunks, delete it.
         if List.Head = null then

            declare
               Next_List : constant Chunk_List_Pointer := List.Next_List;
            begin

               if This.Head = List then

                  --  This is the head list of the pool; make the next
                  --  list the new head.
                  This.Head := Next_List;

               else

                  --  This isn't the head list of the pool, so there
                  --  is a previous list; make its next list this
                  --  list's next list.
                  if Previous_List = null then
                     raise Pool_Error;
                  end if;
                  Previous_List.Next_List := Next_List;

               end if;

               Dispose (List);

               List := Next_List;

            end;

         else

            --  List wasn't empty
            Previous_List := List;
            List := List.Next_List;

         end if;

      end loop;
      pragma Style_Checks (On);

   end Reclaim_Unused_Chunks;


   function Storage_Size (This : Pool) return SSE.Storage_Count
   is
      pragma Warnings (Off, This);
   begin
      return SSE.Storage_Count'Last; -- well, what else can we say!?
   end Storage_Size;


   function Total_Chunks (This : Pool) return Natural
   is
   begin
      return Dirty_Chunks (This) + Unused_Chunks (This);
   end Total_Chunks;


   function Unused_Chunks (This : Pool) return Natural
   is
      Chunk : Chunk_Pointer;
      Result : Natural := 0;
   begin
      Chunk := This.Unused;
      while Chunk /= null loop
         Result := Result + 1;
         Chunk := Chunk.Next_Chunk;
      end loop;
      return Result;
   end Unused_Chunks;


   procedure Usable_Size_And_Alignment
     (For_Size : SSE.Storage_Count;
      For_Alignment : SSE.Storage_Count;
      Is_Size : out SSE.Storage_Count;
      Is_Alignment : out SSE.Storage_Count)
   is
      --  The usable alignment is at least the alignment of a
      --  System.Address, because of the way that elements within a
      --  chunk are chained.
      --  The usable size must be a multiple of the size of a
      --  System.Address, likewise.
      Minimum_Size : constant SSE.Storage_Count :=
        SSE.Storage_Count'Max (For_Size, Address_Size_SC);
   begin
      Is_Size :=
        ((Minimum_Size + Address_Size_SC - 1) / Address_Size_SC)
        * Address_Size_SC;
      Is_Alignment :=
        SSE.Storage_Count'Max (For_Alignment,
                               System.Address'Alignment);
   end Usable_Size_And_Alignment;


   function Value_At (Location : System.Address) return System.Address
   is
   begin
      return PeekPoke.To_Pointer (Location).all;
   end Value_At;


   function Within_Range (Target : System.Address;
                          Base : Chunk_Pointer) return Boolean
   is
      use type System.Address;
   begin

      --  Element is within this chunk (NB, we check <= the last
      --  address because this is a legal position, at least for
      --  elements no larger than a System.Address).
      return Base.Payload (Base.Payload'First)'Address <= Target
        and Target <= Base.Payload (Base.Payload'Last)'Address;

   end Within_Range;


end BC.Support.Managed_Storage;
