--  Copyright 1994 Grady Booch
--  Copyright 1999 Pat Rogers
--  Copyright 1999-2008 Simon Wright <simon@pushface.org>

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

with System.Storage_Pools;
with System.Storage_Elements;

package BC.Support.Managed_Storage is

   pragma Preelaborate;

   package SSE renames System.Storage_Elements;

   type Pool (Chunk_Size : SSE.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with private;
   --  This pool can allocate objects of type Your_Type that are no
   --  larger than Chunk_Size (including any extra space for the
   --  constraints of objects of indefinite types). BC.Storage_Error
   --  will be raised for attempts to allocate larger objects.

   --  At any time, each chunk that is in use may contain objects of a
   --  specific size and alignment. There may be more than one chunk
   --  containing objects of the same size and alignment. A used chunk
   --  may currently contain no objects.

   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count);

   procedure Deallocate (The_Pool : in out Pool;
                         Storage_Address : System.Address;
                         Size_In_Storage_Elements : SSE.Storage_Count;
                         Alignment : SSE.Storage_Count);

   function Storage_Size (This : Pool) return SSE.Storage_Count;

   --  Create Count empty chunks, place on the Unused list.
   procedure Preallocate_Chunks (This : in out Pool;  Count : Positive);

   --  Place any in-use chunks without current allocations back on the
   --  Unused list.
   procedure Reclaim_Unused_Chunks (This : in out Pool);

   --  Release any chunks on the Unused list.
   procedure Purge_Unused_Chunks (This : in out Pool);

   function Total_Chunks (This : Pool) return Natural;

   --  Count of in-use chunks (which may not contain any current
   --  allocations).
   function Dirty_Chunks (This : Pool) return Natural;

   --  Count of chunks on the Unused list.
   function Unused_Chunks (This : Pool) return Natural;

private

   --  Chunks are organised in a doubly-linked list, where each member
   --  is the head of a list of chunks all of the same aligned element
   --  size and alignment.
   --
   --  The doubly-linked list is organised by decreasing aligned
   --  element size and then (for the same aligned element size) by
   --  decreasing alignment.
   type Chunk_List;
   type Chunk_List_Pointer is access all Chunk_List;

   --  The user data is organised as an array of System.Address, large
   --  enough to hold at least the number of bytes required.
   type Address_Array is array (Positive range <>) of System.Address;

   type Chunk (Address_Array_Size : Natural);
   type Chunk_Pointer is access all Chunk;

   type Chunk_List is record

         --  Chain of heads
         Next_List : Chunk_List_Pointer;

         --  Chain of chunks all of the same element size & alignment
         --  (as below).
         Head : Chunk_Pointer;

         --  Size of elements held in this chunk.
         Element_Size : SSE.Storage_Count;

         --  Alignment of elements held in this chunk.
         Alignment : SSE.Storage_Count;

   end record;

   type Chunk (Address_Array_Size : Natural) is record

      --  Owning list of chunks all of the same element size & alignment.
      Parent : Chunk_List_Pointer;

      --  Chain of chunks all of the same element size & alignment.
      Next_Chunk : Chunk_Pointer;

      --  Usable size, depending on current alignment.
      Usable_Chunk_Size : SSE.Storage_Count;

      --  Number of free (or used, depending on context) elements
      --  in this chunk.
      Number_Elements : SSE.Storage_Count;

      --  Address of next free element in this chunk.
      Next_Element : System.Address;

      Payload : Address_Array (1 .. Address_Array_Size);

   end record;

   type Pool (Chunk_Size : SSE.Storage_Count)
      is new System.Storage_Pools.Root_Storage_Pool with record
         Head : Chunk_List_Pointer;
         Unused : Chunk_Pointer;
         Address_Array_Size : Natural;
   end record;

   procedure Initialize (This : in out Pool);
   procedure Finalize (This : in out Pool);

end BC.Support.Managed_Storage;
