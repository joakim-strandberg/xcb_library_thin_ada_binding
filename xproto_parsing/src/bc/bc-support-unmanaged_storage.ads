--  Copyright 1998-1999 Pat Rogers
--  Copyright 1999-2002 Simon Wright <simon@pushface.org>

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

with System.Storage_Pools;
with System.Storage_Elements;

package BC.Support.Unmanaged_Storage is

   pragma Preelaborate;

   package SSE renames System.Storage_Elements;
   package SSP renames System.Storage_Pools;

   type Pool is new SSP.Root_Storage_Pool with private;


   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count);
   pragma Inline (Allocate);

   procedure Deallocate (The_Pool : in out Pool;
                         Storage_Address : System.Address;
                         Size_In_Storage_Elements : SSE.Storage_Count;
                         Alignment : SSE.Storage_Count);
   pragma Inline (Deallocate);

   function Storage_Size (This : Pool) return SSE.Storage_Count;
   pragma Inline (Storage_Size);

private

   type Pool is new SSP.Root_Storage_Pool with null record;

end BC.Support.Unmanaged_Storage;
