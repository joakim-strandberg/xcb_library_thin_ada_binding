--  Copyright 1998-1999 Pat Rogers
--  Copyright 1999-2005 Simon Wright <simon@pushface.org>

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

--  $Revision: 1469 $
--  $Date: 2011-06-22 17:59:35 +0100 (Wed, 22 Jun 2011) $
--  $Author: simonjwright $

package body BC.Support.Unmanaged_Storage is

   type Default_Access_Type is access Integer;  -- arbitrary designated subtype

   --  In the subprograms below, we would use just
   --  Default_Access_Type'Storage_Pool but that GNAT 3.11b has an error in
   --  this area. Thanks to Gene Ouye <geneo@rational.com> for the idea.

   procedure Allocate (The_Pool : in out Pool;
                       Storage_Address : out System.Address;
                       Size_In_Storage_Elements : SSE.Storage_Count;
                       Alignment : SSE.Storage_Count) is
      pragma Unreferenced (The_Pool);
   begin
      pragma Warnings (Off, "redundant conversion*");
      SSP.Allocate (SSP.Root_Storage_Pool'Class
                      (Default_Access_Type'Storage_Pool),
                    Storage_Address,
                    Size_In_Storage_Elements,
                    Alignment);
      pragma Warnings (On, "redundant conversion*");
   end Allocate;


   procedure Deallocate (The_Pool : in out Pool;
                         Storage_Address : System.Address;
                         Size_In_Storage_Elements : SSE.Storage_Count;
                         Alignment : SSE.Storage_Count) is
      pragma Unreferenced (The_Pool);
   begin
      pragma Warnings (Off, "redundant conversion*");
      SSP.Deallocate (SSP.Root_Storage_Pool'Class
                        (Default_Access_Type'Storage_Pool),
                      Storage_Address,
                      Size_In_Storage_Elements,
                      Alignment);
      pragma Warnings (On, "redundant conversion*");
   end Deallocate;


   function Storage_Size (This : Pool) return SSE.Storage_Count is
      pragma Warnings (Off, This);
   begin
      pragma Warnings (Off, "redundant conversion*");
      return SSP.Storage_Size (SSP.Root_Storage_Pool'Class
                                 (Default_Access_Type'Storage_Pool));
      pragma Warnings (On, "redundant conversion*");
   end Storage_Size;


end BC.Support.Unmanaged_Storage;
