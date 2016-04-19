--  Copyright 2000-2004 Simon Wright <simon@pushface.org>

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

with Ada.Text_IO; use Ada.Text_IO;
with System.Storage_Elements;

package body BC.Support.Synchronization.Debug is


   function Image (The_Address : System.Address) return String;
   function Image (The_Address : System.Address) return String is
      use System.Storage_Elements;
   begin
      return Integer_Address'Image (To_Integer (The_Address));
   end Image;


   procedure Seize (The_Semaphore : in out Debug_Semaphore) is
   begin
      Put_Line ("Semaphore.Seize:"
                  & Image (The_Semaphore'Address));
      Seize (Semaphore (The_Semaphore));
   end Seize;

   procedure Release (The_Semaphore : in out Debug_Semaphore) is
   begin
      Put_Line ("Semaphore.Release:"
                  & Image (The_Semaphore'Address));
      Release (Semaphore (The_Semaphore));
   end Release;


   procedure Seize (The_Semaphore : in out Debug_Recursive_Semaphore) is
   begin
      Put_Line ("Recursive_Semaphore.Seize:"
                  & Image (The_Semaphore'Address));
      Seize (Recursive_Semaphore (The_Semaphore));
   end Seize;

   procedure Release (The_Semaphore : in out Debug_Recursive_Semaphore) is
   begin
      Put_Line ("Recursive_Semaphore.Release:"
                  & Image (The_Semaphore'Address));
      Release (Recursive_Semaphore (The_Semaphore));
   end Release;


   procedure Seize_For_Reading
     (The_Monitor : in out Debug_Single_Monitor) is
   begin
      Put_Line ("Single_Monitor.Seize_For_Reading:"
                  & Image (The_Monitor'Address));
      Seize_For_Reading (Single_Monitor (The_Monitor));
   end Seize_For_Reading;

   procedure Seize_For_Writing
     (The_Monitor : in out Debug_Single_Monitor) is
   begin
      Put_Line ("Single_Monitor.Seize_For_Writing:"
                  & Image (The_Monitor'Address));
      Seize_For_Writing (Single_Monitor (The_Monitor));
   end Seize_For_Writing;

   procedure Release_From_Reading
     (The_Monitor : in out Debug_Single_Monitor) is
   begin
      Put_Line ("Single_Monitor.Release_From_Reading:"
                  & Image (The_Monitor'Address));
      Release_From_Reading (Single_Monitor (The_Monitor));
   end Release_From_Reading;

   procedure Release_From_Writing
     (The_Monitor : in out Debug_Single_Monitor) is
   begin
      Put_Line ("Single_Monitor.Release_From_Writing:"
                  & Image (The_Monitor'Address));
      Release_From_Writing (Single_Monitor (The_Monitor));
   end Release_From_Writing;


   procedure Seize_For_Reading
     (The_Monitor : in out Debug_Multiple_Monitor) is
   begin
      Put_Line ("Multiple_Monitor.Seize_For_Reading:"
                  & Image (The_Monitor'Address));
      Seize_For_Reading (Multiple_Monitor (The_Monitor));
   end Seize_For_Reading;

   procedure Seize_For_Writing
     (The_Monitor : in out Debug_Multiple_Monitor) is
   begin
      Put_Line ("Multiple_Monitor.Seize_For_Writing:"
                  & Image (The_Monitor'Address));
      Seize_For_Writing (Multiple_Monitor (The_Monitor));
   end Seize_For_Writing;

   procedure Release_From_Reading
     (The_Monitor : in out Debug_Multiple_Monitor) is
   begin
      Put_Line ("Multiple_Monitor.Release_From_Reading:"
                  & Image (The_Monitor'Address));
      Release_From_Reading (Multiple_Monitor (The_Monitor));
   end Release_From_Reading;

   procedure Release_From_Writing
     (The_Monitor : in out Debug_Multiple_Monitor) is
   begin
      Put_Line ("Multiple_Monitor.Release_From_Writing:"
                  & Image (The_Monitor'Address));
      Release_From_Writing (Multiple_Monitor (The_Monitor));
   end Release_From_Writing;


end BC.Support.Synchronization.Debug;
