--  Copyright 2000-2009 Simon Wright <simon@pushface.org>

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

package BC.Support.Synchronization.Debug is

   pragma Elaborate_Body;

   --  Use these types when you need logging of activity.  Each
   --  operation reports itself (using the address of the Semaphore or
   --  Monitor) before calling its parent operation.


   type Debug_Semaphore is new Semaphore with private;
   procedure Seize (The_Semaphore : in out Debug_Semaphore);
   procedure Release (The_Semaphore : in out Debug_Semaphore);


   type Debug_Recursive_Semaphore is new Recursive_Semaphore with private;
   procedure Seize (The_Semaphore : in out Debug_Recursive_Semaphore);
   procedure Release (The_Semaphore : in out Debug_Recursive_Semaphore);


   type Debug_Single_Monitor is new Single_Monitor with private;
   procedure Seize_For_Reading
     (The_Monitor : in out Debug_Single_Monitor);
   procedure Seize_For_Writing
     (The_Monitor : in out Debug_Single_Monitor);
   procedure Release_From_Reading
     (The_Monitor : in out Debug_Single_Monitor);
   procedure Release_From_Writing
     (The_Monitor : in out Debug_Single_Monitor);


   type Debug_Multiple_Monitor is new Multiple_Monitor with private;
   procedure Seize_For_Reading
     (The_Monitor : in out Debug_Multiple_Monitor);
   procedure Seize_For_Writing
     (The_Monitor : in out Debug_Multiple_Monitor);
   procedure Release_From_Reading
     (The_Monitor : in out Debug_Multiple_Monitor);
   procedure Release_From_Writing
     (The_Monitor : in out Debug_Multiple_Monitor);


private

   type Debug_Semaphore is new Semaphore with null record;

   type Debug_Recursive_Semaphore is new Recursive_Semaphore with null record;

   type Debug_Single_Monitor is new Single_Monitor with null record;

   type Debug_Multiple_Monitor is new Multiple_Monitor with null record;

end BC.Support.Synchronization.Debug;
