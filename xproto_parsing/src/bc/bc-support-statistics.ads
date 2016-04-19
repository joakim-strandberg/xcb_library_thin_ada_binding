--  Copyright 2003-2009 Simon Wright <simon@pushface.org>

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

package BC.Support.Statistics is

   pragma Preelaborate;

   type Instance is private;

   procedure Add (Datum : Long_Float; To : in out Instance);

   function Count (Of_Instance : Instance) return Natural;

   function Mean (Of_Instance : Instance) return Long_Float;

   function Variance (Of_Instance : Instance) return Long_Float;

   function Sigma (Of_Instance : Instance) return Long_Float;

   function Min (Of_Instance : Instance) return Long_Float;

   function Max (Of_Instance : Instance) return Long_Float;

private

   subtype Positive_Long_Float is Long_Float range 0.0 .. Long_Float'Last;

   type Instance is record
      Count : Natural := 0;
      Min : Long_Float := Long_Float'Last;
      Max : Long_Float := Long_Float'First;
      Summed_Data : Long_Float := 0.0;
      Summed_Squared_Data : Positive_Long_Float := 0.0;
   end record;

end BC.Support.Statistics;
