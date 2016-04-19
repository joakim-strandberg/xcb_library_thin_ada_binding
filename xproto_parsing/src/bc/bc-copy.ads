--  Copyright 2001-2002 Simon Wright <simon@pushface.org>

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

with BC.Containers;
generic

   type Item is private;
   --  The type to be copied.

   with package Source is new BC.Containers (Item);
   --  A Container instantiation.

   type From (<>) is new Source.Container with private;
   --  The Container type which contains the source Items.

   with package Target is new BC.Containers (Item);
   --  A Container instantiation (possibly different from Source).

   type To (<>) is new Target.Container with private;
   --  The Container type which is to contain the copied Items.

   with procedure Clear (The_Container : in out To) is <>;
   --  A procedure to clear the destination Container.

   with procedure Add (To_The_Container : in out To; I : Item) is <>;
   --  A procedure to add an Item to the destination Container.

procedure BC.Copy (Input : From; Output : in out To);
pragma Preelaborate (BC.Copy);
