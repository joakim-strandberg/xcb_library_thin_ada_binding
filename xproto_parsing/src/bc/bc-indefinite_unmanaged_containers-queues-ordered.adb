--  Copyright 1994 Grady Booch
--  Copyright 2005 Martin Krischik
--  Copyright 2003-2012 Simon Wright <simon@pushface.org>

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

--  $Revision: 1508 $
--  $Date: 2012-06-03 19:01:11 +0100 (Sun, 03 Jun 2012) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Indefinite_Unmanaged_Containers.Queues.Ordered is

   procedure Append (Q : in out Queue; Elem : Item) is
   begin
      for Index in 1 .. Queue_Nodes.Length (Q.Rep)
      loop
         if Elem < Queue_Nodes.Item_At (Q.Rep, Index) then
            Queue_Nodes.Insert (Q.Rep, Elem, Index);
            return;
         end if;
      end loop;
      Queue_Nodes.Append (Q.Rep, Elem);
   end Append;

   package Address_Conversions
   is new System.Address_To_Access_Conversions (Queue);

   function New_Iterator (For_The_Queue : Queue) return Iterator'Class is
      Result : Queue_Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer (For_The_Queue'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   function Null_Container return Queue is
      Empty_Container : Queue;
      pragma Warnings (Off, Empty_Container);
   begin
      return Empty_Container;
   end Null_Container;

end BC.Indefinite_Unmanaged_Containers.Queues.Ordered;
