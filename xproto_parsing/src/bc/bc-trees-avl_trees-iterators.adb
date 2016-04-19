--  Copyright 2004 Simon Wright <simon@pushface.org>

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

with System.Address_To_Access_Conversions;

package body BC.Trees.AVL_Trees.Iterators is

   package Address_Conversions
   is new System.Address_To_Access_Conversions (AVL_Tree);

   function New_Iterator (For_The_Container : AVL_Tree) return Iterator is
      Result : Iterator;
   begin
      Result.For_The_Container :=
        Container_Ptr (Address_Conversions.To_Pointer
                         (For_The_Container'Address));
      Reset (Result);
      return Result;
   end New_Iterator;

   procedure Reset (It : in out Iterator) is
      This : AVL_Node_Ref := It.For_The_Container.Rep;
   begin
      It.Previous := null;
      It.Current := null;
      while This /= null loop
         It.Current := This;
         This := This.Left;
      end loop;
   end Reset;

   function Is_Done (It : Iterator) return Boolean is
   begin
      return It.Current = null;
   end Is_Done;

   function Current_Item (It : Iterator) return Item is
   begin
      return It.Current.Element;
   end Current_Item;

   procedure Next (It : in out Iterator) is
      procedure Visit (Node : AVL_Node_Ref);
      Found_Previous : Boolean := False;
      Continue : Boolean := True;
      procedure Visit (Node : AVL_Node_Ref) is
      begin
         if Node /= null then
            Visit (Node.Left);
            if not Continue then
               return;
            elsif Found_Previous then
               It.Current := Node;
               Continue := False;
               return;
            elsif Node = It.Previous then
               Found_Previous := True;
            end if;
            Visit (Node.Right);
         end if;
      end Visit;
   begin
      It.Previous := It.Current;
      It.Current := null;
      Visit (It.For_The_Container.Rep);
   end Next;

end BC.Trees.AVL_Trees.Iterators;
