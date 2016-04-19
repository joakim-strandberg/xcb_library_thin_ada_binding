--  Copyright 1994 Grady Booch
--  Copyright 1998-2011 Simon Wright <simon@pushface.org>

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

--  $Revision: 1473 $
--  $Date: 2011-06-25 21:02:07 +0100 (Sat, 25 Jun 2011) $
--  $Author: simonjwright $

with System.Address_To_Access_Conversions;

package body BC.Graphs.Undirected is


   ----------------------
   -- Graph operations --
   ----------------------

   procedure Create_Arc (G : in out Graph;
                         A : in out Arc'Class;
                         I : Arc_Item;
                         First : in out Vertex'Class;
                         Second : in out Vertex'Class) is
   begin
      Clear (A);
      A.Rep := new Arc_Node'(Ada.Finalization.Controlled with
                             Item => I,
                             Enclosing => G'Unchecked_Access,
                             From => First.Rep,
                             To => Second.Rep,
                             Next_Incoming => null,
                             Next_Outgoing => null,
                             Count => 1);
      if Second.Rep /= null then
         A.Rep.Next_Incoming := Second.Rep.Incoming;
         Second.Rep.Incoming := A.Rep;
         A.Rep.Count := A.Rep.Count + 1;
         Second.Rep.Count := Second.Rep.Count + 1;
      end if;
      if First.Rep /= null then
         A.Rep.Next_Outgoing := First.Rep.Outgoing;
         First.Rep.Outgoing := A.Rep;
         A.Rep.Count := A.Rep.Count + 1;
         First.Rep.Count := First.Rep.Count + 1;
      end if;
   end Create_Arc;


   -----------------------
   -- Vertex operations --
   -----------------------

   function Arity (V : Vertex) return Natural is
      Count : Natural := 0;
      Curr : Arc_Node_Ptr;
   begin
      if V.Rep = null then
         raise BC.Is_Null;
      end if;
      Curr := V.Rep.Incoming;
      while Curr /= null loop
         Count := Count + 1;
         Curr := Curr.Next_Incoming;
      end loop;
      Curr := V.Rep.Outgoing;
      while Curr /= null loop
         if Curr.From /= Curr.To then
            Count := Count + 1;
         end if;
         Curr := Curr.Next_Outgoing;
      end loop;
      return Count;
   end Arity;


   --------------------
   -- Arc operations --
   --------------------

   procedure Set_First_Vertex (A : in out Arc;
                               V : access Vertex'Class) is
      Prev, Curr : Arc_Node_Ptr;
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      if A.Rep.From /= null then
         Prev := null;
         Curr := A.Rep.From.Outgoing;
         while Curr /= A.Rep loop
            Prev := Curr;
            Curr := Curr.Next_Outgoing;
         end loop;
         if Prev = null then
            A.Rep.From.Outgoing := Curr.Next_Outgoing;
         else
            Prev.Next_Outgoing := Curr.Next_Outgoing;
         end if;
         A.Rep.From.Count := A.Rep.From.Count - 1;
         A.Rep.Count := A.Rep.Count - 1;
      end if;
      if V.Rep /= null then
         A.Rep.Next_Outgoing := V.Rep.Outgoing;
         V.Rep.Outgoing := A.Rep;
         A.Rep.Count := A.Rep.Count + 1;
         V.Rep.Count := V.Rep.Count + 1;
      end if;
      A.Rep.From := V.Rep;
   end Set_First_Vertex;


   procedure Set_Second_Vertex (A : in out Arc;
                                V : access Vertex'Class) is
      Prev, Curr : Arc_Node_Ptr;
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      if A.Rep.To /= null then
         Prev := null;
         Curr := A.Rep.To.Incoming;
         while Curr /= A.Rep loop
            Prev := Curr;
            Curr := Curr.Next_Incoming;
         end loop;
         if Prev = null then
            A.Rep.To.Incoming := Curr.Next_Incoming;
         else
            Prev.Next_Incoming := Curr.Next_Incoming;
         end if;
         A.Rep.To.Count := A.Rep.To.Count - 1;
         A.Rep.Count := A.Rep.Count - 1;
      end if;
      if V.Rep /= null then
         A.Rep.Next_Incoming := V.Rep.Incoming;
         V.Rep.Incoming := A.Rep;
         A.Rep.Count := A.Rep.Count + 1;
         V.Rep.Count := V.Rep.Count + 1;
      end if;
      A.Rep.To := V.Rep;
   end Set_Second_Vertex;


   procedure First_Vertex (A : Arc;
                           V : in out Vertex'Class) is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      Clear (V);
      V.Rep := A.Rep.From;
      if V.Rep /= null then
         V.Rep.Count := V.Rep.Count + 1;
      end if;
   end First_Vertex;


   procedure Second_Vertex (A : Arc;
                            V : in out Vertex'Class) is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      Clear (V);
      V.Rep := A.Rep.To;
      if V.Rep /= null then
         V.Rep.Count := V.Rep.Count + 1;
      end if;
   end Second_Vertex;


   ---------------------
   -- Graph iterators --
   ---------------------


   package Graph_Address_Conversions
   is new System.Address_To_Access_Conversions (Graph);

   function New_Graph_Iterator
     (For_The_Graph : Graph) return Graph_Iterator'Class is
      Result : constant Undirected_Graph_Iterator
        := (For_The_Graph => Graph_Ptr (Graph_Address_Conversions.To_Pointer
                                          (For_The_Graph'Address)),
            Index => For_The_Graph.Rep);
   begin
      return Result;
   end New_Graph_Iterator;


   package Vertex_Address_Conversions
   is new System.Address_To_Access_Conversions (Vertex);

   function New_Vertex_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class is
      Result : Undirected_Vertex_Iterator;
   begin
      Result.For_The_Vertex :=
        Vertex_Ptr (Vertex_Address_Conversions.To_Pointer
                      (For_The_Vertex'Address));
      Reset (Result);
      return Result;
   end New_Vertex_Iterator;


   -------------------------------
   -- Private iteration support --
   -------------------------------

   procedure Reset (It : in out Undirected_Graph_Iterator) is
   begin
      It.Index := It.For_The_Graph.Rep;
   end Reset;


   procedure Next (It : in out Undirected_Graph_Iterator) is
   begin
      if It.Index /= null then
         It.Index := It.Index.Next;
      end if;
   end Next;


   function Is_Done (It : Undirected_Graph_Iterator) return Boolean is
   begin
      return It.Index = null;
   end Is_Done;


   function Current_Vertex
     (It : Undirected_Graph_Iterator) return Abstract_Vertex'Class is
   begin
      if It.Index = null then
         raise BC.Is_Null;
      end if;
      It.Index.Count := It.Index.Count + 1;
      return Vertex'
        (Ada.Finalization.Controlled with Rep => It.Index);
   end Current_Vertex;


   ----------------------
   -- Vertex iterators --
   ----------------------

   procedure Reset (It : in out Undirected_Vertex_Iterator) is
   begin
      It.Do_Outgoing := True;
      if It.For_The_Vertex.Rep /= null then
         It.Index := It.For_The_Vertex.Rep.Outgoing;
         if It.Index = null then
            It.Do_Outgoing := False;
            It.Index := It.For_The_Vertex.Rep.Incoming;
            --  skip self-directed arcs, already seen in outgoing side
            --  XXX hmm, wouldn't .Outgoing have been non-null?
            while It.Index /= null and then It.Index.From = It.Index.To loop
               pragma Assert (False);
               It.Index := It.Index.Next_Incoming;
            end loop;
         end if;
      else
         It.Index := null;
      end if;
   end Reset;


   procedure Next (It : in out Undirected_Vertex_Iterator) is
   begin
      --  XXX I think we ought to check here that there is an Index!
      if It.Do_Outgoing then
         It.Index := It.Index.Next_Outgoing;
         if It.Index = null then
            It.Do_Outgoing := False;
            It.Index := It.For_The_Vertex.Rep.Incoming;
            --  skip self-directed arcs, already seen in outgoing side
            while It.Index /= null and then It.Index.From = It.Index.To loop
               It.Index := It.Index.Next_Incoming;
            end loop;
         end if;
      elsif It.Index /= null then
         It.Index := It.Index.Next_Incoming;
         --  skip self-directed arcs, already seen in outgoing side
         while It.Index /= null and then It.Index.From = It.Index.To loop
            It.Index := It.Index.Next_Incoming;
         end loop;
      end if;
   end Next;


   function Is_Done (It : Undirected_Vertex_Iterator) return Boolean is
   begin
      return It.Index = null;
   end Is_Done;


   function Current_Arc (It : Undirected_Vertex_Iterator)
                        return Abstract_Arc'Class is
   begin
      if It.Index = null then
         raise BC.Is_Null;
      end if;
      It.Index.Count := It.Index.Count + 1;
      return Arc'(Ada.Finalization.Controlled with Rep => It.Index);
   end Current_Arc;


end BC.Graphs.Undirected;
