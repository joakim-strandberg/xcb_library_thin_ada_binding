--  Copyright 1994 Grady Booch
--  Copyright 1998-2003 Simon Wright <simon@pushface.org>

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

with Ada.Unchecked_Deallocation;

package body BC.Graphs is

   procedure Delete is new Ada.Unchecked_Deallocation
     (Vertex_Node, Vertex_Node_Ptr);
   procedure Delete is new Ada.Unchecked_Deallocation
     (Arc_Node, Arc_Node_Ptr);


   ----------------------
   -- Graph operations --
   ----------------------

   procedure Clear (G : in out Abstract_Graph) is
      Curr : Vertex_Node_Ptr := G.Rep;
      Next : Vertex_Node_Ptr;
   begin
      --  In the C++, this was done using Iterators which created a
      --  Vertex and then called Destroy_Vertex.  We can't do that,
      --  because our Vertices are abstract.
      while Curr /= null loop
         Next := Curr.Next;
         Clear_Vertex_Node (G, Curr);
         Curr := Next;
      end loop;
   end Clear;


   procedure Create_Vertex (G : in out Abstract_Graph;
                            V : in out Abstract_Vertex'Class;
                            I : Vertex_Item) is
   begin
      Clear (V);
      V.Rep := new Vertex_Node'(Ada.Finalization.Controlled with
                                Item => I,
                                Enclosing => G'Unchecked_Access,
                                Incoming => null,
                                Outgoing => null,
                                Next => G.Rep,
                                Count => 1);
      G.Rep := V.Rep;
      G.Rep.Count := G.Rep.Count + 1;
   end Create_Vertex;


   procedure Destroy_Vertex (G : in out Abstract_Graph;
                             V : in out Abstract_Vertex'Class) is
   begin
      if not Is_Member (G, V) then
         raise BC.Not_Found;
      end if;
      if V.Rep /= null then
         --  The C++ had the body of what is now Clear_Vertex_Node
         --  here, because it had the iterators available for the
         --  Clear (Graph) operation. Also, the type Vertex wasn't
         --  abstract (GEB didn't make much use of inheritance).
         Clear_Vertex_Node (G, V.Rep);
         Clear (V);
      end if;
   end Destroy_Vertex;


   procedure Destroy_Arc (G : in out Abstract_Graph;
                          A : in out Abstract_Arc'Class) is
      Prev, Curr : Arc_Node_Ptr;
   begin
      if not Is_Member (G, A) then
         raise BC.Not_Found;
      end if;
      if A.Rep /= null then
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
         A.Rep.From := null;
         A.Rep.To := null;
         A.Rep.Next_Incoming := null;
         A.Rep.Next_Outgoing := null;
         A.Rep.Enclosing := null;
         --  XXX should we decrement the count one more, like
         --  Destroy_Vertex?  (presumably for the lost Enclosing?)
         Clear (A);
      end if;
   end Destroy_Arc;


   function Number_Of_Vertices (G : Abstract_Graph) return Natural is
      Count : Natural := 0;
      Curr : Vertex_Node_Ptr := G.Rep;
   begin
      while Curr /= null loop
         Curr := Curr.Next;
         Count := Count + 1;
      end loop;
      return Count;
   end Number_Of_Vertices;


   function Is_Empty (G : Abstract_Graph) return Boolean is
   begin
      return G.Rep = null;
   end Is_Empty;


   function Is_Member (G : Abstract_Graph;
                       V : Abstract_Vertex'Class) return Boolean is
      --  Thanks to Tucker Taft for this workround to an access level
      --  problem
      type Graph_Const_Ptr is access constant Abstract_Graph;
   begin
      if V.Rep = null then
         return False;
      else
         return Graph_Const_Ptr (V.Rep.Enclosing) = G'Access;
      end if;
   end Is_Member;


   function Is_Member (G : Abstract_Graph;
                       A : Abstract_Arc'Class) return Boolean is
      --  Thanks to Tucker Taft for this workround to an access level
      --  problem
      type Graph_Const_Ptr is access constant Abstract_Graph;
   begin
      if A.Rep = null then
         return False;
      else
         return Graph_Const_Ptr (A.Rep.Enclosing) = G'Access;
      end if;
   end Is_Member;


   -----------------------
   -- Vertex operations --
   -----------------------

   function "=" (L, R : Abstract_Vertex) return Boolean is
   begin
      return L.Rep = R.Rep;
   end "=";


   procedure Clear (V : in out Abstract_Vertex) is
   begin
      if V.Rep /= null then
         if V.Rep.Count > 1 then
            V.Rep.Count := V.Rep.Count - 1;
         else
            Delete (V.Rep);
         end if;
         V.Rep := null;
      end if;
   end Clear;


   procedure Set_Item (V : in out Abstract_Vertex; I : Vertex_Item) is
   begin
      if V.Rep = null then
         raise BC.Is_Null;
      end if;
      V.Rep.Item := I;
   end Set_Item;


   function Is_Null (V : Abstract_Vertex) return Boolean is
   begin
      return V.Rep = null;
   end Is_Null;


   function Is_Shared (V : Abstract_Vertex) return Boolean is
   begin
      return V.Rep /= null and then V.Rep.Count > 1;
   end Is_Shared;


   function Item (V : Abstract_Vertex) return Vertex_Item is
   begin
      if V.Rep = null then
         raise BC.Is_Null;
      end if;
      return V.Rep.Item;
   end Item;


   procedure Access_Vertex_Item (V : Abstract_Vertex'Class) is
   begin
      if V.Rep = null then
         raise BC.Is_Null;
      end if;
      Process (V.Rep.Item);
   end Access_Vertex_Item;


   function Enclosing_Graph (V : Abstract_Vertex) return Graph_Ptr is
   begin
      if V.Rep = null then
         raise BC.Is_Null;
      end if;
      return V.Rep.Enclosing;
   end Enclosing_Graph;


   --------------------
   -- Arc operations --
   --------------------

   function "=" (L, R : Abstract_Arc) return Boolean is
   begin
      return L.Rep = R.Rep;
   end "=";


   procedure Clear (A : in out Abstract_Arc) is
   begin
      if A.Rep /= null then
         if A.Rep.Count > 1 then
            A.Rep.Count := A.Rep.Count - 1;
         else
            Delete (A.Rep);
         end if;
         A.Rep := null;
      end if;
   end Clear;


   procedure Set_Item (A : in out Abstract_Arc; I : Arc_Item) is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      A.Rep.Item := I;
   end Set_Item;


   function Is_Null (A : Abstract_Arc) return Boolean is
   begin
      return A.Rep = null;
   end Is_Null;


   function Is_Shared (A : Abstract_Arc) return Boolean is
   begin
      return A.Rep /= null and then A.Rep.Count > 1;
   end Is_Shared;


   function Item (A : Abstract_Arc) return Arc_Item is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      return A.Rep.Item;
   end Item;


   procedure Access_Arc_Item (A : Abstract_Arc'Class) is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      Process (A.Rep.Item);
   end Access_Arc_Item;


   function Enclosing_Graph (A : Abstract_Arc) return Graph_Ptr is
   begin
      if A.Rep = null then
         raise BC.Is_Null;
      end if;
      return A.Rep.Enclosing;
   end Enclosing_Graph;


   --------------------------------------------
   -- Iteration over the Vertices in a Graph --
   --------------------------------------------

   procedure Visit_Vertices (Using : in out Graph_Iterator'Class) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Vertex (Using), Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Visit_Vertices;


   ---------------------------------------------------
   -- Iteration over the Arcs connected to a Vertex --
   ---------------------------------------------------

   procedure Visit_Arcs (Using : in out Vertex_Iterator'Class) is
      Success : Boolean;
   begin
      Reset (Using);
      while not Is_Done (Using) loop
         Apply (Current_Arc (Using), Success);
         exit when not Success;
         Next (Using);
      end loop;
   end Visit_Arcs;


   ----------------------------------------------
   -- Utilities, controlled storage management --
   ----------------------------------------------

   procedure Clear_Vertex_Node (G : in out Abstract_Graph'Class;
                                N : in out Vertex_Node_Ptr) is
      Curr : Arc_Node_Ptr;
      Prev, Index : Vertex_Node_Ptr;
   begin
      while N.Incoming /= null loop
         Curr := N.Incoming;
         N.Incoming := Curr.Next_Incoming;
         Curr.To := null;
         Curr.Next_Incoming := null;
         Curr.Enclosing := null;
         if Curr.Count > 1 then
            Curr.Count := Curr.Count - 1;
         else
            Delete (Curr);
         end if;
         N.Count := N.Count - 1;
      end loop;
      while N.Outgoing /= null loop
         Curr := N.Outgoing;
         N.Outgoing := Curr.Next_Outgoing;
         Curr.From := null;
         Curr.Next_Outgoing := null;
         Curr.Enclosing := null;
         if Curr.Count > 1 then
            Curr.Count := Curr.Count - 1;
         else
            Delete (Curr);
         end if;
         N.Count := N.Count - 1;
      end loop;
      Prev := null;
      Index := G.Rep;
      while Index /= N loop
         Prev := Index;
         Index := Index.Next;
      end loop;
      if Prev = null then
         G.Rep := Index.Next;
      else
         Prev.Next := Index.Next;
      end if;
      Index.Next := null;
      N.Enclosing := null;
      N.Count := N.Count - 1;
      if N.Count = 0 then
         Delete (N);
      end if;
   end Clear_Vertex_Node;


   procedure Finalize (V : in out Vertex_Node) is
   begin
      if V.Count > 1 then
         raise Graph_Error;
      end if;
   end Finalize;


   procedure Finalize (A : in out Arc_Node) is
   begin
      if A.Count > 1 then
         raise Graph_Error;
      end if;
   end Finalize;


   procedure Finalize (G : in out Abstract_Graph) is
   begin
      Clear (G);
   end Finalize;


   procedure Adjust (V : in out Abstract_Vertex) is
   begin
      if V.Rep /= null then
         V.Rep.Count := V.Rep.Count + 1;
      end if;
   end Adjust;


   procedure Finalize (V : in out Abstract_Vertex) is
      Curr : Arc_Node_Ptr;
   begin
      if V.Rep /= null then
         if V.Rep.Count > 1 then
            V.Rep.Count := V.Rep.Count - 1;
         else
            while V.Rep.Incoming /= null loop
               Curr := V.Rep.Incoming;
               V.Rep.Incoming := Curr.Next_Incoming;
               Curr.To := null;
               Curr.Next_Incoming := null;
               Curr.Enclosing := null;
               if Curr.Count > 1 then
                  Curr.Count := Curr.Count - 1;
               else
                  Delete (Curr);
               end if;
            end loop;
            while V.Rep.Outgoing /= null loop
               Curr := V.Rep.Outgoing;
               V.Rep.Outgoing := Curr.Next_Outgoing;
               Curr.From := null;
               Curr.Next_Outgoing := null;
               Curr.Enclosing := null;
               if Curr.Count > 1 then
                  Curr.Count := Curr.Count - 1;
               else
                  Delete (Curr);
               end if;
            end loop;
            Clear (V);
         end if;
      end if;
   end Finalize;


   procedure Adjust (A : in out Abstract_Arc) is
   begin
      if A.Rep /= null then
         A.Rep.Count := A.Rep.Count + 1;
      end if;
   end Adjust;


   procedure Finalize (A : in out Abstract_Arc) is
      Prev, Curr : Arc_Node_Ptr;
   begin
      if A.Rep /= null then
         if A.Rep.Count > 1 then
            A.Rep.Count := A.Rep.Count - 1;
         else
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
               if A.Rep.To.Count > 1 then
                  A.Rep.To.Count := A.Rep.To.Count - 1;
               else
                  Delete (A.Rep.To);
               end if;
               A.Rep.Count := A.Rep.Count - 1;
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
               if A.Rep.From.Count > 1 then
                  A.Rep.From.Count := A.Rep.From.Count - 1;
               else
                  Delete (A.Rep.From);
               end if;
               --  XXX bug in C++ here?
               A.Rep.Count := A.Rep.Count - 1;
            end if;
            Clear (A);
         end if;
      end if;
   end Finalize;


end BC.Graphs;
