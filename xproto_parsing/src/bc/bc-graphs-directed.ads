--  Copyright 1994 Grady Booch
--  Copyright 1998-2002 Simon Wright <simon@pushface.org>

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

generic
package BC.Graphs.Directed is

   pragma Preelaborate;

   type Graph is new Abstract_Graph with private;
   type Vertex is new Abstract_Vertex with private;
   type Arc is new Abstract_Arc with private;

   ----------------------
   -- Graph operations --
   ----------------------

   procedure Create_Arc (G : in out Graph;
                         A : in out Arc'Class;
                         I : Arc_Item;
                         From : in out Vertex'Class;
                         To : in out Vertex'Class);
   --  Create a new arc between the given vertices and add it to the
   --  graph, setting the second argument of this function as an alias
   --  to this new arc.

   -----------------------
   -- Vertex operations --
   -----------------------

   function Number_Of_Incoming_Arcs (V : Vertex) return Natural;
   --  Return the number of arcs directed to the vertex.

   function Number_Of_Outgoing_Arcs (V : Vertex) return Natural;
   --  Return the number of arcs emerging from the vertex.

   --------------------
   -- Arc operations --
   --------------------

   procedure Set_From_Vertex (A : in out Arc;
                              V : access Vertex'Class);
   --  Change the source of the arc to the given vertex. This change
   --  requires that the arc be removed from the collection of
   --  outgoing arcs in the original source vertex and added to the
   --  collection of outgoing arcs in the given source vertex.

   procedure Set_To_Vertex (A : in out Arc;
                            V : access Vertex'Class);
   --  Change the destination of the arc to the given vertex. This
   --  change requires that the arc be removed from the collection of
   --  incoming arcs in the original destination vertex and added to
   --  the collection of incoming arcs in the given destination
   --  vertex.

   procedure From_Vertex (A : Arc; V : in out Vertex'Class);
   --  Return the source vertex of the arc.

   procedure To_Vertex (A : Arc; V : in out Vertex'Class);
   --  Return the destination vertex of the arc.

   -----------------------
   -- Iteration support --
   -----------------------

   function New_Graph_Iterator (For_The_Graph : Graph)
                               return Graph_Iterator'Class;
   --  Return a reset Iterator bound to the specific Graph.

   ------------------------------------------------------
   -- Vertex iteration over incoming and outgoing arcs --
   ------------------------------------------------------

   function New_Vertex_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class;
   --  Return a reset Iterator bound to the specific Vertex; both
   --  outgoing and incoming Arcs are visited.

   -----------------------------------------
   -- Vertex iteration over incoming arcs --
   -----------------------------------------

   function New_Vertex_Incoming_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class;
   --  Return a reset Iterator bound to the specific Vertex; only
   --  incoming Arcs are visited.

   -----------------------------------------
   -- Vertex iteration over outgoing arcs --
   -----------------------------------------

   function New_Vertex_Outgoing_Iterator
     (For_The_Vertex : Vertex) return Vertex_Iterator'Class;
   --  Return a reset Iterator bound to the specific Vertex; only
   --  outgoing Arcs are visited.

private

   type Graph is new Abstract_Graph with null record;
   type Vertex is new Abstract_Vertex with null record;
   type Arc is new Abstract_Arc with null record;

   type Directed_Graph_Iterator is new Graph_Iterator with record
      Index : Vertex_Node_Ptr;
   end record;

   procedure Reset (It : in out Directed_Graph_Iterator);

   procedure Next (It : in out Directed_Graph_Iterator);

   function Is_Done (It : Directed_Graph_Iterator) return Boolean;

   function Current_Vertex (It : Directed_Graph_Iterator)
                           return Abstract_Vertex'Class;

   type Vertex_Abstract_Iterator is abstract new Vertex_Iterator with record
      Index : Arc_Node_Ptr;
   end record;

   function Is_Done (It : Vertex_Abstract_Iterator) return Boolean;

   function Current_Arc
     (It : Vertex_Abstract_Iterator) return Abstract_Arc'Class;

   type Vertex_Bothways_Iterator
   is new Vertex_Abstract_Iterator with record
      Do_Outgoing : Boolean;
   end record;

   procedure Reset (It : in out Vertex_Bothways_Iterator);

   procedure Next (It : in out Vertex_Bothways_Iterator);

   type Vertex_Outgoing_Iterator
   is new Vertex_Abstract_Iterator with null record;

   procedure Reset (It : in out Vertex_Outgoing_Iterator);

   procedure Next (It : in out Vertex_Outgoing_Iterator);

   type Vertex_Incoming_Iterator
   is new Vertex_Abstract_Iterator with null record;

   procedure Reset (It : in out Vertex_Incoming_Iterator);

   procedure Next (It : in out Vertex_Incoming_Iterator);

end BC.Graphs.Directed;
