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
package BC.Graphs.Undirected is

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
                         First : in out Vertex'Class;
                         Second : in out Vertex'Class);
   --  Create a new arc between the given vertices and add it to the
   --  graph, setting the second argument of this function as an alias
   --  to this new arc.

   -----------------------
   -- Vertex operations --
   -----------------------

   function Arity (V : Vertex) return Natural;
   --  Return the number of arcs connected to the vertex; self-arcs
   --  are counted only once.

   --------------------
   -- Arc operations --
   --------------------

   procedure Set_First_Vertex (A : in out Arc;
                               V : access Vertex'Class);
   --  Change the first vertex of the arc to the given vertex. This
   --  change requires that the arc be removed from the collection of
   --  arcs in the original vertex and added to the collection of arcs
   --  in the given vertex.

   procedure Set_Second_Vertex (A : in out Arc;
                                V : access Vertex'Class);
   --  Change the second vertex of the arc to the given vertex. This
   --  change requires that the arc be removed from the collection of
   --  arcs in the original vertex and added to the collection of arcs
   --  in the given vertex.

   procedure First_Vertex (A : Arc;
                           V : in out Vertex'Class);
   --  Return the first vertex associated with the arc.

   procedure Second_Vertex (A : Arc;
                            V : in out Vertex'Class);
   --  Return the second vertex associated with the arc.

   -----------------------
   -- Iteration support --
   -----------------------

   function New_Graph_Iterator (For_The_Graph : Graph)
                               return Graph_Iterator'Class;
   --  Return a reset Iterator bound to the specific Graph.

   function New_Vertex_Iterator (For_The_Vertex : Vertex)
                                return Vertex_Iterator'Class;
   --  Return a reset Iterator bound to the specific Vertex.

private

   type Graph is new Abstract_Graph with null record;
   type Vertex is new Abstract_Vertex with null record;
   type Arc is new Abstract_Arc with null record;

   type Undirected_Graph_Iterator is new Graph_Iterator with record
      Index : Vertex_Node_Ptr;
   end record;

   procedure Reset (It : in out Undirected_Graph_Iterator);

   procedure Next (It : in out Undirected_Graph_Iterator);

   function Is_Done (It : Undirected_Graph_Iterator) return Boolean;

   function Current_Vertex (It : Undirected_Graph_Iterator)
                           return Abstract_Vertex'Class;

   type Undirected_Vertex_Iterator is new Vertex_Iterator with record
      Index : Arc_Node_Ptr;
      Do_Outgoing : Boolean;
   end record;

   procedure Reset (It : in out Undirected_Vertex_Iterator);

   procedure Next (It : in out Undirected_Vertex_Iterator);

   function Is_Done (It : Undirected_Vertex_Iterator) return Boolean;

   function Current_Arc (It : Undirected_Vertex_Iterator)
                        return Abstract_Arc'Class;

end BC.Graphs.Undirected;
