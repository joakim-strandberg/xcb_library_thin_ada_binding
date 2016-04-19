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

with Ada.Finalization;
with System.Storage_Pools;

generic
   type Vertex_Item is private;
   with function "=" (L, R : Vertex_Item) return Boolean is <>;
   type Arc_Item is private;
   with function "=" (L, R : Arc_Item) return Boolean is <>;
   Storage : in out System.Storage_Pools.Root_Storage_Pool'Class;
package BC.Graphs is

   pragma Preelaborate;

   --  A directed graph is an unrooted collection of vertices and
   --  directed arcs where cycles and cross-references are not
   --  permitted. An undirected graph is an unrooted collection of
   --  vertices and undirected arcs where cycles and cross-references
   --  are permitted. Three types collaborate to form the abstraction
   --  of a directed and undirected graph: a graph type, a vertex
   --  type, and an arc type.

   --  Directed and undirected graphs are monolithic structures
   --  although copying, assignment, and equality are
   --  prohibited. Their vertices and arcs are polylithic structures,
   --  and hence the semantics of copying, assignment, and equality
   --  involve structural sharing. Care must be taken in manipulating
   --  the same vertex or arc named by more than one alias.

   --  These classes are not intended to be subclassed.

   --  These abstractions have been carefully constructed to eliminate
   --  all storage leaks, except in the case of intentional
   --  abuses. When a graph is manipulated, all items that become
   --  unreachable are automatically reclaimed. Furthermore, this
   --  design protects against dangling references: an item is never
   --  reclaimed if there exists a reference to it.

   --  Each vertex and arc is a member of exactly one graph;
   --  furthermore, the vertices at both ends of an arc are guaranteed
   --  to be members of the same graph as the arc. This guarantee is
   --  provided by an implementation strategy whereby every graph is
   --  given a unique identity, and each vertex and arc is created
   --  only in the context of a particular graph.

   --  Note that objects of type Vertex, Arc are handles or references
   --  to the actual Graph components.

   --  Note that Containers contain just one sort of Item; Graphs
   --  aren't therefore Containers.

   type Abstract_Graph
      is abstract new Ada.Finalization.Limited_Controlled with private;
   type Graph_Ptr is access all Abstract_Graph'Class;

   type Abstract_Vertex
      is abstract new Ada.Finalization.Controlled with private;

   type Abstract_Arc is abstract new Ada.Finalization.Controlled with private;

   ----------------------
   -- Graph operations --
   ----------------------

   procedure Clear (G : in out Abstract_Graph);
   --  Destroy all the vertices in the graph, and by implication, all
   --  the arcs in the graph. The semantics of destroy are such that
   --  any aliased vertices and arcs are not eliminated from the
   --  graph, because to do so would introduce dangling references.

   procedure Create_Vertex (G : in out Abstract_Graph;
                            V : in out Abstract_Vertex'Class;
                            I : Vertex_Item);
   --  Create a new vertex and add it to the graph, setting the second
   --  argument of this function as an alias to this new vertex.

   --  Arc creation is provided in concrete derivations.

   procedure Destroy_Vertex (G : in out Abstract_Graph;
                             V : in out Abstract_Vertex'Class);
   --  Destroy the given vertex and any associated arcs. If the vertex
   --  has no other aliases, eliminate it from the graph.

   procedure Destroy_Arc (G : in out Abstract_Graph;
                          A : in out Abstract_Arc'Class);
   --  Destroy the given arc. If the arc has no other aliases,
   --  eliminate it from the graph.

   function Number_Of_Vertices (G : Abstract_Graph) return Natural;
   --  Return the number of vertices in the graph.

   function Is_Empty (G : Abstract_Graph) return Boolean;
   --  Return True if and only if the graph does not contain any
   --  vertices or arcs.

   function Is_Member (G : Abstract_Graph;
                       V : Abstract_Vertex'Class) return Boolean;
   --  Return True if and only if the given vertex is not null and
   --  denotes a vertex in the graph.

   function Is_Member (G : Abstract_Graph;
                       A : Abstract_Arc'Class) return Boolean;
   --  Return True if and only if the given arc is not null and
   --  denotes an arc in the graph.

   -----------------------
   -- Vertex operations --
   -----------------------

   function "=" (L, R : Abstract_Vertex) return Boolean;
   --  Return True if and only if both vertices are null or are an
   --  alias to the same vertex.

   procedure Clear (V : in out Abstract_Vertex);
   --  If the vertex is not null, remove this alias.

   procedure Set_Item (V : in out Abstract_Vertex; I : Vertex_Item);
   --  Set the item of the given vertex.

   function Is_Null (V : Abstract_Vertex) return Boolean;
   --  Return True if and only if the vertex is null.

   function Is_Shared (V : Abstract_Vertex) return Boolean;
   --  Return True if and only if the vertex has other aliases.

   function Item (V : Abstract_Vertex) return Vertex_Item;
   --  Return the item associated with the vertex.

   generic
      with procedure Process (I : in out Vertex_Item);
   procedure Access_Vertex_Item (V : Abstract_Vertex'Class);
   --  Process has read-write access to the item associated with the
   --  vertex.

   function Enclosing_Graph (V : Abstract_Vertex) return Graph_Ptr;
   --  Return the graph enclosing the vertex.

   --------------------
   -- Arc operations --
   --------------------

   function "=" (L, R : Abstract_Arc) return Boolean;
   --  Return True if and only if both arcs are null or are an alias
   --  to the same arc.

   procedure Clear (A : in out Abstract_Arc);
   --  If the arc is not null, remove this alias.

   procedure Set_Item (A : in out Abstract_Arc; I : Arc_Item);
   --  Set the item of the given arc.

   function Is_Null (A : Abstract_Arc) return Boolean;
   --  Return 1 if and only if the arc is null.

   function Is_Shared (A : Abstract_Arc) return Boolean;
   --  Return True if and only if the arc has other aliases.

   function Item (A : Abstract_Arc) return Arc_Item;
   --  Return the item associated with the arc.

   generic
      with procedure Process (I : in out Arc_Item);
   procedure Access_Arc_Item (A : Abstract_Arc'Class);
   --  Process has read-write access to the item associated with the arc.

   function Enclosing_Graph (A : Abstract_Arc) return Graph_Ptr;
   --  Return the graph enclosing the arc.

   --------------------------------------------
   -- Iteration over the Vertices in a Graph --
   --------------------------------------------

   --  Active iteration

   type Graph_Iterator (<>) is abstract tagged private;

   function New_Graph_Iterator (For_The_Graph : Abstract_Graph)
                               return Graph_Iterator'Class is abstract;
   --  Return a reset Graph_Iterator bound to the specific Graph.

   procedure Reset (Obj : in out Graph_Iterator) is abstract;
   --  Reset the Graph_Iterator to the beginning.

   procedure Next (Obj : in out Graph_Iterator) is abstract;
   --  Advance the Graph_Iterator to the next Vertex in the Graph.

   function Is_Done (Obj : Graph_Iterator) return Boolean is abstract;
   --  Return True if there are no more Vertices in the Graph.

   function Current_Vertex (Obj : Graph_Iterator) return Abstract_Vertex'Class
      is abstract;
   --  Return the current Vertex.

   --  Passive iteration

   generic
      with procedure Apply (Elem : in Abstract_Vertex'Class; OK : out Boolean);
   procedure Visit_Vertices (Using : in out Graph_Iterator'Class);
   --  Call Apply with a handle on each Vertex in the Graph to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   ---------------------------------------------------
   -- Iteration over the Arcs connected to a Vertex --
   ---------------------------------------------------

   --  Note, in the case of a Directed Graph, this iterator covers
   --  both outgoing and incoming arcs. See BC.Graphs.Directed_Graphs
   --  for separate iteration over either type of arc.

   --  Active iteration

   type Vertex_Iterator (<>) is abstract tagged private;

   function New_Vertex_Iterator (For_The_Vertex : Abstract_Vertex)
                                return Vertex_Iterator'Class is abstract;
   --  Return a reset Vertex_Iterator bound to the specific Vertex.

   procedure Reset (Obj : in out Vertex_Iterator) is abstract;
   --  Reset the Vertex_Iterator to the beginning.

   procedure Next (Obj : in out Vertex_Iterator) is abstract;
   --  Advance the Vertex_Iterator to the next Arc in the Vertex.

   function Is_Done (Obj : Vertex_Iterator) return Boolean is abstract;
   --  Return True if there are no more Arcs in the Vertex.

   function Current_Arc
     (Obj : Vertex_Iterator) return Abstract_Arc'Class is abstract;
   --  Return the current Arc.

   --  Passive iteration

   generic
      with procedure Apply (Elem : in Abstract_Arc'Class; OK : out Boolean);
   procedure Visit_Arcs (Using : in out Vertex_Iterator'Class);
   --  Call Apply with a handle on each Arc in the Vertex to which the
   --  iterator Using is bound. The iteration will terminate early if
   --  Apply sets OK to False.

private


   --  Suppress "unreferenced" warnings here (GNAT 5.02). Can't use
   --  pragma Unreferenced, because then we get warnings in child
   --  packages.
   pragma Warnings (Off, "=");

   type Vertex_Node;
   type Vertex_Node_Ptr is access Vertex_Node;
   for Vertex_Node_Ptr'Storage_Pool use Storage;
   type Arc_Node;
   type Arc_Node_Ptr is access Arc_Node;
   for Arc_Node_Ptr'Storage_Pool use Storage;

   --  A Vertex Node is a simple node consisting of an item, a pointer
   --  to the enclosing graph, a pointer to the next vertex, pointers
   --  to the outgoing and incoming arcs, and a reference count
   --  XXX controlled only for check at finalization
   type Vertex_Node is new Ada.Finalization.Controlled with record
      Item : Vertex_Item;
      Enclosing : Graph_Ptr;
      Incoming : Arc_Node_Ptr;
      Outgoing : Arc_Node_Ptr;
      Next : Vertex_Node_Ptr;
      Count : Natural;
   end record;
   procedure Clear_Vertex_Node (G : in out Abstract_Graph'Class;
                                N : in out Vertex_Node_Ptr);
   procedure Finalize (V : in out Vertex_Node);

   --  An Arc Node is a simple node consisting of an item, a pointer
   --  to the enclosing graph, a pointer to the next arc, pointers to
   --  the vertices to and from the arc, and a reference count
   --  XXX controlled only for check at finalization
   type Arc_Node is new Ada.Finalization.Controlled with record
      Item : Arc_Item;
      Enclosing : Graph_Ptr;
      From : Vertex_Node_Ptr;
      To : Vertex_Node_Ptr;
      Next_Incoming : Arc_Node_Ptr;
      Next_Outgoing : Arc_Node_Ptr;
      Count : Natural;
   end record;
   procedure Finalize (A : in out Arc_Node);

   type Abstract_Graph
   is abstract new Ada.Finalization.Limited_Controlled with record
      Rep : Vertex_Node_Ptr;
   end record;
   procedure Finalize (G : in out Abstract_Graph);

   type Abstract_Vertex is abstract new Ada.Finalization.Controlled with record
      Rep : Vertex_Node_Ptr;
   end record;
   procedure Adjust (V : in out Abstract_Vertex);
   procedure Finalize (V : in out Abstract_Vertex);

   type Abstract_Arc is abstract new Ada.Finalization.Controlled with record
      Rep : Arc_Node_Ptr;
   end record;
   procedure Adjust (A : in out Abstract_Arc);
   procedure Finalize (A : in out Abstract_Arc);

   --  Iteration

   type Graph_Iterator is abstract tagged record
      For_The_Graph : Graph_Ptr;
   end record;

   type Vertex_Ptr is access all Abstract_Vertex'Class;

   type Vertex_Iterator is abstract tagged record
      For_The_Vertex : Vertex_Ptr;
   end record;

end BC.Graphs;
