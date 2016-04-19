--  Copyright 1994 Grady Booch
--  Copyright 1994-1997 David Weller
--  Copyright 1998-2009 Simon Wright <simon@pushface.org>
--  Copyright 2005 Martin Krischik

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

--  This package is the root of a tree of container packages that
--  support indefinite non-limited types (for example, String).
--
--  Storage for the container itself is either stack-based (for
--  Bounded forms) or managed via the storage pool provided by the
--  user at instantiation of the Unbounded form.
--
--  Storage for the contained elements is provided via the generic
--  parameter Item_Ptr.

generic
   type Item (<>) is private;
   type Item_Ptr is access Item;
   with function "=" (L, R : Item) return Boolean is <>;
package BC.Indefinite_Containers is

   pragma Preelaborate;

   --  This package specifies the common protocol of all Container
   --  classes. This common protocol consists of Iterators.

   type Container is abstract tagged private;

   function Null_Container return Container is abstract;

   --  Active iteration

   type Iterator (<>) is abstract tagged private;

   function New_Iterator (For_The_Container : Container) return Iterator'Class
      is abstract;
   --  Return a reset Iterator bound to the specific Container.

   procedure Reset (It : in out Iterator) is abstract;
   --  Reset the Iterator to the beginning.

   procedure Next (It : in out Iterator) is abstract;
   --  Advance the Iterator to the next Item in the Container.

   function Is_Done (It : Iterator) return Boolean is abstract;
   --  Return True if there are no more Items in the Container.

   function Current_Item (It : Iterator) return Item is abstract;
   --  Return a copy of the current Item.

   generic
      with procedure Apply (Elem : in out Item);
   procedure Access_Current_Item (In_The_Iterator : Iterator'Class);
   --  Call Apply for the Iterator's current Item.

   procedure Delete_Item_At (It : in out Iterator) is abstract;
   --  Remove the current item.

   --  Passive iteration

   generic
      with procedure Apply (Elem : in Item; OK : out Boolean);
   procedure Visit (Using : in out Iterator'Class);
   --  Call Apply with a copy of each Item in the Container to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in Item;
                            Param : in Param_Type;
                            OK : out Boolean);
   procedure Visit_With_In_Param (Using : in out Iterator'Class;
                                  Param : in Param_Type);
   --  Call Apply with a Parameter for each Item in the Container to
   --  which the iterator Using is bound. The iteration will terminate
   --  early if Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in Item;
                            Param : in out Param_Type;
                            OK : out Boolean);
   procedure Visit_With_In_Out_Param (Using : in out Iterator'Class;
                                      Param : in out Param_Type);
   --  Call Apply with a Parameter for each Item in the Container to
   --  which the iterator Using is bound. The iteration will terminate
   --  early if Apply sets OK to False.

   generic
      with procedure Apply (Elem : in out Item; OK : out Boolean);
   procedure Modify (Using : in out Iterator'Class);
   --  Call Apply with a copy of each Item in the Container to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in out Item;
                            Param : in Param_Type;
                            OK : out Boolean);
   procedure Modify_With_In_Param (Using : in out Iterator'Class;
                                   Param : in Param_Type);
   --  Call Apply with a Parameter each Item in the Container to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

   generic
      type Param_Type (<>) is limited private;
      with procedure Apply (Elem : in out Item;
                            Param : in out Param_Type;
                            OK : out Boolean);
   procedure Modify_With_In_Out_Param (Using : in out Iterator'Class;
                                       Param : in out Param_Type);
   --  Call Apply with a copy of each Item in the Container to which
   --  the iterator Using is bound. The iteration will terminate early
   --  if Apply sets OK to False.

private

   --  Suppress "unreferenced" warnings here (GNAT 5.02). Can't use
   --  pragma Unreferenced, because then we get warnings in child
   --  packages.
   pragma Warnings (Off, "=");

   type Container is abstract tagged null record;

   --  Private primitive operations of Container.  These should
   --  ideally be abstract; instead, we provide implementations, but
   --  they raise Should_Have_Been_Overridden.

   function Item_At (C : Container; Index : Positive) return Item_Ptr;

   --  Iteration

   type Container_Ptr is access all Container'Class;
   for Container_Ptr'Storage_Size use 0;

   type Iterator is abstract tagged record
      For_The_Container : Container_Ptr;
   end record;

   --  Private primitive operations of Iterator.  These should ideally
   --  be abstract; instead, we provide implementations, but they
   --  raise Should_Have_Been_Overridden.
   function Current_Item_Ptr (It : Iterator) return Item_Ptr;

end BC.Indefinite_Containers;
