--  Copyright 1999-2004 Simon Wright <simon@pushface.org>

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

with Ada.Text_IO;
procedure BC.Containers.Trees.AVL.Validate (T : AVL_Tree) is
   use Ada.Text_IO;

   Overall_Depth : Natural;
   pragma Warnings (Off, Overall_Depth);

   function Validate (N : Support.AVL_Node_Ref) return Natural;
   function Validate (N : Support.AVL_Node_Ref) return Natural is

      Left_Depth : Natural;
      Right_Depth : Natural;

      use type Support.AVL_Node_Ref;
      use type Support.Node_Balance;

   begin

      if N = null then
         return 0;
      end if;

      Left_Depth := Validate (N.Left);
      Right_Depth := Validate (N.Right);

      if Left_Depth = Right_Depth then
         if N.Balance /= Support.Middle then
            Put_Line ("depths equal but balance "
                        & Support.Node_Balance'Image (N.Balance));
         end if;
      elsif Left_Depth > Right_Depth then
         if Left_Depth - Right_Depth /= 1 then
            Put_Line ("left depth is"
                        & Natural'Image (Left_Depth - Right_Depth)
                        & " greater than right depth");
         end if;
         if N.Balance /= Support.Left then
            Put_Line ("left deeper than right but balance "
                        & Support.Node_Balance'Image (N.Balance));
         end if;
      else
         if Right_Depth - Left_Depth /= 1 then
            Put_Line ("right depth is"
                        & Natural'Image (Right_Depth - Left_Depth)
                        & " greater than left depth");
         end if;
         if N.Balance /= Support.Right then
            Put_Line ("right deeper than left but balance "
                        & Support.Node_Balance'Image (N.Balance));
         end if;
      end if;

      return 1 + Natural'Max (Left_Depth, Right_Depth);

   end Validate;

begin
   Overall_Depth :=  Validate (T.Rep.Rep);
end BC.Containers.Trees.AVL.Validate;
