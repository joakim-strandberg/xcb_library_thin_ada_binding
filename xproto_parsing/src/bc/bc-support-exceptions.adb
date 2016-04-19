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

--  $Revision: 1391 $
--  $Date: 2009-01-12 20:55:33 +0000 (Mon, 12 Jan 2009) $
--  $Author: simonjwright $

package body BC.Support.Exceptions is


   procedure Report
     (The_Exception : Ada.Exceptions.Exception_Occurrence;
      To : Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output) is
      use Ada.Text_IO;
   begin
      if Ada.Exceptions.Exception_Message (The_Exception)'Length = 0 then
         Put_Line (File => To,
                   Item => "Exception "
                     & Ada.Exceptions.Exception_Name (The_Exception)
                     & " occurred.");
      else
         Put_Line (File => To,
                   Item => "Exception "
                     & Ada.Exceptions.Exception_Name (The_Exception)
                     & " ("
                     & Ada.Exceptions.Exception_Message (The_Exception)
                     & ") occurred.");
      end if;
   end Report;


end BC.Support.Exceptions;
