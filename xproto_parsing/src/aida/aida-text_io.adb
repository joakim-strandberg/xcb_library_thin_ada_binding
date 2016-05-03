with Ada.Text_IO;

package body Aida.Text_IO is

   procedure Put_Line (Item : String)
   is
   begin
      Ada.Text_IO.Put_Line(Item);
   end Put_Line;

   function Get_Line return String is
   begin
      return Ada.Text_IO.Get_Line;
   end Get_Line;

end Aida.Text_IO;
