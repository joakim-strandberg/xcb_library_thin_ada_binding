package Aida.Text_IO is

   -- Wrapper around Ada.Test_IO.Put_Line.
   -- If one uses Ada.Text_IO.Put_Line directly one will get Warning from GNATProve
   -- that Put_Line lacks a global aspect.
   procedure Put_Line (Item : String);

   -- Wrapper around Ada.Test_IO.Get_Line.
   function Get_Line return String;

end Aida.Text_IO;
