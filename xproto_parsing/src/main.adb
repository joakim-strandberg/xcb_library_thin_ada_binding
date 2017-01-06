with Ada.Direct_IO;
with Ada.Text_IO;
with GNAT.IO_Aux;
with Aida.XML;
with X_Proto_XML;
with XML_File_Parser;
with XCB_Package_Creator;
with Bounded_Dynamic_Pools;

procedure Main is

   Mega_Pool : aliased Bounded_Dynamic_Pools.Dynamic_Pool (Default_Subpool_Size => 10_000_000,
                                                           Maximum_Subpools     => 2);

   Subpool : Bounded_Dynamic_Pools.Scoped_Subpool (Pool           => Mega_Pool'Unchecked_Access,
                                                   Size           => 10_000_000,
                                                   Heap_Allocated => True);

   use Aida.XML.Error_Message_P;

   File_Name : constant String  := "xproto.xml";

   procedure Main_Internal is

      function Determine_File_Size return Natural is

         type Char_T is new Character;
         for Char_T'Size use 8;

         package DIO is new Ada.Direct_IO (Char_T);

         File : DIO.File_Type;

         C : DIO.Count;
      begin
         DIO.Open (File => File,
                   Mode => DIO.In_File,
                   Name => File_Name);

         C := DIO.Size (File);

         DIO.Close (File);
         return Natural (C);
      end Determine_File_Size;

      File_Size : constant Natural := Determine_File_Size;

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Is_Success : Boolean;

      Error_Message : Aida.XML.Error_Message_P.T;

      Xcb : X_Proto_XML.Xcb.Ptr := null;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      XML_File_Parser.Parse (Contents,
                             Xcb,
                             Subpool,
                             Error_Message,
                             Is_Success);

      if Is_Success then
         Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
         XCB_Package_Creator.Create_XCB_Package (Xcb.all, Subpool);
      else
         Ada.Text_IO.Put_Line (To_String (Error_Message));
      end if;
   end Main_Internal;

begin
   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Ada.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
