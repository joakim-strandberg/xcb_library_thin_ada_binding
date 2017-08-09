with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Exceptions;
with XProto_XML.Storage;
with XProto_XML.Max_Indices;
with Ada.Unchecked_Deallocation;
with XProto_XML.Parser;
with Aida.XML;
with Aida.Text_IO;

procedure XML_Parser is
--   use Aida.XML.Error_Message_P;

   use all type XProto_XML.Storage.Header_Comment_T;

   use all type Aida.XML.Procedure_Call_Result.T;

   File_Name : constant String  := "xproto.xml";

--     package X_Proto_XML is new Generic_X_Proto_XML (Size => 220_000_000);
--
--     package Parser is new XML_File_Parser (X_Proto_XML);
--
--     package Creator is new XCB_Package_Creator (X_Proto_XML);

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

--     Is_Success : Boolean;

--     Error_Message : Aida.XML.Error_Message_P.T;
--
--     Xcb : X_Proto_XML.Xcb.Ptr := null;

   type Globals_T is record
      Storage     : XProto_XML.Storage.T;
      Max_Indices : XProto_XML.Max_Indices.T;
   end record;

   type Global_Ptr is access Globals_T;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Globals_T,
                                                     Name   => Global_Ptr);

   Globals : Global_Ptr;

   Call_Result : Aida.XML.Procedure_Call_Result.T;
begin
   File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                         Name => File_Name);
   File_String_IO.Read  (File, Item => Contents);
   File_String_IO.Close (File);

   Globals := new Globals_T;

   XProto_XML.Parser.Parse (Storage     => Globals.Storage,
                            Max_Indices => Globals.Max_Indices,
                            Contents    => Aida.String_T (Contents),
                            Call_Result => Call_Result);

   if Has_Failed (Call_Result) then
      Aida.Text_IO.Put_Line (Message (Call_Result));
   else
      Aida.Text_IO.Put_Line ("Parsing of xml file successfull!!");
   end if;

--   Aida.Text_IO.Put_Line (To_String (Globals.Storage.Header_Comment));

   Free (Globals);
--     Parser.Parse (Contents,
--                   Xcb,
--                   Error_Message,
--                   Is_Success);
--
--     if Is_Success then
--        Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
--        Creator.Create_XCB_Package (Xcb.all);
--     else
--        Ada.Text_IO.Put_Line (To_String (Error_Message));
--     end if;
exception
   when File_String_IO.Name_Error =>
      Ada.Text_IO.Put_Line ("Could not find file!");
      Free (Globals);
   when Unknown_Exception : others =>
      Free (Globals);
      Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));
end XML_Parser;
