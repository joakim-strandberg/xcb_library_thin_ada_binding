with Ada.Direct_IO;
with Ada.Text_IO;
with Aida.XML;
with X_Proto_XML;
with XML_File_Parser;
with XCB_Package_Creator;
with Ada.Exceptions;

package body Xcb_Parser is

   use Aida.XML.Error_Message_P;

   File_Name : constant String  := "xproto.xml";

   package Parser renames XML_File_Parser;

   package Creator renames XCB_Package_Creator;

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

   procedure Run is

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

      Parser.Parse (Contents,
                    Xcb,
                    Error_Message,
                    Is_Success);

      if Is_Success then
         Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
         Creator.Create_XCB_Package (Xcb.all);
      else
         Ada.Text_IO.Put_Line (To_String (Error_Message));
      end if;
   exception
      when File_String_IO.Name_Error =>
         Ada.Text_IO.Put_Line ("Could not find file!");
      when Unknown_Exception : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (Unknown_Exception));   end Run;
end Xcb_Parser;
