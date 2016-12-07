with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;
with GNAT.IO_Aux;
with Strings_Edit.UTF8;
with Ada.Characters.Latin_1;
with BC.Indefinite_Unmanaged_Containers.Collections;
with Ada.Strings.Unbounded;
with SXML.Generic_Parse_XML_File;
with X_Proto.XML;
with XCB_Package_Creator;

procedure Main is

   File_Name : String  := "xproto.xml";

   procedure Main_Internal is
      File_Size : Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Is_Success : Boolean;

      Error_Message : SXML.Error_Message_Type;

      Xcb : X_Proto.Xcb.Ptr := null;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      X_Proto.XML.Parse (Contents,
                         Xcb,
                         Error_Message,
                         Is_Success);

      if Is_Success then
         Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
         XCB_Package_Creator.Create_XCB_Package (Xcb.all);
      else
         Ada.Text_IO.Put_Line (Error_Message.To_String);
      end if;
   end Main_Internal;

begin
   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Ada.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
