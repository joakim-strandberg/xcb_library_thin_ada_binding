with Ada.Directories;
with Ada.Direct_IO;
with Ada.Text_IO;
with GNAT.IO_Aux;
--  with Strings_Edit.UTF8;
--  with Ada.Characters.Latin_1;
--  with BC.Indefinite_Unmanaged_Containers.Collections;
--  with Ada.Strings.Unbounded;
with SXML;
with X_Proto.XML;
with XCB_Package_Creator;
--  with Aida.Containers.Bounded_Hash_Map;
--  with Aida.Bounded_String;

procedure Main is

   use SXML.Error_Message_P;

   File_Name : constant String  := "xproto.xml";

   procedure Main_Internal is
      File_Size : constant Natural := Natural (Ada.Directories.Size (File_Name));

      subtype File_String    is String (1 .. File_Size);
      package File_String_IO is new Ada.Direct_IO (File_String);

      File     : File_String_IO.File_Type;
      Contents : File_String;

      Is_Success : Boolean;

      Error_Message : SXML.Error_Message_P.T;

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
         Ada.Text_IO.Put_Line (To_String (Error_Message));
      end if;
   end Main_Internal;

--     package Person_Name is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 100);
--
--     use Person_Name;
--
--     package Car_Name is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 100);
--
--     use Car_Name;
--
--     package Person_Name_To_Car_Name_Map is new Aida.Containers.Bounded_Hash_Map (Key_T             => Person_Name.T,
--                                                                                  Element_T         => Car_Name.T,
--                                                                                  Hash              => Person_Name.Hash32,
--                                                                                  Equivalent_Keys   => Person_Name."=",
--                                                                                  "="               => Car_Name."=",
--                                                                                  Max_Hash_Map_Size => 3);
--
--     use Person_Name_To_Car_Name_Map;
--
--     Map : Person_Name_To_Car_Name_Map.T;
--
--     Adam : Person_Name.T;
--
--     Bertil : Person_Name.T;
--
--     BMW : Car_Name.T;

begin
--     Initialize (This => Adam,
--                 Text => "Adam");
--
--     Initialize (This => Bertil,
--                 Text => "Bertil");
--
--     Initialize (This => BMW,
--                 Text => "BMW");
--
--     Insert (This    => Map,
--             Key     => Adam,
--             Element => BMW);
--
--     Ada.Text_IO.Put_Line ("Adam exists " & (To_String (Element (Map, Adam))));

   if not GNAT.IO_Aux.File_Exists (File_Name) then
      Ada.Text_IO.Put_Line ("Could not find file!");
      return;
   end if;

   Main_Internal;
end Main;
