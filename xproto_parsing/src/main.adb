with Ada.Direct_IO;
with Ada.Text_IO;
with GNAT.IO_Aux;
with Aida.XML;
with X_Proto_XML;
with XML_File_Parser;
with XCB_Package_Creator;
with Basic_Bounded_Dynamic_Pools;
with Main_Allocator_Interface;

procedure Main is

   Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool (Size           => 120_000_000,
                                                          Heap_Allocated => True);

   package Allocator is

      type T is new Main_Allocator_Interface.T with null record;

      function New_Xcb (This : T) return X_Proto_XML.Xcb.Ptr;

      function New_Struct (This : T) return X_Proto_XML.Struct.Ptr;

      function New_X_Id (This : T) return X_Proto_XML.X_Id.Ptr;

      function New_X_Id_Union (This : T) return X_Proto_XML.X_Id_Union.Ptr;

      function New_Type_Definition (This : T) return X_Proto_XML.Type_Definition.Ptr;

      function New_Enum (This : T) return X_Proto_XML.Enum.Ptr;

      function New_Event (This : T) return X_Proto_XML.Event.Ptr;

      function New_Event_Copy (This : T) return X_Proto_XML.Event_Copy.Ptr;

      function New_Union (This : T) return X_Proto_XML.Union.Ptr;

      function New_Error (This : T) return X_Proto_XML.Error.Ptr;

      function New_Error_Copy (This : T) return X_Proto_XML.Error_Copy.Ptr;

      function New_Request (This : T) return X_Proto_XML.Request.Ptr;

      function New_Field (This : T) return X_Proto_XML.Field.Ptr;

      function New_Pad (This : T) return X_Proto_XML.Pad.Ptr;

      function New_List (This : T) return X_Proto_XML.List.Ptr;

      function New_Struct_Member (This : T;
                                  Kind : X_Proto_XML.Struct.Fs.Member_Kind_Id.Enum_T) return X_Proto_XML.Struct.Fs.Member_Ptr;

      function New_Type (This : T) return X_Proto_XML.Type_P.Ptr;

      function New_Item (This : T) return X_Proto_XML.Item.Ptr;

      function New_Documentation (This : T) return X_Proto_XML.Documentation.Ptr;

      function New_List_Member (This : T;
                                Kind : X_Proto_XML.List.Fs.Member_Kind_Id_Type) return X_Proto_XML.List.Fs.Member_Ptr;

      function New_Operation (This : T) return X_Proto_XML.Operation.Ptr;

      function New_Operation_Member (This : T;
                                     Kind : X_Proto_XML.Operation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Operation.Fs.Member_Ptr;

      function New_Event_Member (This : T;
                                 Kind : X_Proto_XML.Event.Fs.Member_Kind_Id_Type) return X_Proto_XML.Event.Fs.Member_Ptr;

      function New_Documentation_Member (This : T;
                                         Kind : X_Proto_XML.Documentation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Documentation.Fs.Member_Ptr;

      function New_Union_Child (This : T;
                                Kind : X_Proto_XML.Union.Fs.Child_Kind_Id_Type) return X_Proto_XML.Union.Fs.Child_Ptr;

      function New_Error_Child (This : T;
                                Kind : X_Proto_XML.Error.Fs.Child_Kind_Id_Type) return X_Proto_XML.Error.Fs.Child_Ptr;

      function New_Request_Child (This : T;
                                  Kind : X_Proto_XML.Request.Fs.Child_Kind_Id_Type) return X_Proto_XML.Request.Fs.Child_Ptr;

      function New_Reply_Child (This : T;
                                Kind : X_Proto_XML.Reply.Fs.Child_Kind_Id_Type) return X_Proto_XML.Reply.Fs.Child_Ptr;

      function New_Expression_Field_Child (This : T;
                                           Kind : X_Proto_XML.Expression_Field.Fs.Child_Kind_Id_Type) return X_Proto_XML.Expression_Field.Fs.Child_Ptr;


   end Allocator;

   package body Allocator is separate;

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

      A : Allocator.T;
   begin
      File_String_IO.Open  (File, Mode => File_String_IO.In_File,
                            Name => File_Name);
      File_String_IO.Read  (File, Item => Contents);
      File_String_IO.Close (File);

      XML_File_Parser.Parse (Contents,
                             Xcb,
                             Pool,
                             A,
                             Error_Message,
                             Is_Success);

      if Is_Success then
         Ada.Text_IO.Put_Line ("Successfully parsed " & File_Name & "! Will create xcb.ads");
         XCB_Package_Creator.Create_XCB_Package (Xcb.all, Pool);
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
