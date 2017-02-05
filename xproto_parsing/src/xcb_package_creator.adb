with Aida;
with Ada.Text_IO;
with GNAT.Source_Info;
with Aida.Containers.Bounded_Vector;
with Aida.Containers.Bounded_Hash_Map;
--      if checked:
--          _h(' * This form can be used only if the request will not cause')
--          _h(' * a reply to be generated. Any returned error will be')
--          _h(' * saved for handling by xcb_request_check().')
--      if unchecked:
--          _h(' * This form can be used only if the request will cause')
--          _h(' * a reply to be generated. Any returned error will be')
--          _h(' * placed in the event queue.')

package body XCB_Package_Creator is

   use type X_Proto_XML.Value_Type;
   use type X_Proto_XML.Request.Fs.Child_Kind_Id_Type;
   use type Aida.Int32.T;

   use X_Proto_XML.Large_Bounded_String;

   use Aida.Int32;
   use Aida.UTF8_Code_Point;

   use X_Proto_XML.Xcb.Fs.Struct_Vector;
   use X_Proto_XML.Xcb.Fs.X_Id_Vector;
   use X_Proto_XML.Xcb.Fs.X_Id_Union_Vector;
   use X_Proto_XML.Enum.Fs.Item_Vector;
   use X_Proto_XML.Request.Fs.Child_Vector;
   use X_Proto_XML.Reply.Fs.Child_Vector;
   use X_Proto_XML.List.Fs.Member_Vector;
   use X_Proto_XML.Xcb.Fs.Request_Vector;
   use X_Proto_XML.Struct.Fs.Member_Vector;
   use X_Proto_XML.X_Id_Union.Fs.Type_Vector;
   use X_Proto_XML.Union.Fs.Child_Vector;
   use X_Proto_XML.Event.Fs.Member_Vector;
   use X_Proto_XML.Error.Fs.Child_Vector;
   use X_Proto_XML.X_Id;
   use X_Proto_XML.Xcb;
   use X_Proto_XML.Struct;
   use X_Proto_XML.Union;
   use X_Proto_XML.Enum;
   use X_Proto_XML.X_Id_Union;
   use X_Proto_XML.List;
   use X_Proto_XML.Event;
   use X_Proto_XML.Event_Copy;
   use X_Proto_XML.Documentation;
   use X_Proto_XML.Error;
   use X_Proto_XML.Error_Copy;
   use X_Proto_XML.Request;
   use X_Proto_XML.Reply;
   use X_Proto_XML.Expression_Field;
   use X_Proto_XML.Field;
   use X_Proto_XML.Type_Definition;
   use X_Proto_XML.Pad;
   use X_Proto_XML.Item;
   use X_Proto_XML.See;
   use X_Proto_XML.Value_Param;
   use X_Proto_XML.Type_P;
   use X_Proto_XML.Example;
   use X_Proto_XML.Struct.Fs.Member_Kind_Id;

   package Unbounded_String_Vector_P is new Aida.Containers.Bounded_Vector (Element_T  => X_Proto_XML.Large_Bounded_String.T,
                                                                            "="        => X_Proto_XML.Large_Bounded_String."=",
                                                                            MAX_LENGTH => 1_000);

   use Unbounded_String_Vector_P;

   subtype Unbounded_String_Vector_T is Unbounded_String_Vector_P.T;

   type Unbounded_String_Vector_Ptr is access all Unbounded_String_Vector_T;
   for Unbounded_String_Vector_Ptr'Storage_Pool use X_Proto_XML.Pool;

   Processed_X_Ids : constant Unbounded_String_Vector_Ptr := new Unbounded_String_Vector_T;

   Eight_Bit_Variable_Type_Names : constant Unbounded_String_Vector_Ptr := new Unbounded_String_Vector_T;

   Thirty_Two_Bit_Variable_Type_Names : constant Unbounded_String_Vector_Ptr := new Unbounded_String_Vector_T;

   package Original_Name_To_Adaified_Name_P is new Aida.Containers.Bounded_Hash_Map (Key_T             => X_Proto_XML.Large_Bounded_String.T,
                                                                                     Element_T         => X_Proto_XML.Large_Bounded_String.T,
                                                                                     Hash              => X_Proto_XML.Large_Bounded_String.Hash32,
                                                                                     Equivalent_Keys   => X_Proto_XML.Large_Bounded_String."=",
                                                                                     Max_Hash_Map_Size => 501,
                                                                                     Max_Collisions    => 5);

   use Original_Name_To_Adaified_Name_P;

   subtype Original_Name_To_Adaified_Name_T is Original_Name_To_Adaified_Name_P.T;

   type Original_Name_To_Adaified_Name_Ptr is access Original_Name_To_Adaified_Name_T;
   for Original_Name_To_Adaified_Name_Ptr'Storage_Pool use X_Proto_XML.Pool;

   package Enum_Name_To_Size_Identifier_Map_P is new Aida.Containers.Bounded_Hash_Map (Key_T             => X_Proto_XML.Large_Bounded_String.T,
                                                                                       Element_T         => X_Proto_XML.Large_Bounded_String.T,
                                                                                       Hash              => X_Proto_XML.Large_Bounded_String.Hash32,
                                                                                       Equivalent_Keys   => X_Proto_XML.Large_Bounded_String."=",
                                                                                       Max_Hash_Map_Size => 501,
                                                                                       Max_Collisions    => 5);

   subtype Enum_Name_To_Size_Identifier_Map_T is Enum_Name_To_Size_Identifier_Map_P.T;

   type Enum_Name_To_Size_Identifier_Map_Ptr is access Enum_Name_To_Size_Identifier_Map_T;
   for Enum_Name_To_Size_Identifier_Map_Ptr'Storage_Pool use X_Proto_XML.Pool;

   use Enum_Name_To_Size_Identifier_Map_P;

   function Value_Of_Bit (B : X_Proto_XML.Item.Fs.Bit_Type) return Long_Integer is
   begin
      return 2 ** Integer (B);
   end Value_Of_Bit;

   procedure Generate_Struct_Name (Old_Name : String;
                                   New_Name : in out X_Proto_XML.Large_Bounded_String.T)
   is
      P : Integer := Old_Name'First;

      CP : Aida.Code_Point_T := 0;

      Is_Previous_Lowercase : Boolean := False;
      Is_Previous_A_Number  : Boolean := False;
      Is_Previous_An_Undercase  : Boolean := False;
   begin
      if Old_Name = "CHAR2B" then
         Initialize (New_Name, "Char_2B");
         return;
      end if;

      if Old_Name = "VISUALTYPE" then
         Initialize (New_Name, "Visual_Kind");
         return;
      end if;

      if Old_Name = "TIMECOORD" then
         Initialize (New_Name, "Time_Coordinate");
         return;
      end if;

      if Old_Name = "FONTPROP" then
         Initialize (New_Name, "Font_Properties");
         return;
      end if;

      if Old_Name = "CHARINFO" then
         Initialize (New_Name, "Character_Information");
         return;
      end if;

      if Old_Name = "COLORITEM" then
         Initialize (New_Name, "Color_Item");
         return;
      end if;

      if Old_Name = "RGB" then
         Initialize (New_Name, "Red_Green_Blue");
         return;
      end if;

      if Old_Name = "VISUALID" then
         Initialize (New_Name, "Visual_Id");
         return;
      end if;

      if Old_Name = "BUTTON" then
         Initialize (New_Name, "Button_Id");
         return;
      end if;

      if Old_Name = "GetPropertyType" then
         Initialize (New_Name, "Get_Property_Kind");
         return;
      end if;

      if Old_Name = "new" then
         Initialize (New_Name, "U_New"); -- The same as in the C header file.
         return;
      end if;

      if Old_Name = "delta" then
         Initialize (New_Name, "U_Delta");
         return;
      end if;

      if Old_Name = "type" then
         Initialize (New_Name, "Kind");
         return;
      end if;

      if Old_Name = "string" then
         Initialize (New_Name, "Text");
         return;
      end if;

      Initialize (New_Name, "");
      Aida.UTF8.Get (Source  => Old_Name,
                     Pointer => P,
                     Value   => CP);

      if Is_Uppercase (CP) then
         Append (New_Name, Image (CP));
      else
         Append (New_Name, Image (To_Uppercase (CP)));
      end if;

      while P <= Old_Name'Last loop
         Aida.UTF8.Get (Source  => Old_Name,
                        Pointer => P,
                        Value   => CP);

         if Image (CP) = "_" then
            Append (New_Name, "_");
            Is_Previous_An_Undercase := True;
         else
            if Is_Digit (CP) then
               if Is_Previous_A_Number then
                  Append (New_Name, Image (CP));
               else
                  Append (New_Name, "_" & Image (CP));
               end if;

               Is_Previous_A_Number := True;
            else
               if Is_Uppercase (CP) then
                  if Is_Previous_Lowercase then
                     Append (New_Name, "_" & Image (CP));
                     Is_Previous_Lowercase := False;
                  else
                     Append (New_Name, Image (To_Lowercase (CP)));
                  end if;
               else
                  if Is_Previous_An_Undercase then
                     Append (New_Name, Image (To_Uppercase (CP)));
                  else
                     Append (New_Name, Image (CP));
                  end if;
                  Is_Previous_Lowercase := True;
               end if;

               Is_Previous_A_Number := False;
            end if;

            Is_Previous_An_Undercase := False;
         end if;

      end loop;
   end Generate_Struct_Name;

   procedure Generate_Classic_Type_Name (Old_Name : String;
                                         New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Type");
   end Generate_Classic_Type_Name;

   procedure Generate_Classic_Access_Type_Name (Old_Name : String;
                                                New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Access_Type");
   end Generate_Classic_Access_Type_Name;

   procedure Generate_Classic_Iterator_Type_Name (Old_Name : String;
                                                  New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Iterator_Type");
   end Generate_Classic_Iterator_Type_Name;

   procedure Generate_Classic_Iterator_Access_Type_Name (Old_Name : String;
                                                         New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Iterator_Access_Type");
   end Generate_Classic_Iterator_Access_Type_Name;

   procedure Generate_Classic_Variable_Id_Name (Old_Name : String;
                                            New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Id");
   end Generate_Classic_Variable_Id_Name;

   procedure Generate_Classic_Type_Id_Name (Old_Name : String;
                                            New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Id_Type");
   end Generate_Classic_Type_Id_Name;

   procedure Generate_Classic_Access_Type_Id_Name (Old_Name : String;
                                                   New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Id_Access_Type");
   end Generate_Classic_Access_Type_Id_Name;

   procedure Generate_Classic_Iterator_Type_Id_Name (Old_Name : String;
                                                     New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Id_Iterator_Type");
   end Generate_Classic_Iterator_Type_Id_Name;

   procedure Generate_Classic_Iterator_Access_Type_Id_Name (Old_Name : String;
                                                            New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Id_Iterator_Access_Type");
   end Generate_Classic_Iterator_Access_Type_Id_Name;

   procedure Generate_Classic_Event_Type_Name (Old_Name : String;
                                               New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Event_Type");
   end Generate_Classic_Event_Type_Name;

   procedure Generate_Classic_Event_Access_Type_Name (Old_Name : String;
                                                      New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Event_Access_Type");
   end Generate_Classic_Event_Access_Type_Name;

   procedure Generate_Classic_Error_Type_Name (Old_Name : String;
                                               New_Name : in out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      Append (New_Name, "_Error_Type");
   end Generate_Classic_Error_Type_Name;

   procedure Generate_Classic_Event_List_Type_Name (Enum_Name : String;
                                                    List_Name : String;
                                                    New_Name  : in out X_Proto_XML.Large_Bounded_String.T)
   is
      Adafied_List_Name : X_Proto_XML.Large_Bounded_String.T;
   begin
      Generate_Struct_Name (List_Name,
                            Adafied_List_Name);

      Generate_Struct_Name (Enum_Name,
                            New_Name);
      Append (New_Name, "_Event_" & To_String (Adafied_List_Name) & "_Array_Type");
   end Generate_Classic_Event_List_Type_Name;

   procedure Translate_Classic_Variable_Type_Name (Variable_Type_Name : String;
                                                   Is_Success         : out Boolean;
                                                   Translated_Name    : out X_Proto_XML.Large_Bounded_String.T) is
   begin
      Is_Success := True;
      if Variable_Type_Name = "CARD8" then
         Initialize (Translated_Name, "Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "CARD16" then
         Initialize (Translated_Name, "Interfaces.Unsigned_16");
      elsif Variable_Type_Name = "CARD32" then
         Initialize (Translated_Name, "Interfaces.Unsigned_32");
      elsif Variable_Type_Name = "BYTE" then
         Initialize (Translated_Name, "Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "BOOL" then
         Initialize (Translated_Name, "Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "INT8" then
         Initialize (Translated_Name, "Interfaces.Integer_8");
      elsif Variable_Type_Name = "INT16" then
         Initialize (Translated_Name, "Interfaces.Integer_16");
      elsif Variable_Type_Name = "INT32" then
         Initialize (Translated_Name, "Interfaces.Integer_32");
      else
         Is_Success := False;
         Initialize (Translated_Name, "");
         return;
      end if;
   end Translate_Classic_Variable_Type_Name;

   procedure Generate_Classic_Array_Type_Name (Prefix_Name : String;
                                               Field_Name  : String;
                                               New_Name    : in out X_Proto_XML.Large_Bounded_String.T)
   is
      Adafied_List_Name : X_Proto_XML.Large_Bounded_String.T;
   begin
      if Prefix_Name = "CARD32" then
         Initialize (New_Name, "Unsigned_32");
      else
         Generate_Struct_Name (Prefix_Name,
                               New_Name);
      end if;

      if Field_Name /= "" then
         Generate_Struct_Name (Field_Name,
                               Adafied_List_Name);
         Append (New_Name, "_" & To_String (Adafied_List_Name) & "_Array_Type");
      else
         Append (New_Name, "_Array_Type");
      end if;
   end Generate_Classic_Array_Type_Name;

   procedure Create_XCB_Package (XCB : X_Proto_XML.Xcb.T) is
      File   : Ada.Text_IO.File_Type;

      procedure Put_Tabs (N : Natural) is
      begin
         for I in Natural range 1..N loop
            Ada.Text_IO.Put (File => File,
                             Item => "   ");
         end loop;
      end Put_Tabs;

      procedure Put_Line (Text : String) is
      begin
         Ada.Text_IO.Put_Line (File => File,
                               Item => Text);
      end Put_Line;

      procedure Put (Text : String) is
      begin
         Ada.Text_IO.Put (File => File,
                          Item => Text);
      end Put;

      Original_Variable_Name_To_Adaified_Name : constant Original_Name_To_Adaified_Name_Ptr := new Original_Name_To_Adaified_Name_T;

      Original_Name_To_Adaified_Name : constant Original_Name_To_Adaified_Name_Ptr := new Original_Name_To_Adaified_Name_T;

      Original_Name_To_Adaified_Iterator_Type_Name : constant Original_Name_To_Adaified_Name_Ptr := new Original_Name_To_Adaified_Name_T;

      Original_Name_To_Adaified_Iterator_Access_Type_Name : constant Original_Name_To_Adaified_Name_Ptr := new Original_Name_To_Adaified_Name_T;

      procedure Translate_Variable_Type_Name (Variable_Type_Name : String;
                                              Is_Success         : out Boolean;
                                              Translated_Name    : out X_Proto_XML.Large_Bounded_String.T) is
      begin
         Translate_Classic_Variable_Type_Name (Variable_Type_Name,
                                               Is_Success,
                                               Translated_Name);

         if not Is_Success then
            declare
               Searched_For : X_Proto_XML.Large_Bounded_String.T;
            begin
               Initialize (Searched_For, Variable_Type_Name);
               declare
                  FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
                    Find_Element (This => Original_Name_To_Adaified_Name.all,
                                  Key  => Searched_For);
               begin
                  if FER.Exists then
                     Is_Success := True;
                     Translated_Name := FER.Element;
                  end if;
               end;
            end;
         end if;
      end Translate_Variable_Type_Name;

      procedure Translate_To_Variable_Name (Variable_Type_Name : String;
                                            Is_Success         : out Boolean;
                                            Translated_Name    : out X_Proto_XML.Large_Bounded_String.T)
      is
         Searched_For : X_Proto_XML.Large_Bounded_String.T;
      begin
         Initialize (Searched_For, Variable_Type_Name);
         declare
            FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
              Find_Element (Original_Name_To_Adaified_Name.all, Searched_For);
         begin
            if FER.Exists then
               Is_Success := True;
               Translated_Name := FER.Element;
            else
               Is_Success := False;
            end if;
         end;
      end Translate_To_Variable_Name;

      procedure Translate_To_Iterator_Type_Name (Variable_Type_Name : String;
                                                 Is_Success         : out Boolean;
                                                 Translated_Name    : out X_Proto_XML.Large_Bounded_String.T)
      is
         Searched_For : X_Proto_XML.Large_Bounded_String.T;
      begin
         Initialize (Searched_For, Variable_Type_Name);
         declare
            FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
              Find_Element (Original_Name_To_Adaified_Name.all, Searched_For);
         begin
            if FER.Exists then
               Is_Success := True;
               Translated_Name := FER.Element;
            else
               Is_Success := False;
            end if;
         end;
      end Translate_To_Iterator_Type_Name;

      procedure Translate_To_Iterator_Access_Type_Name (Variable_Type_Name : String;
                                                        Is_Success         : out Boolean;
                                                        Translated_Name    : out X_Proto_XML.Large_Bounded_String.T)
      is
         Searched_For : X_Proto_XML.Large_Bounded_String.T;
      begin
         Initialize (Searched_For, Variable_Type_Name);
         declare
            FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
              Find_Element (Original_Name_To_Adaified_Name.all, Searched_For);
         begin
            if FER.Exists then
               Is_Success := True;
               Translated_Name := FER.Element;
            else
               Is_Success := False;
            end if;
         end;
      end Translate_To_Iterator_Access_Type_Name;

      function Determine_Largest_Value (Items : X_Proto_XML.Enum.Fs.Item_Vector.T) return Long_Integer is
         R : Long_Integer := 0;
      begin
         for I in X_Proto_XML.Enum.Fs.Item_Vector.Index_T range 1..Last_Index (Items) loop
            case Kind_Id (Element (Items, I).all) is
               when X_Proto_XML.Item.Fs.Not_Specified =>
                  Ada.Text_IO.Put_Line ("Can never happen");
               when X_Proto_XML.Item.Fs.Specified_As_Value =>
                  if Long_Integer (Value (Element (Items, I).all)) > R then
                     R := Long_Integer (Value (Element (Items, I).all));
                  end if;
               when X_Proto_XML.Item.Fs.Specified_As_Bit =>
                  if Value_Of_Bit (Bit (Element (Items, I).all)) > R then
                     R := Value_Of_Bit (Bit (Element (Items, I).all));
                  end if;
            end case;
         end loop;
         return R;
      end Determine_Largest_Value;

      Generic_Iterator_Type_Name : constant String := "Generic_Iterator_Type";

      procedure Generate_Code_For_Next_Procedure (Name : String) is
         N                         : X_Proto_XML.Large_Bounded_String.T;
         Iterator_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;
         Procedure_Name            : X_Proto_XML.Large_Bounded_String.T;
         C_Function_Name           : X_Proto_XML.Large_Bounded_String.T;
         Is_Success : Boolean;
      begin
         Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (Name) & "_next");

         Translate_To_Variable_Name (Variable_Type_Name => Name,
                                     Is_Success         => Is_Success,
                                     Translated_Name    => N);

         if not Is_Success then
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding variable name.");
         end if;

         Translate_To_Iterator_Access_Type_Name (Variable_Type_Name => Name,
                                                 Is_Success         => Is_Success,
                                                 Translated_Name    => Iterator_Access_Type_Name);

         if Is_Success then
            Initialize (Procedure_Name, To_String (N) & "_Next");

            Put_Tabs (1); Put_Line ("procedure " & To_String (Procedure_Name) & " (I : " & To_String (Iterator_Access_Type_Name) & ");");
            Put_Tabs (1); Put_Line ("pragma Import (C, " & To_String (Procedure_Name) & ", """ & To_String (C_Function_Name) & """);");
            Put_Line ("");
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding iterator type name.");
         end if;
      end Generate_Code_For_Next_Procedure;

      procedure Generate_Code_For_End_Function (Name : String) is
         N                  : X_Proto_XML.Large_Bounded_String.T;
         Iterator_Type_Name : X_Proto_XML.Large_Bounded_String.T;
         Function_Name      : X_Proto_XML.Large_Bounded_String.T;
         C_Function_Name    : X_Proto_XML.Large_Bounded_String.T;
         Is_Success : Boolean;
      begin
         Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (Name) & "_end");

         Translate_To_Variable_Name (Variable_Type_Name => Name,
                                     Is_Success         => Is_Success,
                                     Translated_Name    => N);

         if not Is_Success then
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding variable name.");
         end if;

         Translate_To_Iterator_Type_Name (Variable_Type_Name => Name,
                                          Is_Success         => Is_Success,
                                          Translated_Name    => Iterator_Type_Name);

         if Is_Success then
            Initialize (Function_Name, To_String (N) & "_End");

            Put_Tabs (1); Put_Line ("function " & To_String (Function_Name) & " (I : " & To_String (Iterator_Type_Name) & ") return " & Generic_Iterator_Type_Name & ";");
            Put_Tabs (1); Put_Line ("pragma Import (C, " & To_String (Function_Name) & ", """ & To_String (C_Function_Name) & """);");
            Put_Line ("");
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding iterator access type name.");
         end if;
      end Generate_Code_For_End_Function;

      type How_New_Type_Should_Be_Generated_Type is (
                                                     Use_The_New_Keyword,
                                                     Use_The_Subtype_Keyword
                                                    );

      procedure Generate_Code_For_X_Id (Name      : X_Proto_XML.Large_Bounded_String.T;
                                        Type_Name : String;
                                        How       : How_New_Type_Should_Be_Generated_Type)
      is
         New_Variable_Name                      : X_Proto_XML.Large_Bounded_String.T;
         New_Variable_Type_Name                 : X_Proto_XML.Large_Bounded_String.T;
         New_Variable_Access_Type_Name          : X_Proto_XML.Large_Bounded_String.T;
         New_Variable_Iterator_Type_Name        : X_Proto_XML.Large_Bounded_String.T;
         New_Variable_Iterator_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;
      begin
         Generate_Classic_Variable_Id_Name (Old_Name => To_String (Name),
                                            New_Name => New_Variable_Name);

         Generate_Classic_Type_Id_Name (Old_Name => To_String (Name),
                                        New_Name => New_Variable_Type_Name);
         case How is
            when Use_The_New_Keyword =>
               Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is new " & Type_Name & ";");
            when Use_The_Subtype_Keyword =>
               Put_Tabs (1); Put_Line ("subtype " & To_String (New_Variable_Type_Name) & " is " & Type_Name & ";");
         end case;
         Put_Line ("");

         Generate_Classic_Access_Type_Id_Name (Old_Name => To_String (Name),
                                               New_Name => New_Variable_Access_Type_Name);
         Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
         Put_Line ("");

         Generate_Classic_Iterator_Type_Id_Name (Old_Name => To_String (Name),
                                                 New_Name => New_Variable_Iterator_Type_Name);
         Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Type_Name) & " is record");
         Put_Tabs (2); Put_Line ("Data  : " & To_String (New_Variable_Access_Type_Name) & ";");
         Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
         Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
         Put_Tabs (1); Put_Line ("end record;");
         Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Iterator_Type_Name) & ");");
         Put_Line ("");

         Generate_Classic_Iterator_Access_Type_Id_Name (Old_Name => To_String (Name),
                                                        New_Name => New_Variable_Iterator_Access_Type_Name);
         Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Access_Type_Name) & " is access all " &
                                   To_String (New_Variable_Iterator_Type_Name) & ";");
         Put_Line ("");

         Insert (This        => Original_Variable_Name_To_Adaified_Name.all,
                 Key         => Name,
                 New_Element => New_Variable_Name);

         Insert (This        => Original_Name_To_Adaified_Name.all,
                 Key         => Name,
                 New_Element => New_Variable_Type_Name);

         Insert (This        => Original_Name_To_Adaified_Iterator_Type_Name.all,
                 Key         => Name,
                 New_Element => New_Variable_Iterator_Type_Name);

         Insert (This        => Original_Name_To_Adaified_Iterator_Access_Type_Name.all,
                 Key         => Name,
                 New_Element => New_Variable_Iterator_Access_Type_Name);
      end Generate_Code_For_X_Id;

      function There_Is_No_Value_Param_With_Same_Name_And_Type (Variable_Name      : String;
                                                                Variable_Type_Name : String;
                                                                Children           : X_Proto_XML.Request.Fs.Child_Vector.T) return Boolean is
      begin
         for J in X_Proto_XML.Request.Fs.Child_Vector.Index_T range 1..Last_Index (Children) loop
            if Element (Children, J).Kind_Id = X_Proto_XML.Request.Fs.Child_Value_Param then
               if
                 To_String (Mask_Kind (Element (Children, J).V).Value) = Variable_Type_Name and
                 To_String (Mask_Name (Element (Children, J).V).Value) = Variable_Name
               then
                  return True;
               end if;
            end if;
         end loop;

         return False;
      end There_Is_No_Value_Param_With_Same_Name_And_Type;

      procedure Generate_Request_With_Reply_Code (Function_Name   : X_Proto_XML.Large_Bounded_String.T;
                                                  C_Function_Name : X_Proto_XML.Large_Bounded_String.T;
                                                  Children        : X_Proto_XML.Request.Fs.Child_Vector.T;
                                                  Request_Name    : String;
                                                  Reply_Type_Name : X_Proto_XML.Large_Bounded_String.T)
      is
         Is_First_Parameter : Boolean := True;
      begin
         Put_Tabs (1); Put_Line ("function " & To_String (Function_Name));
         Put_Tabs (1); Put_Line ("  (");
         Put_Tabs (2); Put ("C : Connection_Access_Type");

         for I in X_Proto_XML.Request.Fs.Child_Vector.Index_T range 1..Last_Index (Children) loop
            case Element (Children, I).Kind_Id is
               when X_Proto_XML.Request.Fs.Child_Field =>
                  if Kind(Element (Children, I).F).Exists then
                     if not There_Is_No_Value_Param_With_Same_Name_And_Type (Variable_Name      => To_String (Name (Element (Children, I).F).Value),
                                                                             Variable_Type_Name => To_String (Kind (Element (Children, I).F).Value),
                                                                             Children           => Children)
                     then
                        declare
                           Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                           Is_Success : Boolean;
                        begin
                           Translate_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Element (Children, I).F).Value),
                                                         Is_Success         => Is_Success,
                                                         Translated_Name    => Variable_Type_Name);

                           if Is_Success then
                              declare
                                 Field_Name : X_Proto_XML.Large_Bounded_String.T;
                              begin
                                 Generate_Struct_Name (Old_Name => To_String (Name (Element (Children, I).F).Value),
                                                       New_Name => Field_Name);
                                 Put_Line (";");
                                 if Is_First_Parameter then
                                    Is_First_Parameter := False;
                                 end if;

                                 if Enum (Element (Children, I).F).Exists then
                                    declare
                                       FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
                                         Find_Element (This => Original_Name_To_Adaified_Name.all,
                                                       Key  => Enum (Element (Children, I).F).Value);
                                    begin
                                       if FER.Exists then
                                          Put_Tabs (2); Put (To_String (Field_Name) & " : " & To_String (FER.Element));
                                       else
                                          Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", could not find enum type name " & To_String (Enum (Element (Children, I).F).Value));
                                       end if;
                                    end;
                                 else
                                    Put_Tabs (2); Put (To_String (Field_Name) & " : " & To_String (Variable_Type_Name));
                                 end if;
                              end;
                           else
                              Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & To_String (Kind (Element (Children, I).F).Value));
                           end if;
                        end;
                     end if;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request has field withot type!?");
                  end if;
               when X_Proto_XML.Request.Fs.Child_Pad =>
                  --                          if Members (Event).Element (I).P.Bytes.Value > 1 then
                  --                             declare
                  --                                Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                  --                             begin
                  --                                Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Event).Value),
                  --                                                                       List_Name => "Padding" & Aida.Strings.To_String (Padding_Number),
                  --                                                                       New_Name  => New_Variable_Type_Name);
                  --
                  --                                Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is array (0.." &
                  --                                                          Aida.Strings.To_String (Integer (Members (Event).Element (I).P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                  --                             end;
                  --                          end if;
                  --                          Padding_Number := Padding_Number  + 1;
                  null;
               when X_Proto_XML.Request.Fs.Child_Value_Param =>
                  if
                    Mask_Kind (Element (Children, I).V).Exists and
                    Mask_Name (Element (Children, I).V).Exists and
                    List_Name (Element (Children, I).V).Exists
                  then
                     declare
                        Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                        Is_Success : Boolean;
                     begin
                        Translate_Classic_Variable_Type_Name (Variable_Type_Name => To_String (Mask_Kind (Element (Children, I).V).Value),
                                                              Is_Success         => Is_Success,
                                                              Translated_Name    => Variable_Type_Name);

                        if Is_Success then
                           declare
                              Field_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => To_String (Mask_Name (Element (Children, I).V).Value),
                                                    New_Name => Field_Name);
                              Put_Line (";");
                              if Is_First_Parameter then
                                 Is_First_Parameter := False;
                              end if;
                              Put_Tabs (2); Put_Line (To_String (Field_Name) & " : " & To_String (Variable_Type_Name) & ";");
                           end;

                           declare
                              Field_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => To_String (List_Name (Element (Children, I).V).Value),
                                                    New_Name => Field_Name);

                              Put_Tabs (2); Put (To_String (Field_Name) & " : Value_List_Array");
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request " & Request_Name &
                                                   " has  unexpected or unknown field type name " & To_String (Mask_Kind (Element (Children, I).V).Value));
                        end if;
                     end;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request has value param that misses either mask type, mask name or list name!?");
                  end if;
               when X_Proto_XML.Request.Fs.Child_Documentation =>
                  null;
               when X_Proto_XML.Request.Fs.Child_Reply =>
                  null;
               when X_Proto_XML.Request.Fs.Child_List =>
                  if
                    Name (Element (Children, I).L).Exists and
                    Kind (Element (Children, I).L).Exists
                  then
                     declare
                        Field_Name               : X_Proto_XML.Large_Bounded_String.T;
                        Variable_Array_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                     begin
                        Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Kind (Element (Children, I).L).Value),
                                                          Field_Name  => "",
                                                          New_Name    => Variable_Array_Type_Name);
                        Generate_Struct_Name (Old_Name => To_String (Name (Element (Children, I).L).Value),
                                              New_Name => Field_Name);
                        Put_Line (";");
                        if Is_Empty (Members (Element (Children, I).L).all) then
                           -- Assuming the refence indicates length is already specified
                           Put_Tabs (2); Put_Line (To_String (Field_Name) & "_Length : Interfaces.Unsigned_32;");
                        end if;
                        if To_String (Kind (Element (Children, I).L).Value) = "char" then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : Interfaces.C.Strings.chars_ptr");
                        elsif
                          To_String (Kind (Element (Children, I).L).Value) = "void" or
                          To_String (Kind (Element (Children, I).L).Value) = "STR"
                        then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : System.Address");
                        elsif To_String (Kind (Element (Children, I).L).Value) = "CARD8" then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : access Interfaces.Unsigned_8");
                        elsif To_String (Kind (Element (Children, I).L).Value) = "KEYSYM" then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : access Keysym_Type");
                        elsif To_String (Kind (Element (Children, I).L).Value) = "ATOM" then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : access Atom_Id_Type");
                        elsif To_String (Kind (Element (Children, I).L).Value) = "KEYCODE" then
                           Put_Tabs (2); Put (To_String (Field_Name) & " : access Keycode_Type");
                        else
                           Put_Tabs (2); Put (To_String (Field_Name) & " : " & To_String (Variable_Array_Type_Name));
                        end if;
                     end;
                  end if;
               when X_Proto_XML.Request.Fs.Child_Expression_Field =>
                  null;
            end case;
         end loop;

         Put_Line ("");
         Put_Tabs (1); Put_Line ("  ) return " & To_String (Reply_Type_Name) & ";");
         Put_Tabs (1); Put_Line ("pragma Import (C, " & To_String (Function_Name) & ", """ & To_String (C_Function_Name) & """);");
         Put_Line ("");
      end Generate_Request_With_Reply_Code;

      package Unbounded_String_Vector_P is new Aida.Containers.Bounded_Vector (Element_T  => X_Proto_XML.Large_Bounded_String.T,
                                                                               "="        => X_Proto_XML.Large_Bounded_String."=",
                                                                               MAX_LENGTH => 1000);

      subtype Unbounded_String_Vector_T is Unbounded_String_Vector_P.T;

      type Unbounded_String_Vector_Ptr is access Unbounded_String_Vector_T;
      for Unbounded_String_Vector_Ptr'Storage_Pool use X_Proto_XML.Pool;

      use Unbounded_String_Vector_P;

      Names_Of_Types_To_Make_Array_Types : constant Unbounded_String_Vector_Ptr := new Unbounded_String_Vector_T;

      Enum_Name_To_Size_Identifier_Map : constant Enum_Name_To_Size_Identifier_Map_Ptr := new Enum_Name_To_Size_Identifier_Map_T;

      procedure Pre_Process_Requests is

         procedure Handle_Request (Request : X_Proto_XML.Request.T) is

            procedure Handle_Request_Field (F : X_Proto_XML.Field.T) is
               Is_Success : Boolean;
               Translated_Name : X_Proto_XML.Large_Bounded_String.T;
            begin
               if Enum (F).Exists then
                  Translate_Classic_Variable_Type_Name (Variable_Type_Name => To_String (Kind (F).Value),
                                                        Is_Success         => Is_Success,
                                                        Translated_Name    => Translated_Name);

                  if Is_Success then
                     declare
                        FER : constant Enum_Name_To_Size_Identifier_Map_P.Find_Element_Result_T
                          := Find_Element (Enum_Name_To_Size_Identifier_Map.all, Enum (F).Value);
                     begin
                        if FER.Exists then
                           if FER.Element /= Translated_Name then
                              Ada.Text_IO.Put_Line ("Expected: '" & To_String (Translated_Name) & "', but was: " & To_String (FER.Element) & "'");
                           end if;
                        else
                           Include (This        => Enum_Name_To_Size_Identifier_Map.all,
                                    Key         => Enum (F).Value,
                                    New_Element => Translated_Name);
                        end if;
                     end;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location  & ", could not translate " & To_String (Kind (F).Value));
                  end if;
               else
                  null; -- Far from all fields are expected to have an enum specified
               end if;
            end Handle_Request_Field;

            procedure Process_Request_Child (Request_Child : X_Proto_XML.Request.Fs.Child_Ptr) is
            begin
               case Request_Child.Kind_Id is
                  when X_Proto_XML.Request.Fs.Child_Field            =>
                     Handle_Request_Field (Request_Child.F);
                  when X_Proto_XML.Request.Fs.Child_Pad              =>
                     null;
                  when X_Proto_XML.Request.Fs.Child_Value_Param      =>
                     null;
                  when X_Proto_XML.Request.Fs.Child_Documentation    =>
                     null;
                  when X_Proto_XML.Request.Fs.Child_Reply            =>
                     null;
                  when X_Proto_XML.Request.Fs.Child_List             =>
                     if
                       Is_Empty (Members (Request_Child.L).all) and
                       Name (Request_Child.L).Exists and
                       Kind (Request_Child.L).Exists
                     then
                        if not Contains (Names_Of_Types_To_Make_Array_Types.all, Kind (Request_Child.L).Value) then
                           Append (Names_Of_Types_To_Make_Array_Types.all, Kind (Request_Child.L).Value);
                        end if;
                     end if;
                  when X_Proto_XML.Request.Fs.Child_Expression_Field =>
                     null;
               end case;
            end Process_Request_Child;

         begin
            if Name (Request).Exists then
               for I in X_Proto_XML.Request.Fs.Child_Vector.Index_T range 1..Last_Index (Children (Request).all) loop
                  Process_Request_Child (Element (Children (Request).all, I));
               end loop;
            end if;
         end Handle_Request;

      begin
         for I in X_Proto_XML.Xcb.Fs.Request_Vector.Index_T range 1..Last_Index (Requests (XCB).all) loop
            Handle_Request (Element (Requests (XCB).all, I).all);
         end loop;
      end Pre_Process_Requests;

      procedure Generate_Ada_Code_For_Structs (Structs : X_Proto_XML.Xcb.Fs.Struct_Vector.Elements_Array_T) is

         procedure Handle_Struct (Struct : X_Proto_XML.Struct.T) is

            procedure Generate_Code_For_Struct_Array is
               Padding_Number : Aida.Int32.T := 0;

               procedure Handle_Struct_Child (Child : X_Proto_XML.Struct.Fs.Member_Type) is
               begin
                  case Child.Kind_Id is
                  when Field_Member =>
                     null;
                  when Pad_Member =>
                     if Bytes (Child.P).Value > 1 then
                        declare
                           Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Name (Struct).Value),
                                                             Field_Name  => "Padding" & To_String (Padding_Number),
                                                             New_Name    => Variable_Type_Name);

                           Put_Tabs (1); Put_Line ("type " & To_String (Variable_Type_Name) & " is array (0.." &
                                                     To_String (Aida.Int32.T (Bytes (Child.P).Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                        end;
                     end if;
                     Padding_Number := Padding_Number  + 1;
                  when List_Member =>
                     null; -- This information does not have any impact on resulting Ada code. Why?
                  end case;
               end Handle_Struct_Child;

            begin
               for I in X_Proto_XML.Struct.Fs.Member_Vector.Index_T range 1..Last_Index (Members (Struct).all) loop
                  Handle_Struct_Child (Element (Members (Struct).all, I).all);
               end loop;
            end Generate_Code_For_Struct_Array;

            procedure Generate_Code_For_The_Struct is
               New_Variable_Name                      : X_Proto_XML.Large_Bounded_String.T;
               New_Variable_Type_Name                 : X_Proto_XML.Large_Bounded_String.T;
               New_Variable_Access_Type_Name          : X_Proto_XML.Large_Bounded_String.T;
               New_Variable_Iterator_Type_Name        : X_Proto_XML.Large_Bounded_String.T;
               New_Variable_Iterator_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;

               Padding_Number : Aida.Int32.T := 0;

               procedure Handle_Struct_Child (Child : X_Proto_XML.Struct.Fs.Member_Type) is
               begin
                  case Child.Kind_Id is
                     when Field_Member =>
                        if Kind (Child.F).Exists then
                           declare
                              Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                              Is_Success : Boolean;
                           begin
                              Translate_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Child.F).Value),
                                                            Is_Success         => Is_Success,
                                                            Translated_Name    => Variable_Type_Name);

                              if Is_Success then
                                 declare
                                    Field_Name : X_Proto_XML.Large_Bounded_String.T;
                                 begin
                                    Generate_Struct_Name (Old_Name => To_String (Name (Child.F).Value),
                                                          New_Name => Field_Name);
                                    Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                                 end;
                              else
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & To_String (Kind (Child.F).Value));
                              end if;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                        end if;
                     when Pad_Member =>
                        if Bytes (Child.P).Value = 1 then
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           declare
                              New_Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Name (Struct).Value),
                                                                Field_Name  => "Padding" & To_String (Padding_Number),
                                                                New_Name    => New_Variable_Type_Name);

                              Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & To_String (New_Variable_Type_Name) & ";");
                           end;
                        end if;
                        Padding_Number := Padding_Number + 1;
                     when List_Member =>
                        null; -- This information does not have any impact on resulting Ada code. Why?
                  end case;
               end Handle_Struct_Child;

            begin
               Generate_Struct_Name (Old_Name => To_String (Name (Struct).Value),
                                     New_Name => New_Variable_Name);

               Generate_Classic_Type_Name (Old_Name => To_String (Name (Struct).Value),
                                           New_Name => New_Variable_Type_Name);
               Generate_Classic_Access_Type_Name (Old_Name => To_String (Name (Struct).Value),
                                                  New_Name => New_Variable_Access_Type_Name);

               Generate_Classic_Iterator_Type_Name (Old_Name => To_String (Name (Struct).Value),
                                                    New_Name => New_Variable_Iterator_Type_Name);

               Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is record");

               for I in X_Proto_XML.Struct.Fs.Member_Vector.Index_T range 1..Last_Index (Members (Struct).all) loop
                  Handle_Struct_Child (Element (Members (Struct).all, I).all);
               end loop;

               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Type_Name) & ");");
               Put_Line ("");
               Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
               Put_Line ("");
               Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Type_Name) & " is record");
               Put_Tabs (2); Put_Line ("Data  : " & To_String (New_Variable_Access_Type_Name) & ";");
               Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
               Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Iterator_Type_Name) & ");");
               Put_Line ("");

               Generate_Classic_Iterator_Access_Type_Name (Old_Name => To_String (Name (Struct).Value),
                                                           New_Name => New_Variable_Iterator_Access_Type_Name);
               Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Access_Type_Name) & " is access all " &
                                         To_String (New_Variable_Iterator_Type_Name) & ";");
               Put_Line ("");

               Insert (This        => Original_Variable_Name_To_Adaified_Name.all,
                       Key         => Name (Struct).Value,
                       New_Element => New_Variable_Name);

               Insert (This        => Original_Name_To_Adaified_Name.all,
                       Key         => Name (Struct).Value,
                       New_Element => New_Variable_Type_Name);

               Insert (This        => Original_Name_To_Adaified_Iterator_Type_Name.all,
                       Key         => Name (Struct).Value,
                       New_Element => New_Variable_Iterator_Type_Name);

               Insert (This        => Original_Name_To_Adaified_Iterator_Access_Type_Name.all,
                       Key         => Name (Struct).Value,
                       New_Element => New_Variable_Iterator_Access_Type_Name);
            end Generate_Code_For_The_Struct;

         begin
            if Name (Struct).Exists then
               Generate_Code_For_Struct_Array; -- array type declaration before record type declaration

               Generate_Code_For_The_Struct; -- record type declaration
            else
               Ada.Text_IO.Put_Line ("A struct exists without name attribute");
            end if;
         end Handle_Struct;
      begin
         for I in Structs'Range loop
            Handle_Struct (Structs (I).all);
         end loop;
      end Generate_Ada_Code_For_Structs;

      procedure Generate_Ada_Code_For_Structs is new X_Proto_XML.Xcb.Fs.Struct_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Structs);

      procedure Generate_Ada_Code_For_Event_Constants (Events : X_Proto_XML.Xcb.Fs.Event_Vector.Elements_Array_T) is

         procedure Handle_Event (Event : X_Proto_XML.Event.T) is
         begin
            if
              Number (Event).Exists and
              Name (Event).Exists
            then
               declare
                  Constant_Name : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => To_String (Name (Event).Value),
                                        New_Name => Constant_Name);
                  Initialize (Constant_Name, "XCB_" & Aida.UTF8.To_Uppercase (To_String (Constant_Name)));
                  Put_Tabs (1); Put_Line (To_String (Constant_Name) & " : constant :=" & Number (Event).Value'Img & ";");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event without name or number!?");
            end if;
         end Handle_Event;

      begin
         for I in Events'First..Events'Last loop
            Handle_Event (Events (I).all);
         end loop;

         Put_Line ("");
      end Generate_Ada_Code_For_Event_Constants;

      procedure Generate_Ada_Code_For_Event_Constants is new X_Proto_XML.Xcb.Fs.Event_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Event_Constants);

      procedure Generate_Ada_Code_For_Event_Copy_Constants (Event_Copies : X_Proto_XML.Xcb.Fs.Event_Copy_Vector.Elements_Array_T) is

         procedure Handle_Event_Copy (Event_Copy : X_Proto_XML.Event_Copy.T) is
         begin
            if
              Number (Event_Copy).Exists and
              Name (Event_Copy).Exists
            then
               declare
                  Constant_Name : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => To_String (Name (Event_Copy).Value),
                                        New_Name => Constant_Name);
                  Initialize (Constant_Name, "XCB_" & Aida.UTF8.To_Uppercase (To_String (Constant_Name)));
                  Put_Tabs (1); Put_Line (To_String (Constant_Name) & " : constant :=" & Number (Event_Copy).Value'Img & ";");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event copy without name or number!?");
            end if;
         end Handle_Event_Copy;

      begin
         for I in Event_Copies'First..Event_Copies'Last loop
            Handle_Event_Copy (Event_Copies (I).all);
         end loop;
      end Generate_Ada_Code_For_Event_Copy_Constants;

      procedure Generate_Ada_Code_For_Event_Copy_Constants is new X_Proto_XML.Xcb.Fs.Event_Copy_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Event_Copy_Constants);

      procedure Generate_Ada_Code_For_Error_Constants (Errors : X_Proto_XML.Xcb.Fs.Error_Vector.Elements_Array_T) is

         procedure Handle_Error (Error : X_Proto_XML.Error.T) is
         begin
            if
              Number (Error).Exists and
              Name (Error).Exists
            then
               declare
                  Constant_Name : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => To_String (Name (Error).Value),
                                        New_Name => Constant_Name);
                  Initialize (Constant_Name, "XCB_" & Aida.UTF8.To_Uppercase (To_String (Constant_Name)));
                  Put_Tabs (1); Put_Line (To_String (Constant_Name) & " : constant :=" & Number (Error).Value'Img & ";");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error without name or number!?");
            end if;
         end Handle_Error;

      begin
         for I in Errors'First..Errors'Last loop
            Handle_Error (Errors (I).all);
         end loop;
      end Generate_Ada_Code_For_Error_Constants;

      procedure Generate_Ada_Code_For_Error_Constants is new X_Proto_XML.Xcb.Fs.Error_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Error_Constants);

      procedure Generate_Ada_Code_For_Error_Copy_Constants (Error_Copies : X_Proto_XML.Xcb.Fs.Error_Copy_Vector.Elements_Array_T) is

         procedure Handle_Error_Copy (Error_Copy : X_Proto_XML.Error_Copy.T) is
         begin
            if
              Number (Error_Copy).Exists and
              Name (Error_Copy).Exists
            then
               declare
                  Constant_Name : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => To_String (Name (Error_Copy).Value),
                                        New_Name => Constant_Name);
                  Initialize (Constant_Name, "XCB_" & Aida.UTF8.To_Uppercase (To_String (Constant_Name)));
                  Put_Tabs (1); Put_Line (To_String (Constant_Name) & " : constant :=" & Number (Error_Copy).Value'Img & ";");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error copy without name or number!?");
            end if;
         end Handle_Error_Copy;

      begin
         for I in Error_Copies'First..Error_Copies'Last loop
            Handle_Error_Copy (Error_Copies (I).all);
         end loop;

         Put_Line ("");
      end Generate_Ada_Code_For_Error_Copy_Constants;

      procedure Generate_Ada_Code_For_Error_Copy_Constants is new X_Proto_XML.Xcb.Fs.Error_Copy_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Error_Copy_Constants);

      procedure Generate_Ada_Code_For_Request_Constants (Requests : X_Proto_XML.Xcb.Fs.Request_Vector.Elements_Array_T) is

         procedure Handle_Request (Request : X_Proto_XML.Request.T) is
         begin
            if
              Op_Code (Request).Exists and
              Name (Request).Exists
            then
               declare
                  Constant_Name : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => To_String (Name (Request).Value),
                                        New_Name => Constant_Name);
                  Initialize (Constant_Name, "XCB_" & Aida.UTF8.To_Uppercase (To_String (Constant_Name)));
                  Put_Tabs (1); Put_Line (To_String (Constant_Name) & " : constant :=" & Op_Code (Request).Value'Img & ";");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event without name or number!?");
            end if;
         end Handle_Request;

      begin
         for I in Requests'First..Requests'Last loop
            Handle_Request (Requests (I).all);
         end loop;

         Put_Line ("");
      end Generate_Ada_Code_For_Request_Constants;

      procedure Generate_Ada_Code_For_Request_Constants is new X_Proto_XML.Xcb.Fs.Request_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Request_Constants);

      procedure Generate_Ada_Code_For_X_Id_Unions (X_Id_Unions : X_Proto_XML.Xcb.Fs.X_Id_Union_Vector.Elements_Array_T) is

         procedure Handle_X_Id_Union (X_Id_Union : X_Proto_XML.X_Id_Union.T) is
            Searched_For : X_Proto_XML.Large_Bounded_String.T;
            X_Id_Union_Type_Name : X_Proto_XML.Large_Bounded_String.T;
         begin
            if Name (X_Id_Union).Exists then
               Generate_Code_For_X_Id (Name (X_Id_Union).Value,
                                       "X_Id_Type",
                                       How => Use_The_New_Keyword);

               Initialize (Searched_For, To_String (Name (X_Id_Union).Value));

               declare
                  FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
                    Find_Element (This => Original_Name_To_Adaified_Name.all,
                                  Key => Searched_For);
               begin
                  if FER.Exists then
                     X_Id_Union_Type_Name := FER.Element;

                     for I in 1..Last_Index (Kinds (X_Id_Union).all) loop
                        declare
                           Kind : X_Proto_XML.Type_P.T renames Element (Kinds (X_Id_Union).all, I).all;
                        begin
                           if Value (Kind).Exists then
                              for X_Id_Index in 1..Last_Index (X_Ids (Xcb).all) loop
                                 declare
                                    X_Id : X_Proto_XML.X_Id.Ptr renames Element (X_Ids (Xcb).all, X_Id_Index);
                                 begin
                                    if Name (X_Id.all).Exists then
                                       if To_String (Value (Kind).Value) = To_String (Name (X_Id.all).Value) then
                                          Generate_Code_For_X_Id (Name (X_Id.all).Value,
                                                                  To_String (X_Id_Union_Type_Name),
                                                                  How => Use_The_Subtype_Keyword);

                                          Append (Processed_X_Ids.all, Name (X_Id.all).Value);

                                          exit;
                                       end if;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                                    end if;
                                 end;
                              end loop;
                           else
                              Ada.Text_IO.Put_Line ("xidunion " & To_String (Name (X_Id_Union).Value) & " has errors");
                           end if;
                        end;
                     end loop;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Failed to translate: " & To_String (Searched_For));
                  end if;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end Handle_X_Id_Union;

      begin
         for I in X_Id_Unions'First..X_Id_Unions'Last loop
            Handle_X_Id_Union (X_Id_Unions (I).all);
         end loop;
      end Generate_Ada_Code_For_X_Id_Unions;

      procedure Generate_Ada_Code_For_X_Id_Unions is new X_Proto_XML.Xcb.Fs.X_Id_Union_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_X_Id_Unions);

      procedure Generate_Ada_Code_For_Enums (Enums : X_Proto_XML.Xcb.Fs.Enum_Vector.Elements_Array_T) is

         procedure Handle_Enum (Enum : X_Proto_XML.Enum.T) is
         begin
            if Name (Enum).Exists then
               declare
                  New_Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;

                  Enum_Prefix_Name       : X_Proto_XML.Large_Bounded_String.T;
                  Enum_Value_Name        : X_Proto_XML.Large_Bounded_String.T;
               begin
                  Generate_Classic_Type_Name (Old_Name => To_String (Name (Enum).Value),
                                              New_Name => New_Variable_Type_Name);

                  Generate_Struct_Name (Old_Name => To_String (Name (Enum).Value),
                                        New_Name => Enum_Prefix_Name);

                  if To_String (Name (Enum).Value) = "Atom" then
                     for I in X_Proto_XML.Enum.Fs.Item_Vector.Index_T range 1..Last_Index (Items (Enum).all) loop
                        Generate_Struct_Name (Old_Name => To_String (Name (Element (Items (Enum).all, I).all).Value),
                                              New_Name => Enum_Value_Name);

                        Put_Tabs (1); Put ("XCB_" & Aida.UTF8.To_Uppercase (To_String (Enum_Prefix_Name) & "_" & To_String (Enum_Value_Name)) & " : constant Atom_Id_Type :=");
                        case Kind_Id (Element (Items (Enum).all, I).all) is
                           when X_Proto_XML.Item.Fs.Not_Specified =>
                              Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", should never happen");
                              Put_Line ("0;");
                           when X_Proto_XML.Item.Fs.Specified_As_Value =>
                              Put_Line (Value (Element (Items (Enum).all, I).all)'Img & ";");
                           when X_Proto_XML.Item.Fs.Specified_As_Bit =>
                              Put_Line (Value_Of_Bit (Bit (Element (Items (Enum).all, I).all))'Img & ";");
                        end case;
                     end loop;
                     Put_Tabs (1); Put_Line ("");
                  else
                     declare
                        Largest_Value : constant Long_Integer := Determine_Largest_Value (Items (Enum).all);

                        FER : constant Enum_Name_To_Size_Identifier_Map_P.Find_Element_Result_T :=
                          Find_Element (Enum_Name_To_Size_Identifier_Map.all, Name (Enum).Value);
                     begin
                        if FER.Exists then
                           Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is new " & To_String (FER.Element) &";");
                        else
                           if Largest_Value <= 127 then
                              Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is new Interfaces.Unsigned_8;");
                              Append (Eight_Bit_Variable_Type_Names.all, Name (Enum).Value);
                           else
                              Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is new Interfaces.Unsigned_32;");
                           end if;
                        end if;

                        Insert (This        => Original_Name_To_Adaified_Name.all,
                                Key         => Name (Enum).Value,
                                New_Element => New_Variable_Type_Name);


                        for I in X_Proto_XML.Enum.Fs.Item_Vector.Index_T range 1..Last_Index (Items (Enum).all) loop
                           Generate_Struct_Name (Old_Name => To_String (Name (Element (Items (Enum).all, I).all).Value),
                                                 New_Name => Enum_Value_Name);

                           Put_Tabs (1); Put ("XCB_" & Aida.UTF8.To_Uppercase (To_String (Enum_Prefix_Name) & "_" & To_String (Enum_Value_Name)) & " : constant " &
                                                To_String (New_Variable_Type_Name) & " :=");
                           case Kind_Id (Element (Items (Enum).all, I).all) is
                              when X_Proto_XML.Item.Fs.Not_Specified =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", should never happen");
                                 Put_Line ("0;");
                              when X_Proto_XML.Item.Fs.Specified_As_Value =>
                                 Put_Line (Value (Element (Items (Enum).all, I).all)'Img & ";");
                              when X_Proto_XML.Item.Fs.Specified_As_Bit =>
                                 Put_Line (Value_Of_Bit (Bit (Element (Items (Enum).all, I).all))'Img & ";");
                           end case;
                        end loop;
                        Put_Tabs (1); Put_Line ("");
                     end;
                  end if;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end Handle_Enum;

      begin
         for I in Enums'First..Enums'Last loop
            Handle_Enum (Enums (I).all);
         end loop;
      end Generate_Ada_Code_For_Enums;

      procedure Generate_Ada_Code_For_Enums is new X_Proto_XML.Xcb.Fs.Enum_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Enums);

      procedure Generate_Ada_Code_For_Type_Definitions (Type_Definitions : X_Proto_XML.Xcb.Fs.Type_Definition_Vector.Elements_Array_T) is

         procedure Handle_Type_Definition (Type_Definition : X_Proto_XML.Type_Definition.T) is
            Old_Variable_Type_Name                 : X_Proto_XML.Large_Bounded_String.T;
            New_Variable_Name                      : X_Proto_XML.Large_Bounded_String.T;
            New_Variable_Type_Name                 : X_Proto_XML.Large_Bounded_String.T;
            New_Variable_Access_Type_Name          : X_Proto_XML.Large_Bounded_String.T;
            New_Variable_Iterator_Type_Name        : X_Proto_XML.Large_Bounded_String.T;
            New_Variable_Iterator_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;
            Is_Success : Boolean;
         begin
            if Old_Name (Type_Definition).Exists and New_Name (Type_Definition).Exists then
               Translate_Classic_Variable_Type_Name (Variable_Type_Name => To_String (Old_Name (Type_Definition).Value),
                                                     Is_Success         => Is_Success,
                                                     Translated_Name    => Old_Variable_Type_Name);

               if Is_Success then
                  Generate_Struct_Name (Old_Name => To_String (New_Name (Type_Definition).Value),
                                        New_Name => New_Variable_Name);

                  Generate_Classic_Type_Name (Old_Name => To_String (New_Name (Type_Definition).Value),
                                              New_Name => New_Variable_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is new " & To_String (Old_Variable_Type_Name) & ";");
                  Put_Line ("");

                  Generate_Classic_Access_Type_Name (Old_Name => To_String (New_Name (Type_Definition).Value),
                                                     New_Name => New_Variable_Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
                  Put_Line ("");

                  Generate_Classic_Iterator_Type_Name (Old_Name => To_String (New_Name (Type_Definition).Value),
                                                       New_Name => New_Variable_Iterator_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Type_Name) & " is record");
                  Put_Tabs (2); Put_Line ("Data  : " & To_String (New_Variable_Access_Type_Name) & ";");
                  Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
                  Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Iterator_Type_Name) & ");");
                  Put_Line ("");

                  Generate_Classic_Iterator_Access_Type_Name (Old_Name => To_String (New_Name (Type_Definition).Value),
                                                              New_Name => New_Variable_Iterator_Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Access_Type_Name) & " is access all " &
                                            To_String (New_Variable_Iterator_Type_Name) & ";");
                  Put_Line ("");

                  Insert (This        => Original_Variable_Name_To_Adaified_Name.all,
                          Key         => New_Name (Type_Definition).Value,
                          New_Element => New_Variable_Name);

                  Insert (This        => Original_Name_To_Adaified_Name.all,
                          Key         => New_Name (Type_Definition).Value,
                          New_Element => New_Variable_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Type_Name.all,
                          Key         => New_Name (Type_Definition).Value,
                          New_Element => New_Variable_Iterator_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Access_Type_Name.all,
                          Key         => New_Name (Type_Definition).Value,
                          New_Element => New_Variable_Iterator_Access_Type_Name);

                  if
                    To_String (Old_Name (Type_Definition).Value) = "CARD8" or
                    To_String (Old_Name (Type_Definition).Value) = "BYTE" or
                    Contains (Eight_Bit_Variable_Type_Names.all, Old_Name (Type_Definition).Value)
                  then
                     Append (Eight_Bit_Variable_Type_Names.all, New_Name (Type_Definition).Value);
                  elsif
                    To_String (Old_Name (Type_Definition).Value) = "CARD32"
                  then
                     Append (Thirty_Two_Bit_Variable_Type_Names.all, New_Name (Type_Definition).Value);
                  end if;
               else
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Failed to translate: " & To_String (Old_Name (Type_Definition).Value));
               end if;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end Handle_Type_Definition;

      begin
         for I in Type_Definitions'First..Type_Definitions'Last loop
            Handle_Type_Definition (Type_Definitions (I).all);
         end loop;
      end Generate_Ada_Code_For_Type_Definitions;

      procedure Generate_Ada_Code_For_Type_Definitions is new X_Proto_XML.Xcb.Fs.Type_Definition_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Type_Definitions);

      procedure Generate_Ada_Code_For_Unions (Unions : X_Proto_XML.Xcb.Fs.Union_Vector.Elements_Array_T) is

         procedure Handle_Union (Union : X_Proto_XML.Union.T) is
         begin
            if Name (Union).Exists then
               declare
                  New_Variable_Type_Name                 : X_Proto_XML.Large_Bounded_String.T;
                  New_Variable_Access_Type_Name          : X_Proto_XML.Large_Bounded_String.T;
                  New_Variable_Iterator_Type_Name        : X_Proto_XML.Large_Bounded_String.T;
                  New_Variable_Iterator_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;

                  Discriminant_Number : Integer := 0; -- is increased for each field in the union
               begin
                  for I in X_Proto_XML.Union.Fs.Child_Vector.Index_T range 1..Last_Index (Children (Union).all) loop
                     case Element (Children (Union).all, I).Kind_Id is
                     when X_Proto_XML.Union.Fs.Child_List =>
                        if Last_Index (Members (Element (Children (Union).all, I).L).all) = 1 then
                           Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Name (Union).Value),
                                                             Field_Name  => To_String (Name (Element (Children (Union).all, I).L).Value),
                                                             New_Name    => New_Variable_Type_Name);

                           case Element (Members (Element (Children (Union).all, I).L).all, 1).Kind_Id is
                              when X_Proto_XML.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & To_String (Name (Union).Value) & " with list field child is unimplemented.");
                              when X_Proto_XML.List.Fs.List_Member_Kind_Value =>
                                 declare
                                    Is_Success : Boolean;
                                    N : X_Proto_XML.Large_Bounded_String.T;
                                 begin
                                    Translate_Classic_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Element (Children (Union).all, I).L).Value),
                                                                          Is_Success         => Is_Success,
                                                                          Translated_Name    => N);

                                    if Is_Success then
                                       Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is array (0.." &
                                                                 To_String (Aida.Int32.T (Element (Members (Element (Children (Union).all, I).L).all, 1).Value - 1)) & ") of aliased " & To_String (N) & ";");
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & To_String (Name (Union).Value) & ", failed to identify kind of array item: " &
                                                               To_String (Kind (Element (Children (Union).all, I).L).Value));
                                    end if;
                                 end;
                              when X_Proto_XML.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & To_String (Name (Union).Value) & " with list kind child is unimplemented.");
                           end case;

                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & To_String (Name (Union).Value) & " contains list child with" &
                                                   Last_Index (Members (Element (Children (Union).all, I).L).all)'Img & " number fo children");
                        end if;
                     end case;
                  end loop;

                  Generate_Classic_Type_Name (Old_Name => To_String (Name (Union).Value),
                                              New_Name => New_Variable_Type_Name);

                  Generate_Classic_Access_Type_Name (Old_Name => To_String (Name (Union).Value),
                                                     New_Name => New_Variable_Access_Type_Name);

                  Generate_Classic_Iterator_Type_Name (Old_Name => To_String (Name (Union).Value),
                                                       New_Name => New_Variable_Iterator_Type_Name);

                  Generate_Classic_Iterator_Access_Type_Name (Old_Name => To_String (Name (Union).Value),
                                                              New_Name => New_Variable_Iterator_Access_Type_Name);

                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " (Discriminant : Natural := 0) is record");
                  Put_Tabs (2); Put_Line ("case Discriminant is");

                  for I in X_Proto_XML.Union.Fs.Child_Vector.Index_T range 1..Last_Index (Children (Union).all) loop
                     case Element (Children (Union).all, I).Kind_Id is
                     when X_Proto_XML.Union.Fs.Child_List =>
                        declare
                           Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Name (Union).Value),
                                                             Field_Name  => To_String (Name (Element (Children (Union).all, I).L).Value),
                                                             New_Name    => Variable_Type_Name);

                           declare
                              Field_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => To_String (Name (Element (Children (Union).all, I).L).Value),
                                                    New_Name => Field_Name);

                              if Last_Index (Children (Union).all) /= I then
                                 Put_Tabs (3); Put_Line ("when" & Discriminant_Number'Img & " =>");
                                 Put_Tabs (4); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                              else
                                 Put_Tabs (3); Put_Line ("when others =>");
                                 Put_Tabs (4); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                              end if;
                              Discriminant_Number := Discriminant_Number + 1;
                           end;
                        end;
                     end case;
                  end loop;

                  Put_Tabs (2); Put_Line ("end case;");
                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Type_Name) & ");");
                  Put_Tabs (1); Put_Line ("pragma Unchecked_Union (" & To_String (New_Variable_Type_Name) & ");");
                  Put_Tabs (1); Put_Line ("");
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
                  Put_Line ("");
                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Type_Name) & " is record");
                  Put_Tabs (2); Put_Line ("Data  : " & To_String (New_Variable_Access_Type_Name) & ";");
                  Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
                  Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Iterator_Type_Name) & ");");
                  Put_Line ("");

                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Iterator_Access_Type_Name) & " is access all " &
                                            To_String (New_Variable_Iterator_Type_Name) & ";");
                  Put_Line ("");


                  Insert (This        => Original_Name_To_Adaified_Name.all,
                          Key         => Name (Union).Value,
                          New_Element => New_Variable_Type_Name);
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Union exists without name!");
            end if;
         end Handle_Union;

      begin
         for I in Unions'First..Unions'Last loop
            Handle_Union (Unions (I).all);
         end loop;
      end Generate_Ada_Code_For_Unions;

      procedure Generate_Ada_Code_For_Unions is new X_Proto_XML.Xcb.Fs.Union_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Unions);

      procedure Generate_Ada_Code_For_Events (Events : X_Proto_XML.Xcb.Fs.Event_Vector.Elements_Array_T) is

         procedure Handle_Event (Event : X_Proto_XML.Event.T) is
         begin
            if Name (Event).Exists then
               declare
                  New_Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;

                  Padding_Number : Aida.Int32.T := 0;
               begin
                  for I in X_Proto_XML.Event.Fs.Member_Vector.Index_T range 1..Last_Index (Members (Event).all) loop
                     case Element (Members (Event).all, I).Kind_Id is
                     when X_Proto_XML.Event.Fs.Event_Member_Field =>
                        null;
                     when X_Proto_XML.Event.Fs.Event_Member_Pad =>
                        if Bytes (Element (Members (Event).all, I).P).Value > 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Event).Value),
                                                                  List_Name => "Padding" & To_String (Padding_Number),
                                                                  New_Name  => New_Variable_Type_Name);

                           Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is array (0.." &
                                                     To_String (Aida.Int32.T (Bytes (Element (Members (Event).all, I).P).Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                        end if;
                        Padding_Number := Padding_Number  + 1;
                     when X_Proto_XML.Event.Fs.Event_Member_Doc =>
                        null;
                     when X_Proto_XML.Event.Fs.Event_Member_List =>
                        if Last_Index (Members (Element (Members (Event).all, I).L).all) = 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Event).Value),
                                                                  List_Name => To_String (Name (Element (Members (Event).all, I).L).Value),
                                                                  New_Name  => New_Variable_Type_Name);

                           case Element (Members (Element (Members (Event).all, I).L).all, 1).Kind_Id is
                              when X_Proto_XML.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " with list field child is unimplemented.");
                              when X_Proto_XML.List.Fs.List_Member_Kind_Value =>
                                 declare
                                    Is_Success : Boolean;
                                    N : X_Proto_XML.Large_Bounded_String.T;
                                 begin
                                    Translate_Classic_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Element (Members (Event).all, I).L).Value),
                                                                          Is_Success         => Is_Success,
                                                                          Translated_Name    => N);

                                    if Is_Success then
                                       Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is array (0.." &
                                                                 To_String (Aida.Int32.T (Element (Members (Element (Members (Event).all, I).L).all, 1).Value) - 1) & ") of aliased " & To_String (N) & ";");
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & ", failed to identify kind of array item: " &
                                                               To_String (Kind (Element (Members (Event).all, I).L).Value));
                                    end if;
                                 end;
                              when X_Proto_XML.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " with list kind child is unimplemented.");
                           end case;

                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " contains list child with" &
                                                   Last_Index (Members (Element (Members (Event).all, I).L).all)'Img & " number fo children");
                        end if;
                     end case;
                  end loop;

                  Padding_Number := 0;

                  Generate_Classic_Event_Type_Name (Old_Name => To_String (Name (Event).Value),
                                                    New_Name => New_Variable_Type_Name);

                  Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is record");
                  Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");

                  for I in X_Proto_XML.Event.Fs.Member_Vector.Index_T range 1..Last_Index (Members (Event).all) loop
                     case Element (Members (Event).all, I).Kind_Id is
                     when X_Proto_XML.Event.Fs.Event_Member_Field =>
                        declare
                           Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                           Is_Success : Boolean;
                        begin
                           Translate_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Element (Members (Event).all, I).F).Value),
                                                         Is_Success         => Is_Success,
                                                         Translated_Name    => Variable_Type_Name);

                           if Is_Success then
                              declare
                                 Field_Name : X_Proto_XML.Large_Bounded_String.T;
                              begin
                                 Generate_Struct_Name (Old_Name => To_String (Name (Element (Members (Event).all, I).F).Value),
                                                       New_Name => Field_Name);

                                 if I = 1 then
                                    if
                                      Contains (Eight_Bit_Variable_Type_Names.all, (Kind (Element (Members (Event).all, I).F).Value))
                                    then
                                       if Enum (Element (Members (Event).all, I).F).Exists then
                                          Translate_Variable_Type_Name (Variable_Type_Name => To_String (Enum (Element (Members (Event).all, I). F).Value),
                                                                        Is_Success         => Is_Success,
                                                                        Translated_Name    => Variable_Type_Name);
                                       end if;


                                       Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");

                                       if No_Sequence_Number (Event).Exists and then not No_Sequence_Number (Event).Value then
                                          Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       elsif not No_Sequence_Number (Event).Exists then
                                          Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       end if;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " has first field non-8-bits.");
                                       -- This is interesting because in xproto.xml for lib xcb version 1.10 the
                                       -- first field in all events were 8-bits!

                                       Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                                       Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");

                                       Padding_Number := Padding_Number + 1;
                                    end if;
                                 else
                                    Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                                 end if;
                              end;
                           else
                              Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & To_String (Kind (Element (Members (Event).all, I).F).Value));
                           end if;
                        end;
                     when X_Proto_XML.Event.Fs.Event_Member_Pad =>
                        if Bytes (Element (Members (Event).all, I).P).Value = 1 then
                           Put_Tabs (2); Put_Line (   "Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Event).Value),
                                                                  List_Name => "Padding" & To_String (Padding_Number),
                                                                  New_Name  => New_Variable_Type_Name);
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & To_String (New_Variable_Type_Name) & ";");
                        end if;
                        Padding_Number := Padding_Number + 1;
                     when X_Proto_XML.Event.Fs.Event_Member_Doc =>
                        null;
                     when X_Proto_XML.Event.Fs.Event_Member_List =>
                        if Last_Index (Members (Element (Members (Event).all, I).L).all) = 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Event).Value),
                                                                  List_Name => To_String (Name (Element (Members (Event).all, I).L).Value),
                                                                  New_Name  => New_Variable_Type_Name);

                           declare
                              Variable_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => To_String (Name (Element (Members (Event).all, I).L).Value),
                                                    New_Name => Variable_Name);
                              case Element (Members (Element (Members (Event).all, I).L).all, 1).Kind_Id is
                              when X_Proto_XML.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " with list field child is unimplemented.");
                              when X_Proto_XML.List.Fs.List_Member_Kind_Value =>
                                 Put_Tabs (2); Put_Line (To_String (Variable_Name) & " : aliased " & To_String (New_Variable_Type_Name) & ";");
                              when X_Proto_XML.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " with list kind child is unimplemented.");
                              end case;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & To_String (Name (Event).Value) & " contains list child with" &
                                                   Last_Index (Members (Element (Members (Event).all, I).L).all)'Img & " number fo children");
                        end if;
                     end case;
                  end loop;

                  Generate_Classic_Event_Type_Name (Old_Name => To_String (Name (Event).Value),
                                                    New_Name => New_Variable_Type_Name);

                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Type_Name) & ");");
                  Put_Line ("");
                  declare
                     Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Generate_Classic_Event_Access_Type_Name (Old_Name => To_String (Name (Event).Value),
                                                              New_Name => Access_Type_Name);
                     Put_Tabs (1); Put_Line ("type " & To_String (Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
                     Put_Line ("");
                  end;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end Handle_Event;

      begin
         for I in Events'First..Events'Last loop
            Handle_Event (Events (I).all);
         end loop;
      end Generate_Ada_Code_For_Events;

      procedure Generate_Ada_Code_For_Events is new X_Proto_XML.Xcb.Fs.Event_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Events);

      procedure Generate_Ada_Code_For_Event_Copies (Event_Copies : X_Proto_XML.Xcb.Fs.Event_Copy_Vector.Elements_Array_T) is

         procedure Handle_Event_Copy (Event_Copy : X_Proto_XML.Event_Copy.T) is
         begin
            if Name (Event_Copy).Exists then
               if Ref (Event_Copy).Exists then
                  declare
                     Original_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                     Derived_Type_Name  : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Generate_Classic_Event_Type_Name (Old_Name => To_String (Name (Event_Copy).Value),
                                                       New_Name => Derived_Type_Name);

                     Generate_Classic_Event_Type_Name (Old_Name => To_String (Ref (Event_Copy).Value),
                                                       New_Name => Original_Type_Name);

                     Put_Tabs (1); Put_Line ("type " & To_String (Derived_Type_Name) & " is new " & To_String (Original_Type_Name) & ";");
                     Put_Line ("");
                     declare
                        Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                     begin
                        Generate_Classic_Event_Access_Type_Name (Old_Name => To_String (Name (Event_Copy).Value),
                                                                 New_Name => Access_Type_Name);
                        Put_Tabs (1); Put_Line ("type " & To_String (Access_Type_Name) & " is access all " & To_String (Derived_Type_Name) & ";");
                        Put_Line ("");
                     end;
                  end;
               else
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy " & To_String (Name (Event_Copy).Value) & " with no ref!?");
               end if;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy exists without a name!?");
            end if;
         end Handle_Event_Copy;

      begin
         for I in Event_Copies'First..Event_Copies'Last loop
            Handle_Event_Copy (Event_Copies (I).all);
         end loop;
      end Generate_Ada_Code_For_Event_Copies;

      procedure Generate_Ada_Code_For_Event_Copies is new X_Proto_XML.Xcb.Fs.Event_Copy_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Event_Copies);

      procedure Generate_Ada_Code_For_Errors (Errors : X_Proto_XML.Xcb.Fs.Error_Vector.Elements_Array_T) is

         procedure Handle_Error (Error : X_Proto_XML.Error.T) is
         begin
            if Name (Error).Exists then
               declare
                  Padding_Number : Aida.Int32.T := 0;

                  procedure Handle_Error_Child (Child : X_Proto_XML.Error.Fs.Child_Type) is
                  begin
                     case Child.Kind_Id is
                     when X_Proto_XML.Error.Fs.Child_Field =>
                        null;
                     when X_Proto_XML.Error.Fs.Child_Pad =>
                        if Bytes (Child.P).Value > 1 then
                           declare
                              Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Name (Error).Value),
                                                                Field_Name  => "Padding" & To_String (Padding_Number),
                                                                New_Name    => Variable_Type_Name);

                              Put_Tabs (1); Put_Line ("type " & To_String (Variable_Type_Name) & " is array (0.." &
                                                        To_String (Aida.Int32.T (Bytes (Child.P).Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                           end;
                        end if;
                        Padding_Number := Padding_Number  + 1;
                     end case;
                  end Handle_Error_Child;

               begin
                  for I in 1..Last_Index (Children (Error).all) loop
                     Handle_Error_Child (Element (Children (Error).all, I).all);
                  end loop;
               end;

               declare
                  Error_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                  Padding_Number : Aida.Int32.T := 0;

                  procedure Handle_Error_Child (Child : X_Proto_XML.Error.Fs.Child_Type) is
                  begin
                     case Child.Kind_Id is
                     when X_Proto_XML.Error.Fs.Child_Field =>
                        if Kind (Child.F).Exists then
                           declare
                              Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                              Is_Success : Boolean;
                           begin
                              Translate_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Child.F).Value),
                                                            Is_Success         => Is_Success,
                                                            Translated_Name    => Variable_Type_Name);

                              if Is_Success then
                                 declare
                                    Field_Name : X_Proto_XML.Large_Bounded_String.T;
                                 begin
                                    Generate_Struct_Name (Old_Name => To_String (Name (Child.F).Value),
                                                          New_Name => Field_Name);
                                    Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");

                                    if Enum (Child.F).Exists then
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                                    end if;
                                 end;
                              else
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & To_String (Kind (Child.F).Value));
                              end if;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                        end if;
                     when X_Proto_XML.Error.Fs.Child_Pad =>
                        if Bytes (Child.P).Value = 1 then
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           declare
                              New_Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Event_List_Type_Name (Enum_Name => To_String (Name (Error).Value),
                                                                     List_Name => "Padding" & To_String (Padding_Number),
                                                                     New_Name  => New_Variable_Type_Name);
                              Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & To_String (New_Variable_Type_Name) & ";");
                           end;
                        end if;
                        Padding_Number := Padding_Number + 1;
                     end case;
                  end Handle_Error_Child;

               begin
                  Generate_Classic_Error_Type_Name (Old_Name => To_String (Name (Error).Value),
                                                    New_Name => Error_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & To_String (Error_Type_Name) & " is record");

                  Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                  Put_Tabs (2); Put_Line ("Error_Code : aliased Interfaces.Unsigned_8;");
                  Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");

                  for I in 1..Last_Index (Children (Error).all) loop
                     Handle_Error_Child (Element (Children (Error).all, I).all);
                  end loop;

                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (Error_Type_Name) & ");");
                  Put_Line ("");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Error exists without a name!?");
            end if;
         end Handle_Error;

      begin
         for I in Errors'First..Errors'Last loop
            Handle_Error (Errors (I).all);
         end loop;
      end Generate_Ada_Code_For_Errors;

      procedure Generate_Ada_Code_For_Errors is new X_Proto_XML.Xcb.Fs.Error_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Errors);

      procedure Generate_Ada_Code_For_Error_Copies (Error_Copies : X_Proto_XML.Xcb.Fs.Error_Copy_Vector.Elements_Array_T) is

         procedure Handle_Error_Copy (Error_Copy : X_Proto_XML.Error_Copy.T) is
         begin
            if Name (Error_Copy).Exists then
               if Ref (Error_Copy).Exists then
                  declare
                     Original_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                     Derived_Type_Name  : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Generate_Classic_Error_Type_Name (Old_Name => To_String (Name (Error_Copy).Value),
                                                       New_Name => Derived_Type_Name);

                     Generate_Classic_Error_Type_Name (Old_Name => To_String (Ref (Error_Copy).Value),
                                                       New_Name => Original_Type_Name);

                     Put_Tabs (1); Put_Line ("type " & To_String (Derived_Type_Name) & " is new " & To_String (Original_Type_Name) & ";");
                     Put_Line ("");
                  end;
               else
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy " & To_String (Name (Error_Copy).Value) & " with no ref!?");
               end if;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy exists without a name!?");
            end if;
         end Handle_Error_Copy;

      begin
         for I in Error_Copies'First..Error_Copies'Last loop
            Handle_Error_Copy (Error_Copies (I).all);
         end loop;
      end Generate_Ada_Code_For_Error_Copies;

      procedure Generate_Ada_Code_For_Error_Copies is new X_Proto_XML.Xcb.Fs.Error_Copy_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Error_Copies);

      procedure Generate_Ada_Subprograms_From_Type_Definitions (Type_Definitions : X_Proto_XML.Xcb.Fs.Type_Definition_Vector.Elements_Array_T) is

         procedure Handle_Type_Definition (Type_Def : X_Proto_XML.Type_Definition.T) is
         begin
            if New_Name (Type_Def).Exists then
               Generate_Code_For_Next_Procedure (To_String (New_Name (Type_Def).Value));
               Generate_Code_For_End_Function (To_String (New_Name (Type_Def).Value));
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Type definition exists without new name!?");
            end if;
         end Handle_Type_Definition;

      begin
         for I in Type_Definitions'First..Type_Definitions'Last loop
            Handle_Type_Definition (Type_Definitions (I).all);
         end loop;
      end Generate_Ada_Subprograms_From_Type_Definitions;

      procedure Generate_Ada_Subprograms_From_Type_Definitions is new X_Proto_XML.Xcb.Fs.Type_Definition_Vector.Act_On_Immutable_Elements (Generate_Ada_Subprograms_From_Type_Definitions);

      procedure Generate_Ada_Code_For_Requests (Requests : X_Proto_XML.Xcb.Fs.Request_Vector.Elements_Array_T) is

         procedure Handle_Request (Request : X_Proto_XML.Request.T) is
         begin
            if Name (Request).Exists then
               declare
                  Does_Specified_Reply_Exist : Boolean := False;

                  Request_Reply_Access_Type_Name : X_Proto_XML.Large_Bounded_String.T;

                  Shall_Generate_Size_Of_Function : Boolean := False;

                  procedure Process (Request_Child : X_Proto_XML.Request.Fs.Child_Ptr) is
                  begin
                     case Request_Child.Kind_Id is
                     when X_Proto_XML.Request.Fs.Child_Field =>
                        null;
                     when X_Proto_XML.Request.Fs.Child_Pad =>
                        null;
                     when X_Proto_XML.Request.Fs.Child_Value_Param =>
                        Shall_Generate_Size_Of_Function := True;
                     when X_Proto_XML.Request.Fs.Child_Documentation =>
                        null;
                     when X_Proto_XML.Request.Fs.Child_Reply =>
                        Does_Specified_Reply_Exist := True;

                        declare
                           Padding_Number : Aida.Int32.T := 0;
                           Reply_Name : X_Proto_XML.Large_Bounded_String.T;

                           procedure Handle_Child (Child : X_Proto_XML.Reply.Fs.Child_Type) is
                           begin
                              case Child.Kind_Id is
                                 when X_Proto_XML.Reply.Fs.Child_Field =>
                                    null;
                                 when X_Proto_XML.Reply.Fs.Child_Pad =>
                                    if Bytes (Child.P).Value > 1 then
                                       declare
                                          Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                                       begin
                                          Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Reply_Name),
                                                                            Field_Name  => "Padding" & To_String (Padding_Number),
                                                                            New_Name    => Variable_Type_Name);

                                          Put_Tabs (1); Put_Line ("type " & To_String (Variable_Type_Name) & " is array (0.." &
                                                                    To_String (Aida.Int32.T (Bytes (Child.P).Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                                       end;
                                    end if;
                                    Padding_Number := Padding_Number  + 1;
                                 when X_Proto_XML.Reply.Fs.Child_Documentation =>
                                    null;
                                 when X_Proto_XML.Reply.Fs.Child_List =>
                                    Shall_Generate_Size_Of_Function := True;
                              end case;
                           end Handle_Child;

                        begin
                           Initialize (Reply_Name, To_String (Name (Request).Value) & "Reply");

                           for I in 1..Last_Index (Children (Request_Child.R).all) loop
                              Handle_Child (Element (Children (Request_Child.R).all, I).all);
                           end loop;
                        end;

                        declare
                           New_Variable_Name             : X_Proto_XML.Large_Bounded_String.T;
                           New_Variable_Type_Name        : X_Proto_XML.Large_Bounded_String.T;

                           Padding_Number : Aida.Int32.T := 0;

                           Reply_Name : X_Proto_XML.Large_Bounded_String.T;

                           procedure Process_Reply_Child (Reply_Child : X_Proto_XML.Reply.Fs.Child_Ptr;
                                                          Is_First    : Boolean) is
                           begin
                              case Reply_Child.Kind_Id is
                                 when X_Proto_XML.Reply.Fs.Child_Field =>
                                    if Kind (Reply_Child.F).Exists then
                                       declare
                                          Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                                          Is_Success : Boolean;
                                       begin
                                          Translate_Variable_Type_Name (Variable_Type_Name => To_String (Kind (Reply_Child.F).Value),
                                                                        Is_Success         => Is_Success,
                                                                        Translated_Name    => Variable_Type_Name);

                                          if Is_Success then
                                             declare
                                                Field_Name : X_Proto_XML.Large_Bounded_String.T;
                                             begin
                                                Generate_Struct_Name (Old_Name => To_String (Name (Reply_Child.F).Value),
                                                                      New_Name => Field_Name);

                                                if Is_First then
                                                   if
                                                     Contains (Eight_Bit_Variable_Type_Names.all, Kind (Reply_Child.F).Value)
                                                   then
                                                      if Enum (Reply_Child.F).Exists then
                                                         Translate_Variable_Type_Name (Variable_Type_Name => To_String (Enum (Reply_Child.F).Value),
                                                                                       Is_Success         => Is_Success,
                                                                                       Translated_Name    => Variable_Type_Name);
                                                      end if;

                                                      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                                                      Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                                      Put_Tabs (2); Put_Line ("Length : aliased Interfaces.Unsigned_32;");
                                                   elsif To_String (Kind (Reply_Child.F).Value) = "BOOL" then
                                                      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                                      Put_Tabs (2); Put_Line ("Length : aliased Interfaces.Unsigned_32;");
                                                   else
                                                      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Request " & To_String (Name (Request).Value) &
                                                                              ", reply " & To_String (Name (Reply_Child.F).Value) & " has first field " &
                                                                              To_String (Kind (Reply_Child.F).Value) & ", which is non-8-bits.");
                                                   end if;
                                                else
                                                   Put_Tabs (2); Put_Line (To_String (Field_Name) & " : aliased " & To_String (Variable_Type_Name) & ";");
                                                end if;
                                             end;
                                          else
                                             Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & To_String (Kind (Reply_Child.F).Value));
                                          end if;
                                       end;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", kind does not exist!?");
                                    end if;
                                 when X_Proto_XML.Reply.Fs.Child_Pad =>
                                    if Bytes (Reply_Child.P).Value = 1 then
                                       if Is_First then
                                          Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                                          Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                                          Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                          Put_Tabs (2); Put_Line ("Length : aliased Interfaces.Unsigned_32;");
                                       else
                                          Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                                       end if;
                                    else
                                       declare
                                          New_Variable_Type_Name : X_Proto_XML.Large_Bounded_String.T;
                                       begin
                                          Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Reply_Name),
                                                                            Field_Name  => "Padding" & To_String (Padding_Number),
                                                                            New_Name    => New_Variable_Type_Name);

                                          Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & To_String (New_Variable_Type_Name) & ";");
                                       end;
                                    end if;
                                    Padding_Number := Padding_Number + 1;
                                 when X_Proto_XML.Reply.Fs.Child_Documentation =>
                                    null;
                                 when X_Proto_XML.Reply.Fs.Child_List =>
                                    null; -- This information does not have any impact on resulting Ada code. Why?
                              end case;
                           end Process_Reply_Child;

                        begin
                           Initialize (Reply_Name, To_String (Name (Request).Value) & "Reply");

                           Generate_Struct_Name (Old_Name => To_String (Reply_Name),
                                                 New_Name => New_Variable_Name);

                           Generate_Classic_Type_Name (Old_Name => To_String (Reply_Name),
                                                       New_Name => New_Variable_Type_Name);

                           Generate_Classic_Access_Type_Name (Old_Name => To_String (Reply_Name),
                                                              New_Name => Request_Reply_Access_Type_Name);

                           Put_Tabs (1); Put_Line ("type " & To_String (New_Variable_Type_Name) & " is record");

                           for I in X_Proto_XML.Reply.Fs.Child_Vector.Index_T range 1..Last_Index (Children (Request_Child.R).all) loop
                              Process_Reply_Child (Element (Children (Request_Child.R).all, I),
                                                   1 = I);
                           end loop;

                           Put_Tabs (1); Put_Line ("end record;");
                           Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (New_Variable_Type_Name) & ");");
                           Put_Line ("");
                           Put_Tabs (1); Put_Line ("type " & To_String (Request_Reply_Access_Type_Name) & " is access all " & To_String (New_Variable_Type_Name) & ";");
                           Put_Tabs (1); Put_Line ("for " & To_String (Request_Reply_Access_Type_Name) & "'Storage_Size use 0;");
                           Put_Tabs (1); Put_Line ("pragma Convention (C, " & To_String (Request_Reply_Access_Type_Name) & ");");
                           Put_Line ("");
                        end;
                     when X_Proto_XML.Request.Fs.Child_List =>
                        Shall_Generate_Size_Of_Function := True;
                     when X_Proto_XML.Request.Fs.Child_Expression_Field =>
                        null;
                     end case;
                  end Process;

                  Reply_Type_Name : X_Proto_XML.Large_Bounded_String.T;

                  procedure Generate_Checked_Or_Unchecked_Function (Suffix : String) is
                     Name            : X_Proto_XML.Large_Bounded_String.T;
                     C_Function_Name : X_Proto_XML.Large_Bounded_String.T;
                     Function_Name   : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Generate_Struct_Name (Old_Name => To_String (X_Proto_XML.Request.Name (Request).Value),
                                           New_Name => Name); -- There is risk here of erroneous Name

                     Initialize (Function_Name, To_String (Name) & Suffix);

                     Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (To_String (Name) & Suffix));

                     Generate_Request_With_Reply_Code (Function_Name,
                                                       C_Function_Name,
                                                       Children (Request).all,
                                                       To_String (X_Proto_XML.Request.Name (Request).Value),
                                                       Reply_Type_Name);
                  end Generate_Checked_Or_Unchecked_Function;

               begin
                  for I in X_Proto_XML.Request.Fs.Child_Vector.Index_T range 1..Last_Index (Children (Request).all) loop
                     Process (Element (Children (Request).all, I));
                  end loop;

                  if Shall_Generate_Size_Of_Function then
                     declare
                        Name            : X_Proto_XML.Large_Bounded_String.T;
                        C_Function_Name : X_Proto_XML.Large_Bounded_String.T;
                        Function_Name   : X_Proto_XML.Large_Bounded_String.T;
                     begin
                        Generate_Struct_Name (Old_Name => To_String (X_Proto_XML.Request.Name (Request).Value),
                                              New_Name => Name); -- There is risk here of erroneous Name

                        Initialize (Function_Name, To_String (Name) & "_Size_Of");

                        Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (To_String (Name)) & "_sizeof");
                        Put_Tabs (1); Put_Line ("function " & To_String (Function_Name) & " (Buffer : System.Address) return Interfaces.C.int;");
                        Put_Tabs (1); Put_Line ("pragma Import (C, " & To_String (Function_Name) & ", """ & To_String (C_Function_Name) & """);");
                        Put_Line ("");
                     end;
                  end if;

                  if Does_Specified_Reply_Exist then
                     declare
                        Name      : X_Proto_XML.Large_Bounded_String.T;
                        Type_Name : X_Proto_XML.Large_Bounded_String.T;
                     begin
                        Generate_Struct_Name (Old_Name => To_String (X_Proto_XML.Request.Name (Request).Value),
                                              New_Name => Name);

                        Initialize (Type_Name, To_String (Name) & "_Cookie_Type");

                        Put_Tabs (1); Put_Line ("type " & To_String (Type_Name) & " is record");
                        Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.C.unsigned;");
                        Put_Tabs (1); Put_Line ("end record;");
                        Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & To_String (Type_Name) & ");");
                        Put_Line ("");

                        Reply_Type_Name := Type_Name;
                     end;

                     declare
                        Name            : X_Proto_XML.Large_Bounded_String.T;
                        C_Function_Name : X_Proto_XML.Large_Bounded_String.T;
                        Function_Name   : X_Proto_XML.Large_Bounded_String.T;
                     begin
                        Generate_Struct_Name (Old_Name => To_String (X_Proto_XML.Request.Name (Request).Value),
                                              New_Name => Name); -- There is risk here of erroneous Name

                        Initialize (Function_Name, To_String (Name) & "_Reply");

                        Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (To_String (Name)) & "_reply");

                        Put_Tabs (1); Put_Line ("function " & To_String (Function_Name));
                        Put_Tabs (2); Put_Line ("(");
                        Put_Tabs (2); Put_Line (" C : Connection_Access_Type;");
                        Put_Tabs (2); Put_Line (" Cookie : " & To_String (Reply_Type_Name) & ";");
                        Put_Tabs (2); Put_Line (" Error  : System.Address");
                        Put_Tabs (2); Put_Line (") return " & To_String (Request_Reply_Access_Type_Name) & ";");
                        Put_Tabs (1); Put_Line ("pragma Import (C, " &  To_String (Function_Name) & ", """ & To_String (C_Function_Name) & """);");
                        Put_Line ("");
                     end;

                     Generate_Checked_Or_Unchecked_Function ("_Unchecked");
                  else
                     Initialize (Reply_Type_Name, "Void_Cookie_Type");

                     Generate_Checked_Or_Unchecked_Function ("_Checked");
                  end if;

                  declare
                     Name            : X_Proto_XML.Large_Bounded_String.T;
                     C_Function_Name : X_Proto_XML.Large_Bounded_String.T;
                     Function_Name   : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Generate_Struct_Name (Old_Name => To_String (X_Proto_XML.Request.Name (Request).Value),
                                           New_Name => Name); -- There is risk here of erroneous Name

                     Initialize (Function_Name, To_String (Name));

                     Initialize (C_Function_Name, "xcb_" & Aida.UTF8.To_Lowercase (To_String (Name)));

                     Generate_Request_With_Reply_Code (Function_Name,
                                                       C_Function_Name,
                                                       Children (Request).all,
                                                       To_String (X_Proto_XML.Request.Name (Request).Value),
                                                       Reply_Type_Name);
                  end;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Type definition exists without new name!?");
            end if;
         end Handle_Request;

      begin
         for I in Requests'First..Requests'Last loop
            Handle_Request (Requests (I).all);
         end loop;
      end Generate_Ada_Code_For_Requests;

      procedure Generate_Ada_Code_For_Requests is new X_Proto_XML.Xcb.Fs.Request_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Requests);

   begin
      declare
         Ada_Name : X_Proto_XML.Large_Bounded_String.T;
      begin
         Initialize (Ada_Name, "CARD8");
         Append (Eight_Bit_Variable_Type_Names.all, Ada_Name);
         Initialize (Ada_Name, "BYTE");
         Append (Eight_Bit_Variable_Type_Names.all, Ada_Name);
      end;

      Ada.Text_IO.Create (File => File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => "xcb.ads");
      Put_Line ("with Interfaces.C.Strings;");
      Put_Line ("with System;");
      Put_Line ("with Ada.Unchecked_Conversion;");
      Put_Line ("with Ada.Unchecked_Deallocation;");
      Put_Line ("");
      Put_Line ("package XCB is");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("pragma Linker_Options (""-lxcb"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("X_PROTOCOL                       : constant := 11;");
      Put_Tabs (1); Put_Line ("X_PROTOCOL_REVISION              : constant := 0;");
      Put_Tabs (1); Put_Line ("X_TCP_PORT                       : constant := 6000;");
      Put_Tabs (1); Put_Line ("XCB_CONN_ERROR                   : constant := 1;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_EXT_NOTSUPPORTED : constant := 2;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_MEM_INSUFFICIENT : constant := 3;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_REQ_LEN_EXCEED   : constant := 4;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_PARSE_ERR        : constant := 5;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_INVALID_SCREEN   : constant := 6;");
      Put_Tabs (1); Put_Line ("XCB_CONN_CLOSED_FDPASSING_FAILED : constant := 7;");
      Put_Tabs (1); Put_Line ("XCB_NONE                         : constant := 0;");
      Put_Tabs (1); Put_Line ("XCB_COPY_FROM_PARENT             : constant := 0;");
      Put_Tabs (1); Put_Line ("XCB_CURRENT_TIME                 : constant := 0;");
      Put_Tabs (1); Put_Line ("XCB_NO_SYMBOL                    : constant := 0;");
      Put_Line ("");

      Pre_Process_Requests;

      Generate_Ada_Code_For_Event_Constants (Events (Xcb).all);

      Generate_Ada_Code_For_Event_Copy_Constants (Event_Copies (Xcb).all);

      Generate_Ada_Code_For_Error_Constants (Errors (Xcb).all);

      Generate_Ada_Code_For_Error_Copy_Constants (Error_Copies (Xcb).all);

      Generate_Ada_Code_For_Request_Constants (Requests (XCB).all);

      Put_Tabs (1); Put_Line ("-- Identifier for objects in the XCB library. For example Windows,");
      Put_Tabs (1); Put_Line ("-- Graphical Contexts,...");
      Put_Tabs (1); Put_Line ("type X_Id_Type is new Interfaces.Unsigned_32;");
      Put_Line ("");

      Generate_Ada_Code_For_X_Id_Unions (X_Id_Unions (Xcb).all);

      for X_Id_Index in 1..Last_Index (X_Ids (Xcb).all) loop
         declare
            X_Id : X_Proto_XML.X_Id.Ptr renames Element (X_Ids (Xcb).all, X_Id_Index);
         begin
            if Name (X_Id.all).Exists then
               if not Contains (Processed_X_Ids.all, Name (X_Id.all).Value) then
                  Generate_Code_For_X_Id (Name (X_Id.all).Value,
                                          "X_Id_Type",
                                          How => Use_The_New_Keyword);
               end if;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end;
      end loop;

      Put_Line ("");

      Generate_Ada_Code_For_Enums (Enums (Xcb).all);

      Generate_Ada_Code_For_Type_Definitions (Type_Definitions (Xcb).all);

      Generate_Ada_Code_For_Structs (Structs (XCB).all);

      for I in 1..Last_Index (Names_Of_Types_To_Make_Array_Types.all) loop
         declare
            Text                     : X_Proto_XML.Large_Bounded_String.T renames Element (Names_Of_Types_To_Make_Array_Types.all, I);
            Variable_Type_Name       : X_Proto_XML.Large_Bounded_String.T;
            Variable_Array_Type_Name : X_Proto_XML.Large_Bounded_String.T;
            Is_Success : Boolean;
         begin
            Translate_Variable_Type_Name (Variable_Type_Name => To_String (Text),
                                          Is_Success         => Is_Success,
                                          Translated_Name    => Variable_Type_Name);

            if Is_Success then
               Generate_Classic_Array_Type_Name (Prefix_Name => To_String (Text),
                                                 Field_Name  => "",
                                                 New_Name    => Variable_Array_Type_Name);
               Put_Tabs (1); Put_Line ("type " & To_String (Variable_Array_Type_Name) & " is array (Natural range <>) of " & To_String (Variable_Type_Name) & ";");
               Put_Tabs (1); Put_Line ("pragma Convention (C, " & To_String (Variable_Array_Type_Name) & ");");
               Put_Line ("");
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Could not translate type " & To_String (Text));
            end if;
         end;
      end loop;

      Generate_Ada_Code_For_Unions (Unions (Xcb).all);

      Generate_Ada_Code_For_Events (Events (Xcb).all);

      Generate_Ada_Code_For_Event_Copies (Event_Copies (Xcb).all);

      Generate_Ada_Code_For_Errors (Errors (Xcb).all);

      Generate_Ada_Code_For_Error_Copies (Error_Copies (Xcb).all);

      -- This type is from xcb.h
      Put_Tabs (1); Put_Line ("type " & Generic_Iterator_Type_Name & " is record");
      Put_Tabs (2); Put_Line ("Data  : System.Address;");
      Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
      Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("end record;");
      Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & Generic_Iterator_Type_Name & ");");
      Put_Line ("");

      Put_Tabs (1); Put_Line ("-- Opaque structure containing all data that XCB needs in order");
      Put_Tabs (1); Put_Line ("-- to communicate with an X server.");
      Put_Tabs (1); Put_Line ("type Connection_Type is limited null record;");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("type Connection_Access_Type is access all Connection_Type;");
      Put_Tabs (1); Put_Line ("for Connection_Access_Type'Storage_Size use 0;");
      Put_Tabs (1); Put_Line ("pragma Convention (C, Connection_Access_Type);");
      Put_Line ("");

      Put_Tabs (1); Put_Line ("type Value_List_Array is array (Natural range <>) of Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("pragma Convention (C, Value_List_Array);");
      Put_Line ("");

      Put_Tabs (1); Put_Line ("type Void_Cookie_Type is record");
      Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.C.unsigned;");
      Put_Tabs (1); Put_Line ("end record;");
      Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, Void_Cookie_Type);");
      Put_Line ("");

      Put_Tabs (1); Put_Line ("type Generic_Error_Padding_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("type Generic_Error_Type is record");
      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Error_Code    : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Sequence      : aliased Interfaces.Unsigned_16;");
      Put_Tabs (2); Put_Line ("Resource_Id   : aliased Interfaces.Unsigned_32;");
      Put_Tabs (2); Put_Line ("Minor_Code    : aliased Interfaces.Unsigned_16;");
      Put_Tabs (2); Put_Line ("Major_Code    : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Padding_0     : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Padding_Array : aliased Generic_Error_Padding_Array_Type;");
      Put_Tabs (2); Put_Line ("Full_Sequence : aliased Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("end record;");
      Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, Generic_Error_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Generic_Error_Access_Type is access all Generic_Error_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Generic_Event_Pad_Array_Type is array (0 .. 6) of aliased Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("type Generic_Event_Type is record");
      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Pad_0         : aliased Interfaces.Unsigned_8;");
      Put_Tabs (2); Put_Line ("Sequence      : aliased Interfaces.Unsigned_16;");
      Put_Tabs (2); Put_Line ("Pad           : aliased Generic_Event_Pad_Array_Type;");
      Put_Tabs (2); Put_Line ("Full_Sequence : aliased Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("end record;");
      Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, Generic_Event_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Generic_Event_Access_Type is access all Generic_Event_Type;");
      Put_Tabs (1); Put_Line ("pragma Convention (C, Generic_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Setup_Constant_Access_Type is access constant Setup_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Request_Check");
      Put_Tabs (2); Put_Line ("(");
      Put_Tabs (2); Put_Line (" C      : Connection_Access_Type;");
      Put_Tabs (2); Put_Line (" Cookie : Void_Cookie_Type");
      Put_Tabs (2); Put_Line (") return Generic_Error_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Request_Check, ""xcb_request_check"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Expose_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                          Target => Expose_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Button_Press_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                                Target => Button_Press_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Button_Release_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                                  Target => Button_Release_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Motion_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                                 Target => Motion_Notify_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Enter_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                                Target => Enter_Notify_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Leave_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                                Target => Leave_Notify_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Key_Press_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                             Target => Key_Press_Event_Access_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function To_Key_Release_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,");
      Put_Tabs (1); Put_Line ("                                                               Target => Key_Release_Event_Access_Type);");
      Put_Line ("");

      Put_Tabs (1); Put_Line ("-- Connects to the X server specified by displayname and");
      Put_Tabs (1); Put_Line ("-- returns a newly allocated xcb_connection_t structure.");
      Put_Tabs (1); Put_Line ("-- If displayname is NULL, uses the value of the DISPLAY environment");
      Put_Tabs (1); Put_Line ("-- variable. If a particular screen on that server is preferred, the");
      Put_Tabs (1); Put_Line ("-- int pointed to by @p screenp (if not @c NULL) will be set to that");
      Put_Tabs (1); Put_Line ("-- screen; otherwise the screen will be set to 0.");
      Put_Tabs (1); Put_Line ("function Connect (Display_Name   : Interfaces.C.Strings.chars_ptr;");
      Put_Tabs (1); Put_Line ("                  Screen_Pointer : access Interfaces.C.int := null) return Connection_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Connect, ""xcb_connect"");");
      Put_Tabs (1); Put_Line ("   ");
      Put_Tabs (1); Put_Line ("-- Closes the connection to the X-server. Closes the file descriptor and");
      Put_Tabs (1); Put_Line ("-- frees all memory associated with the connection.");
      Put_Tabs (1); Put_Line ("procedure Disconnect (C : Connection_Access_Type);");
      Put_Tabs (1); Put_Line ("pragma Import (C, Disconnect, ""xcb_disconnect"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("-- Returns the next event or error from the server.");
      Put_Tabs (1); Put_Line ("-- c: The connection to the X server.");
      Put_Tabs (1); Put_Line ("-- Returns the next event from the server.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- Returns the next event or error from the server, or returns null in");
      Put_Tabs (1); Put_Line ("-- the event of an I/O error. Blocks until either an event or error");
      Put_Tabs (1); Put_Line ("-- arrive, or an I/O error occurs.");
      Put_Tabs (1); Put_Line ("function Wait_For_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Wait_For_Event, ""xcb_wait_for_event"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("function Poll_For_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Poll_For_Event, ""xcb_poll_for_event"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("procedure Free is new Ada.Unchecked_Deallocation (Object => Generic_Event_Type,");
      Put_Tabs (1); Put_Line ("                                                  Name   => Generic_Event_Access_Type);");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("-- Access the data returned by the server.");
      Put_Tabs (1); Put_Line ("-- c: The connection.");
      Put_Tabs (1); Put_Line ("-- Returns: A pointer to an xcb_setup_t structure.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- Accessor for the data returned by the server when the xcb_connection_t");
      Put_Tabs (1); Put_Line ("-- was initialized. This data includes");
      Put_Tabs (1); Put_Line ("-- - the server's required format for images,");
      Put_Tabs (1); Put_Line ("-- - a list of available visuals,");
      Put_Tabs (1); Put_Line ("-- - a list of available screens,");
      Put_Tabs (1); Put_Line ("-- - the server's maximum request length (in the absence of the BIG-REQUESTS extension),");
      Put_Tabs (1); Put_Line ("-- - and other assorted information.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- See the X protocol specification for more details.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- The result must not be freed.");
      Put_Tabs (1); Put_Line ("function Get_Setup (C : Connection_Access_Type) return Setup_Constant_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Get_Setup, ""xcb_get_setup"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("-- Allocates an XID for a new object.");
      Put_Tabs (1); Put_Line ("-- c: The connection.");
      Put_Tabs (1); Put_Line ("-- Returns: A newly allocated XID.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- Allocates an XID for a new object. Typically used just prior to");
      Put_Tabs (1); Put_Line ("-- various object creation functions, such as xcb_create_window.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("function Generate_Id (C : Connection_Access_Type) return X_Id_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Generate_Id, ""xcb_generate_id"");");
      Put_Tabs (1); Put_Line ("   ");
      Put_Tabs (1); Put_Line ("-- Test whether the connection has shut down due to a fatal error.");
      Put_Tabs (1); Put_Line ("-- c: The connection.");
      Put_Tabs (1); Put_Line ("-- Returns: > 0 if the connection is in an error state; 0 otherwise.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- Some errors that occur in the context of an xcb_connection_t");
      Put_Tabs (1); Put_Line ("-- are unrecoverable. When such an error occurs, the");
      Put_Tabs (1); Put_Line ("-- connection is shut down and further operations on the");
      Put_Tabs (1); Put_Line ("-- xcb_connection_t have no effect.");
      Put_Tabs (1); Put_Line ("--");
      Put_Tabs (1); Put_Line ("-- Different error codes:");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_ERROR, because of socket errors, pipe errors or other stream errors.");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_CLOSED_EXT_NOTSUPPORTED, when extension not supported.");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_CLOSED_MEM_INSUFFICIENT, when memory not available.");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_CLOSED_REQ_LEN_EXCEED, exceeding request length that server accepts.");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_CLOSED_PARSE_ERR, error during parsing display string.");
      Put_Tabs (1); Put_Line ("-- XCB_CONN_CLOSED_INVALID_SCREEN, because the server does not have a screen matching the display.");
      Put_Tabs (1); Put_Line ("function Connection_Has_Error (C : Connection_Access_Type) return Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Connection_Has_Error, ""xcb_connection_has_error"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("-- Forces any buffered output to be written to the server. Blocks until the write is complete.");
      Put_Tabs (1); Put_Line ("-- Returns > 0 on success, otherwise <= 0");
      Put_Tabs (1); Put_Line ("function Flush (C : Connection_Access_Type) return Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Flush, ""xcb_flush"");");
      Put_Tabs (1); Put_Line ("");
      Put_Tabs (1); Put_Line ("function Setup_Roots_Iterator (R : Setup_Constant_Access_Type) return Screen_Iterator_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Setup_Roots_Iterator, ""xcb_setup_roots_iterator"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Generate_Id (C : Connection_Access_Type) return Fontable_Id_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Generate_Id (C : Connection_Access_Type) return Drawable_Id_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Generate_Id (C : Connection_Access_Type) return Colormap_Id_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Auth_Info_Type is record");
      Put_Tabs (1); Put_Line ("   Name_Length : aliased Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("   Name        : Interfaces.C.Strings.chars_ptr;");
      Put_Tabs (1); Put_Line ("   Data_Length : aliased Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("   Data        : Interfaces.C.Strings.chars_ptr;");
      Put_Tabs (1); Put_Line ("end record;");
      Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, Auth_Info_Type);");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Auth_Info_Access_Type is access all Auth_Info_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Get_Maximum_Request_Length (C : Connection_Access_Type) return Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Get_Maximum_Request_Length, ""xcb_get_maximum_request_length"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("procedure Prefetch_Maximum_Request_Length (C : Connection_Access_Type);");
      Put_Tabs (1); Put_Line ("pragma Import (C, Prefetch_Maximum_Request_Length, ""xcb_prefetch_maximum_request_length"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Poll_For_Queued_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Poll_For_Queued_Event, ""xcb_poll_for_queued_event"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Special_Event_Type is null record;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Special_Event_Access_Type is access all Special_Event_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Poll_For_Special_Event (C : Connection_Access_Type; Special_Event : Special_Event_Access_Type) return Generic_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Poll_For_Special_Event, ""xcb_poll_for_special_event"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Wait_For_Special_Event (C : Connection_Access_Type; Special_Event : Special_Event_Access_Type) return Generic_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Wait_For_Special_Event, ""xcb_wait_for_special_event"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Extension_Type is null record;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("type Extension_Access_Type is access all Extension_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Register_For_Special_XGE (C            : Connection_Access_Type;");
      Put_Tabs (1); Put_Line ("                                   Extension    : Extension_Access_Type;");
      Put_Tabs (1); Put_Line ("                                   Extension_Id : Interfaces.Unsigned_32;");
      Put_Tabs (1); Put_Line ("                                   Stamp        : access Interfaces.Unsigned_32) return Special_Event_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Register_For_Special_XGE, ""xcb_register_for_special_xge"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("procedure Unregister_For_Special_Event (C : Connection_Access_Type; Special_Event : Special_Event_Access_Type);");
      Put_Tabs (1); Put_Line ("pragma Import (C, Unregister_For_Special_Event, ""xcb_unregister_for_special_event"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("procedure Discard_Reply (C : Connection_Access_Type; Sequence : Interfaces.C.unsigned);");
      Put_Tabs (1); Put_Line ("pragma Import (C, Discard_Reply, ""xcb_discard_reply"");");
      Put_Line ("");

      for I in 1..Last_Index (Structs (XCB).all) loop
         if Name (Element (Structs (XCB).all, I).all).Exists then
            Generate_Code_For_Next_Procedure (To_String (Name (Element (Structs (XCB).all, I).all).Value));
            Generate_Code_For_End_Function (To_String (Name (Element (Structs (XCB).all, I).all).Value));
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Struct exists without a name!?");
         end if;
      end loop;

      for X_Id_Index in 1..Last_Index (X_Ids (Xcb).all) loop
         declare
            X_Id : X_Proto_XML.X_Id.Ptr renames Element (X_Ids (Xcb).all, X_Id_Index);
         begin
            if Name (X_Id.all).Exists then
               Generate_Code_For_Next_Procedure (To_String (Name (X_Id.all).Value));
               Generate_Code_For_End_Function (To_String (Name (X_Id.all).Value));
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "X_Id exists without a name!?");
            end if;
         end;
      end loop;

      Generate_Ada_Subprograms_From_Type_Definitions (Type_Definitions (Xcb).all);

      Generate_Ada_Code_For_Requests (Requests (XCB).all);

      Put_Tabs (1); Put_Line ("type Query_Extension_Reply_Constant_Access_Type is access constant Query_Extension_Reply_Type;");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Get_Extension_Data (C : Connection_Access_Type; Extension : Extension_Access_Type) return Query_Extension_Reply_Constant_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Get_Extension_Data, ""xcb_get_extension_data"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("procedure Prefetch_Extension_Data (C : Connection_Access_Type; Extension : Extension_Access_Type);");
      Put_Tabs (1); Put_Line ("pragma Import (C, Prefetch_Extension_Data, ""xcb_prefetch_extension_data"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Get_File_Descriptor (C : Connection_Access_Type) return Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Get_File_Descriptor, ""xcb_get_file_descriptor"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Connect_To_Fd (Fd : Interfaces.C.int; Auth_Info : Auth_Info_Access_Type) return Connection_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Connect_To_Fd, ""xcb_connect_to_fd"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Parse_Display (Name    : Interfaces.C.Strings.chars_ptr;");
      Put_Tabs (1); Put_Line ("                        Host    : System.Address;");
      Put_Tabs (1); Put_Line ("                        Display : access Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("                        Screen  : access Interfaces.C.int) return Interfaces.C.int;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Parse_Display, ""xcb_parse_display"");");
      Put_Line ("");
      Put_Tabs (1); Put_Line ("function Connect_To_Display_With_Auth_Info (display : Interfaces.C.Strings.chars_ptr;");
      Put_Tabs (1); Put_Line ("                                            Auth    : access Auth_Info_Type;");
      Put_Tabs (1); Put_Line ("                                            Screen  : access Interfaces.C.int) return Connection_Access_Type;");
      Put_Tabs (1); Put_Line ("pragma Import (C, Connect_To_Display_With_Auth_Info, ""xcb_connect_to_display_with_auth_info"");");
      Put_Line ("");

      Put_Line ("end XCB;");

      Ada.Text_IO.Close (File);

   end Create_XCB_Package;

end XCB_Package_Creator;
