with Aida;
with Ada.Text_IO;
with Strings_Edit.UTF8.Mapping;
with Strings_Edit.UTF8.Categorization;
with GNAT.Source_Info;
with Ada.Containers.Vectors;
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

   use type Ada.Containers.Count_Type;
   use type X_Proto.Value_Type;
   use type X_Proto.Request.Fs.Child_Kind_Id_Type;
   use type Aida.Int32.T;

   use X_Proto.Large_Bounded_String;

   use Aida.Int32;

   use X_Proto.Xcb.Fs.Struct_Vector;
   use X_Proto.Xcb.Fs.X_Id_Vector;
   use X_Proto.Xcb.Fs.X_Id_Union_Vector;

   use X_Proto.Struct.Fs.Member_Kind_Id;

   package Unbounded_String_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                   Element_Type => X_Proto.Large_Bounded_String.T,
                                                                   "="          => X_Proto.Large_Bounded_String."=");

   Processed_X_Ids : Unbounded_String_Vectors.Vector;

   Eight_Bit_Variable_Type_Names : Unbounded_String_Vectors.Vector;

   Thirty_Two_Bit_Variable_Type_Names : Unbounded_String_Vectors.Vector;

   package Original_Name_To_Adaified_Name_P is new Aida.Containers.Bounded_Hash_Map (Key_T             => X_Proto.Large_Bounded_String.T,
                                                                                     Element_T         => X_Proto.Large_Bounded_String.T,
                                                                                     Hash              => X_Proto.Large_Bounded_String.Hash32,
                                                                                     Equivalent_Keys   => X_Proto.Large_Bounded_String."=",
                                                                                     Max_Hash_Map_Size => 501,
                                                                                     Max_Collisions    => 5);

   use Original_Name_To_Adaified_Name_P;

   subtype Original_Name_To_Adaified_Name_T is Original_Name_To_Adaified_Name_P.T;

   subtype Original_Name_To_Adaified_Name_Ptr is Original_Name_To_Adaified_Name_P.Ptr;

   package Enum_Name_To_Size_Identifier_Map_P is new Aida.Containers.Bounded_Hash_Map (Key_T             => X_Proto.Large_Bounded_String.T,
                                                                                       Element_T         => X_Proto.Large_Bounded_String.T,
                                                                                       Hash              => X_Proto.Large_Bounded_String.Hash32,
                                                                                       Equivalent_Keys   => X_Proto.Large_Bounded_String."=",
                                                                                       Max_Hash_Map_Size => 501,
                                                                                       Max_Collisions    => 5);

   subtype Enum_Name_To_Size_Identifier_Map_Ptr is Enum_Name_To_Size_Identifier_Map_P.Ptr;

   subtype Enum_Name_To_Size_Identifier_Map_T is Enum_Name_To_Size_Identifier_Map_P.T;

   use Enum_Name_To_Size_Identifier_Map_P;

   function Value_Of_Bit (B : X_Proto.Item.Fs.Bit_Type) return Long_Integer is
   begin
      return 2 ** Integer (B);
   end Value_Of_Bit;

   procedure Generate_Struct_Name (Old_Name : String;
                                   New_Name : in out X_Proto.Large_Bounded_String.T)
   is
      P : Integer := Old_Name'First;

      CP : Strings_Edit.UTF8.Code_Point := 0;

      Is_Previous_Lowercase : Boolean := False;
      Is_Previous_A_Number  : Boolean := False;
      Is_Previous_An_Undercase  : Boolean := False;
   begin
      if Old_Name = "CHAR2B" then
         New_Name.Initialize ("Char_2B");
         return;
      end if;

      if Old_Name = "VISUALTYPE" then
         New_Name.Initialize ("Visual_Kind");
         return;
      end if;

      if Old_Name = "TIMECOORD" then
         New_Name.Initialize ("Time_Coordinate");
         return;
      end if;

      if Old_Name = "FONTPROP" then
         New_Name.Initialize ("Font_Properties");
         return;
      end if;

      if Old_Name = "CHARINFO" then
         New_Name.Initialize ("Character_Information");
         return;
      end if;

      if Old_Name = "COLORITEM" then
         New_Name.Initialize ("Color_Item");
         return;
      end if;

      if Old_Name = "RGB" then
         New_Name.Initialize ("Red_Green_Blue");
         return;
      end if;

      if Old_Name = "VISUALID" then
         New_Name.Initialize ("Visual_Id");
         return;
      end if;

      if Old_Name = "BUTTON" then
         New_Name.Initialize ("Button_Id");
         return;
      end if;

      if Old_Name = "GetPropertyType" then
         New_Name.Initialize ("Get_Property_Kind");
         return;
      end if;

      if Old_Name = "new" then
         New_Name.Initialize ("U_New"); -- The same as in the C header file.
         return;
      end if;

      if Old_Name = "delta" then
         New_Name.Initialize ("U_Delta");
         return;
      end if;

      if Old_Name = "type" then
         New_Name.Initialize ("Kind");
         return;
      end if;

      if Old_Name = "string" then
         New_Name.Initialize ("Text");
         return;
      end if;

      New_Name.Initialize ("");
      Strings_Edit.UTF8.Get (Source  => Old_Name,
                             Pointer => P,
                             Value   => CP);

      if Strings_Edit.UTF8.Mapping.Is_Uppercase (CP) then
         New_Name.Append (Strings_Edit.UTF8.Image (CP));
      else
         New_Name.Append (Strings_Edit.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Uppercase (CP)));
      end if;

      while P <= Old_Name'Last loop
         Strings_Edit.UTF8.Get (Source  => Old_Name,
                                Pointer => P,
                                Value   => CP);

         if Strings_Edit.UTF8.Image (CP) = "_" then
            New_Name.Append ("_");
            Is_Previous_An_Undercase := True;
         else
            if Strings_Edit.UTF8.Categorization.Is_Digit (CP) then
               if Is_Previous_A_Number then
                  New_Name.Append (Strings_Edit.UTF8.Image (CP));
               else
                  New_Name.Append ("_" & Strings_Edit.UTF8.Image (CP));
               end if;

               Is_Previous_A_Number := True;
            else
               if Strings_Edit.UTF8.Mapping.Is_Uppercase (CP) then
                  if Is_Previous_Lowercase then
                     New_Name.Append ("_" & Strings_Edit.UTF8.Image (CP));
                     Is_Previous_Lowercase := False;
                  else
                     New_Name.Append (Strings_Edit.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Lowercase (CP)));
                  end if;
               else
                  if Is_Previous_An_Undercase then
                     New_Name.Append (Strings_Edit.UTF8.Image (Strings_Edit.UTF8.Mapping.To_Uppercase (CP)));
                  else
                     New_Name.Append (Strings_Edit.UTF8.Image (CP));
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
                                         New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Type");
   end Generate_Classic_Type_Name;

   procedure Generate_Classic_Access_Type_Name (Old_Name : String;
                                                New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Access_Type");
   end Generate_Classic_Access_Type_Name;

   procedure Generate_Classic_Iterator_Type_Name (Old_Name : String;
                                                  New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Iterator_Type");
   end Generate_Classic_Iterator_Type_Name;

   procedure Generate_Classic_Iterator_Access_Type_Name (Old_Name : String;
                                                         New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Iterator_Access_Type");
   end Generate_Classic_Iterator_Access_Type_Name;

   procedure Generate_Classic_Variable_Id_Name (Old_Name : String;
                                            New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Id");
   end Generate_Classic_Variable_Id_Name;

   procedure Generate_Classic_Type_Id_Name (Old_Name : String;
                                            New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Id_Type");
   end Generate_Classic_Type_Id_Name;

   procedure Generate_Classic_Access_Type_Id_Name (Old_Name : String;
                                                   New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Id_Access_Type");
   end Generate_Classic_Access_Type_Id_Name;

   procedure Generate_Classic_Iterator_Type_Id_Name (Old_Name : String;
                                                     New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Id_Iterator_Type");
   end Generate_Classic_Iterator_Type_Id_Name;

   procedure Generate_Classic_Iterator_Access_Type_Id_Name (Old_Name : String;
                                                            New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Id_Iterator_Access_Type");
   end Generate_Classic_Iterator_Access_Type_Id_Name;

   procedure Generate_Classic_Event_Type_Name (Old_Name : String;
                                               New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Event_Type");
   end Generate_Classic_Event_Type_Name;

   procedure Generate_Classic_Event_Access_Type_Name (Old_Name : String;
                                                      New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Event_Access_Type");
   end Generate_Classic_Event_Access_Type_Name;

   procedure Generate_Classic_Error_Type_Name (Old_Name : String;
                                               New_Name : in out X_Proto.Large_Bounded_String.T) is
   begin
      Generate_Struct_Name (Old_Name,
                            New_Name);
      New_Name.Append ("_Error_Type");
   end Generate_Classic_Error_Type_Name;

   procedure Generate_Classic_Event_List_Type_Name (Enum_Name : String;
                                                    List_Name : String;
                                                    New_Name  : in out X_Proto.Large_Bounded_String.T)
   is
      Adafied_List_Name : X_Proto.Large_Bounded_String.T;
   begin
      Generate_Struct_Name (List_Name,
                            Adafied_List_Name);

      Generate_Struct_Name (Enum_Name,
                            New_Name);
      New_Name.Append ("_Event_" & Adafied_List_Name.To_String & "_Array_Type");
   end Generate_Classic_Event_List_Type_Name;

   procedure Translate_Classic_Variable_Type_Name (Variable_Type_Name : String;
                                                   Is_Success         : out Boolean;
                                                   Translated_Name    : out X_Proto.Large_Bounded_String.T) is
   begin
      Is_Success := True;
      if Variable_Type_Name = "CARD8" then
         Translated_Name.Initialize ("Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "CARD16" then
         Translated_Name.Initialize ("Interfaces.Unsigned_16");
      elsif Variable_Type_Name = "CARD32" then
         Translated_Name.Initialize ("Interfaces.Unsigned_32");
      elsif Variable_Type_Name = "BYTE" then
         Translated_Name.Initialize ("Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "BOOL" then
         Translated_Name.Initialize ("Interfaces.Unsigned_8");
      elsif Variable_Type_Name = "INT8" then
         Translated_Name.Initialize ("Interfaces.Integer_8");
      elsif Variable_Type_Name = "INT16" then
         Translated_Name.Initialize ("Interfaces.Integer_16");
      elsif Variable_Type_Name = "INT32" then
         Translated_Name.Initialize ("Interfaces.Integer_32");
      else
         Is_Success := False;
         Translated_Name.Initialize ("");
         return;
      end if;
   end Translate_Classic_Variable_Type_Name;

   procedure Generate_Classic_Array_Type_Name (Prefix_Name : String;
                                               Field_Name  : String;
                                               New_Name    : in out X_Proto.Large_Bounded_String.T)
   is
      Adafied_List_Name : X_Proto.Large_Bounded_String.T;
   begin
      if Prefix_Name = "CARD32" then
         New_Name.Initialize ("Unsigned_32");
      else
         Generate_Struct_Name (Prefix_Name,
                               New_Name);
      end if;

      if Field_Name /= "" then
         Generate_Struct_Name (Field_Name,
                               Adafied_List_Name);
         New_Name.Append ("_" & Adafied_List_Name.To_String & "_Array_Type");
      else
         New_Name.Append ("_Array_Type");
      end if;
   end Generate_Classic_Array_Type_Name;

   procedure Create_XCB_Package (XCB : X_Proto.Xcb.T) is
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
                                              Translated_Name    : out X_Proto.Large_Bounded_String.T) is
      begin
         Translate_Classic_Variable_Type_Name (Variable_Type_Name,
                                               Is_Success,
                                               Translated_Name);

         if not Is_Success then
            declare
               Searched_For : X_Proto.Large_Bounded_String.T;
            begin
               Searched_For.Initialize (Variable_Type_Name);
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
                                            Translated_Name    : out X_Proto.Large_Bounded_String.T)
      is
         Searched_For : X_Proto.Large_Bounded_String.T;
      begin
         Searched_For.Initialize (Variable_Type_Name);
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
                                                 Translated_Name    : out X_Proto.Large_Bounded_String.T)
      is
         Searched_For : X_Proto.Large_Bounded_String.T;
      begin
         Searched_For.Initialize (Variable_Type_Name);
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
                                                        Translated_Name    : out X_Proto.Large_Bounded_String.T)
      is
         Searched_For : X_Proto.Large_Bounded_String.T;
      begin
         Searched_For.Initialize (Variable_Type_Name);
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

      function Determine_Largest_Value (Items : X_Proto.Enum.Fs.Item_Vectors.Vector) return Long_Integer is
         R : Long_Integer := 0;
      begin
         for I in Positive range Items.First_Index..Items.Last_Index loop
            case Items.Element (I).Kind_Id is
               when X_Proto.Item.Fs.Not_Specified =>
                  Ada.Text_IO.Put_Line ("Can never happen");
               when X_Proto.Item.Fs.Specified_As_Value =>
                  if Long_Integer (Items.Element (I).Value) > R then
                     R := Long_Integer (Items.Element (I).Value);
                  end if;
               when X_Proto.Item.Fs.Specified_As_Bit =>
                  if Value_Of_Bit (Items.Element (I).Bit) > R then
                     R := Value_Of_Bit (Items.Element (I).Bit);
                  end if;
            end case;
         end loop;
         return R;
      end Determine_Largest_Value;

      Generic_Iterator_Type_Name : constant String := "Generic_Iterator_Type";

      procedure Generate_Code_For_Next_Procedure (Name : String) is
         N                         : X_Proto.Large_Bounded_String.T;
         Iterator_Access_Type_Name : X_Proto.Large_Bounded_String.T;
         Procedure_Name            : X_Proto.Large_Bounded_String.T;
         C_Function_Name           : X_Proto.Large_Bounded_String.T;
         Is_Success : Boolean;
      begin
         C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name) & "_next");

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
            Procedure_Name.Initialize (N.To_String & "_Next");

            Put_Tabs (1); Put_Line ("procedure " & Procedure_Name.To_String & " (I : " & Iterator_Access_Type_Name.To_String & ");");
            Put_Tabs (1); Put_Line ("pragma Import (C, " & Procedure_Name.To_String & ", """ & C_Function_Name.To_String & """);");
            Put_Line ("");
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding iterator type name.");
         end if;
      end Generate_Code_For_Next_Procedure;

      procedure Generate_Code_For_End_Function (Name : String) is
         N                  : X_Proto.Large_Bounded_String.T;
         Iterator_Type_Name : X_Proto.Large_Bounded_String.T;
         Function_Name      : X_Proto.Large_Bounded_String.T;
         C_Function_Name    : X_Proto.Large_Bounded_String.T;
         Is_Success : Boolean;
      begin
         C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name) & "_end");

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
            Function_Name.Initialize (N.To_String & "_End");

            Put_Tabs (1); Put_Line ("function " & Function_Name.To_String & " (I : " & Iterator_Type_Name.To_String & ") return " & Generic_Iterator_Type_Name & ";");
            Put_Tabs (1); Put_Line ("pragma Import (C, " & Function_Name.To_String & ", """ & C_Function_Name.To_String & """);");
            Put_Line ("");
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Failed to convert " & Name & " to corresponding iterator access type name.");
         end if;
      end Generate_Code_For_End_Function;

      type How_New_Type_Should_Be_Generated_Type is (
                                                     Use_The_New_Keyword,
                                                     Use_The_Subtype_Keyword
                                                    );

      procedure Generate_Code_For_X_Id (Name      : X_Proto.Large_Bounded_String.T;
                                        Type_Name : String;
                                        How       : How_New_Type_Should_Be_Generated_Type)
      is
         New_Variable_Name                      : X_Proto.Large_Bounded_String.T;
         New_Variable_Type_Name                 : X_Proto.Large_Bounded_String.T;
         New_Variable_Access_Type_Name          : X_Proto.Large_Bounded_String.T;
         New_Variable_Iterator_Type_Name        : X_Proto.Large_Bounded_String.T;
         New_Variable_Iterator_Access_Type_Name : X_Proto.Large_Bounded_String.T;
      begin
         Generate_Classic_Variable_Id_Name (Old_Name => Name.To_String,
                                            New_Name => New_Variable_Name);

         Generate_Classic_Type_Id_Name (Old_Name => Name.To_String,
                                        New_Name => New_Variable_Type_Name);
         case How is
            when Use_The_New_Keyword =>
               Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is new " & Type_Name & ";");
            when Use_The_Subtype_Keyword =>
               Put_Tabs (1); Put_Line ("subtype " & New_Variable_Type_Name.To_String & " is " & Type_Name & ";");
         end case;
         Put_Line ("");

         Generate_Classic_Access_Type_Id_Name (Old_Name => Name.To_String,
                                               New_Name => New_Variable_Access_Type_Name);
         Put_Tabs (1); Put_Line ("type " & New_Variable_Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
         Put_Line ("");

         Generate_Classic_Iterator_Type_Id_Name (Old_Name => Name.To_String,
                                                 New_Name => New_Variable_Iterator_Type_Name);
         Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Type_Name.To_String & " is record");
         Put_Tabs (2); Put_Line ("Data  : " & New_Variable_Access_Type_Name.To_String & ";");
         Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
         Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
         Put_Tabs (1); Put_Line ("end record;");
         Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Iterator_Type_Name.To_String & ");");
         Put_Line ("");

         Generate_Classic_Iterator_Access_Type_Id_Name (Old_Name => Name.To_String,
                                                        New_Name => New_Variable_Iterator_Access_Type_Name);
         Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Access_Type_Name.To_String & " is access all " &
                                   New_Variable_Iterator_Type_Name.To_String & ";");
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
                                                                Children           : X_Proto.Request.Fs.Child_Vectors.Vector) return Boolean is
      begin
         for J in Positive range Children.First_Index..Children.Last_Index loop
            if Children.Element (J).Kind_Id = X_Proto.Request.Fs.Child_Value_Param then
               if
                 Children.Element (J).V.Mask_Kind.Value.To_String = Variable_Type_Name and
                 Children.Element (J).V.Mask_Name.Value.To_String = Variable_Name
               then
                  return True;
               end if;
            end if;
         end loop;

         return False;
      end There_Is_No_Value_Param_With_Same_Name_And_Type;

      procedure Generate_Request_With_Reply_Code (Function_Name   : X_Proto.Large_Bounded_String.T;
                                                  C_Function_Name : X_Proto.Large_Bounded_String.T;
                                                  Children        : X_Proto.Request.Fs.Child_Vectors.Vector;
                                                  Request_Name    : String;
                                                  Reply_Type_Name : X_Proto.Large_Bounded_String.T)
      is
         Is_First_Parameter : Boolean := True;
      begin
         Put_Tabs (1); Put_Line ("function " & Function_Name.To_String);
         Put_Tabs (1); Put_Line ("  (");
         Put_Tabs (2); Put ("C : Connection_Access_Type");

         for I in Positive range Children.First_Index..Children.Last_Index loop
            case Children.Element (I).Kind_Id is
               when X_Proto.Request.Fs.Child_Field =>
                  if Children.Element (I).F.Kind.Exists then
                     if not There_Is_No_Value_Param_With_Same_Name_And_Type (Variable_Name      => Children.Element (I).F.Name.Value.To_String,
                                                                             Variable_Type_Name => Children.Element (I).F.Kind.Value.To_String,
                                                                             Children           => Children)
                     then
                        declare
                           Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           Is_Success : Boolean;
                        begin
                           Translate_Variable_Type_Name (Variable_Type_Name => Children.Element (I).F.Kind.Value.To_String,
                                                         Is_Success         => Is_Success,
                                                         Translated_Name    => Variable_Type_Name);

                           if Is_Success then
                              declare
                                 Field_Name : X_Proto.Large_Bounded_String.T;
                              begin
                                 Generate_Struct_Name (Old_Name => Children.Element (I).F.Name.Value.To_String,
                                                       New_Name => Field_Name);
                                 Put_Line (";");
                                 if Is_First_Parameter then
                                    Is_First_Parameter := False;
                                 end if;

                                 if Children.Element (I).F.Enum.Exists then
                                    declare
                                       FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
                                         Find_Element (This => Original_Name_To_Adaified_Name.all,
                                                       Key  => Children.Element (I).F.Enum.Value);
                                    begin
                                       if FER.Exists then
                                          Put_Tabs (2); Put (Field_Name.To_String & " : " & To_String (FER.Element));
                                       else
                                          Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", could not find enum type name " & Children.Element (I).F.Enum.Value.To_String);
                                       end if;
                                    end;
                                 else
                                    Put_Tabs (2); Put (Field_Name.To_String & " : " & Variable_Type_Name.To_String);
                                 end if;
                              end;
                           else
                              Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & Children.Element (I).F.Kind.Value.To_String);
                           end if;
                        end;
                     end if;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request has field withot type!?");
                  end if;
               when X_Proto.Request.Fs.Child_Pad =>
                  --                          if Event.Members.Element (I).P.Bytes.Value > 1 then
                  --                             declare
                  --                                Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                  --                             begin
                  --                                Generate_Classic_Event_List_Type_Name (Enum_Name => Event.Name.Value.To_String,
                  --                                                                       List_Name => "Padding" & Aida.Strings.To_String (Padding_Number),
                  --                                                                       New_Name  => New_Variable_Type_Name);
                  --
                  --                                Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is array (0.." &
                  --                                                          Aida.Strings.To_String (Integer (Event.Members.Element (I).P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                  --                             end;
                  --                          end if;
                  --                          Padding_Number := Padding_Number  + 1;
                  null;
               when X_Proto.Request.Fs.Child_Value_Param =>
                  if
                    Children.Element (I).V.Mask_Kind.Exists and
                    Children.Element (I).V.Mask_Name.Exists and
                    Children.Element (I).V.List_Name.Exists
                  then
                     declare
                        Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                        Is_Success : Boolean;
                     begin
                        Translate_Classic_Variable_Type_Name (Variable_Type_Name => Children.Element (I).V.Mask_Kind.Value.To_String,
                                                              Is_Success         => Is_Success,
                                                              Translated_Name    => Variable_Type_Name);

                        if Is_Success then
                           declare
                              Field_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => Children.Element (I).V.Mask_Name.Value.To_String,
                                                    New_Name => Field_Name);
                              Put_Line (";");
                              if Is_First_Parameter then
                                 Is_First_Parameter := False;
                              end if;
                              Put_Tabs (2); Put_Line (Field_Name.To_String & " : " & Variable_Type_Name.To_String & ";");
                           end;

                           declare
                              Field_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => Children.Element (I).V.List_Name.Value.To_String,
                                                    New_Name => Field_Name);

                              Put_Tabs (2); Put (Field_Name.To_String & " : Value_List_Array");
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request " & Request_Name &
                                                   " has  unexpected or unknown field type name " & Children.Element (I).V.Mask_Kind.Value.To_String);
                        end if;
                     end;
                  else
                     Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Request has value param that misses either mask type, mask name or list name!?");
                  end if;
               when X_Proto.Request.Fs.Child_Documentation =>
                  null;
               when X_Proto.Request.Fs.Child_Reply =>
                  null;
               when X_Proto.Request.Fs.Child_List =>
                  if
                    Children.Element (I).L.Name.Exists and
                    Children.Element (I).L.Kind.Exists
                  then
                     declare
                        Field_Name               : X_Proto.Large_Bounded_String.T;
                        Variable_Array_Type_Name : X_Proto.Large_Bounded_String.T;
                     begin
                        Generate_Classic_Array_Type_Name (Prefix_Name => Children.Element (I).L.Kind.Value.To_String,
                                                          Field_Name  => "",
                                                          New_Name    => Variable_Array_Type_Name);
                        Generate_Struct_Name (Old_Name => Children.Element (I).L.Name.Value.To_String,
                                              New_Name => Field_Name);
                        Put_Line (";");
                        if Children.Element (I).L.Members.Is_Empty then
                           -- Assuming the refence indicates length is already specified
                           Put_Tabs (2); Put_Line (Field_Name.To_String & "_Length : Interfaces.Unsigned_32;");
                        end if;
                        if Children.Element (I).L.Kind.Value.To_String = "char" then
                           Put_Tabs (2); Put (Field_Name.To_String & " : Interfaces.C.Strings.chars_ptr");
                        elsif
                          Children.Element (I).L.Kind.Value.To_String = "void" or
                          Children.Element (I).L.Kind.Value.To_String = "STR"
                        then
                           Put_Tabs (2); Put (Field_Name.To_String & " : System.Address");
                        elsif Children.Element (I).L.Kind.Value.To_String = "CARD8" then
                           Put_Tabs (2); Put (Field_Name.To_String & " : access Interfaces.Unsigned_8");
                        elsif Children.Element (I).L.Kind.Value.To_String = "KEYSYM" then
                           Put_Tabs (2); Put (Field_Name.To_String & " : access Keysym_Type");
                        elsif Children.Element (I).L.Kind.Value.To_String = "ATOM" then
                           Put_Tabs (2); Put (Field_Name.To_String & " : access Atom_Id_Type");
                        elsif Children.Element (I).L.Kind.Value.To_String = "KEYCODE" then
                           Put_Tabs (2); Put (Field_Name.To_String & " : access Keycode_Type");
                        else
                           Put_Tabs (2); Put (Field_Name.To_String & " : " & Variable_Array_Type_Name.To_String);
                        end if;
                     end;
                  end if;
               when X_Proto.Request.Fs.Child_Expression_Field =>
                  null;
            end case;
         end loop;

         Put_Line ("");
         Put_Tabs (1); Put_Line ("  ) return " & Reply_Type_Name.To_String & ";");
         Put_Tabs (1); Put_Line ("pragma Import (C, " & Function_Name.To_String & ", """ & C_Function_Name.To_String & """);");
         Put_Line ("");
      end Generate_Request_With_Reply_Code;

      package Unbounded_String_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                      Element_Type => X_Proto.Large_Bounded_String.T,
                                                                      "="          => X_Proto.Large_Bounded_String."=");

      Names_Of_Types_To_Make_Array_Types : Unbounded_String_Vectors.Vector;

      Enum_Name_To_Size_Identifier_Map : constant Enum_Name_To_Size_Identifier_Map_Ptr := new Enum_Name_To_Size_Identifier_Map_T;

      procedure Pre_Process_Requests is
      begin
         for Request of XCB.Requests.all loop
            if Request.Name.Exists then
               declare
                  procedure Handle_Request_Field (F : X_Proto.Field.T) is
                     Is_Success : Boolean;
                     Translated_Name : X_Proto.Large_Bounded_String.T;
                  begin
                     if F.Enum.Exists then
                        Translate_Classic_Variable_Type_Name (Variable_Type_Name => F.Kind.Value.To_String,
                                                              Is_Success         => Is_Success,
                                                              Translated_Name    => Translated_Name);

                        if Is_Success then
                           declare
                              FER : constant Enum_Name_To_Size_Identifier_Map_P.Find_Element_Result_T
                               := Find_Element (Enum_Name_To_Size_Identifier_Map.all, F.Enum.Value);
                           begin
                              if FER.Exists then
                                 if FER.Element /= Translated_Name then
                                    Ada.Text_IO.Put_Line ("Expected: '" & Translated_Name.To_String & "', but was: " & To_String (FER.Element) & "'");
                                 end if;
                              else
                                 Include (This        => Enum_Name_To_Size_Identifier_Map.all,
                                          Key         => F.Enum.Value,
                                          New_Element => Translated_Name);
                              end if;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location  & ", could not translate " & F.Kind.Value.To_String);
                        end if;
                     else
                        null; -- Far from all fields are expected to have an enum specified
                     end if;
                  end Handle_Request_Field;

                  procedure Process_Request_Child (Request_Child : X_Proto.Request.Fs.Child_Ptr) is
                  begin
                     case Request_Child.Kind_Id is
                        when X_Proto.Request.Fs.Child_Field            =>
                           Handle_Request_Field (Request_Child.F);
                        when X_Proto.Request.Fs.Child_Pad              =>
                           null;
                        when X_Proto.Request.Fs.Child_Value_Param      =>
                           null;
                        when X_Proto.Request.Fs.Child_Documentation    =>
                           null;
                        when X_Proto.Request.Fs.Child_Reply            =>
                           null;
                        when X_Proto.Request.Fs.Child_List             =>
                           if
                             Request_Child.L.Members.Is_Empty and
                             Request_Child.L.Name.Exists and
                             Request_Child.L.Kind.Exists
                           then
                              if not Names_Of_Types_To_Make_Array_Types.Contains (Request_Child.L.Kind.Value) then
                                 Names_Of_Types_To_Make_Array_Types.Append (Request_Child.L.Kind.Value);
                              end if;
                           end if;
                        when X_Proto.Request.Fs.Child_Expression_Field =>
                           null;
                     end case;
                  end Process_Request_Child;
               begin
                  for I in Positive range Request.Children.First_Index..Request.Children.Last_Index loop
                     Process_Request_Child (Request.Children.Element (I));
                  end loop;
               end;
            end if;
         end loop;
      end Pre_Process_Requests;

      procedure Generate_Ada_Code_For_Structs (Structs : X_Proto.Xcb.Fs.Struct_Vector.Elements_Array_T) is

         procedure Handle_Struct (Struct : X_Proto.Struct.T) is
         begin
            if Struct.Name.Exists then
               declare
                  Padding_Number : Aida.Int32.T := 0;
               begin
                  for Child of Struct.Members loop
                     case Child.Kind_Id is
                     when Field_Member =>
                        null;
                     when Pad_Member =>
                        if Child.P.Bytes.Value > 1 then
                           declare
                              Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Array_Type_Name (Prefix_Name => Struct.Name.Value.To_String,
                                                                Field_Name  => "Padding" & To_String (Padding_Number),
                                                                New_Name    => Variable_Type_Name);

                              Put_Tabs (1); Put_Line ("type " & Variable_Type_Name.To_String & " is array (0.." &
                                                        To_String (Aida.Int32.T (Child.P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                           end;
                        end if;
                        Padding_Number := Padding_Number  + 1;
                     when List_Member =>
                        null; -- This information does not have any impact on resulting Ada code. Why?
                     end case;
                  end loop;
               end;

               declare
                  New_Variable_Name                      : X_Proto.Large_Bounded_String.T;
                  New_Variable_Type_Name                 : X_Proto.Large_Bounded_String.T;
                  New_Variable_Access_Type_Name          : X_Proto.Large_Bounded_String.T;
                  New_Variable_Iterator_Type_Name        : X_Proto.Large_Bounded_String.T;
                  New_Variable_Iterator_Access_Type_Name : X_Proto.Large_Bounded_String.T;

                  Padding_Number : Aida.Int32.T := 0;
               begin
                  Generate_Struct_Name (Old_Name => Struct.Name.Value.To_String,
                                        New_Name => New_Variable_Name);

                  Generate_Classic_Type_Name (Old_Name => Struct.Name.Value.To_String,
                                              New_Name => New_Variable_Type_Name);
                  Generate_Classic_Access_Type_Name (Old_Name => Struct.Name.Value.To_String,
                                                     New_Name => New_Variable_Access_Type_Name);

                  Generate_Classic_Iterator_Type_Name (Old_Name => Struct.Name.Value.To_String,
                                                       New_Name => New_Variable_Iterator_Type_Name);

                  Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is record");

                  for Child of Struct.Members loop
                     case Child.Kind_Id is
                     when Field_Member =>
                        if Child.F.Kind.Exists then
                           declare
                              Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                              Is_Success : Boolean;
                           begin
                              Translate_Variable_Type_Name (Variable_Type_Name => Child.F.Kind.Value.To_String,
                                                            Is_Success         => Is_Success,
                                                            Translated_Name    => Variable_Type_Name);

                              if Is_Success then
                                 declare
                                    Field_Name : X_Proto.Large_Bounded_String.T;
                                 begin
                                    Generate_Struct_Name (Old_Name => Child.F.Name.Value.To_String,
                                                          New_Name => Field_Name);
                                    Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                                 end;
                              else
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & Child.F.Kind.Value.To_String);
                              end if;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                        end if;
                     when Pad_Member =>
                        if Child.P.Bytes.Value = 1 then
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           declare
                              New_Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Array_Type_Name (Prefix_Name => Struct.Name.Value.To_String,
                                                                Field_Name  => "Padding" & To_String (Padding_Number),
                                                                New_Name    => New_Variable_Type_Name);

                              Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & New_Variable_Type_Name.To_String & ";");
                           end;
                        end if;
                        Padding_Number := Padding_Number + 1;
                     when List_Member =>
                        null; -- This information does not have any impact on resulting Ada code. Why?
                     end case;
                  end loop;

                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Type_Name.To_String & ");");
                  Put_Line ("");
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
                  Put_Line ("");
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Type_Name.To_String & " is record");
                  Put_Tabs (2); Put_Line ("Data  : " & New_Variable_Access_Type_Name.To_String & ";");
                  Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
                  Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Iterator_Type_Name.To_String & ");");
                  Put_Line ("");

                  Generate_Classic_Iterator_Access_Type_Name (Old_Name => Struct.Name.Value.To_String,
                                                              New_Name => New_Variable_Iterator_Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Access_Type_Name.To_String & " is access all " &
                                            New_Variable_Iterator_Type_Name.To_String & ";");
                  Put_Line ("");

                  Insert (This        => Original_Variable_Name_To_Adaified_Name.all,
                          Key         => Struct.Name.Value,
                          New_Element => New_Variable_Name);

                  Insert (This        => Original_Name_To_Adaified_Name.all,
                          Key         => Struct.Name.Value,
                          New_Element => New_Variable_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Type_Name.all,
                          Key         => Struct.Name.Value,
                          New_Element => New_Variable_Iterator_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Access_Type_Name.all,
                          Key         => Struct.Name.Value,
                          New_Element => New_Variable_Iterator_Access_Type_Name);
               end;
            else
               Ada.Text_IO.Put_Line ("A struct exists without name attribute");
            end if;
         end Handle_Struct;
      begin
         for I in Structs'Range loop
            Handle_Struct (Structs (I).all);
         end loop;
      end Generate_Ada_Code_For_Structs;

      procedure Generate_Ada_Code_For_Structs is new X_Proto.Xcb.Fs.Struct_Vector.Act_On_Immutable_Elements (Generate_Ada_Code_For_Structs);

   begin
      declare
         Ada_Name : X_Proto.Large_Bounded_String.T;
      begin
         Ada_Name.Initialize ("CARD8");
         Eight_Bit_Variable_Type_Names.Append (Ada_Name);
         Ada_Name.Initialize ("BYTE");
         Eight_Bit_Variable_Type_Names.Append (Ada_Name);
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

      for Event of Xcb.Events.all loop
         if
           Event.Number.Exists and
           Event.Name.Exists
         then
            declare
               Constant_Name : X_Proto.Large_Bounded_String.T;
            begin
               Generate_Struct_Name (Old_Name => Event.Name.Value.To_String,
                                     New_Name => Constant_Name);
               Constant_Name.Initialize ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Constant_Name.To_String));
               Put_Tabs (1); Put_Line (Constant_Name.To_String & " : constant :=" & Event.Number.Value'Img & ";");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event without name or number!?");
         end if;
      end loop;

      Put_Line ("");

      for Event_Copy of Xcb.Event_Copies.all loop
         if
           Event_Copy.Number.Exists and
           Event_Copy.Name.Exists
         then
            declare
               Constant_Name : X_Proto.Large_Bounded_String.T;
            begin
               Generate_Struct_Name (Old_Name => Event_Copy.Name.Value.To_String,
                                     New_Name => Constant_Name);
               Constant_Name.Initialize ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Constant_Name.To_String));
               Put_Tabs (1); Put_Line (Constant_Name.To_String & " : constant :=" & Event_Copy.Number.Value'Img & ";");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event copy without name or number!?");
         end if;
      end loop;

      for Error of Xcb.Errors.all loop
         if
           Error.Number.Exists and
           Error.Name.Exists
         then
            declare
               Constant_Name : X_Proto.Large_Bounded_String.T;
            begin
               Generate_Struct_Name (Old_Name => Error.Name.Value.To_String,
                                     New_Name => Constant_Name);
               Constant_Name.Initialize ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Constant_Name.To_String));
               Put_Tabs (1); Put_Line (Constant_Name.To_String & " : constant :=" & Error.Number.Value'Img & ";");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error without name or number!?");
         end if;
      end loop;

      for Error_Copy of Xcb.Error_Copies.all loop
         if
           Error_Copy.Number.Exists and
           Error_Copy.Name.Exists
         then
            declare
               Constant_Name : X_Proto.Large_Bounded_String.T;
            begin
               Generate_Struct_Name (Old_Name => Error_Copy.Name.Value.To_String,
                                     New_Name => Constant_Name);
               Constant_Name.Initialize ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Constant_Name.To_String));
               Put_Tabs (1); Put_Line (Constant_Name.To_String & " : constant :=" & Error_Copy.Number.Value'Img & ";");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error copy without name or number!?");
         end if;
      end loop;

      Put_Line ("");

      for Request of Xcb.Requests.all loop
         if
           Request.Op_Code.Exists and
           Request.Name.Exists
         then
            declare
               Constant_Name : X_Proto.Large_Bounded_String.T;
            begin
               Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                     New_Name => Constant_Name);
               Constant_Name.Initialize ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Constant_Name.To_String));
               Put_Tabs (1); Put_Line (Constant_Name.To_String & " : constant :=" & Request.Op_Code.Value'Img & ";");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", event without name or number!?");
         end if;
      end loop;

      Put_Line ("");

      Put_Tabs (1); Put_Line ("-- Identifier for objects in the XCB library. For example Windows,");
      Put_Tabs (1); Put_Line ("-- Graphical Contexts,...");
      Put_Tabs (1); Put_Line ("type X_Id_Type is new Interfaces.Unsigned_32;");
      Put_Line ("");

      for X_Id_Union_Index in 1..Last_Index (Xcb.X_Id_Unions.all) loop
         declare
            X_Id_Union : X_Proto.X_Id_Union.Ptr renames Element (Xcb.X_Id_Unions.all, X_Id_Union_Index);
         begin
            if X_Id_Union.Name.Exists then
               Generate_Code_For_X_Id (X_Id_Union.Name.Value,
                                       "X_Id_Type",
                                       How => Use_The_New_Keyword);

               declare
                  Searched_For : X_Proto.Large_Bounded_String.T;
                  X_Id_Union_Type_Name : X_Proto.Large_Bounded_String.T;
               begin
                  Searched_For.Initialize (X_Id_Union.Name.Value.To_String);
                  declare
                     FER : constant Original_Name_To_Adaified_Name_P.Find_Element_Result_T :=
                       Find_Element (This => Original_Name_To_Adaified_Name.all,
                                     Key => Searched_For);
                  begin
                     if FER.Exists then
                        X_Id_Union_Type_Name := FER.Element;

                        for Kind of X_Id_Union.Kinds.all loop
                           if Kind.Value.Exists then
                              for X_Id_Index in 1..Last_Index (Xcb.X_Ids.all) loop
                                 declare
                                    X_Id : X_Proto.X_Id.Ptr renames Element (Xcb.X_Ids.all, X_Id_Index);
                                 begin
                                    if X_Id.Name.Exists then
                                       if Kind.Value.Value.To_String = X_Id.Name.Value.To_String then
                                          Generate_Code_For_X_Id (X_Id.Name.Value,
                                                                  X_Id_Union_Type_Name.To_String,
                                                                  How => Use_The_Subtype_Keyword);

                                          Processed_X_Ids.Append (X_Id.Name.Value);

                                          exit;
                                       end if;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                                    end if;
                                 end;
                              end loop;
                           else
                              Ada.Text_IO.Put_Line ("xidunion " & X_Id_Union.Name.Value.To_String & " has errors");
                           end if;
                        end loop;
                     else
                        Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Failed to translate: " & Searched_For.To_String);
                     end if;
                  end;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end;
      end loop;

      for X_Id_Index in 1..Last_Index (Xcb.X_Ids.all) loop
         declare
            X_Id : X_Proto.X_Id.Ptr renames Element (Xcb.X_Ids.all, X_Id_Index);
         begin
            if X_Id.Name.Exists then
               if not Processed_X_Ids.Contains (X_Id.Name.Value) then
                  Generate_Code_For_X_Id (X_Id.Name.Value,
                                          "X_Id_Type",
                                          How => Use_The_New_Keyword);
               end if;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
            end if;
         end;
      end loop;

      Put_Line ("");

      for Enum of Xcb.Enums.all loop
         if Enum.Name.Exists then
            declare
               Number_Of_Values : Integer := 0;
               Number_Of_Bits   : Integer := 0;
            begin
               for Item of Enum.Items.all loop
                  case Item.Kind_Id is
                     when X_Proto.Item.Fs.Not_Specified => null;
                     when X_Proto.Item.Fs.Specified_As_Value =>
                        Number_Of_Values := Number_Of_Values + 1;
                     when X_Proto.Item.Fs.Specified_As_Bit =>
                        Number_Of_Bits := Number_Of_Bits + 1;
                  end case;
               end loop;

               declare
                  New_Variable_Type_Name : X_Proto.Large_Bounded_String.T;

                  Enum_Prefix_Name       : X_Proto.Large_Bounded_String.T;
                  Enum_Value_Name        : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Classic_Type_Name (Old_Name => Enum.Name.Value.To_String,
                                              New_Name => New_Variable_Type_Name);

                  Generate_Struct_Name (Old_Name => Enum.Name.Value.To_String,
                                        New_Name => Enum_Prefix_Name);

                  if Enum.Name.Value.To_String = "Atom" then
                     for I in Positive range Enum.Items.First_Index..Enum.Items.Last_Index loop
                        Generate_Struct_Name (Old_Name => Enum.Items.Element (I).Name.Value.To_String,
                                              New_Name => Enum_Value_Name);

                        Put_Tabs (1); Put ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Enum_Prefix_Name.To_String & "_" & Enum_Value_Name.To_String) & " : constant Atom_Id_Type :=");
                        case Enum.Items.Element (I).Kind_Id is
                        when X_Proto.Item.Fs.Not_Specified =>
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", should never happen");
                           Put_Line ("0;");
                        when X_Proto.Item.Fs.Specified_As_Value =>
                           Put_Line (Enum.Items.Element (I).Value'Img & ";");
                        when X_Proto.Item.Fs.Specified_As_Bit =>
                           Put_Line (Value_Of_Bit (Enum.Items.Element (I).Bit)'Img & ";");
                        end case;
                     end loop;
                     Put_Tabs (1); Put_Line ("");
                  else
                     declare
                        Largest_Value : constant Long_Integer := Determine_Largest_Value (Enum.Items.all);

                        FER : constant Enum_Name_To_Size_Identifier_Map_P.Find_Element_Result_T :=
                          Find_Element (Enum_Name_To_Size_Identifier_Map.all, Enum.Name.Value);
                     begin
                        if FER.Exists then
                           Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is new " & To_String (FER.Element) &";");
                        else
                           if Largest_Value <= 127 then
                              Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is new Interfaces.Unsigned_8;");
                              Eight_Bit_Variable_Type_Names.Append (Enum.Name.Value);
                           else
                              Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is new Interfaces.Unsigned_32;");
                           end if;
                        end if;

                        Insert (This        => Original_Name_To_Adaified_Name.all,
                                Key         => Enum.Name.Value,
                                New_Element => New_Variable_Type_Name);


                        for I in Positive range Enum.Items.First_Index..Enum.Items.Last_Index loop
                           Generate_Struct_Name (Old_Name => Enum.Items.Element (I).Name.Value.To_String,
                                                 New_Name => Enum_Value_Name);

                           Put_Tabs (1); Put ("XCB_" & Strings_Edit.UTF8.Mapping.To_Uppercase (Enum_Prefix_Name.To_String & "_" & Enum_Value_Name.To_String) & " : constant " &
                                                New_Variable_Type_Name.To_String & " :=");
                           case Enum.Items.Element (I).Kind_Id is
                              when X_Proto.Item.Fs.Not_Specified =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", should never happen");
                                 Put_Line ("0;");
                              when X_Proto.Item.Fs.Specified_As_Value =>
                                 Put_Line (Enum.Items.Element (I).Value'Img & ";");
                              when X_Proto.Item.Fs.Specified_As_Bit =>
                                 Put_Line (Value_Of_Bit (Enum.Items.Element (I).Bit)'Img & ";");
                           end case;
                        end loop;
                        Put_Tabs (1); Put_Line ("");
                     end;
                  end if;
               end;
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
         end if;
      end loop;

      for Type_Definition of Xcb.Type_Definitions.all loop
         if Type_Definition.Old_Name.Exists and Type_Definition.New_Name.Exists then
            declare
               Old_Variable_Type_Name                 : X_Proto.Large_Bounded_String.T;
               New_Variable_Name                      : X_Proto.Large_Bounded_String.T;
               New_Variable_Type_Name                 : X_Proto.Large_Bounded_String.T;
               New_Variable_Access_Type_Name          : X_Proto.Large_Bounded_String.T;
               New_Variable_Iterator_Type_Name        : X_Proto.Large_Bounded_String.T;
               New_Variable_Iterator_Access_Type_Name : X_Proto.Large_Bounded_String.T;
               Is_Success : Boolean;
            begin
               Translate_Classic_Variable_Type_Name (Variable_Type_Name => Type_Definition.Old_Name.Value.To_String,
                                                     Is_Success         => Is_Success,
                                                     Translated_Name    => Old_Variable_Type_Name);

               if Is_Success then
                  Generate_Struct_Name (Old_Name => Type_Definition.New_Name.Value.To_String,
                                        New_Name => New_Variable_Name);

                  Generate_Classic_Type_Name (Old_Name => Type_Definition.New_Name.Value.To_String,
                                              New_Name => New_Variable_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is new " & Old_Variable_Type_Name.To_String & ";");
                  Put_Line ("");

                  Generate_Classic_Access_Type_Name (Old_Name => Type_Definition.New_Name.Value.To_String,
                                                     New_Name => New_Variable_Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
                  Put_Line ("");

                  Generate_Classic_Iterator_Type_Name (Old_Name => Type_Definition.New_Name.Value.To_String,
                                                       New_Name => New_Variable_Iterator_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Type_Name.To_String & " is record");
                  Put_Tabs (2); Put_Line ("Data  : " & New_Variable_Access_Type_Name.To_String & ";");
                  Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
                  Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
                  Put_Tabs (1); Put_Line ("end record;");
                  Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Iterator_Type_Name.To_String & ");");
                  Put_Line ("");

                  Generate_Classic_Iterator_Access_Type_Name (Old_Name => Type_Definition.New_Name.Value.To_String,
                                                              New_Name => New_Variable_Iterator_Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Access_Type_Name.To_String & " is access all " &
                                          New_Variable_Iterator_Type_Name.To_String & ";");
                  Put_Line ("");

                  Insert (This        => Original_Variable_Name_To_Adaified_Name.all,
                          Key         => Type_Definition.New_Name.Value,
                          New_Element => New_Variable_Name);

                  Insert (This        => Original_Name_To_Adaified_Name.all,
                          Key         => Type_Definition.New_Name.Value,
                          New_Element => New_Variable_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Type_Name.all,
                          Key         => Type_Definition.New_Name.Value,
                          New_Element => New_Variable_Iterator_Type_Name);

                  Insert (This        => Original_Name_To_Adaified_Iterator_Access_Type_Name.all,
                          Key         => Type_Definition.New_Name.Value,
                          New_Element => New_Variable_Iterator_Access_Type_Name);

                  if
                    Type_Definition.Old_Name.Value.To_String = "CARD8" or
                    Type_Definition.Old_Name.Value.To_String = "BYTE" or
                    Eight_Bit_Variable_Type_Names.Contains (Type_Definition.Old_Name.Value)
                  then
                     Eight_Bit_Variable_Type_Names.Append (Type_Definition.New_Name.Value);
                  elsif
                    Type_Definition.Old_Name.Value.To_String = "CARD32"
                  then
                     Thirty_Two_Bit_Variable_Type_Names.Append (Type_Definition.New_Name.Value);
                  end if;
               else
                  Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Failed to translate: " & Type_Definition.Old_Name.Value.To_String);
               end if;
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
         end if;
      end loop;

      Generate_Ada_Code_For_Structs (XCB.Structs.all);

      for Text of Names_Of_Types_To_Make_Array_Types loop
         declare
            Variable_Type_Name       : X_Proto.Large_Bounded_String.T;
            Variable_Array_Type_Name : X_Proto.Large_Bounded_String.T;
            Is_Success : Boolean;
         begin
            Translate_Variable_Type_Name (Variable_Type_Name => Text.To_String,
                                          Is_Success         => Is_Success,
                                          Translated_Name    => Variable_Type_Name);

            if Is_Success then
               Generate_Classic_Array_Type_Name (Prefix_Name => Text.To_String,
                                                 Field_Name  => "",
                                                 New_Name    => Variable_Array_Type_Name);
               Put_Tabs (1); Put_Line ("type " & Variable_Array_Type_Name.To_String & " is array (Natural range <>) of " & Variable_Type_Name.To_String & ";");
               Put_Tabs (1); Put_Line ("pragma Convention (C, " & Variable_Array_Type_Name.To_String & ");");
               Put_Line ("");
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Could not translate type " & Text.To_String);
            end if;
         end;
      end loop;

      for Union of Xcb.Unions.all loop
         if Union.Name.Exists then
            declare
               New_Variable_Type_Name                 : X_Proto.Large_Bounded_String.T;
               New_Variable_Access_Type_Name          : X_Proto.Large_Bounded_String.T;
               New_Variable_Iterator_Type_Name        : X_Proto.Large_Bounded_String.T;
               New_Variable_Iterator_Access_Type_Name : X_Proto.Large_Bounded_String.T;

               Discriminant_Number : Integer := 0; -- is increased for each field in the union
            begin
               for I in Positive range Union.Children.First_Index..Union.Children.Last_Index loop
                  case Union.Children.all.Element (I).Kind_Id is
                     when X_Proto.Union.Fs.Child_List =>
                        if Union.Children.Element (I).L.Members.Length = 1 then
                           Generate_Classic_Array_Type_Name (Prefix_Name => Union.Name.Value.To_String,
                                                             Field_Name  => Union.Children.Element (I).L.Name.Value.To_String,
                                                             New_Name    => New_Variable_Type_Name);

                           case Union.Children.Element (I).L.Members.Element (1).Kind_Id is
                              when X_Proto.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & Union.Name.Value.To_String & " with list field child is unimplemented.");
                              when X_Proto.List.Fs.List_Member_Kind_Value =>
                                 declare
                                    Is_Success : Boolean;
                                    N : X_Proto.Large_Bounded_String.T;
                                 begin
                                    Translate_Classic_Variable_Type_Name (Variable_Type_Name => Union.Children.Element (I).L.Kind.Value.To_String,
                                                                          Is_Success         => Is_Success,
                                                                          Translated_Name    => N);

                                    if Is_Success then
                                       Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is array (0.." &
                                                                 To_String (Aida.Int32.T (Union.Children.Element (I).L.Members.Element (1).Value - 1)) & ") of aliased " & N.To_String & ";");
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & Union.Name.Value.To_String & ", failed to identify kind of array item: " &
                                                               Union.Children.Element (I).L.Kind.Value.To_String);
                                    end if;
                                 end;
                              when X_Proto.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & Union.Name.Value.To_String & " with list kind child is unimplemented.");
                           end case;

                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Union " & Union.Name.Value.To_String & " contains list child with" &
                                                Union.Children.Element (I).L.Members.Length'Img & " number fo children");
                        end if;
                  end case;
               end loop;

               Generate_Classic_Type_Name (Old_Name => Union.Name.Value.To_String,
                                           New_Name => New_Variable_Type_Name);

               Generate_Classic_Access_Type_Name (Old_Name => Union.Name.Value.To_String,
                                                  New_Name => New_Variable_Access_Type_Name);

               Generate_Classic_Iterator_Type_Name (Old_Name => Union.Name.Value.To_String,
                                                    New_Name => New_Variable_Iterator_Type_Name);

               Generate_Classic_Iterator_Access_Type_Name (Old_Name => Union.Name.Value.To_String,
                                                           New_Name => New_Variable_Iterator_Access_Type_Name);

               Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " (Discriminant : Natural := 0) is record");
               Put_Tabs (2); Put_Line ("case Discriminant is");

               for I in Positive range Union.Children.First_Index..Union.Children.Last_Index loop
                  case Union.Children.all.Element (I).Kind_Id is
                     when X_Proto.Union.Fs.Child_List =>
                        declare
                           Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                        begin
                           Generate_Classic_Array_Type_Name (Prefix_Name => Union.Name.Value.To_String,
                                                             Field_Name  => Union.Children.Element (I).L.Name.Value.To_String,
                                                             New_Name    => Variable_Type_Name);

                           declare
                              Field_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => Union.Children.Element (I).L.Name.Value.To_String,
                                                    New_Name => Field_Name);

                              if Union.Children.Last_Index /= I then
                                 Put_Tabs (3); Put_Line ("when" & Discriminant_Number'Img & " =>");
                                 Put_Tabs (4); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                              else
                                 Put_Tabs (3); Put_Line ("when others =>");
                                 Put_Tabs (4); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                              end if;
                              Discriminant_Number := Discriminant_Number + 1;
                           end;
                        end;
                  end case;
               end loop;

               Put_Tabs (2); Put_Line ("end case;");
               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Type_Name.To_String & ");");
               Put_Tabs (1); Put_Line ("pragma Unchecked_Union (" & New_Variable_Type_Name.To_String & ");");
               Put_Tabs (1); Put_Line ("");
               Put_Tabs (1); Put_Line ("type " & New_Variable_Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
               Put_Line ("");
               Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Type_Name.To_String & " is record");
               Put_Tabs (2); Put_Line ("Data  : " & New_Variable_Access_Type_Name.To_String & ";");
               Put_Tabs (2); Put_Line ("C_Rem : aliased Interfaces.C.int;");
               Put_Tabs (2); Put_Line ("Index : aliased Interfaces.C.int;");
               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Iterator_Type_Name.To_String & ");");
               Put_Line ("");

               Put_Tabs (1); Put_Line ("type " & New_Variable_Iterator_Access_Type_Name.To_String & " is access all " &
                                         New_Variable_Iterator_Type_Name.To_String & ";");
               Put_Line ("");


               Insert (This        => Original_Name_To_Adaified_Name.all,
                       Key         => Union.Name.Value,
                       New_Element => New_Variable_Type_Name);
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Union exists without name!");
         end if;
      end loop;

      for Event of Xcb.Events.all loop
         if Event.Name.Exists then
            declare
               New_Variable_Type_Name : X_Proto.Large_Bounded_String.T;

               Padding_Number : Aida.Int32.T := 0;
            begin
               for I in Positive range Event.Members.First_Index..Event.Members.Last_Index loop
                  case Event.Members.all.Element (I).Kind_Id is
                     when X_Proto.Event.Fs.Event_Member_Field =>
                        null;
                     when X_Proto.Event.Fs.Event_Member_Pad =>
                        if Event.Members.Element (I).P.Bytes.Value > 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => Event.Name.Value.To_String,
                                                                  List_Name => "Padding" & To_String (Padding_Number),
                                                                  New_Name  => New_Variable_Type_Name);

                           Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is array (0.." &
                                                     To_String (Aida.Int32.T (Event.Members.Element (I).P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                        end if;
                        Padding_Number := Padding_Number  + 1;
                     when X_Proto.Event.Fs.Event_Member_Doc =>
                        null;
                     when X_Proto.Event.Fs.Event_Member_List =>
                        if Event.Members.Element (I).L.Members.Length = 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => Event.Name.Value.To_String,
                                                                  List_Name => Event.Members.Element (I).L.Name.Value.To_String,
                                                                  New_Name  => New_Variable_Type_Name);

                           case Event.Members.Element (I).L.Members.Element (1).Kind_Id is
                              when X_Proto.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " with list field child is unimplemented.");
                              when X_Proto.List.Fs.List_Member_Kind_Value =>
                                 declare
                                    Is_Success : Boolean;
                                    N : X_Proto.Large_Bounded_String.T;
                                 begin
                                    Translate_Classic_Variable_Type_Name (Variable_Type_Name => Event.Members.Element (I).L.Kind.Value.To_String,
                                                                          Is_Success         => Is_Success,
                                                                          Translated_Name    => N);

                                    if Is_Success then
                                       Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is array (0.." &
                                                                 To_String (Aida.Int32.T (Event.Members.Element (I).L.Members.Element (1).Value) - 1) & ") of aliased " & N.To_String & ";");
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & ", failed to identify kind of array item: " &
                                                               Event.Members.Element (I).L.Kind.Value.To_String);
                                    end if;
                                 end;
                              when X_Proto.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " with list kind child is unimplemented.");
                           end case;

                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " contains list child with" &
                                                Event.Members.Element (I).L.Members.Length'Img & " number fo children");
                        end if;
                  end case;
               end loop;

               Padding_Number := 0;

               Generate_Classic_Event_Type_Name (Old_Name => Event.Name.Value.To_String,
                                                 New_Name => New_Variable_Type_Name);

               Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is record");
               Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");

               for I in Positive range Event.Members.First_Index..Event.Members.Last_Index loop
                  case Event.Members.all.Element (I).Kind_Id is
                     when X_Proto.Event.Fs.Event_Member_Field =>
                        declare
                           Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           Is_Success : Boolean;
                        begin
                           Translate_Variable_Type_Name (Variable_Type_Name => Event.Members.Element (I).F.Kind.Value.To_String,
                                                         Is_Success         => Is_Success,
                                                         Translated_Name    => Variable_Type_Name);

                           if Is_Success then
                              declare
                                 Field_Name : X_Proto.Large_Bounded_String.T;
                              begin
                                 Generate_Struct_Name (Old_Name => Event.Members.Element (I).F.Name.Value.To_String,
                                                       New_Name => Field_Name);

                                 if Event.Members.First_Index = I then
                                    if
                                      Eight_Bit_Variable_Type_Names.Contains (Event.Members.Element (I).F.Kind.Value)
                                    then
                                       if Event.Members.Element (I).F.Enum.Exists then
                                          Translate_Variable_Type_Name (Variable_Type_Name => Event.Members.Element (I).F.Enum.Value.To_String,
                                                                        Is_Success         => Is_Success,
                                                                        Translated_Name    => Variable_Type_Name);
                                       end if;


                                       Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");

                                       if Event.No_Sequence_Number.Exists and then not Event.No_Sequence_Number.Value then
                                          Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       elsif not Event.No_Sequence_Number.Exists then
                                          Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       end if;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " has first field non-8-bits.");
                                       -- This is interesting because in xproto.xml for lib xcb version 1.10 the
                                       -- first field in all events were 8-bits!

                                       Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                                       Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                       Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");

                                       Padding_Number := Padding_Number + 1;
                                    end if;
                                 else
                                    Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                                 end if;
                              end;
                           else
                              Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & Event.Members.Element (I).F.Kind.Value.To_String);
                           end if;
                        end;
                     when X_Proto.Event.Fs.Event_Member_Pad =>
                        if Event.Members.Element (I).P.Bytes.Value = 1 then
                           Put_Tabs (2); Put_Line (   "Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           Generate_Classic_Event_List_Type_Name (Enum_Name => Event.Name.Value.To_String,
                                                                  List_Name => "Padding" & To_String (Padding_Number),
                                                                  New_Name  => New_Variable_Type_Name);
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & New_Variable_Type_Name.To_String & ";");
                        end if;
                        Padding_Number := Padding_Number + 1;
                     when X_Proto.Event.Fs.Event_Member_Doc =>
                        null;
                     when X_Proto.Event.Fs.Event_Member_List =>
                        if Event.Members.Element (I).L.Members.Length = 1 then
                           Generate_Classic_Event_List_Type_Name (Enum_Name => Event.Name.Value.To_String,
                                                                  List_Name => Event.Members.Element (I).L.Name.Value.To_String,
                                                                  New_Name  => New_Variable_Type_Name);

                           declare
                              Variable_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Struct_Name (Old_Name => Event.Members.Element (I).L.Name.Value.To_String,
                                                    New_Name => Variable_Name);
                              case Event.Members.Element (I).L.Members.Element (1).Kind_Id is
                              when X_Proto.List.Fs.List_Member_Kind_Field_Reference =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " with list field child is unimplemented.");
                              when X_Proto.List.Fs.List_Member_Kind_Value =>
                                 Put_Tabs (2); Put_Line (Variable_Name.To_String & " : aliased " & New_Variable_Type_Name.To_String & ";");
                              when X_Proto.List.Fs.List_Member_Kind_Operation =>
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " with list kind child is unimplemented.");
                              end case;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event " & Event.Name.Value.To_String & " contains list child with" &
                                                Event.Members.Element (I).L.Members.Length'Img & " number fo children");
                        end if;
                  end case;
               end loop;

               Generate_Classic_Event_Type_Name (Old_Name => Event.Name.Value.To_String,
                                                 New_Name => New_Variable_Type_Name);

               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Type_Name.To_String & ");");
               Put_Line ("");
               declare
                  Access_Type_Name : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Classic_Event_Access_Type_Name (Old_Name => Event.Name.Value.To_String,
                                                           New_Name => Access_Type_Name);
                  Put_Tabs (1); Put_Line ("type " & Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
                  Put_Line ("");
               end;
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
         end if;
      end loop;

      for Event_Copy of Xcb.Event_Copies.all loop
         if Event_Copy.Name.Exists then
            if Event_Copy.Ref.Exists then
               declare
                  Original_Type_Name : X_Proto.Large_Bounded_String.T;
                  Derived_Type_Name  : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Classic_Event_Type_Name (Old_Name => Event_Copy.Name.Value.To_String,
                                                    New_Name => Derived_Type_Name);

                  Generate_Classic_Event_Type_Name (Old_Name => Event_Copy.Ref.Value.To_String,
                                                    New_Name => Original_Type_Name);

                  Put_Tabs (1); Put_Line ("type " & Derived_Type_Name.To_String & " is new " & Original_Type_Name.To_String & ";");
                  Put_Line ("");
                  declare
                     Access_Type_Name : X_Proto.Large_Bounded_String.T;
                  begin
                     Generate_Classic_Event_Access_Type_Name (Old_Name => Event_Copy.Name.Value.To_String,
                                                              New_Name => Access_Type_Name);
                     Put_Tabs (1); Put_Line ("type " & Access_Type_Name.To_String & " is access all " & Derived_Type_Name.To_String & ";");
                     Put_Line ("");
                  end;
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy " & Event_Copy.Name.Value.To_String & " with no ref!?");
            end if;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy exists without a name!?");
         end if;
      end loop;

      for Error of Xcb.Errors.all loop
         if Error.Name.Exists then
            declare
               Padding_Number : Aida.Int32.T := 0;
            begin
               for Child of Error.Children.all loop
                  case Child.Kind_Id is
                     when X_Proto.Error.Fs.Child_Field =>
                        null;
                     when X_Proto.Error.Fs.Child_Pad =>
                        if Child.P.Bytes.Value > 1 then
                           declare
                              Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Array_Type_Name (Prefix_Name => Error.Name.Value.To_String,
                                                                Field_Name  => "Padding" & To_String (Padding_Number),
                                                                New_Name    => Variable_Type_Name);

                              Put_Tabs (1); Put_Line ("type " & Variable_Type_Name.To_String & " is array (0.." &
                                                        To_String (Aida.Int32.T (Child.P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                           end;
                        end if;
                        Padding_Number := Padding_Number  + 1;
                  end case;
               end loop;
            end;

            declare
               Error_Type_Name : X_Proto.Large_Bounded_String.T;
               Padding_Number : Aida.Int32.T := 0;
            begin
               Generate_Classic_Error_Type_Name (Old_Name => Error.Name.Value.To_String,
                                                 New_Name => Error_Type_Name);
               Put_Tabs (1); Put_Line ("type " & Error_Type_Name.To_String & " is record");

               Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
               Put_Tabs (2); Put_Line ("Error_Code : aliased Interfaces.Unsigned_8;");
               Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");

               for Child of Error.Children.all loop
                  case Child.Kind_Id is
                     when X_Proto.Error.Fs.Child_Field =>
                        if Child.F.Kind.Exists then
                           declare
                              Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                              Is_Success : Boolean;
                           begin
                              Translate_Variable_Type_Name (Variable_Type_Name => Child.F.Kind.Value.To_String,
                                                            Is_Success         => Is_Success,
                                                            Translated_Name    => Variable_Type_Name);

                              if Is_Success then
                                 declare
                                    Field_Name : X_Proto.Large_Bounded_String.T;
                                 begin
                                    Generate_Struct_Name (Old_Name => Child.F.Name.Value.To_String,
                                                          New_Name => Field_Name);
                                    Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");

                                    if Child.F.Enum.Exists then
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                                    end if;
                                 end;
                              else
                                 Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & Child.F.Kind.Value.To_String);
                              end if;
                           end;
                        else
                           Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", error");
                        end if;
                     when X_Proto.Error.Fs.Child_Pad =>
                        if Child.P.Bytes.Value = 1 then
                           Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased Interfaces.Unsigned_8;");
                        else
                           declare
                              New_Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                           begin
                              Generate_Classic_Event_List_Type_Name (Enum_Name => Error.Name.Value.To_String,
                                                                     List_Name => "Padding" & To_String (Padding_Number),
                                                                     New_Name  => New_Variable_Type_Name);
                              Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & New_Variable_Type_Name.To_String & ";");
                           end;
                        end if;
                        Padding_Number := Padding_Number + 1;
                  end case;
               end loop;

               Put_Tabs (1); Put_Line ("end record;");
               Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & Error_Type_Name.To_String & ");");
               Put_Line ("");
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Error exists without a name!?");
         end if;
      end loop;

      for Error_Copy of Xcb.Error_Copies.all loop
         if Error_Copy.Name.Exists then
            if Error_Copy.Ref.Exists then
               declare
                  Original_Type_Name : X_Proto.Large_Bounded_String.T;
                  Derived_Type_Name  : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Classic_Error_Type_Name (Old_Name => Error_Copy.Name.Value.To_String,
                                                    New_Name => Derived_Type_Name);

                  Generate_Classic_Error_Type_Name (Old_Name => Error_Copy.Ref.Value.To_String,
                                                    New_Name => Original_Type_Name);

                  Put_Tabs (1); Put_Line ("type " & Derived_Type_Name.To_String & " is new " & Original_Type_Name.To_String & ";");
                  Put_Line ("");
               end;
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy " & Error_Copy.Name.Value.To_String & " with no ref!?");
            end if;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Event copy exists without a name!?");
         end if;
      end loop;

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

      for I in 1..Last_Index (XCB.Structs.all) loop
         if Element (XCB.Structs.all, I).Name.Exists then
            Generate_Code_For_Next_Procedure (Element (XCB.Structs.all, I).Name.Value.To_String);
            Generate_Code_For_End_Function (Element (XCB.Structs.all, I).Name.Value.To_String);
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Struct exists without a name!?");
         end if;
      end loop;

      for X_Id_Index in 1..Last_Index (Xcb.X_Ids.all) loop
         declare
            X_Id : X_Proto.X_Id.Ptr renames Element (Xcb.X_Ids.all, X_Id_Index);
         begin
            if X_Id.Name.Exists then
               Generate_Code_For_Next_Procedure (X_Id.Name.Value.To_String);
               Generate_Code_For_End_Function (X_Id.Name.Value.To_String);
            else
               Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "X_Id exists without a name!?");
            end if;
         end;
      end loop;

      for Type_Def of XCB.Type_Definitions.all loop
         if Type_Def.New_Name.Exists then
            Generate_Code_For_Next_Procedure (Type_Def.New_Name.Value.To_String);
            Generate_Code_For_End_Function (Type_Def.New_Name.Value.To_String);
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Type definition exists without new name!?");
         end if;
      end loop;

      for Request of XCB.Requests.all loop
         if Request.Name.Exists then
            declare
               Does_Specified_Reply_Exist : Boolean := False;

               Request_Reply_Access_Type_Name : X_Proto.Large_Bounded_String.T;

               Shall_Generate_Size_Of_Function : Boolean := False;

               procedure Process (Request_Child : X_Proto.Request.Fs.Child_Ptr) is
               begin
                  case Request_Child.Kind_Id is
                     when X_Proto.Request.Fs.Child_Field =>
                        null;
                     when X_Proto.Request.Fs.Child_Pad =>
                        null;
                     when X_Proto.Request.Fs.Child_Value_Param =>
                        Shall_Generate_Size_Of_Function := True;
                     when X_Proto.Request.Fs.Child_Documentation =>
                        null;
                     when X_Proto.Request.Fs.Child_Reply =>
                        Does_Specified_Reply_Exist := True;

                        declare
                           Padding_Number : Aida.Int32.T := 0;
                           Reply_Name : X_Proto.Large_Bounded_String.T;
                        begin
                           Reply_Name.Initialize (Request.Name.Value.To_String & "Reply");

                           for Child of Request_Child.R.Children.all loop
                              case Child.Kind_Id is
                                 when X_Proto.Reply.Fs.Child_Field =>
                                    null;
                                 when X_Proto.Reply.Fs.Child_Pad =>
                                    if Child.P.Bytes.Value > 1 then
                                       declare
                                          Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                                       begin
                                          Generate_Classic_Array_Type_Name (Prefix_Name => Reply_Name.To_String,
                                                                            Field_Name  => "Padding" & To_String (Padding_Number),
                                                                            New_Name    => Variable_Type_Name);

                                          Put_Tabs (1); Put_Line ("type " & Variable_Type_Name.To_String & " is array (0.." &
                                                                    To_String (Aida.Int32.T (Child.P.Bytes.Value) - 1) & ") of aliased Interfaces.Unsigned_8;");
                                       end;
                                    end if;
                                    Padding_Number := Padding_Number  + 1;
                                 when X_Proto.Reply.Fs.Child_Documentation =>
                                    null;
                                 when X_Proto.Reply.Fs.Child_List =>
                                    Shall_Generate_Size_Of_Function := True;
                              end case;
                           end loop;
                        end;

                        declare
                           New_Variable_Name             : X_Proto.Large_Bounded_String.T;
                           New_Variable_Type_Name        : X_Proto.Large_Bounded_String.T;

                           Padding_Number : Aida.Int32.T := 0;

                           Reply_Name : X_Proto.Large_Bounded_String.T;

                           procedure Process_Reply_Child (Reply_Child : X_Proto.Reply.Fs.Child_Ptr;
                                                          Is_First    : Boolean) is
                           begin
                              case Reply_Child.Kind_Id is
                                 when X_Proto.Reply.Fs.Child_Field =>
                                    if Reply_Child.F.Kind.Exists then
                                       declare
                                          Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                                          Is_Success : Boolean;
                                       begin
                                          Translate_Variable_Type_Name (Variable_Type_Name => Reply_Child.F.Kind.Value.To_String,
                                                                        Is_Success         => Is_Success,
                                                                        Translated_Name    => Variable_Type_Name);

                                          if Is_Success then
                                             declare
                                                Field_Name : X_Proto.Large_Bounded_String.T;
                                             begin
                                                Generate_Struct_Name (Old_Name => Reply_Child.F.Name.Value.To_String,
                                                                      New_Name => Field_Name);

                                                if Is_First then
                                                   if
                                                     Eight_Bit_Variable_Type_Names.Contains (Reply_Child.F.Kind.Value)
                                                   then
                                                      if Reply_Child.F.Enum.Exists then
                                                         Translate_Variable_Type_Name (Variable_Type_Name => Reply_Child.F.Enum.Value.To_String,
                                                                                       Is_Success         => Is_Success,
                                                                                       Translated_Name    => Variable_Type_Name);
                                                      end if;

                                                      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                                                      Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                                      Put_Tabs (2); Put_Line ("Length : aliased Interfaces.Unsigned_32;");
                                                   elsif Reply_Child.F.Kind.Value.To_String = "BOOL" then
                                                      Put_Tabs (2); Put_Line ("Response_Kind : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased Interfaces.Unsigned_8;");
                                                      Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.Unsigned_16;");
                                                      Put_Tabs (2); Put_Line ("Length : aliased Interfaces.Unsigned_32;");
                                                   else
                                                      Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", Request " & Request.Name.Value.To_String &
                                                                              ", reply " & Reply_Child.F.Name.Value.To_String & " has first field " &
                                                                            Reply_Child.F.Kind.Value.To_String & ", which is non-8-bits.");
                                                   end if;
                                                else
                                                   Put_Tabs (2); Put_Line (Field_Name.To_String & " : aliased " & Variable_Type_Name.To_String & ";");
                                                end if;
                                             end;
                                          else
                                             Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & " Unknown field type name " & Reply_Child.F.Kind.Value.To_String);
                                          end if;
                                       end;
                                    else
                                       Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & ", kind does not exist!?");
                                    end if;
                                 when X_Proto.Reply.Fs.Child_Pad =>
                                    if Reply_Child.P.Bytes.Value = 1 then
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
                                          New_Variable_Type_Name : X_Proto.Large_Bounded_String.T;
                                       begin
                                          Generate_Classic_Array_Type_Name (Prefix_Name => Reply_Name.To_String,
                                                                            Field_Name  => "Padding" & To_String (Padding_Number),
                                                                            New_Name    => New_Variable_Type_Name);

                                          Put_Tabs (2); Put_Line ("Padding_" & To_String (Padding_Number) & " : aliased " & New_Variable_Type_Name.To_String & ";");
                                       end;
                                    end if;
                                    Padding_Number := Padding_Number + 1;
                                 when X_Proto.Reply.Fs.Child_Documentation =>
                                    null;
                                 when X_Proto.Reply.Fs.Child_List =>
                                    null; -- This information does not have any impact on resulting Ada code. Why?
                              end case;
                           end Process_Reply_Child;

                        begin
                           Reply_Name.Initialize (Request.Name.Value.To_String & "Reply");

                           Generate_Struct_Name (Old_Name => Reply_Name.To_String,
                                                 New_Name => New_Variable_Name);

                           Generate_Classic_Type_Name (Old_Name => Reply_Name.To_String,
                                                       New_Name => New_Variable_Type_Name);

                           Generate_Classic_Access_Type_Name (Old_Name => Reply_Name.To_String,
                                                              New_Name => Request_Reply_Access_Type_Name);

                           Put_Tabs (1); Put_Line ("type " & New_Variable_Type_Name.To_String & " is record");

                           for I in Positive range Request_Child.R.Children.First_Index..Request_Child.R.Children.Last_Index loop
                              Process_Reply_Child (Request_Child.R.Children.Element (I),
                                                   Request_Child.R.Children.First_Index = I);
                           end loop;

                           Put_Tabs (1); Put_Line ("end record;");
                           Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & New_Variable_Type_Name.To_String & ");");
                           Put_Line ("");
                           Put_Tabs (1); Put_Line ("type " & Request_Reply_Access_Type_Name.To_String & " is access all " & New_Variable_Type_Name.To_String & ";");
                           Put_Tabs (1); Put_Line ("for " & Request_Reply_Access_Type_Name.To_String & "'Storage_Size use 0;");
                           Put_Tabs (1); Put_Line ("pragma Convention (C, " & Request_Reply_Access_Type_Name.To_String & ");");
                           Put_Line ("");
                        end;
                     when X_Proto.Request.Fs.Child_List =>
                        Shall_Generate_Size_Of_Function := True;
                     when X_Proto.Request.Fs.Child_Expression_Field =>
                        null;
                  end case;
               end Process;

               Reply_Type_Name : X_Proto.Large_Bounded_String.T;

               procedure Generate_Checked_Or_Unchecked_Function (Suffix : String) is
                  Name            : X_Proto.Large_Bounded_String.T;
                  C_Function_Name : X_Proto.Large_Bounded_String.T;
                  Function_Name   : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                        New_Name => Name); -- There is risk here of erroneous Name

                  Function_Name.Initialize (Name.To_String & Suffix);

                  C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name.To_String & Suffix));

                  Generate_Request_With_Reply_Code (Function_Name,
                                                    C_Function_Name,
                                                    Request.Children.all,
                                                    Request.Name.Value.To_String,
                                                    Reply_Type_Name);
               end Generate_Checked_Or_Unchecked_Function;

            begin
               for I in Positive range Request.Children.First_Index..Request.Children.Last_Index loop
                  Process (Request.Children.Element (I));
               end loop;

               if Shall_Generate_Size_Of_Function then
                  declare
                     Name            : X_Proto.Large_Bounded_String.T;
                     C_Function_Name : X_Proto.Large_Bounded_String.T;
                     Function_Name   : X_Proto.Large_Bounded_String.T;
                  begin
                     Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                           New_Name => Name); -- There is risk here of erroneous Name

                     Function_Name.Initialize (Name.To_String & "_Size_Of");

                     C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name.To_String) & "_sizeof");
                     Put_Tabs (1); Put_Line ("function " & Function_Name.To_String & " (Buffer : System.Address) return Interfaces.C.int;");
                     Put_Tabs (1); Put_Line ("pragma Import (C, " & Function_Name.To_String & ", """ & C_Function_Name.To_String & """);");
                     Put_Line ("");
                  end;
               end if;

               if Does_Specified_Reply_Exist then
                  declare
                     Name      : X_Proto.Large_Bounded_String.T;
                     Type_Name : X_Proto.Large_Bounded_String.T;
                  begin
                     Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                           New_Name => Name);

                     Type_Name.Initialize (Name.To_String & "_Cookie_Type");

                     Put_Tabs (1); Put_Line ("type " & Type_Name.To_String & " is record");
                     Put_Tabs (2); Put_Line ("Sequence : aliased Interfaces.C.unsigned;");
                     Put_Tabs (1); Put_Line ("end record;");
                     Put_Tabs (1); Put_Line ("pragma Convention (C_Pass_By_Copy, " & Type_Name.To_String & ");");
                     Put_Line ("");

                     Reply_Type_Name := Type_Name;
                  end;

                  declare
                     Name            : X_Proto.Large_Bounded_String.T;
                     C_Function_Name : X_Proto.Large_Bounded_String.T;
                     Function_Name   : X_Proto.Large_Bounded_String.T;
                  begin
                     Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                           New_Name => Name); -- There is risk here of erroneous Name

                     Function_Name.Initialize (Name.To_String & "_Reply");

                     C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name.To_String) & "_reply");

                     Put_Tabs (1); Put_Line ("function " & Function_Name.To_String);
                     Put_Tabs (2); Put_Line ("(");
                     Put_Tabs (2); Put_Line (" C : Connection_Access_Type;");
                     Put_Tabs (2); Put_Line (" Cookie : " & Reply_Type_Name.To_String & ";");
                     Put_Tabs (2); Put_Line (" Error  : System.Address");
                     Put_Tabs (2); Put_Line (") return " & Request_Reply_Access_Type_Name.To_String & ";");
                     Put_Tabs (1); Put_Line ("pragma Import (C, " &  Function_Name.To_String & ", """ & C_Function_Name.To_String & """);");
                     Put_Line ("");
                  end;

                  Generate_Checked_Or_Unchecked_Function ("_Unchecked");
               else
                  Reply_Type_Name.Initialize ("Void_Cookie_Type");

                  Generate_Checked_Or_Unchecked_Function ("_Checked");
               end if;

               declare
                  Name            : X_Proto.Large_Bounded_String.T;
                  C_Function_Name : X_Proto.Large_Bounded_String.T;
                  Function_Name   : X_Proto.Large_Bounded_String.T;
               begin
                  Generate_Struct_Name (Old_Name => Request.Name.Value.To_String,
                                        New_Name => Name); -- There is risk here of erroneous Name

                  Function_Name.Initialize (Name.To_String);

                  C_Function_Name.Initialize ("xcb_" & Strings_Edit.UTF8.Mapping.To_Lowercase (Name.To_String));

                  Generate_Request_With_Reply_Code (Function_Name,
                                                    C_Function_Name,
                                                    Request.Children.all,
                                                    Request.Name.Value.To_String,
                                                    Reply_Type_Name);
               end;
            end;
         else
            Ada.Text_IO.Put_Line (GNAT.Source_Info.Source_Location & "Type definition exists without new name!?");
         end if;
      end loop;

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
