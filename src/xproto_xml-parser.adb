with Aida.XML.Generic_Parse_XML_File;
--  with Aida.Text_IO;
with XProto_XML.Current_Ids;

package body XProto_XML.Parser is

   use all type Aida.XML.Procedure_Call_Result.T;
   use all type Aida.String_T;

   use all type Current_Ids.Xcb_Id_Vector.T;

   use all type Storage.Header_Comment_T;

   Tag_Xcb                                    : constant Aida.String_T := "xcb";
--     Tag_Xcb_Attribute_Header                   : constant Aida.String_T := "header";
--     Tag_Struct                                 : constant Aida.String_T := "struct";
--     Tag_Struct_Attribute_Name                  : constant Aida.String_T := "name";
--     Tag_Field                                  : constant Aida.String_T := "field";
--     Tag_Field_Attribute_Kind                   : constant Aida.String_T := "type";
--     Tag_Field_Attribute_Name                   : constant Aida.String_T := "name";
--     Tag_Field_Attribute_Enum                   : constant Aida.String_T := "enum";
--     Tag_Field_Attribute_Mask                   : constant Aida.String_T := "mask";
--     Tag_Field_Attribute_Alt_Enum               : constant Aida.String_T := "altenum";
--     Tag_X_Id_Kind                              : constant Aida.String_T := "xidtype";
--     Tag_X_Id_Kind_Attribute_Name               : constant Aida.String_T := "name";
--     Tag_X_Id_Union                             : constant Aida.String_T := "xidunion";
--     Tag_X_Id_Union_Attribute_Name              : constant Aida.String_T := "name";
--     Tag_Kind                                   : constant Aida.String_T := "type";
--     Tag_T_Definition                           : constant Aida.String_T := "typedef";
--     Tag_T_Definition_Attribute_Old_Name        : constant Aida.String_T := "oldname";
--     Tag_T_Definition_Attribute_New_Name        : constant Aida.String_T := "newname";
--     Tag_Pad                                    : constant Aida.String_T := "pad";
--     Tag_Pad_Attribute_Bytes                    : constant Aida.String_T := "bytes";
--     Tag_Enum                                   : constant Aida.String_T := "enum";
--     Tag_Enum_Attribute_Name                    : constant Aida.String_T := "name";
--     Tag_Item                                   : constant Aida.String_T := "item";
--     Tag_Item_Attribute_Name                    : constant Aida.String_T := "name";
--     XML_Tag_Value                              : constant Aida.String_T := "value";
--     Tag_Bit                                    : constant Aida.String_T := "bit";
--     Tag_List                                   : constant Aida.String_T := "list";
--     Tag_List_Attribute_Kind                    : constant Aida.String_T := "type";
--     Tag_List_Attribute_Name                    : constant Aida.String_T := "name";
--     Tag_Field_Reference                        : constant Aida.String_T := "fieldref";
--     XML_Tag_Operation                          : constant Aida.String_T := "op";
--     Tag_Operation_Attribute_Op                 : constant Aida.String_T := "op";
--     XML_Tag_Event                              : constant Aida.String_T := "event";
--     XML_Tag_Event_Attribute_Name               : constant Aida.String_T := "name";
--     XML_Tag_Event_Attribute_Number             : constant Aida.String_T := "number";
--     XML_Tag_Event_Attribute_No_Sequence_Number : constant Aida.String_T := "no-sequence-number";
--     XML_Tag_Event_Attribute_XGE                : constant Aida.String_T := "xge";
--     XML_Tag_Doc                                : constant Aida.String_T := "doc";
--     XML_Tag_Brief                              : constant Aida.String_T := "brief";
--     XML_Tag_Description                        : constant Aida.String_T := "description";
--     XML_Tag_See                                : constant Aida.String_T := "see";
--     XML_Tag_See_Attribute_Kind                 : constant Aida.String_T := "type";
--     XML_Tag_See_Attribute_Name                 : constant Aida.String_T := "name";
--     XML_Tag_Event_Copy                         : constant Aida.String_T := "eventcopy";
--     XML_Tag_Event_Copy_Attribute_Name          : constant Aida.String_T := "name";
--     XML_Tag_Event_Copy_Attribute_Number        : constant Aida.String_T := "number";
--     XML_Tag_Event_Copy_Attribute_Ref           : constant Aida.String_T := "ref";
--     XML_Tag_Union                              : constant Aida.String_T := "union";
--     XML_Tag_Union_Attribute_Name               : constant Aida.String_T := "name";
--     XML_Tag_Error                              : constant Aida.String_T := "error";
--     XML_Tag_Error_Attribute_Name               : constant Aida.String_T := "name";
--     XML_Tag_Error_Attribute_Number             : constant Aida.String_T := "number";
--     XML_Tag_Error_Attribute_Kind               : constant Aida.String_T := "type";
--     XML_Tag_Error_Copy                         : constant Aida.String_T := "errorcopy";
--     XML_Tag_Error_Copy_Attribute_Name          : constant Aida.String_T := "name";
--     XML_Tag_Error_Copy_Attribute_Number        : constant Aida.String_T := "number";
--     XML_Tag_Error_Copy_Attribute_Ref           : constant Aida.String_T := "ref";
--     XML_Tag_Request                            : constant Aida.String_T := "request";
--     XML_Tag_Request_Attribute_Name             : constant Aida.String_T := "name";
--     XML_Tag_Request_Attribute_Op_Code          : constant Aida.String_T := "opcode";
--     XML_Tag_Request_Attribute_Combine_Adjacent : constant Aida.String_T := "combine-adjacent";
--     XML_Tag_Value_Param                        : constant Aida.String_T := "valueparam";
--     XML_Tag_Value_Param_Attribute_Mask_Kind    : constant Aida.String_T := "value-mask-type";
--     XML_Tag_Value_Param_Attribute_Mask_Name    : constant Aida.String_T := "value-mask-name";
--     XML_Tag_Value_Param_Attribute_List_Name    : constant Aida.String_T := "value-list-name";
--     XML_Tag_Reply                              : constant Aida.String_T := "reply";
--     XML_Tag_Example                            : constant Aida.String_T := "example";
--     XML_Tag_Expression_Field                   : constant Aida.String_T := "exprfield";
--     XML_Tag_Expression_Field_Attribute_Kind    : constant Aida.String_T := "type";
--     XML_Tag_Expression_Field_Attribute_Name    : constant Aida.String_T := "name";

   type State_T is (
                    Expecting_Header_Comment,
                    Expecting_Xcb_Tag,
                    End_State
                   );

   procedure Start_Tag (Storage      : in out XProto_XML.Storage.T;
                        Max_Indices  : in out XProto_XML.Max_Indices.T;
                        State        : in out State_T;
                        Current_Ids  : in out XProto_XML.Current_Ids.T;
                        Tag_Name     : Aida.String_T;
                        Call_Storage : in out Aida.XML.Procedure_Call_Result.T) with
     Global => null;

   procedure End_Tag (Storage      : in out XProto_XML.Storage.T;
                      Max_Indices  : in out XProto_XML.Max_Indices.T;
                      State        : in out State_T;
                      Current_Ids  : in out XProto_XML.Current_Ids.T;
                      Tag_Name     : Aida.String_T;
                      Call_Storage : in out Aida.XML.Procedure_Call_Result.T) with
     Global => null;

   procedure Text (Storage      : in out XProto_XML.Storage.T;
                   Max_Indices  : in out XProto_XML.Max_Indices.T;
                   State        : in out State_T;
                   Current_Ids  : in out XProto_XML.Current_Ids.T;
                   Value        : Aida.String_T;
                   Call_Storage : in out Aida.XML.Procedure_Call_Result.T) with
     Global => null;

   procedure Attribute (Storage         : in out XProto_XML.Storage.T;
                        Max_Indices     : in out XProto_XML.Max_Indices.T;
                        State           : in out State_T;
                        Current_Ids     : in out XProto_XML.Current_Ids.T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Storage    : in out Aida.XML.Procedure_Call_Result.T) with
     Global => null;

   procedure CDATA (Storage      : in out XProto_XML.Storage.T;
                    Max_Indices  : in out XProto_XML.Max_Indices.T;
                    State        : in out State_T;
                    Current_Ids  : in out XProto_XML.Current_Ids.T;
                    Value        : Aida.String_T;
                    Call_Storage : in out Aida.XML.Procedure_Call_Result.T) with
     Global => null;

   procedure Start_Tag (Storage      : in out XProto_XML.Storage.T;
                        Max_Indices  : in out XProto_XML.Max_Indices.T;
                        State        : in out State_T;
                        Current_Ids  : in out XProto_XML.Current_Ids.T;
                        Tag_Name     : Aida.String_T;
                        Call_Storage : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Storage);
   begin
      case State is
         when Expecting_Xcb_Tag =>
            if
              Tag_Name = Tag_Xcb and
              Max_Indices.Xcb_Id_Max < Extended_Xcb_Id_T'Last and
              not Is_Full (Current_Ids.Xcb_Ids)
            then
               declare
                  Xcb_Id : Xcb_Id_T;
               begin
                  Max_Indices.Allocate_Xcb_Id (Xcb_Id);
                  Append (Current_Ids.Xcb_Ids, Xcb_Id);
               end;
               State := End_State;
            else
               Initialize (Call_Storage, "53F0F33E-FFD6-4B0E-9803-3AC903131B7B");
            end if;
         when Expecting_Header_Comment |
              End_State =>
            Initialize (Call_Storage, "3F187F97-A098-4C46-8312-10E1C6170F0E");
      end case;
   end Start_Tag;

   procedure End_Tag (Storage      : in out XProto_XML.Storage.T;
                      Max_Indices  : in out XProto_XML.Max_Indices.T;
                      State        : in out State_T;
                      Current_Ids  : in out XProto_XML.Current_Ids.T;
                      Tag_Name     : Aida.String_T;
                      Call_Storage : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
   begin
--        case State is
--           when Expecting_Person_End_Tag =>
--              if Tag_Name = "person" then
--                 State := Final_State;
--              else
--                 Initialize (Call_Storage, "733D4C3B-5724-4A5C-9836-77B86BA348CC");
--              end if;
--           when Expecting_Pre_Age_Text |
--                Expecting_Pre_Age_CDATA |
--                Expecting_Age_Value |
--                Expecting_Person_Start_Tag |
--                Final_State =>
--              Initialize (Call_Storage, "E8859A95-D6F9-4923-80BC-0D3F139BBD79");
--        end case;
null;
   end End_Tag;

   procedure Text (Storage      : in out XProto_XML.Storage.T;
                   Max_Indices  : in out XProto_XML.Max_Indices.T;
                   State        : in out State_T;
                   Current_Ids  : in out XProto_XML.Current_Ids.T;
                   Value        : Aida.String_T;
                   Call_Storage : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
   begin
--        case State is
--           when Expecting_Age_Value  =>
--              if Person_Id_Vector.Is_Empty (Current_Ids.Person_Ids) then
--                 Initialize (Call_Storage, "F434F73C-825A-4B4E-A2EF-DC2A52DCA28D");
--              else
--                 declare
--                    Person_Id : Aida.Json_Parsing_Tests_Model.Person_Id_T renames
--                      Person_Id_Vector.Last_Element (Current_Ids.Person_Ids);
--                 begin
--                    if Value'Length > Json_Parsing_Tests_Model.Person_Def.NAME_MAX then
--                       Initialize (Call_Storage, "A8DFF685-A883-4149-819D-9C0E039D38C3");
--                    else
--                       Initialize (Storage.Person (Person_Id).Name,
--                                   Value);
--                    end if;
--                 end;
--                 State := Expecting_Person_End_Tag;
--              end if;
--           when Expecting_Pre_Age_Text =>
--              if Value = "" then
--                 State := Expecting_Pre_Age_CDATA;
--              else
--                 Initialize (Call_Storage, "F98DD5FC-6225-4435-A285-850754C909DC");
--              end if;
--           when Expecting_Pre_Age_CDATA |
--                Expecting_Person_Start_Tag |
--                Expecting_Person_End_Tag |
--                Final_State =>
--              Initialize (Call_Storage, "AF37FE00-80CF-4A62-BF4A-E467E2145739");
--        end case;
null;
   end Text;

   procedure Attribute (Storage         : in out XProto_XML.Storage.T;
                        Max_Indices     : in out XProto_XML.Max_Indices.T;
                        State           : in out State_T;
                        Current_Ids     : in out XProto_XML.Current_Ids.T;
                        Attribute_Name  : Aida.String_T;
                        Attribute_Value : Aida.String_T;
                        Call_Storage    : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Storage);
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
      pragma Unreferenced (Attribute_Name);
      pragma Unreferenced (Attribute_Value);
   begin
      case State is
         when Expecting_Header_Comment |
              Expecting_Xcb_Tag |
              End_State =>
            Initialize (Call_Storage, "46059C70-1275-4D1E-BB61-EC6B8B87CEC5");
      end case;
   end Attribute;

   procedure Comment (Storage      : in out XProto_XML.Storage.T;
                      Max_Indices  : in out XProto_XML.Max_Indices.T;
                      State        : in out State_T;
                      Current_Ids  : in out XProto_XML.Current_Ids.T;
                      Value        : Aida.String_T;
                      Call_Storage : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
   begin
--        Aida.Text_IO.Put_Line (Aida.String_T (Value'Length'Img));
      case State is
         when Expecting_Header_Comment =>
            if Value'Length <= Storage.Header_Comment.Maximum_Length then
               Initialize (Storage.Header_Comment, Value);
               State := Expecting_Xcb_Tag;
            else
               Initialize (Call_Storage, "AECEF7CF-DC36-437D-927F-8DDCBCB582C1");
            end if;
         when Expecting_Xcb_Tag |
              End_State =>
            Initialize (Call_Storage, "DA8DFC69-0308-47D8-BC48-9757FAB1AB5B");
      end case;
   end Comment;

   procedure CDATA (Storage      : in out XProto_XML.Storage.T;
                    Max_Indices  : in out XProto_XML.Max_Indices.T;
                    State        : in out State_T;
                    Current_Ids  : in out XProto_XML.Current_Ids.T;
                    Value        : Aida.String_T;
                    Call_Storage : in out Aida.XML.Procedure_Call_Result.T)
   is
      pragma Unreferenced (Max_Indices);
      pragma Unreferenced (Current_Ids);
   begin
--        case State is
--           when Expecting_Pre_Age_CDATA =>
--              if Storage.Header_Comment'Length > Value'Length then
--                 Storage.Header_Comment (1..Value'Length) := Value (Value'Range);
--                 State := Expecting_Age_Value;
--              else
--                 Initialize (Call_Storage, "D087DA52-AFEB-48F9-B767-87791980B20D");
--              end if;
--           when Expecting_Pre_Age_Text |
--                Expecting_Age_Value |
--                Expecting_Person_End_Tag |
--                Expecting_Person_Start_Tag |
--                Final_State =>
--              Initialize (Call_Storage, "EC0B3613-D72C-42F4-A55D-4667C3B0D378");
--        end case;
null;
   end CDATA;

   procedure Parse_XML is new Aida.XML.Generic_Parse_XML_File (Storage.T,
                                                               Max_Indices.T,
                                                               State_T,
                                                               XProto_XML.Current_Ids.T,
                                                               Start_Tag,
                                                               End_Tag,
                                                               Text,
                                                               Attribute,
                                                               Comment,
                                                               CDATA);

   procedure Parse (Storage     : in out XProto_XML.Storage.T;
                    Max_Indices : in out XProto_XML.Max_Indices.T;
                    Contents    : in Aida.String_T;
                    Call_Result : in out Aida.XML.Procedure_Call_Result.T)
   is
      State : State_T := State_T'First;
      Current_Ids : XProto_XML.Current_Ids.T;
   begin
      Parse_XML (Storage,
                 Max_Indices,
                 State,
                 Current_Ids,
                 Contents,
                 Call_Result);
   end Parse;

end XProto_XML.Parser;
