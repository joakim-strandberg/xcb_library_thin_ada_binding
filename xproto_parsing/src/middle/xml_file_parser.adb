with Ada.Text_IO;
with Aida.XML.Parse_XML_File;
with Aida.Containers.Bounded_Hash_Map;
with GNAT.Source_Info;
with Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Current_Tag;

package body XML_File_Parser is

   use Aida.String;
   use X_Proto_XML.Struct.Fs.Member_Kind_Id;
   use X_Proto_XML.Operation;
   use Aida.XML.Error_Message_P;
   use Aida.XML.DL;
   use Aida.XML.Bounded_String;
   use X_Proto_XML.Large_Bounded_String;
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
   use X_Proto_XML.X_Id;
   use X_Proto_XML.Type_Definition;
   use X_Proto_XML.Pad;
   use X_Proto_XML.Item;
   use X_Proto_XML.See;
   use X_Proto_XML.Value_Param;
   use X_Proto_XML.Type_P;
   use X_Proto_XML.Example;
   use X_Proto_XML.Field_Reference;
   use Main_Allocator_Interface;
   use Current_Tag.Fs.Tag_Id;

   use type Current_Tag.Ptr;

   Tag_Xcb                                    : constant String := "xcb";
   Tag_Xcb_Attribute_Header                   : constant String := "header";
   Tag_Struct                                 : constant String := "struct";
   Tag_Struct_Attribute_Name                  : constant String := "name";
   Tag_Field                                  : constant String := "field";
   Tag_Field_Attribute_Kind                   : constant String := "type";
   Tag_Field_Attribute_Name                   : constant String := "name";
   Tag_Field_Attribute_Enum                   : constant String := "enum";
   Tag_Field_Attribute_Mask                   : constant String := "mask";
   Tag_Field_Attribute_Alt_Enum               : constant String := "altenum";
   Tag_X_Id_Kind                              : constant String := "xidtype";
   Tag_X_Id_Kind_Attribute_Name               : constant String := "name";
   Tag_X_Id_Union                             : constant String := "xidunion";
   Tag_X_Id_Union_Attribute_Name              : constant String := "name";
   Tag_Kind                                   : constant String := "type";
   Tag_Type_Definition                        : constant String := "typedef";
   Tag_Type_Definition_Attribute_Old_Name     : constant String := "oldname";
   Tag_Type_Definition_Attribute_New_Name     : constant String := "newname";
   Tag_Pad                                    : constant String := "pad";
   Tag_Pad_Attribute_Bytes                    : constant String := "bytes";
   Tag_Enum                                   : constant String := "enum";
   Tag_Enum_Attribute_Name                    : constant String := "name";
   Tag_Item                                   : constant String := "item";
   Tag_Item_Attribute_Name                    : constant String := "name";
   XML_Tag_Value                              : constant String := "value";
   Tag_Bit                                    : constant String := "bit";
   Tag_List                                   : constant String := "list";
   Tag_List_Attribute_Kind                    : constant String := "type";
   Tag_List_Attribute_Name                    : constant String := "name";
   Tag_Field_Reference                        : constant String := "fieldref";
   XML_Tag_Operation                          : constant String := "op";
   Tag_Operation_Attribute_Op                 : constant String := "op";
   XML_Tag_Event                              : constant String := "event";
   XML_Tag_Event_Attribute_Name               : constant String := "name";
   XML_Tag_Event_Attribute_Number             : constant String := "number";
   XML_Tag_Event_Attribute_No_Sequence_Number : constant String := "no-sequence-number";
   XML_Tag_Event_Attribute_XGE                : constant String := "xge";
   XML_Tag_Doc                                : constant String := "doc";
   XML_Tag_Brief                              : constant String := "brief";
   XML_Tag_Description                        : constant String := "description";
   XML_Tag_See                                : constant String := "see";
   XML_Tag_See_Attribute_Kind                 : constant String := "type";
   XML_Tag_See_Attribute_Name                 : constant String := "name";
   XML_Tag_Event_Copy                         : constant String := "eventcopy";
   XML_Tag_Event_Copy_Attribute_Name          : constant String := "name";
   XML_Tag_Event_Copy_Attribute_Number        : constant String := "number";
   XML_Tag_Event_Copy_Attribute_Ref           : constant String := "ref";
   XML_Tag_Union                              : constant String := "union";
   XML_Tag_Union_Attribute_Name               : constant String := "name";
   XML_Tag_Error                              : constant String := "error";
   XML_Tag_Error_Attribute_Name               : constant String := "name";
   XML_Tag_Error_Attribute_Number             : constant String := "number";
   XML_Tag_Error_Attribute_Kind               : constant String := "type";
   XML_Tag_Error_Copy                         : constant String := "errorcopy";
   XML_Tag_Error_Copy_Attribute_Name          : constant String := "name";
   XML_Tag_Error_Copy_Attribute_Number        : constant String := "number";
   XML_Tag_Error_Copy_Attribute_Ref           : constant String := "ref";
   XML_Tag_Request                            : constant String := "request";
   XML_Tag_Request_Attribute_Name             : constant String := "name";
   XML_Tag_Request_Attribute_Op_Code          : constant String := "opcode";
   XML_Tag_Request_Attribute_Combine_Adjacent : constant String := "combine-adjacent";
   XML_Tag_Value_Param                        : constant String := "valueparam";
   XML_Tag_Value_Param_Attribute_Mask_Kind    : constant String := "value-mask-type";
   XML_Tag_Value_Param_Attribute_Mask_Name    : constant String := "value-mask-name";
   XML_Tag_Value_Param_Attribute_List_Name    : constant String := "value-list-name";
   XML_Tag_Reply                              : constant String := "reply";
   XML_Tag_Example                            : constant String := "example";
   XML_Tag_Expression_Field                   : constant String := "exprfield";
   XML_Tag_Expression_Field_Attribute_Kind    : constant String := "type";
   XML_Tag_Expression_Field_Attribute_Name    : constant String := "name";

   function Hash (Parent_And_Self_Tags : Aida.XML.DL.T) return Aida.Hash32_T is
      R : Aida.Hash32_T := 0;

      use type Aida.Hash32_T;
   begin
      if Is_Empty (Parent_And_Self_Tags) then
         return 0;
      end if;

      for I in Aida.XML.DL.Index_T range 1..Last_Index (Parent_And_Self_Tags) loop
         R := R + Aida.XML.Bounded_String.Hash32 (Const_Ref (Parent_And_Self_Tags, I).all);
      end loop;
      return R;
   end Hash;

   package List_Of_Tag_Names_To_Current_Tag_Maps is new Aida.Containers.Bounded_Hash_Map (Key_T             => Aida.XML.DL.T,
                                                                                          Element_T         => Current_Tag.Ptr,
                                                                                          Hash              => Hash,
                                                                                          Equivalent_Keys   => Aida.XML.DL."=",
                                                                                          Max_Hash_Map_Size => 1001,
                                                                                          Max_Collisions    => 5);

   Parents_Including_Self_To_Current_Tag_Map : List_Of_Tag_Names_To_Current_Tag_Maps.T;

   use List_Of_Tag_Names_To_Current_Tag_Maps;

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    A             : in Main_Allocator_Interface.T'Class;
                    Error_Message : out Aida.XML.Error_Message_T;
                    Is_Success    : out Boolean)
   is
      procedure Populate_Parents_Including_Self (Parents_Including_Self : in out Aida.XML.DL.T;
                                                 Parents                : Aida.XML.DL.T;
                                                 Tag_Name               : String)
      is
         TN : Aida.XML.Bounded_String_T;
      begin
         for I in Aida.XML.DL.Index_T range 1..Last_Index (Parents) loop
            Append (This     => Parents_Including_Self,
                    New_Item => Const_Ref (Parents, I).all);
         end loop;
         Initialize (This => TN,
                     Text => Tag_Name);
         Append (This     => Parents_Including_Self,
                 New_Item => TN);
      end Populate_Parents_Including_Self;

      function To_String (Tags : Aida.XML.DL.T) return String is
         R : X_Proto_XML.Large_Bounded_String.T;
      begin
         for I in Aida.XML.DL.Index_T range 1..Last_Index (Tags) loop
            Append (R, (To_String (Element (Tags, I)) & ", "));
         end loop;

         return To_String (R);
      end To_String;

      function Find_Tag (Key : Aida.XML.DL.T) return Current_Tag.Ptr is
         R : constant List_Of_Tag_Names_To_Current_Tag_Maps.Find_Element_Result_T :=
           Find_Element (Parents_Including_Self_To_Current_Tag_Map, Key);
      begin
         if R.Exists then
            return R.Element;
         else
            return null;
         end if;
      end Find_Tag;

      procedure Start_Tag (Tag_Name      : String;
                           Parent_Tags   : Aida.XML.DL.T;
                           Error_Message : out Aida.XML.Error_Message_T;
                           Is_Success    : out Boolean)
      is
         Parents_Including_Self : Aida.XML.DL.T;

         procedure Insert (CT : Current_Tag.Ptr) is
         begin
            Insert (This        => Parents_Including_Self_To_Current_Tag_Map,
                    Key         => Parents_Including_Self,
                    New_Element => CT);
         end Insert;

         Prev_Tag : constant Current_Tag.Ptr := Find_Tag (Parent_Tags);
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         if Prev_Tag = null then
            if Tag_Name = Tag_Xcb then
               if Xcb_V /= null then
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected Xcb access to be null, tag name is " & Tag_Name);
                  return;
               end if;

               Xcb_V := New_Xcb (A);
               Insert (This        => Parents_Including_Self_To_Current_Tag_Map,
                       Key         => Parents_Including_Self,
                       New_Element => New_Current_Tag (A, Find_Tag (Parent_Tags), Xcb_V));
               Is_Success := True;
            else
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Xcb & ", but found " & Tag_Name);
            end if;
            return;
         end if;

         case Prev_Tag.Kind_Id is
            when Xcb =>
               if Tag_Name = Tag_Struct then
                  declare
                     Struct_V : constant X_Proto_XML.Struct.Ptr := New_Struct (A);
                  begin
                     case Prev_Tag.Kind_Id is
                        when Xcb =>
                           Append_Struct (Prev_Tag.Xcb_V.all, Struct_V);
                           Insert (New_Current_Tag (This       => A,
                                                    Parent_Tag => Prev_Tag,
                                                    Struct     => Struct_V));
                           Is_Success := True;
                        when others =>
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Struct & ", but found " & Tag_Name);
                     end case;
                  end;
               elsif Tag_Name = Tag_X_Id_Kind then
                  declare
                     X_Id_Type_V : constant X_Proto_XML.X_Id.Ptr := New_X_Id (A);
                  begin
                     Append_X_Id (Prev_Tag.Xcb_V.all, X_Id_Type_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This        => A,
                                              Parent_Tag  => Prev_Tag,
                                              X_Id        => X_Id_Type_V));
                  end;
               elsif Tag_Name = Tag_X_Id_Union then
                  declare
                     X_Id_Union_V : constant X_Proto_XML.X_Id_Union.Ptr := New_X_Id_Union (A);
                  begin
                     Append_X_Id_Union (Prev_Tag.Xcb_V.all, X_Id_Union_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              X_Id_Union => X_Id_Union_V));
                  end;
               elsif Tag_Name = Tag_Type_Definition then
                  declare
                     TD : constant X_Proto_XML.Type_Definition.Ptr := New_Type_Definition (A);
                  begin
                     Append_Type_Definition (Prev_Tag.Xcb_V.all, TD);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag       => Prev_Tag,
                                              Type_Definition  => TD));
                  end;
               elsif Tag_Name = Tag_Enum then
                  declare
                     Enum_V : constant X_Proto_XML.Enum.Ptr := New_Enum (A);
                  begin
                     Append_Enum (Prev_Tag.Xcb_V.all, Enum_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Enum       => Enum_V));
                  end;
               elsif Tag_Name = XML_Tag_Event then
                  declare
                     E : constant X_Proto_XML.Event.Ptr := New_Event (A);
                  begin
                     Append_Event (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Event      => E));
                  end;
               elsif Tag_Name = XML_Tag_Event_Copy then
                  declare
                     EC : constant X_Proto_XML.Event_Copy.Ptr := New_Event_Copy (A);
                  begin
                     Append_Event_Copy (Prev_Tag.Xcb_V.all, EC);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Event_Copy => EC));
                  end;
               elsif Tag_Name = XML_Tag_Union then
                  declare
                     U : constant X_Proto_XML.Union.Ptr := New_Union (A);
                  begin
                     Append_Union (Prev_Tag.Xcb_V.all, U);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Union      => U));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     E : constant X_Proto_XML.Error.Ptr := New_Error (A);
                  begin
                     Append_Error (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Error      => E));
                  end;
               elsif Tag_Name = XML_Tag_Error_Copy then
                  declare
                     E : constant X_Proto_XML.Error_Copy.Ptr := New_Error_Copy (A);
                  begin
                     Append_Error_Copy (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Error_Copy => E));
                  end;
               elsif Tag_Name = XML_Tag_Request then
                  declare
                     R : constant X_Proto_XML.Request.Ptr := New_Request (A);
                  begin
                     Append_Request (Prev_Tag.Xcb_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Request    => R));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Struct =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_Member (A, Field_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_Member (A, Pad_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_Member (A, List_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              List       => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when X_Id_Union =>
               if Tag_Name = Tag_Kind then
                  declare
                     Kind : constant X_Proto_XML.Type_P.Ptr := New_Type (A);
                  begin
                     Append_Kind (Prev_Tag.X_Id_Union_V.all, Kind);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Kind       => Kind));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Enum =>
               if Tag_Name = Tag_Item then
                  declare
                     Item_V : constant X_Proto_XML.Item.Ptr := New_Item (A);
                  begin
                     Append_Item (Prev_Tag.Enum_V.all, Item_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Item       => Item_V));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : constant X_Proto_XML.Documentation.Ptr := New_Documentation (A);
                  begin
                     Append_Documentation (Prev_Tag.Enum_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag    => Prev_Tag,
                                              Documentation => D));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Item =>
               if Tag_Name = XML_Tag_Value then
                  Is_Success := True;
               elsif Tag_Name = Tag_Bit then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when List =>
               if Tag_Name = Tag_Field_Reference then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Operation then
                  declare
                     Operation : X_Proto_XML.List.Fs.Member_Ptr := New_List_Member (A, X_Proto_XML.List.Fs.List_Member_Kind_Operation);
                  begin
                     Append_Member (Prev_Tag.List_V.all, Operation);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Operation  => Operation.Operation'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.List.Fs.Member_Ptr := New_List_Member (A, X_Proto_XML.List.Fs.List_Member_Kind_Value);
                  begin
                     Append_Member (Prev_Tag.List_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Value      => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Op =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     V : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Member (A, X_Proto_XML.Operation.Fs.Member_Operation);
                  begin
                     V.Operation := New_Operation (A);
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Operation  => V.Operation));
                  end;
               elsif Tag_Name = Tag_Field_Reference then
                  declare
                     V : X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Member (A, X_Proto_XML.Operation.Fs.Member_Kind_Field_Reference);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag      => Prev_Tag,
                                              Field_Reference => V.Field_Reference'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Member (A, X_Proto_XML.Operation.Fs.Member_Kind_Value);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Value      => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Event =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Member (A, X_Proto_XML.Event.Fs.Event_Member_Field);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Member (A, X_Proto_XML.Event.Fs.Event_Member_Pad);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Member (A, X_Proto_XML.Event.Fs.Event_Member_Doc);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag    => Prev_Tag,
                                              Documentation => D.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Member (A, X_Proto_XML.Event.Fs.Event_Member_List);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              List       => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Documentation =>
               if Tag_Name = Tag_Field then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Member (A, X_Proto_XML.Documentation.Fs.Member_Field);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => D.F'Access));
                  end;
               elsif Tag_Name = XML_Tag_See then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Member (A, X_Proto_XML.Documentation.Fs.Member_See);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              See        => D.S'Access));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Member (A, X_Proto_XML.Documentation.Fs.Member_Error);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Error      => D.E'Access));
                  end;
               elsif Tag_Name = XML_Tag_Example then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Member (A, X_Proto_XML.Documentation.Fs.Member_Example);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Example    => D.Ex'Access));
                  end;
               elsif Tag_Name = XML_Tag_Brief then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Description then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Union =>
               if Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Union.Fs.Child_Ptr := New_Union_Child (A, X_Proto_XML.Union.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Union_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              List       => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Error =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Error.Fs.Child_Ptr := New_Error_Child (A, X_Proto_XML.Error.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Error.Fs.Child_Ptr := New_Error_Child (A, X_Proto_XML.Error.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Request =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value_Param then
                  declare
                     V : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Value_Param);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag  => Prev_Tag,
                                              Value_Param => V.V'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     V : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Documentation);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag    => Prev_Tag,
                                              Documentation => V.D'Access));
                  end;
               elsif Tag_Name = XML_Tag_Reply then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Reply);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Reply      => R.R'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              List       => R.L'Access));
                  end;
               elsif Tag_Name = XML_Tag_Expression_Field then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Child (A, X_Proto_XML.Request.Fs.Child_Expression_Field);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag       => Prev_Tag,
                                              Expression_Field => R.EF'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Reply =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Child (A, X_Proto_XML.Reply.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Child (A, X_Proto_XML.Reply.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Pad        => F.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Child (A, X_Proto_XML.Reply.Fs.Child_Documentation);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag    => Prev_Tag,
                                              Documentation => F.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Child (A, X_Proto_XML.Reply.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              List       => F.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Expression_Field =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     C : X_Proto_XML.Expression_Field.Fs.Child_Ptr := New_Expression_Field_Child (A, X_Proto_XML.Expression_Field.Fs.Child_Operation);
                  begin
                     Append_Child (Prev_Tag.Expression_Field_V.all, C);
                     Is_Success := True;
                     Insert (New_Current_Tag (This       => A,
                                              Parent_Tag => Prev_Tag,
                                              Operation  => C.Op'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Field |
                 X_Id_Kind |
                 Kind |
                 Type_Definition |
                 Pad |
                 Value |
                 Bit |
                 Field_Reference |
                 See |
                 Event_Copy |
                 Error_Copy |
                 Value_Param |
                 Example =>
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Error, tag name is " & Tag_Name);
         end case;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : Aida.XML.DL.T;
                           Error_Message               : out Aida.XML.Error_Message_T;
                           Is_Success                  : out Boolean)
      is
         CT : constant Current_Tag.Ptr := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if CT = null then
            Is_Success := False;
            Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (
                        Parent_Tags_And_Current_Tag));
            return;
         end if;

         Is_Success := True;
         case CT.Kind_Id is
            when Xcb =>
               if Attribute_Name = Tag_Xcb_Attribute_Header then
                  Is_Success := True;
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Header (CT.Xcb_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Struct =>
               if Attribute_Name = Tag_Struct_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Struct_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Field =>
               if Attribute_Name = Tag_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (CT.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Enum (CT.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Mask then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask (CT.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Alt_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Alt_Enum (CT.Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when X_Id_Kind =>
               if Attribute_Name = Tag_X_Id_Kind_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.X_Id_Kind_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when X_Id_Union =>
               if Attribute_Name = Tag_X_Id_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.X_Id_Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Type_Definition =>
               if Attribute_Name = Tag_Type_Definition_Attribute_Old_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Old_Name (CT.Type_Definition_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Type_Definition_Attribute_New_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_New_Name (CT.Type_Definition_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Pad =>
               if Attribute_Name = Tag_Pad_Attribute_Bytes then
                  declare
                     V : Aida.Int32_T;

                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => V,
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Bytes (CT.Pad_V.all, Positive (V));
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Enum =>
               if Attribute_Name = Tag_Enum_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Enum_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Item =>
               if Attribute_Name = Tag_Item_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Item_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when List =>
               if Attribute_Name = Tag_List_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (CT.List_V.all, V);
                  end;
               elsif Attribute_Name = Tag_List_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.List_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Op =>
               if Attribute_Name = Tag_Operation_Attribute_Op then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Op (CT.Op_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Event =>
               if Attribute_Name = XML_Tag_Event_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Event_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_Number then
                  declare
                     V : Aida.Int32_T;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => V,
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Number (CT.Event_V.all, Positive (V));
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_No_Sequence_Number then
                  if Attribute_Value = "true" then
                     Set_No_Sequence_Number (CT.Event_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_No_Sequence_Number (CT.Event_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Event_Attribute_XGE then
                  if Attribute_Value = "true" then
                     Set_XGE (CT.Event_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_XGE (CT.Event_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when See =>
               if Attribute_Name = XML_Tag_See_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.See_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_See_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (CT.See_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Event_Copy =>
               if Attribute_Name = XML_Tag_Event_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Event_Copy_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Ref then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Ref (CT.Event_Copy_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Number then
                  declare
                     V : Positive;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Number (CT.Event_Copy_V.all, V);
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Union =>
               if Attribute_Name = XML_Tag_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Error =>
               if Attribute_Name = XML_Tag_Error_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Error_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Number then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Number (CT.Error_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (CT.Error_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Error_Copy =>
               if Attribute_Name = XML_Tag_Error_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Error_Copy_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Number then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Number (CT.Error_Copy_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Ref then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Ref (CT.Error_Copy_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Request =>
               if Attribute_Name = XML_Tag_Request_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Request_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Op_Code then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Attribute_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Op_Code (CT.Request_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Combine_Adjacent then
                  if Attribute_Value = "true" then
                     Set_Shall_Combine_Adjacent (CT.Request_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_Shall_Combine_Adjacent (CT.Request_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Value_Param =>
               if Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Kind (CT.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Name (CT.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_List_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_List_Name (CT.Value_Param_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Expression_Field =>
               if Attribute_Name = XML_Tag_Expression_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (CT.Expression_Field_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Expression_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (CT.Expression_Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Kind |
                 Value |
                 Bit |
                 Field_Reference |
                 Documentation |
                 Reply |
                 Example =>
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
         end case;
      end Attribute;

      procedure End_Tag (Tag_Name      : String;
                         Parent_Tags   : Aida.XML.DL.T;
                         Error_Message : out Aida.XML.Error_Message_T;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : Aida.XML.DL.T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self => Parents_Including_Self,
                                          Parents                => Parent_Tags,
                                          Tag_Name               => Tag_Name);

         begin
            Delete (Parents_Including_Self_To_Current_Tag_Map, Parents_Including_Self);
         exception
            when Unknown_Exception : others =>
               Ada.Text_IO.Put_Line ("delete 1");
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & Ada.Exceptions.Exception_Information(Unknown_Exception));
               return;
         end;

         Is_Success := True;
      end End_Tag;

      procedure End_Tag (Tag_Name      : String;
                         Tag_Value     : String;
                         Parent_Tags   : Aida.XML.DL.T;
                         Error_Message : out Aida.XML.Error_Message_T;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : Aida.XML.DL.T;

         CT : Current_Tag.Ptr;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         CT := Find_Tag (Parents_Including_Self);

         Is_Success := True;

         if CT = null then
            declare
               Prev_Tag : constant Current_Tag.Ptr := Find_Tag (Parent_Tags);
            begin
               if Prev_Tag = null then
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Both Current_Tag and Prev_Tag was null for end tag '" & Tag_Name & "' and value '" & Tag_Value & "'");
                  return;
               end if;

               case Prev_Tag.Kind_Id is
                  when Item =>
                     declare
                        V : X_Proto_XML.Value_Type;
                        Has_Failed : Boolean;
                     begin
                        To_Int32 (Source     => Aida.String_T (Tag_Value),
                                  Target     => Aida.Int32_T (V),
                                  Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           case Kind_Id (Prev_Tag.Item_V.all) is
                              when X_Proto_XML.Item.Fs.Not_Specified =>
                                 if Tag_Name = XML_Tag_Value then
                                    Set_Kind_Id (Prev_Tag.Item_V.all, X_Proto_XML.Item.Fs.Specified_As_Value);
                                    Set_Value (Prev_Tag.Item_V.all, V);
                                 elsif Tag_Name = Tag_Bit then
                                    Set_Kind_Id (Prev_Tag.Item_V.all, X_Proto_XML.Item.Fs.Specified_As_Bit);
                                    Set_Bit (Prev_Tag.Item_V.all, X_Proto_XML.Item.Fs.Bit_Type (V));
                                 else
                                    Is_Success := False;
                                    Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", unknown end tag '" & Tag_Name & "'");
                                 end if;
                              when X_Proto_XML.Item.Fs.Specified_As_Value |
                                   X_Proto_XML.Item.Fs.Specified_As_Bit =>
                                 Is_Success := False;
                                 Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", value already initialized for end tag " & Tag_Name & ", kind id " & Kind_Id (Prev_Tag.Item_V.all)'Img)
;
                           end case;
                        end if;
                     end;
                  when List =>
                     if Tag_Name = Tag_Field_Reference then
                        declare
                           L : constant X_Proto_XML.List.Fs.Member_Ptr := New_List_Member (A, X_Proto_XML.List.Fs.List_Member_Kind_Field_Reference);
                        begin
                           Initialize (L.Field_Reference, Tag_Value);
                           Append_Member (Prev_Tag.List_V.all, L);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when Documentation =>
                     if Tag_Name = XML_Tag_Brief then
                        declare
                           V : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Initialize (V, Tag_Value);
                           Set_Brief_Description (Prev_Tag.Documentation_V.all, V);
                        end;
                     elsif Tag_Name = XML_Tag_Description then
                        declare
                           V : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Initialize (V, Tag_Value);
                           Set_Description (Prev_Tag.Documentation_V.all, V);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when others =>
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "'");
               end case;
            end;
         else
            begin
               Delete (Parents_Including_Self_To_Current_Tag_Map, Parents_Including_Self);
            exception
               when Unknown_Exception : others =>
                  Ada.Text_IO.Put_Line ("delete 2");
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & Ada.Exceptions.Exception_Information(Unknown_Exception));
                  return;
            end;

            case CT.Kind_Id is
               when Kind =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (CT.Kind.all, V);
                  end;
               when Field =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (CT.Field_V.all, V);
                  end;
               when Value =>
                  declare
                     V : X_Proto_XML.Value_Type;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Tag_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                     else
                        CT.Value_V.all := V;
                     end if;
                  end;
               when Error =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (CT.Error_V.all, V);
                  end;
               when Example =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (CT.Example_V.all, V);
                  end;
               when Op =>
                  if Tag_Name = Tag_Field_Reference then
                     declare
                        V : X_Proto_XML.Field_Reference_Type;
                        FR : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Member (A, X_Proto_XML.Operation.Fs.Member_Kind_Field_Reference);
                     begin
                        Initialize (V, Tag_Value);
                        FR.Field_Reference := V;
                        Append_Member (CT.Op_V.all, FR);
                     end;
                  elsif Tag_Name = XML_Tag_Value then
                     declare
                        V : X_Proto_XML.Value_Type;
                        Has_Failed : Boolean;
                     begin
                        To_Int32 (Source     => Aida.String_T (Tag_Value),
                                  Target     => Aida.Int32_T (V),
                                  Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           declare
                              Operation_Value : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Member (A, X_Proto_XML.Operation.Fs.Member_Kind_Value);
                           begin
                              Operation_Value.Value := V;
                              Append_Member (CT.Op_V.all, Operation_Value);
                           end;
                        end if;
                     end;
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & CT.Kind_Id'Img);
                  end if;
               when Field_Reference =>
                  declare
                     V : X_Proto_XML.Field_Reference_Type;
                  begin
                     Initialize (V, Tag_Value);
                     CT.Field_Reference.all := V;
                  end;
               when Xcb |
                    Struct |
                    Bit |
                    X_Id_Kind |
                    X_Id_Union |
                    Type_Definition |
                    Pad |
                    Enum |
                    Item |
                    List |
                    Event |
                    Documentation |
                    See |
                    Event_Copy |
                    Union |
                    Error_Copy |
                    Request |
                    Value_Param |
                    Reply |
                    Expression_Field =>
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", but found unexpected end tag " & Tag_Name);
            end case;
         end if;
      end End_Tag;

      procedure Parse_X_Proto_XML_File is new Aida.XML.Parse_XML_File (Start_Tag,
                                                                       Attribute,
                                                                       End_Tag,
                                                                       End_Tag);
   begin
      Parse_X_Proto_XML_File (Contents,
                              Error_Message,
                              Is_Success);
   end Parse;

end XML_File_Parser;
