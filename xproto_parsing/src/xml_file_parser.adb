with Ada.Text_IO;
with Aida.XML.Parse_XML_File;
with Aida.Containers.Bounded_Hash_Map;
with Ada.Exceptions;

package body XML_File_Parser is

   use Aida.String;
   use Aida.XML.Error_Message_P;
   use Aida.XML.DL;
   use Aida.XML.Bounded_String;
   use X_Proto_XML.Struct.Member_Kind_Id;
   use X_Proto_XML.Operation.Member_Vector;
   use X_Proto_XML.Large_Bounded_String;
   use X_Proto_XML.Xcb.Struct_Vector;
   use X_Proto_XML.Xcb.X_Id_Vector;
   use X_Proto_XML.Xcb.X_Id_Union_Vector;
   use X_Proto_XML.Xcb.Type_Definition_Vector;
   use X_Proto_XML.Xcb.Enum_Vector;
   use X_Proto_XML.Xcb.Event_Vector;
   use X_Proto_XML.Xcb.Event_Copy_Vector;
   use X_Proto_XML.Xcb.Union_Vector;
   use X_Proto_XML.Xcb.Error_Vector;
   use X_Proto_XML.Xcb.Error_Copy_Vector;
   use X_Proto_XML.Xcb.Request_Vector;
   use X_Proto_XML.Struct.Member_Vector;
   use X_Proto_XML.Union.Child_Vector;
   use X_Proto_XML.Enum.Item_Vector;
   use X_Proto_XML.Enum.Documentation_Vector;
   use X_Proto_XML.X_Id_Union.Type_Vector;
   use X_Proto_XML.List.Member_Vector;
   use X_Proto_XML.Event.Member_Vector;
   use X_Proto_XML.Documentation.Member_Vector;
   use X_Proto_XML.Error.Child_Vector;
   use X_Proto_XML.Error_Copy;
   use X_Proto_XML.Request.Child_Vector;
   use X_Proto_XML.Reply.Child_Vector;
   use X_Proto_XML.Expression_Field.Child_Vector;
   use X_Proto_XML.Field_Reference;

   use type X_Proto_XML.Xcb.Ptr;

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

   package Tag_Id is

      type Enumeration_Type is (
                                Xcb,
                                Struct,
                                Field,
                                X_Id_Kind,
                                X_Id_Union,
                                Kind,
                                Type_Definition,
                                Pad,
                                Enum,
                                Item,
                                Value,
                                Bit,
                                List,
                                Field_Reference,
                                Op,
                                Event,
                                Documentation,
                                See,
                                Event_Copy,
                                Union,
                                Error,
                                Error_Copy,
                                Request,
                                Value_Param,
                                Reply,
                                Example,
                                Expression_Field
                               );

   end Tag_Id;

   use Tag_Id;

   type Current_Tag_T;

   type Current_Tag_Access_Type is access all XML_File_Parser.Current_Tag_T;
   for Current_Tag_Access_Type'Storage_Pool use X_Proto_XML.Pool;

   type Current_Tag_T (Kind_Id : Tag_Id.Enumeration_Type) is record
      Find_Tag : Current_Tag_Access_Type := null;
      case Kind_Id is
         when Xcb              => Xcb_V              : X_Proto_XML.Xcb.Ptr;
         when Struct           => Struct_V           : X_Proto_XML.Struct.Ptr;
         when Field            => Field_V            : X_Proto_XML.Field.Ptr;
         when X_Id_Kind        => X_Id_Kind_V        : X_Proto_XML.X_Id.Ptr;
         when X_Id_Union       => X_Id_Union_V       : X_Proto_XML.X_Id_Union.Ptr;
         when Kind             => Kind               : X_Proto_XML.Type_P.Ptr;
         when Type_Definition  => Type_Definition_V  : X_Proto_XML.Type_Definition.Ptr;
         when Pad              => Pad_V              : X_Proto_XML.Pad.Ptr;
         when Enum             => Enum_V             : X_Proto_XML.Enum.Ptr;
         when Item             => Item_V             : X_Proto_XML.Item.Ptr;
         when Value            => Value_V            : X_Proto_XML.Value_Access_Type;
         when Bit              => Bit_V              : X_Proto_XML.Item.Bit_Ptr;
         when List             => List_V             : X_Proto_XML.List.Ptr;
         when Field_Reference  => Field_Reference    : X_Proto_XML.Field_Reference_Access_Type;
         when Op               => Op_V               : X_Proto_XML.Operation.Ptr;
         when Event            => Event_V            : X_Proto_XML.Event.Ptr;
         when Documentation    => Documentation_V    : X_Proto_XML.Documentation.Ptr;
         when See              => See_V              : X_Proto_XML.See.Ptr;
         when Event_Copy       => Event_Copy_V       : X_Proto_XML.Event_Copy.Ptr;
         when Union            => Union_V            : X_Proto_XML.Union.Ptr;
         when Error            => Error_V            : X_Proto_XML.Error.Ptr;
         when Error_Copy       => Error_Copy_V       : X_Proto_XML.Error_Copy.Ptr;
         when Request          => Request_V          : X_Proto_XML.Request.Ptr;
         when Value_Param      => Value_Param_V      : X_Proto_XML.Value_Param.Ptr;
         when Reply            => Reply_V            : X_Proto_XML.Reply.Ptr;
         when Example          => Example_V          : X_Proto_XML.Example.Ptr;
         when Expression_Field => Expression_Field_V : X_Proto_XML.Expression_Field.Ptr;
      end case;
   end record;

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
                                                                                          Element_T         => Current_Tag_Access_Type,
                                                                                          Hash              => Hash,
                                                                                          Equivalent_Keys   => Aida.XML.DL."=",
                                                                                          Max_Hash_Map_Size => 1001,
                                                                                          Max_Collisions    => 5);

   use List_Of_Tag_Names_To_Current_Tag_Maps;

   subtype List_Of_Tag_Names_To_Current_Tag_Maps_T is List_Of_Tag_Names_To_Current_Tag_Maps.T;

   type List_Of_Tag_Names_To_Current_Tag_Maps_Ptr is access all List_Of_Tag_Names_To_Current_Tag_Maps.T;
   for List_Of_Tag_Names_To_Current_Tag_Maps_Ptr'Storage_Pool use X_Proto_XML.Pool;

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    Error_Message : out Aida.XML.Error_Message_T;
                    Is_Success    : out Boolean)
   is
      Parents_Including_Self_To_Current_Tag_Map : constant List_Of_Tag_Names_To_Current_Tag_Maps_Ptr
        := new List_Of_Tag_Names_To_Current_Tag_Maps_T;

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

      function Find_Tag (Key : Aida.XML.DL.T) return Current_Tag_Access_Type is
         R : constant List_Of_Tag_Names_To_Current_Tag_Maps.Find_Element_Result_T :=
           Find_Element (Parents_Including_Self_To_Current_Tag_Map.all, Key);
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
         This_Subprogram : constant String := "Start_Tag";

         Parents_Including_Self : Aida.XML.DL.T;

         procedure Insert (CT : Current_Tag_Access_Type) is
         begin
            Insert (This        => Parents_Including_Self_To_Current_Tag_Map.all,
                    Key         => Parents_Including_Self,
                    New_Element => CT);
         end Insert;

         Prev_Tag : constant Current_Tag_Access_Type := Find_Tag (Parent_Tags);
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         if Prev_Tag = null then
            if Tag_Name = Tag_Xcb then
               if Xcb_V /= null then
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & "1. Expected Xcb access to be null, tag name is " & Tag_Name);
                  return;
               end if;

               Xcb_V := new X_Proto_XML.Xcb.T;
               Insert (This        => Parents_Including_Self_To_Current_Tag_Map.all,
                       Key         => Parents_Including_Self,
                       New_Element => new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Xcb,
                                                                         Find_Tag => Find_Tag (Parent_Tags),
                                                                         Xcb_V    => Xcb_V));
               Is_Success := True;
            else
               Is_Success := False;
               Initialize (Error_Message, This_Subprogram & "2. Expected " & Tag_Xcb & ", but found " & Tag_Name);
            end if;
            return;
         end if;

         case Prev_Tag.Kind_Id is
            when Tag_Id.Xcb =>
               if Tag_Name = Tag_Struct then
                  declare
                     Struct_V : constant X_Proto_XML.Struct.Ptr := new X_Proto_XML.Struct.T;
                  begin
                     case Prev_Tag.Kind_Id is
                        when Tag_Id.Xcb =>
                           Append (Prev_Tag.Xcb_V.Structs, Struct_V);
                           Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Struct,
                                                                      Find_Tag             => Prev_Tag,
                                                                      Struct_V             => Struct_V));
                           Is_Success := True;
                        when others =>
                           Is_Success := False;
                           Initialize (Error_Message, This_Subprogram & "3. Expected " & Tag_Struct & ", but found " & Tag_Name);
                     end case;
                  end;
               elsif Tag_Name = Tag_X_Id_Kind then
                  declare
                     X_Id_Type_V : constant X_Proto_XML.X_Id.Ptr := new X_Proto_XML.X_Id.T;
                  begin
                     Append (Prev_Tag.Xcb_V.X_Ids, X_Id_Type_V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id     => Tag_Id.X_Id_Kind,
                                                                Find_Tag    => Prev_Tag,
                                                                X_Id_Kind_V => X_Id_Type_V));
                  end;
               elsif Tag_Name = Tag_X_Id_Union then
                  declare
                     X_Id_Union_V : constant X_Proto_XML.X_Id_Union.Ptr := new X_Proto_XML.X_Id_Union.T;
                  begin
                     Append (Prev_Tag.Xcb_V.X_Id_Unions, X_Id_Union_V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id      => Tag_Id.X_Id_Union,
                                                                Find_Tag     => Prev_Tag,
                                                                X_Id_Union_V => X_Id_Union_V));
                  end;
               elsif Tag_Name = Tag_Type_Definition then
                  declare
                     TD : constant X_Proto_XML.Type_Definition.Ptr := new X_Proto_XML.Type_Definition.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Type_Definitions, TD);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id           => Tag_Id.Type_Definition,
                                                                Find_Tag          => Prev_Tag,
                                                                Type_Definition_V => TD));
                  end;
               elsif Tag_Name = Tag_Enum then
                  declare
                     Enum_V : constant X_Proto_XML.Enum.Ptr := new X_Proto_XML.Enum.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Enums, Enum_V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Enum,
                                                                Find_Tag => Prev_Tag,
                                                                Enum_V   => Enum_V));
                  end;
               elsif Tag_Name = XML_Tag_Event then
                  declare
                     E : constant X_Proto_XML.Event.Ptr := new X_Proto_XML.Event.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Events, E);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Event,
                                                                Find_Tag => Prev_Tag,
                                                                Event_V  => E));
                  end;
               elsif Tag_Name = XML_Tag_Event_Copy then
                  declare
                     EC : constant X_Proto_XML.Event_Copy.Ptr := new X_Proto_XML.Event_Copy.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Event_Copies, EC);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id      => Tag_Id.Event_Copy,
                                                                Find_Tag     => Prev_Tag,
                                                                Event_Copy_V => EC));
                  end;
               elsif Tag_Name = XML_Tag_Union then
                  declare
                     U : constant X_Proto_XML.Union.Ptr := new X_Proto_XML.Union.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Unions, U);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Union,
                                                                Find_Tag             => Prev_Tag,
                                                                Union_V              => U));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     E : constant X_Proto_XML.Error.Ptr := new X_Proto_XML.Error.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Errors, E);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Error,
                                                                Find_Tag             => Prev_Tag,
                                                                Error_V              => E));
                  end;
               elsif Tag_Name = XML_Tag_Error_Copy then
                  declare
                     E : constant X_Proto_XML.Error_Copy.Ptr := new X_Proto_XML.Error_Copy.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Error_Copies, E);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Error_Copy,
                                                                Find_Tag             => Prev_Tag,
                                                                Error_Copy_V         => E));
                  end;
               elsif Tag_Name = XML_Tag_Request then
                  declare
                     R : constant X_Proto_XML.Request.Ptr := new X_Proto_XML.Request.T;
                  begin
                     Append (Prev_Tag.Xcb_V.Requests, R);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Request,
                                                                Find_Tag             => Prev_Tag,
                                                                Request_V            => R));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & "4. Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Struct =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Struct.Member_Ptr := new X_Proto_XML.Struct.Member_Type (Field_Member);
                  begin
                     Append (Prev_Tag.Struct_V.Members, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Field,
                                                                Find_Tag => Prev_Tag,
                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Struct.Member_Ptr := new X_Proto_XML.Struct.Member_Type (Pad_Member);
                  begin
                     Append (Prev_Tag.Struct_V.Members, P);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Pad,
                                                                Find_Tag => Prev_Tag,
                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Struct.Member_Ptr := new X_Proto_XML.Struct.Member_Type (List_Member);
                  begin
                     Append (Prev_Tag.Struct_V.Members, L);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.List,
                                                                Find_Tag => Prev_Tag,
                                                                List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & "5. Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.X_Id_Union =>
               if Tag_Name = Tag_Kind then
                  declare
                     Kind : constant X_Proto_XML.Type_P.Ptr := new X_Proto_XML.Type_P.T;
                  begin
                     Append (Prev_Tag.X_Id_Union_V.Kinds, Kind);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Kind,
                                                                Find_Tag => Prev_Tag,
                                                                Kind     => Kind));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 6. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum =>
               if Tag_Name = Tag_Item then
                  declare
                     Item_V : constant X_Proto_XML.Item.Ptr := new X_Proto_XML.Item.T;
                  begin
                     Append (Prev_Tag.Enum_V.Items, Item_V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Item,
                                                                Find_Tag => Prev_Tag,
                                                                Item_V   => Item_V));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : constant X_Proto_XML.Documentation.Ptr := new X_Proto_XML.Documentation.T;
                  begin
                     Append (Prev_Tag.Enum_V.Documentations, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id         => Tag_Id.Documentation,
                                                                Find_Tag        => Prev_Tag,
                                                                Documentation_V => D));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 7. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Item =>
               if Tag_Name = XML_Tag_Value then
                  Is_Success := True;
               elsif Tag_Name = Tag_Bit then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 8. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.List =>
               if Tag_Name = Tag_Field_Reference then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Operation then
                  declare
                     Operation : X_Proto_XML.List.Member_Ptr := new X_Proto_XML.List.Member_Type (X_Proto_XML.List.List_Member_Kind_Operation);
                  begin
                     Append (Prev_Tag.List_V.Members, Operation);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Op,
                                                                Find_Tag             => Prev_Tag,
                                                                Op_V                 => Operation.Operation'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.List.Member_Ptr := new X_Proto_XML.List.Member_Type (X_Proto_XML.List.List_Member_Kind_Value);
                  begin
                     Append (Prev_Tag.List_V.Members, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Value,
                                                                Find_Tag => Prev_Tag,
                                                                Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 9. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Op =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     V : constant X_Proto_XML.Operation.Member_Ptr := new X_Proto_XML.Operation.Member_Type (X_Proto_XML.Operation.Member_Operation);
                  begin
                     V.Operation := new X_Proto_XML.Operation.T;
                     Append (Prev_Tag.Op_V.Members, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Op,
                                                                Find_Tag             => Prev_Tag,
                                                                Op_V                 => V.Operation));
                  end;
               elsif Tag_Name = Tag_Field_Reference then
                  declare
                     V : X_Proto_XML.Operation.Member_Ptr := new X_Proto_XML.Operation.Member_Type (X_Proto_XML.Operation.Member_Kind_Field_Reference);
                  begin
                     Append (Prev_Tag.Op_V.Members, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Field_Reference,
                                                                Find_Tag             => Prev_Tag,
                                                                Field_Reference      => V.Field_Reference'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.Operation.Member_Ptr := new X_Proto_XML.Operation.Member_Type (X_Proto_XML.Operation.Member_Kind_Value);
                  begin
                     Append (Prev_Tag.Op_V.Members, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Value,
                                                                Find_Tag => Prev_Tag,
                                                                Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 10. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Event =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Event.Member_Ptr := new X_Proto_XML.Event.Member_Type (X_Proto_XML.Event.Event_Member_Field);
                  begin
                     Append (Prev_Tag.Event_V.Members, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Field,
                                                                Find_Tag             => Prev_Tag,
                                                                Field_V              => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Event.Member_Ptr := new X_Proto_XML.Event.Member_Type (X_Proto_XML.Event.Event_Member_Pad);
                  begin
                     Append (Prev_Tag.Event_V.Members, P);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Pad,
                                                                Find_Tag => Prev_Tag,
                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : X_Proto_XML.Event.Member_Ptr := new X_Proto_XML.Event.Member_Type (X_Proto_XML.Event.Event_Member_Doc);
                  begin
                     Append (Prev_Tag.Event_V.Members, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Documentation,
                                                                Find_Tag             => Prev_Tag,
                                                                Documentation_V      => D.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Event.Member_Ptr := new X_Proto_XML.Event.Member_Type (X_Proto_XML.Event.Event_Member_List);
                  begin
                     Append (Prev_Tag.Event_V.Members, L);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.List,
                                                                Find_Tag             => Prev_Tag,
                                                                List_V               => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 11. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Documentation =>
               if Tag_Name = Tag_Field then
                  declare
                     D : X_Proto_XML.Documentation.Member_Ptr := new X_Proto_XML.Documentation.Member_Type (X_Proto_XML.Documentation.Member_Field);
                  begin
                     Append (Prev_Tag.Documentation_V.Members, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Field,
                                                                Find_Tag => Prev_Tag,
                                                                Field_V  => D.F'Access));
                  end;
               elsif Tag_Name = XML_Tag_See then
                  declare
                     D : X_Proto_XML.Documentation.Member_Ptr := new X_Proto_XML.Documentation.Member_Type (X_Proto_XML.Documentation.Member_See);
                  begin
                     Append (Prev_Tag.Documentation_V.Members, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.See,
                                                                Find_Tag             => Prev_Tag,
                                                                See_V                => D.S'Access));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     D : X_Proto_XML.Documentation.Member_Ptr := new X_Proto_XML.Documentation.Member_Type (X_Proto_XML.Documentation.Member_Error);
                  begin
                     Append (Prev_Tag.Documentation_V.Members, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Error,
                                                                Find_Tag             => Prev_Tag,
                                                                Error_V              => D.E'Access));
                  end;
               elsif Tag_Name = XML_Tag_Example then
                  declare
                     D : X_Proto_XML.Documentation.Member_Ptr := new X_Proto_XML.Documentation.Member_Type (X_Proto_XML.Documentation.Member_Example);
                  begin
                     Append (Prev_Tag.Documentation_V.Members, D);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Example,
                                                                Find_Tag             => Prev_Tag,
                                                                Example_V            => D.Ex'Access));
                  end;
               elsif Tag_Name = XML_Tag_Brief then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Description then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 12. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Union =>
               if Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Union.Child_Ptr := new X_Proto_XML.Union.Child_Type (X_Proto_XML.Union.Child_List);
                  begin
                     Append (Prev_Tag.Union_V.Children, L);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.List,
                                                                Find_Tag => Prev_Tag,
                                                                List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 13. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Error =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Error.Child_Ptr := new X_Proto_XML.Error.Child_Type (X_Proto_XML.Error.Child_Field);
                  begin
                     Append (Prev_Tag.Error_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Field,
                                                                Find_Tag => Prev_Tag,
                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Error.Child_Ptr := new X_Proto_XML.Error.Child_Type (X_Proto_XML.Error.Child_Pad);
                  begin
                     Append (Prev_Tag.Error_V.Children, P);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Pad,
                                                                Find_Tag => Prev_Tag,
                                                                Pad_V    => P.P'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 14. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Request =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Field);
                  begin
                     Append (Prev_Tag.Request_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Field,
                                                                Find_Tag => Prev_Tag,
                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Pad);
                  begin
                     Append (Prev_Tag.Request_V.Children, P);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Pad,
                                                                Find_Tag => Prev_Tag,
                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value_Param then
                  declare
                     V : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Value_Param);
                  begin
                     Append (Prev_Tag.Request_V.Children, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Value_Param,
                                                                Find_Tag             => Prev_Tag,
                                                                Value_Param_V        => V.V'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     V : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Documentation);
                  begin
                     Append (Prev_Tag.Request_V.Children, V);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Documentation,
                                                                Find_Tag             => Prev_Tag,
                                                                Documentation_V      => V.D'Access));
                  end;
               elsif Tag_Name = XML_Tag_Reply then
                  declare
                     R : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Reply);
                  begin
                     Append (Prev_Tag.Request_V.Children, R);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Reply,
                                                                Find_Tag             => Prev_Tag,
                                                                Reply_V              => R.R'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     R : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_List);
                  begin
                     Append (Prev_Tag.Request_V.Children, R);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.List,
                                                                Find_Tag             => Prev_Tag,
                                                                List_V               => R.L'Access));
                  end;
               elsif Tag_Name = XML_Tag_Expression_Field then
                  declare
                     R : X_Proto_XML.Request.Child_Ptr := new X_Proto_XML.Request.Child_Type (X_Proto_XML.Request.Child_Expression_Field);
                  begin
                     Append (Prev_Tag.Request_V.Children, R);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Expression_Field,
                                                                Find_Tag             => Prev_Tag,
                                                                Expression_Field_V   => R.EF'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 15. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Reply =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Reply.Child_Ptr := new X_Proto_XML.Reply.Child_Type (X_Proto_XML.Reply.Child_Field);
                  begin
                     Append (Prev_Tag.Reply_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Field,
                                                                Find_Tag => Prev_Tag,
                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     F : X_Proto_XML.Reply.Child_Ptr := new X_Proto_XML.Reply.Child_Type (X_Proto_XML.Reply.Child_Pad);
                  begin
                     Append (Prev_Tag.Reply_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id  => Tag_Id.Pad,
                                                                Find_Tag => Prev_Tag,
                                                                Pad_V    => F.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     F : X_Proto_XML.Reply.Child_Ptr := new X_Proto_XML.Reply.Child_Type (X_Proto_XML.Reply.Child_Documentation);
                  begin
                     Append (Prev_Tag.Reply_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Documentation,
                                                                Find_Tag             => Prev_Tag,
                                                                Documentation_V      => F.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     F : X_Proto_XML.Reply.Child_Ptr := new X_Proto_XML.Reply.Child_Type (X_Proto_XML.Reply.Child_List);
                  begin
                     Append (Prev_Tag.Reply_V.Children, F);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.List,
                                                                Find_Tag             => Prev_Tag,
                                                                List_V               => F.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 16. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Expression_Field =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     C : X_Proto_XML.Expression_Field.Child_Ptr := new X_Proto_XML.Expression_Field.Child_Type (X_Proto_XML.Expression_Field.Child_Operation);
                  begin
                     Append (Prev_Tag.Expression_Field_V.Children, C);
                     Is_Success := True;
                     Insert (new XML_File_Parser.Current_Tag_T'(Kind_Id              => Tag_Id.Op,
                                                                Find_Tag             => Prev_Tag,
                                                                Op_V                 => C.Op'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 17. found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Field |
                 Tag_Id.X_Id_Kind |
                 Tag_Id.Kind |
                 Tag_Id.Type_Definition |
                 Tag_Id.Pad |
                 Tag_Id.Value |
                 Tag_Id.Bit |
                 Tag_Id.Field_Reference |
                 Tag_Id.See |
                 Tag_Id.Event_Copy |
                 Tag_Id.Error_Copy |
                 Tag_Id.Value_Param |
                 Tag_Id.Example =>
               Is_Success := False;
               Initialize (Error_Message, This_Subprogram & ", 18, Error, tag name is " & Tag_Name);
         end case;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : Aida.XML.DL.T;
                           Error_Message               : out Aida.XML.Error_Message_T;
                           Is_Success                  : out Boolean)
      is
         This_Subprogram : constant String := "Attribute";

         Current_Tag : constant Current_Tag_Access_Type := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if Current_Tag = null then
            Is_Success := False;
            Initialize (Error_Message, This_Subprogram & ", 1, attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (
                        Parent_Tags_And_Current_Tag));
            return;
         end if;

         Is_Success := True;
         case Current_Tag.Kind_Id is
            when Tag_Id.Xcb =>
               if Attribute_Name = Tag_Xcb_Attribute_Header then
                  Is_Success := True;
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Xcb_V.Header := (Exists => True,
                                                  Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 2, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Struct =>
               if Attribute_Name = Tag_Struct_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Struct_V.Name := (Exists => True,
                                                   Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 3, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Field =>
               if Attribute_Name = Tag_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Field_V.Kind := (Exists => True,
                                                  Value  => V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Field_V.Name := (Exists => True,
                                                  Value  => V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Field_V.Enum := (Exists => True,
                                                  Value  => V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Mask then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Field_V.Mask := (Exists => True,
                                                  Value  => V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Alt_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Field_V.Alt_Enum := (Exists => True,
                                                      Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 4, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Kind =>
               if Attribute_Name = Tag_X_Id_Kind_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.X_Id_Kind_V.Name := (Exists => True,
                                                      Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 5, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Union =>
               if Attribute_Name = Tag_X_Id_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.X_Id_Union_V.Name := (Exists => True,
                                                       Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 6, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Type_Definition =>
               if Attribute_Name = Tag_Type_Definition_Attribute_Old_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Type_Definition_V.Old_Name := (Exists => True,
                                                                Value  => V);
                  end;
               elsif Attribute_Name = Tag_Type_Definition_Attribute_New_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Type_Definition_V.New_Name := (Exists => True,
                                                                Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 7, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Pad =>
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
                        Initialize (Error_Message, This_Subprogram & ", 8, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Pad_V.all.Bytes := (Exists => True, Value => Positive (V));
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 9, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum =>
               if Attribute_Name = Tag_Enum_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Enum_V.Name := (Exists => True,
                                                 Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 9, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Item =>
               if Attribute_Name = Tag_Item_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Item_V.Name := (Exists => True,
                                                 Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 10, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.List =>
               if Attribute_Name = Tag_List_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.List_V.Kind := (Exists => True,
                                                 Value  => V);
                  end;
               elsif Attribute_Name = Tag_List_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.List_V.Name := (Exists => True,
                                                 Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 11, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Op =>
               if Attribute_Name = Tag_Operation_Attribute_Op then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Op_V.Op := (Exists => True,
                                             Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 12, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event =>
               if Attribute_Name = XML_Tag_Event_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Event_V.Name := (Exists => True,
                                                  Value  => V);
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
                        Initialize (Error_Message, This_Subprogram & ", 13, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Event_V.Number := (Exists => True,
                                                       Value  => Positive (V));
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_No_Sequence_Number then
                  if Attribute_Value = "true" then
                     Current_Tag.Event_V.No_Sequence_Number := (Exists => True,
                                                                Value  => True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Event_V.No_Sequence_Number := (Exists => True,
                                                                Value  => False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, This_Subprogram & ", 14, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Event_Attribute_XGE then
                  if Attribute_Value = "true" then
                     Current_Tag.Event_V.XGE := (Exists => True,
                                                 Value  => True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Event_V.XGE := (Exists => True,
                                                 Value  => False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, This_Subprogram & ", 15, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 16, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.See =>
               if Attribute_Name = XML_Tag_See_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.See_V.Name := (Exists => True,
                                                Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_See_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.See_V.Kind := (Exists => True,
                                                Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 17, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Copy =>
               if Attribute_Name = XML_Tag_Event_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Event_Copy_V.Name := (Exists => True,
                                                       Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Ref then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Event_Copy_V.Ref := (Exists => True,
                                                      Value  => V);
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
                        Initialize (Error_Message, This_Subprogram & ", 18, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Event_Copy_V.Number := (Exists => True,
                                                            Value  => V);
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 19, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Union =>
               if Attribute_Name = XML_Tag_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Union_V.Name := (Exists => True,
                                                  Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 20, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error =>
               if Attribute_Name = XML_Tag_Error_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Error_V.Name := (Exists => True,
                                                  Value  => V);
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
                        Initialize (Error_Message, This_Subprogram & ", 21, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Error_V.Number := (Exists => True,
                                                       Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Error_V.Kind := (Exists => True,
                                                  Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 22, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error_Copy =>
               if Attribute_Name = XML_Tag_Error_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Error_Copy_V.Name := (Exists => True,
                                                       Value  => V);
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
                        Initialize (Error_Message, This_Subprogram & ", 23, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Error_Copy_V.Number := (Exists => True,
                                                            Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Ref then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Error_Copy_V.Ref := (Exists => True,
                                                      Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 23, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Request =>
               if Attribute_Name = XML_Tag_Request_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Request_V.Name := (Exists => True,
                                                    Value  => V);
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
                        Initialize (Error_Message, This_Subprogram & ", 24, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Request_V.Op_Code := (Exists => True,
                                                          Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Combine_Adjacent then
                  if Attribute_Value = "true" then
                     Current_Tag.Request_V.Shall_Combine_Adjacent := (Exists => True,
                                                                      Value  => True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Request_V.Shall_Combine_Adjacent := (Exists => True,
                                                                      Value  => False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, This_Subprogram & ", 25, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 26, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Value_Param =>
               if Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Value_Param_V.Mask_Kind := (Exists => True,
                                                             Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Value_Param_V.Mask_Name := (Exists => True,
                                                             Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_List_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Value_Param_V.List_Name := (Exists => True,
                                                             Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 27, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Expression_Field =>
               if Attribute_Name = XML_Tag_Expression_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Expression_Field_V.Name := (Exists => True,
                                                             Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Expression_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Current_Tag.Expression_Field_V.Kind := (Exists => True,
                                                             Value  => V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 28, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Kind |
                 Tag_Id.Value |
                 Tag_Id.Bit |
                 Tag_Id.Field_Reference |
                 Tag_Id.Documentation |
                 Tag_Id.Reply |
                 Tag_Id.Example =>
               Is_Success := False;
               Initialize (Error_Message, This_Subprogram & ", 29, found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
         end case;
      end Attribute;

      procedure End_Tag (Tag_Name      : String;
                         Parent_Tags   : Aida.XML.DL.T;
                         Error_Message : out Aida.XML.Error_Message_T;
                         Is_Success    : out Boolean)
      is
         This_Subprogram : constant String := "End_Tag";

         Parents_Including_Self : Aida.XML.DL.T;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self => Parents_Including_Self,
                                          Parents                => Parent_Tags,
                                          Tag_Name               => Tag_Name);

         begin
            Delete (Parents_Including_Self_To_Current_Tag_Map.all, Parents_Including_Self);
         exception
            when Unknown_Exception : others =>
               Ada.Text_IO.Put_Line ("delete 1");
               Is_Success := False;
               Initialize (Error_Message, This_Subprogram & Ada.Exceptions.Exception_Information(Unknown_Exception));
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
         This_Subprogram : constant String := "End_Tag2";

         Parents_Including_Self : Aida.XML.DL.T;

         Current_Tag : Current_Tag_Access_Type;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         Current_Tag := Find_Tag (Parents_Including_Self);

         Is_Success := True;

         if Current_Tag = null then
            declare
               Prev_Tag : constant Current_Tag_Access_Type := Find_Tag (Parent_Tags);
            begin
               if Prev_Tag = null then
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 1, Both Current_Tag and Prev_Tag was null for end tag '" & Tag_Name & "' and value '" & Tag_Value & "'");
                  return;
               end if;

               case Prev_Tag.Kind_Id is
                  when Tag_Id.Item =>
                     declare
                        V : X_Proto_XML.Value_Type;
                        Has_Failed : Boolean;
                     begin
                        To_Int32 (Source     => Aida.String_T (Tag_Value),
                                  Target     => Aida.Int32_T (V),
                                  Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Initialize (Error_Message, This_Subprogram & ", 1, failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           case Prev_Tag.Item_V.Kind_Id is
                              when X_Proto_XML.Item.Not_Specified =>
                                 if Tag_Name = XML_Tag_Value then
                                    Prev_Tag.Item_V.Kind_Id := X_Proto_XML.Item.Specified_As_Value;
                                    Prev_Tag.Item_V.Value := V;
                                 elsif Tag_Name = Tag_Bit then
                                    Prev_Tag.Item_V.Kind_Id := X_Proto_XML.Item.Specified_As_Bit;
                                    Prev_Tag.Item_V.Bit     := X_Proto_XML.Item.Bit_Type (V);
                                 else
                                    Is_Success := False;
                                    Initialize (Error_Message, This_Subprogram & ", 2, unknown end tag '" & Tag_Name & "'");
                                 end if;
                              when X_Proto_XML.Item.Specified_As_Value |
                                   X_Proto_XML.Item.Specified_As_Bit =>
                                 Is_Success := False;
                                 Initialize (Error_Message, This_Subprogram & ", 3, value already initialized for end tag " & Tag_Name & ", kind id " & X_Proto_XML.Item.Kind_Id_Type'Image (Prev_Tag.Item_V.Kind_Id))
;
                           end case;
                        end if;
                     end;
                  when Tag_Id.List =>
                     if Tag_Name = Tag_Field_Reference then
                        declare
                           L : constant X_Proto_XML.List.Member_Ptr := new X_Proto_XML.List.Member_Type (X_Proto_XML.List.List_Member_Kind_Field_Reference);
                        begin
                           Initialize (L.Field_Reference, Tag_Value);
                           Append (Prev_Tag.List_V.Members, L);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, This_Subprogram & ", 4, found unexpected end tag '" & Tag_Name & "' and previous tag is " & Tag_Id.Enumeration_Type'Image (Prev_Tag.Kind_Id));
                     end if;
                  when Tag_Id.Documentation =>
                     if Tag_Name = XML_Tag_Brief then
                        declare
                           V : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Initialize (V, Tag_Value);
                           Prev_Tag.Documentation_V.Brief_Description := (Exists => True,
                                                                          Value  => V);
                        end;
                     elsif Tag_Name = XML_Tag_Description then
                        declare
                           V : X_Proto_XML.Large_Bounded_String.T;
                        begin
                           Initialize (V, Tag_Value);
                           Prev_Tag.Documentation_V.Description := (Exists => True,
                                                                    Value  => V);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, This_Subprogram & ", 5, found unexpected end tag '" & Tag_Name & "' and previous tag is " & Tag_Id.Enumeration_Type'Image (Prev_Tag.Kind_Id));
                     end if;
                  when others =>
                     Is_Success := False;
                     Initialize (Error_Message, This_Subprogram & ", 6, found unexpected end tag '" & Tag_Name & "'");
               end case;
            end;
         else
            begin
               Delete (Parents_Including_Self_To_Current_Tag_Map.all, Parents_Including_Self);
            exception
               when Unknown_Exception : others =>
                  Ada.Text_IO.Put_Line ("delete 2");
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 7," & Ada.Exceptions.Exception_Information(Unknown_Exception));
                  return;
            end;

            case Current_Tag.Kind_Id is
               when Tag_Id.Kind =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Kind.Value := (Exists => True,
                                                Value  => V);
                  end;
               when Tag_Id.Field =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Field_V.Value := (Exists => True,
                                                   Value  => V);
                  end;
               when Tag_Id.Value =>
                  declare
                     V : X_Proto_XML.Value_Type;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Tag_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, This_Subprogram & ", 8, failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                     else
                        Current_Tag.Value_V.all := V;
                     end if;
                  end;
               when Tag_Id.Error =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Error_V.Value := (Exists => True,
                                                   Value  => V);
                  end;
               when Tag_Id.Example =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Example_V.Value := (Exists => True,
                                                     Value  => V);
                  end;
               when Tag_Id.Op =>
                  if Tag_Name = Tag_Field_Reference then
                     declare
                        V : X_Proto_XML.Field_Reference_Type;
                     begin
                        Initialize (V, Tag_Value);
                        Append (Current_Tag.Op_V.Members, new X_Proto_XML.Operation.Member_Type'(Kind_Id         => X_Proto_XML.Operation.Member_Kind_Field_Reference,
                                                                                                 Field_Reference => V));
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
                           Initialize (Error_Message, This_Subprogram & ", 9, failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           Append (Current_Tag.Op_V.Members, new X_Proto_XML.Operation.Member_Type'(Kind_Id => X_Proto_XML.Operation.Member_Kind_Value,
                                                                                                    Value   => V));
                        end if;
                     end;
                  else
                     Is_Success := False;
                     Initialize (Error_Message, This_Subprogram & ", 10, found unexpected end tag '" & Tag_Name & "' and previous tag is " & Tag_Id.Enumeration_Type'Image (Current_Tag.Kind_Id));
                  end if;
               when Tag_Id.Field_Reference =>
                  declare
                     V : X_Proto_XML.Field_Reference_Type;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Field_Reference.all := V;
                  end;
               when Tag_Id.Xcb |
                    Tag_Id.Struct |
                    Tag_Id.Bit |
                    Tag_Id.X_Id_Kind |
                    Tag_Id.X_Id_Union |
                    Tag_Id.Type_Definition |
                    Tag_Id.Pad |
                    Tag_Id.Enum |
                    Tag_Id.Item |
                    Tag_Id.List |
                    Tag_Id.Event |
                    Tag_id.Documentation |
                    Tag_Id.See |
                    Tag_Id.Event_Copy |
                    Tag_Id.Union |
                    Tag_Id.Error_Copy |
                    Tag_Id.Request |
                    Tag_Id.Value_Param |
                    Tag_Id.Reply |
                    Tag_Id.Expression_Field =>
                  Is_Success := False;
                  Initialize (Error_Message, This_Subprogram & ", 11, but found unexpected end tag " & Tag_Name);
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
