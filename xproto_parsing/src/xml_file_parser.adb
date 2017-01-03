with Ada.Text_IO;
with SXML.Generic_Parse_XML_File;
with Aida.Containers.Bounded_Hash_Map;
with GNAT.Source_Info;
with Ada.Exceptions;

package body XML_File_Parser is

   use Aida.String;
   use X_Proto.Struct.Fs.Member_Kind_Id;
   use X_Proto.Operation;
   use SXML.Error_Message_P;
   use SXML.DL;
   use SXML.Bounded_String;
   use X_Proto.Large_Bounded_String;
   use X_Proto.Xcb;
   use X_Proto.Struct;
   use X_Proto.Union;
   use X_Proto.Enum;
   use X_Proto.X_Id_Union;
   use X_Proto.List;
   use X_Proto.Event;
   use X_Proto.Event_Copy;
   use X_Proto.Documentation;
   use X_Proto.Error;
   use X_Proto.Error_Copy;
   use X_Proto.Request;
   use X_Proto.Reply;
   use X_Proto.Expression_Field;
   use X_Proto.Field;
   use X_Proto.X_Id;
   use X_Proto.Type_Definition;
   use X_Proto.Pad;
   use X_Proto.Item;
   use X_Proto.See;
   use X_Proto.Value_Param;
   use X_Proto.Type_P;
   use X_Proto.Example;
   use X_Proto.Field_Reference;

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
                                Xcb_Type_Id,
                                Enum_Struct,
                                Enum_Field,
                                X_Id_Kind_Type_Id,
                                Enum_X_Id_Union,
                                Kind,
                                Type_Definition_Type_Id,
                                Enum_Pad,
                                Enum_Enum,
                                Item_Type_Id,
                                Value_Type_Id,
                                Bit_Type_Id,
                                List_Type_Id,
                                Field_Reference,
                                Op_Type_Id,
                                Event_Type_Id,
                                Documentation_Type_Id,
                                See_Type_Id,
                                Event_Copy_Type_Id,
                                Union_Type_Id,
                                Error_Type_Id,
                                Error_Copy_Type_Id,
                                Request_Type_Id,
                                Value_Param_Type_Id,
                                Reply_Type_Id,
                                Example_Type_Id,
                                Expression_Field_Type_Id
                               );

   end Tag_Id;

   use Tag_Id;

   type Current_Tag_Type;

   type Current_Tag_Access_Type is access all Current_Tag_Type;

   type Current_Tag_Type (Kind_Id : Tag_Id.Enumeration_Type) is record
      Find_Tag : Current_Tag_Access_Type := null;
      case Kind_Id is
         when Xcb_Type_Id              => Xcb_V              : X_Proto.Xcb.Ptr;
         when Enum_Struct              => Struct_V           : X_Proto.Struct.Ptr;
         when Enum_Field               => Field_V            : X_Proto.Field.Ptr;
         when X_Id_Kind_Type_Id        => X_Id_Kind_V        : X_Proto.X_Id.Ptr;
         when Enum_X_Id_Union          => X_Id_Union_V       : X_Proto.X_Id_Union.Ptr;
         when Kind                     => Kind               : X_Proto.Type_P.Ptr;
         when Type_Definition_Type_Id  => Type_Definition_V  : X_Proto.Type_Definition.Ptr;
         when Enum_Pad                 => Pad_V              : X_Proto.Pad.Ptr;
         when Enum_Enum                => Enum_V             : X_Proto.Enum.Ptr;
         when Item_Type_Id             => Item_V             : X_Proto.Item.Ptr;
         when Value_Type_Id            => Value_V            : X_Proto.Value_Access_Type;
         when Bit_Type_Id              => Bit_V              : X_Proto.Item.Fs.Bit_Ptr;
         when List_Type_Id             => List_V             : X_Proto.List.Ptr;
         when Field_Reference          => Field_Reference    : X_Proto.Field_Reference_Access_Type;
         when Op_Type_Id               => Op_V               : X_Proto.Operation.Ptr;
         when Event_Type_Id            => Event_V            : X_Proto.Event.Ptr;
         when Documentation_Type_Id    => Documentation_V    : X_Proto.Documentation.Ptr;
         when See_Type_Id              => See_V              : X_Proto.See.Ptr;
         when Event_Copy_Type_Id       => Event_Copy_V       : X_Proto.Event_Copy.Ptr;
         when Union_Type_Id            => Union_V            : X_Proto.Union.Ptr;
         when Error_Type_Id            => Error_V            : X_Proto.Error.Ptr;
         when Error_Copy_Type_Id       => Error_Copy_V       : X_Proto.Error_Copy.Ptr;
         when Request_Type_Id          => Request_V          : X_Proto.Request.Ptr;
         when Value_Param_Type_Id      => Value_Param_V      : X_Proto.Value_Param.Ptr;
         when Reply_Type_Id            => Reply_V            : X_Proto.Reply.Ptr;
         when Example_Type_Id          => Example_V          : X_Proto.Example.Ptr;
         when Expression_Field_Type_Id => Expression_Field_V : X_Proto.Expression_Field.Ptr;
      end case;
   end record;

   function Hash (Parent_And_Self_Tags : SXML.DL.T) return Aida.Hash32_T is
      R : Aida.Hash32_T := 0;

      use type Aida.Hash32_T;
   begin
      if Is_Empty (Parent_And_Self_Tags) then
         return 0;
      end if;

      for I in SXML.DL.Index_T range 1..Last_Index (Parent_And_Self_Tags) loop
         R := R + SXML.Bounded_String.Hash32 (Const_Ref (Parent_And_Self_Tags, I).all);
      end loop;
      return R;
   end Hash;

   package List_Of_Tag_Names_To_Current_Tag_Maps is new Aida.Containers.Bounded_Hash_Map (Key_T             => SXML.DL.T,
                                                                                          Element_T         => Current_Tag_Access_Type,
                                                                                          Hash              => Hash,
                                                                                          Equivalent_Keys   => SXML.DL."=",
                                                                                          Max_Hash_Map_Size => 1001,
                                                                                          Max_Collisions    => 5);

   use List_Of_Tag_Names_To_Current_Tag_Maps;

   Parents_Including_Self_To_Current_Tag_Map : List_Of_Tag_Names_To_Current_Tag_Maps.T;

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto.Xcb.Ptr;
                    Error_Message : out SXML.Error_Message_T;
                    Is_Success    : out Boolean)
   is
--        Parents_Including_Self_To_Current_Tag_Map_Pointer : List_Of_Tag_Names_To_Current_Tag_Maps.Ptr :=
--          new List_Of_Tag_Names_To_Current_Tag_Maps.T;
--        Parents_Including_Self_To_Current_Tag_Map renames Parents_Including_Self_To_Current_Tag_Map_Pointer.all;
--
      procedure Populate_Parents_Including_Self (Parents_Including_Self : in out SXML.DL.T;
                                                 Parents                : SXML.DL.T;
                                                 Tag_Name               : String)
      is
         TN : SXML.Bounded_String_T;
      begin
         for I in SXML.DL.Index_T range 1..Last_Index (Parents) loop
            Append (This     => Parents_Including_Self,
                    New_Item => Const_Ref (Parents, I).all);
         end loop;
         Initialize (This => TN,
                     Text => Tag_Name);
         Append (This     => Parents_Including_Self,
                 New_Item => TN);
      end Populate_Parents_Including_Self;

      function To_String (Tags : SXML.DL.T) return String is
         R : X_Proto.Large_Bounded_String.T;
      begin
         for I in SXML.DL.Index_T range 1..Last_Index (Tags) loop
            Append (R, (To_String (Element (Tags, I)) & ", "));
         end loop;

         return To_String (R);
      end To_String;

      function Find_Tag (Key : SXML.DL.T) return Current_Tag_Access_Type is
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
                           Parent_Tags   : SXML.DL.T;
                           Error_Message : out SXML.Error_Message_T;
                           Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.T;

         procedure Insert (CT : Current_Tag_Access_Type) is
         begin
            Insert (This        => Parents_Including_Self_To_Current_Tag_Map,
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
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected Xcb access to be null, tag name is " & Tag_Name);
                  return;
               end if;

               Xcb_V := new X_Proto.Xcb.T;
               Insert (This        => Parents_Including_Self_To_Current_Tag_Map,
                       Key         => Parents_Including_Self,
                       New_Element => new Current_Tag_Type'(Kind_Id  => Tag_Id.Xcb_Type_Id,
                                                            Find_Tag => Find_Tag (Parent_Tags),
                                                        Xcb_V    => Xcb_V));
               Is_Success := True;
            else
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Xcb & ", but found " & Tag_Name);
            end if;
            return;
         end if;

         case Prev_Tag.Kind_Id is
            when Tag_Id.Xcb_Type_Id =>
               if Tag_Name = Tag_Struct then
                  declare
                     Struct_V : constant X_Proto.Struct.Ptr := new X_Proto.Struct.T;
                  begin
                     case Prev_Tag.Kind_Id is
                        when Tag_Id.Xcb_Type_Id =>
                           Append_Struct (Prev_Tag.Xcb_V.all, Struct_V);
                           Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Enum_Struct,
                                                         Find_Tag             => Prev_Tag,
                                                         Struct_V             => Struct_V));
                           Is_Success := True;
                        when others =>
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Struct & ", but found " & Tag_Name);
                     end case;
                  end;
               elsif Tag_Name = Tag_X_Id_Kind then
                  declare
                     X_Id_Type_V : constant X_Proto.X_Id.Ptr := new X_Proto.X_Id.T;
                  begin
                     Append_X_Id (Prev_Tag.Xcb_V.all, X_Id_Type_V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id     => Tag_Id.X_Id_Kind_Type_Id,
                                                   Find_Tag    => Prev_Tag,
                                                   X_Id_Kind_V => X_Id_Type_V));
                  end;
               elsif Tag_Name = Tag_X_Id_Union then
                  declare
                     X_Id_Union_V : constant X_Proto.X_Id_Union.Ptr := new X_Proto.X_Id_Union.T;
                  begin
                     Append_X_Id_Union (Prev_Tag.Xcb_V.all, X_Id_Union_V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id      => Tag_Id.Enum_X_Id_Union,
                                                   Find_Tag     => Prev_Tag,
                                                   X_Id_Union_V => X_Id_Union_V));
                  end;
               elsif Tag_Name = Tag_Type_Definition then
                  declare
                     TD : constant X_Proto.Type_Definition.Ptr := new X_Proto.Type_Definition.T;
                  begin
                     Append_Type_Definition (Prev_Tag.Xcb_V.all, TD);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id           => Tag_Id.Type_Definition_Type_Id,
                                                   Find_Tag          => Prev_Tag,
                                                   Type_Definition_V => TD));
                  end;
               elsif Tag_Name = Tag_Enum then
                  declare
                     Enum_V : constant X_Proto.Enum.Ptr := new X_Proto.Enum.T;
                  begin
                     Append_Enum (Prev_Tag.Xcb_V.all, Enum_V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Enum,
                                                   Find_Tag => Prev_Tag,
                                                   Enum_V   => Enum_V));
                  end;
               elsif Tag_Name = XML_Tag_Event then
                  declare
                     E : constant X_Proto.Event.Ptr := new X_Proto.Event.T;
                  begin
                     Append_Event (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Event_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   Event_V  => E));
                  end;
               elsif Tag_Name = XML_Tag_Event_Copy then
                  declare
                     EC : constant X_Proto.Event_Copy.Ptr := new X_Proto.Event_Copy.T;
                  begin
                     Append_Event_Copy (Prev_Tag.Xcb_V.all, EC);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id      => Tag_Id.Event_Copy_Type_Id,
                                                   Find_Tag     => Prev_Tag,
                                                   Event_Copy_V => EC));
                  end;
               elsif Tag_Name = XML_Tag_Union then
                  declare
                     U : constant X_Proto.Union.Ptr := new X_Proto.Union.T;
                  begin
                     Append_Union (Prev_Tag.Xcb_V.all, U);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Union_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Union_V              => U));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     E : constant X_Proto.Error.Ptr := new X_Proto.Error.T;
                  begin
                     Append_Error (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Error_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Error_V              => E));
                  end;
               elsif Tag_Name = XML_Tag_Error_Copy then
                  declare
                     E : constant X_Proto.Error_Copy.Ptr := new X_Proto.Error_Copy.T;
                  begin
                     Append_Error_Copy (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Error_Copy_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Error_Copy_V         => E));
                  end;
               elsif Tag_Name = XML_Tag_Request then
                  declare
                     R : constant X_Proto.Request.Ptr := new X_Proto.Request.T;
                  begin
                     Append_Request (Prev_Tag.Xcb_V.all, R);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Request_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Request_V            => R));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Struct =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto.Struct.Fs.Member_Ptr := new X_Proto.Struct.Fs.Member_Type (Field_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                   Find_Tag => Prev_Tag,
                                                   Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto.Struct.Fs.Member_Ptr := new X_Proto.Struct.Fs.Member_Type (Pad_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, P);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                   Find_Tag => Prev_Tag,
                                                   Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto.Struct.Fs.Member_Ptr := new X_Proto.Struct.Fs.Member_Type (List_Member);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, L);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.List_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_X_Id_Union =>
               if Tag_Name = Tag_Kind then
                  declare
                     Kind : constant X_Proto.Type_P.Ptr := new X_Proto.Type_P.T;
                  begin
                     Append_Kind (Prev_Tag.X_Id_Union_V.all, Kind);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Kind,
                                                   Find_Tag => Prev_Tag,
                                                   Kind     => Kind));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Enum =>
               if Tag_Name = Tag_Item then
                  declare
                     Item_V : constant X_Proto.Item.Ptr := new X_Proto.Item.T;
                  begin
                     Append_Item (Prev_Tag.Enum_V.all, Item_V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Item_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   Item_V   => Item_V));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : constant X_Proto.Documentation.Ptr := new X_Proto.Documentation.T;
                  begin
                     Append_Documentation (Prev_Tag.Enum_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id         => Tag_Id.Documentation_Type_Id,
                                                   Find_Tag        => Prev_Tag,
                                                   Documentation_V => D));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Item_Type_Id =>
               if Tag_Name = XML_Tag_Value then
                  Is_Success := True;
               elsif Tag_Name = Tag_Bit then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.List_Type_Id =>
               if Tag_Name = Tag_Field_Reference then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Operation then
                  declare
                     Operation : X_Proto.List.Fs.Member_Ptr := new X_Proto.List.Fs.Member_Type (X_Proto.List.Fs.List_Member_Kind_Operation);
                  begin
                     Append_Member (Prev_Tag.List_V.all, Operation);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Op_V                 => Operation.Operation'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto.List.Fs.Member_Ptr := new X_Proto.List.Fs.Member_Type (X_Proto.List.Fs.List_Member_Kind_Value);
                  begin
                     Append_Member (Prev_Tag.List_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Value_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Op_Type_Id =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     V : constant X_Proto.Operation.Fs.Member_Ptr := new X_Proto.Operation.Fs.Member_Type (X_Proto.Operation.Fs.Member_Operation);
                  begin
                     V.Operation := new X_Proto.Operation.T;
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Op_V                 => V.Operation));
                  end;
               elsif Tag_Name = Tag_Field_Reference then
                  declare
                     V : X_Proto.Operation.Fs.Member_Ptr := new X_Proto.Operation.Fs.Member_Type (X_Proto.Operation.Fs.Member_Kind_Field_Reference);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Field_Reference,
                                                   Find_Tag             => Prev_Tag,
                                                   Field_Reference      => V.Field_Reference'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto.Operation.Fs.Member_Ptr := new X_Proto.Operation.Fs.Member_Type (X_Proto.Operation.Fs.Member_Kind_Value);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Value_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Event_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto.Event.Fs.Member_Ptr := new X_Proto.Event.Fs.Member_Type (X_Proto.Event.Fs.Event_Member_Field);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Enum_Field,
                                                   Find_Tag             => Prev_Tag,
                                                   Field_V              => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto.Event.Fs.Member_Ptr := new X_Proto.Event.Fs.Member_Type (X_Proto.Event.Fs.Event_Member_Pad);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, P);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                   Find_Tag => Prev_Tag,
                                                   Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : X_Proto.Event.Fs.Member_Ptr := new X_Proto.Event.Fs.Member_Type (X_Proto.Event.Fs.Event_Member_Doc);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Documentation_V      => D.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto.Event.Fs.Member_Ptr := new X_Proto.Event.Fs.Member_Type (X_Proto.Event.Fs.Event_Member_List);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, L);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   List_V               => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Documentation_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     D : X_Proto.Documentation.Fs.Member_Ptr := new X_Proto.Documentation.Fs.Member_Type (X_Proto.Documentation.Fs.Member_Field);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                   Find_Tag => Prev_Tag,
                                                   Field_V  => D.F'Access));
                  end;
               elsif Tag_Name = XML_Tag_See then
                  declare
                     D : X_Proto.Documentation.Fs.Member_Ptr := new X_Proto.Documentation.Fs.Member_Type (X_Proto.Documentation.Fs.Member_See);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.See_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   See_V                => D.S'Access));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     D : X_Proto.Documentation.Fs.Member_Ptr := new X_Proto.Documentation.Fs.Member_Type (X_Proto.Documentation.Fs.Member_Error);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Error_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Error_V              => D.E'Access));
                  end;
               elsif Tag_Name = XML_Tag_Example then
                  declare
                     D : X_Proto.Documentation.Fs.Member_Ptr := new X_Proto.Documentation.Fs.Member_Type (X_Proto.Documentation.Fs.Member_Example);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Example_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Example_V            => D.Ex'Access));
                  end;
               elsif Tag_Name = XML_Tag_Brief then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Description then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Union_Type_Id =>
               if Tag_Name = Tag_List then
                  declare
                     L : X_Proto.Union.Fs.Child_Ptr := new X_Proto.Union.Fs.Child_Type (X_Proto.Union.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Union_V.all, L);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.List_Type_Id,
                                                   Find_Tag => Prev_Tag,
                                                   List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Error_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto.Error.Fs.Child_Ptr := new X_Proto.Error.Fs.Child_Type (X_Proto.Error.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                   Find_Tag => Prev_Tag,
                                                   Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto.Error.Fs.Child_Ptr := new X_Proto.Error.Fs.Child_Type (X_Proto.Error.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, P);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                   Find_Tag => Prev_Tag,
                                                   Pad_V    => P.P'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Request_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                   Find_Tag => Prev_Tag,
                                                   Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, P);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                   Find_Tag => Prev_Tag,
                                                   Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value_Param then
                  declare
                     V : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Value_Param);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Value_Param_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Value_Param_V        => V.V'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     V : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Documentation);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Documentation_V      => V.D'Access));
                  end;
               elsif Tag_Name = XML_Tag_Reply then
                  declare
                     R : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Reply);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Reply_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Reply_V              => R.R'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     R : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   List_V               => R.L'Access));
                  end;
               elsif Tag_Name = XML_Tag_Expression_Field then
                  declare
                     R : X_Proto.Request.Fs.Child_Ptr := new X_Proto.Request.Fs.Child_Type (X_Proto.Request.Fs.Child_Expression_Field);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Expression_Field_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Expression_Field_V   => R.EF'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Reply_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto.Reply.Fs.Child_Ptr := new X_Proto.Reply.Fs.Child_Type (X_Proto.Reply.Fs.Child_Field);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                   Find_Tag => Prev_Tag,
                                                   Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     F : X_Proto.Reply.Fs.Child_Ptr := new X_Proto.Reply.Fs.Child_Type (X_Proto.Reply.Fs.Child_Pad);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                   Find_Tag => Prev_Tag,
                                                   Pad_V    => F.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     F : X_Proto.Reply.Fs.Child_Ptr := new X_Proto.Reply.Fs.Child_Type (X_Proto.Reply.Fs.Child_Documentation);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Documentation_V      => F.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     F : X_Proto.Reply.Fs.Child_Ptr := new X_Proto.Reply.Fs.Child_Type (X_Proto.Reply.Fs.Child_List);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   List_V               => F.L'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Expression_Field_Type_Id =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     C : X_Proto.Expression_Field.Fs.Child_Ptr := new X_Proto.Expression_Field.Fs.Child_Type (X_Proto.Expression_Field.Fs.Child_Operation);
                  begin
                     Append_Child (Prev_Tag.Expression_Field_V.all, C);
                     Is_Success := True;
                     Insert (new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                   Find_Tag             => Prev_Tag,
                                                   Op_V                 => C.Op'Access));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Field |
                 Tag_Id.X_Id_Kind_Type_Id |
                 Tag_Id.Kind |
                 Tag_Id.Type_Definition_Type_Id |
                 Tag_Id.Enum_Pad |
                 Tag_Id.Value_Type_Id |
                 Tag_Id.Bit_Type_Id |
                 Tag_Id.Field_Reference |
                 Tag_Id.See_Type_Id |
                 Tag_Id.Event_Copy_Type_Id |
                 Tag_Id.Error_Copy_Type_Id |
                 Tag_Id.Value_Param_Type_Id |
                 Tag_Id.Example_Type_Id =>
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Error, tag name is " & Tag_Name);
         end case;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : SXML.DL.T;
                           Error_Message               : out SXML.Error_Message_T;
                           Is_Success                  : out Boolean)
      is
         Current_Tag : constant Current_Tag_Access_Type := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if Current_Tag = null then
            Is_Success := False;
            Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (Parent_Tags_And_Current_Tag));
            return;
         end if;

         Is_Success := True;
         case Current_Tag.Kind_Id is
            when Tag_Id.Xcb_Type_Id =>
               if Attribute_Name = Tag_Xcb_Attribute_Header then
                  Is_Success := True;
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Header (Current_Tag.Xcb_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Struct =>
               if Attribute_Name = Tag_Struct_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Struct_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Field =>
               if Attribute_Name = Tag_Field_Attribute_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Enum then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Enum (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Mask then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Alt_Enum then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Alt_Enum (Current_Tag.Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Kind_Type_Id =>
               if Attribute_Name = Tag_X_Id_Kind_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.X_Id_Kind_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_X_Id_Union =>
               if Attribute_Name = Tag_X_Id_Union_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.X_Id_Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Type_Definition_Type_Id =>
               if Attribute_Name = Tag_Type_Definition_Attribute_Old_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Old_Name (Current_Tag.Type_Definition_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Type_Definition_Attribute_New_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_New_Name (Current_Tag.Type_Definition_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Pad =>
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
                        Set_Bytes (Current_Tag.Pad_V.all, Positive (V));
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Enum =>
               if Attribute_Name = Tag_Enum_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Enum_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Item_Type_Id =>
               if Attribute_Name = Tag_Item_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Item_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.List_Type_Id =>
               if Attribute_Name = Tag_List_Attribute_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.List_V.all, V);
                  end;
               elsif Attribute_Name = Tag_List_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.List_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Op_Type_Id =>
               if Attribute_Name = Tag_Operation_Attribute_Op then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Op (Current_Tag.Op_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Type_Id =>
               if Attribute_Name = XML_Tag_Event_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Event_V.all, V);
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
                        Set_Number (Current_Tag.Event_V.all, Positive (V));
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_No_Sequence_Number then
                  if Attribute_Value = "true" then
                     Set_No_Sequence_Number (Current_Tag.Event_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_No_Sequence_Number (Current_Tag.Event_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Event_Attribute_XGE then
                  if Attribute_Value = "true" then
                     Set_XGE (Current_Tag.Event_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_XGE (Current_Tag.Event_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.See_Type_Id =>
               if Attribute_Name = XML_Tag_See_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.See_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_See_Attribute_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.See_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Copy_Type_Id =>
               if Attribute_Name = XML_Tag_Event_Copy_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Event_Copy_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Ref then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Ref (Current_Tag.Event_Copy_V.all, V);
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
                        Set_Number (Current_Tag.Event_Copy_V.all, V);
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Union_Type_Id =>
               if Attribute_Name = XML_Tag_Union_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error_Type_Id =>
               if Attribute_Name = XML_Tag_Error_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Error_V.all, V);
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
                        Set_Number (Current_Tag.Error_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Error_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error_Copy_Type_Id =>
               if Attribute_Name = XML_Tag_Error_Copy_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Error_Copy_V.all, V);
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
                        Set_Number (Current_Tag.Error_Copy_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Ref then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Ref (Current_Tag.Error_Copy_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Request_Type_Id =>
               if Attribute_Name = XML_Tag_Request_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Request_V.all, V);
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
                        Set_Op_Code (Current_Tag.Request_V.all, V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Combine_Adjacent then
                  if Attribute_Value = "true" then
                     Set_Shall_Combine_Adjacent (Current_Tag.Request_V.all, True);
                  elsif Attribute_Value = "false" then
                     Set_Shall_Combine_Adjacent (Current_Tag.Request_V.all, False);
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Value_Param_Type_Id =>
               if Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Kind (Current_Tag.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Name (Current_Tag.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_List_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_List_Name (Current_Tag.Value_Param_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Expression_Field_Type_Id =>
               if Attribute_Name = XML_Tag_Expression_Field_Attribute_Name then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Expression_Field_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Expression_Field_Attribute_Kind then
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Expression_Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Kind |
                 Tag_Id.Value_Type_Id |
                 Tag_Id.Bit_Type_Id |
                 Tag_Id.Field_Reference |
                 Tag_Id.Documentation_Type_Id |
                 Tag_Id.Reply_Type_Id |
                 Tag_Id.Example_Type_Id =>
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
         end case;
      end Attribute;

      procedure End_Tag (Tag_Name      : String;
                         Parent_Tags   : SXML.DL.T;
                         Error_Message : out SXML.Error_Message_T;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.T;
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
                         Parent_Tags   : SXML.DL.T;
                         Error_Message : out SXML.Error_Message_T;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.T;

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
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Both Current_Tag and Prev_Tag was null for end tag '" & Tag_Name & "' and value '" & Tag_Value & "'");
                  return;
               end if;

               case Prev_Tag.Kind_Id is
                  when Tag_Id.Item_Type_Id =>
                     declare
                        V : X_Proto.Value_Type;
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
                              when X_Proto.Item.Fs.Not_Specified =>
                                 if Tag_Name = XML_Tag_Value then
                                    Set_Kind_Id (Prev_Tag.Item_V.all, X_Proto.Item.Fs.Specified_As_Value);
                                    Set_Value (Prev_Tag.Item_V.all, V);
                                 elsif Tag_Name = Tag_Bit then
                                    Set_Kind_Id (Prev_Tag.Item_V.all, X_Proto.Item.Fs.Specified_As_Bit);
                                    Set_Bit (Prev_Tag.Item_V.all, X_Proto.Item.Fs.Bit_Type (V));
                                 else
                                    Is_Success := False;
                                    Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", unknown end tag '" & Tag_Name & "'");
                                 end if;
                              when X_Proto.Item.Fs.Specified_As_Value |
                                   X_Proto.Item.Fs.Specified_As_Bit =>
                                 Is_Success := False;
                                 Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", value already initialized for end tag " & Tag_Name & ", kind id " & Kind_Id (Prev_Tag.Item_V.all)'Img);
                           end case;
                        end if;
                     end;
                  when Tag_Id.List_Type_Id =>
                     if Tag_Name = Tag_Field_Reference then
                        declare
                           L : constant X_Proto.List.Fs.Member_Ptr := new X_Proto.List.Fs.Member_Type (X_Proto.List.Fs.List_Member_Kind_Field_Reference);
                        begin
                           Initialize (L.Field_Reference, Tag_Value);
                           Append_Member (Prev_Tag.List_V.all, L);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when Tag_Id.Documentation_Type_Id =>
                     if Tag_Name = XML_Tag_Brief then
                        declare
                           V : X_Proto.Large_Bounded_String.T;
                        begin
                           Initialize (V, Tag_Value);
                           Set_Brief_Description (Prev_Tag.Documentation_V.all, V);
                        end;
                     elsif Tag_Name = XML_Tag_Description then
                        declare
                           V : X_Proto.Large_Bounded_String.T;
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

            case Current_Tag.Kind_Id is
               when Tag_Id.Kind =>
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Kind.all, V);
                  end;
               when Tag_Id.Enum_Field =>
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Field_V.all, V);
                  end;
               when Tag_Id.Value_Type_Id =>
                  declare
                     V : X_Proto.Value_Type;
                     Has_Failed : Boolean;
                  begin
                     To_Int32 (Source     => Aida.String_T (Tag_Value),
                               Target     => Aida.Int32_T (V),
                               Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                     else
                        Current_Tag.Value_V.all := V;
                     end if;
                  end;
               when Tag_Id.Error_Type_Id =>
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Error_V.all, V);
                  end;
               when Tag_Id.Example_Type_Id =>
                  declare
                     V : X_Proto.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Example_V.all, V);
                  end;
               when Tag_Id.Op_Type_Id =>
                  if Tag_Name = Tag_Field_Reference then
                     declare
                        V : X_Proto.Field_Reference_Type;
                     begin
                        Initialize (V, Tag_Value);
                        Append_Member (Current_Tag.Op_V.all, new X_Proto.Operation.Fs.Member_Type'(Kind_Id         => X_Proto.Operation.Fs.Member_Kind_Field_Reference,
                                                                                                   Field_Reference => V));
                     end;
                  elsif Tag_Name = XML_Tag_Value then
                     declare
                        V : X_Proto.Value_Type;
                        Has_Failed : Boolean;
                     begin
                        To_Int32 (Source     => Aida.String_T (Tag_Value),
                                  Target     => Aida.Int32_T (V),
                                  Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           Append_Member (Current_Tag.Op_V.all, new X_Proto.Operation.Fs.Member_Type'(Kind_Id => X_Proto.Operation.Fs.Member_Kind_Value,
                                                                                                      Value   => V));
                        end if;
                     end;
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Current_Tag.Kind_Id'Img);
                  end if;
               when Tag_Id.Field_Reference =>
                  declare
                     V : X_Proto.Field_Reference_Type;
                  begin
                     Initialize (V, Tag_Value);
                     Current_Tag.Field_Reference.all := V;
                  end;
               when Tag_Id.Xcb_Type_Id |
                    Tag_Id.Enum_Struct |
                    Tag_Id.Bit_Type_Id |
                    Tag_Id.X_Id_Kind_Type_Id |
                    Tag_Id.Enum_X_Id_Union |
                    Tag_Id.Type_Definition_Type_Id |
                    Tag_Id.Enum_Pad |
                    Tag_Id.Enum_Enum |
                    Tag_Id.Item_Type_Id |
                    Tag_Id.List_Type_Id |
                    Tag_Id.Event_Type_Id |
                    Tag_id.Documentation_Type_Id |
                    Tag_Id.See_Type_Id |
                    Tag_Id.Event_Copy_Type_Id |
                    Tag_Id.Union_Type_Id |
                    Tag_Id.Error_Copy_Type_Id |
                    Tag_Id.Request_Type_Id |
                    Tag_Id.Value_Param_Type_Id |
                    Tag_Id.Reply_Type_Id |
                    Tag_Id.Expression_Field_Type_Id =>
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", but found unexpected end tag " & Tag_Name);
            end case;
         end if;
      end End_Tag;

      procedure Parse_X_Proto_XML_File is new SXML.Generic_Parse_XML_File (Start_Tag,
                                                                           Attribute,
                                                                           End_Tag,
                                                                           End_Tag);
   begin
--        Is_Success := False;
--        return;

      Parse_X_Proto_XML_File (Contents,
                              Error_Message,
                              Is_Success);
   end Parse;

end XML_File_Parser;
