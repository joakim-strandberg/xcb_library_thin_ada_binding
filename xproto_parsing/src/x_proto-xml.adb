with Ada.Text_IO;
with SXML.Generic_Parse_XML_File;
with GNAT.Source_Info;
with Std_String;
with BC.Indefinite_Unmanaged_Containers.Collections;
with BC.Containers.Maps.Unmanaged;
with Ada.Strings.Hash;
with Ada.Exceptions;

package body X_Proto.XML is

   use Struct.Fs.Member_Kind_Id;
   use Operation;

   use type Xcb.Ptr;

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

   type Current_Tag_Type;

   type Current_Tag_Access_Type is access all Current_Tag_Type;

   type Current_Tag_Type (Kind_Id : Tag_Id.Enumeration_Type) is record
      Find_Tag : Current_Tag_Access_Type := null;
      case Kind_Id is
         when Xcb_Type_Id             => Xcb_V             : Xcb.Ptr;
         when Enum_Struct             => Struct_V          : Struct.Ptr;
         when Enum_Field              => Field_V           : Field.Ptr;
         when X_Id_Kind_Type_Id       => X_Id_Kind_V       : X_Id.Ptr;
         when Enum_X_Id_Union         => X_Id_Union_V      : X_Id_Union.Ptr;
         when Kind                    => Kind              : Type_P.Ptr;
         when Type_Definition_Type_Id => Type_Definition_V : Type_Definition.Ptr;
         when Enum_Pad                => Pad_V             : Pad.Ptr;
         when Enum_Enum               => Enum_V            : Enum.Ptr;
         when Item_Type_Id            => Item_V            : Item.Ptr;
         when Value_Type_Id           => Value_V           : Value_Access_Type;
         when Bit_Type_Id             => Bit_V             : Item.Fs.Bit_Ptr;
         when List_Type_Id            => List_V            : List.Ptr;
         when Field_Reference         => Field_Reference   : Field_Reference_Access_Type;
         when Op_Type_Id              => Op_V              : Operation.Ptr;
         when Event_Type_Id           => Event_V           : Event.Ptr;
         when Documentation           => Documentation     : Documentation_Access_Type;
         when See                     => See               : See_Access_Type;
         when Event_Copy              => Event_Copy        : Event_Copy_Access_Type;
         when Union                   => Union             : Union_Access_Type;
         when Error                   => Error             : Error_Access_Type;
         when Error_Copy              => Error_Copy        : Error_Copy_Access_Type;
         when Request                 => Request           : Request_Access_Type;
         when Value_Param             => Value_Param       : Value_Param_Access_Type;
         when Reply                   => Reply             : Reply_Access_Type;
         when Example                 => Example           : Example_Access_Type;
         when Expression_Field        => Expression_Field  : Expression_Field_Access_Type;
      end case;
   end record;

   package Abstract_Current_Tag_Containers is new BC.Containers (Item => Current_Tag_Access_Type,
                                                                 "="  => "=");

   package Abstract_Current_Tag_Maps is new Abstract_Current_Tag_Containers.Maps (Key => SXML.DL.Collection,
                                                                                  "=" => SXML.DL."=");

   function Hash (Parent_And_Self_Tags : SXML.DL.Collection) return Natural is
      R : Ada.Containers.Hash_Type := 0;

      use type Ada.Containers.Hash_Type;
   begin
      if SXML.DL.Is_Empty (Parent_And_Self_Tags) then
         return 0;
      end if;

      declare
         Iter : SXML.String_Containers.Iterator'Class := SXML.DL.New_Iterator (Parent_And_Self_Tags);
      begin
         while not Iter.Is_Done loop
            R := R + Ada.Strings.Hash (Iter.Current_Item);
            Iter.Next;
         end loop;
      end;
      R := R mod Ada.Containers.Hash_Type (Natural'Last);
      return Natural (R);
   end Hash;


   package Unmanaged_Maps is new Abstract_Current_Tag_Maps.Unmanaged (Hash    => Hash,
                                                                      Buckets => 1000);

   procedure Parse (Contents      : String;
                    Xcb_V         : in out Xcb.Ptr;
                    Error_Message : out SXML.Error_Message_Type;
                    Is_Success    : out Boolean)
   is
      Parents_Including_Self_To_Current_Tag_Map : Unmanaged_Maps.Map;

      procedure Populate_Parents_Including_Self (Parents_Including_Self : in out SXML.DL.Collection;
                                                 Parents                : SXML.DL.Collection;
                                                 Tag_Name               : String) is
         Iter : SXML.String_Containers.Iterator'Class := Parents.New_Iterator;
      begin
         while not Iter.Is_Done loop
            Parents_Including_Self.Append (Iter.Current_Item);
            Iter.Next;
         end loop;
         Parents_Including_Self.Append (Tag_Name);
      end Populate_Parents_Including_Self;


      function Find_Tag (Key : SXML.DL.Collection) return Current_Tag_Access_Type is
      begin
         if Parents_Including_Self_To_Current_Tag_Map.Is_Bound (Key) then
            return Parents_Including_Self_To_Current_Tag_Map.Item_Of (Key);
         else
            return null;
         end if;
      end Find_Tag;

      function To_String (Tags : SXML.DL.Collection) return String is
         Iter : SXML.String_Containers.Iterator'Class := Tags.New_Iterator;
         R : Aida.Strings.Unbounded_String_Type;
      begin
         while not Iter.Is_Done loop
            R.Append (Iter.Current_Item & ", ");
            Iter.Next;
         end loop;

         return R.To_String;
      end To_String;

      procedure Start_Tag (Tag_Name      : String;
                           Parent_Tags   : SXML.DL.Collection;
                           Error_Message : out SXML.Error_Message_Type;
                           Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.Collection;

         Prev_Tag : Current_Tag_Access_Type := Find_Tag (Parent_Tags);
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         if Prev_Tag = null then
            if Tag_Name = Tag_Xcb then
               if Xcb_V /= null then
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Expected Xcb access to be null, tag name is " & Tag_Name);
                  return;
               end if;

               Xcb_V := new Xcb.T;
               Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                               I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Xcb_Type_Id,
                                                                                          Find_Tag => Find_Tag (Parent_Tags),
                                                                                          Xcb_V    => Xcb_V));
               Is_Success := True;
            else
               Is_Success := False;
               Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Expected " & Tag_Struct & ", but found " & Tag_Name);
            end if;
            return;
         end if;

         case Prev_Tag.Kind_Id is
            when Tag_Id.Xcb_Type_Id =>
               if Tag_Name = Tag_Struct then
                  declare
                     Struct_V : Struct.Ptr := new Struct.T;
                  begin
                     case Prev_Tag.Kind_Id is
                        when Tag_Id.Xcb_Type_Id =>
                           Prev_Tag.Xcb_V.Append_Struct (Struct_V);
                           Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                           I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Enum_Struct,
                                                                                                      Find_Tag             => Prev_Tag,
                                                                                                      Struct_V             => Struct_V));
                           Is_Success := True;
                        when others =>
                           Is_Success := False;
                           Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Expected " & Tag_Struct & ", but found " & Tag_Name);
                     end case;
                  end;
               elsif Tag_Name = Tag_X_Id_Kind then
                  declare
                     X_Id_Type_V : X_Id.Ptr := new X_Id.T;
                  begin
                     Prev_Tag.Xcb_V.Append_X_Id (X_Id_Type_V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id     => Tag_Id.X_Id_Kind_Type_Id,
                                                                                                Find_Tag    => Prev_Tag,
                                                                                                X_Id_Kind_V => X_Id_Type_V));
                  end;
               elsif Tag_Name = Tag_X_Id_Union then
                  declare
                     X_Id_Union_V : X_Id_Union.Ptr := new X_Id_Union.T;
                  begin
                     Prev_Tag.Xcb_V.Append_X_Id_Union (X_Id_Union_V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id      => Tag_Id.Enum_X_Id_Union,
                                                                                                Find_Tag     => Prev_Tag,
                                                                                                X_Id_Union_V => X_Id_Union_V));
                  end;
               elsif Tag_Name = Tag_Type_Definition then
                  declare
                     TD : Type_Definition.Ptr := new Type_Definition.T;
                  begin
                     Prev_Tag.Xcb_V.Append_Type_Definition (TD);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id           => Tag_Id.Type_Definition_Type_Id,
                                                                                                Find_Tag          => Prev_Tag,
                                                                                                Type_Definition_V => TD));
                  end;
               elsif Tag_Name = Tag_Enum then
                  declare
                     Enum_V : Enum.Ptr := new Enum.T;
                  begin
                     Prev_Tag.Xcb_V.Append_Enum (Enum_V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Enum,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Enum_V   => Enum_V));
                  end;
               elsif Tag_Name = XML_Tag_Event then
                  declare
                     E : Event.Ptr := new Event.T;
                  begin
                     Prev_Tag.Xcb_V.Append_Event (E);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Event_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Event_V  => E));
                  end;
               elsif Tag_Name = XML_Tag_Event_Copy then
                  declare
                     EC : Event_Copy_Access_Type := new Event_Copy_Type;
                  begin
                     Prev_Tag.Xcb_V.Append_Event_Copy (EC);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Event_Copy,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Event_Copy           => EC));
                  end;
               elsif Tag_Name = XML_Tag_Union then
                  declare
                     U : Union_Access_Type := new Union_Type;
                  begin
                     Prev_Tag.Xcb_V.Append_Union (U);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Union,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Union                => U));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     E : Error_Access_Type := new Error_Type;
                  begin
                     Prev_Tag.Xcb_V.Append_Error (E);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Error,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Error                => E));
                  end;
               elsif Tag_Name = XML_Tag_Error_Copy then
                  declare
                     E : Error_Copy_Access_Type := new Error_Copy_Type;
                  begin
                     Prev_Tag.Xcb_V.Append_Error_Copy (E);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Error_Copy,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Error_Copy           => E));
                  end;
               elsif Tag_Name = XML_Tag_Request then
                  declare
                     R : Request_Access_Type := new Request_Type;
                  begin
                     Prev_Tag.Xcb_V.Append_Request (R);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Request,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Request              => R));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Struct =>
               if Tag_Name = Tag_Field then
                  declare
                     F : Struct.Fs.Member_Ptr := new Struct.Fs.Member_Type (Field_Member);
                  begin
                     Prev_Tag.Struct_V.Append_Member (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : Struct.Fs.Member_Ptr := new Struct.Fs.Member_Type (Pad_Member);
                  begin
                     Prev_Tag.Struct_V.Append_Member (P);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : Struct.Fs.Member_Ptr := new Struct.Fs.Member_Type (List_Member);
                  begin
                     Prev_Tag.Struct_V.Append_Member (L);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.List_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_X_Id_Union =>
               if Tag_Name = Tag_Kind then
                  declare
                     Kind : Type_P.Ptr := new Type_P.T;
                  begin
                     Prev_Tag.X_Id_Union_V.Append_Kind (Kind);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Kind,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Kind     => Kind));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Enum =>
               if Tag_Name = Tag_Item then
                  declare
                     Item_V : Item.Ptr := new Item.T;
                  begin
                     Prev_Tag.Enum_V.Append_Item (Item_V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Item_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Item_V   => Item_V));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : Documentation_Access_Type := new Documentation_Type;
                  begin
                     Prev_Tag.Enum_V.Append_Documentation (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Documentation        => D));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Item_Type_Id =>
               if Tag_Name = XML_Tag_Value then
                  Is_Success := True;
               elsif Tag_Name = Tag_Bit then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.List_Type_Id =>
               if Tag_Name = Tag_Field_Reference then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Operation then
                  declare
                     Operation : List.Fs.Member_Ptr := new List.Fs.Member_Type (List.Fs.List_Member_Kind_Operation);
                  begin
                     Prev_Tag.List_V.Append_Member (Operation);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Op_V                 => Operation.Operation'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : List.Fs.Member_Ptr := new List.Fs.Member_Type (List.Fs.List_Member_Kind_Value);
                  begin
                     Prev_Tag.List_V.Append_Member (V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Value_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Op_Type_Id =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     V : Operation.Fs.Member_Ptr := new Operation.Fs.Member_Type (Operation.Fs.Member_Operation);
                  begin
                     V.Operation := new Operation.T;
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Op_V                 => V.Operation));
                  end;
               elsif Tag_Name = Tag_Field_Reference then
                  declare
                     V : Operation.Fs.Member_Ptr := new Operation.Fs.Member_Type (Operation.Fs.Member_Kind_Field_Reference);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Field_Reference,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Field_Reference      => V.Field_Reference'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : Operation.Fs.Member_Ptr := new Operation.Fs.Member_Type (Operation.Fs.Member_Kind_Value);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Value_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Value_V  => V.Value'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Event_Type_Id =>
               if Tag_Name = Tag_Field then
                  declare
                     F : Event.Fs.Member_Ptr := new Event.Fs.Member_Type (Event.Fs.Event_Member_Field);
                  begin
                     Prev_Tag.Event_V.Append_Member (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Enum_Field,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Field_V              => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : Event.Fs.Member_Ptr := new Event.Fs.Member_Type (Event.Fs.Event_Member_Pad);
                  begin
                     Prev_Tag.Event_V.Append_Member (P);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : Event.Fs.Member_Ptr := new Event.Fs.Member_Type (Event.Fs.Event_Member_Doc);
                  begin
                     Prev_Tag.Event_V.Append_Member (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Documentation        => D.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : Event.Fs.Member_Ptr := new Event.Fs.Member_Type (Event.Fs.Event_Member_List);
                  begin
                     Prev_Tag.Event_V.Append_Member (L);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                List_V               => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Documentation =>
               if Tag_Name = Tag_Field then
                  declare
                     D : Documentation_Member_Access_Type := new Documentation_Member_Type (Documentation_Member_Field);
                  begin
                     Prev_Tag.Documentation.Members.Append (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Field_V  => D.F'Access));
                  end;
               elsif Tag_Name = XML_Tag_See then
                  declare
                     D : Documentation_Member_Access_Type := new Documentation_Member_Type (Documentation_Member_See);
                  begin
                     Prev_Tag.Documentation.Members.Append (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.See,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                See                  => D.S'Access));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     D : Documentation_Member_Access_Type := new Documentation_Member_Type (Documentation_Member_Error);
                  begin
                     Prev_Tag.Documentation.Members.Append (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Error,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Error                => D.E'Access));
                  end;
               elsif Tag_Name = XML_Tag_Example then
                  declare
                     D : Documentation_Member_Access_Type := new Documentation_Member_Type (Documentation_Member_Example);
                  begin
                     Prev_Tag.Documentation.Members.Append (D);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Example,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Example              => D.Ex'Access));
                  end;
               elsif Tag_Name = XML_Tag_Brief then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Description then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Union =>
               if Tag_Name = Tag_List then
                  declare
                     L : Union_Child_Access_Type := new Union_Child_Type (Union_Child_List);
                  begin
                     Prev_Tag.Union.Children.Append (L);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.List_Type_Id,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                List_V   => L.L'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Error =>
               if Tag_Name = Tag_Field then
                  declare
                     F : Error_Child_Access_Type := new Error_Child_Type (Error_Child_Field);
                  begin
                     Prev_Tag.Error.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : Error_Child_Access_Type := new Error_Child_Type (Error_Child_Pad);
                  begin
                     Prev_Tag.Error.Children.Append (P);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Pad_V    => P.P'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Request =>
               if Tag_Name = Tag_Field then
                  declare
                     F : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Field);
                  begin
                     Prev_Tag.Request.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Pad);
                  begin
                     Prev_Tag.Request.Children.Append (P);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Pad_V    => P.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Value_Param then
                  declare
                     V : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Value_Param);
                  begin
                     Prev_Tag.Request.Children.Append (V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Value_Param,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Value_Param          => V.V'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     V : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Documentation);
                  begin
                     Prev_Tag.Request.Children.Append (V);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Documentation        => V.D'Access));
                  end;
               elsif Tag_Name = XML_Tag_Reply then
                  declare
                     R : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Reply);
                  begin
                     Prev_Tag.Request.Children.Append (R);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Reply,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Reply                => R.R'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     R : Request_Child_Access_Type := new Request_Child_Type (Request_Child_List);
                  begin
                     Prev_Tag.Request.Children.Append (R);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                List_V               => R.L'Access));
                  end;
               elsif Tag_Name = XML_Tag_Expression_Field then
                  declare
                     R : Request_Child_Access_Type := new Request_Child_Type (Request_Child_Expression_Field);
                  begin
                     Prev_Tag.Request.Children.Append (R);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Expression_Field,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Expression_Field     => R.EF'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Reply =>
               if Tag_Name = Tag_Field then
                  declare
                     F : Reply_Child_Access_Type := new Reply_Child_Type (Reply_Child_Field);
                  begin
                     Prev_Tag.Reply.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Field,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Field_V  => F.F'Access));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     F : Reply_Child_Access_Type := new Reply_Child_Type (Reply_Child_Pad);
                  begin
                     Prev_Tag.Reply.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id  => Tag_Id.Enum_Pad,
                                                                                                Find_Tag => Prev_Tag,
                                                                                                Pad_V    => F.P'Access));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     F : Reply_Child_Access_Type := new Reply_Child_Type (Reply_Child_Documentation);
                  begin
                     Prev_Tag.Reply.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Documentation,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Documentation        => F.D'Access));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     F : Reply_Child_Access_Type := new Reply_Child_Type (Reply_Child_List);
                  begin
                     Prev_Tag.Reply.Children.Append (F);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.List_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                List_V               => F.L'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Expression_Field =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     C : Expression_Field_Child_Access_Type := new Expression_Field_Child_Type (Expression_Field_Child_Operation);
                  begin
                     Prev_Tag.Expression_Field.Children.Append (C);
                     Is_Success := True;
                     Parents_Including_Self_To_Current_Tag_Map.Bind (K => Parents_Including_Self,
                                                                     I => new Current_Tag_Type'(Kind_Id              => Tag_Id.Op_Type_Id,
                                                                                                Find_Tag             => Prev_Tag,
                                                                                                Op_V                 => C.Op'Access));
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum_Field |
                 Tag_Id.X_Id_Kind_Type_Id |
                 Tag_Id.Kind |
                 Tag_Id.Type_Definition_Type_Id |
                 Tag_Id.Enum_Pad |
                 Tag_Id.Value_Type_Id |
                 Tag_Id.Bit_Type_Id |
                 Tag_Id.Field_Reference |
                 Tag_Id.See |
                 Tag_Id.Event_Copy |
                 Tag_Id.Error_Copy |
                 Tag_Id.Value_Param |
                 Tag_Id.Example =>
                 Is_Success := False;
                 Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Error, tag name is " & Tag_Name);
         end case;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : SXML.DL.Collection;
                           Error_Message               : out SXML.Error_Message_Type;
                           Is_Success                  : out Boolean)
      is
         Current_Tag : Current_Tag_Access_Type := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if Current_Tag = null then
            Is_Success := False;
            Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (Parent_Tags_And_Current_Tag));
            return;
         end if;

         Is_Success := True;
         case Current_Tag.Kind_Id is
            when Tag_Id.Xcb_Type_Id =>
               if Attribute_Name = Tag_Xcb_Attribute_Header then
                  Is_Success := True;
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Xcb_V.Set_Header (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Struct =>
               if Attribute_Name = Tag_Struct_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Struct_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Field =>
               if Attribute_Name = Tag_Field_Attribute_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Field_V.Set_Kind (V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Field_V.Set_Name (V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Enum then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Field_V.Set_Enum (V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Mask then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Field_V.Set_Mask (V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Alt_Enum then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Field_V.Set_Alt_Enum (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Kind_Type_Id =>
               if Attribute_Name = Tag_X_Id_Kind_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.X_Id_Kind_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_X_Id_Union =>
               if Attribute_Name = Tag_X_Id_Union_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.X_Id_Union_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Type_Definition_Type_Id =>
               if Attribute_Name = Tag_Type_Definition_Attribute_Old_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Type_Definition_V.Set_Old_Name (V);
                  end;
               elsif Attribute_Name = Tag_Type_Definition_Attribute_New_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Type_Definition_V.Set_New_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Pad =>
               if Attribute_Name = Tag_Pad_Attribute_Bytes then
                  declare
                     V : Positive;

                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Pad_V.Set_Bytes (V);
                     end if;
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum_Enum =>
               if Attribute_Name = Tag_Enum_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Enum_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Item_Type_Id =>
               if Attribute_Name = Tag_Item_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Item_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.List_Type_Id =>
               if Attribute_Name = Tag_List_Attribute_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.List_V.Set_Kind (V);
                  end;
               elsif Attribute_Name = Tag_List_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.List_V.Set_Name (V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Op_Type_Id =>
               if Attribute_Name = Tag_Operation_Attribute_Op then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Set_Op (Current_Tag.Op_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Type_Id =>
               if Attribute_Name = XML_Tag_Event_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Event_V.Set_Name (V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_Number then
                  declare
                     V : Positive;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Event_V.Set_Number (V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Event_Attribute_No_Sequence_Number then
                  if Attribute_Value = "true" then
                     Current_Tag.Event_V.Set_No_Sequence_Number (True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Event_V.Set_No_Sequence_Number (False);
                  else
                     Is_Success := False;
                     Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               elsif Attribute_Name = XML_Tag_Event_Attribute_XGE then
                  if Attribute_Value = "true" then
                     Current_Tag.Event_V.Set_XGE (True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Event_V.Set_XGE (False);
                  else
                     Is_Success := False;
                     Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.See =>
               if Attribute_Name = XML_Tag_See_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.See.Name := (Exists => True,
                                              Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_See_Attribute_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.See.Kind := (Exists => True,
                                              Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Copy =>
               if Attribute_Name = XML_Tag_Event_Copy_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Event_Copy.Name := (Exists => True,
                                                     Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Ref then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Event_Copy.Ref := (Exists => True,
                                                    Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Number then
                  declare
                     V : Positive;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Event_Copy.Number := (Exists => True,
                                                          Value  => V);
                     end if;
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Union =>
               if Attribute_Name = XML_Tag_Union_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Union.Name := (Exists => True,
                                                Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error =>
               if Attribute_Name = XML_Tag_Error_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Error.Name := (Exists => True,
                                                Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Number then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Error.Number := (Exists => True,
                                                     Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Attribute_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                        Current_Tag.Error.Kind := (Exists => True,
                                                   Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error_Copy =>
               if Attribute_Name = XML_Tag_Error_Copy_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Error_Copy.Name := (Exists => True,
                                                     Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Number then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Error_Copy.Number := (Exists => True,
                                                          Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Error_Copy_Attribute_Ref then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Error_Copy.Ref := (Exists => True,
                                                    Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Request =>
               if Attribute_Name = XML_Tag_Request_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Request.Name := (Exists => True,
                                                  Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Op_Code then
                  declare
                     V : Natural;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Attribute_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Current_Tag.Request.Op_Code := (Exists => True,
                                                        Value  => V);
                     end if;
                  end;
               elsif Attribute_Name = XML_Tag_Request_Attribute_Combine_Adjacent then
                  if Attribute_Value = "true" then
                     Current_Tag.Request.Shall_Combine_Adjacent := (Exists => True,
                                                                    Value  => True);
                  elsif Attribute_Value = "false" then
                     Current_Tag.Request.Shall_Combine_Adjacent := (Exists => True,
                                                                    Value  => False);
                  else
                     Is_Success := False;
                     Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                  end if;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Value_Param =>
               if Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Value_Param.Mask_Kind := (Exists => True,
                                                           Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Value_Param.Mask_Name := (Exists => True,
                                                           Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_List_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Value_Param.List_Name := (Exists => True,
                                                           Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Expression_Field =>
               if Attribute_Name = XML_Tag_Expression_Field_Attribute_Name then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Expression_Field.Name := (Exists => True,
                                                           Value  => V);
                  end;
               elsif Attribute_Name = XML_Tag_Expression_Field_Attribute_Kind then
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Attribute_Value);
                     Current_Tag.Expression_Field.Kind := (Exists => True,
                                                           Value  => V);
                  end;
               else
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Kind |
                 Tag_Id.Value_Type_Id |
                 Tag_Id.Bit_Type_Id |
                 Tag_Id.Field_Reference |
                 Tag_Id.Documentation |
                 Tag_Id.Reply |
                 Tag_Id.Example =>
               Is_Success := False;
               Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
         end case;
      end Attribute;

      procedure End_Tag (Tag_Name      : String;
                         Parent_Tags   : SXML.DL.Collection;
                         Error_Message : out SXML.Error_Message_Type;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.Collection;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self => Parents_Including_Self,
                                          Parents                => Parent_Tags,
                                          Tag_Name               => Tag_Name);

         begin
            Parents_Including_Self_To_Current_Tag_Map.Unbind (K => Parents_Including_Self);
         exception
            when Unknown_Exception : BC.Not_Found =>
               Is_Success := False;
               Error_Message.Initialize (GNAT.Source_Info.Source_Location & Ada.Exceptions.Exception_Information(Unknown_Exception));
               return;
         end;

         Is_Success := True;
      end End_Tag;

      procedure End_Tag (Tag_Name      : String;
                         Tag_Value     : String;
                         Parent_Tags   : SXML.DL.Collection;
                         Error_Message : out SXML.Error_Message_Type;
                         Is_Success    : out Boolean)
      is
         Parents_Including_Self : SXML.DL.Collection;

         Current_Tag : Current_Tag_Access_Type;
      begin
         Populate_Parents_Including_Self (Parents_Including_Self, Parent_Tags, Tag_Name);

         Current_Tag := Find_Tag (Parents_Including_Self);

         Is_Success := True;

         if Current_Tag = null then
            declare
               Prev_Tag : Current_Tag_Access_Type := Find_Tag (Parent_Tags);
            begin
               if Prev_Tag = null then
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & "Both Current_Tag and Prev_Tag was null for end tag '" & Tag_Name & "' and value '" & Tag_Value & "'");
                  return;
               end if;

               case Prev_Tag.Kind_Id is
                  when Tag_Id.Item_Type_Id =>
                     declare
                        V : Value_Type;
                        Has_Failed : Boolean;
                     begin
                        Std_String.To_Integer (Source     => Tag_Value,
                                               Target     => Integer (V),
                                               Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           case Prev_Tag.Item_V.Kind_Id is
                              when Item.Fs.Not_Specified =>
                                 if Tag_Name = XML_Tag_Value then
                                    Prev_Tag.Item_V.Set_Kind_Id (Item.Fs.Specified_As_Value);
                                    Prev_Tag.Item_V.Set_Value (V);
                                 elsif Tag_Name = Tag_Bit then
                                    Prev_Tag.Item_V.Set_Kind_Id (Item.Fs.Specified_As_Bit);
                                    Prev_Tag.Item_V.Set_Bit (Item.Fs.Bit_Type (V));
                                 else
                                    Is_Success := False;
                                    Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", unknown end tag '" & Tag_Name & "'");
                                 end if;
                              when Item.Fs.Specified_As_Value |
                                   Item.Fs.Specified_As_Bit =>
                                 Is_Success := False;
                                 Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", value already initialized for end tag " & Tag_Name & ", kind id " & Prev_Tag.Item_V.Kind_Id'Img);
                           end case;
                        end if;
                     end;
                  when Tag_Id.List_Type_Id =>
                     if Tag_Name = Tag_Field_Reference then
                        declare
                           V : Aida.Strings.Unbounded_String_Type;
                        begin
                           V.Initialize (Tag_Value);
                           Prev_Tag.List_V.Append_Member (new List.Fs.Member_Type'(Kind_Id         => List.Fs.List_Member_Kind_Field_Reference,
                                                                                   Field_Reference => V));
                        end;
                     else
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when Tag_Id.Documentation =>
                     if Tag_Name = XML_Tag_Brief then
                        declare
                           V : Aida.Strings.Unbounded_String_Type;
                        begin
                           V.Initialize (Tag_Value);
                           Prev_Tag.Documentation.Brief_Description := (Exists => True,
                                                                        Value  => V);
                        end;
                     elsif Tag_Name = XML_Tag_Description then
                        declare
                           V : Aida.Strings.Unbounded_String_Type;
                        begin
                           V.Initialize (Tag_Value);
                           Prev_Tag.Documentation.Description := (Exists => True,
                                                                  Value  => V);
                        end;
                     else
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when others =>
                     Is_Success := False;
                     Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "'");
               end case;
            end;
         else
            begin
               Parents_Including_Self_To_Current_Tag_Map.Unbind (K => Parents_Including_Self);
            exception
               when Unknown_Exception : BC.Not_Found =>
                  Is_Success := False;
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & Ada.Exceptions.Exception_Information(Unknown_Exception));
                  return;
            end;

            case Current_Tag.Kind_Id is
               when Tag_Id.Kind =>
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Tag_Value);
                     Current_Tag.Kind.Set_Value (V);
                  end;
               when Tag_Id.Enum_Field =>
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Tag_Value);
                     Current_Tag.Field_V.Set_Value (V);
                  end;
               when Tag_Id.Value_Type_Id =>
                  declare
                     V : Value_Type;
                     Has_Failed : Boolean;
                  begin
                     Std_String.To_Integer (Source     => Tag_Value,
                                            Target     => Integer (V),
                                            Has_Failed => Has_Failed);

                     if Has_Failed then
                        Is_Success := False;
                        Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                     else
                        Current_Tag.Value_V.all := V;
                     end if;
                  end;
               when Tag_Id.Error =>
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Tag_Value);
                     Current_Tag.Error.Value := (Exists => True,
                                                 Value  => V);
                  end;
               when Tag_Id.Example =>
                  declare
                     V : Aida.Strings.Unbounded_String_Type;
                  begin
                     V.Initialize (Tag_Value);
                     Current_Tag.Example.Value := (Exists => True,
                                                   Value  => V);
                  end;
               when Tag_Id.Op_Type_Id =>
                  if Tag_Name = Tag_Field_Reference then
                     declare
                        V : Field_Reference_Type;
                     begin
                        V.Initialize (Tag_Value);
                        Append_Member (Current_Tag.Op_V.all, new Operation.Fs.Member_Type'(Kind_Id         => Operation.Fs.Member_Kind_Field_Reference,
                                                                                           Field_Reference => V));
                     end;
                  elsif Tag_Name = XML_Tag_Value then
                     declare
                        V : Value_Type;
                        Has_Failed : Boolean;
                     begin
                        Std_String.To_Integer (Source     => Tag_Value,
                                               Target     => Integer (V),
                                               Has_Failed => Has_Failed);

                        if Has_Failed then
                           Is_Success := False;
                           Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                        else
                           Append_Member (Current_Tag.Op_V.all, new Operation.Fs.Member_Type'(Kind_Id => Operation.Fs.Member_Kind_Value,
                                                                                              Value   => V));
                        end if;
                     end;
                  else
                     Is_Success := False;
                     Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Current_Tag.Kind_Id'Img);
                  end if;
               when Tag_Id.Field_Reference =>
                  declare
                     V : Field_Reference_Type;
                  begin
                     V.Initialize (Tag_Value);
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
                  Error_Message.Initialize (GNAT.Source_Info.Source_Location & ", but found unexpected end tag " & Tag_Name);
            end case;
         end if;
      end End_Tag;

      procedure Parse_X_Proto_XML_File is new SXML.Generic_Parse_XML_File (Start_Tag,
                                                                           Attribute,
                                                                           End_Tag,
                                                                           End_Tag);
   begin
      Parse_X_Proto_XML_File (Contents,
                              Error_Message,
                              Is_Success);
   end Parse;

end X_Proto.XML;
