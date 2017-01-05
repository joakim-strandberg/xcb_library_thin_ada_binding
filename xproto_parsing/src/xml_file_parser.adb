with Ada.Text_IO;
with Aida.XML.Parse_XML_File;
with Aida.Containers.Bounded_Hash_Map;
with GNAT.Source_Info;
with Ada.Exceptions;
with X_Proto_XML.Allocators;

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
   use X_Proto_XML.Allocators;

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
         when Bit              => Bit_V              : X_Proto_XML.Item.Fs.Bit_Ptr;
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

   package Allocators is

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Xcb        : X_Proto_XML.Xcb.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Struct     : X_Proto_XML.Struct.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                X_Id       : X_Proto_XML.X_Id.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                X_Id_Union : X_Proto_XML.X_Id_Union.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag      : Current_Tag_Access_Type;
                                Type_Definition : X_Proto_XML.Type_Definition.Ptr;
                                Subpool         : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Enum       : X_Proto_XML.Enum.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Event      : X_Proto_XML.Event.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Event_Copy : X_Proto_XML.Event_Copy.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Union      : X_Proto_XML.Union.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Error      : X_Proto_XML.Error.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Error_Copy : X_Proto_XML.Error_Copy.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Request    : X_Proto_XML.Request.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Field      : X_Proto_XML.Field.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Pad        : X_Proto_XML.Pad.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                List       : X_Proto_XML.List.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Kind       : X_Proto_XML.Type_P.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Item       : X_Proto_XML.Item.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag    : Current_Tag_Access_Type;
                                Documentation : X_Proto_XML.Documentation.Ptr;
                                Subpool       : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Operation  : X_Proto_XML.Operation.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Value      : X_Proto_XML.Value_Access_Type;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag      : Current_Tag_Access_Type;
                                Field_Reference : X_Proto_XML.Field_Reference_Access_Type;
                                Subpool         : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                See        : X_Proto_XML.See.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Example    : X_Proto_XML.Example.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag  : Current_Tag_Access_Type;
                                Value_Param : X_Proto_XML.Value_Param.Ptr;
                                Subpool     : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Reply      : X_Proto_XML.Reply.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

      function New_Current_Tag (Parent_Tag       : Current_Tag_Access_Type;
                                Expression_Field : X_Proto_XML.Expression_Field.Ptr;
                                Subpool          : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type;

   end Allocators;

   package body Allocators is

      subtype CT_Xcb_T is Current_Tag_T (Tag_Id.Xcb);

      type CT_Xcb_Ptr is access all CT_Xcb_T;

      function New_CT_Xcb is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Xcb_T,
                                                             Allocation_Type_Access => CT_Xcb_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Xcb        : X_Proto_XML.Xcb.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Xcb_Ptr := New_CT_Xcb (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Xcb_V := Xcb;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Struct_T is Current_Tag_T (Tag_Id.Struct);

      type CT_Struct_Ptr is access all CT_Struct_T;

      function New_CT_Struct is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Struct_T,
                                                             Allocation_Type_Access => CT_Struct_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Struct     : X_Proto_XML.Struct.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Struct_Ptr := New_CT_Struct (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Struct_V := Struct;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_X_Id_T is Current_Tag_T (Tag_Id.X_Id_Kind);

      type CT_X_Id_Ptr is access all CT_X_Id_T;

      function New_CT_X_Id is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_X_Id_T,
                                                             Allocation_Type_Access => CT_X_Id_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                X_Id       : X_Proto_XML.X_Id.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_X_Id_Ptr := New_CT_X_Id (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.X_Id_Kind_V := X_Id;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_X_Id_Union_T is Current_Tag_T (Tag_Id.X_Id_Union);

      type CT_X_Id_Union_Ptr is access all CT_X_Id_Union_T;

      function New_CT_X_Id_Union is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_X_Id_Union_T,
                                                             Allocation_Type_Access => CT_X_Id_Union_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                X_Id_Union : X_Proto_XML.X_Id_Union.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_X_Id_Union_Ptr := New_CT_X_Id_Union (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.X_Id_Union_V := X_Id_Union;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Type_Definition_T is Current_Tag_T (Tag_Id.Type_Definition);

      type CT_Type_Definition_Ptr is access all CT_Type_Definition_T;

      function New_CT_Type_Definition is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Type_Definition_T,
                                                             Allocation_Type_Access => CT_Type_Definition_Ptr);

      function New_Current_Tag (Parent_Tag      : Current_Tag_Access_Type;
                                Type_Definition : X_Proto_XML.Type_Definition.Ptr;
                                Subpool         : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Type_Definition_Ptr := New_CT_Type_Definition (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Type_Definition_V := Type_Definition;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Enum_T is Current_Tag_T (Tag_Id.Enum);

      type CT_Enum_Ptr is access all CT_Enum_T;

      function New_CT_Enum is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Enum_T,
                                                             Allocation_Type_Access => CT_Enum_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Enum       : X_Proto_XML.Enum.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Enum_Ptr := New_CT_Enum (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Enum_V := Enum;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Event_T is Current_Tag_T (Tag_Id.Event);

      type CT_Event_Ptr is access all CT_Event_T;

      function New_CT_Event is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Event_T,
                                                             Allocation_Type_Access => CT_Event_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Event      : X_Proto_XML.Event.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Event_Ptr := New_CT_Event (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Event_V := Event;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Event_Copy_T is Current_Tag_T (Tag_Id.Event_Copy);

      type CT_Event_Copy_Ptr is access all CT_Event_Copy_T;

      function New_CT_Event_Copy is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Event_Copy_T,
                                                             Allocation_Type_Access => CT_Event_Copy_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Event_Copy : X_Proto_XML.Event_Copy.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Event_Copy_Ptr := New_CT_Event_Copy (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Event_Copy_V := Event_Copy;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Union_T is Current_Tag_T (Tag_Id.Union);

      type CT_Union_Ptr is access all CT_Union_T;

      function New_CT_Union is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Union_T,
                                                             Allocation_Type_Access => CT_Union_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Union      : X_Proto_XML.Union.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Union_Ptr := New_CT_Union (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Union_V := Union;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Error_T is Current_Tag_T (Tag_Id.Error);

      type CT_Error_Ptr is access all CT_Error_T;

      function New_CT_Error is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Error_T,
                                                             Allocation_Type_Access => CT_Error_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Error      : X_Proto_XML.Error.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Error_Ptr := New_CT_Error (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Error_V := Error;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Error_Copy_T is Current_Tag_T (Tag_Id.Error_Copy);

      type CT_Error_Copy_Ptr is access all CT_Error_Copy_T;

      function New_CT_Error_Copy is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Error_Copy_T,
                                                             Allocation_Type_Access => CT_Error_Copy_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Error_Copy : X_Proto_XML.Error_Copy.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Error_Copy_Ptr := New_CT_Error_Copy (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Error_Copy_V := Error_Copy;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Request_T is Current_Tag_T (Tag_Id.Request);

      type CT_Request_Ptr is access all CT_Request_T;

      function New_CT_Request is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Request_T,
                                                             Allocation_Type_Access => CT_Request_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Request    : X_Proto_XML.Request.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Request_Ptr := New_CT_Request (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Request_V := Request;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Field_T is Current_Tag_T (Tag_Id.Field);

      type CT_Field_Ptr is access all CT_Field_T;

      function New_CT_Field is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Field_T,
                                                             Allocation_Type_Access => CT_Field_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Field      : X_Proto_XML.Field.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Field_Ptr := New_CT_Field (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Field_V := Field;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Pad_T is Current_Tag_T (Tag_Id.Pad);

      type CT_Pad_Ptr is access all CT_Pad_T;

      function New_CT_Pad is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Pad_T,
                                                             Allocation_Type_Access => CT_Pad_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Pad        : X_Proto_XML.Pad.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Pad_Ptr := New_CT_Pad (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Pad_V := Pad;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_List_T is Current_Tag_T (Tag_Id.List);

      type CT_List_Ptr is access all CT_List_T;

      function New_CT_List is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_List_T,
                                                             Allocation_Type_Access => CT_List_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                List       : X_Proto_XML.List.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_List_Ptr := New_CT_List (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.List_V := List;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Kind_T is Current_Tag_T (Tag_Id.Kind);

      type CT_Kind_Ptr is access all CT_Kind_T;

      function New_CT_Kind is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Kind_T,
                                                             Allocation_Type_Access => CT_Kind_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Kind       : X_Proto_XML.Type_P.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Kind_Ptr := New_CT_Kind (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Kind := Kind;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Item_T is Current_Tag_T (Tag_Id.Item);

      type CT_Item_Ptr is access all CT_Item_T;

      function New_CT_Item is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Item_T,
                                                             Allocation_Type_Access => CT_Item_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Item       : X_Proto_XML.Item.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Item_Ptr := New_CT_Item (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Item_V := Item;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Documentation_T is Current_Tag_T (Tag_Id.Documentation);

      type CT_Documentation_Ptr is access all CT_Documentation_T;

      function New_CT_Documentation is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Documentation_T,
                                                             Allocation_Type_Access => CT_Documentation_Ptr);

      function New_Current_Tag (Parent_Tag    : Current_Tag_Access_Type;
                                Documentation : X_Proto_XML.Documentation.Ptr;
                                Subpool       : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Documentation_Ptr := New_CT_Documentation (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Documentation_V := Documentation;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Operation_T is Current_Tag_T (Tag_Id.Op);

      type CT_Operation_Ptr is access all CT_Operation_T;

      function New_CT_Operation is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Operation_T,
                                                             Allocation_Type_Access => CT_Operation_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Operation  : X_Proto_XML.Operation.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Operation_Ptr := New_CT_Operation (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Op_V := Operation;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Value_T is Current_Tag_T (Tag_Id.Value);

      type CT_Value_Ptr is access all CT_Value_T;

      function New_CT_Value is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Value_T,
                                                             Allocation_Type_Access => CT_Value_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Value      : X_Proto_XML.Value_Access_Type;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Value_Ptr := New_CT_Value (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Value_V := Value;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Field_Reference_T is Current_Tag_T (Tag_Id.Field_Reference);

      type CT_Field_Reference_Ptr is access all CT_Field_Reference_T;

      function New_CT_Field_Reference is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Field_Reference_T,
                                                             Allocation_Type_Access => CT_Field_Reference_Ptr);

      function New_Current_Tag (Parent_Tag      : Current_Tag_Access_Type;
                                Field_Reference : X_Proto_XML.Field_Reference_Access_Type;
                                Subpool         : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Field_Reference_Ptr := New_CT_Field_Reference (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Field_Reference := Field_Reference;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_See_T is Current_Tag_T (Tag_Id.See);

      type CT_See_Ptr is access all CT_See_T;

      function New_CT_See is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_See_T,
                                                             Allocation_Type_Access => CT_See_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                See        : X_Proto_XML.See.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_See_Ptr := New_CT_See (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.See_V := See;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Example_T is Current_Tag_T (Tag_Id.Example);

      type CT_Example_Ptr is access all CT_Example_T;

      function New_CT_Example is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Example_T,
                                                             Allocation_Type_Access => CT_Example_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Example    : X_Proto_XML.Example.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Example_Ptr := New_CT_Example (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Example_V := Example;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Value_Param_T is Current_Tag_T (Tag_Id.Value_Param);

      type CT_Value_Param_Ptr is access all CT_Value_Param_T;

      function New_CT_Value_Param is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Value_Param_T,
                                                             Allocation_Type_Access => CT_Value_Param_Ptr);

      function New_Current_Tag (Parent_Tag  : Current_Tag_Access_Type;
                                Value_Param : X_Proto_XML.Value_Param.Ptr;
                                Subpool     : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Value_Param_Ptr := New_CT_Value_Param (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Value_Param_V := Value_Param;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Reply_T is Current_Tag_T (Tag_Id.Reply);

      type CT_Reply_Ptr is access all CT_Reply_T;

      function New_CT_Reply is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Reply_T,
                                                             Allocation_Type_Access => CT_Reply_Ptr);

      function New_Current_Tag (Parent_Tag : Current_Tag_Access_Type;
                                Reply      : X_Proto_XML.Reply.Ptr;
                                Subpool    : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Reply_Ptr := New_CT_Reply (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Reply_V := Reply;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

      subtype CT_Expression_Field_T is Current_Tag_T (Tag_Id.Expression_Field);

      type CT_Expression_Field_Ptr is access all CT_Expression_Field_T;

      function New_CT_Expression_Field is
        new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => CT_Expression_Field_T,
                                                             Allocation_Type_Access => CT_Expression_Field_Ptr);

      function New_Current_Tag (Parent_Tag       : Current_Tag_Access_Type;
                                Expression_Field : X_Proto_XML.Expression_Field.Ptr;
                                Subpool          : Bounded_Dynamic_Pools.Scoped_Subpool) return Current_Tag_Access_Type
      is
         T : constant CT_Expression_Field_Ptr := New_CT_Expression_Field (Subpool);
      begin
         T.Find_Tag := Parent_Tag;
         T.Expression_Field_V := Expression_Field;
         return Current_Tag_Access_Type (T);
      end New_Current_Tag;

   end Allocators;

   use Allocators;

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

   Parents_Including_Self_To_Current_Tag_Map : List_Of_Tag_Names_To_Current_Tag_Maps.T;

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    Subpool       : in out Bounded_Dynamic_Pools.Scoped_Subpool;
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

      function Find_Tag (Key : Aida.XML.DL.T) return Current_Tag_Access_Type is
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

               Xcb_V := New_Xcb (Subpool);
               Insert (This        => Parents_Including_Self_To_Current_Tag_Map,
                       Key         => Parents_Including_Self,
                       New_Element => New_Current_Tag (Parent_Tag => Find_Tag (Parent_Tags),
                                                       Xcb        => Xcb_V,
                                                       Subpool    => Subpool));
               Is_Success := True;
            else
               Is_Success := False;
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Xcb & ", but found " & Tag_Name);
            end if;
            return;
         end if;

         case Prev_Tag.Kind_Id is
            when Tag_Id.Xcb =>
               if Tag_Name = Tag_Struct then
                  declare
                     Struct_V : constant X_Proto_XML.Struct.Ptr := New_Struct (Subpool);
                  begin
                     case Prev_Tag.Kind_Id is
                        when Tag_Id.Xcb =>
                           Append_Struct (Prev_Tag.Xcb_V.all, Struct_V);
                           Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                                    Struct     => Struct_V,
                                                    Subpool    => Subpool));
                           Is_Success := True;
                        when others =>
                           Is_Success := False;
                           Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Expected " & Tag_Struct & ", but found " & Tag_Name);
                     end case;
                  end;
               elsif Tag_Name = Tag_X_Id_Kind then
                  declare
                     X_Id_Type_V : constant X_Proto_XML.X_Id.Ptr := New_X_Id (Subpool);
                  begin
                     Append_X_Id (Prev_Tag.Xcb_V.all, X_Id_Type_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag  => Prev_Tag,
                                              X_Id        => X_Id_Type_V,
                                              Subpool     => Subpool));
                  end;
               elsif Tag_Name = Tag_X_Id_Union then
                  declare
                     X_Id_Union_V : constant X_Proto_XML.X_Id_Union.Ptr := New_X_Id_Union (Subpool);
                  begin
                     Append_X_Id_Union (Prev_Tag.Xcb_V.all, X_Id_Union_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              X_Id_Union => X_Id_Union_V,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Type_Definition then
                  declare
                     TD : constant X_Proto_XML.Type_Definition.Ptr := New_Type_Definition (Subpool);
                  begin
                     Append_Type_Definition (Prev_Tag.Xcb_V.all, TD);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag       => Prev_Tag,
                                              Type_Definition  => TD,
                                              Subpool          => Subpool));
                  end;
               elsif Tag_Name = Tag_Enum then
                  declare
                     Enum_V : constant X_Proto_XML.Enum.Ptr := New_Enum (Subpool);
                  begin
                     Append_Enum (Prev_Tag.Xcb_V.all, Enum_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Enum       => Enum_V,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Event then
                  declare
                     E : constant X_Proto_XML.Event.Ptr := New_Event (Subpool);
                  begin
                     Append_Event (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Event      => E,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Event_Copy then
                  declare
                     EC : constant X_Proto_XML.Event_Copy.Ptr := New_Event_Copy (Subpool);
                  begin
                     Append_Event_Copy (Prev_Tag.Xcb_V.all, EC);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Event_Copy => EC,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Union then
                  declare
                     U : constant X_Proto_XML.Union.Ptr := New_Union (Subpool);
                  begin
                     Append_Union (Prev_Tag.Xcb_V.all, U);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Union      => U,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     E : constant X_Proto_XML.Error.Ptr := New_Error (Subpool);
                  begin
                     Append_Error (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Error      => E,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Error_Copy then
                  declare
                     E : constant X_Proto_XML.Error_Copy.Ptr := New_Error_Copy (Subpool);
                  begin
                     Append_Error_Copy (Prev_Tag.Xcb_V.all, E);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Error_Copy => E,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Request then
                  declare
                     R : constant X_Proto_XML.Request.Ptr := New_Request (Subpool);
                  begin
                     Append_Request (Prev_Tag.Xcb_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Request    => R,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Struct =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_Field (Subpool);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_Pad (Subpool);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Struct.Fs.Member_Ptr := New_Struct_List (Subpool);
                  begin
                     Append_Member (Prev_Tag.Struct_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              List       => L.L'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.X_Id_Union =>
               if Tag_Name = Tag_Kind then
                  declare
                     Kind : constant X_Proto_XML.Type_P.Ptr := New_Type (Subpool);
                  begin
                     Append_Kind (Prev_Tag.X_Id_Union_V.all, Kind);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Kind       => Kind,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Enum =>
               if Tag_Name = Tag_Item then
                  declare
                     Item_V : constant X_Proto_XML.Item.Ptr := New_Item (Subpool);
                  begin
                     Append_Item (Prev_Tag.Enum_V.all, Item_V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Item       => Item_V,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : constant X_Proto_XML.Documentation.Ptr := New_Documentation (Subpool);
                  begin
                     Append_Documentation (Prev_Tag.Enum_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag    => Prev_Tag,
                                              Documentation => D,
                                              Subpool       => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Item =>
               if Tag_Name = XML_Tag_Value then
                  Is_Success := True;
               elsif Tag_Name = Tag_Bit then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.List =>
               if Tag_Name = Tag_Field_Reference then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Operation then
                  declare
                     Operation : X_Proto_XML.List.Fs.Member_Ptr := New_List_Operation (Subpool);
                  begin
                     Append_Member (Prev_Tag.List_V.all, Operation);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Operation  => Operation.Operation'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.List.Fs.Member_Ptr := New_List_Value (Subpool);
                  begin
                     Append_Member (Prev_Tag.List_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Value      => V.Value'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Op =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     V : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Operation (Subpool);
                  begin
                     V.Operation := New_Operation (Subpool);
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Operation  => V.Operation,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Field_Reference then
                  declare
                     V : X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Field_Reference (Subpool);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag      => Prev_Tag,
                                              Field_Reference => V.Field_Reference'Access,
                                              Subpool         => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Value then
                  declare
                     V : X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Value (Subpool);
                  begin
                     Append_Member (Prev_Tag.Op_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Value      => V.Value'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Event =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Field (Subpool);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Pad(Subpool);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     D : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_Doc(Subpool);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag    => Prev_Tag,
                                              Documentation => D.D'Access,
                                              Subpool       => Subpool));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Event.Fs.Member_Ptr := New_Event_List(Subpool);
                  begin
                     Append_Member (Prev_Tag.Event_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              List       => L.L'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Documentation =>
               if Tag_Name = Tag_Field then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Field(Subpool);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => D.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_See then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_See(Subpool);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              See        => D.S'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Error then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Error(Subpool);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Error      => D.E'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Example then
                  declare
                     D : X_Proto_XML.Documentation.Fs.Member_Ptr := New_Documentation_Example(Subpool);
                  begin
                     Append_Member (Prev_Tag.Documentation_V.all, D);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Example    => D.Ex'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Brief then
                  Is_Success := True;
               elsif Tag_Name = XML_Tag_Description then
                  Is_Success := True;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Union =>
               if Tag_Name = Tag_List then
                  declare
                     L : X_Proto_XML.Union.Fs.Child_Ptr := New_Union_List (Subpool);
                  begin
                     Append_Child (Prev_Tag.Union_V.all, L);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              List       => L.L'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Error =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Error.Fs.Child_Ptr := New_Error_Field(Subpool);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Error.Fs.Child_Ptr := New_Error_Pad (Subpool);
                  begin
                     Append_Child (Prev_Tag.Error_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Request =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Field(Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     P : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Pad (Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, P);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Pad        => P.P'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Value_Param then
                  declare
                     V : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Value_Param (Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag  => Prev_Tag,
                                              Value_Param => V.V'Access,
                                              Subpool     => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     V : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Documentation (Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, V);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag    => Prev_Tag,
                                              Documentation => V.D'Access,
                                              Subpool       => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Reply then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Reply(Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Reply      => R.R'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_List (Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              List       => R.L'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Expression_Field then
                  declare
                     R : X_Proto_XML.Request.Fs.Child_Ptr := New_Request_Expression_Field (Subpool);
                  begin
                     Append_Child (Prev_Tag.Request_V.all, R);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag       => Prev_Tag,
                                              Expression_Field => R.EF'Access,
                                              Subpool          => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Reply =>
               if Tag_Name = Tag_Field then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Field (Subpool);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Field      => F.F'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = Tag_Pad then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Pad (Subpool);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Pad        => F.P'Access,
                                              Subpool    => Subpool));
                  end;
               elsif Tag_Name = XML_Tag_Doc then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_Documentation (Subpool);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag    => Prev_Tag,
                                              Documentation => F.D'Access,
                                              Subpool       => Subpool));
                  end;
               elsif Tag_Name = Tag_List then
                  declare
                     F : X_Proto_XML.Reply.Fs.Child_Ptr := New_Reply_List (Subpool);
                  begin
                     Append_Child (Prev_Tag.Reply_V.all, F);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              List       => F.L'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
               end if;
            when Tag_Id.Expression_Field =>
               if Tag_Name = XML_Tag_Operation then
                  declare
                     C : X_Proto_XML.Expression_Field.Fs.Child_Ptr := New_Expression_Field_Operation (Subpool);
                  begin
                     Append_Child (Prev_Tag.Expression_Field_V.all, C);
                     Is_Success := True;
                     Insert (New_Current_Tag (Parent_Tag => Prev_Tag,
                                              Operation  => C.Op'Access,
                                              Subpool    => Subpool));
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected start tag " & Tag_Name);
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
               Initialize (Error_Message, GNAT.Source_Info.Source_Location & "Error, tag name is " & Tag_Name);
         end case;
      end Start_Tag;

      procedure Attribute (Attribute_Name              : String;
                           Attribute_Value             : String;
                           Parent_Tags_And_Current_Tag : Aida.XML.DL.T;
                           Error_Message               : out Aida.XML.Error_Message_T;
                           Is_Success                  : out Boolean)
      is
         Current_Tag : constant Current_Tag_Access_Type := Find_Tag (Parent_Tags_And_Current_Tag);
      begin
         if Current_Tag = null then
            Is_Success := False;
            Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", attribute name " & Attribute_Name & " and value " & Attribute_Value & ", parents: " & To_String (
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
                     Set_Header (Current_Tag.Xcb_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Struct =>
               if Attribute_Name = Tag_Struct_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Struct_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Field =>
               if Attribute_Name = Tag_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Enum (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Mask then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask (Current_Tag.Field_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Field_Attribute_Alt_Enum then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Alt_Enum (Current_Tag.Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Kind =>
               if Attribute_Name = Tag_X_Id_Kind_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.X_Id_Kind_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.X_Id_Union =>
               if Attribute_Name = Tag_X_Id_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.X_Id_Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Type_Definition =>
               if Attribute_Name = Tag_Type_Definition_Attribute_Old_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Old_Name (Current_Tag.Type_Definition_V.all, V);
                  end;
               elsif Attribute_Name = Tag_Type_Definition_Attribute_New_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_New_Name (Current_Tag.Type_Definition_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
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
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
                     else
                        Set_Bytes (Current_Tag.Pad_V.all, Positive (V));
                     end if;
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Enum =>
               if Attribute_Name = Tag_Enum_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Enum_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Item =>
               if Attribute_Name = Tag_Item_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Item_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.List =>
               if Attribute_Name = Tag_List_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.List_V.all, V);
                  end;
               elsif Attribute_Name = Tag_List_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.List_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Op =>
               if Attribute_Name = Tag_Operation_Attribute_Op then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Op (Current_Tag.Op_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event =>
               if Attribute_Name = XML_Tag_Event_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
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
            when Tag_Id.See =>
               if Attribute_Name = XML_Tag_See_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.See_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_See_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.See_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Event_Copy =>
               if Attribute_Name = XML_Tag_Event_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Event_Copy_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Event_Copy_Attribute_Ref then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
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
            when Tag_Id.Union =>
               if Attribute_Name = XML_Tag_Union_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Union_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error =>
               if Attribute_Name = XML_Tag_Error_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
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
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Error_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Error_Copy =>
               if Attribute_Name = XML_Tag_Error_Copy_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
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
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Ref (Current_Tag.Error_Copy_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Request =>
               if Attribute_Name = XML_Tag_Request_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
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
            when Tag_Id.Value_Param =>
               if Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Kind (Current_Tag.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_Mask_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Mask_Name (Current_Tag.Value_Param_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Value_Param_Attribute_List_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_List_Name (Current_Tag.Value_Param_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Expression_Field =>
               if Attribute_Name = XML_Tag_Expression_Field_Attribute_Name then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Name (Current_Tag.Expression_Field_V.all, V);
                  end;
               elsif Attribute_Name = XML_Tag_Expression_Field_Attribute_Kind then
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Attribute_Value);
                     Set_Kind (Current_Tag.Expression_Field_V.all, V);
                  end;
               else
                  Is_Success := False;
                  Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected attribute name " & Attribute_Name & " and value " & Attribute_Value);
               end if;
            when Tag_Id.Kind |
                 Tag_Id.Value |
                 Tag_Id.Bit |
                 Tag_Id.Field_Reference |
                 Tag_Id.Documentation |
                 Tag_Id.Reply |
                 Tag_Id.Example =>
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
                  when Tag_Id.List =>
                     if Tag_Name = Tag_Field_Reference then
                        declare
                           L : constant X_Proto_XML.List.Fs.Member_Ptr := New_List_Field_Reference (Subpool);
                        begin
                           Initialize (L.Field_Reference, Tag_Value);
                           Append_Member (Prev_Tag.List_V.all, L);
                        end;
                     else
                        Is_Success := False;
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Prev_Tag.Kind_Id'Img);
                     end if;
                  when Tag_Id.Documentation =>
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

            case Current_Tag.Kind_Id is
               when Tag_Id.Kind =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Kind.all, V);
                  end;
               when Tag_Id.Field =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Field_V.all, V);
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
                        Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", failed to interpret '" & Tag_Value & "' as a number for end tag " & Tag_Name);
                     else
                        Current_Tag.Value_V.all := V;
                     end if;
                  end;
               when Tag_Id.Error =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Error_V.all, V);
                  end;
               when Tag_Id.Example =>
                  declare
                     V : X_Proto_XML.Large_Bounded_String.T;
                  begin
                     Initialize (V, Tag_Value);
                     Set_Value (Current_Tag.Example_V.all, V);
                  end;
               when Tag_Id.Op =>
                  if Tag_Name = Tag_Field_Reference then
                     declare
                        V : X_Proto_XML.Field_Reference_Type;
                        FR : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Field_Reference (Subpool);
                     begin
                        Initialize (V, Tag_Value);
                        FR.Field_Reference := V;
                        Append_Member (Current_Tag.Op_V.all, FR);
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
                              Operation_Value : constant X_Proto_XML.Operation.Fs.Member_Ptr := New_Operation_Value (Subpool);
                           begin
                              Operation_Value.Value := V;
                              Append_Member (Current_Tag.Op_V.all, Operation_Value);
                           end;
                        end if;
                     end;
                  else
                     Is_Success := False;
                     Initialize (Error_Message, GNAT.Source_Info.Source_Location & ", found unexpected end tag '" & Tag_Name & "' and previous tag is " & Current_Tag.Kind_Id'Img);
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
