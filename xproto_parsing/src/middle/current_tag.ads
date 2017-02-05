with X_Proto_XML;
with System.Address_To_Access_Conversions;

package Current_Tag is

   package Fs is

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

   end Fs;

   use Fs.Tag_Id;

   type T;

   type Ptr is access all T;

   type T (Kind_Id : Fs.Tag_Id.Enumeration_Type) is record
      Find_Tag : Ptr := null;
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

   package Ptr_Conversions is new System.Address_To_Access_Conversions (Current_Tag.T);

end Current_Tag;
