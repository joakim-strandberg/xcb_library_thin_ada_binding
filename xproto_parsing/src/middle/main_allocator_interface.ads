with X_Proto_XML;
with Current_Tag;

package Main_Allocator_Interface is

   type T is abstract tagged limited null record;

   function New_Xcb (This : T) return X_Proto_XML.Xcb.Ptr is abstract;

   function New_Struct (This : T) return X_Proto_XML.Struct.Ptr is abstract;

   function New_X_Id (This : T) return X_Proto_XML.X_Id.Ptr is abstract;

   function New_X_Id_Union (This : T) return X_Proto_XML.X_Id_Union.Ptr is abstract;

   function New_Type_Definition (This : T) return X_Proto_XML.Type_Definition.Ptr is abstract;

   function New_Enum (This : T) return X_Proto_XML.Enum.Ptr is abstract;

   function New_Event (This : T) return X_Proto_XML.Event.Ptr is abstract;

   function New_Event_Copy (This : T) return X_Proto_XML.Event_Copy.Ptr is abstract;

   function New_Union (This : T) return X_Proto_XML.Union.Ptr is abstract;

   function New_Error (This : T) return X_Proto_XML.Error.Ptr is abstract;

   function New_Error_Copy (This : T) return X_Proto_XML.Error_Copy.Ptr is abstract;

   function New_Request (This : T) return X_Proto_XML.Request.Ptr is abstract;

   function New_Field (This : T) return X_Proto_XML.Field.Ptr is abstract;

   function New_Pad (This : T) return X_Proto_XML.Pad.Ptr is abstract;

   function New_List (This : T) return X_Proto_XML.List.Ptr is abstract;

   function New_Struct_Member (This : T;
                               Kind : X_Proto_XML.Struct.Fs.Member_Kind_Id.Enum_T) return X_Proto_XML.Struct.Fs.Member_Ptr is abstract;

   function New_Type (This : T) return X_Proto_XML.Type_P.Ptr is abstract;

   function New_Item (This : T) return X_Proto_XML.Item.Ptr is abstract;

   function New_Documentation (This : T) return X_Proto_XML.Documentation.Ptr is abstract;

   function New_List_Member (This : T;
                             Kind : X_Proto_XML.List.Fs.Member_Kind_Id_Type) return X_Proto_XML.List.Fs.Member_Ptr is abstract;

   function New_Operation (This : T) return X_Proto_XML.Operation.Ptr is abstract;

   function New_Operation_Member (This : T;
                                  Kind : X_Proto_XML.Operation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Operation.Fs.Member_Ptr is abstract;

   function New_Event_Member (This : T;
                              Kind : X_Proto_XML.Event.Fs.Member_Kind_Id_Type) return X_Proto_XML.Event.Fs.Member_Ptr is abstract;

   function New_Documentation_Member (This : T;
                                      Kind : X_Proto_XML.Documentation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Documentation.Fs.Member_Ptr is abstract;

   function New_Union_Child (This : T;
                             Kind : X_Proto_XML.Union.Fs.Child_Kind_Id_Type) return X_Proto_XML.Union.Fs.Child_Ptr is abstract;

   function New_Error_Child (This : T;
                             Kind : X_Proto_XML.Error.Fs.Child_Kind_Id_Type) return X_Proto_XML.Error.Fs.Child_Ptr is abstract;

   function New_Request_Child (This : T;
                               Kind : X_Proto_XML.Request.Fs.Child_Kind_Id_Type) return X_Proto_XML.Request.Fs.Child_Ptr is abstract;

   function New_Reply_Child (This : T;
                             Kind : X_Proto_XML.Reply.Fs.Child_Kind_Id_Type) return X_Proto_XML.Reply.Fs.Child_Ptr is abstract;

   function New_Expression_Field_Child (This : T;
                                        Kind : X_Proto_XML.Expression_Field.Fs.Child_Kind_Id_Type) return X_Proto_XML.Expression_Field.Fs.Child_Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Xcb        : X_Proto_XML.Xcb.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Struct     : X_Proto_XML.Struct.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             X_Id       : X_Proto_XML.X_Id.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             X_Id_Union : X_Proto_XML.X_Id_Union.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag      : Current_Tag.Ptr;
                             Type_Definition : X_Proto_XML.Type_Definition.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Enum       : X_Proto_XML.Enum.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Event      : X_Proto_XML.Event.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Event_Copy : X_Proto_XML.Event_Copy.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Union      : X_Proto_XML.Union.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Error      : X_Proto_XML.Error.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Error_Copy : X_Proto_XML.Error_Copy.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Request    : X_Proto_XML.Request.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Field      : X_Proto_XML.Field.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Pad        : X_Proto_XML.Pad.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             List       : X_Proto_XML.List.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Kind       : X_Proto_XML.Type_P.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Item       : X_Proto_XML.Item.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag    : Current_Tag.Ptr;
                             Documentation : X_Proto_XML.Documentation.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Operation  : X_Proto_XML.Operation.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Value      : X_Proto_XML.Value_Access_Type) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag      : Current_Tag.Ptr;
                             Field_Reference : X_Proto_XML.Field_Reference_Access_Type) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             See        : X_Proto_XML.See.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Example    : X_Proto_XML.Example.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag  : Current_Tag.Ptr;
                             Value_Param : X_Proto_XML.Value_Param.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag : Current_Tag.Ptr;
                             Reply      : X_Proto_XML.Reply.Ptr) return Current_Tag.Ptr is abstract;

   function New_Current_Tag (This       : T;
                             Parent_Tag       : Current_Tag.Ptr;
                             Expression_Field : X_Proto_XML.Expression_Field.Ptr) return Current_Tag.Ptr is abstract;

end Main_Allocator_Interface;
