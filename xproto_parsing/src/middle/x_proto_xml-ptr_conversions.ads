with System.Address_To_Access_Conversions;

package X_Proto_XML.Ptr_Conversions is

   package Xcb is new System.Address_To_Access_Conversions (X_Proto_XML.Xcb.T);

   package Struct is new System.Address_To_Access_Conversions (X_Proto_XML.Struct.T);

   package X_Id is new System.Address_To_Access_Conversions (X_Proto_XML.X_Id.T);

   package X_Id_Union is new System.Address_To_Access_Conversions (X_Proto_XML.X_Id_Union.T);

   package Type_Definition is new System.Address_To_Access_Conversions (X_Proto_XML.Type_Definition.T);

   package Enum is new System.Address_To_Access_Conversions (X_Proto_XML.Enum.T);

   package Event is new System.Address_To_Access_Conversions (X_Proto_XML.Event.T);

   package Event_Copy is new System.Address_To_Access_Conversions (X_Proto_XML.Event_Copy.T);

   package Union is new System.Address_To_Access_Conversions (X_Proto_XML.Union.T);

   package Error is new System.Address_To_Access_Conversions (X_Proto_XML.Error.T);

   package Error_Copy is new System.Address_To_Access_Conversions (X_Proto_XML.Error_Copy.T);

   package Request is new System.Address_To_Access_Conversions (X_Proto_XML.Request.T);

   package Field is new System.Address_To_Access_Conversions (X_Proto_XML.Field.T);

   package Pad is new System.Address_To_Access_Conversions (X_Proto_XML.Pad.T);

   package List is new System.Address_To_Access_Conversions (X_Proto_XML.List.T);

   package Struct_Member is new System.Address_To_Access_Conversions (X_Proto_XML.Struct.Fs.Member_Type);

   package Type_P is new System.Address_To_Access_Conversions (X_Proto_XML.Type_P.T);

   package Item is new System.Address_To_Access_Conversions (X_Proto_XML.Item.T);

   package Documentation is new System.Address_To_Access_Conversions (X_Proto_XML.Documentation.T);

   package List_Member is new System.Address_To_Access_Conversions (X_Proto_XML.List.Fs.Member_Type);

   package Operation is new System.Address_To_Access_Conversions (X_Proto_XML.Operation.T);

   package Operation_Member is new System.Address_To_Access_Conversions (X_Proto_XML.Operation.Fs.Member_Type);

   package Event_Member is new System.Address_To_Access_Conversions (X_Proto_XML.Event.Fs.Member_Type);

   package Documentation_Member is new System.Address_To_Access_Conversions (X_Proto_XML.Documentation.Fs.Member_Type);

   package Union_Child is new System.Address_To_Access_Conversions (X_Proto_XML.Union.Fs.Child_Type);

   package Error_Child is new System.Address_To_Access_Conversions (X_Proto_XML.Error.Fs.Child_Type);

   package Request_Child is new System.Address_To_Access_Conversions (X_Proto_XML.Request.Fs.Child_Type);

   package Reply_Child is new System.Address_To_Access_Conversions (X_Proto_XML.Reply.Fs.Child_Type);

   package Expression_Field_Child is new System.Address_To_Access_Conversions (X_Proto_XML.Expression_Field.Fs.Child_Type);

end X_Proto_XML.Ptr_Conversions;
