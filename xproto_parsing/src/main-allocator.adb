with Ada.Unchecked_Conversion;
with System.Address_To_Access_Conversions;
with X_Proto_XML.Ptr_Conversions;

separate (Main)
package body Allocator is

   type Local_Xcb_Ptr is access all X_Proto_XML.Xcb.T;
   for Local_Xcb_Ptr'Storage_Pool use Pool;

   package Xcb_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Xcb.T);

   function New_Xcb (This : T) return X_Proto_XML.Xcb.Ptr is
      L : constant Local_Xcb_Ptr := new X_Proto_XML.Xcb.T;

      SA : constant System.Address := Xcb_Conversions.To_Address (Xcb_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Xcb.Ptr (X_Proto_XML.Ptr_Conversions.Xcb.To_Pointer (SA));
   end New_Xcb;

   type Local_Struct_Ptr is access all X_Proto_XML.Struct.T;
   for Local_Struct_Ptr'Storage_Pool use Pool;

   package Struct_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Struct.T);

   function New_Struct (This : T) return X_Proto_XML.Struct.Ptr is
      L : constant Local_Struct_Ptr := new X_Proto_XML.Struct.T;

      SA : constant System.Address := Struct_Conversions.To_Address (Struct_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Struct.Ptr (X_Proto_XML.Ptr_Conversions.Struct.To_Pointer (SA));
   end New_Struct;

   type Local_X_Id_Ptr is access all X_Proto_XML.X_Id.T;
   for Local_X_Id_Ptr'Storage_Pool use Pool;

   package X_Id_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.X_Id.T);

   function New_X_Id (This : T) return X_Proto_XML.X_Id.Ptr is
      L : constant Local_X_Id_Ptr := new X_Proto_XML.X_Id.T;
      SA : constant System.Address := X_Id_Conversions.To_Address (X_Id_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.X_Id.Ptr (X_Proto_XML.Ptr_Conversions.X_Id.To_Pointer (SA));
   end New_X_Id;

   type Local_X_Id_Union_Ptr is access all X_Proto_XML.X_Id_Union.T;
   for Local_X_Id_Union_Ptr'Storage_Pool use Pool;

   package X_Id_Union_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.X_Id_Union.T);

   function New_X_Id_Union (This : T) return X_Proto_XML.X_Id_Union.Ptr is
      L : constant Local_X_Id_Union_Ptr := new X_Proto_XML.X_Id_Union.T;
      SA : constant System.Address := X_Id_Union_Conversions.To_Address (X_Id_Union_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.X_Id_Union.Ptr (X_Proto_XML.Ptr_Conversions.X_Id_Union.To_Pointer (SA));
   end New_X_Id_Union;

   type Local_Type_Definition_Ptr is access all X_Proto_XML.Type_Definition.T;
   for Local_Type_Definition_Ptr'Storage_Pool use Pool;

   package Type_Definition_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Type_Definition.T);

   function New_Type_Definition (This : T) return X_Proto_XML.Type_Definition.Ptr is
      L : constant Local_Type_Definition_Ptr := new X_Proto_XML.Type_Definition.T;
      SA : constant System.Address := Type_Definition_Conversions.To_Address (Type_Definition_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Type_Definition.Ptr (X_Proto_XML.Ptr_Conversions.Type_Definition.To_Pointer (SA));
   end New_Type_Definition;

   type Local_Enum_Ptr is access all X_Proto_XML.Enum.T;
   for Local_Enum_Ptr'Storage_Pool use Pool;

   package Enum_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Enum.T);

   function New_Enum (This : T) return X_Proto_XML.Enum.Ptr is
      L : constant Local_Enum_Ptr := new X_Proto_XML.Enum.T;
      SA : constant System.Address := Enum_Conversions.To_Address (Enum_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Enum.Ptr (X_Proto_XML.Ptr_Conversions.Enum.To_Pointer (SA));
   end New_Enum;

   type Local_Event_Ptr is access all X_Proto_XML.Event.T;
   for Local_Event_Ptr'Storage_Pool use Pool;

   package Event_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Event.T);

   function New_Event (This : T) return X_Proto_XML.Event.Ptr is
      L : constant Local_Event_Ptr := new X_Proto_XML.Event.T;
      SA : constant System.Address := Event_Conversions.To_Address (Event_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Event.Ptr (X_Proto_XML.Ptr_Conversions.Event.To_Pointer (SA));
   end New_Event;

   type Local_Event_Copy_Ptr is access all X_Proto_XML.Event_Copy.T;
   for Local_Event_Copy_Ptr'Storage_Pool use Pool;

   package Event_Copy_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Event_Copy.T);

   function New_Event_Copy (This : T) return X_Proto_XML.Event_Copy.Ptr is
      L : constant Local_Event_Copy_Ptr := new X_Proto_XML.Event_Copy.T;
      SA : constant System.Address := Event_Copy_Conversions.To_Address (Event_Copy_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Event_Copy.Ptr (X_Proto_XML.Ptr_Conversions.Event_Copy.To_Pointer (SA));
   end New_Event_Copy;

   type Local_Union_Ptr is access all X_Proto_XML.Union.T;
   for Local_Union_Ptr'Storage_Pool use Pool;

   package Union_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Union.T);

   function New_Union (This : T) return X_Proto_XML.Union.Ptr is
      L : constant Local_Union_Ptr := new X_Proto_XML.Union.T;
      SA : constant System.Address := Union_Conversions.To_Address (Union_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Union.Ptr (X_Proto_XML.Ptr_Conversions.Union.To_Pointer (SA));
   end New_Union;

   type Local_Error_Ptr is access all X_Proto_XML.Error.T;
   for Local_Error_Ptr'Storage_Pool use Pool;

   package Error_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Error.T);

   function New_Error (This : T) return X_Proto_XML.Error.Ptr is
      L : constant Local_Error_Ptr := new X_Proto_XML.Error.T;
      SA : constant System.Address := Error_Conversions.To_Address (Error_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Error.Ptr (X_Proto_XML.Ptr_Conversions.Error.To_Pointer (SA));
   end New_Error;

   type Local_Error_Copy_Ptr is access all X_Proto_XML.Error_Copy.T;
   for Local_Error_Copy_Ptr'Storage_Pool use Pool;

   package Error_Copy_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Error_Copy.T);

   function New_Error_Copy (This : T) return X_Proto_XML.Error_Copy.Ptr is
      L : constant Local_Error_Copy_Ptr := new X_Proto_XML.Error_Copy.T;
      SA : constant System.Address := Error_Copy_Conversions.To_Address (Error_Copy_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Error_Copy.Ptr (X_Proto_XML.Ptr_Conversions.Error_Copy.To_Pointer (SA));
   end New_Error_Copy;

   type Local_Request_Ptr is access all X_Proto_XML.Request.T;
   for Local_Request_Ptr'Storage_Pool use Pool;

   package Request_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Request.T);

   function New_Request (This : T) return X_Proto_XML.Request.Ptr is
      L : constant Local_Request_Ptr := new X_Proto_XML.Request.T;
      SA : constant System.Address := Request_Conversions.To_Address (Request_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Request.Ptr (X_Proto_XML.Ptr_Conversions.Request.To_Pointer (SA));
   end New_Request;

   type Local_Field_Ptr is access all X_Proto_XML.Field.T;
   for Local_Field_Ptr'Storage_Pool use Pool;

   package Field_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Field.T);

   function New_Field (This : T) return X_Proto_XML.Field.Ptr is
      L : constant Local_Field_Ptr := new X_Proto_XML.Field.T;
      SA : constant System.Address := Field_Conversions.To_Address (Field_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Field.Ptr (X_Proto_XML.Ptr_Conversions.Field.To_Pointer (SA));
   end New_Field;

   type Local_Pad_Ptr is access all X_Proto_XML.Pad.T;
   for Local_Pad_Ptr'Storage_Pool use Pool;

   package Pad_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Pad.T);

   function New_Pad (This : T) return X_Proto_XML.Pad.Ptr is
      L : constant Local_Pad_Ptr := new X_Proto_XML.Pad.T;
      SA : constant System.Address := Pad_Conversions.To_Address (Pad_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Pad.Ptr (X_Proto_XML.Ptr_Conversions.Pad.To_Pointer (SA));
   end New_Pad;

   type Local_List_Ptr is access all X_Proto_XML.List.T;
   for Local_List_Ptr'Storage_Pool use Pool;

   package List_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.List.T);

   function New_List (This : T) return X_Proto_XML.List.Ptr is
      L : constant Local_List_Ptr := new X_Proto_XML.List.T;
      SA : constant System.Address := List_Conversions.To_Address (List_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.List.Ptr (X_Proto_XML.Ptr_Conversions.List.To_Pointer (SA));
   end New_List;

   type Local_Struct_Member_Ptr is access all X_Proto_XML.Struct.Fs.Member_Type;
   for Local_Struct_Member_Ptr'Storage_Pool use Pool;

   package Struct_Member_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Struct.Fs.Member_Type);

   function New_Struct_Member (This : T;
                               Kind : X_Proto_XML.Struct.Fs.Member_Kind_Id.Enum_T) return X_Proto_XML.Struct.Fs.Member_Ptr
   is
      L : constant Local_Struct_Member_Ptr := new X_Proto_XML.Struct.Fs.Member_Type (Kind);
      SA : constant System.Address := Struct_Member_Conversions.To_Address (Struct_Member_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (X_Proto_XML.Ptr_Conversions.Struct_Member.To_Pointer (SA));
   end New_Struct_Member;

   type Local_Type_P_Ptr is access all X_Proto_XML.Type_P.T;
   for Local_Type_P_Ptr'Storage_Pool use Pool;

   package Type_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Type_P.T);

   function New_Type (This : T) return X_Proto_XML.Type_P.Ptr is
      L : constant Local_Type_P_Ptr := new X_Proto_XML.Type_P.T;
      SA : constant System.Address := Type_Conversions.To_Address (Type_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Type_P.Ptr (X_Proto_XML.Ptr_Conversions.Type_P.To_Pointer (SA));
   end New_Type;

   type Local_Item_Ptr is access all X_Proto_XML.Item.T;
   for Local_Item_Ptr'Storage_Pool use Pool;

   package Item_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Item.T);

   function New_Item (This : T) return X_Proto_XML.Item.Ptr is
      L : constant Local_Item_Ptr := new X_Proto_XML.Item.T;
      SA : constant System.Address := Item_Conversions.To_Address (Item_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Item.Ptr (X_Proto_XML.Ptr_Conversions.Item.To_Pointer (SA));
   end New_Item;

   type Local_Documentation_Ptr is access all X_Proto_XML.Documentation.T;
   for Local_Documentation_Ptr'Storage_Pool use Pool;

   package Documentation_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Documentation.T);

   function New_Documentation (This : T) return X_Proto_XML.Documentation.Ptr is
      L : constant Local_Documentation_Ptr := new X_Proto_XML.Documentation.T;
      SA : constant System.Address := Documentation_Conversions.To_Address (Documentation_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Documentation.Ptr (X_Proto_XML.Ptr_Conversions.Documentation.To_Pointer (SA));
   end New_Documentation;

   type Local_List_Member_Ptr is access all X_Proto_XML.List.Fs.Member_Type;
   for Local_List_Member_Ptr'Storage_Pool use Pool;

   package List_Member_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.List.Fs.Member_Type);

   function New_List_Member (This : T;
                             Kind : X_Proto_XML.List.Fs.Member_Kind_Id_Type) return X_Proto_XML.List.Fs.Member_Ptr
   is
      L : constant Local_List_Member_Ptr := new X_Proto_XML.List.Fs.Member_Type (Kind);
      SA : constant System.Address := List_Member_Conversions.To_Address (List_Member_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.List.Fs.Member_Ptr (X_Proto_XML.Ptr_Conversions.List_Member.To_Pointer (SA));
   end New_List_Member;

   type Local_Operation_Ptr is access all X_Proto_XML.Operation.T;
   for Local_Operation_Ptr'Storage_Pool use Pool;

   package Operation_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Operation.T);

   function New_Operation (This : T) return X_Proto_XML.Operation.Ptr is
      L : constant Local_Operation_Ptr := new X_Proto_XML.Operation.T;
      SA : constant System.Address := Operation_Conversions.To_Address (Operation_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Operation.Ptr (X_Proto_XML.Ptr_Conversions.Operation.To_Pointer (SA));
   end New_Operation;

   type Local_Operation_Member_Ptr is access all X_Proto_XML.Operation.Fs.Member_Type;
   for Local_Operation_Member_Ptr'Storage_Pool use Pool;

   package Operation_Member_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Operation.Fs.Member_Type);

   function New_Operation_Member (This : T;
                                  Kind : X_Proto_XML.Operation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Operation.Fs.Member_Ptr
   is
      L : constant Local_Operation_Member_Ptr := new X_Proto_XML.Operation.Fs.Member_Type (Kind);
      SA : constant System.Address := Operation_Member_Conversions.To_Address (Operation_Member_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Operation.Fs.Member_Ptr (X_Proto_XML.Ptr_Conversions.Operation_Member.To_Pointer (SA));
   end New_Operation_Member;

   type Local_Event_Member_Ptr is access all X_Proto_XML.Event.Fs.Member_Type;
   for Local_Event_Member_Ptr'Storage_Pool use Pool;

   package Event_Member_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Event.Fs.Member_Type);

   function New_Event_Member (This : T;
                              Kind : X_Proto_XML.Event.Fs.Member_Kind_Id_Type) return X_Proto_XML.Event.Fs.Member_Ptr
   is
      L : constant Local_Event_Member_Ptr := new X_Proto_XML.Event.Fs.Member_Type (Kind);
      SA : constant System.Address := Event_Member_Conversions.To_Address (Event_Member_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Event.Fs.Member_Ptr (X_Proto_XML.Ptr_Conversions.Event_Member.To_Pointer (SA));
   end New_Event_Member;

   type Local_Documentation_Member_Ptr is access all X_Proto_XML.Documentation.Fs.Member_Type;
   for Local_Documentation_Member_Ptr'Storage_Pool use Pool;

   package Documentation_Member_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Documentation.Fs.Member_Type);

   function New_Documentation_Member (This : T;
                                      Kind : X_Proto_XML.Documentation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Documentation.Fs.Member_Ptr
   is
      L : constant Local_Documentation_Member_Ptr := new X_Proto_XML.Documentation.Fs.Member_Type (Kind);
      SA : constant System.Address := Documentation_Member_Conversions.To_Address (Documentation_Member_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Documentation.Fs.Member_Ptr (X_Proto_XML.Ptr_Conversions.Documentation_Member.To_Pointer (SA));
   end New_Documentation_Member;

   type Local_Union_Child_Ptr is access all X_Proto_XML.Union.Fs.Child_Type;
   for Local_Union_Child_Ptr'Storage_Pool use Pool;

   package Union_Child_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Union.Fs.Child_Type);

   function New_Union_Child (This : T;
                             Kind : X_Proto_XML.Union.Fs.Child_Kind_Id_Type) return X_Proto_XML.Union.Fs.Child_Ptr
   is
      L : constant Local_Union_Child_Ptr := new X_Proto_XML.Union.Fs.Child_Type (Kind);
      SA : constant System.Address := Union_Child_Conversions.To_Address (Union_Child_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Union.Fs.Child_Ptr (X_Proto_XML.Ptr_Conversions.Union_Child.To_Pointer (SA));
   end New_Union_Child;

   type Local_Error_Child_Ptr is access all X_Proto_XML.Error.Fs.Child_Type;
   for Local_Error_Child_Ptr'Storage_Pool use Pool;

   package Error_Child_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Error.Fs.Child_Type);

   function New_Error_Child (This : T;
                             Kind : X_Proto_XML.Error.Fs.Child_Kind_Id_Type) return X_Proto_XML.Error.Fs.Child_Ptr
   is
      L : constant Local_Error_Child_Ptr := new X_Proto_XML.Error.Fs.Child_Type (Kind);
      SA : constant System.Address := Error_Child_Conversions.To_Address (Error_Child_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Error.Fs.Child_Ptr (X_Proto_XML.Ptr_Conversions.Error_Child.To_Pointer (SA));
   end New_Error_Child;

   type Local_Request_Child_Ptr is access all X_Proto_XML.Request.Fs.Child_Type;
   for Local_Request_Child_Ptr'Storage_Pool use Pool;

   package Request_Child_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Request.Fs.Child_Type);

   function New_Request_Child (This : T;
                               Kind : X_Proto_XML.Request.Fs.Child_Kind_Id_Type) return X_Proto_XML.Request.Fs.Child_Ptr
   is
      L : constant Local_Request_Child_Ptr := new X_Proto_XML.Request.Fs.Child_Type (Kind);
      SA : constant System.Address := Request_Child_Conversions.To_Address (Request_Child_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (X_Proto_XML.Ptr_Conversions.Request_Child.To_Pointer (SA));
   end New_Request_Child;

   type Local_Reply_Child_Ptr is access all X_Proto_XML.Reply.Fs.Child_Type;
   for Local_Reply_Child_Ptr'Storage_Pool use Pool;

   package Reply_Child_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Reply.Fs.Child_Type);

   function New_Reply_Child (This : T;
                             Kind : X_Proto_XML.Reply.Fs.Child_Kind_Id_Type) return X_Proto_XML.Reply.Fs.Child_Ptr
   is
      L : constant Local_Reply_Child_Ptr := new X_Proto_XML.Reply.Fs.Child_Type (Kind);
      SA : constant System.Address := Reply_Child_Conversions.To_Address (Reply_Child_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Reply.Fs.Child_Ptr (X_Proto_XML.Ptr_Conversions.Reply_Child.To_Pointer (SA));
   end New_Reply_Child;

   type Local_Expression_Field_Child_Ptr is access all X_Proto_XML.Expression_Field.Fs.Child_Type;
   for Local_Expression_Field_Child_Ptr'Storage_Pool use Pool;

   package Expression_Field_Child_Conversions is new System.Address_To_Access_Conversions (X_Proto_XML.Expression_Field.Fs.Child_Type);

   function New_Expression_Field_Child (This : T;
                                        Kind : X_Proto_XML.Expression_Field.Fs.Child_Kind_Id_Type) return X_Proto_XML.Expression_Field.Fs.Child_Ptr
   is
      L : constant Local_Expression_Field_Child_Ptr := new X_Proto_XML.Expression_Field.Fs.Child_Type (Kind);
      SA : constant System.Address := Expression_Field_Child_Conversions.To_Address (Expression_Field_Child_Conversions.Object_Pointer (L));
   begin
      return X_Proto_XML.Expression_Field.Fs.Child_Ptr (X_Proto_XML.Ptr_Conversions.Expression_Field_Child.To_Pointer (SA));
   end New_Expression_Field_Child;

end Allocator;
