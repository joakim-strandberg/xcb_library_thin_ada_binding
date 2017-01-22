with Basic_Bounded_Dynamic_Pools;

package X_Proto_XML.Allocators is

   function New_Xcb (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Xcb.Ptr;

   function New_Struct (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Struct.Ptr;

   function New_X_Id (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return X_Id.Ptr;

   function New_X_Id_Union (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return X_Id_Union.Ptr;

   function New_Type_Definition (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Type_Definition.Ptr;

   function New_Enum (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Enum.Ptr;

   function New_Event (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Event.Ptr;

   function New_Event_Copy (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Event_Copy.Ptr;

   function New_Union (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Union.Ptr;

   function New_Error (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Error.Ptr;

   function New_Error_Copy (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Error_Copy.Ptr;

   function New_Request (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Request.Ptr;

   function New_Field (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Field.Ptr;

   function New_Pad (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Pad.Ptr;

   function New_List (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return List.Ptr;

   function New_Struct_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                               Kind : X_Proto_XML.Struct.Fs.Member_Kind_Id.Enum_T) return X_Proto_XML.Struct.Fs.Member_Ptr;

   function New_Type (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Type_P.Ptr;

   function New_Item (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Item.Ptr;

   function New_Documentation (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Documentation.Ptr;

   function New_List_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                             Kind : X_Proto_XML.List.Fs.Member_Kind_Id_Type) return X_Proto_XML.List.Fs.Member_Ptr;

   function New_Operation (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Operation.Ptr;

   function New_Operation_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                  Kind : X_Proto_XML.Operation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Operation.Fs.Member_Ptr;

   function New_Event_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                              Kind : X_Proto_XML.Event.Fs.Member_Kind_Id_Type) return X_Proto_XML.Event.Fs.Member_Ptr;

   function New_Documentation_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                      Kind : X_Proto_XML.Documentation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Documentation.Fs.Member_Ptr;

   function New_Union_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                             Kind : X_Proto_XML.Union.Fs.Child_Kind_Id_Type) return X_Proto_XML.Union.Fs.Child_Ptr;

   function New_Error_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                             Kind : X_Proto_XML.Error.Fs.Child_Kind_Id_Type) return X_Proto_XML.Error.Fs.Child_Ptr;

   function New_Request_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                               Kind : X_Proto_XML.Request.Fs.Child_Kind_Id_Type) return X_Proto_XML.Request.Fs.Child_Ptr;

   function New_Reply_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                             Kind : X_Proto_XML.Reply.Fs.Child_Kind_Id_Type) return X_Proto_XML.Reply.Fs.Child_Ptr;

   function New_Expression_Field_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                        Kind : X_Proto_XML.Expression_Field.Fs.Child_Kind_Id_Type) return X_Proto_XML.Expression_Field.Fs.Child_Ptr;


end X_Proto_XML.Allocators;
