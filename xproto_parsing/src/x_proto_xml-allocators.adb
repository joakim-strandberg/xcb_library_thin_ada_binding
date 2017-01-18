with Ada.Unchecked_Conversion;

package body X_Proto_XML.Allocators is

   package body Alloc is

      function New_Xcb (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Xcb.Ptr is
         type Local_Xcb_Ptr is access all Xcb.T;
         for Local_Xcb_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Xcb_Ptr,
                                                           Target => Xcb.Ptr);

         L : constant Local_Xcb_Ptr := new Xcb.T;
      begin
         return Convert (L);
      end New_Xcb;

      function New_Struct (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Struct.Ptr is
         type Local_Struct_Ptr is access all Struct.T;
         for Local_Struct_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Struct_Ptr,
                                                           Target => Struct.Ptr);

         L : constant Local_Struct_Ptr := new Struct.T;
      begin
         return Convert (L);
      end New_Struct;

      function New_X_Id (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return X_Id.Ptr is
         type Local_X_Id_Ptr is access all X_Id.T;
         for Local_X_Id_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_X_Id_Ptr,
                                                           Target => X_Id.Ptr);

         L : constant Local_X_Id_Ptr := new X_Id.T;
      begin
         return Convert (L);
      end New_X_Id;

      function New_X_Id_Union (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return X_Id_Union.Ptr is
         type Local_X_Id_Union_Ptr is access all X_Id_Union.T;
         for Local_X_Id_Union_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_X_Id_Union_Ptr,
                                                           Target => X_Id_Union.Ptr);

         L : constant Local_X_Id_Union_Ptr := new X_Id_Union.T;
      begin
         return Convert (L);
      end New_X_Id_Union;

      function New_Type_Definition (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Type_Definition.Ptr is
         type Local_Type_Definition_Ptr is access all Type_Definition.T;
         for Local_Type_Definition_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Type_Definition_Ptr,
                                                           Target => Type_Definition.Ptr);

         L : constant Local_Type_Definition_Ptr := new Type_Definition.T;
      begin
         return Convert (L);
      end New_Type_Definition;

      function New_Enum (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Enum.Ptr is
         type Local_Enum_Ptr is access all Enum.T;
         for Local_Enum_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Enum_Ptr,
                                                           Target => Enum.Ptr);

         L : constant Local_Enum_Ptr := new Enum.T;
      begin
         return Convert (L);
      end New_Enum;

      function New_Event (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Event.Ptr is
         type Local_Event_Ptr is access all Event.T;
         for Local_Event_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Event_Ptr,
                                                           Target => Event.Ptr);

         L : constant Local_Event_Ptr := new Event.T;
      begin
         return Convert (L);
      end New_Event;

      function New_Event_Copy (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Event_Copy.Ptr is
         type Local_Event_Copy_Ptr is access all Event_Copy.T;
         for Local_Event_Copy_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Event_Copy_Ptr,
                                                           Target => Event_Copy.Ptr);

         L : constant Local_Event_Copy_Ptr := new Event_Copy.T;
      begin
         return Convert (L);
      end New_Event_Copy;

      function New_Union (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Union.Ptr is
         type Local_Union_Ptr is access all Union.T;
         for Local_Union_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Union_Ptr,
                                                           Target => Union.Ptr);

         L : constant Local_Union_Ptr := new Union.T;
      begin
         return Convert (L);
      end New_Union;

      function New_Error (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Error.Ptr is
         type Local_Error_Ptr is access all Error.T;
         for Local_Error_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Error_Ptr,
                                                           Target => Error.Ptr);

         L : constant Local_Error_Ptr := new Error.T;
      begin
         return Convert (L);
      end New_Error;

      function New_Error_Copy (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Error_Copy.Ptr is
         type Local_Error_Copy_Ptr is access all Error_Copy.T;
         for Local_Error_Copy_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Error_Copy_Ptr,
                                                           Target => Error_Copy.Ptr);

         L : constant Local_Error_Copy_Ptr := new Error_Copy.T;
      begin
         return Convert (L);
      end New_Error_Copy;

      function New_Request (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Request.Ptr is
         type Local_Request_Ptr is access all Request.T;
         for Local_Request_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Request_Ptr,
                                                           Target => Request.Ptr);

         L : constant Local_Request_Ptr := new Request.T;
      begin
         return Convert (L);
      end New_Request;

      function New_Field (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Field.Ptr is
         type Local_Field_Ptr is access all Field.T;
         for Local_Field_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Field_Ptr,
                                                           Target => Field.Ptr);

         L : constant Local_Field_Ptr := new Field.T;
      begin
         return Convert (L);
      end New_Field;

      function New_Pad (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Pad.Ptr is
         type Local_Pad_Ptr is access all Pad.T;
         for Local_Pad_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Pad_Ptr,
                                                           Target => Pad.Ptr);

         L : constant Local_Pad_Ptr := new Pad.T;
      begin
         return Convert (L);
      end New_Pad;

      function New_List (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return List.Ptr is
         type Local_List_Ptr is access all List.T;
         for Local_List_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_List_Ptr,
                                                           Target => List.Ptr);

         L : constant Local_List_Ptr := new List.T;
      begin
         return Convert (L);
      end New_List;

      function New_Struct_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                  Kind : X_Proto_XML.Struct.Fs.Member_Kind_Id.Enum_T) return X_Proto_XML.Struct.Fs.Member_Ptr
      is
         type Local_Struct_Member_Ptr is access all X_Proto_XML.Struct.Fs.Member_Type;
         for Local_Struct_Member_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Struct_Member_Ptr,
                                                           Target => X_Proto_XML.Struct.Fs.Member_Ptr);

         L : constant Local_Struct_Member_Ptr := new X_Proto_XML.Struct.Fs.Member_Type (Kind);
      begin
         return Convert (L);
      end New_Struct_Member;

      function New_Type (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Type_P.Ptr is
          type Local_Type_P_Ptr is access all Type_P.T;
          for Local_Type_P_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Type_P_Ptr,
                                                            Target => Type_P.Ptr);

          L : constant Local_Type_P_Ptr := new Type_P.T;
      begin
          return Convert (L);
      end New_Type;

      function New_Item (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Item.Ptr is
          type Local_Item_Ptr is access all Item.T;
          for Local_Item_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Item_Ptr,
                                                            Target => Item.Ptr);

          L : constant Local_Item_Ptr := new Item.T;
      begin
          return Convert (L);
      end New_Item;

      function New_Documentation (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Documentation.Ptr is
         type Local_Documentation_Ptr is access all Documentation.T;
         for Local_Documentation_Ptr'Storage_Pool use Pool.all;

         function Convert is new Ada.Unchecked_Conversion (Source => Local_Documentation_Ptr,
                                                           Target => Documentation.Ptr);

         L : constant Local_Documentation_Ptr := new Documentation.T;
      begin
         return Convert (L);
      end New_Documentation;

      function New_List_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                Kind : X_Proto_XML.List.Fs.Member_Kind_Id_Type) return X_Proto_XML.List.Fs.Member_Ptr
      is
          type Local_List_Member_Ptr is access all X_Proto_XML.List.Fs.Member_Type;
          for Local_List_Member_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_List_Member_Ptr,
                                                            Target => X_Proto_XML.List.Fs.Member_Ptr);

          L : constant Local_List_Member_Ptr := new X_Proto_XML.List.Fs.Member_Type (Kind);
      begin
          return Convert (L);
      end New_List_Member;

      function New_Operation (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access) return Operation.Ptr is
          type Local_Operation_Ptr is access all Operation.T;
          for Local_Operation_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Operation_Ptr,
                                                            Target => Operation.Ptr);

          L : constant Local_Operation_Ptr := new Operation.T;
      begin
          return Convert (L);
      end New_Operation;

      function New_Operation_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                     Kind : X_Proto_XML.Operation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Operation.Fs.Member_Ptr
      is
          type Local_Operation_Member_Ptr is access all X_Proto_XML.Operation.Fs.Member_Type;
          for Local_Operation_Member_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Operation_Member_Ptr,
                                                            Target => X_Proto_XML.Operation.Fs.Member_Ptr);

          L : constant Local_Operation_Member_Ptr := new X_Proto_XML.Operation.Fs.Member_Type (Kind);
      begin
          return Convert (L);
      end New_Operation_Member;

      function New_Event_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                 Kind : X_Proto_XML.Event.Fs.Member_Kind_Id_Type) return X_Proto_XML.Event.Fs.Member_Ptr
      is
          type Local_Event_Member_Ptr is access all X_Proto_XML.Event.Fs.Member_Type;
          for Local_Event_Member_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Event_Member_Ptr,
                                                            Target => X_Proto_XML.Event.Fs.Member_Ptr);

          L : constant Local_Event_Member_Ptr := new X_Proto_XML.Event.Fs.Member_Type (Kind);
      begin
          return Convert (L);
      end New_Event_Member;

      function New_Documentation_Member (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                         Kind : X_Proto_XML.Documentation.Fs.Member_Kind_Id_Type) return X_Proto_XML.Documentation.Fs.Member_Ptr
      is
          type Local_Documentation_Member_Ptr is access all X_Proto_XML.Documentation.Fs.Member_Type;
          for Local_Documentation_Member_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Documentation_Member_Ptr,
                                                            Target => X_Proto_XML.Documentation.Fs.Member_Ptr);

          L : constant Local_Documentation_Member_Ptr := new X_Proto_XML.Documentation.Fs.Member_Type (Kind);
      begin
          return Convert (L);
      end New_Documentation_Member;

      function New_Union_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                Kind : X_Proto_XML.Union.Fs.Child_Kind_Id_Type) return X_Proto_XML.Union.Fs.Child_Ptr
      is
          type Local_Union_Child_Ptr is access all X_Proto_XML.Union.Fs.Child_Type;
          for Local_Union_Child_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Union_Child_Ptr,
                                                            Target => X_Proto_XML.Union.Fs.Child_Ptr);

          L : constant Local_Union_Child_Ptr := new X_Proto_XML.Union.Fs.Child_Type (Kind);
      begin
          return Convert (L);
      end New_Union_Child;

      function New_Error_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                Kind : X_Proto_XML.Error.Fs.Child_Kind_Id_Type) return X_Proto_XML.Error.Fs.Child_Ptr
      is
          type Local_Error_Child_Ptr is access all X_Proto_XML.Error.Fs.Child_Type;
          for Local_Error_Child_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Error_Child_Ptr,
                                                            Target => X_Proto_XML.Error.Fs.Child_Ptr);

          L : constant Local_Error_Child_Ptr := new X_Proto_XML.Error.Fs.Child_Type (Kind);
      begin
          return Convert (L);
      end New_Error_Child;

      function New_Request_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                  Kind : X_Proto_XML.Request.Fs.Child_Kind_Id_Type) return X_Proto_XML.Request.Fs.Child_Ptr
      is
          type Local_Request_Child_Ptr is access all X_Proto_XML.Request.Fs.Child_Type;
          for Local_Request_Child_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Request_Child_Ptr,
                                                            Target => X_Proto_XML.Request.Fs.Child_Ptr);

          L : constant Local_Request_Child_Ptr := new X_Proto_XML.Request.Fs.Child_Type (Kind);
      begin
          return Convert (L);
      end New_Request_Child;

      function New_Reply_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                Kind : X_Proto_XML.Reply.Fs.Child_Kind_Id_Type) return X_Proto_XML.Reply.Fs.Child_Ptr
      is
          type Local_Reply_Child_Ptr is access all X_Proto_XML.Reply.Fs.Child_Type;
          for Local_Reply_Child_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Reply_Child_Ptr,
                                                            Target => X_Proto_XML.Reply.Fs.Child_Ptr);

          L : constant Local_Reply_Child_Ptr := new X_Proto_XML.Reply.Fs.Child_Type (Kind);
      begin
          return Convert (L);
      end New_Reply_Child;

      function New_Expression_Field_Child (Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool_Access;
                                           Kind : X_Proto_XML.Expression_Field.Fs.Child_Kind_Id_Type) return X_Proto_XML.Expression_Field.Fs.Child_Ptr
      is
          type Local_Expression_Field_Child_Ptr is access all X_Proto_XML.Expression_Field.Fs.Child_Type;
          for Local_Expression_Field_Child_Ptr'Storage_Pool use Pool.all;

          function Convert is new Ada.Unchecked_Conversion (Source => Local_Expression_Field_Child_Ptr,
                                                            Target => X_Proto_XML.Expression_Field.Fs.Child_Ptr);

          L : constant Local_Expression_Field_Child_Ptr := new X_Proto_XML.Expression_Field.Fs.Child_Type (Kind);
      begin
          return Convert (L);
      end New_Expression_Field_Child;

   end Alloc;

end X_Proto_XML.Allocators;
