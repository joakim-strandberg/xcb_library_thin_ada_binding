package body X_Proto_XML.Allocators is

   use X_Proto_XML.Struct.Fs.Member_Kind_Id;

   subtype Struct_Field_Member_T is X_Proto_XML.Struct.Fs.Member_Type (Field_Member);

   type Struct_Field_Member_Ptr is access Struct_Field_Member_T;

   function New_Struct_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Struct_Field_Member_T,
                                                                                                     Allocation_Type_Access => Struct_Field_Member_Ptr);

   function New_Struct_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr is
      SFM : constant Struct_Field_Member_Ptr := New_Struct_Field (Subpool);
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (SFM);
   end New_Struct_Field;

   subtype Struct_Pad_Member_T is X_Proto_XML.Struct.Fs.Member_Type (Pad_Member);

   type Struct_Pad_Member_Ptr is access Struct_Pad_Member_T;

   function New_Struct_Pad is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Struct_Pad_Member_T,
                                                                                                   Allocation_Type_Access => Struct_Pad_Member_Ptr);

   function New_Struct_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr is
      SFM : constant Struct_Pad_Member_Ptr := New_Struct_Pad (Subpool);
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (SFM);
   end New_Struct_Pad;

   subtype Struct_List_Member_T is X_Proto_XML.Struct.Fs.Member_Type (List_Member);

   type Struct_List_Member_Ptr is access Struct_List_Member_T;

   function New_Struct_List is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Struct_List_Member_T,
                                                                                                    Allocation_Type_Access => Struct_List_Member_Ptr);

   function New_Struct_List (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr is
      SFM : constant Struct_List_Member_Ptr := New_Struct_List (Subpool);
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (SFM);
   end New_Struct_List;

   subtype List_Operation_Member_T is X_Proto_XML.List.Fs.Member_Type (X_Proto_XML.List.Fs.List_Member_Kind_Operation);

   type List_Operation_Member_Ptr is access List_Operation_Member_T;

   function New_List_Operation is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => List_Operation_Member_T,
                                                                                                       Allocation_Type_Access => List_Operation_Member_Ptr);

   function New_List_Operation (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.List.Fs.Member_Ptr is
      SFM : constant List_Operation_Member_Ptr := New_List_Operation (Subpool);
   begin
      return X_Proto_XML.List.Fs.Member_Ptr (SFM);
   end New_List_Operation;

   subtype List_Value_Member_T is X_Proto_XML.List.Fs.Member_Type (X_Proto_XML.List.Fs.List_Member_Kind_Value);

   type List_Value_Member_Ptr is access List_Value_Member_T;

   function New_List_Value is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => List_Value_Member_T,
                                                                                                   Allocation_Type_Access => List_Value_Member_Ptr);

   function New_List_Value (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.List.Fs.Member_Ptr is
      SFM : constant List_Value_Member_Ptr := New_List_Value (Subpool);
   begin
      return X_Proto_XML.List.Fs.Member_Ptr (SFM);
   end New_List_Value;

   subtype Operation_Operation_Member_T is X_Proto_XML.Operation.Fs.Member_Type (X_Proto_XML.Operation.Fs.Member_Operation);

   type Operation_Operation_Member_Ptr is access Operation_Operation_Member_T;

   function New_Operation_Operation is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Operation_Operation_Member_T,
                                                                                                            Allocation_Type_Access => Operation_Operation_Member_Ptr);

   function New_Operation_Operation (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Operation.Fs.Member_Ptr is
      SFM : constant Operation_Operation_Member_Ptr := New_Operation_Operation (Subpool);
   begin
      return X_Proto_XML.Operation.Fs.Member_Ptr (SFM);
   end New_Operation_Operation;

   subtype Operation_Field_Reference_Member_T is X_Proto_XML.Operation.Fs.Member_Type (X_Proto_XML.Operation.Fs.Member_Kind_Field_Reference);

   type Operation_Field_Reference_Member_Ptr is access Operation_Field_Reference_Member_T;

   function New_Operation_Field_Reference is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Operation_Field_Reference_Member_T,
                                                                                                                  Allocation_Type_Access => Operation_Field_Reference_Member_Ptr);

   function New_Operation_Field_Reference (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Operation.Fs.Member_Ptr is
      SFM : constant Operation_Field_Reference_Member_Ptr := New_Operation_Field_Reference (Subpool);
   begin
      return X_Proto_XML.Operation.Fs.Member_Ptr (SFM);
   end New_Operation_Field_Reference;

   subtype Operation_Value_Member_T is X_Proto_XML.Operation.Fs.Member_Type (X_Proto_XML.Operation.Fs.Member_Kind_Value);

   type Operation_Value_Member_Ptr is access Operation_Value_Member_T;

   function New_Operation_Value is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Operation_Value_Member_T,
                                                                                                        Allocation_Type_Access => Operation_Value_Member_Ptr);

   function New_Operation_Value (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Operation.Fs.Member_Ptr is
      SFM : constant Operation_Value_Member_Ptr := New_Operation_Value (Subpool);
   begin
      return X_Proto_XML.Operation.Fs.Member_Ptr (SFM);
   end New_Operation_Value;

   subtype Event_Field_Member_T is X_Proto_XML.Event.Fs.Member_Type (X_Proto_XML.Event.Fs.Event_Member_Field);

   type Event_Field_Member_Ptr is access Event_Field_Member_T;

   function New_Event_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Event_Field_Member_T,
                                                                                                    Allocation_Type_Access => Event_Field_Member_Ptr);

   function New_Event_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Event.Fs.Member_Ptr is
      SFM : constant Event_Field_Member_Ptr := New_Event_Field (Subpool);
   begin
      return X_Proto_XML.Event.Fs.Member_Ptr (SFM);
   end New_Event_Field;

   subtype Event_Pad_Member_T is X_Proto_XML.Event.Fs.Member_Type (X_Proto_XML.Event.Fs.Event_Member_Pad);

   type Event_Pad_Member_Ptr is access Event_Pad_Member_T;

   function New_Event_Pad is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Event_Pad_Member_T,
                                                                                                  Allocation_Type_Access => Event_Pad_Member_Ptr);

   function New_Event_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Event.Fs.Member_Ptr is
      SFM : constant Event_Pad_Member_Ptr := New_Event_Pad (Subpool);
   begin
      return X_Proto_XML.Event.Fs.Member_Ptr (SFM);
   end New_Event_Pad;

   subtype Event_Doc_Member_T is X_Proto_XML.Event.Fs.Member_Type (X_Proto_XML.Event.Fs.Event_Member_Doc);

   type Event_Doc_Member_Ptr is access Event_Doc_Member_T;

   function New_Event_Doc is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Event_Doc_Member_T,
                                                                                                  Allocation_Type_Access => Event_Doc_Member_Ptr);

   function New_Event_Doc (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Event.Fs.Member_Ptr is
      SFM : constant Event_Doc_Member_Ptr := New_Event_Doc (Subpool);
   begin
      return X_Proto_XML.Event.Fs.Member_Ptr (SFM);
   end New_Event_Doc;

   subtype Event_List_Member_T is X_Proto_XML.Event.Fs.Member_Type (X_Proto_XML.Event.Fs.Event_Member_List);

   type Event_List_Member_Ptr is access Event_List_Member_T;

   function New_Event_List is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Event_List_Member_T,
                                                                                                   Allocation_Type_Access => Event_List_Member_Ptr);

   function New_Event_List (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Event.Fs.Member_Ptr is
      SFM : constant Event_List_Member_Ptr := New_Event_List (Subpool);
   begin
      return X_Proto_XML.Event.Fs.Member_Ptr (SFM);
   end New_Event_List;

   subtype Documentation_Field_Member_T is X_Proto_XML.Documentation.Fs.Member_Type (X_Proto_XML.Documentation.Fs.Member_Field);

   type Documentation_Field_Member_Ptr is access Documentation_Field_Member_T;

   function New_Documentation_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Documentation_Field_Member_T,
                                                                                                            Allocation_Type_Access => Documentation_Field_Member_Ptr);

   function New_Documentation_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Documentation.Fs.Member_Ptr is
      SFM : constant Documentation_Field_Member_Ptr := New_Documentation_Field (Subpool);
   begin
      return X_Proto_XML.Documentation.Fs.Member_Ptr (SFM);
   end New_Documentation_Field;

   subtype Documentation_See_Member_T is X_Proto_XML.Documentation.Fs.Member_Type (X_Proto_XML.Documentation.Fs.Member_See);

   type Documentation_See_Member_Ptr is access Documentation_See_Member_T;

   function New_Documentation_See is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Documentation_See_Member_T,
                                                                                                          Allocation_Type_Access => Documentation_See_Member_Ptr);

   function New_Documentation_See (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Documentation.Fs.Member_Ptr is
      SFM : constant Documentation_See_Member_Ptr := New_Documentation_See (Subpool);
   begin
      return X_Proto_XML.Documentation.Fs.Member_Ptr (SFM);
   end New_Documentation_See;

   subtype Documentation_Error_Member_T is X_Proto_XML.Documentation.Fs.Member_Type (X_Proto_XML.Documentation.Fs.Member_Error);

   type Documentation_Error_Member_Ptr is access Documentation_Error_Member_T;

   function New_Documentation_Error is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Documentation_Error_Member_T,
                                                                                                            Allocation_Type_Access => Documentation_Error_Member_Ptr);

   function New_Documentation_Error (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Documentation.Fs.Member_Ptr is
      SFM : constant Documentation_Error_Member_Ptr := New_Documentation_Error (Subpool);
   begin
      return X_Proto_XML.Documentation.Fs.Member_Ptr (SFM);
   end New_Documentation_Error;

   subtype Documentation_Example_Member_T is X_Proto_XML.Documentation.Fs.Member_Type (X_Proto_XML.Documentation.Fs.Member_Example);

   type Documentation_Example_Member_Ptr is access Documentation_Example_Member_T;

   function New_Documentation_Example is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Documentation_Example_Member_T,
                                                                                                              Allocation_Type_Access => Documentation_Example_Member_Ptr);

   function New_Documentation_Example (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Documentation.Fs.Member_Ptr is
      SFM : constant Documentation_Example_Member_Ptr := New_Documentation_Example (Subpool);
   begin
      return X_Proto_XML.Documentation.Fs.Member_Ptr (SFM);
   end New_Documentation_Example;

   subtype Union_List_Member_T is X_Proto_XML.Union.Fs.Child_Type (X_Proto_XML.Union.Fs.Child_List);

   type Union_List_Member_Ptr is access Union_List_Member_T;

   function New_Union_List is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Union_List_Member_T,
                                                                                                   Allocation_Type_Access => Union_List_Member_Ptr);

   function New_Union_List (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Union.Fs.Child_Ptr is
      SFM : constant Union_List_Member_Ptr := New_Union_List (Subpool);
   begin
      return X_Proto_XML.Union.Fs.Child_Ptr (SFM);
   end New_Union_List;

   subtype Error_Field_Member_T is X_Proto_XML.Error.Fs.Child_Type (X_Proto_XML.Error.Fs.Child_Field);

   type Error_Field_Member_Ptr is access Error_Field_Member_T;

   function New_Error_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Error_Field_Member_T,
                                                                                                    Allocation_Type_Access => Error_Field_Member_Ptr);

   function New_Error_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Error.Fs.Child_Ptr is
      SFM : constant Error_Field_Member_Ptr := New_Error_Field (Subpool);
   begin
      return X_Proto_XML.Error.Fs.Child_Ptr (SFM);
   end New_Error_Field;

   subtype Error_Pad_Member_T is X_Proto_XML.Error.Fs.Child_Type (X_Proto_XML.Error.Fs.Child_Pad);

   type Error_Pad_Member_Ptr is access Error_Pad_Member_T;

   function New_Error_Pad is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Error_Pad_Member_T,
                                                                                                  Allocation_Type_Access => Error_Pad_Member_Ptr);

   function New_Error_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Error.Fs.Child_Ptr is
      SFM : constant Error_Pad_Member_Ptr := New_Error_Pad (Subpool);
   begin
      return X_Proto_XML.Error.Fs.Child_Ptr (SFM);
   end New_Error_Pad;

   subtype Request_Field_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Field);

   type Request_Field_Member_Ptr is access Request_Field_Member_T;

   function New_Request_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Field_Member_T,
                                                                                                      Allocation_Type_Access => Request_Field_Member_Ptr);

   function New_Request_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Field_Member_Ptr := New_Request_Field (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Field;

   subtype Request_Pad_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Pad);

   type Request_Pad_Member_Ptr is access Request_Pad_Member_T;

   function New_Request_Pad is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Pad_Member_T,
                                                                                                    Allocation_Type_Access => Request_Pad_Member_Ptr);

   function New_Request_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Pad_Member_Ptr := New_Request_Pad (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Pad;

   subtype Request_Value_Param_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Value_Param);

   type Request_Value_Param_Member_Ptr is access Request_Value_Param_Member_T;

   function New_Request_Value_Param is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Value_Param_Member_T,
                                                                                                            Allocation_Type_Access => Request_Value_Param_Member_Ptr);

   function New_Request_Value_Param (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Value_Param_Member_Ptr := New_Request_Value_Param (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Value_Param;

   subtype Request_Documentation_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Documentation);

   type Request_Documentation_Member_Ptr is access Request_Documentation_Member_T;

   function New_Request_Documentation is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Documentation_Member_T,
                                                                                                              Allocation_Type_Access => Request_Documentation_Member_Ptr);

   function New_Request_Documentation (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Documentation_Member_Ptr := New_Request_Documentation (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Documentation;

   subtype Request_Reply_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Reply);

   type Request_Reply_Member_Ptr is access Request_Reply_Member_T;

   function New_Request_Reply is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Reply_Member_T,
                                                                                                      Allocation_Type_Access => Request_Reply_Member_Ptr);

   function New_Request_Reply (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Reply_Member_Ptr := New_Request_Reply (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Reply;

   subtype Request_List_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_List);

   type Request_List_Member_Ptr is access Request_List_Member_T;

   function New_Request_List is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_List_Member_T,
                                                                                                     Allocation_Type_Access => Request_List_Member_Ptr);

   function New_Request_List (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_List_Member_Ptr := New_Request_List (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_List;

   subtype Request_Expression_Field_Member_T is X_Proto_XML.Request.Fs.Child_Type (X_Proto_XML.Request.Fs.Child_Expression_Field);

   type Request_Expression_Field_Member_Ptr is access Request_Expression_Field_Member_T;

   function New_Request_Expression_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Request_Expression_Field_Member_T,
                                                                                                                 Allocation_Type_Access => Request_Expression_Field_Member_Ptr);

   function New_Request_Expression_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Request.Fs.Child_Ptr is
      SFM : constant Request_Expression_Field_Member_Ptr := New_Request_Expression_Field (Subpool);
   begin
      return X_Proto_XML.Request.Fs.Child_Ptr (SFM);
   end New_Request_Expression_Field;

   subtype Reply_Field_Member_T is X_Proto_XML.Reply.Fs.Child_Type (X_Proto_XML.Reply.Fs.Child_Field);

   type Reply_Field_Member_Ptr is access Reply_Field_Member_T;

   function New_Reply_Field is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Reply_Field_Member_T,
                                                                                                    Allocation_Type_Access => Reply_Field_Member_Ptr);

   function New_Reply_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Reply.Fs.Child_Ptr is
      SFM : constant Reply_Field_Member_Ptr := New_Reply_Field (Subpool);
   begin
      return X_Proto_XML.Reply.Fs.Child_Ptr (SFM);
   end New_Reply_Field;

   subtype Reply_Pad_Member_T is X_Proto_XML.Reply.Fs.Child_Type (X_Proto_XML.Reply.Fs.Child_Pad);

   type Reply_Pad_Member_Ptr is access Reply_Pad_Member_T;

   function New_Reply_Pad is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Reply_Pad_Member_T,
                                                                                                  Allocation_Type_Access => Reply_Pad_Member_Ptr);

   function New_Reply_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Reply.Fs.Child_Ptr is
      SFM : constant Reply_Pad_Member_Ptr := New_Reply_Pad (Subpool);
   begin
      return X_Proto_XML.Reply.Fs.Child_Ptr (SFM);
   end New_Reply_Pad;

   subtype Reply_Documentation_Member_T is X_Proto_XML.Reply.Fs.Child_Type (X_Proto_XML.Reply.Fs.Child_Documentation);

   type Reply_Documentation_Member_Ptr is access Reply_Documentation_Member_T;

   function New_Reply_Documentation is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Reply_Documentation_Member_T,
                                                                                                            Allocation_Type_Access => Reply_Documentation_Member_Ptr);

   function New_Reply_Documentation (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Reply.Fs.Child_Ptr is
      SFM : constant Reply_Documentation_Member_Ptr := New_Reply_Documentation (Subpool);
   begin
      return X_Proto_XML.Reply.Fs.Child_Ptr (SFM);
   end New_Reply_Documentation;

   subtype Reply_List_Member_T is X_Proto_XML.Reply.Fs.Child_Type (X_Proto_XML.Reply.Fs.Child_List);

   type Reply_List_Member_Ptr is access Reply_List_Member_T;

   function New_Reply_List is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Reply_List_Member_T,
                                                                                                   Allocation_Type_Access => Reply_List_Member_Ptr);

   function New_Reply_List (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Reply.Fs.Child_Ptr is
      SFM : constant Reply_List_Member_Ptr := New_Reply_List (Subpool);
   begin
      return X_Proto_XML.Reply.Fs.Child_Ptr (SFM);
   end New_Reply_List;

   subtype Expression_Field_Operation_Member_T is X_Proto_XML.Expression_Field.Fs.Child_Type (X_Proto_XML.Expression_Field.Fs.Child_Operation);

   type Expression_Field_Operation_Member_Ptr is access Expression_Field_Operation_Member_T;

   function New_Expression_Field_Operation is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => Expression_Field_Operation_Member_T,
                                                                                                                   Allocation_Type_Access => Expression_Field_Operation_Member_Ptr);

   function New_Expression_Field_Operation (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Expression_Field.Fs.Child_Ptr is
      SFM : constant Expression_Field_Operation_Member_Ptr := New_Expression_Field_Operation (Subpool);
   begin
      return X_Proto_XML.Expression_Field.Fs.Child_Ptr (SFM);
   end New_Expression_Field_Operation;

   subtype List_Field_Reference_Member_T is X_Proto_XML.List.Fs.Member_Type (X_Proto_XML.List.Fs.List_Member_Kind_Field_Reference);

   type List_Field_Reference_Member_Ptr is access List_Field_Reference_Member_T;

   function New_List_Field_Reference is new Bounded_Dynamic_Pools.Allocation_Of_Tiny_Item_In_Scoped_Subpool (Allocation_Type        => List_Field_Reference_Member_T,
                                                                                                             Allocation_Type_Access => List_Field_Reference_Member_Ptr);

   function New_List_Field_Reference (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.List.Fs.Member_Ptr is
      SFM : constant List_Field_Reference_Member_Ptr := New_List_Field_Reference (Subpool);
   begin
      return X_Proto_XML.List.Fs.Member_Ptr (SFM);
   end New_List_Field_Reference;

end X_Proto_XML.Allocators;
