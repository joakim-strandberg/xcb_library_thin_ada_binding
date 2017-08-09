pragma Warnings (Off);
with Aida.Bounded_String; -- Withed because it is used in "all" child packages.
with Aida.Bounded_Vector; -- Withed because it is used in "all" child packages.
pragma Warnings (On);

package XProto_XML with SPARK_Mode is

   type Field_Id_T is new Integer range 1..10;

   subtype Extended_Field_Id_T is Field_Id_T'Base range 0..Field_Id_T'Last;

   type Op_Id_T is new Integer range 1..30;

   subtype Extended_Op_Id_T is Op_Id_T'Base range 0..Op_Id_T'Last;

   type Value_Id_T is new Integer range 1..30;

   subtype Extended_Value_Id_T is Value_Id_T'Base range 0..Value_Id_T'Last;

   type Fieldref_Id_T is new Integer range 1..30;

   subtype Extended_Fieldref_Id_T is Fieldref_Id_T'Base range 0..Fieldref_Id_T'Last;

   type CDATA_Id_T is new Integer range 1..30;

   subtype Extended_CDATA_Id_T is CDATA_Id_T'Base range 0..CDATA_Id_T'Last;

   type Pad_Id_T is new Integer range 1..30;

   subtype Extended_Pad_Id_T is Pad_Id_T'Base range 0..Pad_Id_T'Last;

   type See_Id_T is new Integer range 1..30;

   subtype Extended_See_Id_T is See_Id_T'Base range 0..See_Id_T'Last;

   type Error_Id_T is new Integer range 1..30;

   subtype Extended_Error_Id_T is Error_Id_T'Base range 0..Error_Id_T'Last;

   type Errorcopy_Id_T is new Integer range 1..30;

   subtype Extended_Errorcopy_Id_T is Errorcopy_Id_T'Base range 0..Errorcopy_Id_T'Last;

   type Example_Id_T is new Integer range 1..30;

   subtype Extended_Example_Id_T is Example_Id_T'Base range 0..Example_Id_T'Last;

   type Type_Id_T is new Integer range 1..30;

   subtype Extended_Type_Id_T is Type_Id_T'Base range 0..Type_Id_T'Last;

   type Item_Id_T is new Integer range 1..30;

   subtype Extended_Item_Id_T is Item_Id_T'Base range 0..Item_Id_T'Last;

   type Doc_Id_T is new Integer range 1..30;

   subtype Extended_Doc_Id_T is Doc_Id_T'Base range 0..Doc_Id_T'Last;

   type List_Id_T is new Integer range 1..30;

   subtype Extended_List_Id_T is List_Id_T'Base range 0..List_Id_T'Last;

   type Valueparam_Id_T is new Integer range 1..30;

   subtype Extended_Valueparam_Id_T is Valueparam_Id_T'Base range 0..Valueparam_Id_T'Last;

   type Reply_Id_T is new Integer range 1..30;

   subtype Extended_Reply_Id_T is Reply_Id_T'Base range 0..Reply_Id_T'Last;

   type Request_Id_T is new Integer range 1..30;

   subtype Extended_Request_Id_T is Request_Id_T'Base range 0..Request_Id_T'Last;

   type Exprfield_Id_T is new Integer range 1..30;

   subtype Extended_Exprfield_Id_T is Exprfield_Id_T'Base range 0..Exprfield_Id_T'Last;

   type Struct_Id_T is new Integer range 1..30;

   subtype Extended_Struct_Id_T is Struct_Id_T'Base range 0..Struct_Id_T'Last;

   type Xidtype_Id_T is new Integer range 1..30;

   subtype Extended_Xidtype_Id_T is Xidtype_Id_T'Base range 0..Xidtype_Id_T'Last;

   type Xidunion_Id_T is new Integer range 1..30;

   subtype Extended_Xidunion_Id_T is Xidunion_Id_T'Base range 0..Xidunion_Id_T'Last;

   type Typedef_Id_T is new Integer range 1..30;

   subtype Extended_Typedef_Id_T is Typedef_Id_T'Base range 0..Typedef_Id_T'Last;

   type Enum_Id_T is new Integer range 1..30;

   subtype Extended_Enum_Id_T is Enum_Id_T'Base range 0..Enum_Id_T'Last;

   type Event_Id_T is new Integer range 1..30;

   subtype Extended_Event_Id_T is Event_Id_T'Base range 0..Event_Id_T'Last;

   type Eventcopy_Id_T is new Integer range 1..30;

   subtype Extended_Eventcopy_Id_T is Eventcopy_Id_T'Base range 0..Eventcopy_Id_T'Last;

   type Union_Id_T is new Integer range 1..30;

   subtype Extended_Union_Id_T is Union_Id_T'Base range 0..Union_Id_T'Last;

   type Xcb_Id_T is new Integer range 1..1;

   subtype Extended_Xcb_Id_T is Xcb_Id_T'Base range 0..Xcb_Id_T'Last;

end XProto_XML;
