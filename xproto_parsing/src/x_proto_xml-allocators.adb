package body X_Proto_XML.Allocators is

   use X_Proto_XML.Struct.Fs.Member_Kind_Id;

   subtype Struct_Field_Member_T is X_Proto_XML.Struct.Fs.Member_Type (Field_Member);

   type Struct_Field_Member_Ptr is access Struct_Field_Member_T;

   function New_Struct_Field is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Struct_Field_Member_T,
                                                                                     Allocation_Type_Access => Struct_Field_Member_Ptr);

   function New_Struct_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr is
      SFM : constant Struct_Field_Member_Ptr := New_Struct_Field (Subpool);
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (SFM);
   end New_Struct_Field;

   subtype Struct_Pad_Member_T is X_Proto_XML.Struct.Fs.Member_Type (Pad_Member);

   type Struct_Pad_Member_Ptr is access Struct_Pad_Member_T;

   function New_Struct_Pad is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Struct_Pad_Member_T,
                                                                                   Allocation_Type_Access => Struct_Pad_Member_Ptr);

   function New_Struct_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr is
      SFM : constant Struct_Pad_Member_Ptr := New_Struct_Pad (Subpool);
   begin
      return X_Proto_XML.Struct.Fs.Member_Ptr (SFM);
   end New_Struct_Pad;

end X_Proto_XML.Allocators;
