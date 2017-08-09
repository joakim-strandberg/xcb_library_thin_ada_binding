package body XProto_XML.Max_Indices is

   procedure Allocate_Field_Id (This : in out T;
                                Id   :    out Field_Id_T)
   is
   begin
      This.My_Field_Id_Max := This.My_Field_Id_Max + 1;
      Id := This.My_Field_Id_Max;
   end Allocate_Field_Id;

   procedure Allocate_Xcb_Id (This : in out T;
                              Id   :    out Xcb_Id_T)
   is
   begin
      This.My_Xcb_Id_Max := This.My_Xcb_Id_Max + 1;
      Id := This.My_Xcb_Id_Max;
   end Allocate_Xcb_Id;

end XProto_XML.Max_Indices;
