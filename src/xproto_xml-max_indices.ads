package XProto_XML.Max_Indices with SPARK_Mode is

   type T is tagged limited private;

   function Field_Id_Max (This : T) return Extended_Field_Id_T;

   procedure Allocate_Field_Id (This : in out T;
                                Id   : out Field_Id_T) with
     Global     => null,
     Pre'Class  => Field_Id_Max (This) < Extended_Field_Id_T'Last,
     Post'Class => Field_Id_Max (This) = Field_Id_Max (This)'Old + 1;

   function Xcb_Id_Max (This : T) return Extended_Xcb_Id_T;

   procedure Allocate_Xcb_Id (This : in out T;
                              Id   : out Xcb_Id_T) with
     Global     => null,
     Pre'Class  => Xcb_Id_Max (This) < Extended_Xcb_Id_T'Last,
     Post'Class => Xcb_Id_Max (This) = Xcb_Id_Max (This)'Old + 1;

private

   type T is tagged limited record
      My_Xcb_Id_Max   : Extended_Xcb_Id_T   := 0;
      My_Field_Id_Max : Extended_Field_Id_T := 0;
   end record;

   function Field_Id_Max (This : T) return Extended_Field_Id_T is (This.My_Field_Id_Max);

   function Xcb_Id_Max (This : T) return Extended_Xcb_Id_T is (This.My_Xcb_Id_Max);

end XProto_XML.Max_Indices;
