package XProto_XML.Current_Ids with SPARK_Mode is

   function Default_Doc_Id return Doc_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Doc_Id);

   package Doc_Id_Vector is new Aida.Bounded_Vector (Index_T         => Doc_Id_T,
                                                     Element_T       => Doc_Id_T,
                                                     Default_Element => Default_Doc_Id);

   function Default_Xcb_Id return Xcb_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Xcb_Id);

   package Xcb_Id_Vector is new Aida.Bounded_Vector (Index_T         => Xcb_Id_T,
                                                     Element_T       => Xcb_Id_T,
                                                     Default_Element => Default_Xcb_Id);

   type T is limited record
      Doc_Ids  : Doc_Id_Vector.T (3);
      Xcb_Ids  : Xcb_Id_Vector.T (1);
   end record;

end XProto_XML.Current_Ids;
