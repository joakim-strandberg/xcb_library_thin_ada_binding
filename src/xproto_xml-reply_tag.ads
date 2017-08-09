package XProto_XML.Reply_Tag with SPARK_Mode is

   type Child_Kind_Id_T is (
                            Child_Field,
                            Child_Pad,
                            Child_Documentation,
                            Child_List
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Pad) is record
      case Kind_Id is
         when Child_Field         => F : Field_Id_T;
         when Child_Pad           => P : Pad_Id_T;
         when Child_Documentation => D : Doc_Id_T;
         when Child_List          => L : List_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Child_Pad,
                                             P       => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Children : Child_Vector.T (30);
      end record;

end XProto_XML.Reply_Tag;
