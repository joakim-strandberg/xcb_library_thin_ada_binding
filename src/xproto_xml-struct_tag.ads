package XProto_XML.Struct_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type Child_Kind_Id_T is (
                            Field_Child,
                            Pad_Child,
                            List_Child
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Field_Child) is record
      case Kind_Id is
         when Field_Child => F : Field_Id_T;
         when Pad_Child   => P : Pad_Id_T;
         when List_Child  => L : List_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Field_Child,
                                             F       => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Name     : Name_T;
         Children : Child_Vector.T (30);
      end record;

end XProto_XML.Struct_Tag;
