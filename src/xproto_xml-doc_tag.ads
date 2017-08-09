package XProto_XML.Doc_Tag with SPARK_Mode is

   type Brief_Description_Value_T is new Aida.Bounded_String.T (400);

   type Brief_Description_T is record
      Exists : Boolean := False;
      Value  : Brief_Description_Value_T;
   end record;

   type Description_Value_T is new Aida.Bounded_String.T (300);

   type Description_T is record
      Exists : Boolean := False;
      Value  : Description_Value_T;
   end record;

   type Child_Kind_Id_T is (
                            Child_Field,
                            Child_See,
                            Child_Error,
                            Child_Example
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_See) is record
      case Kind_Id is
         when Child_Field   => F  : Field_Id_T;
         when Child_See     => S  : See_Id_T;
         when Child_Error   => E  : Error_Id_T;
         when Child_Example => Ex : Example_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Child_See,
                                             S       => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Brief_Description : Brief_Description_T;
         Description       : Description_T;
         Children          : Child_Vector.T (30);
      end record;

end XProto_XML.Doc_Tag;
