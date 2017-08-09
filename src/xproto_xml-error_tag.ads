package XProto_XML.Error_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type Number_T is record
      Exists : Boolean := False;
      Value  : Natural;
   end record;

   type CDATA_Value_T is new Aida.Bounded_String.T (100);

   type CDATA_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : CDATA_Value_T;
         when False => null;
      end case;
   end record;

   type Kind_Value_T is new Aida.Bounded_String.T (30);

   type Kind_T is record
      Exists : Boolean := False;
      Value  : Kind_Value_T;
   end record;

   type Child_Kind_Id_T is (
                            Child_Field,
                            Child_Pad
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Pad) is record
      case Kind_Id is
         when Child_Field  => F : Field_Id_T;
         when Child_Pad    => P : Pad_Id_T;
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
         Name     : Name_T;
         Number   : Number_T;
         Kind     : Kind_T;
         CDATA    : CDATA_T;
         Children : Child_Vector.T (30);
      end record;

end XProto_XML.Error_Tag;
