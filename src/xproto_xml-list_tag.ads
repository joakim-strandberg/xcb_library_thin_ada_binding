package XProto_XML.List_Tag with SPARK_Mode is

   type Type_Attribute_Value_T is new Aida.Bounded_String.T (30);

   type Type_Attribute_T is record
      Exists : Boolean := False;
      Value  : Type_Attribute_Value_T;
   end record;

   type Name_Attribute_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Attribute_Value_T;
   end record;

   type Child_Kind_Id_T is (
                            Child_Kind_Field_Reference,
                            Child_Kind_Value,
                            Child_Kind_Operation
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Kind_Value) is record
      case Kind_Id is
         when Child_Kind_Field_Reference => Fieldref_Id : Fieldref_Id_T;
         when Child_Kind_Value           => Value_Id    : Value_Id_T;
         when Child_Kind_Operation       => Op_Id       : Op_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id  => Child_Kind_Value,
                                             Value_Id => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Type_Attr : Type_Attribute_T;
         Name      : Name_T;
         Children  : Child_Vector.T (50);
      end record;

end XProto_XML.List_Tag;
