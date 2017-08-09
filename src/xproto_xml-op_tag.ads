package XProto_XML.Op_Tag with SPARK_Mode is

   type Op_Value_T is new Aida.Bounded_String.T (30);

   type Op_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Op_Value_T;
         when False => null;
      end case;
   end record;

   type Child_Kind_Id_T is (
                            Child_Kind_Field_Reference,
                            Child_Kind_Value,
                            Child_Operation
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Operation) is record
      case Kind_Id is
         when Child_Kind_Field_Reference => Fieldref_Id : Fieldref_Id_T;
         when Child_Kind_Value           => Value_Id    : Value_Id_T;
         when Child_Operation            => Op_Id       : Op_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Child_Operation,
                                             Op_Id   => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Op       : Op_T;
         Children : Child_Vector.T (50);
      end record;

end XProto_XML.Op_Tag;
