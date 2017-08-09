package XProto_XML.Xidunion_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (20);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   function Default_Value return Type_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Value);

   package Type_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                   Element_T       => Type_Id_T,
                                                   Default_Element => Default_Value);

   type T is limited
      record
         Name  : Name_T;
         Kinds : Type_Vector.T (30);
      end record;

end XProto_XML.Xidunion_Tag;
