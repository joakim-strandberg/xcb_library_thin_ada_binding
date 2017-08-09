package XProto_XML.Type_Tag with SPARK_Mode is

   type Value_T is new Aida.Bounded_String.T (20);

   type Nullable_Value_T is record
      Exists : Boolean := False;
      Value  : Value_T;
   end record;

   type T is limited
      record
         Value : Nullable_Value_T;
      end record;

end XProto_XML.Type_Tag;
