package XProto_XML.Fieldref_Tag with SPARK_Mode is

   type Value_T is new Aida.Bounded_String.T (30);

   type T is limited
      record
         Value : Value_T;
      end record;

end XProto_XML.Fieldref_Tag;
