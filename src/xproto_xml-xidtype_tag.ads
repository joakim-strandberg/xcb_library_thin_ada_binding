package XProto_XML.Xidtype_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (20);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type T is limited
      record
         Name : Name_T;
      end record;

end XProto_XML.Xidtype_Tag;
