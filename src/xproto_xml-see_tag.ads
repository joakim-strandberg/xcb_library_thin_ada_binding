package XProto_XML.See_Tag with SPARK_Mode is

   type Kind_Value_T is new Aida.Bounded_String.T (20);

   type Kind_T is record
      Exists : Boolean := False;
      Value  : Kind_Value_T;
   end record;

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type T is limited
      record
         Kind : Kind_T;
         Name : Name_T;
      end record;

end XProto_XML.See_Tag;
