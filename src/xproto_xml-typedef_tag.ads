package XProto_XML.Typedef_Tag with SPARK_Mode is

   type Old_Name_Value_T is new Aida.Bounded_String.T (20);

   type Old_Name_T is record
      Exists : Boolean := False;
      Value  : Old_Name_Value_T;
   end record;

   type New_Name_Value_T is new Aida.Bounded_String.T (20);

   type New_Name_T is record
      Exists : Boolean := False;
      Value  : New_Name_Value_T;
   end record;

   type T is limited
      record
         Old_Name : Old_Name_T;
         New_Name : New_Name_T;
      end record;

end XProto_XML.Typedef_Tag;
