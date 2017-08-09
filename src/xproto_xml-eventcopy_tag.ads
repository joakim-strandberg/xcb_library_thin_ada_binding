package XProto_XML.Eventcopy_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type Number_T is record
      Exists : Boolean := False;
      Value  : Aida.Nat32_T;
   end record;

   type Ref_Value_T is new Aida.Bounded_String.T (30);

   type Ref_T is record
      Exists : Boolean := False;
      Value  : Ref_Value_T;
   end record;

   type T is limited
      record
         Name   : Name_T;
         Number : Number_T;
         Ref    : Ref_T;
      end record;

end XProto_XML.Eventcopy_Tag;
