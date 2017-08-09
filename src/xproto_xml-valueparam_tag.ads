package XProto_XML.Valueparam_Tag with SPARK_Mode is

   type Mask_Kind_Value_T is new Aida.Bounded_String.T (30);

   type Mask_Kind_T is record
      Exists : Boolean := False;
      Value  : Mask_Kind_Value_T;
   end record;

   type Mask_Name_Value_T is new Aida.Bounded_String.T (30);

   type Mask_Name_T is record
      Exists : Boolean := False;
      Value  : Mask_Name_Value_T;
   end record;

   type List_Name_Value_T is new Aida.Bounded_String.T (30);

   type List_Name_T is record
      Exists : Boolean := False;
      Value  : List_Name_Value_T;
   end record;

   type T is limited
      record
         Mask_Kind : Mask_Kind_T;
         Mask_Name : Mask_Name_T;
         List_Name : List_Name_T;
      end record;

end XProto_XML.Valueparam_Tag;
