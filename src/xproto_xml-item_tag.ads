package XProto_XML.Item_Tag with SPARK_Mode is

   type Kind_Id_T is (
                      Not_Specified,
                      Specified_As_Value,
                      Specified_As_Bit
                     );

   type Bit_T is new Natural;

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type T is limited record
      Kind_Id : Kind_Id_T := Not_Specified;
      Name    : Name_T;
      Value   : Value_Id_T; -- TODO: Can be improved?
      Bit     : Bit_T;
   end record;

end XProto_XML.Item_Tag;
