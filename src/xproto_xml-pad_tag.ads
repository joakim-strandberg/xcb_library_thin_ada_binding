package XProto_XML.Pad_Tag with SPARK_Mode is

   type Bytes_T is record
      Exists : Boolean := False;
      Value  : Positive;
   end record;

   type T is limited
      record
         Bytes : Bytes_T;
      end record;

end XProto_XML.Pad_Tag;
