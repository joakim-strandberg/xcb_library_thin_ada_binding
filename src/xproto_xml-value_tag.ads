package XProto_XML.Value_Tag with SPARK_Mode is

   type Value_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value  : Natural;
         when False => null;
      end case;
   end record;

   type T is limited
      record
         Value : Value_T;
      end record;

end XProto_XML.Value_Tag;
