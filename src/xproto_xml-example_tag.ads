package XProto_XML.Example_Tag with SPARK_Mode is

   type CDATA_Value_T is new Aida.Bounded_String.T (300);

   type CDATA_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : CDATA_Value_T;
         when False => null;
      end case;
   end record;

   type T is limited
      record
         Value : CDATA_T;
      end record;

end XProto_XML.Example_Tag;
