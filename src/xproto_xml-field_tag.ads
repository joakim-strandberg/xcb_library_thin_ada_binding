package XProto_XML.Field_Tag with SPARK_Mode is

   type Kind_T (Exists : Boolean := False) is record
      case Exists is
         when True  => Value  : Aida.Bounded_String.T (100);
         when False => null;
      end case;
   end record;

   type Name_T is record
      Exists : Boolean := False;
      Value  : Aida.Bounded_String.T (100);
   end record;

   type Enum_T is record
      Exists : Boolean := False;
      Value  : Aida.Bounded_String.T (100);
   end record;

   type Mask_T is record
      Exists : Boolean := False;
      Value  : Aida.Bounded_String.T (100);
   end record;

   type Value_T is record
      Exists : Boolean := False;
      Value  : Aida.Bounded_String.T (100);
   end record;

   type Alt_Enum_T is record
      Exists : Boolean := False;
      Value  : Aida.Bounded_String.T (100);
   end record;

   type T is limited
      record
         Kind     : Kind_T;
         Name     : Name_T;
         Enum     : Enum_T;
         Mask     : Mask_T;
         Alt_Enum : Alt_Enum_T;
         Value    : Value_T;
      end record;

end XProto_XML.Field_Tag;
