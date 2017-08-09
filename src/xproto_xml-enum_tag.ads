package XProto_XML.Enum_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type Child_Kind_Id_T is (
                            Child_Item,
                            Child_Doc
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Item) is record
      case Kind_Id is
         when Child_Item => Item_Id : Item_Id_T;
         when Child_Doc  =>  Doc_Id :  Doc_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Child_Item,
                                             Item_Id => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Name     : Name_T;
         Children : Child_Vector.T (100);
      end record;

end XProto_XML.Enum_Tag;
