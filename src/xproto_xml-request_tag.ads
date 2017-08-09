package XProto_XML.Request_Tag with SPARK_Mode is

   type Name_Value_T is new Aida.Bounded_String.T (30);

   type Name_T is record
      Exists : Boolean := False;
      Value  : Name_Value_T;
   end record;

   type Op_Code_T is record
      Exists : Boolean := False;
      Value  : Natural;
   end record;

   type Shall_Combine_Adjacent_T is record
      Exists : Boolean := False;
      Value  : Boolean;
   end record;

   type Child_Kind_Id_T is (
                            Child_Field,
                            Child_Pad,
                            Child_Value_Param,
                            Child_Documentation,
                            Child_Reply,
                            Child_List,
                            Child_Expression_Field
                           );

   type Child_T (Kind_Id : Child_Kind_Id_T := Child_Pad) is record
      case Kind_Id is
         when Child_Field            => F  : Field_Id_T;
         when Child_Pad              => P  : Pad_Id_T;
         when Child_Value_Param      => V  : Valueparam_Id_T;
         when Child_Documentation    => D  : Doc_Id_T;
         when Child_Reply            => R  : Reply_Id_T;
         when Child_List             => L  : List_Id_T;
         when Child_Expression_Field => EF : Exprfield_Id_T;
      end case;
   end record;

   function Default_Child return Child_T is (Kind_Id => Child_Pad,
                                             P       => 1);
   pragma Annotate (GNATprove, Terminating, Default_Child);

   package Child_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Child_T,
                                                    Default_Element => Default_Child);

   type T is limited
      record
         Name                   : Name_T;
         Op_Code                : Op_Code_T;
         Shall_Combine_Adjacent : Shall_Combine_Adjacent_T;
         Children               : Child_Vector.T (30);
      end record;

end XProto_XML.Request_Tag;
