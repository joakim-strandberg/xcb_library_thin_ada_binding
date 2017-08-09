package XProto_XML.Xcb_Tag with SPARK_Mode is

   type Header_Value_T is new Aida.Bounded_String.T (30);

   type Header_T is record
      Exists : Boolean := False;
      Value  : Header_Value_T;
   end record;

   function Default_Struct return Struct_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Struct);

   package Struct_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                     Element_T       => Struct_Id_T,
                                                     Default_Element => Default_Struct);

   function Default_Xidtype return Xidtype_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Xidtype);

   package X_Id_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                   Element_T       => Xidtype_Id_T,
                                                   Default_Element => Default_Xidtype);

   function Default_Xidunion return Xidunion_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Xidtype);

   package X_Id_Union_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                         Element_T       => Xidunion_Id_T,
                                                         Default_Element => Default_Xidunion);

   function Default_Typedef return Typedef_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Typedef);

   package Type_Definition_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                              Element_T       => Typedef_Id_T,
                                                              Default_Element => Default_Typedef);

   function Default_Enum return Enum_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Enum);

   package Enum_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                   Element_T       => Enum_Id_T,
                                                   Default_Element => Default_Enum);

   function Default_Event return Event_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Event);

   package Event_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Event_Id_T,
                                                    Default_Element => Default_Event);

   function Default_Eventcopy return Eventcopy_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Eventcopy);

   package Event_Copy_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                         Element_T       => Eventcopy_Id_T,
                                                         Default_Element => Default_Eventcopy);

   function Default_Union return Union_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Union);

   package Union_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Union_Id_T,
                                                    Default_Element => Default_Union);

   function Default_Error return Error_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Error);

   package Error_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                    Element_T       => Error_Id_T,
                                                    Default_Element => Default_Error);

   function Default_Errorcopy return Errorcopy_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Errorcopy);

   package Error_Copy_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                         Element_T       => Errorcopy_Id_T,
                                                         Default_Element => Default_Errorcopy);

   function Default_Request return Request_Id_T is (1);
   pragma Annotate (GNATprove, Terminating, Default_Request);

   package Request_Vector is new Aida.Bounded_Vector (Index_T         => Aida.Pos32_T,
                                                      Element_T       => Request_Id_T,
                                                      Default_Element => Default_Request);

   type T is limited
      record
         Header           : Header_T;
         Structs          : Struct_Vector.T (400);
         X_Ids            : X_Id_Vector.T (100);
         X_Id_Unions      : X_Id_Union_Vector.T (100);
         Type_Definitions : Type_Definition_Vector.T (100);
         Enums            : Enum_Vector.T (100);
         Events           : Event_Vector.T (100);
         Event_Copies     : Event_Copy_Vector.T (100);
         Unions           : Union_Vector.T (100);
         Errors           : Error_Vector.T (100);
         Error_Copies     : Error_Copy_Vector.T (100);
         Requests         : Request_Vector.T (150);
      end record;

end XProto_XML.Xcb_Tag;
