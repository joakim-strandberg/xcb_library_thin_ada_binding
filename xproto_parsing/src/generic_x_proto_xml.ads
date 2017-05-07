with Aida.Containers.Bounded_Vector;
with Aida.Bounded_String;
with System.Storage_Elements;
with Basic_Bounded_Dynamic_Pools;

generic
   Size : System.Storage_Elements.Storage_Offset;
package Generic_X_Proto_XML is

   Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool (Size           => Size,
                                                          Heap_Allocated => True);

   package Large_Bounded_String is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 3_000);

   package Field is

      type Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Enum_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Mask_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Value_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Alt_Enum_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Kind     : aliased Kind_Type;
            Name     : aliased Name_Type;
            Enum     : aliased Enum_Type;
            Mask     : aliased Mask_Type;
            Alt_Enum : aliased Alt_Enum_Type;
            Value    : aliased Value_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Field;

   package Pad is

      type Bytes_Type is record
         Exists : Boolean := False;
         Value  : Positive;
      end record;

      type T is limited
         record
            Bytes : aliased Bytes_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Pad;

   package Field_Reference is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 1_00);

   subtype Field_Reference_Type is Field_Reference.T;

   type Field_Reference_Access_Type is access all Field_Reference_Type;
   for Field_Reference_Access_Type'Storage_Pool use Pool;

   type Value_Type is new Natural;

   type Value_Access_Type is access all Value_Type;
   for Value_Access_Type'Storage_Pool use Pool;

   package Operation is

      type Op_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

      type Member_Kind_Id_Type is (
                                   Member_Kind_Field_Reference,
                                   Member_Kind_Value,
                                   Member_Operation
                                  );

      type Member_Type (Kind_Id : Member_Kind_Id_Type) is record
         case Kind_Id is
            when Member_Kind_Field_Reference => Field_Reference : aliased Field_Reference_Type;
            when Member_Kind_Value           => Value           : aliased Value_Type;
            when Member_Operation            => Operation       : aliased Ptr;
         end case;
      end record;

      type Member_Ptr is access all Member_Type;
      for Member_Ptr'Storage_Pool use Pool;

      package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                   "="        => "=",
                                                                   MAX_LENGTH => 50);

      type T is limited
         record
            Op      : aliased Op_Type;
            Members : aliased Member_Vector.T;
         end record;

   end Operation;

   subtype Operation_T is Operation.T;

   package List is

      type Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Member_Kind_Id_Type is (
                                   List_Member_Kind_Field_Reference,
                                   List_Member_Kind_Value,
                                   List_Member_Kind_Operation
                                  );

      type Member_Type (Kind_Id : Member_Kind_Id_Type) is limited record
         case Kind_Id is
            when List_Member_Kind_Field_Reference => Field_Reference : Large_Bounded_String.T;
            when List_Member_Kind_Value           => Value           : aliased Value_Type;
            when List_Member_Kind_Operation       => Operation       : aliased Operation_T;
         end case;
      end record;

      type Member_Ptr is access all Member_Type;
      for Member_Ptr'Storage_Pool use Pool;

      package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                   "="        => "=",
                                                                   MAX_LENGTH => 50);

      type T is limited
         record
            Kind    : aliased Kind_Type;
            Name    : aliased Name_Type;
            Members : aliased Member_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end List;

   package Item is

      type Kind_Id_Type is (
                            Not_Specified,
                            Specified_As_Value,
                            Specified_As_Bit
                           );

      type Bit_Type is new Natural;

      type Bit_Ptr is access all Bit_Type;

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited record
         Kind_Id : aliased Kind_Id_Type := Not_Specified;
         Name    : aliased Name_Type;
         Value   : aliased Value_Type;
         Bit     : aliased Bit_Type;
      end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Item;

   package Expression_Field is

      type Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_Type is (
                                  Child_Operation
                                 );

      type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
         case Kind_Id is
            when Child_Operation  => Op : aliased Operation.T;
         end case;
      end record;

      type Child_Ptr is access all Child_Type;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Kind     : aliased Kind_Type;
            Name     : aliased Name_Type;
            Children : aliased Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Expression_Field;

   package Error is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_Type is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Value_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_Type is (
                                  Child_Field,
                                  Child_Pad
                                 );

      type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
         case Kind_Id is
            when Child_Field  => F : aliased Field.T;
            when Child_Pad    => P : aliased Pad.T;
         end case;
      end record;

      type Child_Ptr is access all Child_Type;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Name     : aliased Name_Type;
            Number   : aliased Number_Type;
            Kind     : aliased Kind_Type;
            Value    : aliased Value_Type;
            Children : aliased Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Error;

   package Error_Copy is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Number_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Ref_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

      type T is limited
         record
            Name   : aliased Name_Type;
            Number : aliased Number_Type;
            Ref    : aliased Ref_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Error_Copy;

   package Example is

      type Value_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Value : aliased Value_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Example;

   package See is

      type Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Kind : aliased Kind_Type;
            Name : aliased Name_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end See;

   package Documentation is

      type Brief_Description_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Description_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Member_Kind_Id_Type is (
                                   Member_Field,
                                   Member_See,
                                   Member_Error,
                                   Member_Example
                                  );

      type Member_Type (Kind_Id : Member_Kind_Id_Type) is record
         case Kind_Id is
            when Member_Field   => F  : aliased Field.T;
            when Member_See     => S  : aliased See.T;
            when Member_Error   => E  : aliased Error.T;
            when Member_Example => Ex : aliased Example.T;
         end case;
      end record;

      type Member_Ptr is access all Member_Type;
      for Member_Ptr'Storage_Pool use Pool;

      package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                   "="        => "=",
                                                                   MAX_LENGTH => 30);

      type T is limited
         record
            Brief_Description : aliased Brief_Description_Type;
            Description       : aliased Description_Type;
            Members           : aliased Member_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Documentation;

   package Event_Copy is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_Type is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type Ref_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Name   : aliased Name_Type;
            Number : aliased Number_Type;
            Ref    : aliased Ref_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Event_Copy;

   package X_Id is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Name : aliased Name_Type;
         end record;

      type Ptr is access T;
      for Ptr'Storage_Pool use Pool;

   end X_Id;

   package Type_P is

      type Value_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Value : aliased Value_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Type_P;

   package X_Id_Union is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Type_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Type_P.Ptr,
                                                                 "="        => Type_P."=",
                                                                 MAX_LENGTH => 30);

      type T is limited
         record
            Name  : aliased Name_Type;
            Kinds : aliased Type_Vector.T;
         end record;

      type Ptr is access T;
      for Ptr'Storage_Pool use Pool;

   end X_Id_Union;

   package Type_Definition is

      type Old_Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type New_Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Old_Name : aliased Old_Name_Type;
            New_Name : aliased New_Name_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Type_Definition;

   package Enum is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Item_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Item.Ptr,
                                                                 "="        => Item."=",
                                                                 MAX_LENGTH => 100);

      package Documentation_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Documentation.Ptr,
                                                                          "="        => Documentation."=",
                                                                          MAX_LENGTH => 30);

      type T is limited
         record
            Name           : aliased Name_Type;
            Items          : aliased Item_Vector.T;
            Documentations : aliased Documentation_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Enum;

   package Union is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_Type is (
                                  Child_List
                                 );

      type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
         case Kind_Id is
            when Child_List  => L : aliased List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_Type;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 10);

      type T is limited
         record
            Name     : aliased Name_Type;
            Children : aliased Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Union;

   package Struct is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Member_Kind_Id is
         type Enum_T is (
                         Field_Member,
                         Pad_Member,
                         List_Member
                        );
      end Member_Kind_Id;

      use Member_Kind_Id;

      type Member_Type (Kind_Id : Member_Kind_Id.Enum_T) is record
         case Kind_Id is
            when Field_Member => F : aliased Field.T;
            when Pad_Member   => P : aliased Pad.T;
            when List_Member  => L : aliased List.T;
         end case;
      end record;

      type Member_Ptr is access all Member_Type;
      for Member_Ptr'Storage_Pool use Pool;

      package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                   "="        => "=",
                                                                   MAX_LENGTH => 30);

      type T is limited
         record
            Name    : aliased Name_Type;
            Members : aliased Member_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Struct;

   package Event is

      type Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_Type is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type No_Sequence_Number_Type is record
         Exists : Boolean := False;
         Value  : Boolean;
      end record;

      type XGE_Type is record
         Exists : Boolean := False;
         Value  : Boolean;
      end record;

      type Member_Kind_Id_Type is (
                                   Event_Member_Field,
                                   Event_Member_Pad,
                                   Event_Member_Doc,
                                   Event_Member_List
                                  );

      type Member_Type (Kind_Id : Member_Kind_Id_Type) is record
         case Kind_Id is
            when Event_Member_Field => F : aliased Field.T;
            when Event_Member_Pad   => P : aliased Pad.T;
            when Event_Member_Doc   => D : aliased Documentation.T;
            when Event_Member_List  => L : aliased List.T;
         end case;
      end record;

      type Member_Ptr is access all Member_Type;
      for Member_Ptr'Storage_Pool use Pool;

      package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                   "="        => "=",
                                                                   MAX_LENGTH => 30);

      type T is limited
         record
            Name               : aliased Name_Type;
            Number             : aliased Number_Type;
            No_Sequence_Number : aliased No_Sequence_Number_Type;
            Members            : aliased Member_Vector.T;
            XGE                : aliased XGE_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Event;

   package Value_Param is

      type Mask_Kind_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Mask_Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type List_Name_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Mask_Kind : aliased Mask_Kind_Type;
            Mask_Name : aliased Mask_Name_Type;
            List_Name : aliased List_Name_Type;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Value_Param;

   package Reply is

      type Child_Kind_Id_Type is (
                                  Child_Field,
                                  Child_Pad,
                                  Child_Documentation,
                                  Child_List
                                 );

      type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
         case Kind_Id is
            when Child_Field         => F : aliased Field.T;
            when Child_Pad           => P : aliased Pad.T;
            when Child_Documentation => D : aliased Documentation.T;
            when Child_List          => L : aliased List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_Type;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Children : aliased Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Reply;

   package Request is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Op_Code_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Shall_Combine_Adjacent_Type is record
            Exists : Boolean := False;
            Value  : Boolean;
         end record;

         type Child_Kind_Id_Type is (
                                     Child_Field,
                                     Child_Pad,
                                     Child_Value_Param,
                                     Child_Documentation,
                                     Child_Reply,
                                     Child_List,
                                     Child_Expression_Field
                                    );

         type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
            case Kind_Id is
              when Child_Field            => F  : aliased Field.T;
              when Child_Pad              => P  : aliased Pad.T;
              when Child_Value_Param      => V  : aliased Value_Param.T;
              when Child_Documentation    => D  : aliased Documentation.T;
              when Child_Reply            => R  : aliased Reply.T;
              when Child_List             => L  : aliased List.T;
              when Child_Expression_Field => EF : aliased Expression_Field.T;
            end case;
         end record;

         type Child_Ptr is access all Child_Type;
         for Child_Ptr'Storage_Pool use Pool;

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                     "="        => "=",
                                                                     MAX_LENGTH => 30);

      type T is limited
         record
            Name                   : aliased Name_Type;
            Op_Code                : aliased Op_Code_Type;
            Shall_Combine_Adjacent : aliased Shall_Combine_Adjacent_Type;
            Children               : aliased Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Request;

   package Xcb is

      type Header_Type is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Struct_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Struct.Ptr,
                                                                   "="        => Struct."=",
                                                                   MAX_LENGTH => 400);

      package X_Id_Vector is new Aida.Containers.Bounded_Vector (Element_T  => X_Id.Ptr,
                                                                 "="        => X_Id."=",
                                                                 MAX_LENGTH => 100);

      package X_Id_Union_Vector is new Aida.Containers.Bounded_Vector (Element_T  => X_Id_Union.Ptr,
                                                                       "="        => X_Id_Union."=",
                                                                       MAX_LENGTH => 100);

      package Type_Definition_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Type_Definition.Ptr,
                                                                            "="        => Type_Definition."=",
                                                                            MAX_LENGTH => 100);

      package Enum_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Enum.Ptr,
                                                                 "="        => Enum."=",
                                                                 MAX_LENGTH => 100);

      package Event_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Event.Ptr,
                                                                  "="        => Event."=",
                                                                  MAX_LENGTH => 100);

      package Event_Copy_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Event_Copy.Ptr,
                                                                       "="        => Event_Copy."=",
                                                                       MAX_LENGTH => 100);

      package Union_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Union.Ptr,
                                                                  "="        => Union."=",
                                                                  MAX_LENGTH => 100);

      package Error_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Error.Ptr,
                                                                  "="        => Error."=",
                                                                  MAX_LENGTH => 100);

      package Error_Copy_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Error_Copy.Ptr,
                                                                       "="        => Error_Copy."=",
                                                                       MAX_LENGTH => 100);

      package Request_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Request.Ptr,
                                                                    "="        => Request."=",
                                                                    MAX_LENGTH => 150);

      type T is limited
         record
            Header           : aliased Header_Type;
            Structs          : aliased Struct_Vector.T;
            X_Ids            : aliased X_Id_Vector.T;
            X_Id_Unions      : aliased X_Id_Union_Vector.T;
            Type_Definitions : aliased Type_Definition_Vector.T;
            Enums            : aliased Enum_Vector.T;
            Events           : aliased Event_Vector.T;
            Event_Copies     : aliased Event_Copy_Vector.T;
            Unions           : aliased Union_Vector.T;
            Errors           : aliased Error_Vector.T;
            Error_Copies     : aliased Error_Copy_Vector.T;
            Requests         : aliased Request_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Xcb;

end Generic_X_Proto_XML;
