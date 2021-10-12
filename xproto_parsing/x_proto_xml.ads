with Aida.Containers.Bounded_Vector;
with Aida.Bounded_String;
with System.Storage_Elements;
with Basic_Bounded_Dynamic_Pools;
pragma Elaborate_All (Basic_Bounded_Dynamic_Pools);

package X_Proto_XML is

   Pool : Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool (Size => 220_000_000);

   package Large_Bounded_String is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 3_000);

   package Field is

      type Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Enum_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Mask_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Value_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Alt_Enum_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
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

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Field;

   package Pad is

      type Bytes_T is record
         Exists : Boolean := False;
         Value  : Positive;
      end record;

      type T is limited
         record
            Bytes : Bytes_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Pad;

   package Field_Reference is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 1_00);

   subtype Field_Reference_T is Field_Reference.T;

   type Field_Reference_Ptr is access all Field_Reference_T;
   for Field_Reference_Ptr'Storage_Pool use Pool;

   type Value_T is new Natural;

   type Value_Ptr is access all Value_T;
   for Value_Ptr'Storage_Pool use Pool;

   package Operation is

      type Op_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

      type Child_Kind_Id_T is (
                               Child_Kind_Field_Reference,
                               Child_Kind_Value,
                               Child_Operation
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Kind_Field_Reference => Field_Reference : aliased Field_Reference_T;
            when Child_Kind_Value           => Value           : aliased Value_T;
            when Child_Operation            => Op              : Ptr;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 50);

      type T is limited
         record
            Op       : Op_T;
            Children : Child_Vector.T;
         end record;

   end Operation;

   subtype Operation_T is Operation.T;

   package List is

      type Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_T is (
                               Child_Kind_Field_Reference,
                               Child_Kind_Value,
                               Child_Kind_Operation
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is limited record
         case Kind_Id is
            when Child_Kind_Field_Reference => Field_Reference : Large_Bounded_String.T;
            when Child_Kind_Value           => Value           : Value_Ptr     := new Value_T;
            when Child_Kind_Operation       => Op              : Operation.Ptr := new Operation_T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 50);

      type T is limited
         record
            Kind     : Kind_T;
            Name     : Name_T;
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end List;

   package Item is

      type Kind_Id_T is (
                         Not_Specified,
                         Specified_As_Value,
                         Specified_As_Bit
                        );

      type Bit_T is new Natural;

      type Bit_Ptr is access all Bit_T;

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited record
         Kind_Id : Kind_Id_T := Not_Specified;
         Name    : Name_T;
         Value   : Value_T;
         Bit     : Bit_T;
      end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Item;

   package Expression_Field is

      type Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_T is (
                               Child_Operation
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Operation  => Op : aliased Operation.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Kind     : Kind_T;
            Name     : Name_T;
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Expression_Field;

   package Error is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_T is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Value_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_T is (
                               Child_Field,
                               Child_Pad
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Field  => F : aliased Field.T;
            when Child_Pad    => P : aliased Pad.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Name     : Name_T;
            Number   : Number_T;
            Kind     : Kind_T;
            Value    : Value_T;
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Error;

   package Error_Copy is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_T is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type Ref_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Name   : Name_T;
            Number : Number_T;
            Ref    : Ref_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Error_Copy;

   package Example is

      type Value_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Value : Value_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Example;

   package See is

      type Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Kind : Kind_T;
            Name : Name_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end See;

   package Documentation is

      type Brief_Description_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Description_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_T is (
                               Child_Field,
                               Child_See,
                               Child_Error,
                               Child_Example
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Field   => F  : aliased Field.T;
            when Child_See     => S  : aliased See.T;
            when Child_Error   => E  : aliased Error.T;
            when Child_Example => Ex : aliased Example.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Brief_Description : Brief_Description_T;
            Description       : Description_T;
            Children          : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Documentation;

   package Event_Copy is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_T is record
         Exists : Boolean := False;
         Value  : Natural;
      end record;

      type Ref_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Name   : Name_T;
            Number : Number_T;
            Ref    : Ref_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Event_Copy;

   package X_Id is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Name : Name_T;
         end record;

      type Ptr is access T;
      for Ptr'Storage_Pool use Pool;

   end X_Id;

   package Type_P is

      type Value_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Value : Value_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Type_P;

   package X_Id_Union is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Type_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Type_P.Ptr,
                                                                 "="        => Type_P."=",
                                                                 MAX_LENGTH => 30);

      type T is limited
         record
            Name  : Name_T;
            Kinds : Type_Vector.T;
         end record;

      type Ptr is access T;
      for Ptr'Storage_Pool use Pool;

   end X_Id_Union;

   package Type_Definition is

      type Old_Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type New_Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Old_Name : Old_Name_T;
            New_Name : New_Name_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Type_Definition;

   package Enum is

      type Name_T is record
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
            Name           : Name_T;
            Items          : Item_Vector.T;
            Documentations : Documentation_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Enum;

   package Union is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Child_Kind_Id_T is (
                               Child_List
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_List  => L : aliased List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 10);

      type T is limited
         record
            Name     : Name_T;
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Union;

   package Struct is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      package Child_Kind_Id is
         type Enum_T is (
                         Field_Child,
                         Pad_Child,
                         List_Child
                        );
      end Child_Kind_Id;

      use Child_Kind_Id;

      type Child_T (Kind_Id : Child_Kind_Id.Enum_T) is record
         case Kind_Id is
            when Field_Child => F : Field.Ptr := new Field.T;
            when Pad_Child   => P : Pad.Ptr   := new Pad.T;
            when List_Child  => L : List.Ptr  := new List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Name     : Name_T;
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Struct;

   package Event is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Number_T is record
         Exists : Boolean := False;
         Value2  : Natural;
      end record;

      type No_Sequence_Number_T is record
         Exists : Boolean := False;
         Value  : Boolean;
      end record;

      type XGE_T is record
         Exists : Boolean := False;
         Value  : Boolean;
      end record;

      type Child_Kind_Id_T is (
                               Child_Field,
                               Child_Pad,
                               Child_Doc,
                               Child_List
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Field => F : aliased Field.T;
            when Child_Pad   => P : aliased Pad.T;
            when Child_Doc   => D : aliased Documentation.T;
            when Child_List  => L : aliased List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Name               : Name_T;
            Number             : Number_T;
            No_Sequence_Number : No_Sequence_Number_T;
            Children           : Child_Vector.T;
            XGE                : XGE_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Event;

   package Value_Param is

      type Mask_Kind_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type Mask_Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type List_Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
      end record;

      type T is limited
         record
            Mask_Kind : Mask_Kind_T;
            Mask_Name : Mask_Name_T;
            List_Name : List_Name_T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Value_Param;

   package Reply is

      type Child_Kind_Id_T is (
                               Child_Field,
                               Child_Pad,
                               Child_Documentation,
                               Child_List
                              );

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
         case Kind_Id is
            when Child_Field         => F : aliased Field.T;
            when Child_Pad           => P : aliased Pad.T;
            when Child_Documentation => D : aliased Documentation.T;
            when Child_List          => L : aliased List.T;
         end case;
      end record;

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Children : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Reply;

   package Request is

      type Name_T is record
         Exists : Boolean := False;
         Value  : Large_Bounded_String.T;
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

      type Child_T (Kind_Id : Child_Kind_Id_T) is record
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

      type Child_Ptr is access all Child_T;
      for Child_Ptr'Storage_Pool use Pool;

      package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                  "="        => "=",
                                                                  MAX_LENGTH => 30);

      type T is limited
         record
            Name                   : Name_T;
            Op_Code                : Op_Code_T;
            Shall_Combine_Adjacent : Shall_Combine_Adjacent_T;
            Children               : Child_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Request;

   package Xcb is

      type Header_T is record
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
            Header           : Header_T;
            Structs          : Struct_Vector.T;
            X_Ids            : X_Id_Vector.T;
            X_Id_Unions      : X_Id_Union_Vector.T;
            Type_Definitions : Type_Definition_Vector.T;
            Enums            : Enum_Vector.T;
            Events           : Event_Vector.T;
            Event_Copies     : Event_Copy_Vector.T;
            Unions           : Union_Vector.T;
            Errors           : Error_Vector.T;
            Error_Copies     : Error_Copy_Vector.T;
            Requests         : Request_Vector.T;
         end record;

      type Ptr is access all T;
      for Ptr'Storage_Pool use Pool;

   end Xcb;

end X_Proto_XML;
