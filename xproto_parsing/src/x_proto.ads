with Ada.Containers.Vectors;
with Aida.Strings;
with Ada.Unchecked_Deallocation;

package X_Proto is

   type XCB_Header_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Type_Definition_Old_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Type_Definition_New_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Pad_Bytes_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Positive;
         when False => null;
      end case;
   end record;

   type Enum_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Item_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Enum_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type List_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type List_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Mask_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Operation_Op_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_Number_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Documentation_Brief_Description_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type See_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type See_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Value_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Field_Alt_Enum_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_Copy_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_Copy_Number_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Event_Copy_Ref_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_No_Sequence_Number_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Union_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Documentation_Description_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Event_XGE_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Error_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Error_Number_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Error_Copy_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Error_Copy_Number_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Error_Copy_Ref_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Request_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Request_Op_Code_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Value_Param_Mask_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Value_Param_Mask_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Value_Param_List_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Error_Value_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Error_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Reply_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Reply_Op_Code_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Natural;
         when False => null;
      end case;
   end record;

   type Example_Value_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Expression_Field_Kind_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Expression_Field_Name_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
         when False => null;
      end case;
   end record;

   type Request_Shall_Combine_Adjacent_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Boolean;
         when False => null;
      end case;
   end record;

   type Field_Type is tagged limited private;

   function Kind (This : Field_Type) return Field_Kind_Type;

   function Name (This : Field_Type) return Field_Name_Type;

   function Enum (This : Field_Type) return Field_Enum_Type;

   function Mask (This : Field_Type) return Field_Mask_Type;

   function Value (This : Field_Type) return Field_Value_Type;

   function Alt_Enum (This : Field_Type) return Field_Alt_Enum_Type;

   type Field_Access_Type is access all Field_Type;

   type Pad_Type is tagged limited private;

   function Bytes (This : Pad_Type) return Pad_Bytes_Type;

   type Pad_Access_Type is access all Pad_Type;

   type Field_Reference_Type is new Aida.Strings.Unbounded_String_Type with null record;

   type Field_Reference_Access_Type is access all Field_Reference_Type;

   type Value_Type is new Natural;

   type Value_Access_Type is access all Value_Type;

   type Operation_Type;

   type Operation_Access_Type is access all Operation_Type;

   type Operation_Member_Kind_Id_Type is (
                                          Operation_Member_Kind_Field_Reference,
                                          Operation_Member_Kind_Value,
                                          Operation_Member_Operation
                                          );

   type Operation_Member_Type (Kind_Id : Operation_Member_Kind_Id_Type) is record
      case Kind_Id is
         when Operation_Member_Kind_Field_Reference => Field_Reference : aliased Field_Reference_Type;
         when Operation_Member_Kind_Value           => Value           : aliased Value_Type;
         when Operation_Member_Operation            => Operation       : aliased Operation_Access_Type;
      end case;
   end record;

   type Operation_Member_Access_Type is access all Operation_Member_Type;

   package Operation_Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                   Element_Type => Operation_Member_Access_Type);

   type Operation_Type is tagged limited private;

   function Op (This : Operation_Type) return Operation_Op_Type;

   function Members (This : Operation_Type) return Operation_Member_Vectors.Vector;

   type List_Member_Kind_Id_Type is (
                                     List_Member_Kind_Field_Reference,
                                     List_Member_Kind_Value,
                                     List_Member_Kind_Operation
                                     );

   type List_Member_Type (Kind_Id : List_Member_Kind_Id_Type) is record
      case Kind_Id is
         when List_Member_Kind_Field_Reference => Field_Reference : Aida.Strings.Unbounded_String_Type;
         when List_Member_Kind_Value           => Value           : aliased Value_Type;
         when List_Member_Kind_Operation       => Operation       : aliased Operation_Type;
      end case;
   end record;

   type List_Member_Access_Type is access all List_Member_Type;

   package List_Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => List_Member_Access_Type);

   type List_Type is tagged limited private;

   function Kind (This : List_Type) return List_Kind_Type;

   function Name (This : List_Type) return List_Name_Type;

   function Members (This : List_Type) return List_Member_Vectors.Vector;



   type Enum_Item_Kind_Id_Type is (
                                   Not_Specified,
                                   Specified_As_Value,
                                   Specified_As_Bit
                                  );

   type Bit_Type is new Natural;

   type Bit_Access_Type is access all Bit_Type;

   type Item_Type is record
      Kind_Id : Enum_Item_Kind_Id_Type := Not_Specified;
      Name    : Item_Name_Type;
      Value   : Value_Type;
      Bit     : Bit_Type;
   end record;

   type Item_Access_Type is access all Item_Type;

   procedure Free is new Ada.Unchecked_Deallocation (Object => Item_Type,
                                                     Name   => Item_Access_Type);

   package Item_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                       Element_Type => Item_Access_Type);

   type Expression_Field_Child_Kind_Id_Type is (
                                     Expression_Field_Child_Operation
                                     );

   type Expression_Field_Child_Type (Kind_Id : Expression_Field_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Expression_Field_Child_Operation  => Op : aliased Operation_Type;
      end case;
   end record;

   type Expression_Field_Child_Access_Type is access all Expression_Field_Child_Type;

   package Expression_Field_Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                         Element_Type => Expression_Field_Child_Access_Type);

   type Expression_Field_Type is tagged limited private;

   function Kind (This : Expression_Field_Type) return Expression_Field_Kind_Type;

   function Name (This : Expression_Field_Type) return Expression_Field_Name_Type;

   function Children (This : Expression_Field_Type) return Expression_Field_Child_Vectors.Vector;

   type Expression_Field_Access_Type is access all Expression_Field_Type;

   type Error_Child_Kind_Id_Type is (
                                     Error_Child_Field,
                                     Error_Child_Pad
                                     );

   type Error_Child_Type (Kind_Id : Error_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Error_Child_Field  => F : aliased Field_Type;
         when Error_Child_Pad    => P : aliased Pad_Type;
      end case;
   end record;

   type Error_Child_Access_Type is access all Error_Child_Type;

   package Error_Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Error_Child_Access_Type);

   type Error_Type is tagged limited private;

   function Name (This : Error_Type) return Error_Name_Type;

   function Number (This : Error_Type) return Error_Number_Type;

   function Kind (This : Error_Type) return Error_Kind_Type;

   function Value (This : Error_Type) return Error_Value_Type;

   function Children (This : Error_Type) return Error_Child_Vectors.Vector;

   type Error_Access_Type is access all Error_Type;

   package Error_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Error_Access_Type);

   type Error_Copy_Type is tagged limited private;

   function Name (This : Error_Copy_Type) return Error_Copy_Name_Type;

   function Number (This : Error_Copy_Type) return Error_Copy_Number_Type;

   function Ref (This : Error_Copy_Type) return Error_Copy_Ref_Type;

   type Error_Copy_Access_Type is access all Error_Copy_Type;

   package Error_Copy_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Error_Copy_Access_Type);

   type Example_Type is tagged limited private;

   function Value (This : Example_Type) return Example_Value_Type;

   type Example_Access_Type is access all Example_Type;

   type See_Type is tagged limited private;

   function Kind (This : See_Type) return See_Kind_Type;

   function Name (This : See_Type) return See_Name_Type;

   type See_Access_Type is access all See_Type;

   type Documentation_Member_Kind_Id_Type is (
                                              Documentation_Member_Field,
                                              Documentation_Member_See,
                                              Documentation_Member_Error,
                                              Documentation_Member_Example
                                             );

   type Documentation_Member_Type (Kind_Id : Documentation_Member_Kind_Id_Type) is record
      case Kind_Id is
         when Documentation_Member_Field   => F : aliased Field_Type;
         when Documentation_Member_See     => S : aliased See_Type;
         when Documentation_Member_Error   => E : aliased Error_Type;
         when Documentation_Member_Example => Ex : aliased Example_Type;
      end case;
   end record;

   type Documentation_Member_Access_Type is access all Documentation_Member_Type;

   package Documentation_Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                       Element_Type => Documentation_Member_Access_Type);

   type Documentation_Type is tagged limited private;

   function Brief_Description (This : Documentation_Type) return Documentation_Brief_Description_Type;

   function Description (This : Documentation_Type) return Documentation_Description_Type;

   function Members (This : Documentation_Type) return Documentation_Member_Vectors.Vector;

   type Documentation_Access_Type is access all Documentation_Type;

   package Documentation_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Documentation_Access_Type);

   type Event_Member_Kind_Id_Type is (
                                      Event_Member_Field,
                                      Event_Member_Pad,
                                      Event_Member_Doc,
                                      Event_Member_List
                                     );

   type Event_Member_Type (Kind_Id : Event_Member_Kind_Id_Type) is record
      case Kind_Id is
         when Event_Member_Field => F : aliased Field_Type;
         when Event_Member_Pad   => P : aliased Pad_Type;
         when Event_Member_Doc   => D : aliased Documentation_Type;
         when Event_Member_List  => L : aliased List_Type;
      end case;
   end record;

   type Event_Member_Access_Type is access all Event_Member_Type;

   package Event_Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Event_Member_Access_Type);

   type Event_Copy_Type is tagged limited private;

   function Name (This : Event_Copy_Type) return Event_Copy_Name_Type;

   function Number (This : Event_Copy_Type) return Event_Copy_Number_Type;

   function Ref (This : Event_Copy_Type) return Event_Copy_Ref_Type;

   type Event_Copy_Access_Type is access all Event_Copy_Type;

   package Event_Copy_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Event_Copy_Access_Type);

   package X_Id_Type is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      type Ptr is access T;

   private

      type T is tagged limited
         record
            My_Name : aliased Fs.Name_Type;
         end record;

   end X_Id_Type;

   package X_Id_Kind_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                            Element_Type => X_Id_Type.Ptr,
                                                            "="          => X_Id_Type."=");

   package Type_P is

      package Fs is

         type Value_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

      end Fs;

      type T is tagged limited private;

      function Value (This : T) return Fs.Value_Const_Ptr;

      procedure Set_Value (This : in out T;
                           Name : Aida.Strings.Unbounded_String_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Value : aliased Fs.Value_Type;
         end record;

   end Type_P;

   package X_Id_Union is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
            when True  => Value : Aida.Strings.Unbounded_String_Type;
            when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         package Type_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Type_P.Ptr,
                                                             "="          => Type_P."=");

         type Type_Vector_Const_Ptr is access constant Type_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Kinds (This : T) return Fs.Type_Vector_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Append_Kind (This : in out T;
                             Kind : Type_P.Ptr);

      type Ptr is access T;

   private
      type T is tagged limited
         record
            My_Name  : aliased Fs.Name_Type;
            My_Kinds : aliased Fs.Type_Vectors.Vector;
         end record;

   end X_Id_Union;

   package X_Id_Union_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => X_Id_Union.Ptr,
                                                             "="          => X_Id_Union."=");

   type Type_Definition_Type is tagged limited private;

   function Old_Name (This : Type_Definition_Type) return Type_Definition_Old_Name_Type;

   function New_Name (This : Type_Definition_Type) return Type_Definition_New_Name_Type;

   type Type_Definition_Access_Type is access all Type_Definition_Type;

   package Type_Definition_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                  Element_Type => Type_Definition_Access_Type);

   type Enum_Type is tagged limited private;

   function Name (This : Enum_Type) return Enum_Name_Type;

   function Items (This : Enum_Type) return Item_Vectors.Vector;

   function Documentations (This : Enum_Type) return Documentation_Vectors.Vector;

   type Enum_Access_Type is access all Enum_Type;

   package Enum_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                       Element_Type => Enum_Access_Type);

   type Union_Child_Kind_Id_Type is (
                                      Union_Child_List
                                     );

   type Union_Child_Type (Kind_Id : Union_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Union_Child_List  => L : aliased List_Type;
      end case;
   end record;

   type Union_Child_Access_Type is access all Union_Child_Type;

   package Union_Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Union_Child_Access_Type);

   type Union_Type is tagged limited private;

   function Name (This : Union_Type) return Union_Name_Type;

   function Children (This : Union_Type) return Union_Child_Vectors.Vector;

   type Union_Access_Type is access all Union_Type;

   package Union_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Union_Access_Type);

   package Struct is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Const_Name_Ptr is access constant Name_Type;

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
               when Field_Member => F : aliased Field_Type;
               when Pad_Member   => P : aliased Pad_Type;
               when List_Member  => L : aliased List_Type;
            end case;
         end record;

         type Member_Ptr is access Member_Type;

         package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Member_Ptr);

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Const_Name_Ptr;

      function Members (This : T) return Fs.Member_Vectors.Vector;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            Name    : aliased Fs.Name_Type;
            Members : Fs.Member_Vectors.Vector;
         end record;

   end Struct;

   package Struct_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                         Element_Type => Struct.Ptr,
                                                         "="          => Struct."=");

   type Event_Type is tagged limited private;

   function Name (This : Event_Type) return Event_Name_Type;

   function Number (This : Event_Type) return Event_Number_Type;

   function No_Sequence_Number (This : Event_Type) return Event_No_Sequence_Number_Type;

   function XGE (This : Event_Type) return Event_XGE_Type;

   function Members (This : Event_Type) return Event_Member_Vectors.Vector;

   type Event_Access_Type is access all Event_Type;

   package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                        Element_Type => Event_Access_Type);

   type Value_Param_Type is tagged limited private;

   function Mask_Kind (This : Value_Param_Type) return Value_Param_Mask_Kind_Type;

   function Mask_Name (This : Value_Param_Type) return Value_Param_Mask_Name_Type;

   function List_Name (This : Value_Param_Type) return Value_Param_List_Name_Type;

   type Value_Param_Access_Type is access all Value_Param_Type;

   type Reply_Child_Kind_Id_Type is (
                                     Reply_Child_Field,
                                     Reply_Child_Pad,
                                     Reply_Child_Documentation,
                                     Reply_Child_List
                                    );

   type Reply_Child_Type (Kind_Id : Reply_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Reply_Child_Field         => F : aliased Field_Type;
         when Reply_Child_Pad           => P : aliased Pad_Type;
         when Reply_Child_Documentation => D : aliased Documentation_Type;
         when Reply_Child_List          => L : aliased List_Type;
      end case;
   end record;

   type Reply_Child_Access_Type is access all Reply_Child_Type;

   package Reply_Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Reply_Child_Access_Type);

   type Reply_Type is tagged limited private;

   function Children (This : Reply_Type) return Reply_Child_Vectors.Vector;

   type Reply_Access_Type is access all Reply_Type;

   type Request_Child_Kind_Id_Type is (
                                       Request_Child_Field,
                                       Request_Child_Pad,
                                       Request_Child_Value_Param,
                                       Request_Child_Documentation,
                                       Request_Child_Reply,
                                       Request_Child_List,
                                       Request_Child_Expression_Field
                                     );

   type Request_Child_Type (Kind_Id : Request_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Request_Child_Field            => F  : aliased Field_Type;
         when Request_Child_Pad              => P  : aliased Pad_Type;
         when Request_Child_Value_Param      => V  : aliased Value_Param_Type;
         when Request_Child_Documentation    => D  : aliased Documentation_Type;
         when Request_Child_Reply            => R  : aliased Reply_Type;
         when Request_Child_List             => L  : aliased List_Type;
         when Request_Child_Expression_Field => EF : aliased Expression_Field_Type;
      end case;
   end record;

   type Request_Child_Access_Type is access all Request_Child_Type;

   package Request_Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Request_Child_Access_Type);

   type Request_Type is tagged limited private;

   function Name (This : Request_Type) return Request_Name_Type;

   function Op_Code (This : Request_Type) return Request_Op_Code_Type;

   function Shall_Combine_Adjacent (This : Request_Type) return Request_Shall_Combine_Adjacent_Type;

   function Children (This : Request_Type) return Request_Child_Vectors.Vector;

   type Request_Access_Type is access all Request_Type;

   package Request_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                          Element_Type => Request_Access_Type);

   type Xcb_Type is tagged limited private;

   function Header (This : Xcb_Type) return XCB_Header_Type;

   function Structs (This : Xcb_Type) return Struct_Vectors.Vector;

   function X_Ids (This : Xcb_Type) return X_Id_Kind_Vectors.Vector;

   function X_Id_Unions (This : Xcb_Type) return X_Id_Union_Vectors.Vector;

   function Type_Definitions (This : Xcb_Type) return Type_Definition_Vectors.Vector;

   function Enums (This : Xcb_Type) return Enum_Vectors.Vector;

   function Events (This : Xcb_Type) return Event_Vectors.Vector;

   function Event_Copies (This : Xcb_Type) return Event_Copy_Vectors.Vector;

   function Unions (This : Xcb_Type) return Union_Vectors.Vector;

   function Errors (This : Xcb_Type) return Error_Vectors.Vector;

   function Error_Copies (This : Xcb_Type) return Error_Copy_Vectors.Vector;

   function Requests (This : Xcb_Type) return Request_Vectors.Vector;

   type Xcb_Access_Type is access all Xcb_Type;

   package XCB_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                      Element_Type => Xcb_Access_Type);

private

   type Expression_Field_Type is tagged limited
      record
         Kind : Expression_Field_Kind_Type;
         Name : Expression_Field_Name_Type;
         Children : Expression_Field_Child_Vectors.Vector;
      end record;

   function Kind (This : Expression_Field_Type) return Expression_Field_Kind_Type is (This.Kind);

   function Name (This : Expression_Field_Type) return Expression_Field_Name_Type is (This.Name);

   function Children (This : Expression_Field_Type) return Expression_Field_Child_Vectors.Vector is (This.Children);

   type Example_Type is tagged limited
      record
         Value : Example_Value_Type;
      end record;

   function Value (This : Example_Type) return Example_Value_Type is (This.Value);

   type Reply_Type is tagged limited
      record
         Children : Reply_Child_Vectors.Vector;
      end record;

   function Children (This : Reply_Type) return Reply_Child_Vectors.Vector is (This.Children);

   type Value_Param_Type is tagged limited
      record
         Mask_Kind : Value_Param_Mask_Kind_Type;
         Mask_Name : Value_Param_Mask_Name_Type;
         List_Name : Value_Param_List_Name_Type;
      end record;

   function Mask_Kind (This : Value_Param_Type) return Value_Param_Mask_Kind_Type is (This.Mask_Kind);

   function Mask_Name (This : Value_Param_Type) return Value_Param_Mask_Name_Type is (This.Mask_Name);

   function List_Name (This : Value_Param_Type) return Value_Param_List_Name_Type is (This.List_Name);

   type Request_Type is tagged limited
      record
         Name                   : Request_Name_Type;
         Op_Code                : Request_Op_Code_Type;
         Shall_Combine_Adjacent : Request_Shall_Combine_Adjacent_Type;
         Children               : Request_Child_Vectors.Vector;
      end record;

   function Name (This : Request_Type) return Request_Name_Type is (This.Name);

   function Op_Code (This : Request_Type) return Request_Op_Code_Type is (This.Op_Code);

   function Shall_Combine_Adjacent (This : Request_Type) return Request_Shall_Combine_Adjacent_Type is (This.Shall_Combine_Adjacent);

   function Children (This : Request_Type) return Request_Child_Vectors.Vector is (This.Children);

   type Error_Copy_Type is tagged limited
      record
         Name   : Error_Copy_Name_Type;
         Number : Error_Copy_Number_Type;
         Ref    : Error_Copy_Ref_Type;
      end record;

   function Name (This : Error_Copy_Type) return Error_Copy_Name_Type is (This.Name);

   function Number (This : Error_Copy_Type) return Error_Copy_Number_Type is (This.Number);

   function Ref (This : Error_Copy_Type) return Error_Copy_Ref_Type is (This.Ref);

   type Error_Type is tagged limited
      record
         Name     : Error_Name_Type;
         Number   : Error_Number_Type;
         Kind     : Error_Kind_Type;
         Value    : Error_Value_Type;
         Children : Error_Child_Vectors.Vector;
      end record;

   function Name (This : Error_Type) return Error_Name_Type is (This.Name);

   function Number (This : Error_Type) return Error_Number_Type is (This.Number);

   function Kind (This : Error_Type) return Error_Kind_Type is (This.Kind);

   function Value (This : Error_Type) return Error_Value_Type is (This.Value);

   function Children (This : Error_Type) return Error_Child_Vectors.Vector is (This.Children);

   type Union_Type is tagged limited
      record
         Name : Union_Name_Type;
         Children : Union_Child_Vectors.Vector;
      end record;

   function Children (This : Union_Type) return Union_Child_Vectors.Vector is (This.Children);

   function Name (This : Union_Type) return Union_Name_Type is (This.Name);

   type Event_Copy_Type is tagged limited
      record
         Name   : Event_Copy_Name_Type;
         Number : Event_Copy_Number_Type;
         Ref    : Event_Copy_Ref_Type;
      end record;

   function Name (This : Event_Copy_Type) return Event_Copy_Name_Type is (This.Name);

   function Number (This : Event_Copy_Type) return Event_Copy_Number_Type is (This.Number);

   function Ref (This : Event_Copy_Type) return Event_Copy_Ref_Type is (This.Ref);

   type See_Type is tagged limited
      record
         Kind : See_Kind_Type;
         Name : See_Name_Type;
      end record;

   function Kind (This : See_Type) return See_Kind_Type is (This.Kind);

   function Name (This : See_Type) return See_Name_Type is (This.Name);

   type Documentation_Type is tagged limited
      record
         Brief_Description : Documentation_Brief_Description_Type;
         Description       : Documentation_Description_Type;
         Members           : Documentation_Member_Vectors.Vector;
      end record;

   function Brief_Description (This : Documentation_Type) return Documentation_Brief_Description_Type is (This.Brief_Description);

   function Description (This : Documentation_Type) return Documentation_Description_Type is (This.Description);

   function Members (This : Documentation_Type) return Documentation_Member_Vectors.Vector is (This.Members);

   type Event_Type is tagged limited
      record
         Name               : Event_Name_Type;
         Number             : Event_Number_Type;
         No_Sequence_Number : Event_No_Sequence_Number_Type;
         Members            : Event_Member_Vectors.Vector;
         XGE                : Event_XGE_Type;
      end record;

   function Name (This : Event_Type) return Event_Name_Type is (This.Name);

   function Number (This : Event_Type) return Event_Number_Type is (This.Number);

   function No_Sequence_Number (This : Event_Type) return Event_No_Sequence_Number_Type is (This.No_Sequence_Number);

   function XGE (This : Event_Type) return Event_XGE_Type is (This.XGE);

   function Members (This : Event_Type) return Event_Member_Vectors.Vector is (This.Members);

   type Operation_Type is tagged limited
      record
         Op      : Operation_Op_Type;
         Members : Operation_Member_Vectors.Vector;
      end record;

   function Op (This : Operation_Type) return Operation_Op_Type is (This.Op);

   function Members (This : Operation_Type) return Operation_Member_Vectors.Vector is (This.Members);

   type List_Type is tagged limited
      record
         Kind    : List_Kind_Type;
         Name    : List_Name_Type;
         Members : List_Member_Vectors.Vector;
      end record;

   function Kind (This : List_Type) return List_Kind_Type is (This.Kind);

   function Name (This : List_Type) return List_Name_Type is (This.Name);

   function Members (This : List_Type) return List_Member_Vectors.Vector is (This.Members);

   type List_Access_Type is access all List_Type;

   type Enum_Type is tagged limited
      record
         Name           : Enum_Name_Type;
         Items          : Item_Vectors.Vector;
         Documentations : Documentation_Vectors.Vector;
      end record;

   function Name (This : Enum_Type) return Enum_Name_Type is (This.Name);

   function Items (This : Enum_Type) return Item_Vectors.Vector is (This.Items);

   function Documentations (This : Enum_Type) return Documentation_Vectors.Vector is (This.Documentations);

   type Pad_Type is tagged limited
      record
         Bytes : Pad_Bytes_Type;
      end record;

   function Bytes (This : Pad_Type) return Pad_Bytes_Type is (This.Bytes);

   type Type_Definition_Type is tagged limited
      record
         Old_Name : Type_Definition_Old_Name_Type;
         New_Name : Type_Definition_New_Name_Type;
      end record;

   function Old_Name (This : Type_Definition_Type) return Type_Definition_Old_Name_Type is (This.Old_Name);

   function New_Name (This : Type_Definition_Type) return Type_Definition_New_Name_Type is (This.New_Name);

   type Field_Type is tagged limited
      record
         Kind     : Field_Kind_Type;
         Name     : Field_Name_Type;
         Enum     : Field_Enum_Type;
         Mask     : Field_Mask_Type;
         Alt_Enum : Field_Alt_Enum_Type;
         Value    : Field_Value_Type;
      end record;

   function Kind (This : Field_Type) return Field_Kind_Type is (This.Kind);

   function Name (This : Field_Type) return Field_Name_Type is (This.Name);

   function Enum (This : Field_Type) return Field_Enum_Type is (This.Enum);

   function Mask (This : Field_Type) return Field_Mask_Type is (This.Mask);

   function Alt_Enum (This : Field_Type) return Field_Alt_Enum_Type is (This.Alt_Enum);

   function Value (This : Field_Type) return Field_Value_Type is (This.Value);

   type Xcb_Type is tagged limited
      record
         Header           : XCB_Header_Type;
         Structs          : Struct_Vectors.Vector;
         X_Ids            : X_Id_Kind_Vectors.Vector;
         X_Id_Unions      : X_Id_Union_Vectors.Vector;
         Type_Definitions : Type_Definition_Vectors.Vector;
         Enums            : Enum_Vectors.Vector;
         Events           : Event_Vectors.Vector;
         Event_Copies     : Event_Copy_Vectors.Vector;
         Unions           : Union_Vectors.Vector;
         Errors           : Error_Vectors.Vector;
         Error_Copies     : Error_Copy_Vectors.Vector;
         Requests         : Request_Vectors.Vector;
      end record;

   function Header (This : Xcb_Type) return XCB_Header_Type is (This.Header);

   function Structs (This : Xcb_Type) return Struct_Vectors.Vector is (This.Structs);

   function X_Ids (This : Xcb_Type) return X_Id_Kind_Vectors.Vector is (This.X_Ids);

   function X_Id_Unions (This : Xcb_Type) return X_Id_Union_Vectors.Vector is (This.X_Id_Unions);

   function Type_Definitions (This : Xcb_Type) return Type_Definition_Vectors.Vector is (This.Type_Definitions);

   function Enums (This : Xcb_Type) return Enum_Vectors.Vector is (This.Enums);

   function Events (This : Xcb_Type) return Event_Vectors.Vector is (This.Events);

   function Event_Copies (This : Xcb_Type) return Event_Copy_Vectors.Vector is (This.Event_Copies);

   function Unions (This : Xcb_Type) return Union_Vectors.Vector is (This.Unions);

   function Errors (This : Xcb_Type) return Error_Vectors.Vector is (This.Errors);

   function Error_Copies (This : Xcb_Type) return Error_Copy_Vectors.Vector is (This.Error_Copies);

   function Requests (This : Xcb_Type) return Request_Vectors.Vector is (This.Requests);

end X_Proto;
