with Ada.Containers.Vectors;
with Aida.Strings;

package X_Proto is

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

   type Documentation_Description_Type (Exists : Boolean := False) is record
      case Exists is
         when True  => Value : Aida.Strings.Unbounded_String_Type;
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

   package Field is

      package Fs is

         type Kind_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Enum_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Enum_Const_Ptr is access constant Enum_Type;

         type Mask_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Mask_Const_Ptr is access constant Mask_Type;

         type Value_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

         type Alt_Enum_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Alt_Enum_Const_Ptr is access constant Alt_Enum_Type;

      end Fs;

      type T is tagged limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Enum (This : T) return Fs.Enum_Const_Ptr;

      function Mask (This : T) return Fs.Mask_Const_Ptr;

      function Value (This : T) return Fs.Value_Const_Ptr;

      function Alt_Enum (This : T) return Fs.Alt_Enum_Const_Ptr;

      procedure Set_Kind (This : in out T;
                          Kind : Aida.Strings.Unbounded_String_Type);

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Set_Enum (This : in out T;
                          Enum : Aida.Strings.Unbounded_String_Type);

      procedure Set_Mask (This : in out T;
                          Mask : Aida.Strings.Unbounded_String_Type);

      procedure Set_Value (This  : in out T;
                           Value : Aida.Strings.Unbounded_String_Type);

      procedure Set_Alt_Enum (This     : in out T;
                              Alt_Enum : Aida.Strings.Unbounded_String_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Kind     : aliased Fs.Kind_Type;
            My_Name     : aliased Fs.Name_Type;
            My_Enum     : aliased Fs.Enum_Type;
            My_Mask     : aliased Fs.Mask_Type;
            My_Alt_Enum : aliased Fs.Alt_Enum_Type;
            My_Value    : aliased Fs.Value_Type;
         end record;

   end Field;

   package Pad is

      package Fs is

         type Bytes_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Positive;
               when False => null;
            end case;
         end record;

         type Bytes_Const_Ptr is access constant Bytes_Type;

      end Fs;

      type T is tagged limited private;

      function Bytes (This : T) return Fs.Bytes_Const_Ptr;

      procedure Set_Bytes (This  : in out T;
                           Bytes : Positive);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Bytes : aliased Fs.Bytes_Type;
         end record;

   end Pad;

   type Field_Reference_Type is new Aida.Strings.Unbounded_String_Type with null record;

   type Field_Reference_Access_Type is access all Field_Reference_Type;

   type Value_Type is new Natural;

   type Value_Access_Type is access all Value_Type;

   type Operation_T is tagged limited private;

   package Operation is

      package Fs is

         type Op_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Op_Const_Ptr is access constant Op_Type;

         type Operation_Ptr is access all Operation_T;

         type Member_Kind_Id_Type is (
                                      Member_Kind_Field_Reference,
                                      Member_Kind_Value,
                                      Member_Operation
                                     );

         type Member_Type (Kind_Id : Member_Kind_Id_Type) is record
            case Kind_Id is
               when Member_Kind_Field_Reference => Field_Reference : aliased Field_Reference_Type;
               when Member_Kind_Value           => Value           : aliased Value_Type;
               when Member_Operation            => Operation       : aliased Operation_Ptr;
            end case;
         end record;

         type Member_Ptr is access Member_Type;

         package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Member_Ptr);

         type Members_Const_Ptr is access constant Member_Vectors.Vector;

      end Fs;

      subtype T is Operation_T;

      function Op (This : T) return Fs.Op_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Op (This : in out T;
                        Op   : Aida.Strings.Unbounded_String_Type);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      subtype Ptr is Fs.Operation_Ptr;

   end Operation;

   package List is

      package Fs is

         type Kind_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Member_Kind_Id_Type is (
                                      List_Member_Kind_Field_Reference,
                                      List_Member_Kind_Value,
                                      List_Member_Kind_Operation
                                     );

         type Member_Type (Kind_Id : Member_Kind_Id_Type) is record
            case Kind_Id is
            when List_Member_Kind_Field_Reference => Field_Reference : Aida.Strings.Unbounded_String_Type;
            when List_Member_Kind_Value           => Value           : aliased Value_Type;
            when List_Member_Kind_Operation       => Operation       : aliased Operation_T;
            end case;
         end record;

         type Member_Ptr is access all Member_Type;

         package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Member_Ptr);

         type Members_Const_Ptr is access constant Member_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Kind (This : in out T;
                          Kind : Aida.Strings.Unbounded_String_Type);

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private
      type T is tagged limited
         record
            My_Kind    : aliased Fs.Kind_Type;
            My_Name    : aliased Fs.Name_Type;
            My_Members : aliased Fs.Member_Vectors.Vector;
         end record;

   end List;

   package Item is

      package Fs is

         type Kind_Id_Type is (
                               Not_Specified,
                               Specified_As_Value,
                               Specified_As_Bit
                              );

         type Bit_Type is new Natural;

         type Bit_Ptr is access all Bit_Type;

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

      end Fs;

      type T is tagged limited private;

      function Kind_Id (This : T) return Fs.Kind_Id_Type;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Value (This : T) return Value_Type;

      function Bit (This : T) return Fs.Bit_Type;

      procedure Set_Kind_Id (This    : in out T;
                             Kind_Id : Fs.Kind_Id_Type);

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Set_Value (This  : in out T;
                           Value : Value_Type);

      procedure Set_Bit (This : in out T;
                         Bit  : Fs.Bit_Type);

      type Ptr is access all T;

   private

      type T is tagged limited record
         My_Kind_Id : aliased Fs.Kind_Id_Type := Fs.Not_Specified;
         My_Name    : aliased Fs.Name_Type;
         My_Value   : aliased Value_Type;
         My_Bit     : aliased Fs.Bit_Type;
      end record;

   end Item;

   type Expression_Field_Child_Kind_Id_Type is (
                                     Expression_Field_Child_Operation
                                     );

   type Expression_Field_Child_Type (Kind_Id : Expression_Field_Child_Kind_Id_Type) is record
      case Kind_Id is
         when Expression_Field_Child_Operation  => Op : aliased Operation.T;
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
         when Error_Child_Field  => F : aliased Field.T;
         when Error_Child_Pad    => P : aliased Pad.T;
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

   type Error_Copy_Type is tagged limited private;

   function Name (This : Error_Copy_Type) return Error_Copy_Name_Type;

   function Number (This : Error_Copy_Type) return Error_Copy_Number_Type;

   function Ref (This : Error_Copy_Type) return Error_Copy_Ref_Type;

   type Error_Copy_Access_Type is access all Error_Copy_Type;

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
         when Documentation_Member_Field   => F : aliased Field.T;
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

   package Event_Copy is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Natural;
               when False => null;
            end case;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type Ref_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Ref_Const_Ptr is access constant Ref_Type;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function Ref (This : T) return Fs.Ref_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Set_Number (This  : in out T;
                            Value : Natural);

      procedure Set_Ref (This : in out T;
                         Name : Aida.Strings.Unbounded_String_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Name   : aliased Fs.Name_Type;
            My_Number : aliased Fs.Number_Type;
            My_Ref    : aliased Fs.Ref_Type;
         end record;

   end Event_Copy;

   package X_Id is

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

   end X_Id;

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

   package Type_Definition is

      package Fs is

         type Old_Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Old_Name_Const_Ptr is access constant Old_Name_Type;

         type New_Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type New_Name_Const_Ptr is access constant New_Name_Type;

      end Fs;

      type T is tagged limited private;

      function Old_Name (This : T) return Fs.Old_Name_Const_Ptr;

      function New_Name (This : T) return Fs.New_Name_Const_Ptr;

      procedure Set_Old_Name (This     : in out T;
                              Old_Name : Aida.Strings.Unbounded_String_Type);

      procedure Set_New_Name (This     : in out T;
                              New_Name : Aida.Strings.Unbounded_String_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Old_Name : aliased Fs.Old_Name_Type;
            My_New_Name : aliased Fs.New_Name_Type;
         end record;

   end Type_Definition;

   package Enum is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         package Item_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Item.Ptr,
                                                             "="          => Item."=");

         type Items_Const_Ptr is access constant Item_Vectors.Vector;

         package Documentation_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                      Element_Type => Documentation_Access_Type);

         type Documentations_Const_Ptr is access constant Documentation_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Items (This : T) return Fs.Items_Const_Ptr;

      function Documentations (This : T) return Fs.Documentations_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Append_Item (This   : in out T;
                             Item_V : Item.Ptr);

      procedure Append_Documentation (This          : in out T;
                                      Documentation : Documentation_Access_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Name           : aliased Fs.Name_Type;
            My_Items          : aliased Fs.Item_Vectors.Vector;
            My_Documentations : aliased Fs.Documentation_Vectors.Vector;
         end record;

   end Enum;

   package Union is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Child_Kind_Id_Type is (
                                     Child_List
                                    );

         type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
            case Kind_Id is
              when Child_List  => L : aliased List.T;
            end case;
         end record;

         type Child_Ptr is access all Child_Type;

         package Child_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Child_Ptr);

         type Children_Const_Ptr is access constant Child_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Name     : aliased Fs.Name_Type;
            My_Children : aliased Fs.Child_Vectors.Vector;
         end record;

   end Union;

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
               when Field_Member => F : aliased Field.T;
               when Pad_Member   => P : aliased Pad.T;
               when List_Member  => L : aliased List.T;
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

   package Event is

      package Fs is

         type Name_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Natural;
               when False => null;
            end case;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type No_Sequence_Number_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Boolean;
               when False => null;
            end case;
         end record;

         type No_Sequence_Number_Const_Ptr is access constant No_Sequence_Number_Type;

         type XGE_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Boolean;
               when False => null;
            end case;
         end record;

         type XGE_Const_Ptr is access constant XGE_Type;

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
               when Event_Member_Doc   => D : aliased Documentation_Type;
               when Event_Member_List  => L : aliased List.T;
            end case;
         end record;

         type Member_Ptr is access all Member_Type;

         package Member_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Member_Ptr);

         type Members_Const_Ptr is access constant Member_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function No_Sequence_Number (This : T) return Fs.No_Sequence_Number_Const_Ptr;

      function XGE (This : T) return Fs.XGE_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type);

      procedure Set_Number (This : in out T;
                            Value : Natural);

      procedure Set_No_Sequence_Number (This  : in out T;
                                        Value : Boolean);

      procedure Set_XGE (This  : in out T;
                         Value : Boolean);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Name               : aliased Fs.Name_Type;
            My_Number             : aliased Fs.Number_Type;
            My_No_Sequence_Number : aliased Fs.No_Sequence_Number_Type;
            My_Members            : aliased Fs.Member_Vectors.Vector;
            My_XGE                : aliased Fs.XGE_Type;
         end record;

   end Event;

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
         when Reply_Child_Field         => F : aliased Field.T;
         when Reply_Child_Pad           => P : aliased Pad.T;
         when Reply_Child_Documentation => D : aliased Documentation_Type;
         when Reply_Child_List          => L : aliased List.T;
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
         when Request_Child_Field            => F  : aliased Field.T;
         when Request_Child_Pad              => P  : aliased Pad.T;
         when Request_Child_Value_Param      => V  : aliased Value_Param_Type;
         when Request_Child_Documentation    => D  : aliased Documentation_Type;
         when Request_Child_Reply            => R  : aliased Reply_Type;
         when Request_Child_List             => L  : aliased List.T;
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

   package Xcb is

      package Fs is

         type Header_Type (Exists : Boolean := False) is record
            case Exists is
               when True  => Value : Aida.Strings.Unbounded_String_Type;
               when False => null;
            end case;
         end record;

         type Header_Const_Ptr is access constant Header_Type;

         package Struct_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                               Element_Type => Struct.Ptr,
                                                               "="          => Struct."=");

         type Structs_Const_Ptr is access constant Struct_Vectors.Vector;

         package X_Id_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => X_Id.Ptr,
                                                             "="          => X_Id."=");

         type X_Ids_Const_Ptr is access constant X_Id_Vectors.Vector;

         package X_Id_Union_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                   Element_Type => X_Id_Union.Ptr,
                                                                   "="          => X_Id_Union."=");

         type X_Id_Unions_Const_Ptr is access constant X_Id_Union_Vectors.Vector;

         package Type_Definition_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                        Element_Type => Type_Definition.Ptr,
                                                                        "="          => Type_Definition."=");

         type Type_Definitions_Const_Ptr is access constant Type_Definition_Vectors.Vector;

         package Enum_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                             Element_Type => Enum.Ptr,
                                                             "="          => Enum."=");
         type Enums_Const_Ptr is access constant Enum_Vectors.Vector;

         package Event_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Event.Ptr,
                                                              "="          => Event."=");

         type Events_Const_Ptr is access constant Event_Vectors.Vector;

         package Event_Copy_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                   Element_Type => Event_Copy.Ptr,
                                                                   "="          => Event_Copy."=");

         type Event_Copies_Const_Ptr is access constant Event_Copy_Vectors.Vector;

         package Union_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Union.Ptr,
                                                              "="          => Union."=");

         type Unions_Const_Ptr is access constant Union_Vectors.Vector;

         package Error_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                              Element_Type => Error_Access_Type);

         type Errors_Const_Ptr is access constant Error_Vectors.Vector;

         package Error_Copy_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                   Element_Type => Error_Copy_Access_Type);

         type Error_Copies_Const_Ptr is access constant Error_Copy_Vectors.Vector;

         package Request_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                                Element_Type => Request_Access_Type);

         type Requests_Const_Ptr is access constant Request_Vectors.Vector;

      end Fs;

      type T is tagged limited private;

      function Header (This : T) return Fs.Header_Const_Ptr;

      function Structs (This : T) return Fs.Structs_Const_Ptr;

      function X_Ids (This : T) return Fs.X_Ids_Const_Ptr;

      function X_Id_Unions (This : T) return Fs.X_Id_Unions_Const_Ptr;

      function Type_Definitions (This : T) return Fs.Type_Definitions_Const_Ptr;

      function Enums (This : T) return Fs.Enums_Const_Ptr;

      function Events (This : T) return Fs.Events_Const_Ptr;

      function Event_Copies (This : T) return Fs.Event_Copies_Const_Ptr;

      function Unions (This : T) return Fs.Unions_Const_Ptr;

      function Errors (This : T) return Fs.Errors_Const_Ptr;

      function Error_Copies (This : T) return Fs.Error_Copies_Const_Ptr;

      function Requests (This : T) return Fs.Requests_Const_Ptr;

      procedure Set_Header (This : in out T;
                            Text : Aida.Strings.Unbounded_String_Type);

      procedure Append_Struct (This : in out T;
                               Item : Struct.Ptr);

      procedure Append_X_Id (This : in out T;
                             Item : X_Id.Ptr);

      procedure Append_X_Id_Union (This : in out T;
                                   Item : X_Id_Union.Ptr);

      procedure Append_Type_Definition (This : in out T;
                                        Item : Type_Definition.Ptr);

      procedure Append_Enum (This : in out T;
                             Item : Enum.Ptr);

      procedure Append_Event (This : in out T;
                              Item : Event.Ptr);

      procedure Append_Event_Copy (This : in out T;
                                   Item : Event_Copy.Ptr);

      procedure Append_Union (This : in out T;
                              Item : Union.Ptr);

      procedure Append_Error (This : in out T;
                              Item : Error_Access_Type);

      procedure Append_Error_Copy (This : in out T;
                                   Item : Error_Copy_Access_Type);

      procedure Append_Request (This : in out T;
                                Item : Request_Access_Type);

      type Ptr is access all T;

   private

      type T is tagged limited
         record
            My_Header           : aliased Fs.Header_Type;
            My_Structs          : aliased Fs.Struct_Vectors.Vector;
            My_X_Ids            : aliased Fs.X_Id_Vectors.Vector;
            My_X_Id_Unions      : aliased Fs.X_Id_Union_Vectors.Vector;
            My_Type_Definitions : aliased Fs.Type_Definition_Vectors.Vector;
            My_Enums            : aliased Fs.Enum_Vectors.Vector;
            My_Events           : aliased Fs.Event_Vectors.Vector;
            My_Event_Copies     : aliased Fs.Event_Copy_Vectors.Vector;
            My_Unions           : aliased Fs.Union_Vectors.Vector;
            My_Errors           : aliased Fs.Error_Vectors.Vector;
            My_Error_Copies     : aliased Fs.Error_Copy_Vectors.Vector;
            My_Requests         : aliased Fs.Request_Vectors.Vector;
         end record;

   end Xcb;

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

   type Operation_T is tagged limited
      record
         My_Op      : aliased Operation.Fs.Op_Type;
         My_Members : aliased Operation.Fs.Member_Vectors.Vector;
      end record;

end X_Proto;
