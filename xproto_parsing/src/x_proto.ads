with Aida.Containers.Bounded_Vector;
with Aida.Bounded_String;

package X_Proto is

   package Large_Bounded_String is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 3_000);

   package Field is

      package Fs is

         type Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Enum_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Enum_Const_Ptr is access constant Enum_Type;

         type Mask_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Mask_Const_Ptr is access constant Mask_Type;

         type Value_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

         type Alt_Enum_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Alt_Enum_Const_Ptr is access constant Alt_Enum_Type;

      end Fs;

      type T is limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Enum (This : T) return Fs.Enum_Const_Ptr;

      function Mask (This : T) return Fs.Mask_Const_Ptr;

      function Value (This : T) return Fs.Value_Const_Ptr;

      function Alt_Enum (This : T) return Fs.Alt_Enum_Const_Ptr;

      procedure Set_Kind (This : in out T;
                          Kind : Large_Bounded_String.T);

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Enum (This : in out T;
                          Enum : Large_Bounded_String.T);

      procedure Set_Mask (This : in out T;
                          Mask : Large_Bounded_String.T);

      procedure Set_Value (This  : in out T;
                           Value : Large_Bounded_String.T);

      procedure Set_Alt_Enum (This     : in out T;
                              Alt_Enum : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
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

         type Bytes_Type is record
            Exists : Boolean := False;
            Value  : Positive;
         end record;

         type Bytes_Const_Ptr is access constant Bytes_Type;

      end Fs;

      type T is limited private;

      function Bytes (This : T) return Fs.Bytes_Const_Ptr;

      procedure Set_Bytes (This  : in out T;
                           Bytes : Positive);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Bytes : aliased Fs.Bytes_Type;
         end record;

   end Pad;

   type Field_Reference_Type is new Large_Bounded_String.T;

   type Field_Reference_Access_Type is access all Field_Reference_Type;

   type Value_Type is new Natural;

   type Value_Access_Type is access all Value_Type;

   type Operation_T is tagged limited private;

   package Operation is

      package Fs is

         type Op_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
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

         package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                       "="        => "=",
                                                                       MAX_LENGTH => 50);

         type Members_Const_Ptr is access constant Member_Vector.T;

      end Fs;

      subtype T is Operation_T;

      function Op (This : T) return Fs.Op_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Op (This : in out T;
                        Op   : Large_Bounded_String.T);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      subtype Ptr is Fs.Operation_Ptr;

   end Operation;

   package List is

      package Fs is

         type Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

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

         package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                       "="        => "=",
                                                                       MAX_LENGTH => 50);

         type Members_Const_Ptr is access constant Member_Vector.T;

      end Fs;

      type T is limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Kind (This : in out T;
                          Kind : Large_Bounded_String.T);

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private
      type T is limited
         record
            My_Kind    : aliased Fs.Kind_Type;
            My_Name    : aliased Fs.Name_Type;
            My_Members : aliased Fs.Member_Vector.T;
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

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

      end Fs;

      type T is limited private;

      function Kind_Id (This : T) return Fs.Kind_Id_Type;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Value (This : T) return Value_Type;

      function Bit (This : T) return Fs.Bit_Type;

      procedure Set_Kind_Id (This    : in out T;
                             Kind_Id : Fs.Kind_Id_Type);

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Value (This  : in out T;
                           Value : Value_Type);

      procedure Set_Bit (This : in out T;
                         Bit  : Fs.Bit_Type);

      type Ptr is access all T;

   private

      type T is limited record
         My_Kind_Id : aliased Fs.Kind_Id_Type := Fs.Not_Specified;
         My_Name    : aliased Fs.Name_Type;
         My_Value   : aliased Value_Type;
         My_Bit     : aliased Fs.Bit_Type;
      end record;

   end Item;

   package Expression_Field is

      package Fs is

         type Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Child_Kind_Id_Type is (
                                     Child_Operation
                                    );

         type Child_Type (Kind_Id : Child_Kind_Id_Type) is record
            case Kind_Id is
               when Child_Operation  => Op : aliased Operation.T;
            end case;
         end record;

         type Child_Ptr is access all Child_Type;

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                      "="        => "=",
                                                                      MAX_LENGTH => 30);

         type Children_Const_Ptr is access constant Child_Vector.T;

      end Fs;

      type T is limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Set_Kind (This  : in out T;
                          Value : Large_Bounded_String.T);

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Kind     : aliased Fs.Kind_Type;
            My_Name     : aliased Fs.Name_Type;
            My_Children : aliased Fs.Child_Vector.T;
         end record;

   end Expression_Field;

   package Error is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Value_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

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

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                     "="        => "=",
                                                                     MAX_LENGTH => 30);

         type Children_Const_Ptr is access constant Child_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Value (This : T) return Fs.Value_Const_Ptr;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Number (This : in out T;
                            Value : Natural);

      procedure Set_Kind (This  : in out T;
                          Value : Large_Bounded_String.T);

      procedure Set_Value (This  : in out T;
                           Value : Large_Bounded_String.T);

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private
      type T is limited
         record
            My_Name     : aliased Fs.Name_Type;
            My_Number   : aliased Fs.Number_Type;
            My_Kind     : aliased Fs.Kind_Type;
            My_Value    : aliased Fs.Value_Type;
            My_Children : aliased Fs.Child_Vector.T;
         end record;

   end Error;

   package Error_Copy is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type Ref_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Ref_Const_Ptr is access constant Ref_Type;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function Ref (This : T) return Fs.Ref_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Number (This : in out T;
                            Value : Natural);

      procedure Set_Ref (This  : in out T;
                         Value : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Name   : aliased Fs.Name_Type;
            My_Number : aliased Fs.Number_Type;
            My_Ref    : aliased Fs.Ref_Type;
         end record;

   end Error_Copy;

   package Example is

      package Fs is

         type Value_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

      end Fs;

      type T is limited private;

      function Value (This : T) return Fs.Value_Const_Ptr;

      procedure Set_Value (This  : in out T;
                           Value : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Value : aliased Fs.Value_Type;
         end record;

   end Example;

   package See is

      package Fs is

         type Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Kind_Const_Ptr is access constant Kind_Type;

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

      end Fs;

      type T is limited private;

      function Kind (This : T) return Fs.Kind_Const_Ptr;

      function Name (This : T) return Fs.Name_Const_Ptr;

      procedure Set_Kind (This  : in out T;
                          Value : Large_Bounded_String.T);

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Kind : aliased Fs.Kind_Type;
            My_Name : aliased Fs.Name_Type;
         end record;

   end See;

   package Documentation is

      package Fs is

         type Brief_Description_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Brief_Description_Const_Ptr is access constant Brief_Description_Type;

         type Description_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Description_Const_Ptr is access constant Description_Type;

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

         package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                      "="        => "=",
                                                                      MAX_LENGTH => 30);

         type Members_Const_Ptr is access constant Member_Vector.T;

      end Fs;

      type T is limited private;

      function Brief_Description (This : T) return Fs.Brief_Description_Const_Ptr;

      function Description (This : T) return Fs.Description_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Brief_Description (This  : in out T;
                                       Value : Large_Bounded_String.T);

      procedure Set_Description (This  : in out T;
                                 Value : Large_Bounded_String.T);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Brief_Description : aliased Fs.Brief_Description_Type;
            My_Description       : aliased Fs.Description_Type;
            My_Members           : aliased Fs.Member_Vector.T;
         end record;

   end Documentation;

   package Event_Copy is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type Ref_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Ref_Const_Ptr is access constant Ref_Type;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function Ref (This : T) return Fs.Ref_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Number (This  : in out T;
                            Value : Natural);

      procedure Set_Ref (This : in out T;
                         Name : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Name   : aliased Fs.Name_Type;
            My_Number : aliased Fs.Number_Type;
            My_Ref    : aliased Fs.Ref_Type;
         end record;

   end Event_Copy;

   package X_Id is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      type Ptr is access T;

   private

      type T is limited
         record
            My_Name : aliased Fs.Name_Type;
         end record;

   end X_Id;

   package Type_P is

      package Fs is

         type Value_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Value_Const_Ptr is access constant Value_Type;

      end Fs;

      type T is limited private;

      function Value (This : T) return Fs.Value_Const_Ptr;

      procedure Set_Value (This : in out T;
                           Name : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Value : aliased Fs.Value_Type;
         end record;

   end Type_P;

   package X_Id_Union is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         package Type_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Type_P.Ptr,
                                                                    "="        => Type_P."=",
                                                                    MAX_LENGTH => 30);

         type Type_Vector_Const_Ptr is access constant Type_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Kinds (This : T) return Fs.Type_Vector_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Kind (This : in out T;
                             Kind : Type_P.Ptr);

      type Ptr is access T;

   private
      type T is limited
         record
            My_Name  : aliased Fs.Name_Type;
            My_Kinds : aliased Fs.Type_Vector.T;
         end record;

   end X_Id_Union;

   package Type_Definition is

      package Fs is

         type Old_Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Old_Name_Const_Ptr is access constant Old_Name_Type;

         type New_Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type New_Name_Const_Ptr is access constant New_Name_Type;

      end Fs;

      type T is limited private;

      function Old_Name (This : T) return Fs.Old_Name_Const_Ptr;

      function New_Name (This : T) return Fs.New_Name_Const_Ptr;

      procedure Set_Old_Name (This     : in out T;
                              Old_Name : Large_Bounded_String.T);

      procedure Set_New_Name (This     : in out T;
                              New_Name : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Old_Name : aliased Fs.Old_Name_Type;
            My_New_Name : aliased Fs.New_Name_Type;
         end record;

   end Type_Definition;

   package Enum is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         package Item_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Item.Ptr,
                                                                     "="        => Item."=",
                                                                     MAX_LENGTH => 100);

         type Items_Const_Ptr is access constant Item_Vector.T;

         package Documentation_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Documentation.Ptr,
                                                                             "="        => Documentation."=",
                                                                             MAX_LENGTH => 30);

         type Documentations_Const_Ptr is access constant Documentation_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Items (This : T) return Fs.Items_Const_Ptr;

      function Documentations (This : T) return Fs.Documentations_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Item (This   : in out T;
                             Item_V : Item.Ptr);

      procedure Append_Documentation (This            : in out T;
                                      Documentation_V : Documentation.Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Name           : aliased Fs.Name_Type;
            My_Items          : aliased Fs.Item_Vector.T;
            My_Documentations : aliased Fs.Documentation_Vector.T;
         end record;

   end Enum;

   package Union is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
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

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                     "="        => "=",
                                                                     MAX_LENGTH => 10);

         type Children_Const_Ptr is access constant Child_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Name     : aliased Fs.Name_Type;
            My_Children : aliased Fs.Child_Vector.T;
         end record;

   end Union;

   package Struct is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
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

         package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                      "="        => "=",
                                                                      MAX_LENGTH => 30);

         type Members_Const_Ptr is access constant Member_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Const_Name_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            Name    : aliased Fs.Name_Type;
            Members : aliased Fs.Member_Vector.T;
         end record;

   end Struct;

   package Event is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Number_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Number_Const_Ptr is access constant Number_Type;

         type No_Sequence_Number_Type is record
            Exists : Boolean := False;
            Value  : Boolean;
         end record;

         type No_Sequence_Number_Const_Ptr is access constant No_Sequence_Number_Type;

         type XGE_Type is record
            Exists : Boolean := False;
            Value  : Boolean;
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
               when Event_Member_Doc   => D : aliased Documentation.T;
               when Event_Member_List  => L : aliased List.T;
            end case;
         end record;

         type Member_Ptr is access all Member_Type;

         package Member_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Member_Ptr,
                                                                      "="        => "=",
                                                                      MAX_LENGTH => 30);

         type Members_Const_Ptr is access constant Member_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Number (This : T) return Fs.Number_Const_Ptr;

      function No_Sequence_Number (This : T) return Fs.No_Sequence_Number_Const_Ptr;

      function XGE (This : T) return Fs.XGE_Const_Ptr;

      function Members (This : T) return Fs.Members_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

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

      type T is limited
         record
            My_Name               : aliased Fs.Name_Type;
            My_Number             : aliased Fs.Number_Type;
            My_No_Sequence_Number : aliased Fs.No_Sequence_Number_Type;
            My_Members            : aliased Fs.Member_Vector.T;
            My_XGE                : aliased Fs.XGE_Type;
         end record;

   end Event;

   package Value_Param is

      package Fs is

         type Mask_Kind_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Mask_Kind_Const_Ptr is access constant Mask_Kind_Type;

         type Mask_Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Mask_Name_Const_Ptr is access constant Mask_Name_Type;

         type List_Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type List_Name_Const_Ptr is access constant List_Name_Type;

      end Fs;

      type T is limited private;

      function Mask_Kind (This : T) return Fs.Mask_Kind_Const_Ptr;

      function Mask_Name (This : T) return Fs.Mask_Name_Const_Ptr;

      function List_Name (This : T) return Fs.List_Name_Const_Ptr;

      procedure Set_Mask_Kind (This : in out T;
                               Value : Large_Bounded_String.T);

      procedure Set_Mask_Name (This : in out T;
                               Value : Large_Bounded_String.T);

      procedure Set_List_Name (This : in out T;
                               Value : Large_Bounded_String.T);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Mask_Kind : aliased Fs.Mask_Kind_Type;
            My_Mask_Name : aliased Fs.Mask_Name_Type;
            My_List_Name : aliased Fs.List_Name_Type;
         end record;

   end Value_Param;

   package Reply is

      package Fs is

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

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                     "="        => "=",
                                                                     MAX_LENGTH => 30);

         type Children_Const_Ptr is access constant Child_Vector.T;

      end Fs;

      type T is limited private;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Children : aliased Fs.Child_Vector.T;
         end record;

   end Reply;

   package Request is

      package Fs is

         type Name_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Name_Const_Ptr is access constant Name_Type;

         type Op_Code_Type is record
            Exists : Boolean := False;
            Value  : Natural;
         end record;

         type Op_Code_Const_Ptr is access constant Op_Code_Type;

         type Shall_Combine_Adjacent_Type is record
            Exists : Boolean := False;
            Value  : Boolean;
         end record;

         type Shall_Combine_Adjacent_Const_Ptr is access constant Shall_Combine_Adjacent_Type;

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

         package Child_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Child_Ptr,
                                                                     "="        => "=",
                                                                     MAX_LENGTH => 30);

         type Children_Const_Ptr is access constant Child_Vector.T;

      end Fs;

      type T is limited private;

      function Name (This : T) return Fs.Name_Const_Ptr;

      function Op_Code (This : T) return Fs.Op_Code_Const_Ptr;

      function Shall_Combine_Adjacent (This : T) return Fs.Shall_Combine_Adjacent_Const_Ptr;

      function Children (This : T) return Fs.Children_Const_Ptr;

      procedure Set_Name (This : in out T;
                          Name : Large_Bounded_String.T);

      procedure Set_Op_Code (This  : in out T;
                             Value : Natural);

      procedure Set_Shall_Combine_Adjacent (This  : in out T;
                                            Value : Boolean);

      procedure Append_Child (This  : in out T;
                              Child : Fs.Child_Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Name                   : aliased Fs.Name_Type;
            My_Op_Code                : aliased Fs.Op_Code_Type;
            My_Shall_Combine_Adjacent : aliased Fs.Shall_Combine_Adjacent_Type;
            My_Children               : aliased Fs.Child_Vector.T;
         end record;

   end Request;

   package Xcb is

      package Fs is

         type Header_Type is record
            Exists : Boolean := False;
            Value  : Large_Bounded_String.T;
         end record;

         type Header_Const_Ptr is access constant Header_Type;

         package Struct_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Struct.Ptr,
                                                                      "="        => Struct."=",
                                                                      MAX_LENGTH => 400);

         type Structs_Const_Ptr is access constant Struct_Vector.T;

         package X_Id_Vector is new Aida.Containers.Bounded_Vector (Element_T  => X_Id.Ptr,
                                                                    "="        => X_Id."=",
                                                                    MAX_LENGTH => 100);

         type X_Ids_Const_Ptr is access constant X_Id_Vector.T;

         package X_Id_Union_Vector is new Aida.Containers.Bounded_Vector (Element_T  => X_Id_Union.Ptr,
                                                                          "="        => X_Id_Union."=",
                                                                          MAX_LENGTH => 100);

         type X_Id_Unions_Const_Ptr is access constant X_Id_Union_Vector.T;

         package Type_Definition_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Type_Definition.Ptr,
                                                                               "="        => Type_Definition."=",
                                                                               MAX_LENGTH => 100);

         type Type_Definitions_Const_Ptr is access constant Type_Definition_Vector.T;

         package Enum_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Enum.Ptr,
                                                                    "="        => Enum."=",
                                                                    MAX_LENGTH => 100);

         type Enums_Const_Ptr is access constant Enum_Vector.T;

         package Event_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Event.Ptr,
                                                                     "="        => Event."=",
                                                                     MAX_LENGTH => 100);

         type Events_Const_Ptr is access constant Event_Vector.T;

         package Event_Copy_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Event_Copy.Ptr,
                                                                          "="        => Event_Copy."=",
                                                                          MAX_LENGTH => 100);

         type Event_Copies_Const_Ptr is access constant Event_Copy_Vector.T;

         package Union_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Union.Ptr,
                                                                     "="        => Union."=",
                                                                     MAX_LENGTH => 100);

         type Unions_Const_Ptr is access constant Union_Vector.T;

         package Error_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Error.Ptr,
                                                                     "="        => Error."=",
                                                                     MAX_LENGTH => 100);

         type Errors_Const_Ptr is access constant Error_Vector.T;

         package Error_Copy_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Error_Copy.Ptr,
                                                                          "="        => Error_Copy."=",
                                                                          MAX_LENGTH => 100);

         type Error_Copies_Const_Ptr is access constant Error_Copy_Vector.T;

         package Request_Vector is new Aida.Containers.Bounded_Vector (Element_T  => Request.Ptr,
                                                                       "="        => Request."=",
                                                                       MAX_LENGTH => 150);

         type Requests_Const_Ptr is access constant Request_Vector.T;

      end Fs;

      type T is limited private;

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
                            Text : Large_Bounded_String.T);

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
                              Item : Error.Ptr);

      procedure Append_Error_Copy (This : in out T;
                                   Item : Error_Copy.Ptr);

      procedure Append_Request (This : in out T;
                                Item : Request.Ptr);

      type Ptr is access all T;

   private

      type T is limited
         record
            My_Header           : aliased Fs.Header_Type;
            My_Structs          : aliased Fs.Struct_Vector.T;
            My_X_Ids            : aliased Fs.X_Id_Vector.T;
            My_X_Id_Unions      : aliased Fs.X_Id_Union_Vector.T;
            My_Type_Definitions : aliased Fs.Type_Definition_Vector.T;
            My_Enums            : aliased Fs.Enum_Vector.T;
            My_Events           : aliased Fs.Event_Vector.T;
            My_Event_Copies     : aliased Fs.Event_Copy_Vector.T;
            My_Unions           : aliased Fs.Union_Vector.T;
            My_Errors           : aliased Fs.Error_Vector.T;
            My_Error_Copies     : aliased Fs.Error_Copy_Vector.T;
            My_Requests         : aliased Fs.Request_Vector.T;
         end record;

   end Xcb;

private

   type Operation_T is tagged limited
      record
         My_Op      : aliased Operation.Fs.Op_Type;
         My_Members : aliased Operation.Fs.Member_Vector.T;
      end record;

end X_Proto;
