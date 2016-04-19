with Ada.Containers.Formal_Doubly_Linked_Lists;

package Std_Integer is
   pragma Pure;
   pragma SPARK_Mode;

   type One_Digit_Integer_Type is new Integer range 0..9;

   function To_Character (I : One_Digit_Integer_Type) return Character is (case I is
                                                               when 0 => '0',
                                                               when 1 => '1',
                                                               when 2 => '2',
                                                               when 3 => '3',
                                                               when 4 => '4',
                                                               when 5 => '5',
                                                               when 6 => '6',
                                                               when 7 => '7',
                                                               when 8 => '8',
                                                               when 9 => '9'
                                                                          );
--   function To_String (I : Integer) return String;-- with
--     Pre => I >= 0 and I <= 9;--,
--       Contract_Cases => (I >= 0 and I <= 9 => To_String'Result'Length = 1 and then (To_String'Result ( To_String'Result'First) = To_Character (One_Digit_Integer_Type(I))));

   type Base_10_Integer_Type;

   package Base_10_Digits is new Ada.Containers.Formal_Doubly_Linked_Lists (Element_Type => One_Digit_Integer_Type,
                                                                            "="          => "=");

   type Base_10_Integer_Type is private;

--     procedure Convert (Source : in  Integer;
--                        Target : out Base_10_Integer_Type) with
--       Post => Source = Convert (Target);

--     function Convert (Source : Base_10_Integer_Type) return Integer with
--       Pre => Integer (Length (Source)) >= 1 and Integer (Length (Source)) <= 10;

   function Length (Number : Base_10_Integer_Type) return Ada.Containers.Count_Type;

private

   type Base_10_Integer_Type is
      record
         Is_Negative    : Boolean := False;
         Integer_Digits : Base_10_Digits.List (10);
      end record;

   function Length (Number : Base_10_Integer_Type) return Ada.Containers.Count_Type is (Base_10_Digits.Length (Number.Integer_Digits));

   function Calculate_Negative_Target (S : Base_10_Integer_Type) return Integer with
     Pre => (Integer (Base_10_Digits.Length (S.Integer_Digits)) >= 1 and Integer (Base_10_Digits.Length (S.Integer_Digits)) <= 3);

end Std_Integer;
