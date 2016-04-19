package Aida.Hexadecimal is
   pragma Pure;
   pragma SPARK_Mode;

   type Nibble_Type; -- 0..15
--   type Character_Of_Nibble_Type; -- '0'..'F'

   type Byte_Type; -- 0..255
   type String_Of_Byte_Type; --"00".."FF"

   type Nibble_Type is range 0..(2 ** 4 - 1);
   for Nibble_Type'Size use 4;

   type Byte_Type is range 0..(2 ** 8 - 1);
   for Byte_Type'Size use 8;

   type Byte_Record_Type is
      record
         High : Nibble_Type;
         Low  : Nibble_Type;
      end record;
   for Byte_Record_Type'Size use 8;
   for Byte_Record_Type use
      record
         High at 0 range 0..3;
         Low  at 0 range 4..7;
      end record;

   function To_Nibbles (B : Byte_Type) return Byte_Record_Type with
     Post => B = 16*Byte_Type (To_Nibbles'Result.High) + Byte_Type (To_Nibbles'Result.Low);

   function Is_Hex (C : Character) return Boolean is (case C is
                                                         when '0' => True,
                                                         when '1' => True,
                                                         when '2' => True,
                                                         when '3' => True,
                                                         when '4' => True,
                                                         when '5' => True,
                                                         when '6' => True,
                                                         when '7' => True,
                                                         when '8' => True,
                                                         when '9' => True,
                                                         when 'A' => True,
                                                         when 'B' => True,
                                                         when 'C' => True,
                                                         when 'D' => True,
                                                         when 'E' => True,
                                                         when 'F' => True,
                                                         when others => False);

   subtype Character_Of_Nibble_Type is Character;-- with Predicate =>
--     (Character_Of_Nibble_Type in '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F');

   function To_Nibble (C : Character_Of_Nibble_Type) return Nibble_Type with
     Pre => C in '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F';

   function To_Character_Of_Nibble (N : Nibble_Type) return Character_Of_Nibble_Type with
     Post => To_Character_Of_Nibble'Result in '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F';

   type String_Of_Byte_Type is array (1..2) of Character_Of_Nibble_Type;

   function To_Byte (C : Character) return Byte_Type is (Character'Pos (C));

   function To_Character (B : Byte_Type) return Character is (Character'Val (B));

   function To_String (C : Character) return String_Of_Byte_Type with
     Post => ((To_String'Result (1) in '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F' and
                To_String'Result (2) in '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9'|'A'|'B'|'C'|'D'|'E'|'F') and then (
                    To_Byte (C) = 16*Byte_Type (To_Nibble (To_String'Result (1))) + Byte_Type (To_Nibble (To_String'Result (2)))));

private

   function To_Nibble (C : Character_Of_Nibble_Type) return Nibble_Type
   is
     (case C is
         when '0' => 0,
         when '1' => 1,
         when '2' => 2,
         when '3' => 3,
         when '4' => 4,
         when '5' => 5,
         when '6' => 6,
         when '7' => 7,
         when '8' => 8,
         when '9' => 9,
         when 'A' => 10,
         when 'B' => 11,
         when 'C' => 12,
         when 'D' => 13,
         when 'E' => 14,
         when 'F' => 15,
         when others => 0);

   function To_Character_Of_Nibble (N : Nibble_Type) return Character_Of_Nibble_Type is
     (case N is
         when 0  => '0',
         when 1  => '1',
         when 2  => '2',
         when 3  => '3',
         when 4  => '4',
         when 5  => '5',
         when 6  => '6',
         when 7  => '7',
         when 8  => '8',
         when 9  => '9',
         when 10 => 'A',
         when 11 => 'B',
         when 12 => 'C',
         when 13 => 'D',
         when 14 => 'E',
         when 15 => 'F');

end Aida.Hexadecimal;
