package body Std_Character is
   pragma Suppress (Discriminant_Check);
   pragma Suppress (Division_Check);
   pragma Suppress (Index_Check);
   pragma Suppress (Length_Check);
   pragma Suppress (Overflow_Check);
   pragma Suppress (Range_Check);
   pragma Suppress (Tag_Check);
   pragma Suppress (Elaboration_Check);

   function Is_Digit (C : Character) return Boolean is
   begin
      return C in '0'..'9';
   end Is_Digit;

   function To_Integer (Source : in  Character) return Integer is
   begin
      return Character'Pos (Source) - Character'Pos ('0');
   end To_Integer;

   procedure To_Integer (Source : in  Character;
                         Target : out Integer) is
   begin
      Target := Character'Pos (Source) - Character'Pos ('0');
   end To_Integer;

end Std_Character;
