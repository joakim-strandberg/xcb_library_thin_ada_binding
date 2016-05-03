package Std_Character is
   pragma Pure;

   function Is_Digit (C : Character) return Boolean;

   function To_Integer (Source : in  Character) return Integer;

   procedure To_Integer (Source : in  Character;
                         Target : out Integer);

end Std_Character;
