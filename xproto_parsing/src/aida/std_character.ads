package Std_Character is
   pragma Pure;
   pragma SPARK_Mode;

   function Is_Digit (C : Character) return Boolean with
     Contract_Cases => (C = '0' => Is_Digit'Result,
                        C = '1' => Is_Digit'Result,
                        C = '2' => Is_Digit'Result,
                        C = '3' => Is_Digit'Result,
                        C = '4' => Is_Digit'Result,
                        C = '5' => Is_Digit'Result,
                        C = '6' => Is_Digit'Result,
                        C = '7' => Is_Digit'Result,
                        C = '8' => Is_Digit'Result,
                        C = '9' => Is_Digit'Result,
                        C > '9' => Is_Digit'Result = False,
                        C < '0' => Is_Digit'Result = False);

   function To_Integer (Source : in  Character) return Integer with
     Pre  => Std_Character.Is_Digit (Source),
     Post => To_Integer'Result in 0 .. 9,
     Contract_Cases => (Source = '0' => To_Integer'Result = 0,
                        Source = '1' => To_Integer'Result = 1,
                        Source = '2' => To_Integer'Result = 2,
                        Source = '3' => To_Integer'Result = 3,
                        Source = '4' => To_Integer'Result = 4,
                        Source = '5' => To_Integer'Result = 5,
                        Source = '6' => To_Integer'Result = 6,
                        Source = '7' => To_Integer'Result = 7,
                        Source = '8' => To_Integer'Result = 8,
                        Source = '9' => To_Integer'Result = 9);

   procedure To_Integer (Source : in  Character;
                         Target : out Integer) with
     Pre  => Is_Digit (Source),
     Post => Target in 0 .. 9 and Target = To_Integer (Source),
     Contract_Cases => (Source = '0' => Target = 0,
                        Source = '1' => Target = 1,
                        Source = '2' => Target = 2,
                        Source = '3' => Target = 3,
                        Source = '4' => Target = 4,
                        Source = '5' => Target = 5,
                        Source = '6' => Target = 6,
                        Source = '7' => Target = 7,
                        Source = '8' => Target = 8,
                        Source = '9' => Target = 9);

end Std_Character;
