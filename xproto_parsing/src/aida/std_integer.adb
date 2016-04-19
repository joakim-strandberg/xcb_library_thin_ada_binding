package body Std_Integer is
   pragma SPARK_Mode;

   use type Base_10_Digits.Cursor;

--     procedure Convert (Source : in  Integer;
--                        Target : out Base_10_Integer_Type) is
--     begin
--        null;
--     end Convert;

--     function B_F (Source : Base_10_Integer_Type) return Base_10_Digits.Cursor is (Base_10_Digits.First (Source.Integer_Digits)) with Ghost;
--
--     function B_N (Source : Base_10_Integer_Type; C : Base_10_Digits.Cursor) return Base_10_Digits.Cursor is (Base_10_Digits.Next (Source.Integer_Digits, C)) with
--     Ghost => True,
--     Pre => Base_10_Digits.Has_Element (Source.Integer_Digits, C);
--
--     function B_E (Source : Base_10_Integer_Type; C : Base_10_Digits.Cursor) return One_Digit_Integer_Type is (Base_10_Digits.Element (Source.Integer_Digits, C)) with
--     Ghost => True,
--     Pre => Base_10_Digits.Has_Element (Source.Integer_Digits, C);

   function Calculate_Negative_Target (S : Base_10_Integer_Type) return Integer
--       Pre => (Integer (Base_10_Digits.Length (S.Integer_Digits)) >= 1 and Integer (Base_10_Digits.Length (S.Integer_Digits)) <= 10)
--       and then (if Integer (Base_10_Digits.Length (S.Integer_Digits)) = 10 then
--                   (B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S))))))))))) = 2 and
--                        B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S)))))))))) = 1 and
--                      B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S))))))))) = 4 and
--                        B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S)))))))) = 7 and
--                      B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S))))))) = 4 and
--                        B_E (S, B_N (S, B_N (S, B_N (S, B_N (S, B_F (S)))))) = 8 and
--                      B_E (S, B_N (S, B_N (S, B_N (S, B_F (S))))) = 3 and
--                        B_E (S, B_N (S, B_N (S, B_F (S)))) = 6 and
--                      B_E (S, B_N (S, B_F (S))) = 4 and
--                        B_E (S, B_F (S)) <= 8))
   is
      Digit_Weight : constant array (Integer range 1..10) of Positive := (1 => 1,
                                                                          2 => 10,
                                                                          3 => 100,
                                                                          4 => 1_000,
                                                                          5 => 10_000,
                                                                          6 => 100_000,
                                                                          7 => 1_000_000,
                                                                          8 => 10_000_000,
                                                                          9 => 100_000_000,
                                                                          10 => 1_000_000_000);

      Target_Max : constant array (Integer range 0..8) of Integer := (0 => 0,
                                                                      1 => -9,
                                                                      2 => -99,
                                                                      3 => -999,
                                                                      4 => -9_999,
                                                                      5 => -99_999,
                                                                      6 => -999_999,
                                                                      7 => -9_999_999,
                                                                      8 => -99_999_999);

--        function Target_Max (K : Natural) return Integer is (if K=0 then
--                                                                0
--                                                             else
--                                                                -9*10**(K - 1) + Target_Max (K - 1)) with
--          Pre => K <= 8,
--          Post => Target_Max'Result > -10**K;

--        function Target_Max (K : Natural) return Integer is (-10**(K) + 1) with
--          Pre => K <= 8;


      C : Base_10_Digits.Cursor := Base_10_Digits.First (S.Integer_Digits);

      Index : Integer := 0;

      Target : Integer := 0;

--      Exponent : Integer;
   begin
      while Base_10_Digits.Has_Element (S.Integer_Digits, C) loop
         pragma Loop_Invariant (Index = Integer (Base_10_Digits.Length (Base_10_Digits.First_To_Previous (S.Integer_Digits, C))));
         pragma Loop_Invariant (Target >= Target_Max (Index));

         Index := Index + 1;

         Target := Target - Integer(Base_10_Digits.Element (S.Integer_Digits, C))*Digit_Weight (Index);

         C := Base_10_Digits.Next (S.Integer_Digits, C);
      end loop;

      return Target;
   end Calculate_Negative_Target;

--     function Convert (Source : Base_10_Integer_Type) return Integer is
--        Digit_Weight : constant array (Integer range 1..10) of Positive := (1 => 1,
--                                                                            2 => 10,
--                                                                            3 => 100,
--                                                                            4 => 1_000,
--                                                                            5 => 10_000,
--                                                                            6 => 100_000,
--                                                                            7 => 1_000_000,
--                                                                            8 => 10_000_000,
--                                                                            9 => 100_000_000,
--                                                                            10 => 1_000_000_000);
--     begin
--        if Source.Is_Negative then
--           return Calculate_Negative_Target (Source);
--        else
--           declare
--              C : Base_10_Digits.Cursor := Base_10_Digits.First (Source.Integer_Digits);
--
--              Index : Integer range 0..10 := 0;
--
--              Target : Integer := 0;
--           begin
--              while Base_10_Digits.Has_Element (Source.Integer_Digits, C) loop
--                 pragma Loop_Invariant (Index = Integer (Base_10_Digits.Length (Base_10_Digits.First_To_Previous (Source.Integer_Digits, C))));
--                 Index := Index + 1;
--                 declare
--                    E : One_Digit_Integer_Type renames Base_10_Digits.Element (Source.Integer_Digits, C);
--                 begin
--                    Target := Target + Integer(E)*Digit_Weight (Index);
--
--                    --                 pragma Loop_Invariant (Index = Digit_Weight'First + Integer (Base_10_Digits.Length (Source.Integer_Digits)) -
--                    --                                          Integer (Base_10_Digits.Length (Base_10_Digits.Current_To_Last (Source.Integer_Digits, C))));
--
--                    C := Base_10_Digits.Next (Source.Integer_Digits, C);
--                 end;
--              end loop;
--              return Target;
--           end;
--        end if;
--     end Convert;

--     function To_String (I : Integer) return String is
--        Ordo_Array : constant array (Integer range 2..11) of Natural := (11 => 10,
--                                                                10 => 100,
--                                                                9 => 1_000,
--                                                                8 => 10_000,
--                                                                7 => 100_000,
--                                                                6 => 1_000_000,
--                                                                5 => 10_000_000,
--                                                                4 => 100_000_000,
--                                                                3 => 1_000_000_000,
--                                                                2 => 2_147_483_647);
--
--     begin
--        if I = -2_147_483_648 then
--           return "-2147483648";
--        end if;
--
--        declare
--           Result : String (1..11) := (others => '0');
--           Index : Positive;
--
--           Is_Negative : constant Boolean := I < 0;
--
--           Temp2 : Natural := (if Is_Negative then -I else I);
--           Pos_I : constant Natural := Temp2;
--        begin
--           declare
--              Temp : One_Digit_Integer_Type;
--           begin
--              for J in reverse Integer range (Result'First + 1)..Result'Last loop
--                 Temp  := One_Digit_Integer_Type (Temp2 rem 10);
--                 Temp2 := (Temp2 - Integer (Temp))/10;
--
--                 Index := J;
--                 Result (J) := To_Character (Temp);
--
--                 if Pos_I < Ordo_Array (J) then
--                    exit;
--                 end if;
--              end loop;
--           end;
--
--           if Is_Negative then
--              Index := Index - 1;
--              Result (Index) := '-';
--           end if;
--
--           return Result (Index..Result'Last);
--        end;
--     end To_String;

end Std_Integer;
