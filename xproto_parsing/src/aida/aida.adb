with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Aida is

   use Aida.Character;

   package body String is

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_647 (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_647 (Source     : in  String_T;
                                                                            Target     : out Int32_T) is
      begin
         Target := 2_147_483_640 + To_Int32 (Source (Source'First + 9));
      end Calculate_Positive_Target_For_Length_10_Case_2_147_483_647;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_63X (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_63X (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 8)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 8)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + 2_147_483_600;
      end Calculate_Positive_Target_For_Length_10_Case_2_147_483_63X;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_5XX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_483_5XX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 7)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 7)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + 2_147_483_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_147_483_5XX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_482_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_482_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 6)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 6)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + 2_147_480_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_147_482_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_47X_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_47X_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 5)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 5)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + 2_147_400_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_147_47X_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_3XX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_147_3XX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 4)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 4)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + N(Source'First + 4) *       100_000;
         Target := Target + 2_147_000_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_147_3XX_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_146_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_146_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 3)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 3)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + N(Source'First + 4) *       100_000;
         Target := Target + N(Source'First + 3) *     1_000_000;
         Target := Target + 2_140_000_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_146_XXX_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_13X_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_13X_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 2)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 2)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + N(Source'First + 4) *       100_000;
         Target := Target + N(Source'First + 3) *     1_000_000;
         Target := Target + N(Source'First + 2) *    10_000_000;
         Target := Target + 2_100_000_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_13X_XXX_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_2_0XX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_2_0XX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 2)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 2)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + N(Source'First + 4) *       100_000;
         Target := Target + N(Source'First + 3) *     1_000_000;
         Target := Target + N(Source'First + 2) *    10_000_000;
         Target := Target + 2_000_000_000;
      end Calculate_Positive_Target_For_Length_10_Case_2_0XX_XXX_XXX;

      procedure Calculate_Positive_Target_For_Length_10_Case_1_XXX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T);

      procedure Calculate_Positive_Target_For_Length_10_Case_1_XXX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 9);
         Target := Target + N(Source'First + 8) *            10;
         Target := Target + N(Source'First + 7) *           100;
         Target := Target + N(Source'First + 6) *         1_000;
         Target := Target + N(Source'First + 5) *        10_000;
         Target := Target + N(Source'First + 4) *       100_000;
         Target := Target + N(Source'First + 3) *     1_000_000;
         Target := Target + N(Source'First + 2) *    10_000_000;
         Target := Target + N(Source'First + 1) *   100_000_000;
         Target := Target + N(Source'First + 0) * 1_000_000_000;
      end Calculate_Positive_Target_For_Length_10_Case_1_XXX_XXX_XXX;

      procedure Calculate_Positive_Target_Length_9 (Source     : in  String_T;
                                                    Target     : out Int32_T);

      procedure Calculate_Positive_Target_Length_9 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 8)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 8) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 8);
         Target := Target + N(Source'First + 7) *            10;
         Target := Target + N(Source'First + 6) *           100;
         Target := Target + N(Source'First + 5) *         1_000;
         Target := Target + N(Source'First + 4) *        10_000;
         Target := Target + N(Source'First + 3) *       100_000;
         Target := Target + N(Source'First + 2) *     1_000_000;
         Target := Target + N(Source'First + 1) *    10_000_000;
         Target := Target + N(Source'First + 0) *   100_000_000;
      end Calculate_Positive_Target_Length_9;

      procedure Calculate_Positive_Target_Length_8 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 7)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 7) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 7);
         Target := Target + N(Source'First + 6) *            10;
         Target := Target + N(Source'First + 5) *           100;
         Target := Target + N(Source'First + 4) *         1_000;
         Target := Target + N(Source'First + 3) *        10_000;
         Target := Target + N(Source'First + 2) *       100_000;
         Target := Target + N(Source'First + 1) *     1_000_000;
         Target := Target + N(Source'First + 0) *    10_000_000;
      end Calculate_Positive_Target_Length_8;

      procedure Calculate_Positive_Target_Length_7 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 6)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 6) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 6);
         Target := Target + N(Source'First + 5) *            10;
         Target := Target + N(Source'First + 4) *           100;
         Target := Target + N(Source'First + 3) *         1_000;
         Target := Target + N(Source'First + 2) *        10_000;
         Target := Target + N(Source'First + 1) *       100_000;
         Target := Target + N(Source'First + 0) *     1_000_000;
      end Calculate_Positive_Target_Length_7;

      procedure Calculate_Positive_Target_Length_6 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 5)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 5) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 5);
         Target := Target + N(Source'First + 4) *            10;
         Target := Target + N(Source'First + 3) *           100;
         Target := Target + N(Source'First + 2) *         1_000;
         Target := Target + N(Source'First + 1) *        10_000;
         Target := Target + N(Source'First + 0) *       100_000;
      end Calculate_Positive_Target_Length_6;

      procedure Calculate_Positive_Target_Length_5 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 4)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 4) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 4);
         Target := Target + N(Source'First + 3) *            10;
         Target := Target + N(Source'First + 2) *           100;
         Target := Target + N(Source'First + 1) *         1_000;
         Target := Target + N(Source'First + 0) *        10_000;
      end Calculate_Positive_Target_Length_5;

      procedure Calculate_Positive_Target_Length_4 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 3)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 3) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 3);
         Target := Target + N(Source'First + 2) *            10;
         Target := Target + N(Source'First + 1) *           100;
         Target := Target + N(Source'First + 0) *         1_000;
      end Calculate_Positive_Target_Length_4;

      procedure Calculate_Positive_Target_Length_3 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 2)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 2) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 2);
         Target := Target + N(Source'First + 1) *            10;
         Target := Target + N(Source'First + 0) *           100;
      end Calculate_Positive_Target_Length_3;

      procedure Calculate_Positive_Target_Length_2 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range Source'First..(Source'First + 1)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range Source'First..(Source'First + 1) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := N(Source'First + 1);
         Target := Target + N(Source'First + 0) * 10;
      end Calculate_Positive_Target_Length_2;

      procedure Calculate_Positive_Target_Length_1 (Source     : in  String_T;
                                                    Target     : out Int32_T) is
      begin
         To_Int32 (Source => Source (Source'First),
                   Target => Target);
      end Calculate_Positive_Target_Length_1;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_483_648 (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         N : Int32_T;
      begin
         To_Int32 (Source => Source(Source'First + 10),
                   Target => N);

         Target := -N;
         Target := Target - 2_147_483_640;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_483_648;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_483_63X (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 9)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 9)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - 2_147_483_600;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_483_63X;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_483_5XX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 8)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 8)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - 2_147_483_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_483_5XX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_482_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 7)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 7)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - 2_147_480_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_482_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_47X_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 6)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 6)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - 2_147_400_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_47X_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_147_3XX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 5)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 5)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - N(Source'First + 5) *       100_000;
         Target := Target - 2_147_000_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_147_3XX_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_146_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 4)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 4)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - N(Source'First + 5) *       100_000;
         Target := Target - N(Source'First + 4) *     1_000_000;
         Target := Target - 2_140_000_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_146_XXX_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_13X_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 3)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 3)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - N(Source'First + 5) *       100_000;
         Target := Target - N(Source'First + 4) *     1_000_000;
         Target := Target - N(Source'First + 3) *    10_000_000;
         Target := Target - 2_100_000_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_13X_XXX_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_2_0XX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 2)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 2)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - N(Source'First + 5) *       100_000;
         Target := Target - N(Source'First + 4) *     1_000_000;
         Target := Target - N(Source'First + 3) *    10_000_000;
         Target := Target - 2_000_000_000;
      end Calculate_Negative_Target_For_Length_11_Case_2_0XX_XXX_XXX;

      procedure Calculate_Negative_Target_For_Length_11_Case_1_XXX_XXX_XXX (Source     : in  String_T;
                                                                            Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 10)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 10) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 10);
         Target := Target - N(Source'First + 9) *            10;
         Target := Target - N(Source'First + 8) *           100;
         Target := Target - N(Source'First + 7) *         1_000;
         Target := Target - N(Source'First + 6) *        10_000;
         Target := Target - N(Source'First + 5) *       100_000;
         Target := Target - N(Source'First + 4) *     1_000_000;
         Target := Target - N(Source'First + 3) *    10_000_000;
         Target := Target - N(Source'First + 2) *   100_000_000;
         Target := Target - N(Source'First + 1) * 1_000_000_000;
      end Calculate_Negative_Target_For_Length_11_Case_1_XXX_XXX_XXX;

      procedure Calculate_Negative_Target_Length_10 (Source     : in  String_T;
                                                     Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 9)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 9) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 9);
         Target := Target - N(Source'First + 8) *            10;
         Target := Target - N(Source'First + 7) *           100;
         Target := Target - N(Source'First + 6) *         1_000;
         Target := Target - N(Source'First + 5) *        10_000;
         Target := Target - N(Source'First + 4) *       100_000;
         Target := Target - N(Source'First + 3) *     1_000_000;
         Target := Target - N(Source'First + 2) *    10_000_000;
         Target := Target - N(Source'First + 1) *   100_000_000;
      end Calculate_Negative_Target_Length_10;

      procedure Calculate_Negative_Target_Length_9 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 8)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 8) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 8);
         Target := Target - N(Source'First + 7) *            10;
         Target := Target - N(Source'First + 6) *           100;
         Target := Target - N(Source'First + 5) *         1_000;
         Target := Target - N(Source'First + 4) *        10_000;
         Target := Target - N(Source'First + 3) *       100_000;
         Target := Target - N(Source'First + 2) *     1_000_000;
         Target := Target - N(Source'First + 1) *    10_000_000;
      end Calculate_Negative_Target_Length_9;

      procedure Calculate_Negative_Target_Length_8 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 7)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 7) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 7);
         Target := Target - N(Source'First + 6) *            10;
         Target := Target - N(Source'First + 5) *           100;
         Target := Target - N(Source'First + 4) *         1_000;
         Target := Target - N(Source'First + 3) *        10_000;
         Target := Target - N(Source'First + 2) *       100_000;
         Target := Target - N(Source'First + 1) *     1_000_000;
      end Calculate_Negative_Target_Length_8;

      procedure Calculate_Negative_Target_Length_7 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 6)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 6) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 6);
         Target := Target - N(Source'First + 5) *            10;
         Target := Target - N(Source'First + 4) *           100;
         Target := Target - N(Source'First + 3) *         1_000;
         Target := Target - N(Source'First + 2) *        10_000;
         Target := Target - N(Source'First + 1) *       100_000;
      end Calculate_Negative_Target_Length_7;

      procedure Calculate_Negative_Target_Length_6 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 5)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 5) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 5);
         Target := Target - N(Source'First + 4) *            10;
         Target := Target - N(Source'First + 3) *           100;
         Target := Target - N(Source'First + 2) *         1_000;
         Target := Target - N(Source'First + 1) *        10_000;
      end Calculate_Negative_Target_Length_6;

      procedure Calculate_Negative_Target_Length_5 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 4)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 4) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 4);
         Target := Target - N(Source'First + 3) *            10;
         Target := Target - N(Source'First + 2) *           100;
         Target := Target - N(Source'First + 1) *         1_000;
      end Calculate_Negative_Target_Length_5;

      procedure Calculate_Negative_Target_Length_4 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 3)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 3) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 3);
         Target := Target - N(Source'First + 2) *            10;
         Target := Target - N(Source'First + 1) *           100;
      end Calculate_Negative_Target_Length_4;

      procedure Calculate_Negative_Target_Length_3 (Source     : in  String_T;
                                                    Target     : out Int32_T)
      is
         type Number_Array_Type is array (Positive range (Source'First + 1)..(Source'First + 2)) of Int32_T;

         N : Number_Array_Type := (others => 0);
      begin
         for Index in Positive range (Source'First + 1)..(Source'First + 2) loop
            To_Int32 (Source => Source (Index),
                      Target => N (Index));
         end loop;

         Target := -N(Source'First + 2);
         Target := Target - N(Source'First + 1) *            10;
      end Calculate_Negative_Target_Length_3;

      procedure Calculate_Negative_Target_Length_2 (Source     : in  String_T;
                                                    Target     : out Int32_T) is
      begin
         To_Int32 (Source => Source (Source'First + 1),
                   Target => Target);

         Target := -Target;
      end Calculate_Negative_Target_Length_2;

      procedure To_Int32 (Source     : in  String_T;
                          Target     : out Int32_T;
                          Has_Failed : out Boolean) is
      begin
         if Source'Length = 0 then
            Target := 0;
            Has_Failed := True;
            return;
         end if;

         if Source (Source'First) = '-' then
            if Source'Length > 11 then
               Target := 0;
               Has_Failed := True;
               return;
            end if;

            if Source'Length = 1 then
               Target := 0;
               Has_Failed := True;
               return;
            end if;

            for I in Positive range (Source'First + 1)..Source'Last loop
               if not Is_Digit (Source(I)) then
                  Target := 0;
                  Has_Failed := True;
                  return;
               end if;
            end loop;

            Target := 0;

            if Source'Length = 11 then
               if Source (Source'First + 1) > '2' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 1) < '2' then
                  Calculate_Negative_Target_For_Length_11_Case_1_XXX_XXX_XXX (Source,
                                                                              Target);

                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 2) > '1' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 2) < '1' then
                  Calculate_Negative_Target_For_Length_11_Case_2_0XX_XXX_XXX (Source,
                                                                              Target);

                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 3) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 3) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_13X_XXX_XXX (Source,
                                                                              Target);

                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 4) > '7' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 4) < '7' then
                  Calculate_Negative_Target_For_Length_11_Case_2_146_XXX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 5) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 5) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_3XX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 6) > '8' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 6) < '8' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_47X_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 7) > '3' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 7) < '3' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_482_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 8) > '6' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 8) < '6' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_483_5XX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 9) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 9) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_483_63X (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 10) > '8' then
                  Has_Failed := True;
                  return;
               end if;

               Calculate_Negative_Target_For_Length_11_Case_2_147_483_648 (Source,
                                                                           Target);
               Has_Failed := False;
            else
               case Source'Length is
               when 2 =>
                  Calculate_Negative_Target_Length_2 (Source,
                                                      Target);
                  Has_Failed := False;
               when 3 =>
                  Calculate_Negative_Target_Length_3 (Source,
                                                      Target);
                  Has_Failed := False;
               when 4 =>
                  Calculate_Negative_Target_Length_4 (Source,
                                                      Target);
                  Has_Failed := False;
               when 5 =>
                  Calculate_Negative_Target_Length_5 (Source,
                                                      Target);
                  Has_Failed := False;
               when 6 =>
                  Calculate_Negative_Target_Length_6 (Source,
                                                      Target);
                  Has_Failed := False;
               when 7 =>
                  Calculate_Negative_Target_Length_7 (Source,
                                                      Target);
                  Has_Failed := False;
               when 8 =>
                  Calculate_Negative_Target_Length_8 (Source,
                                                      Target);
                  Has_Failed := False;
               when 9 =>
                  Calculate_Negative_Target_Length_9 (Source,
                                                      Target);
                  Has_Failed := False;
               when 10 =>
                  Calculate_Negative_Target_Length_10 (Source,
                                                       Target);
                  Has_Failed := False;
               when others =>
                  Target := 0;
                  Has_Failed := True;
               end case;
            end if;
         else
            if Source'Length > 10 then
               Target := 0;
               Has_Failed := True;
               return;
            end if;

            for I in Positive range Source'First..Source'Last loop
               if not Is_Digit (Source(I)) then
                  Target := 0;
                  Has_Failed := True;
                  return;
               end if;
            end loop;

            Target := 0;

            if Source'Length = 10 then
               if Source (Source'First) > '2' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First) < '2' then
                  Calculate_Positive_Target_For_Length_10_Case_1_XXX_XXX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;

                  return;
               end if;

               if Source (Source'First + 1) > '1' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 1) < '1' then
                  Calculate_Positive_Target_For_Length_10_Case_2_0XX_XXX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 2) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 2) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_13X_XXX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 3) > '7' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 3) < '7' then
                  Calculate_Positive_Target_For_Length_10_Case_2_146_XXX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 4) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 4) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_3XX_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 5) > '8' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 5) < '8' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_47X_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 6) > '3' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 6) < '3' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_482_XXX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 7) > '6' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 7) < '6' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_483_5XX (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 8) > '4' then
                  Has_Failed := True;
                  return;
               end if;

               if Source (Source'First + 8) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_483_63X (Source,
                                                                              Target);
                  Has_Failed := False;
                  return;
               end if;

               if Source (Source'First + 9) > '7' then
                  Has_Failed := True;
                  return;
               end if;

               Calculate_Positive_Target_For_Length_10_Case_2_147_483_647 (Source,
                                                                           Target);
               Has_Failed := False;
            else
               case Source'Length is
               when 1 =>
                  Calculate_Positive_Target_Length_1 (Source,
                                                      Target);
                  Has_Failed := False;
               when 2 =>
                  Calculate_Positive_Target_Length_2 (Source,
                                                      Target);
                  Has_Failed := False;
               when 3 =>
                  Calculate_Positive_Target_Length_3 (Source,
                                                      Target);
                  Has_Failed := False;
               when 4 =>
                  Calculate_Positive_Target_Length_4 (Source,
                                                      Target);
                  Has_Failed := False;
               when 5 =>
                  Calculate_Positive_Target_Length_5 (Source,
                                                      Target);
                  Has_Failed := False;
               when 6 =>
                  Calculate_Positive_Target_Length_6 (Source,
                                                      Target);
                  Has_Failed := False;
               when 7 =>
                  Calculate_Positive_Target_Length_7 (Source,
                                                      Target);
                  Has_Failed := False;
               when 8 =>
                  Calculate_Positive_Target_Length_8 (Source,
                                                      Target);
                  Has_Failed := False;
               when 9 =>
                  Calculate_Positive_Target_Length_9 (Source,
                                                      Target);
                  Has_Failed := False;
               when others =>
                  Target := 0;
                  Has_Failed := True;
               end case;
            end if;
         end if;
      end To_Int32;

      function To_Int32 (Source : T) return Int32_T is
         Target : Int32_T;
      begin
         if Source (Source'First) = '-' then

            if Source'Length = 11 then
               if Source (Source'First + 1) < '2' then
                  Calculate_Negative_Target_For_Length_11_Case_1_XXX_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 2) < '1' then
                  Calculate_Negative_Target_For_Length_11_Case_2_0XX_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 3) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_13X_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 4) < '7' then
                  Calculate_Negative_Target_For_Length_11_Case_2_146_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 5) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_3XX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 6) < '8' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_47X_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 7) < '3' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_482_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 8) < '6' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_483_5XX (Source,
                                                                              Target);
               elsif Source (Source'First + 9) < '4' then
                  Calculate_Negative_Target_For_Length_11_Case_2_147_483_63X (Source,
                                                                              Target);
               else
                  Calculate_Negative_Target_For_Length_11_Case_2_147_483_648 (Source,
                                                                              Target);
               end if;
            else
               case Source'Length is
               when 2 =>
                  Calculate_Negative_Target_Length_2 (Source,
                                                      Target);
               when 3 =>
                  Calculate_Negative_Target_Length_3 (Source,
                                                      Target);
               when 4 =>
                  Calculate_Negative_Target_Length_4 (Source,
                                                      Target);
               when 5 =>
                  Calculate_Negative_Target_Length_5 (Source,
                                                      Target);
               when 6 =>
                  Calculate_Negative_Target_Length_6 (Source,
                                                      Target);
               when 7 =>
                  Calculate_Negative_Target_Length_7 (Source,
                                                      Target);
               when 8 =>
                  Calculate_Negative_Target_Length_8 (Source,
                                                      Target);
               when 9 =>
                  Calculate_Negative_Target_Length_9 (Source,
                                                      Target);
               when 10 =>
                  Calculate_Negative_Target_Length_10 (Source,
                                                       Target);
               when others =>
                  Target := 0;
               end case;
            end if;
         else
            if Source'Length = 10 then
               if Source (Source'First) < '2' then
                  Calculate_Positive_Target_For_Length_10_Case_1_XXX_XXX_XXX (Source,
                                                                              Target);

               elsif Source (Source'First + 1) < '1' then
                  Calculate_Positive_Target_For_Length_10_Case_2_0XX_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 2) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_13X_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 3) < '7' then
                  Calculate_Positive_Target_For_Length_10_Case_2_146_XXX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 4) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_3XX_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 5) < '8' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_47X_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 6) < '3' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_482_XXX (Source,
                                                                              Target);
               elsif Source (Source'First + 7) < '6' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_483_5XX (Source,
                                                                              Target);
               elsif Source (Source'First + 8) < '4' then
                  Calculate_Positive_Target_For_Length_10_Case_2_147_483_63X (Source,
                                                                              Target);
               else
                  Calculate_Positive_Target_For_Length_10_Case_2_147_483_647 (Source,
                                                                              Target);
               end if;
            else
               case Source'Length is
               when 1 =>
                  Calculate_Positive_Target_Length_1 (Source,
                                                      Target);
               when 2 =>
                  Calculate_Positive_Target_Length_2 (Source,
                                                      Target);
               when 3 =>
                  Calculate_Positive_Target_Length_3 (Source,
                                                      Target);
               when 4 =>
                  Calculate_Positive_Target_Length_4 (Source,
                                                      Target);
               when 5 =>
                  Calculate_Positive_Target_Length_5 (Source,
                                                      Target);
               when 6 =>
                  Calculate_Positive_Target_Length_6 (Source,
                                                      Target);
               when 7 =>
                  Calculate_Positive_Target_Length_7 (Source,
                                                      Target);
               when 8 =>
                  Calculate_Positive_Target_Length_8 (Source,
                                                      Target);
               when 9 =>
                  Calculate_Positive_Target_Length_9 (Source,
                                                      Target);
               when others =>
                  Target := 0;
               end case;
            end if;
         end if;

         return Target;
      end To_Int32;

      function Trim (This : T) return T is
      begin
         return T (Ada.Strings.Fixed.Trim (Source => Standard.String (This),
                                           Side   => Ada.Strings.Both));
      end Trim;

      procedure To_Standard_Out (This : T) is
      begin
         Ada.Text_IO.Put_Line (Standard.String (This));
      end To_Standard_Out;

      function Hash32 (This : T) return Hash32_T is
        H : Hash32_T := 0;
        A : Hash32_T := 31_415;
        B : Hash32_T := 27_183;
      begin
         for I in Positive range This'First..This'Last loop
            H := A*H + Standard.Character'Pos (This (I));
            A := A*B;
         end loop;

         return H;
      end Hash32;

   end String;

   package body Character is

      function Is_Digit (Char : T) return Boolean is
      begin
         return Char in '0'..'9';
      end Is_Digit;

      function To_Int32 (Source : in T) return Int32_T is
      begin
         return Int32_T (Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0'));
      end To_Int32;

      procedure To_Int32 (Source : in  T;
                          Target : out Int32_T) is
      begin
         Target := Int32_T (Standard.Character'Pos (Standard.Character (Source)) - Standard.Character'Pos ('0'));
      end To_Int32;

   end Character;

   package body Int32 is

      function To_String (Value : T) return String_T is
      begin
         return String_T (Ada.Strings.Fixed.Trim (Source => Integer'Image (Integer (Value)),
                                                  Side   => Ada.Strings.Both));
      end To_String;

      function To_String (Value : T) return Standard.String is
      begin
         return Ada.Strings.Fixed.Trim (Source => Integer'Image (Integer (Value)),
                                        Side   => Ada.Strings.Both);
      end To_String;

      procedure To_Standard_Out (This : T) is
      begin
         Ada.Text_IO.Put_Line (Standard.String (Aida.String.Trim (String_T (Int32_T'Image (This)))));
      end To_Standard_Out;

   end Int32;

end Aida;
