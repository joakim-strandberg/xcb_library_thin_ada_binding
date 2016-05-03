with Std_Character;

package Std_String is
   pragma Pure;

   procedure To_Integer (Source     : in  String;
                         Target     : out Integer;
                         Has_Failed : out Boolean);

   function To_String (Source : String) return Integer;

end Std_String;
