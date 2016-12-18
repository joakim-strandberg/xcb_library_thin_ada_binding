package Aida is
--   pragma Pure;

   -- max  2147483647
   -- min -2147483648
   type Int32_T is range -2**31 .. (2**31 - 1);
   for Int32_T'Size use 32;

   type Int64_T is range -2**63 .. (2**63 - 1);
   for Int64_T'Size use 64;

   type String_T is new Standard.String;

   subtype Character_T is Standard.Character;
   -- This type is a subtype to simplify the Ada code when used together with
   -- String_T instances.

   type Hash32_T is mod 2**32;
   for Hash32_T'Size use 32;

   package String is

      subtype T is String_T;

      procedure To_Int32 (Source     : in  T;
                          Target     : out Int32_T;
                          Has_Failed : out Boolean);

      function To_Int32 (Source : T) return Int32_T;

      function Trim (This : T) return T;

      procedure To_Standard_Out (This : T);

      function Hash32 (This : T) return Hash32_T;

   end String;

   package Character is

      subtype T is Character_T;

      function Is_Digit (Char : T) return Boolean;

      function To_Int32 (Source : in T) return Int32_T;

      procedure To_Int32 (Source : in  T;
                          Target : out Int32_T);

   end Character;

   package Int32 is

      subtype T is Int32_T;

      function To_String (Value : T) return String_T;

      function To_String (Value : T) return Standard.String;

      procedure To_Standard_Out (This : T);

   end Int32;

end Aida;
