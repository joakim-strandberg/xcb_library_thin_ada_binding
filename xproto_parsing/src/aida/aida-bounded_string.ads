pragma Ada_95;

generic
   Maximum_Length_Of_Bounded_String : Positive;
package Aida.Bounded_String is

   subtype Length_Type is Natural range 0 .. Maximum_Length_Of_Bounded_String;

   Out_Of_Bounds_Exception : exception;

   type T is private;

   procedure Initialize (This : out T;
                         Text : Standard.String);

   procedure Initialize (This                 : out T;
                         Text                 : Standard.String;
                         Has_Truncated_String : out Boolean);

   procedure Append (Target : in out T;
                     Source : Standard.String);

   function To_String (This : T) return Standard.String;

   function Length (This : T) return Length_Type;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean;

   function "="(Left, Right : T) return Boolean;

   function Hash32 (This : T) return Hash32_T;

private

   type T is
      record
         Text        : Standard.String (1 .. Maximum_Length_Of_Bounded_String);
         Text_Length : Length_Type := 0;
      end record;

end Aida.Bounded_String;
