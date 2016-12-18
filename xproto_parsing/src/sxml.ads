with BC.Indefinite_Unmanaged_Containers.Collections;
with Aida.Bounded_String;

-- Package that contains types and subprograms for simple xml parsing.
package SXML is

   package String_Containers is new BC.Indefinite_Unmanaged_Containers (String);

   package DL is new String_Containers.Collections;

   package Error_Message_P is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 2_000);

   subtype Error_Message_T is Error_Message_P.T;

end SXML;
