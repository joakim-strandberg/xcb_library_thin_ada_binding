with BC.Indefinite_Unmanaged_Containers.Collections;
with Aida.Strings;

-- Package that contains types and subprograms for simple xml parsing.
package SXML is

   package String_Containers is new BC.Indefinite_Unmanaged_Containers (String);

   package DL is new String_Containers.Collections;

   type Error_Message_Type is new Aida.Strings.Unbounded_String_Type with null record;

end SXML;
