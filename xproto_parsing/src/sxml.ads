with Aida.Bounded_String;
with Aida.Containers.Bounded_Vector;

-- Package that contains types and subprograms for simple xml parsing.
package SXML is

   package Bounded_String is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 200);

   subtype Bounded_String_T is Bounded_String.T;

   package DL is new Aida.Containers.Bounded_Vector (Element_T  => Bounded_String_T,
                                                     "="        => Bounded_String."=",
                                                     MAX_LENGTH => 100);

   package Error_Message_P is new Aida.Bounded_String (Maximum_Length_Of_Bounded_String => 2_000);

   subtype Error_Message_T is Error_Message_P.T;

end SXML;
