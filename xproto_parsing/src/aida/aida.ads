package Aida is

   -- max  2147483647
   -- min -2147483648
   type Int32_T is range -2**31 .. (2**31 - 1);
   for Int32_T'Size use 32;

   type Int64_T is range -2**63 .. (2**63 - 1);
   for Int64_T'Size use 64;

   type Uint32_T is range  0 .. (2**32 - 1);
   for Uint32_T'Size use 32;

   type String_T is new Standard.String;

   subtype Character_T is Standard.Character;
   -- This type is a subtype to simplify the Ada code when used together with
   -- String_T instances.

   type Code_Point_T is mod 2**32;
   subtype UTF8_Code_Point_T is Code_Point_T range 0..16#10FFFF#;

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

   package UTF8_Code_Point is

      package Fs is

         --
         -- General_Category of a code point according to the  Unicode  character
         -- database. The names of the enumeration correspond to the names in the
         -- database.
         --
         type General_Category is
           (  Lu, -- Letter, Uppercase
              Ll, --         Lowercase
              Lt, --         Titlecase
              Lm, --         Modifier
              Lo, --         Other

              Mn, -- Mark, Nonspacing
              Mc, --       Spacing Combining
              Me, --       Enclosing

              Nd, -- Number, Decimal Digit
              Nl, --         Letter
              No, --         Other

              Pc, -- Punctuation, Connector
              Pd, --              Dash
              Ps, --              Open
              Pe, --              Close
              Pi, --              Initial quote
              Pf, --              Final quote
              Po, --              Other

              Sm, -- Symbol, Math
              Sc, --         Currency
              Sk, --         Modifier
              So, --         Other

              Zs, -- Separator, Space
              Zl, --            Line
              Zp, --            Paragraph

              Cc, -- Other, Control
              Cf, --        Format
              Cs, --        Surrogate
              Co, --        Private Use
              Cn  --        Not Assigned
             );
         --
         -- Classes of categories
         --
         subtype Letter      is General_Category range Lu..Lo;
         subtype Mark        is General_Category range Mn..Me;
         subtype Mumber      is General_Category range Nd..No;
         subtype Punctuation is General_Category range Pc..Po;
         subtype Symbol      is General_Category range Sm..So;
         subtype Separator   is General_Category range Zs..Zp;
         subtype Other       is General_Category range Cc..Cn;

      end Fs;

      subtype T is UTF8_Code_Point_T;

      --
      -- Image -- Of an UTF-8 code point
      --
      --    Value - The code point
      --
      -- Returns :
      --
      --    UTF-8 encoded equivalent
      --
      function Image (Value : T) return Standard.String;

      --
      -- Has_Case -- Case test
      --
      --    Value - Code point
      --
      -- Returns :
      --
      --    True if Value has either an  upper  or  a  lower  case  equivalent
      --    different from Code.
      --
      function Has_Case (Value : T) return Boolean;

      --
      -- Is_Lowercase -- Case test
      --
      --    Value - Code point
      --
      -- Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Lowercase (Value : T) return Boolean;

      --
      -- Is_Uppercase -- Case test
      --
      --    Value - Code point
      --
      -- Returns :
      --
      --    True if Value is a lower case point
      --
      function Is_Uppercase (Value : T) return Boolean;

      --
      -- To_Lowercase -- Convert to lower case
      --
      --    Value - Code point or UTF-8 encoded string
      --
      -- Returns :
      --
      --    The lower case eqivalent or else Value itself
      --
      function To_Lowercase (Value : T) return T;

      --
      -- To_Uppercase -- Convert to upper case
      --
      --    Value - Code point or UTF-8 encoded string
      --
      -- Returns :
      --
      --    The upper case eqivalent or else Value itself
      --
      function To_Uppercase (Value : T) return T;

      --
      -- Category -- Get category of a code point
      --
      --    Value - Code point
      --
      -- Returns :
      --
      --    The category of value
      --
      function Category (Value : T) return Fs.General_Category;

      --
      -- Is_* -- Category tests
      --
      function Is_Alphanumeric (Value : in T) return Boolean;

      function Is_Digit        (Value : in T) return Boolean;

      function Is_Control      (Value : in T) return Boolean;

      function Is_ISO_646      (Value : in T) return Boolean;

      function Is_Letter       (Value : in T) return Boolean;

      function Is_Lower        (Value : in T) return Boolean;

      function Is_Other_Format (Value : in T) return Boolean;

      function Is_Space        (Value : in T) return Boolean;

      function Is_Title        (Value : in T) return Boolean;

      function Is_Upper        (Value : in T) return Boolean;

      --
      -- Special digits
      --
      function Is_Subscript_Digit (Value : in T) return Boolean;

      function Is_Superscript_Digit (Value : in T) return Boolean;
      --
      -- Ada 2005 identifier sets
      --
      --    identifier_start,  see ARM 2.3(3/2)
      --    identifier_extend, see ARM 2.3(3.1/2)
      --
      function Is_Identifier_Start (Value : in T) return Boolean;

      function Is_Identifier_Extend (Value : in T) return Boolean;

   private
      pragma Inline
        (  Is_Alphanumeric, Is_Control, Is_Digit, Is_ISO_646,
           Is_Letter,       Is_Lower,   Is_Title, Is_Upper,
           Is_Subscript_Digit,  Is_Superscript_Digit,
           Is_Identifier_Start, Is_Identifier_Extend
          );

   end UTF8_Code_Point;

   package UTF8 is

      use Aida.UTF8_Code_Point;

      function Is_Valid_UTF8_Code_Point (Source      : Standard.String;
                                         Pointer     : Integer) return Boolean;

      --
      -- Get -- Get one UTF-8 code point
      --
      --    Source  - The source string
      --    Pointer - The string position to start at
      --    Value   - The result
      --
      -- This  procedure  decodes one UTF-8 code point from the string Source.
      -- It starts at Source (Pointer). After successful completion Pointer is
      -- advanced to the first character following the input.  The  result  is
      -- returned through the parameter Value.
      --
      procedure Get (Source      : Standard.String;
                     Pointer     : in out Integer;
                     Value       : out Aida.UTF8_Code_Point.T);

      --function Is_Valid_UTF8 (Source : String) return Boolean;

      --
      -- Length -- The length of an UTF-8 string
      --
      --    Source - The string containing UTF-8 encoded code points
      --
      -- Returns :
      --
      --    The number of UTF-8 encoded code points in Source
      --
      function Length (Source : Standard.String) return Natural;

      --
      -- Put -- Put one UTF-8 code point
      --
      --    Destination - The target string
      --    Pointer     - The position where to place the character
      --    Value       - The code point to put
      --
      -- This  procedure  puts  one  UTF-8  code  point into the string Source
      -- starting from the position Source (Pointer). Pointer is then advanced
      -- to the first character following the output.
      --
      procedure Put (Destination : in out Standard.String;
                     Pointer     : in out Integer;
                     Value       : Aida.UTF8_Code_Point.T);

      function To_Lowercase (Value : Standard.String) return Standard.String;

      function To_Uppercase (Value : Standard.String) return Standard.String;

   end UTF8;

end Aida;
