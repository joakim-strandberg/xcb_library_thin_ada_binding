generic
   with procedure Start_Tag (Tag_Name      : Standard.String;
                             Parent_Tags   : DL.T;
                             Error_Message : out Error_Message_P.T;
                             Is_Success    : out Boolean);

   with procedure Attribute (Attribute_Name              : Standard.String;
                             Attribute_Value             : Standard.String;
                             Parent_Tags_And_Current_Tag : DL.T;
                             Error_Message               : out Error_Message_P.T;
                             Is_Success                  : out Boolean);

   with procedure End_Tag (Tag_Name    : Standard.String;
                           Parent_Tags : DL.T;
                           Error_Message : out Error_Message_P.T;
                           Is_Success    : out Boolean);

   with procedure End_Tag (Tag_Name      : Standard.String;
                           Tag_Value     : Standard.String;
                           Parent_Tags   : DL.T;
                           Error_Message : out Error_Message_P.T;
                           Is_Success    : out Boolean);
procedure Aida.XML.Parse_XML_File (Contents      : Standard.String;
                                   Error_Message : out Error_Message_P.T;
                                   Is_Success    : out Boolean);
