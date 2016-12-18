generic
   with procedure Start_Tag (Tag_Name      : String;
                             Parent_Tags   : DL.Collection;
                             Error_Message : out Error_Message_P.T;
                             Is_Success    : out Boolean);

   with procedure Attribute (Attribute_Name              : String;
                             Attribute_Value             : String;
                             Parent_Tags_And_Current_Tag : DL.Collection;
                             Error_Message               : out Error_Message_P.T;
                             Is_Success                  : out Boolean);

   with procedure End_Tag (Tag_Name    : String;
                           Parent_Tags : DL.Collection;
                           Error_Message : out Error_Message_P.T;
                           Is_Success    : out Boolean);

   with procedure End_Tag (Tag_Name      : String;
                           Tag_Value     : String;
                           Parent_Tags   : DL.Collection;
                           Error_Message : out Error_Message_P.T;
                           Is_Success    : out Boolean);
procedure SXML.Generic_Parse_XML_File (Contents      : String;
                                       Error_Message : out Error_Message_P.T;
                                       Is_Success    : out Boolean);
