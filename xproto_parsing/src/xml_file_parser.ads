with SXML;
with X_Proto;

package XML_File_Parser is

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto.Xcb.Ptr;
                    Error_Message : out SXML.Error_Message_T;
                    Is_Success    : out Boolean);

end XML_File_Parser;