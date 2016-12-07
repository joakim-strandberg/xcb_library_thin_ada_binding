with SXML;

package X_Proto.XML is

   procedure Parse (Contents      : String;
                    Xcb_V         : in out Xcb.Ptr;
                    Error_Message : out SXML.Error_Message_Type;
                    Is_Success    : out Boolean);

end X_Proto.XML;
