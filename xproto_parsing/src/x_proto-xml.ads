with SXML;

package X_Proto.XML is

   procedure Parse (Contents      : String;
                    Xcb           : in out Xcb_Access_Type;
                    Error_Message : out SXML.Error_Message_Type;
                    Is_Success    : out Boolean);

end X_Proto.XML;
