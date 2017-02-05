with Aida.XML;
with Generic_X_Proto_XML;

generic
   with package X_Proto_XML is new Generic_X_Proto_XML (<>);
package XML_File_Parser is

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    Error_Message : out Aida.XML.Error_Message_T;
                    Is_Success    : out Boolean);

end XML_File_Parser;
