with Aida.XML;
with X_Proto_XML;
with Bounded_Dynamic_Pools;

package XML_File_Parser is

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    Subpool       : in out Bounded_Dynamic_Pools.Scoped_Subpool;
                    Error_Message : out Aida.XML.Error_Message_T;
                    Is_Success    : out Boolean);

end XML_File_Parser;
