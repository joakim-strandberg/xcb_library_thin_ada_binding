with Aida.XML;
with X_Proto_XML;
with Basic_Bounded_Dynamic_Pools;
with Main_Allocator_Interface;

package XML_File_Parser is

   procedure Parse (Contents      : String;
                    Xcb_V         : in out X_Proto_XML.Xcb.Ptr;
                    Pool          : in out Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool;
                    A             : in Main_Allocator_Interface.T'Class;
                    Error_Message : out Aida.XML.Error_Message_T;
                    Is_Success    : out Boolean);

end XML_File_Parser;
