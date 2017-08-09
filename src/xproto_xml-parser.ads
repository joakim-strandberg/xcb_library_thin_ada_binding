with Aida.XML;
with XProto_XML.Storage;
with XProto_XML.Max_Indices;

package XProto_XML.Parser is

   procedure Parse (Storage     : in out XProto_XML.Storage.T;
                    Max_Indices : in out XProto_XML.Max_Indices.T;
                    Contents    : in Aida.String_T;
                    Call_Result : in out Aida.XML.Procedure_Call_Result.T);

end XProto_XML.Parser;
