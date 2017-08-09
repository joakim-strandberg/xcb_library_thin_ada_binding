with Aida.Bounded_String;
with XProto_XML.Doc_Tag;

package XProto_XML.Storage with SPARK_Mode is

   type Header_Comment_T is new Aida.Bounded_String.T (1500);

   type Doc_Array_T is array (Doc_Id_T) of Doc_Tag.T;

   type T is limited
      record
         Header_Comment : Header_Comment_T;
         Doc : Doc_Array_T;
      end record;

end XProto_XML.Storage;
