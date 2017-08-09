with Generic_X_Proto_XML;

generic
   with package X_Proto_XML is new Generic_X_Proto_XML (<>);
package XCB_Package_Creator is

   procedure Create_XCB_Package (XCB : X_Proto_XML.Xcb.T);

end XCB_Package_Creator;
