with X_Proto_XML;
with Basic_Bounded_Dynamic_Pools;

package XCB_Package_Creator is

   procedure Create_XCB_Package (XCB  : X_Proto_XML.Xcb.T;
                                 Pool : in out Basic_Bounded_Dynamic_Pools.Basic_Dynamic_Pool);

end XCB_Package_Creator;
