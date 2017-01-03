with X_Proto_XML;
with Bounded_Dynamic_Pools;

package XCB_Package_Creator is

   procedure Create_XCB_Package (XCB     : X_Proto_XML.Xcb.T;
                                 Subpool : in out Bounded_Dynamic_Pools.Scoped_Subpool);

end XCB_Package_Creator;
