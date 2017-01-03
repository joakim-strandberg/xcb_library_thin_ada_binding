with Bounded_Dynamic_Pools;

package X_Proto_XML.Allocators is

   function New_Xcb is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Xcb.T,
                                                                            Allocation_Type_Access => Xcb.Ptr);

   function New_Struct is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Struct.T,
                                                                               Allocation_Type_Access => Struct.Ptr);

   function New_X_Id is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => X_Id.T,
                                                                             Allocation_Type_Access => X_Id.Ptr);

   function New_X_Id_Union is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => X_Id_Union.T,
                                                                                   Allocation_Type_Access => X_Id_Union.Ptr);

   function New_Type_Definition is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Type_Definition.T,
                                                                                        Allocation_Type_Access => Type_Definition.Ptr);

   function New_Enum is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Enum.T,
                                                                            Allocation_Type_Access => Enum.Ptr);

   function New_Event is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Event.T,
                                                                              Allocation_Type_Access => Event.Ptr);

   function New_Event_Copy is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Event_Copy.T,
                                                                                   Allocation_Type_Access => Event_Copy.Ptr);

   function New_Union is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Union.T,
                                                                              Allocation_Type_Access => Union.Ptr);

   function New_Error is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Error.T,
                                                                              Allocation_Type_Access => Error.Ptr);

   function New_Error_Copy is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Error_Copy.T,
                                                                                   Allocation_Type_Access => Error_Copy.Ptr);

   function New_Request is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Request.T,
                                                                                Allocation_Type_Access => Request.Ptr);

   function New_Field is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Field.T,
                                                                              Allocation_Type_Access => Field.Ptr);

   function New_Pad is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => Pad.T,
                                                                            Allocation_Type_Access => Pad.Ptr);

   function New_List is new Bounded_Dynamic_Pools.Allocation_Scoped_Subpool (Allocation_Type        => List.T,
                                                                             Allocation_Type_Access => List.Ptr);

   function New_Struct_Field (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr;

   function New_Struct_Pad (Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) return X_Proto_XML.Struct.Fs.Member_Ptr;


end X_Proto_XML.Allocators;
