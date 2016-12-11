package Aida.Containers is
   pragma Pure;

   Container_Is_Empty_Exception : exception;
   -- Raised when trying to get an element from an empty container.

   Out_Of_Bounds_Exception : exception;
   -- Raised when trying to get an element that does not exist from a non-empty
   -- container.

   End_Of_Container_Exception : exception;
   -- Raised when trying to add more items to a container that has reached
   -- full capacity.

end Aida.Containers;
