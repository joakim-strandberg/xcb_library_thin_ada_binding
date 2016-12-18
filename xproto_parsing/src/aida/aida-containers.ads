package Aida.Containers is
--     pragma Pure;

   Container_Is_Empty_Exception : exception;
   -- Raised when trying to get an element from an empty container.

   Out_Of_Bounds_Exception : exception;
   -- Raised when trying to get an element that does not exist from a non-empty
   -- container.

   End_Of_Container_Exception : exception;
   -- Raised when trying to add more items to a container that has reached
   -- full capacity.

   type Max_Number_Of_Buckets_In_Hash_Map_T is range 4..(2**31);

   type Hash32_T is mod 2**32;
   for Hash32_T'Size use 32;

end Aida.Containers;
