pragma Ada_95;

generic
   type Key_T is private;
   type Element_T is private;

   with function Hash (Key : Key_T) return Hash32_T;
   with function Equivalent_Keys (Left, Right : Key_T) return Boolean;
--     with function "=" (Left, Right : Element_T) return Boolean is <>;

   Max_Hash_Map_Size : Max_Hash_Map_Size_T;

   Max_Collisions : Aida.Int32.T := 5;
package Aida.Containers.Bounded_Hash_Map is

   type T is limited private;

   procedure Insert (This    : in out T;
                     Key     : Key_T;
                     Element : Element_T);

   function Element (This : T;
                     Key  : Key_T) return Element_T;

   type Find_Element_Result_T (Exists : Boolean) is
      record
         case Exists is
            when True  => Element : Element_T;
            when False => null;
         end case;
      end record;

   function Find_Element (This : T;
                          Key  : Key_T) return Find_Element_Result_T;

   function Contains (This : T;
                      Key  : Key_T) return Boolean;

   procedure Delete (This : in out T;
                     Key  : Key_T);

   type Ptr is access all T;

private

   type Node_T is
      record
         Key     : Key_T;
         Element : Element_T;
      end record;

   subtype Bucket_Index_T is Hash32_T range Hash32_T'(0)..Hash32_T (Max_Hash_Map_Size - 1);

   type Number_Of_Stored_Elements_T is new Int32_T range Int32_T'(0)..Int32_T (Max_Collisions);

   type Number_Of_Stored_Elements_Array_T is array (Bucket_Index_T) of Number_Of_Stored_Elements_T;

   subtype Collision_Index_T is Number_Of_Stored_Elements_T range Number_Of_Stored_Elements_T'(1)..Number_Of_Stored_Elements_T (Max_Collisions);

   type Collision_Array_T is array (Collision_Index_T) of Node_T;

   type Bucket_Array_T is array (Bucket_Index_T) of Collision_Array_T;

   type T is
      record
         Buckets                   : aliased Bucket_Array_T;
         Number_Of_Stored_Elements : Number_Of_Stored_Elements_Array_T := (others => Number_Of_Stored_Elements_T'(0));
      end record;

end Aida.Containers.Bounded_Hash_Map;
