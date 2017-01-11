with Ada.Exceptions;

package body Aida.Containers.Deepend_Bounded_Hash_Map is

   function New_Bucket_Array is
     new Bounded_Dynamic_Pools.Allocate_Huge_Item_Scoped_Subpool (Allocation_Type        => Bucket_Array_T,
                                                                  Allocation_Type_Access => Bucket_Array_Ptr);

   function New_Number_Of_Stored_Elements_Array is
     new Bounded_Dynamic_Pools.Allocate_Huge_Item_Scoped_Subpool (Allocation_Type        => Number_Of_Stored_Elements_Array_T,
                                                                  Allocation_Type_Access => Number_Of_Stored_Elements_Array_Ptr);

   procedure Create (This    : out T;
                     Subpool : Bounded_Dynamic_Pools.Scoped_Subpool) is
   begin
      This.Buckets := New_Bucket_Array (Subpool);
      This.Number_Of_Stored_Elements := New_Number_Of_Stored_Elements_Array (Subpool);
      This.Number_Of_Stored_Elements.all := (others => Number_Of_Stored_Elements_T'(0));
   end Create;

   function Normalize_Index (H : Hash32_T) return Hash32_T;
   pragma Inline (Normalize_Index);

   function Normalize_Index (H : Hash32_T) return Hash32_T is
   begin
      if H < Hash32_T (Max_Hash_Map_Size) then
         return H;
      else
         return H - ((H/Hash32_T (Max_Hash_Map_Size)))*Hash32_T (Max_Hash_Map_Size);
      end if;
   end Normalize_Index;

   procedure Hidden_Insert (This              : in out T;
                            Key               : Key_T;
                            New_Element       : Element_T;
                            BI                : Bucket_Index_T;
                            Exception_Message : Standard.String) is
   begin
      if Int32_T (This.Number_Of_Stored_Elements (BI)) < Max_Collisions then
         declare
            CI : constant Collision_Index_T := Collision_Index_T (This.Number_Of_Stored_Elements (BI) + 1);
         begin
            This.Buckets (BI)(CI).Key           := Key;
            This.Buckets (BI)(CI).Element       := New_Element;
            This.Number_Of_Stored_Elements (BI) := This.Number_Of_Stored_Elements (BI) + 1;
         end;
      else
         Ada.Exceptions.Raise_Exception (E       => End_Of_Container_Exception'Identity,
                                         Message => Exception_Message);
      end if;
   end Hidden_Insert;
   pragma Inline (Hidden_Insert);

   procedure Insert (This        : in out T;
                     Key         : Key_T;
                     New_Element : Element_T)
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..This.Number_Of_Stored_Elements (BI) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key , Key) then
            Ada.Exceptions.Raise_Exception (E       => Key_Already_Present_Exception'Identity,
                                            Message => "Insert");
         end if;
      end loop;

      Hidden_Insert (This, Key, New_Element, Bi, "Insert");
   end Insert;

   procedure Include (This        : in out T;
                      Key         : Key_T;
                      New_Element : Element_T)
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..This.Number_Of_Stored_Elements (BI) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key , Key) then
            This.Buckets (BI)(I).Key           := Key;
            This.Buckets (BI)(I).Element       := New_Element;
            return;
         end if;
      end loop;

      Hidden_Insert (This, Key, New_Element, Bi, "Include");
   end Include;

   procedure Delete (This : in out T;
                     Key  : Key_T)
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..This.Number_Of_Stored_Elements (BI) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key, Key) then
            declare
               LI : constant Collision_Index_T := This.Number_Of_Stored_Elements (BI); -- LI short for Last Index
            begin
               This.Buckets (BI)(I).Key     := This.Buckets (BI)(LI).Key;
               This.Buckets (BI)(I).Element := This.Buckets (BI)(LI).Element;
               This.Number_Of_Stored_Elements (BI) := LI - 1;
            end;
            return;
         end if;
      end loop;

      Ada.Exceptions.Raise_Exception (E       => Key_Not_Found_Exception'Identity,
                                      Message => "Delete");
   end Delete;

   function Element (This : T;
                     Key  : Key_T) return Element_T
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..This.Number_Of_Stored_Elements (BI) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key , Key) then
            return This.Buckets (BI)(I).Element;
         end if;
      end loop;
      Ada.Exceptions.Raise_Exception (E       => Key_Not_Found_Exception'Identity,
                                      Message => "Element");
   end Element;

   function Find_Element (This : T;
                          Key  : Key_T) return Find_Element_Result_T
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..This.Number_Of_Stored_Elements (BI) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key, Key) then
            return (Exists  => True,
                    Element => This.Buckets (BI)(I).Element);
         end if;
      end loop;
      return (Exists => False);
   end Find_Element;

   function Contains (This : T;
                      Key  : Key_T) return Boolean
   is
      H : constant Hash32_T := Normalize_Index (Hash (Key));
      BI : constant Bucket_Index_T := Bucket_Index_T (H);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..Collision_Index_T (This.Number_Of_Stored_Elements (BI)) loop
         if Equivalent_Keys (This.Buckets (BI)(I).Key, Key) then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

end Aida.Containers.Deepend_Bounded_Hash_Map;
