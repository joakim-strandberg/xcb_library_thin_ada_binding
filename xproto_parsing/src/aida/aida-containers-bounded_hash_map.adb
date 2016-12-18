with Ada.Exceptions;

package body Aida.Containers.Bounded_Hash_Map is

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

   procedure Insert (This    : in out T;
                     Key     : Key_T;
                     Element : Element_T)
   is
      H : Hash32_T := Normalize_Index (Hash (Key));
      BI : Bucket_Index_T := Bucket_Index_T (H);
   begin
      if Int32_T (This.Number_Of_Stored_Elements (BI)) < Max_Collisions then
         declare
            CI : Collision_Index_T := Collision_Index_T (This.Number_Of_Stored_Elements (BI));
         begin
            This.Buckets (BI)(CI).Key               := Key;
            This.Buckets (BI)(CI).Element           := Element;
            This.Number_Of_Stored_Elements (BI) := This.Number_Of_Stored_Elements (BI) + 1;
         end;
      else
         Ada.Exceptions.Raise_Exception (E       => End_Of_Container_Exception'Identity,
                                         Message => "Insert");
      end if;
   end Insert;

   function Element (This : T;
                     Key  : Key_T) return Element_T
   is
      H : Hash32_T := Normalize_Index (Hash (Key));
      BI : Bucket_Index_T := Bucket_Index_T (Aida.Int32.T (H) mod Max_Collisions);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..Collision_Index_T (This.Number_Of_Stored_Elements (BI)) loop
         if This.Buckets (BI)(I).Key = Key then
            return This.Buckets (BI)(I).Element;
         end if;
      end loop;
      Ada.Exceptions.Raise_Exception (E       => Key_Not_Found_Exception'Identity,
                                      Message => "Element");
   end Element;

   function Find_Element (This : T;
                          Key  : Key_T) return Find_Element_Result_T
   is
      H : Hash32_T := Normalize_Index (Hash (Key));
      BI : Bucket_Index_T := Bucket_Index_T (Aida.Int32.T (H) mod Max_Collisions);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..Collision_Index_T (This.Number_Of_Stored_Elements (BI)) loop
         if This.Buckets (BI)(I).Key = Key then
            return (Element_Exists => True,
                    Element        => This.Buckets (BI)(I).Element);
         end if;
      end loop;
      return (Element_Exists => False);
   end Find_Element;

   function Contains (This : T;
                      Key  : Key_T) return Boolean
   is
      H : Hash32_T := Normalize_Index (Hash (Key));
      BI : Bucket_Index_T := Bucket_Index_T (Aida.Int32.T (H) mod Max_Collisions);
   begin
      for I in Collision_Index_T range Collision_Index_T'First..Collision_Index_T (This.Number_Of_Stored_Elements (BI)) loop
         if This.Buckets (BI)(I).Key = Key then
            return True;
         end if;
      end loop;
      return False;
   end Contains;

end Aida.Containers.Bounded_Hash_Map;
