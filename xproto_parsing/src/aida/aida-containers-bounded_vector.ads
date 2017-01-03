pragma Ada_95;

generic
   type Element_T is private;
   with function "=" (L, R : Element_T) return Boolean is <>;
   MAX_LENGTH : Positive := 100;
package Aida.Containers.Bounded_Vector is
--     pragma Preelaborate;

   type Extended_Index_T is new Integer range 0..MAX_LENGTH;

   subtype Index_T is Extended_Index_T range Extended_Index_T'First + 1..Extended_Index_T'Last;


   type Elements_Array_T is array (Index_T range <>) of aliased Element_T;

   type T is private;
   -- The vector type is limited because making copies would mean
   -- introducing reference counting to know when to deallocate the vector.

   function "=" (L, R : T) return Boolean;

   procedure Append (This     : in out T;
                     New_Item : Element_T);

   function Contains (This    : T;
                      Element : Element_T) return Boolean;

   function Last_Index (This : T) return Extended_Index_T;

   function Element (This  : T;
                     Index : Index_T) return Element_T;

   function Is_Empty (This : T) return Boolean;

   function Last_Element (This : T) return Element_T;

   procedure Delete_Last (This : in out T);

   procedure Clear (This : out T);

   generic
      with procedure Do_Something (Elements : Elements_Array_T);
   procedure Act_On_Immutable_Elements (This : in T);

   generic
      with procedure Do_Something (Elements : in out Elements_Array_T);
   procedure Act_On_Mutable_Elements (This : in out T);

   type Element_Const_Ptr is access constant Element_T;

   function Const_Ref (This  : T;
                       Index : Index_T) return Element_Const_Ptr;

   type Ptr is access all T;

private

   pragma Inline (Append);
   pragma Inline (Last_Index);
   pragma Inline (Is_Empty);
   pragma Inline (Last_Element);
   pragma Inline (Delete_Last);

   subtype Items_T is Elements_Array_T (Index_T'Range);


   type Items_Ptr is access constant Items_T;

   type T is
      record
         Items      : aliased Items_T;
         Last_Index : Extended_Index_T := 0;
      end record;

end Aida.Containers.Bounded_Vector;
