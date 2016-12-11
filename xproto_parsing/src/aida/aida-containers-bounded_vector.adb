with Ada.Exceptions;

package body Aida.Containers.Bounded_Vector is

   procedure Append (This     : in out T;
                     New_Item : Element_T) is
   begin
      if This.Last_Index = 0 then
         This.Last_Index := 1;
         This.Items (1) := New_Item;
      else
         if This.Last_Index = Extended_Index_T (MAX_LENGTH) then
            Ada.Exceptions.Raise_Exception (E       => End_Of_Container_Exception'Identity,
                                            Message => "Append");
         end if;
         This.Last_Index := This.Last_Index + 1;
         This.Items (Index_T (This.Last_Index)) := New_Item;
      end if;
   end Append;

   function Element (This  : T;
                     Index : Index_T) return Element_T is
   begin
      if Index > This.Last_Index then
         Ada.Exceptions.Raise_Exception (E       => Out_Of_Bounds_Exception'Identity,
                                         Message => "Element");
      else
         return This.Items (Index);
      end if;
   end Element;

   function Last_Index (This : T) return Extended_Index_T is
   begin
      return This.Last_Index;
   end Last_Index;

   function Last_Element (This : T) return Element_T is
   begin
      if This.Last_Index = 0 then
         Ada.Exceptions.Raise_Exception (E       => Container_Is_Empty_Exception'Identity,
                                         Message => "Last_Element");
      else
         return This.Items (Index_T (This.Last_Index));
      end if;
   end Last_Element;

   function Is_Empty (This : T) return Boolean is
   begin
      if This.Last_Index < 1 then
         return True;
      else
         return False;
      end if;
   end Is_Empty;

   procedure Delete_Last (This : in out T) is
   begin
      if This.Last_Index = 0 then
         Ada.Exceptions.Raise_Exception (E       => Container_Is_Empty_Exception'Identity,
                                         Message => "Delete_Last");
      else
         This.Last_Index := This.Last_Index - 1;
      end if;
   end Delete_Last;

   procedure Act_On_Immutable_Elements (This : T) is
   begin
      Do_Something (This.Items (1..Index_T(This.Last_Index)));
   end Act_On_Immutable_Elements;

   procedure Act_On_Mutable_Elements (This : in out T) is
   begin
      Do_Something (This.Items (1..Index_T(This.Last_Index)));
   end Act_On_Mutable_Elements;

end Aida.Containers.Bounded_Vector;
