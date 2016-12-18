package body Aida.Bounded_String is

   use Aida.String;

   procedure Initialize (This : out T;
                         Text : Standard.String) is
   begin
      if Text'Length > Maximum_Length_Of_Bounded_String then
         raise Out_Of_Bounds_Exception;
      end if;

      for I in Integer range 1..Text'Length loop
         This.Text (I) := Text (Text'First - 1 + I);
      end loop;

      This.Text_Length := Text'Length;
   end Initialize;

   procedure Initialize (This                 : out T;
                         Text                 : Standard.String;
                         Has_Truncated_String : out Boolean)
   is
      Text_Length : constant Integer := Text'Length;
   begin
      if Text_Length > Maximum_Length_Of_Bounded_String then
         for I in Integer range 1..Maximum_Length_Of_Bounded_String loop
            This.Text (I) := Text (Text'First - 1 + I);
         end loop;

         Has_Truncated_String := True;
         This.Text_Length := Maximum_Length_Of_Bounded_String;
      else
         for I in Integer range 1..Text_Length loop
            This.Text (I) := Text (Text'First - 1 + I);
         end loop;

         Has_Truncated_String := False;
         This.Text_Length := Text'Length;
      end if;
   end Initialize;

   procedure Append (Target : in out T;
                     Source : Standard.String) is
   begin
      for I in Integer range Source'First..Source'Last loop
         Target.Text (Target.Text_Length + I) := Source (I);
      end loop;
      Target.Text_Length := Target.Text_Length + Source'Length;
   end Append;

   function To_String (This : T) return Standard.String is
   begin
      return (This.Text(1 .. This.Text_Length));
   end To_String;

   function Equals (This   : T;
                    Object : Standard.String) return Boolean is
   begin
      return This.To_String = Object;
   end Equals;

   function "="(Left, Right : T) return Boolean is
   begin
      if Left.Text_Length = Right.Text_Length then
         if Left.Text_Length = 0 then
            return True;
         end if;

         return Left.To_String = Right.To_String;
      end if;

      return False;
   end "=";

   function Length (This : T) return Length_Type is
   begin
      return (This.Text_Length);
   end Length;

   function Hash32 (This : T) return Hash32_T is
   begin
      return Hash32 (String_T (This.Text (1..This.Length)));
   end Hash32;

end Aida.Bounded_String;
