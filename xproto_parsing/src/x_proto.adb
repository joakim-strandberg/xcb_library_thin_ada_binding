with Ada.Strings.Unbounded;

package body X_Proto is

   package body Struct is

      function Name (This : T) return Fs.Const_Name_Ptr is
      begin
         return This.Name'Unchecked_Access;
      end Name;

      function Members (This : T) return Fs.Member_Vectors.Vector is
      begin
         return (This.Members);
      end Members;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.Name := (Exists => True,
                       Value  => Name);
      end Set_Name;

      procedure Append_Member (This   : in out T;
                               Member : Fs.Member_Ptr) is
      begin
         This.Members.Append (Member);
      end Append_Member;

   end Struct;

   package body X_Id_Type is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

   end X_Id_Type;

   package body Type_P is

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      procedure Set_Value (This : in out T;
                           Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                           Value  => Name);
      end Set_Value;

   end Type_P;

   package body X_Id_Union is

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Kinds (This : T) return Fs.Type_Vector_Const_Ptr is
      begin
         return This.My_Kinds'Unchecked_Access;
      end Kinds;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Append_Kind (This : in out T;
                             Kind : Type_P.Ptr) is
      begin
         This.My_Kinds.Append (Kind);
      end Append_Kind;

   end X_Id_Union;

   package body Field is

      function Kind (This : T) return Fs.Kind_Const_Ptr is
      begin
         return This.My_Kind'Unchecked_Access;
      end Kind;

      function Name (This : T) return Fs.Name_Const_Ptr is
      begin
         return This.My_Name'Unchecked_Access;
      end Name;

      function Enum (This : T) return Fs.Enum_Const_Ptr is
      begin
         return This.My_Enum'Unchecked_Access;
      end Enum;

      function Mask (This : T) return Fs.Mask_Const_Ptr is
      begin
         return This.My_Mask'Unchecked_Access;
      end Mask;

      function Alt_Enum (This : T) return Fs.Alt_Enum_Const_Ptr is
      begin
         return This.My_Alt_Enum'Unchecked_Access;
      end Alt_Enum;

      function Value (This : T) return Fs.Value_Const_Ptr is
      begin
         return This.My_Value'Unchecked_Access;
      end Value;

      procedure Set_Kind (This : in out T;
                          Kind : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Kind := (Exists => True,
                          Value  => Kind);
      end Set_Kind;

      procedure Set_Name (This : in out T;
                          Name : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Name := (Exists => True,
                          Value  => Name);
      end Set_Name;

      procedure Set_Enum (This : in out T;
                          Enum : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Enum := (Exists => True,
                          Value  => Enum);
      end Set_Enum;

      procedure Set_Mask (This : in out T;
                          Mask : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Mask := (Exists => True,
                          Value  => Mask);
      end Set_Mask;

      procedure Set_Value (This  : in out T;
                           Value : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Value := (Exists => True,
                           Value  => Value);
      end Set_Value;

      procedure Set_Alt_Enum (This     : in out T;
                              Alt_Enum : Aida.Strings.Unbounded_String_Type) is
      begin
         This.My_Alt_Enum := (Exists => True,
                              Value  => Alt_Enum);
      end Set_Alt_Enum;

   end Field;

   package body Pad is

      function Bytes (This : T) return Fs.Bytes_Const_Ptr is
      begin
         return This.My_Bytes'Unchecked_Access;
      end Bytes;

      procedure Set_Bytes (This  : in out T;
                           Bytes : Positive) is
      begin
         This.My_Bytes := (Exists => True,
                           Value  => Bytes);
      end Set_Bytes;

   end Pad;

end X_Proto;
