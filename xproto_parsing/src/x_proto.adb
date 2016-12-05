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

end X_Proto;
