package body XCB is

   function Generate_Id (C : Connection_Access_Type) return Window_Id_Type is
   begin
      return (Window_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

   function Generate_Id (C : Connection_Access_Type) return Gcontext_Id_Type is
   begin
      return (Gcontext_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

   function Generate_Id (C : Connection_Access_Type) return Font_Id_Type is
   begin
      return (Font_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

end XCB;
