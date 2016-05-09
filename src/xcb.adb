package body XCB is

   function Generate_Id (C : Connection_Access_Type) return Drawable_Id_Type is
   begin
      return (Window_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

   function Generate_Id (C : Connection_Access_Type) return Fontable_Id_Type is
   begin
      return (Font_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

   function Generate_Id (C : Connection_Access_Type) return Colormap_Id_Type is
   begin
      return (Colormap_Id_Type (X_Id_Type'(Generate_Id (C))));
   end Generate_Id;

end XCB;
