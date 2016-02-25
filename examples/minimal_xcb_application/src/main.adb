with Ada.Text_IO;
with Interfaces.C.Strings;
with XCB;

-- Close application by pressing any key on the keyboard.
-- Closing by clicking on the "X"-button will generate
-- the error message "I/O error has occurred!? How is this possible?".
procedure Main is
   use type XCB.GC_Type;
   use type Interfaces.C.unsigned;
   use type XCB.Generic_Event_Access_Type;

   C : XCB.Connection_Access_Type;
   S : XCB.Screen_Access_Type;
   W : XCB.Window_Type;
   E : XCB.Generic_Event_Access_Type;

   Mask : XCB.GC_Type;
   Values : aliased XCB.Value_List_Array (0..1);

   Done : Interfaces.C.int := 0;

   R : XCB.Rectangle_Type := (X => 20, Y => 20, Width => 60, Height => 60);

   Unused_Cookie : XCB.Void_Cookie_Type;
   pragma Unreferenced (Unused_Cookie);

   Flush_Number : Interfaces.C.int;

   use type Interfaces.C.int;
begin
   -- open connection with the server
   C := XCB.Connect(Interfaces.C.Strings.Null_Ptr, null);

   if XCB.Connection_Has_Error (C) /= 0 then
      Ada.Text_IO.Put_Line ("Failed to connect to the x-server");
      return;
   end if;

   -- Get the first screen
   S := XCB.Setup_Roots_Iterator ( XCB.Get_Setup (C) ).Data;

   -- Create window
   W := XCB.Generate_Id (C);
   Mask := XCB.XCB_CW_BACK_PIXEL or XCB.XCB_CW_EVENT_MASK;
   Values (0) := XCB.XCB_NONE; --S.White_Pixel;
   Values (1) := XCB.XCB_EVENT_MASK_EXPOSURE or XCB.XCB_EVENT_MASK_KEY_PRESS;
   Unused_Cookie := XCB.Create_Window (C            => C,
                                       Depth        => S.Root_Depth,
                                       Wid          => W,
                                       Parent       => S.Root,
                                       X            => 10,
                                       Y            => 10,
                                       Width        => 100,
                                       Height       => 100,
                                       Border_Width => 1,
                                       U_Class      => XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT,
                                       Visual       => S.Root_Visual,
                                       Value_Mask   => Mask,
                                       Value_List   => Values);

   -- Map (show) the window
   Unused_Cookie := XCB.Map_Window (C, W);

   Flush_Number := XCB.Flush (C);

   if Flush_Number <= 0 then
      Ada.Text_IO.Put_Line ("Failed to flush");
   end if;

   -- Event loop
   while Done = 0 loop
      E := XCB.Wait_For_Event (C);

      if E /= null then
         Ada.Text_IO.Put_Line ("Response kind:" & E.Response_Kind'Img);
         case (E.Response_Kind) is
            when XCB.XCB_EXPOSE =>
               -- Draw or redraw the window
               --              xcb_poly_fill_rectangle(c, w, g,  1, &r);
               Flush_Number := XCB.Flush (C);
            when XCB.XCB_KEY_PRESS =>
               -- Exit on key press
               Done := 1;
            when others =>
               null;
         end case;
         XCB.Free (E);
      else
         Ada.Text_IO.Put_Line ("I/O error has occurred!? How is this possible?");
         exit;
      end if;
   end loop;

   XCB.Disconnect (C);
end Main;
