with Ada.Command_Line;
with Ada.Text_IO;
with Interfaces.C.Strings;
with XCB;
with GNAT.OS_Lib;

-- Close application by pressing any key on the keyboard.
-- Closing by clicking on the "X"-button will generate
-- the error message "I/O error has occurred!? How is this possible?".
procedure Main is
   use type XCB.Gcontext_Id_Type;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Integer_16;
   use type XCB.CW_Type;
   use type XCB.Generic_Event_Access_Type;
   use type XCB.Generic_Error_Access_Type;
   use type XCB.Screen_Access_Type;
   use type XCB.Gc_Type;
   use type XCB.Event_Mask_Type;

   procedure Test_Cookie (Cookie        : XCB.Void_Cookie_Type;
                          Connection    : XCB.Connection_Access_Type;
                          Error_Message : String)
   is
      Error : XCB.Generic_Error_Access_Type;
   begin
      Error := XCB.Request_Check (Connection, Cookie);

      if (Error /= null) then
         Ada.Text_IO.Put_Line ("ERROR: " & Error_Message & " : " & Error.Error_Code'Img);
         XCB.Disconnect (Connection);
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
   end Test_Cookie;

   function Get_Font_GC (Connection : XCB.Connection_Access_Type;
                         Screen : XCB.Screen_Access_Type;
                         Window : XCB.Window_Id_Type;
                         Font_Name : String) return XCB.Gcontext_Id_Type
   is
      Font : XCB.Font_Id_Type;

      Font_Cookie : XCB.Void_Cookie_Type;

      GC : XCB.Gcontext_Id_Type;

      Mask : Interfaces.Unsigned_32;

      Value_List : aliased XCB.Value_List_Array (0..2);

      GC_Cookie : XCB.Void_Cookie_Type;
   begin
      -- Get font
      Font := XCB.Generate_Id (Connection);
      Font_Cookie := XCB.Open_Font_Checked (Connection,
                                            Font,
                                            Font_Name'Length,
                                            Interfaces.C.Strings.New_String (Font_Name));

      Test_Cookie(Font_Cookie, Connection, "can't open font");


      -- Create graphics context
      GC            := XCB.Generate_Id (Connection);
      Mask          := Interfaces.Unsigned_32 (XCB.XCB_GC_FOREGROUND or XCB.XCB_GC_BACKGROUND or XCB.XCB_GC_FONT);
      Value_List := (0 => Screen.Black_Pixel,
                     1 => Screen.White_Pixel,
                     2 => Interfaces.Unsigned_32 (Font));

      GC_Cookie := XCB.Create_GC_Checked (Connection,
                                          GC,
                                          Window,
                                          Mask,
                                          Value_List);

      Test_Cookie(GC_Cookie, Connection, "can't create gc");

      -- Close font
      Font_Cookie := XCB.Close_Font_Checked (Connection, Font);

      Test_Cookie(Font_Cookie, Connection, "can't close font");

      return GC;
   end Get_Font_GC;

   procedure Draw_Text (Connection : XCB.Connection_Access_Type;
                        Screen     : XCB.Screen_Access_Type;
                        Window     : XCB.Window_Id_Type;
                        X_1        : Interfaces.Integer_16;
                        Y_1        : Interfaces.Integer_16;
                        Label      : String)
   is
      GC : XCB.Gcontext_Id_Type;
      Text_Cookie : XCB.Void_Cookie_Type;

      GC_Cookie : XCB.Void_Cookie_Type;
   begin
      -- Get graphics context
      GC := Get_Font_GC (Connection, Screen, Window, "fixed");

      -- Draw the text
      Text_Cookie := XCB.Image_Text_8_Checked (Connection,
                                               Label'Length,
                                               XCB.Drawable_Id_Type (Window),
                                               GC,
                                               X_1, Y_1,
                                               Interfaces.C.Strings.New_String (Label));

      Test_Cookie(Text_Cookie, Connection, "can't paste text");


      -- Free the gc
      GC_Cookie := XCB.Free_GC (Connection, GC);

      Test_Cookie(GC_Cookie, Connection, "can't free gc");
   end Draw_Text;

   Connection : XCB.Connection_Access_Type;

   Screen : XCB.Screen_Access_Type;

   Window : XCB.Window_Id_Type;

   Event : XCB.Generic_Event_Access_Type;

   Mask : XCB.CW_Type;

   Values : aliased XCB.Value_List_Array (0..1);

   Window_Cookie : XCB.Void_Cookie_Type;
   Map_Cookie    : XCB.Void_Cookie_Type;

   Flush_Number : Interfaces.C.int;

   WIDTH  : constant := 300;
   HEIGHT : constant := 100;

   Screen_Number : aliased Interfaces.C.int;

   Iterator : aliased XCB.Screen_Iterator_Type;
begin
   -- Get the connection
   Connection := XCB.Connect(Interfaces.C.Strings.Null_Ptr, Screen_Number'Access);

   if XCB.Connection_Has_Error (Connection) /= 0 then
      Ada.Text_IO.Put_Line ("Failed to connect to the x-server");
      return;
   end if;

   --  Get the current screen
   Iterator := XCB.Setup_Roots_Iterator ( XCB.Get_Setup (Connection) );

   -- We want the screen at index screenNum of the iterator
   for I in Integer range 0..Integer (Screen_Number - 1) loop
      XCB.Screen_Next (Iterator'Unchecked_Access);
   end loop;

   Screen := Iterator.data;


   if Screen = null then
      Ada.Text_IO.Put_Line ("ERROR: can't get the current screen");
      XCB.Disconnect (Connection);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      return;
   end if;

   -- Create the window
   Window := XCB.Generate_Id (Connection);

   Mask := XCB.XCB_CW_BACK_PIXEL or XCB.XCB_CW_EVENT_MASK;
   Values (0) := Screen.White_Pixel;
   Values (1) := Interfaces.Unsigned_32 (
                                         XCB.XCB_EVENT_MASK_KEY_RELEASE or
                                           XCB.XCB_EVENT_MASK_BUTTON_PRESS or
                                             XCB.XCB_EVENT_MASK_EXPOSURE or
                                               XCB.XCB_EVENT_MASK_POINTER_MOTION
                                        );

   Window_Cookie := XCB.Create_Window (C            => Connection,
                                       Depth        => Screen.Root_Depth,
                                       Wid          => Window,
                                       Parent       => Screen.Root,
                                       X            => 20,
                                       Y            => 200,
                                       Width        => WIDTH,
                                       Height       => HEIGHT,
                                       Border_Width => 0,
                                       Class        => Interfaces.Unsigned_16 (XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT),
                                       Visual       => Screen.Root_Visual,
                                       Value_Mask   => Interfaces.Unsigned_32 (Mask),
                                       Value_List   => Values);


   Test_Cookie (Window_Cookie, Connection, "can't create window");

   Map_Cookie := XCB.Map_Window_Checked (Connection, Window);

   Test_Cookie(Map_Cookie, Connection, "can't map window");

   Flush_Number := XCB.Flush (Connection); -- make sure window is drawn

   if Flush_Number <= 0 then
      Ada.Text_IO.Put_Line ("Failed to flush");
   end if;

   -- Event loop
   loop
      Event := XCB.Poll_For_Event (Connection);

      if Event /= null then
         case (Event.Response_Kind mod 128) is
            when XCB.XCB_EXPOSE =>
               Draw_Text (Connection,
                          Screen,
                          Window,
                          10, HEIGHT - 10,
                          "Press ESC key to exit...");
            when XCB.XCB_KEY_RELEASE =>
               declare
                  KR : XCB.Key_Release_Event_Access_Type := XCB.To_Key_Release_Event (Event);
               begin
                  case KR.Detail is
                     when 9 =>
                        XCB.Free (Event);
                        XCB.Disconnect (Connection);
                        Ada.Command_Line.Set_Exit_Status (0);
                        return;
                     when others =>
                        Ada.Text_IO.Put_Line (KR.Detail'Img);
                  end case;
               end;
            when others =>
               null;
         end case;
         XCB.Free (Event);
      end if;
   end loop;
end Main;
