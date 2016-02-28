with Ada.Text_IO;
with Interfaces.C.Strings;
with XCB;

-- Close application by pressing any key on the keyboard.
-- Closing by clicking on the "X"-button will generate
-- the error message "I/O error has occurred!? How is this possible?".
procedure Main is
   use type XCB.GC_Type;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;
   use type XCB.Generic_Event_Access_Type;

   procedure Test_Cookie (Cookie : XCB.Void_Cookie_Type;
                          Connection : XCB.Connection_Access_Type;
                          Error_Message : String)
   is
      Error : XCB.Generic_Error_Access_Type;
   begin
      Error := XCB.Request_Check (Connection, Cookie);

      if (Error /= null) then
         fprintf (stderr, "ERROR: %s : %"PRIu8"\n", errMessage , error->error_code);
         XCB.Disconnect (Connection);
         GNAT.OS_Lib.OS_Exit (-1);
      end if;
   end Test_Cookie;

   function Get_Font_GC (Connection : XCB.Connection_Access_Type;
                         Screen : XCB.Screen_Access_Type;
                         Window : XCB.Window_Id_Type;
                         Font_Name : String) return XCB.GC_Type
   is
      Font : XCB.Font_Id_Type;

      Font_Cookie : XCB.Void_Cookie_Type;

      GC : XCB.Graphical_Context_Type;

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
      Mask          := XCB.Constants.XCB_GC_FOREGROUND or XCB.Constants.XCB_GC_BACKGROUND or XCB.Constants.XCB_GC_FONT;
      Value_List := (0 => Screen.Black_Pixel,
                     1 => Screen.White_Pixel,
                     2 => Font);

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
                        Screen     : XCB.Screen_Type;
                        Window     : XCB.Window_Id_Type;
                        X_1        : Interfaces.Integer_16;
                        Y_1        : Interfaces.Integer_16;
                        Label      : String)
   is
      GC : XCB.Graphical_Context_Type;
      Text_Cookie : XCB.Void_Cookie_Type;

      GC_Cookie : XCB.Void_Cookie_Type;
   begin
      -- Get graphics context
      GC := Get_Font_GC (Connection, Screen, Window, "fixed");

      -- Draw the text
      Text_Cookie = XCB.Image_Text_8_Checked (Connection,
                                              Label'Length,
                                              Window,
                                              GC,
                                              X_1, Y_1,
                                              Label);

      Test_Cookie(Text_Cookie, Connection, "can't paste text");


      -- Free the gc
      GC_Cookie := XCB.Free_GC (Connection, GC);

      Test_Cookie(GC_Cookie, Connection, "can't free gc");
   end Draw_Text;

   Connection : XCB.Connection_Access_Type;
   Screen : XCB.Screen_Access_Type;
--     W : XCB.Window_Id_Type;
--     E : XCB.Generic_Event_Access_Type;
--
--     Mask : XCB.GC_Type;
--     Values : aliased XCB.Value_List_Array (0..1);
--
--     R : XCB.Rectangle_Type := (X => 20, Y => 20, Width => 60, Height => 60);

   Unused_Cookie : XCB.Void_Cookie_Type;
   pragma Unreferenced (Unused_Cookie);

   Flush_Number : Interfaces.C.int;

   WIDTH  : constant := 300;
   HEIGHT : constant := 100;

   Screen_Number : Interfaces.C.int;

   Iterator : XCB.Screen_Iterator_Type;
begin
   -- Get the connection
   Connection := XCB.Connect(Interfaces.C.Strings.Null_Ptr, Screen_Number'Access);

   if XCB.Connection_Has_Error (Connection) /= 0 then
      Ada.Text_IO.Put_Line ("Failed to connect to the x-server");
      return;
   end if;

   --  Get the current screen
   Iterator := XCB.Setup_Roots_Iterator ( XCB.Get_Setup (C) );

   -- We want the screen at index screenNum of the iterator
   for I in Integer range 0..(Screen_Number - 1) loop
      XCB.Screen_Next (Iterator'Access);
   end loop;

   Screen = Iterator.data;

   -- Create window
   W := XCB.Generate_Id (C);
   Mask := XCB.Constants.XCB_CW_BACK_PIXEL or XCB.Constants.XCB_CW_EVENT_MASK;
   Values (0) := S.White_Pixel;
   Values (1) := XCB.Constants.XCB_EVENT_MASK_EXPOSURE or XCB.Constants.XCB_EVENT_MASK_KEY_PRESS;
   Unused_Cookie := XCB.Create_Window (C            => C,
                                       Depth        => S.Root_Depth,
                                       Window_Id    => W,
                                       Parent       => S.Root,
                                       X            => 10,
                                       Y            => 10,
                                       Width        => 100,
                                       Height       => 100,
                                       Border_Width => 1,
                                       U_Class      => XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT,
                                       Visual_Id    => S.Root_Visual_Id,
                                       Value_Mask   => Mask,
                                       Value_List   => Values);

   -- Map (show) the window
   Unused_Cookie := XCB.Map_Window (C, W);

   Flush_Number := XCB.Flush (C);

   if Flush_Number <= 0 then
      Ada.Text_IO.Put_Line ("Failed to flush");
   end if;

   -- Event loop
   loop
      E := XCB.Wait_For_Event (C);

      if E /= null then
         Ada.Text_IO.Put_Line ("Response kind:" & E.Response_Kind'Img);
         case (E.Response_Kind) is
            when XCB.Constants.XCB_KEY_PRESS =>
               -- Exit on key press
               exit;
            when others =>
               null;
         end case;
         XCB.Free (E);
      else
         Ada.Text_IO.Put_Line ("I/O error has occurred!? How is this possible?");
         exit;
      end if;
   end loop;


--
--          if (!screen) {
--              fprintf (stderr, "ERROR: can't get the current screen\n");
--              xcb_disconnect (connection);
--              return -1;
--          }
--
--
--          /* create the window */
--          xcb_window_t window = xcb_generate_id (connection);
--
--          uint32_t mask = XCB_CW_BACK_PIXEL | XCB_CW_EVENT_MASK;
--          uint32_t values[2];
--          values[0] = screen->white_pixel;
--          values[1] = XCB_EVENT_MASK_KEY_RELEASE |
--                      XCB_EVENT_MASK_BUTTON_PRESS |
--                      XCB_EVENT_MASK_EXPOSURE |
--                      XCB_EVENT_MASK_POINTER_MOTION;
--
--          xcb_void_cookie_t windowCookie = xcb_create_window_checked (connection,
--                                                                      screen->root_depth,
--                                                                      window, screen->root,
--                                                                      20, 200,
--                                                                      WIDTH, HEIGHT,
--                                                                      0, XCB_WINDOW_CLASS_INPUT_OUTPUT,
--                                                                      screen->root_visual,
--                                                                      mask, values);
--
--          testCookie(windowCookie, connection, "can't create window");
--
--          xcb_void_cookie_t mapCookie = xcb_map_window_checked (connection, window);
--
--          testCookie(mapCookie, connection, "can't map window");
--
--          xcb_flush(connection);  // make sure window is drawn
--
--
--          /* event loop */
--          xcb_generic_event_t  *event;
--          while (1) { ;
--              if ( (event = xcb_poll_for_event(connection)) ) {
--                  switch (event->response_type & ~0x80) {
--                      case XCB_EXPOSE: {
--                          drawText (connection,
--                                    screen,
--                                    window,
--                                    10, HEIGHT - 10,
--                                    "Press ESC key to exit..." );
--                          break;
--                      }
--                      case XCB_KEY_RELEASE: {
--                          xcb_key_release_event_t *kr = (xcb_key_release_event_t *)event;
--
--                          switch (kr->detail) {
--                              /* ESC */
--                              case 9: {
--                                  free (event);
--                                  xcb_disconnect (connection);
--                                  return 0;
--                              }
--                          }
--                          free (event);
--                      }
--                  }
--              }
--          }
--          return 0;
--      }

   XCB.Disconnect (C);
end Main;
