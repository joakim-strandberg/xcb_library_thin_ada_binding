with Ada.Text_IO;
with Interfaces.C.Strings;
with System;
with XCB;

-- Close application by pressing any key on the keyboard.
-- Closing by clicking on the "X"-button will generate
-- the error message "I/O error has occurred!? How is this possible?".
procedure Main is
   use type XCB.GC_Type;
   use type Interfaces.C.unsigned;
   use type Interfaces.Unsigned_8;
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
   Values (0) := S.White_Pixel;
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

   declare
      Name : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_Char_Array ("WM_PROTOCOLS");
      Name_2 : Interfaces.C.Strings.chars_ptr := Interfaces.C.Strings.New_Char_Array ("WM_DELETE_WINDOW");

      Cookie : XCB.Intern_Atom_Cookie_Type;
      Cookie_2 : XCB.Intern_Atom_Cookie_Type;

      Reply : XCB.Intern_Atom_Reply_Access_Type;
      Reply_2 : XCB.Intern_Atom_Reply_Access_Type;

   begin
      Cookie := XCB.Intern_Atom (C, 1, 12, Name);
      Reply := XCB.Intern_Atom_Reply (C      => C,
                                      Cookie => Cookie,
                                      Error  => System.Null_Address);

      Cookie_2 := XCB.Intern_Atom (C, 0, 16, Name_2);
      Reply_2 := XCB.Intern_Atom_Reply (C, Cookie_2, System.Null_Address);
      Unused_Cookie := XCB.Change_Property (C,
                                            XCB.Prop_Mode_Type'Pos (XCB.XCB_PROP_MODE_REPLACE),
                                            W,
                                            Reply.all.Atom,
                                            4,
                                            32,
                                            1,
                                            Reply_2.all.Atom'Address);
   end;
--    xcb_change_property(c, XCB_PROP_MODE_REPLACE, w, (*reply).atom, 4, 32, 1,
--  &(*reply2).atom);
--
--    xcb_map_window(c, w);
--    xcb_flush(c);
--
--    xcb_generic_event_t* event;
--    while((event = xcb_wait_for_event(c)))
--    {
--      puts("Event occurred");
--      switch((*event).response_type & ~0x80)
--      {
--        case XCB_EXPOSE:
--          puts("Expose");
--          break;
--        case XCB_CLIENT_MESSAGE:
--        {
--          puts("Client Message");
--          if((*(xcb_client_message_event_t*)event).data.data32[0] ==
--  (*reply2).atom)
--          {
--            puts("Kill client");
--            return 0;
--          }
--          break;
--        }
--      }
--    }


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
         case (E.Response_Kind mod 128) is
            when XCB.XCB_EXPOSE =>
               -- Draw or redraw the window
               --              xcb_poly_fill_rectangle(c, w, g,  1, &r);
               Flush_Number := XCB.Flush (C);
            when XCB.XCB_KEY_PRESS =>
               -- Exit on key press
               Done := 1;
            when XCB.XCB_CLIENT_MESSAGE =>
               Done := 1;
               Ada.Text_IO.Put_Line ("Clicked on the X-button");
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
