with Ada.Text_IO;
with Interfaces.C.Strings;
with XCB;

procedure Main is
   use type XCB.Gcontext_Id_Type;
   use type XCB.CW_Type;
   use type XCB.Event_Mask_Type;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;
   use type Interfaces.Unsigned_8;
   use type Interfaces.Unsigned_16;
   use type XCB.Generic_Event_Access_Type;

   procedure Print_Modifiers (Mask : Interfaces.Unsigned_16) is
   begin
      if (Mask and 2#0000_0000_0000_0001#) = 2#0000_0000_0000_0001# then
         Ada.Text_IO.Put_Line ("Shift");
      end if;

      if (Mask and 2#0000_0000_0000_0010#) = 2#0000_0000_0000_0010# then
         Ada.Text_IO.Put_Line ("Lock");
      end if;

      if (Mask and 2#0000_0000_0000_0100#) = 2#0000_0000_0000_0100# then
         Ada.Text_IO.Put_Line ("Ctrl");
      end if;

      if (Mask and 2#0000_0000_0000_1000#) = 2#0000_0000_0000_1000# then
         Ada.Text_IO.Put_Line ("Alt");
      end if;

      if (Mask and 2#0000_0000_0001_0000#) = 2#0000_0000_0001_0000# then
         Ada.Text_IO.Put_Line ("Mod2");
      end if;

      if (Mask and 2#0000_0000_0010_0000#) = 2#0000_0000_0010_0000# then
         Ada.Text_IO.Put_Line ("Mod3");
      end if;

      if (Mask and 2#0000_0000_0100_0000#) = 2#0000_0000_0100_0000# then
         Ada.Text_IO.Put_Line ("Mod4");
      end if;

      if (Mask and 2#0000_0000_1000_0000#) = 2#0000_0000_1000_0000# then
         Ada.Text_IO.Put_Line ("Mod5");
      end if;

      if (Mask and 2#0000_0001_0000_0000#) = 2#0000_0001_0000_0000# then
         Ada.Text_IO.Put_Line ("Button1");
      end if;

      if (Mask and 2#0000_0010_0000_0000#) = 2#0000_0010_0000_0000# then
         Ada.Text_IO.Put_Line ("Button2");
      end if;

      if (Mask and 2#0000_0100_0000_0000#) = 2#0000_0100_0000_0000# then
         Ada.Text_IO.Put_Line ("Button3");
      end if;

      if (Mask and 2#0000_1000_0000_0000#) = 2#0000_1000_0000_0000# then
         Ada.Text_IO.Put_Line ("Button4");
      end if;

      if (Mask and 2#0001_0000_0000_0000#) = 2#0001_0000_0000_0000# then
         Ada.Text_IO.Put_Line ("Button5");
      end if;
   end Print_Modifiers;

   Connection : XCB.Connection_Access_Type;
   Screen : XCB.Screen_Access_Type;
   Window : XCB.Window_Id_Type;

   Mask : XCB.CW_Type;
   Values : aliased XCB.Value_List_Array (0..1);

   Event : XCB.Generic_Event_Access_Type;

   Unused_Cookie : XCB.Void_Cookie_Type;
   pragma Unreferenced (Unused_Cookie);

   Flush_Number : Interfaces.C.int;
begin

   -- Open the connection to the X server
   Connection := XCB.Connect (Interfaces.C.Strings.Null_Ptr, NULL);

   -- Get the first screen
   Screen := XCB.Setup_Roots_Iterator ( XCB.Get_Setup (connection)).data;

   -- Create the window
   Window := XCB.Generate_Id (Connection);

   Mask := XCB.XCB_CW_BACK_PIXEL or XCB.XCB_CW_EVENT_MASK;
   Values := (1 => Screen.White_Pixel,
              2 => Interfaces.Unsigned_32 (XCB.XCB_EVENT_MASK_EXPOSURE or XCB.XCB_EVENT_MASK_BUTTON_PRESS      or
                  XCB.XCB_EVENT_MASK_BUTTON_RELEASE or XCB.XCB_EVENT_MASK_POINTER_MOTION or
                    XCB.XCB_EVENT_MASK_ENTER_WINDOW or XCB.XCB_EVENT_MASK_LEAVE_WINDOW   or
                      XCB.XCB_EVENT_MASK_KEY_PRESS  or XCB.XCB_EVENT_MASK_KEY_RELEASE));

   Unused_Cookie := XCB.Create_Window (C            => Connection,
                                       Depth        => 0,
                                       Wid          => Window,
                                       Parent       => Screen.Root,
                                       X            => 0,
                                       Y            => 0,
                                       Width        => 150,
                                       Height       => 150,
                                       Border_Width => 10,
                                       Class        => Interfaces.Unsigned_16 (XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT),
                                       Visual       => Screen.Root_Visual,
                                       Value_Mask   => Interfaces.Unsigned_32 (Mask),
                                       Value_List   => Values);

   -- Map the window on the screen
   Unused_Cookie := XCB.Map_Window (Connection, Window);

   Flush_Number := XCB.Flush (Connection);

   loop
      Event := XCB.Wait_For_Event (Connection);
      case (Event.Response_Kind mod 128) is
         when XCB.XCB_EXPOSE =>
            declare
               Expose : XCB.Expose_Event_Access_Type;
            begin
               Expose := XCB.To_Expose_Event (Event);
               Ada.Text_IO.Put ("Window" & Expose.Window'Img & " exposed. Region to be redrawn at location (");
               Ada.Text_IO.Put_Line (Expose.X'Img & "," & Expose.Y'Img & "), with dimension (" & Expose.Width'Img & "," & Expose.Height'Img & ")");
            end;
         when XCB.XCB_BUTTON_PRESS =>
            declare
               BP : XCB.Button_Press_Event_Access_Type;
            begin
               BP := XCB.To_Button_Press_Event (Event);
               Print_Modifiers (BP.State);

               case BP.Detail is
                  when 4 =>
                     Ada.Text_IO.Put_Line ("Wheel Button up in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
                  when 5 =>
                     Ada.Text_IO.Put_Line ("Wheel Button down in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
                  when others =>
                     Ada.Text_IO.Put_Line ("Button " & BP.Detail'Img & " pressed in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
               end case;
            end;
         when XCB.XCB_BUTTON_RELEASE =>
            declare
               BR : XCB.Button_Release_Event_Access_Type;
            begin
               BR := XCB.To_Button_Release_Event (Event);
               Print_Modifiers (BR.State);
               Ada.Text_IO.Put_Line ("Button " & BR.Detail'Img & " pressed in window" & BR.Event'Img & ", at coordinates (" & BR.Event_X'Img & "," & BR.Event_Y'Img & ")");
            end;
         when XCB.XCB_MOTION_NOTIFY =>
            declare
               Motion : XCB.Motion_Notify_Event_Access_Type;
            begin
               Motion := XCB.To_Motion_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse moved in window" & Motion.Event'Img & ", at coordinates (" & Motion.Event_X'Img & "," & Motion.Event_Y'Img & ")");
            end;
         when XCB.XCB_ENTER_NOTIFY =>
            declare
               Enter : XCB.Enter_Notify_Event_Access_Type;
            begin
               Enter := XCB.To_Enter_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse entered window" & Enter.Event'Img & ", at coordinates (" & Enter.Event_X'Img & "," & Enter.Event_Y'Img & ")");
            end;
         when XCB.XCB_LEAVE_NOTIFY =>
            declare
               Leave : XCB.Leave_Notify_Event_Access_Type;
            begin
               Leave := XCB.To_Leave_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse left window" & Leave.Event'Img & ", at coordinates (" & Leave.Event_X'Img & "," & Leave.Event_Y'Img & ")");
            end;
         when XCB.XCB_KEY_PRESS =>
            declare
               KP : XCB.Key_Press_Event_Access_Type;
            begin
               KP := XCB.To_Key_Press_Event (Event);
               Print_Modifiers(KP.State);
               Ada.Text_IO.Put_Line ("Key pressed in window" & KP.Event'Img);
            end;
         when XCB.XCB_KEY_RELEASE =>
            declare
               KR : XCB.Key_Release_Event_Access_Type;
            begin
               KR := XCB.To_Key_Release_Event (Event);
               Print_Modifiers(KR.State);
               Ada.Text_IO.Put_Line ("Key released in window" & KR.Event'Img);
            end;
         when others =>
            -- Unknown event type, ignore it
            Ada.Text_IO.Put_Line ("Unknown event:" & Event.Response_Kind'Img);
      end case;
      XCB.Free (Event);
   end loop;
end Main;
