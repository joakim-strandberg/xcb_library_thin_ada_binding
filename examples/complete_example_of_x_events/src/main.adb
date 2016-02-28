with Ada.Text_IO;
with Interfaces.C.Strings;
with XCB;

procedure Main is
   use type XCB.GC_Type;
   use type Interfaces.C.unsigned;
   use type Interfaces.C.int;
   use type Interfaces.Unsigned_8;
   use type XCB.Generic_Event_Access_Type;


--      /* print names of modifiers present in mask */
--      void
--      print_modifiers (uint32_t mask)
--      {
--          const char *MODIFIERS[] = {
--                  "Shift", "Lock", "Ctrl", "Alt",
--                  "Mod2", "Mod3", "Mod4", "Mod5",
--                  "Button1", "Button2", "Button3", "Button4", "Button5"
--          };
--
--          printf ("Modifier mask: ");
--          for (const char **modifier = MODIFIERS ; mask; mask >>= 1, ++modifier) {
--              if (mask & 1) {
--                  printf (*modifier);
--              }
--          }
--          printf ("\n");
--      }

     Connection : XCB.Connection_Access_Type;
     Screen : XCB.Screen_Access_Type;
     Window : XCB.Window_Id_Type;
--
     Mask : XCB.GC_Type;
   Values : aliased XCB.Value_List_Array (0..1);

   Event : XCB.Generic_Event_Access_Type;
--
--     R : XCB.Rectangle_Type := (X => 20, Y => 20, Width => 60, Height => 60);
--
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

   Mask := XCB.Constants.XCB_CW_BACK_PIXEL or XCB.Constants.XCB_CW_EVENT_MASK;
   Values := (1 => Screen.White_Pixel,
              2 => XCB.Constants.XCB_EVENT_MASK_EXPOSURE or XCB.Constants.XCB_EVENT_MASK_BUTTON_PRESS      or
                XCB.Constants.XCB_EVENT_MASK_BUTTON_RELEASE or XCB.Constants.XCB_EVENT_MASK_POINTER_MOTION or
                  XCB.Constants.XCB_EVENT_MASK_ENTER_WINDOW or XCB.Constants.XCB_EVENT_MASK_LEAVE_WINDOW   or
                    XCB.Constants.XCB_EVENT_MASK_KEY_PRESS  or XCB.Constants.XCB_EVENT_MASK_KEY_RELEASE);

   Unused_Cookie := XCB.Create_Window (C            => Connection,
                                       Depth        => 0,
                                       Window_Id    => Window,
                                       Parent       => Screen.Root,
                                       X            => 0,
                                       Y            => 0,
                                       Width        => 150,
                                       Height       => 150,
                                       Border_Width => 10,
                                       U_Class      => XCB.XCB_WINDOW_CLASS_INPUT_OUTPUT,
                                       Visual_Id    => Screen.Root_Visual_Id,
                                       Value_Mask   => Mask,
                                       Value_List   => Values);

   -- Map the window on the screen
   Unused_Cookie := XCB.Map_Window (Connection, Window);

   Flush_Number := XCB.Flush (Connection);

   loop
      Event := XCB.Wait_For_Event (Connection);
      case (Event.Response_Kind mod 128) is
         when XCB.Constants.XCB_EXPOSE =>
            declare
               Expose : XCB.Expose_Event_Access_Type;
            begin
               Expose := XCB.To_Expose_Event (Event);
               Ada.Text_IO.Put ("Window" & Expose.Window_Id'Img & " exposed. Region to be redrawn at location (");
               Ada.Text_IO.Put_Line (Expose.X'Img & "," & Expose.Y'Img & "), with dimension (" & Expose.Width'Img & "," & Expose.Height'Img & ")");
            end;
         when XCB.Constants.XCB_BUTTON_PRESS =>
            declare
               BP : XCB.Button_Press_Event_Access_Type;
            begin
               BP := XCB.To_Button_Press_Event (Event);
               --               print_modifiers (bp->state);

               case BP.Detail is
                  when 4 =>
                     Ada.Text_IO.Put_Line ("Wheel Button up in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
                  when 5 =>
                     Ada.Text_IO.Put_Line ("Wheel Button down in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
                  when others =>
                     Ada.Text_IO.Put_Line ("Button " & BP.Detail'Img & " pressed in window" & BP.Event'Img & ", at coordinates (" & BP.Event_X'Img & "," & BP.Event_Y'Img & ")");
               end case;
            end;
         when XCB.Constants.XCB_BUTTON_RELEASE =>
            declare
               BR : XCB.Button_Release_Event_Access_Type;
            begin
               BR := XCB.To_Button_Release_Event (Event);
               --                print_modifiers(br->state);
               Ada.Text_IO.Put_Line ("Button " & BR.Detail'Img & " pressed in window" & BR.Event'Img & ", at coordinates (" & BR.Event_X'Img & "," & BR.Event_Y'Img & ")");
            end;
         when XCB.Constants.XCB_MOTION_NOTIFY =>
            declare
               Motion : XCB.Motion_Notify_Event_Access_Type;
            begin
               Motion := XCB.To_Motion_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse moved in window" & Motion.Event'Img & ", at coordinates (" & Motion.Event_X'Img & "," & Motion.Event_Y'Img & ")");
            end;
         when XCB.Constants.XCB_ENTER_NOTIFY =>
            declare
               Enter : XCB.Enter_Notify_Event_Access_Type;
            begin
               Enter := XCB.To_Enter_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse entered window" & Enter.Event'Img & ", at coordinates (" & Enter.Event_X'Img & "," & Enter.Event_Y'Img & ")");
            end;
         when XCB.Constants.XCB_LEAVE_NOTIFY =>
            declare
               Leave : XCB.Leave_Notify_Event_Access_Type;
            begin
               Leave := XCB.To_Leave_Notify_Event (Event);
               Ada.Text_IO.Put_Line ("Mouse left window" & Leave.Event'Img & ", at coordinates (" & Leave.Event_X'Img & "," & Leave.Event_Y'Img & ")");
            end;
         when XCB.Constants.XCB_KEY_PRESS =>
            declare
               KP : XCB.Key_Press_Event_Access_Type;
            begin
               KP := XCB.To_Key_Press_Event (Event);
--                  print_modifiers(kp->state);
               Ada.Text_IO.Put_Line ("Key pressed in window" & KP.Event'Img);
            end;
         when XCB.Constants.XCB_KEY_RELEASE =>
            declare
               KR : XCB.Key_Release_Event_Access_Type;
            begin
               KR := XCB.To_Key_Release_Event (Event);
--                  print_modifiers(kr->state);
               Ada.Text_IO.Put_Line ("Key released in window" & KR.Event'Img);
            end;
         when others =>
            -- Unknown event type, ignore it
            Ada.Text_IO.Put_Line ("Unknown event:" & Event.Response_Kind'Img);
      end case;
      XCB.Free (Event);
   end loop;
end Main;
