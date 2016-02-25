-- The original file was xcb.h. Translated to Ada by Joakim Strandberg 2016.
-- The original copyright notice:
--
-- Copyright (C) 2001-2006 Bart Massey, Jamey Sharp, and Josh Triplett.
-- All Rights Reserved.
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the "Software"),
-- to deal in the Software without restriction, including without limitation
-- the rights to use, copy, modify, merge, publish, distribute, sublicense,
-- and/or sell copies of the Software, and to permit persons to whom the
-- Software is furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
-- ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
-- CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
--
-- Except as contained in this notice, the names of the authors or their
-- institutions shall not be used in advertising or otherwise to promote the
-- sale, use or other dealings in this Software without prior written
-- authorization from the authors.
pragma Style_Checks (Off);

with Interfaces.C.Strings;
with System;
with Ada.Unchecked_Deallocation;

package XCB is

   pragma Linker_Options ("-lxcb");

   --  unsupported macro: XCB_PACKED __attribute__((__packed__))
   X_PROTOCOL                       : constant := 11;
   X_PROTOCOL_REVISION              : constant := 0;
   X_TCP_PORT                       : constant := 6000;
   XCB_CONN_ERROR                   : constant := 1;
   XCB_CONN_CLOSED_EXT_NOTSUPPORTED : constant := 2;
   XCB_CONN_CLOSED_MEM_INSUFFICIENT : constant := 3;
   XCB_CONN_CLOSED_REQ_LEN_EXCEED   : constant := 4;
   XCB_CONN_CLOSED_PARSE_ERR        : constant := 5;
   XCB_CONN_CLOSED_INVALID_SCREEN   : constant := 6;
   XCB_CONN_CLOSED_FDPASSING_FAILED : constant := 7;
   XCB_NONE                         : constant := 0;
   XCB_COPY_FROM_PARENT             : constant := 0;
   XCB_CURRENT_TIME                 : constant := 0;
   XCB_NO_SYMBOL                    : constant := 0;


   XCB_KEY_PRESS         : constant := 2;
   XCB_KEY_RELEASE       : constant := 3;
   XCB_BUTTON_PRESS      : constant := 4;
   XCB_BUTTON_RELEASE    : constant := 5;
   XCB_MOTION_NOTIFY     : constant := 6;
   XCB_ENTER_NOTIFY      : constant := 7;
   XCB_LEAVE_NOTIFY      : constant := 8;
   XCB_FOCUS_IN          : constant := 9;
   XCB_FOCUS_OUT         : constant := 10;
   XCB_KEYMAP_NOTIFY     : constant := 11;
   XCB_EXPOSE            : constant := 12;
   XCB_GRAPHICS_EXPOSURE : constant := 13;
   XCB_NO_EXPOSURE       : constant := 14;
   XCB_VISIBILITY_NOTIFY : constant := 15;
   XCB_CREATE_NOTIFY     : constant := 16;
   XCB_DESTROY_NOTIFY    : constant := 17;
   XCB_UNMAP_NOTIFY      : constant := 18;
   XCB_MAP_NOTIFY        : constant := 19;
   XCB_MAP_REQUEST       : constant := 20;
   XCB_REPARENT_NOTIFY   : constant := 21;

   --  unsupported macro: XCB_CONFIGURE_NOTIFY 22
   --  unsupported macro: XCB_CONFIGURE_REQUEST 23
   --  unsupported macro: XCB_GRAVITY_NOTIFY 24
   --  unsupported macro: XCB_RESIZE_REQUEST 25
   --  unsupported macro: XCB_CIRCULATE_NOTIFY 26
   --  unsupported macro: XCB_CIRCULATE_REQUEST 27
   --  unsupported macro: XCB_PROPERTY_NOTIFY 28
   --  unsupported macro: XCB_SELECTION_CLEAR 29
   --  unsupported macro: XCB_SELECTION_REQUEST 30
   --  unsupported macro: XCB_SELECTION_NOTIFY 31
   --  unsupported macro: XCB_COLORMAP_NOTIFY 32
   --  unsupported macro: XCB_CLIENT_MESSAGE 33
   --  unsupported macro: XCB_MAPPING_NOTIFY 34
   --  unsupported macro: XCB_GE_GENERIC 35
   --  unsupported macro: XCB_REQUEST 1
   --  unsupported macro: XCB_VALUE 2
   --  unsupported macro: XCB_WINDOW 3
   --  unsupported macro: XCB_PIXMAP 4
   --  unsupported macro: XCB_ATOM 5
   --  unsupported macro: XCB_CURSOR 6
   --  unsupported macro: XCB_FONT 7
   --  unsupported macro: XCB_MATCH 8
   --  unsupported macro: XCB_DRAWABLE 9
   --  unsupported macro: XCB_ACCESS 10
   --  unsupported macro: XCB_ALLOC 11
   --  unsupported macro: XCB_COLORMAP 12
   --  unsupported macro: XCB_G_CONTEXT 13
   --  unsupported macro: XCB_ID_CHOICE 14
   --  unsupported macro: XCB_NAME 15
   --  unsupported macro: XCB_LENGTH 16
   --  unsupported macro: XCB_IMPLEMENTATION 17
   --  unsupported macro: XCB_CREATE_WINDOW 1
   --  unsupported macro: XCB_CHANGE_WINDOW_ATTRIBUTES 2
   --  unsupported macro: XCB_GET_WINDOW_ATTRIBUTES 3
   --  unsupported macro: XCB_DESTROY_WINDOW 4
   --  unsupported macro: XCB_DESTROY_SUBWINDOWS 5
   --  unsupported macro: XCB_CHANGE_SAVE_SET 6
   --  unsupported macro: XCB_REPARENT_WINDOW 7
   --  unsupported macro: XCB_MAP_WINDOW 8
   --  unsupported macro: XCB_MAP_SUBWINDOWS 9
   --  unsupported macro: XCB_UNMAP_WINDOW 10
   --  unsupported macro: XCB_UNMAP_SUBWINDOWS 11
   --  unsupported macro: XCB_CONFIGURE_WINDOW 12
   --  unsupported macro: XCB_CIRCULATE_WINDOW 13
   --  unsupported macro: XCB_GET_GEOMETRY 14
   --  unsupported macro: XCB_QUERY_TREE 15
   --  unsupported macro: XCB_INTERN_ATOM 16
   --  unsupported macro: XCB_GET_ATOM_NAME 17
   --  unsupported macro: XCB_CHANGE_PROPERTY 18
   --  unsupported macro: XCB_DELETE_PROPERTY 19
   --  unsupported macro: XCB_GET_PROPERTY 20
   --  unsupported macro: XCB_LIST_PROPERTIES 21
   --  unsupported macro: XCB_SET_SELECTION_OWNER 22
   --  unsupported macro: XCB_GET_SELECTION_OWNER 23
   --  unsupported macro: XCB_CONVERT_SELECTION 24
   --  unsupported macro: XCB_SEND_EVENT 25
   --  unsupported macro: XCB_GRAB_POINTER 26
   --  unsupported macro: XCB_UNGRAB_POINTER 27
   --  unsupported macro: XCB_GRAB_BUTTON 28
   --  unsupported macro: XCB_UNGRAB_BUTTON 29
   --  unsupported macro: XCB_CHANGE_ACTIVE_POINTER_GRAB 30
   --  unsupported macro: XCB_GRAB_KEYBOARD 31
   --  unsupported macro: XCB_UNGRAB_KEYBOARD 32
   --  unsupported macro: XCB_GRAB_KEY 33
   --  unsupported macro: XCB_UNGRAB_KEY 34
   --  unsupported macro: XCB_ALLOW_EVENTS 35
   --  unsupported macro: XCB_GRAB_SERVER 36
   --  unsupported macro: XCB_UNGRAB_SERVER 37
   --  unsupported macro: XCB_QUERY_POINTER 38
   --  unsupported macro: XCB_GET_MOTION_EVENTS 39
   --  unsupported macro: XCB_TRANSLATE_COORDINATES 40
   --  unsupported macro: XCB_WARP_POINTER 41
   --  unsupported macro: XCB_SET_INPUT_FOCUS 42
   --  unsupported macro: XCB_GET_INPUT_FOCUS 43
   --  unsupported macro: XCB_QUERY_KEYMAP 44
   --  unsupported macro: XCB_OPEN_FONT 45
   --  unsupported macro: XCB_CLOSE_FONT 46
   --  unsupported macro: XCB_QUERY_FONT 47
   --  unsupported macro: XCB_QUERY_TEXT_EXTENTS 48
   --  unsupported macro: XCB_LIST_FONTS 49
   --  unsupported macro: XCB_LIST_FONTS_WITH_INFO 50
   --  unsupported macro: XCB_SET_FONT_PATH 51
   --  unsupported macro: XCB_GET_FONT_PATH 52
   --  unsupported macro: XCB_CREATE_PIXMAP 53
   --  unsupported macro: XCB_FREE_PIXMAP 54
   --  unsupported macro: XCB_CREATE_GC 55
   --  unsupported macro: XCB_CHANGE_GC 56
   --  unsupported macro: XCB_COPY_GC 57
   --  unsupported macro: XCB_SET_DASHES 58
   --  unsupported macro: XCB_SET_CLIP_RECTANGLES 59
   --  unsupported macro: XCB_FREE_GC 60
   --  unsupported macro: XCB_CLEAR_AREA 61
   --  unsupported macro: XCB_COPY_AREA 62
   --  unsupported macro: XCB_COPY_PLANE 63
   --  unsupported macro: XCB_POLY_POINT 64
   --  unsupported macro: XCB_POLY_LINE 65
   --  unsupported macro: XCB_POLY_SEGMENT 66
   --  unsupported macro: XCB_POLY_RECTANGLE 67
   --  unsupported macro: XCB_POLY_ARC 68
   --  unsupported macro: XCB_FILL_POLY 69
   --  unsupported macro: XCB_POLY_FILL_RECTANGLE 70
   --  unsupported macro: XCB_POLY_FILL_ARC 71
   --  unsupported macro: XCB_PUT_IMAGE 72
   --  unsupported macro: XCB_GET_IMAGE 73
   --  unsupported macro: XCB_POLY_TEXT_8 74
   --  unsupported macro: XCB_POLY_TEXT_16 75
   --  unsupported macro: XCB_IMAGE_TEXT_8 76
   --  unsupported macro: XCB_IMAGE_TEXT_16 77
   --  unsupported macro: XCB_CREATE_COLORMAP 78
   --  unsupported macro: XCB_FREE_COLORMAP 79
   --  unsupported macro: XCB_COPY_COLORMAP_AND_FREE 80
   --  unsupported macro: XCB_INSTALL_COLORMAP 81
   --  unsupported macro: XCB_UNINSTALL_COLORMAP 82
   --  unsupported macro: XCB_LIST_INSTALLED_COLORMAPS 83
   --  unsupported macro: XCB_ALLOC_COLOR 84
   --  unsupported macro: XCB_ALLOC_NAMED_COLOR 85
   --  unsupported macro: XCB_ALLOC_COLOR_CELLS 86
   --  unsupported macro: XCB_ALLOC_COLOR_PLANES 87
   --  unsupported macro: XCB_FREE_COLORS 88
   --  unsupported macro: XCB_STORE_COLORS 89
   --  unsupported macro: XCB_STORE_NAMED_COLOR 90
   --  unsupported macro: XCB_QUERY_COLORS 91
   --  unsupported macro: XCB_LOOKUP_COLOR 92
   --  unsupported macro: XCB_CREATE_CURSOR 93
   --  unsupported macro: XCB_CREATE_GLYPH_CURSOR 94
   --  unsupported macro: XCB_FREE_CURSOR 95
   --  unsupported macro: XCB_RECOLOR_CURSOR 96
   --  unsupported macro: XCB_QUERY_BEST_SIZE 97
   --  unsupported macro: XCB_QUERY_EXTENSION 98
   --  unsupported macro: XCB_LIST_EXTENSIONS 99
   --  unsupported macro: XCB_CHANGE_KEYBOARD_MAPPING 100
   --  unsupported macro: XCB_GET_KEYBOARD_MAPPING 101
   --  unsupported macro: XCB_CHANGE_KEYBOARD_CONTROL 102
   --  unsupported macro: XCB_GET_KEYBOARD_CONTROL 103
   --  unsupported macro: XCB_BELL 104
   --  unsupported macro: XCB_CHANGE_POINTER_CONTROL 105
   --  unsupported macro: XCB_GET_POINTER_CONTROL 106
   --  unsupported macro: XCB_SET_SCREEN_SAVER 107
   --  unsupported macro: XCB_GET_SCREEN_SAVER 108
   --  unsupported macro: XCB_CHANGE_HOSTS 109
   --  unsupported macro: XCB_LIST_HOSTS 110
   --  unsupported macro: XCB_SET_ACCESS_CONTROL 111
   --  unsupported macro: XCB_SET_CLOSE_DOWN_MODE 112
   --  unsupported macro: XCB_KILL_CLIENT 113
   --  unsupported macro: XCB_ROTATE_PROPERTIES 114
   --  unsupported macro: XCB_FORCE_SCREEN_SAVER 115
   --  unsupported macro: XCB_SET_POINTER_MAPPING 116
   --  unsupported macro: XCB_GET_POINTER_MAPPING 117
   --  unsupported macro: XCB_SET_MODIFIER_MAPPING 118
   --  unsupported macro: XCB_GET_MODIFIER_MAPPING 119
   --  unsupported macro: XCB_NO_OPERATION 127

   --
   -- Type definitions
   --

   type Void_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Void_Cookie_Type);

   -- Opaque structure containing all data that XCB needs in order
   -- to communicate with an X server.
   type Connection_Type is limited null record;

   type Connection_Access_Type is access all Connection_Type;
   for Connection_Access_Type'Storage_Size use 0;
   pragma Convention (C, Connection_Access_Type);

   -- Identifier for objects in the XCB library. For example Windows,
   -- Graphical Contexts,...
   type X_Id_Type is new Interfaces.Unsigned_32;

   subtype Keycode_Type is Interfaces.Unsigned_8;

   type Setup_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Setup_Type is record
      Status                      : aliased Interfaces.Unsigned_8;
      Padding_0                   : aliased Interfaces.Unsigned_8;
      Protocol_Major_Version      : aliased Interfaces.Unsigned_16;
      Protocol_Minor_Version      : aliased Interfaces.Unsigned_16;
      Length                      : aliased Interfaces.Unsigned_16;
      Release_Number              : aliased Interfaces.Unsigned_32;
      Resource_Id_Base            : aliased Interfaces.Unsigned_32;
      Resource_Id_Mask            : aliased Interfaces.Unsigned_32;
      Motion_Buffer_Size          : aliased Interfaces.Unsigned_32;
      Vendor_Length               : aliased Interfaces.Unsigned_16;
      Maximum_Request_Length      : aliased Interfaces.Unsigned_16;
      Roots_Length                : aliased Interfaces.Unsigned_8;
      Pixmap_Formats_Length       : aliased Interfaces.Unsigned_8;
      Image_Byte_Order            : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Bit_Order     : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Scanline_Unit : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Scanline_Pad  : aliased Interfaces.Unsigned_8;
      Minimum_Keycode             : aliased Keycode_Type;
      Maximum_Keycode             : aliased Keycode_Type;
      Paddding_1                  : aliased Setup_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Type);

   type Setup_Constant_Access_Type is access constant Setup_Type;

   type Graphical_Context_Type is new X_Id_Type;

   subtype Event_Mask_Type is Interfaces.Unsigned_32;
   XCB_EVENT_MASK_NO_EVENT              : constant Event_Mask_Type := 0;
   XCB_EVENT_MASK_KEY_PRESS             : constant Event_Mask_Type := 1;
   XCB_EVENT_MASK_KEY_RELEASE           : constant Event_Mask_Type := 2;
   XCB_EVENT_MASK_BUTTON_PRESS          : constant Event_Mask_Type := 4;
   XCB_EVENT_MASK_BUTTON_RELEASE        : constant Event_Mask_Type := 8;
   XCB_EVENT_MASK_ENTER_WINDOW          : constant Event_Mask_Type := 16;
   XCB_EVENT_MASK_LEAVE_WINDOW          : constant Event_Mask_Type := 32;
   XCB_EVENT_MASK_POINTER_MOTION        : constant Event_Mask_Type := 64;
   XCB_EVENT_MASK_POINTER_MOTION_HINT   : constant Event_Mask_Type := 128;
   XCB_EVENT_MASK_BUTTON_1_MOTION       : constant Event_Mask_Type := 256;
   XCB_EVENT_MASK_BUTTON_2_MOTION       : constant Event_Mask_Type := 512;
   XCB_EVENT_MASK_BUTTON_3_MOTION       : constant Event_Mask_Type := 1024;
   XCB_EVENT_MASK_BUTTON_4_MOTION       : constant Event_Mask_Type := 2048;
   XCB_EVENT_MASK_BUTTON_5_MOTION       : constant Event_Mask_Type := 4096;
   XCB_EVENT_MASK_BUTTON_MOTION         : constant Event_Mask_Type := 8192;
   XCB_EVENT_MASK_KEYMAP_STATE          : constant Event_Mask_Type := 16384;

   -- In an X program, everything is driven by events. Event painting
   -- on the screen is sometimes done as a response to an event (an Expose event).
   -- If part of a program's window that was hidden, gets exposed (e.g. the window
   -- was raised above other widows), the X server will send an "expose" event
   -- to let the program know it should repaint that part of the window.
   -- User input (key presses, mouse movement, etc) is also received as a set of events.
   XCB_EVENT_MASK_EXPOSURE              : constant Event_Mask_Type := 32768;


   XCB_EVENT_MASK_VISIBILITY_CHANGE     : constant Event_Mask_Type := 65536;
   XCB_EVENT_MASK_STRUCTURE_NOTIFY      : constant Event_Mask_Type := 131072;
   XCB_EVENT_MASK_RESIZE_REDIRECT       : constant Event_Mask_Type := 262144;
   XCB_EVENT_MASK_SUBSTRUCTURE_NOTIFY   : constant Event_Mask_Type := 524288;
   XCB_EVENT_MASK_SUBSTRUCTURE_REDIRECT : constant Event_Mask_Type := 1048576;
   XCB_EVENT_MASK_FOCUS_CHANGE          : constant Event_Mask_Type := 2097152;
   XCB_EVENT_MASK_PROPERTY_CHANGE       : constant Event_Mask_Type := 4194304;
   XCB_EVENT_MASK_COLOR_MAP_CHANGE      : constant Event_Mask_Type := 8388608;
   XCB_EVENT_MASK_OWNER_GRAB_BUTTON     : constant Event_Mask_Type := 16777216;

   subtype GC_Type is Interfaces.Unsigned_32;

   XCB_GC_FUNCTION              : constant GC_Type := 1;
   XCB_GC_PLANE_MASK            : constant GC_Type := 2;
   XCB_GC_FOREGROUND            : constant GC_Type := 4;
   XCB_GC_BACKGROUND            : constant GC_Type := 8;
   XCB_GC_LINE_WIDTH            : constant GC_Type := 16;
   XCB_GC_LINE_STYLE            : constant GC_Type := 32;
   XCB_GC_CAP_STYLE             : constant GC_Type := 64;
   XCB_GC_JOIN_STYLE            : constant GC_Type := 128;
   XCB_GC_FILL_STYLE            : constant GC_Type := 256;
   XCB_GC_FILL_RULE             : constant GC_Type := 512;
   XCB_GC_TILE                  : constant GC_Type := 1024;
   XCB_GC_STIPPLE               : constant GC_Type := 2048;
   XCB_GC_TILE_STIPPLE_ORIGIN_X : constant GC_Type := 4096;
   XCB_GC_TILE_STIPPLE_ORIGIN_Y : constant GC_Type := 8192;
   XCB_GC_FONT                  : constant GC_Type := 16384;
   XCB_GC_SUBWINDOW_MODE        : constant GC_Type := 32768;
   XCB_GC_GRAPHICS_EXPOSURES    : constant GC_Type := 65536;
   XCB_GC_CLIP_ORIGIN_X         : constant GC_Type := 131072;
   XCB_GC_CLIP_ORIGIN_Y         : constant GC_Type := 262144;
   XCB_GC_CLIP_MASK             : constant GC_Type := 524288;
   XCB_GC_DASH_OFFSET           : constant GC_Type := 1048576;
   XCB_GC_DASH_LIST             : constant GC_Type := 2097152;
   XCB_GC_ARC_MODE              : constant GC_Type := 4194304;

   subtype CW_Type is Interfaces.Unsigned_32;
   XCB_CW_BACK_PIXMAP       : constant CW_Type := 1;
   XCB_CW_BACK_PIXEL        : constant CW_Type := 2;
   XCB_CW_BORDER_PIXMAP     : constant CW_Type := 4;
   XCB_CW_BORDER_PIXEL      : constant CW_Type := 8;
   XCB_CW_BIT_GRAVITY       : constant CW_Type := 16;
   XCB_CW_WIN_GRAVITY       : constant CW_Type := 32;
   XCB_CW_BACKING_STORE     : constant CW_Type := 64;
   XCB_CW_BACKING_PLANES    : constant CW_Type := 128;
   XCB_CW_BACKING_PIXEL     : constant CW_Type := 256;
   XCB_CW_OVERRIDE_REDIRECT : constant CW_Type := 512;
   XCB_CW_SAVE_UNDER        : constant CW_Type := 1024;
   XCB_CW_EVENT_MASK        : constant CW_Type := 2048;
   XCB_CW_DONT_PROPAGATE    : constant CW_Type := 4096;
   XCB_CW_COLORMAP          : constant CW_Type := 8192;
   XCB_CW_CURSOR            : constant CW_Type := 16384;

   type Value_List_Array is array (Natural range <>) of Interfaces.Unsigned_32;
   pragma Convention (C, Value_List_Array);
--   for Value_List_Array'Component_Size use Interfaces.Unsigned_32'Size;

   type Generic_Event_Pad_Array_Type is array (0 .. 6) of aliased Interfaces.Unsigned_32;

   type Generic_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Pad_0         : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Pad           : aliased Generic_Event_Pad_Array_Type;
      Full_Sequence : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Event_Type);

   type Generic_Event_Access_Type is access all Generic_Event_Type;
   pragma Convention (C, Generic_Event_Access_Type);

   type Drawable_Type is new X_Id_Type;

   subtype Window_Type is Drawable_Type;

   subtype Colormap_Type is Interfaces.Unsigned_32;

   subtype Visual_Id_Type is Interfaces.Unsigned_32;

   type Rectangle_Type is record
      X      : aliased Interfaces.C.short;
      Y      : aliased Interfaces.C.short;
      Width  : aliased Interfaces.Unsigned_16;
      Height : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Rectangle_Type);

   type Screen_Type is record
      Root                  : aliased Window_Type;
      Default_Colormap      : aliased Colormap_Type;
      White_Pixel           : aliased Interfaces.Unsigned_32;
      Black_Pixel           : aliased Interfaces.Unsigned_32;
      Current_Input_Masks   : aliased Interfaces.Unsigned_32;
      Width_In_Pixels       : aliased Interfaces.Unsigned_16;
      Height_In_Pixels      : aliased Interfaces.Unsigned_16;
      Width_In_Millimeters  : aliased Interfaces.Unsigned_16;
      Height_In_Millimeters : aliased Interfaces.Unsigned_16;
      Min_Installed_Maps    : aliased Interfaces.Unsigned_16;
      Max_Installed_Maps    : aliased Interfaces.Unsigned_16;
      Root_Visual           : aliased Visual_Id_Type;
      Backing_Stores        : aliased Interfaces.Unsigned_8;
      Save_Unders           : aliased Interfaces.Unsigned_8;
      Root_Depth            : aliased Interfaces.Unsigned_8;
      Allowed_Depths_Length : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Screen_Type);

   type Screen_Access_Type is access all Screen_Type;

   type Screen_Iterator_Type is record
      Data  : Screen_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Screen_Iterator_Type);

   type Window_Class_Type is
     (XCB_WINDOW_CLASS_COPY_FROM_PARENT,
      XCB_WINDOW_CLASS_INPUT_OUTPUT,
      XCB_WINDOW_CLASS_INPUT_ONLY);
   pragma Convention (C, Window_Class_Type);

   --
   -- Subprogram definitions
   --

   -- Connects to the X server specified by displayname and
   -- returns a newly allocated xcb_connection_t structure.
   -- If displayname is NULL, uses the value of the DISPLAY environment
   -- variable. If a particular screen on that server is preferred, the
   -- int pointed to by @p screenp (if not @c NULL) will be set to that
   -- screen; otherwise the screen will be set to 0.
   function Connect (Display_Name   : Interfaces.C.Strings.chars_ptr;
                     Screen_Pointer : access Interfaces.C.int := null) return Connection_Access_Type;
   pragma Import (C, Connect, "xcb_connect");

   -- Closes the connection to the X-server. Closes the file descriptor and
   -- frees all memory associated with the connection.
   procedure Disconnect (C : Connection_Access_Type);
   pragma Import (C, Disconnect, "xcb_disconnect");

   -- Returns the next event or error from the server.
   -- c: The connection to the X server.
   -- Returns the next event from the server.
   --
   -- Returns the next event or error from the server, or returns null in
   -- the event of an I/O error. Blocks until either an event or error
   -- arrive, or an I/O error occurs.
   function Wait_For_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;
   pragma Import (C, Wait_For_Event, "xcb_wait_for_event");

   procedure Free is new Ada.Unchecked_Deallocation (Object => Generic_Event_Type,
                                                     Name   => Generic_Event_Access_Type);

   -- Access the data returned by the server.
   -- c: The connection.
   -- Returns: A pointer to an xcb_setup_t structure.
   --
   -- Accessor for the data returned by the server when the xcb_connection_t
   -- was initialized. This data includes
   -- - the server's required format for images,
   -- - a list of available visuals,
   -- - a list of available screens,
   -- - the server's maximum request length (in the absence of the BIG-REQUESTS extension),
   -- - and other assorted information.
   --
   -- See the X protocol specification for more details.
   --
   -- The result must not be freed.
   function Get_Setup (C : Connection_Access_Type) return Setup_Constant_Access_Type;
   pragma Import (C, Get_Setup, "xcb_get_setup");

   -- Allocates an XID for a new object.
   -- c: The connection.
   -- Returns: A newly allocated XID.
   --
   -- Allocates an XID for a new object. Typically used just prior to
   -- various object creation functions, such as xcb_create_window.
   --
   function Generate_Id (C : Connection_Access_Type) return X_Id_Type;
   pragma Import (C, Generate_Id, "xcb_generate_id");

  -- Test whether the connection has shut down due to a fatal error.
  -- c: The connection.
  -- Returns: > 0 if the connection is in an error state; 0 otherwise.
  --
  -- Some errors that occur in the context of an xcb_connection_t
  -- are unrecoverable. When such an error occurs, the
  -- connection is shut down and further operations on the
  -- xcb_connection_t have no effect.
  --
  -- Different error codes:
  -- XCB_CONN_ERROR, because of socket errors, pipe errors or other stream errors.
  -- XCB_CONN_CLOSED_EXT_NOTSUPPORTED, when extension not supported.
  -- XCB_CONN_CLOSED_MEM_INSUFFICIENT, when memory not available.
  -- XCB_CONN_CLOSED_REQ_LEN_EXCEED, exceeding request length that server accepts.
  -- XCB_CONN_CLOSED_PARSE_ERR, error during parsing display string.
  -- XCB_CONN_CLOSED_INVALID_SCREEN, because the server does not have a screen matching the display.
   function Connection_Has_Error (C : Connection_Access_Type) return Interfaces.C.int;
   pragma Import (C, Connection_Has_Error, "xcb_connection_has_error");

   -- Forces any buffered output to be written to the server. Blocks until the write is complete.
   -- Returns > 0 on success, otherwise <= 0
   function Flush (C : Connection_Access_Type) return Interfaces.C.int;
   pragma Import (C, Flush, "xcb_flush");

  -- Creates a graphics context
  --
  -- c The connection
  -- cid The ID with which you will refer to the graphics context, created by `xcb_generate_id`.
  -- drawable Drawable to get the root/depth from.
  -- Returns: A cookie
  --
  -- Creates a graphics context. The graphics context can be used with any drawable
  -- that has the same root and depth as the specified drawable.
  --
  -- This form can be used only if the request will not cause
  -- a reply to be generated. Any returned error will be
  -- saved for handling by xcb_request_check().
   function Create_GC (C          : Connection_Access_Type;
                       Context_Id : Graphical_Context_Type;
                       Drawable   : Drawable_Type;
                       Value_Mask : GC_Type;
                       Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_GC, "xcb_create_gc");

   function Generate_Id (C : Connection_Access_Type) return Graphical_Context_Type;

   function Create_Window (C            : Connection_Access_Type;
                           Depth        : Interfaces.Unsigned_8;
                           Wid          : Window_Type;
                           Parent       : Window_Type;
                           X            : Interfaces.Integer_16;
                           Y            : Interfaces.Integer_16;
                           Width        : Interfaces.Unsigned_16;
                           Height       : Interfaces.Unsigned_16;
                           Border_Width : Interfaces.Unsigned_16;
                           U_Class      : Window_Class_Type;
                           Visual       : Visual_Id_Type;
                           Value_Mask   : Interfaces.Unsigned_32;
                           Value_List   : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Window, "xcb_create_window");

   function Map_Window (C      : Connection_Access_Type;
                        Window : Window_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Window, "xcb_map_window");

   function Generate_Id (C : Connection_Access_Type) return Window_Type;

   function Setup_Roots_Iterator (R : Setup_Constant_Access_Type) return Screen_Iterator_Type;
   pragma Import (C, Setup_Roots_Iterator, "xcb_setup_roots_iterator");

end XCB;
