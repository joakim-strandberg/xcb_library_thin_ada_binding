-- The original files were xcb.h and xproto.h.
-- Translated to Ada by Joakim Strandberg 2016.
--
-- The original copyright notice of xcb.h:
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
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package XCB is

   pragma Linker_Options ("-lxcb");

   type Window_Depth_Type is new Interfaces.Unsigned_8;

   type X_Coordinate_Type is new Interfaces.Integer_16;

   type Y_Coordinate_Type is new Interfaces.Integer_16;

   type Width_Type is new Interfaces.Unsigned_16;

   type Height_Type is new Interfaces.Unsigned_16;

   type Border_Width_Type is new Interfaces.Unsigned_16;

   subtype CW_Mask_Type is Interfaces.Unsigned_32;

   type Value_Mask_Type is new Interfaces.Unsigned_32;

   -- Identifier for objects in the XCB library. For example Windows,
   -- Graphical Contexts,...
   type X_Id_Type is new Interfaces.Unsigned_32;

   type Graphical_Context_Type is new X_Id_Type;

   type Graphical_Context_Access_Type is access all Graphical_Context_Type;

   type Graphical_Context_Iterator_Type is record
      Data : Graphical_Context_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Graphical_Context_Iterator_Type);

   subtype Event_Mask_Type is Interfaces.Unsigned_32;

   type Font_Id_Type is new Interfaces.Unsigned_32;

   type Font_Id_Access_Type is access all Font_Id_Type;

   type Font_Iterator_Type is record
      Data : Font_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Font_Iterator_Type);

   -- The names of the constants correspond exactly to the constant value names
   -- in the C-header files. One might be tempted to remove XCB_ from each
   -- constant name, but this will create XCB_ACCESS -> ACCESS and ACCESS is
   -- a reserved word in the Ada programming language. Because of this XCB_
   -- has not been removed from the names in the Constants package,
   -- maybe this is something that needs to be reviewed in the future?
   package Constants is

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
      XCB_CONFIGURE_NOTIFY  : constant := 22;
      XCB_CONFIGURE_REQUEST : constant := 23;
      XCB_GRAVITY_NOTIFY    : constant := 24;
      XCB_RESIZE_REQUEST    : constant := 25;
      XCB_CIRCULATE_NOTIFY  : constant := 26;
      XCB_CIRCULATE_REQUEST : constant := 27;
      XCB_PROPERTY_NOTIFY   : constant := 28;
      XCB_SELECTION_CLEAR   : constant := 29;
      XCB_SELECTION_REQUEST : constant := 30;
      XCB_SELECTION_NOTIFY  : constant := 31;
      XCB_COLORMAP_NOTIFY   : constant := 32;
      XCB_CLIENT_MESSAGE    : constant := 33;
      XCB_MAPPING_NOTIFY    : constant := 34;
      XCB_GE_GENERIC        : constant := 35;

      XCB_REQUEST                    : constant := 1;
      XCB_VALUE                      : constant := 2;
      XCB_WINDOW                     : constant := 3;
      XCB_PIXMAP                     : constant := 4;
      XCB_ATOM                       : constant := 5;
      XCB_CURSOR                     : constant := 6;
      XCB_FONT                       : constant := 7;
      XCB_MATCH                      : constant := 8;
      XCB_DRAWABLE                   : constant := 9;
      XCB_ACCESS                     : constant := 10;
      XCB_ALLOC                      : constant := 11;
      XCB_COLORMAP                   : constant := 12;
      XCB_G_CONTEXT                  : constant := 13;
      XCB_ID_CHOICE                  : constant := 14;
      XCB_NAME                       : constant := 15;
      XCB_LENGTH                     : constant := 16;
      XCB_IMPLEMENTATION             : constant := 17;

      XCB_CREATE_WINDOW              : constant := 1;
      XCB_CHANGE_WINDOW_ATTRIBUTES   : constant := 2;
      XCB_GET_WINDOW_ATTRIBUTES      : constant := 3;
      XCB_DESTROY_WINDOW             : constant := 4;
      XCB_DESTROY_SUBWINDOWS         : constant := 5;
      XCB_CHANGE_SAVE_SET            : constant := 6;
      XCB_REPARENT_WINDOW            : constant := 7;
      XCB_MAP_WINDOW                 : constant := 8;
      XCB_MAP_SUBWINDOWS             : constant := 9;
      XCB_UNMAP_WINDOW               : constant := 10;
      XCB_UNMAP_SUBWINDOWS           : constant := 11;
      XCB_CONFIGURE_WINDOW           : constant := 12;
      XCB_CIRCULATE_WINDOW           : constant := 13;
      XCB_GET_GEOMETRY               : constant := 14;
      XCB_QUERY_TREE                 : constant := 15;
      XCB_INTERN_ATOM                : constant := 16;
      XCB_GET_ATOM_NAME              : constant := 17;
      XCB_CHANGE_PROPERTY            : constant := 18;
      XCB_DELETE_PROPERTY            : constant := 19;
      XCB_GET_PROPERTY               : constant := 20;
      XCB_LIST_PROPERTIES            : constant := 21;
      XCB_SET_SELECTION_OWNER        : constant := 22;
      XCB_GET_SELECTION_OWNER        : constant := 23;
      XCB_CONVERT_SELECTION          : constant := 24;
      XCB_SEND_EVENT                 : constant := 25;
      XCB_GRAB_POINTER               : constant := 26;
      XCB_UNGRAB_POINTER             : constant := 27;
      XCB_GRAB_BUTTON                : constant := 28;
      XCB_UNGRAB_BUTTON              : constant := 29;
      XCB_CHANGE_ACTIVE_POINTER_GRAB : constant := 30;
      XCB_GRAB_KEYBOARD              : constant := 31;
      XCB_UNGRAB_KEYBOARD            : constant := 32;
      XCB_GRAB_KEY                   : constant := 33;
      XCB_UNGRAB_KEY                 : constant := 34;
      XCB_ALLOW_EVENTS               : constant := 35;
      XCB_GRAB_SERVER                : constant := 36;
      XCB_UNGRAB_SERVER              : constant := 37;
      XCB_QUERY_POINTER              : constant := 38;
      XCB_GET_MOTION_EVENTS          : constant := 39;
      XCB_TRANSLATE_COORDINATES      : constant := 40;
      XCB_WARP_POINTER               : constant := 41;
      XCB_SET_INPUT_FOCUS            : constant := 42;
      XCB_GET_INPUT_FOCUS            : constant := 43;
      XCB_QUERY_KEYMAP               : constant := 44;
      XCB_OPEN_FONT                  : constant := 45;
      XCB_CLOSE_FONT                 : constant := 46;
      XCB_QUERY_FONT                 : constant := 47;
      XCB_QUERY_TEXT_EXTENTS         : constant := 48;
      XCB_LIST_FONTS                 : constant := 49;
      XCB_LIST_FONTS_WITH_INFO       : constant := 50;
      XCB_SET_FONT_PATH              : constant := 51;
      XCB_GET_FONT_PATH              : constant := 52;
      XCB_CREATE_PIXMAP              : constant := 53;
      XCB_FREE_PIXMAP                : constant := 54;
      XCB_CREATE_GC                  : constant := 55;
      XCB_CHANGE_GC                  : constant := 56;
      XCB_COPY_GC                    : constant := 57;
      XCB_SET_DASHES                 : constant := 58;
      XCB_SET_CLIP_RECTANGLES        : constant := 59;
      XCB_FREE_GC                    : constant := 60;
      XCB_CLEAR_AREA                 : constant := 61;
      XCB_COPY_AREA                  : constant := 62;
      XCB_COPY_PLANE                 : constant := 63;
      XCB_POLY_POINT                 : constant := 64;
      XCB_POLY_LINE                  : constant := 65;
      XCB_POLY_SEGMENT               : constant := 66;
      XCB_POLY_RECTANGLE             : constant := 67;
      XCB_POLY_ARC                   : constant := 68;
      XCB_FILL_POLY                  : constant := 69;
      XCB_POLY_FILL_RECTANGLE        : constant := 70;
      XCB_POLY_FILL_ARC              : constant := 71;
      XCB_PUT_IMAGE                  : constant := 72;
      XCB_GET_IMAGE                  : constant := 73;
      XCB_POLY_TEXT_8                : constant := 74;
      XCB_POLY_TEXT_16               : constant := 75;
      XCB_IMAGE_TEXT_8               : constant := 76;
      XCB_IMAGE_TEXT_16              : constant := 77;
      XCB_CREATE_COLORMAP            : constant := 78;
      XCB_FREE_COLORMAP              : constant := 79;
      XCB_COPY_COLORMAP_AND_FREE     : constant := 80;
      XCB_INSTALL_COLORMAP           : constant := 81;
      XCB_UNINSTALL_COLORMAP         : constant := 82;
      XCB_LIST_INSTALLED_COLORMAPS   : constant := 83;
      XCB_ALLOC_COLOR                : constant := 84;
      XCB_ALLOC_NAMED_COLOR          : constant := 85;
      XCB_ALLOC_COLOR_CELLS          : constant := 86;
      XCB_ALLOC_COLOR_PLANES         : constant := 87;
      XCB_FREE_COLORS                : constant := 88;
      XCB_STORE_COLORS               : constant := 89;
      XCB_STORE_NAMED_COLOR          : constant := 90;
      XCB_QUERY_COLORS               : constant := 91;
      XCB_LOOKUP_COLOR               : constant := 92;
      XCB_CREATE_CURSOR              : constant := 93;
      XCB_CREATE_GLYPH_CURSOR        : constant := 94;
      XCB_FREE_CURSOR                : constant := 95;
      XCB_RECOLOR_CURSOR             : constant := 96;
      XCB_QUERY_BEST_SIZE            : constant := 97;
      XCB_QUERY_EXTENSION            : constant := 98;
      XCB_LIST_EXTENSIONS            : constant := 99;
      XCB_CHANGE_KEYBOARD_MAPPING    : constant := 100;
      XCB_GET_KEYBOARD_MAPPING       : constant := 101;
      XCB_CHANGE_KEYBOARD_CONTROL    : constant := 102;
      XCB_GET_KEYBOARD_CONTROL       : constant := 103;
      XCB_BELL                       : constant := 104;
      XCB_CHANGE_POINTER_CONTROL     : constant := 105;
      XCB_GET_POINTER_CONTROL        : constant := 106;
      XCB_SET_SCREEN_SAVER           : constant := 107;
      XCB_GET_SCREEN_SAVER           : constant := 108;
      XCB_CHANGE_HOSTS               : constant := 109;
      XCB_LIST_HOSTS                 : constant := 110;
      XCB_SET_ACCESS_CONTROL         : constant := 111;
      XCB_SET_CLOSE_DOWN_MODE        : constant := 112;
      XCB_KILL_CLIENT                : constant := 113;
      XCB_ROTATE_PROPERTIES          : constant := 114;
      XCB_FORCE_SCREEN_SAVER         : constant := 115;
      XCB_SET_POINTER_MAPPING        : constant := 116;
      XCB_GET_POINTER_MAPPING        : constant := 117;
      XCB_SET_MODIFIER_MAPPING       : constant := 118;
      XCB_GET_MODIFIER_MAPPING       : constant := 119;
      XCB_NO_OPERATION               : constant := 127;

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

      XCB_GC_FUNCTION              : constant Value_Mask_Type := 1;
      XCB_GC_PLANE_MASK            : constant Value_Mask_Type := 2;
      XCB_GC_FOREGROUND            : constant Value_Mask_Type := 4;
      XCB_GC_BACKGROUND            : constant Value_Mask_Type := 8;
      XCB_GC_LINE_WIDTH            : constant Value_Mask_Type := 16;
      XCB_GC_LINE_STYLE            : constant Value_Mask_Type := 32;
      XCB_GC_CAP_STYLE             : constant Value_Mask_Type := 64;
      XCB_GC_JOIN_STYLE            : constant Value_Mask_Type := 128;
      XCB_GC_FILL_STYLE            : constant Value_Mask_Type := 256;
      XCB_GC_FILL_RULE             : constant Value_Mask_Type := 512;
      XCB_GC_TILE                  : constant Value_Mask_Type := 1024;
      XCB_GC_STIPPLE               : constant Value_Mask_Type := 2048;
      XCB_GC_TILE_STIPPLE_ORIGIN_X : constant Value_Mask_Type := 4096;
      XCB_GC_TILE_STIPPLE_ORIGIN_Y : constant Value_Mask_Type := 8192;
      XCB_GC_FONT                  : constant Value_Mask_Type := 16384;
      XCB_GC_SUBWINDOW_MODE        : constant Value_Mask_Type := 32768;
      XCB_GC_GRAPHICS_EXPOSURES    : constant Value_Mask_Type := 65536;
      XCB_GC_CLIP_ORIGIN_X         : constant Value_Mask_Type := 131072;
      XCB_GC_CLIP_ORIGIN_Y         : constant Value_Mask_Type := 262144;
      XCB_GC_CLIP_MASK             : constant Value_Mask_Type := 524288;
      XCB_GC_DASH_OFFSET           : constant Value_Mask_Type := 1048576;
      XCB_GC_DASH_LIST             : constant Value_Mask_Type := 2097152;
      XCB_GC_ARC_MODE              : constant Value_Mask_Type := 4194304;

      XCB_CW_BACK_PIXMAP       : constant CW_Mask_Type := 1;
      XCB_CW_BACK_PIXEL        : constant CW_Mask_Type := 2;
      XCB_CW_BORDER_PIXMAP     : constant CW_Mask_Type := 4;
      XCB_CW_BORDER_PIXEL      : constant CW_Mask_Type := 8;
      XCB_CW_BIT_GRAVITY       : constant CW_Mask_Type := 16;
      XCB_CW_WIN_GRAVITY       : constant CW_Mask_Type := 32;
      XCB_CW_BACKING_STORE     : constant CW_Mask_Type := 64;
      XCB_CW_BACKING_PLANES    : constant CW_Mask_Type := 128;
      XCB_CW_BACKING_PIXEL     : constant CW_Mask_Type := 256;
      XCB_CW_OVERRIDE_REDIRECT : constant CW_Mask_Type := 512;
      XCB_CW_SAVE_UNDER        : constant CW_Mask_Type := 1024;
      XCB_CW_EVENT_MASK        : constant CW_Mask_Type := 2048;
      XCB_CW_DONT_PROPAGATE    : constant CW_Mask_Type := 4096;
      XCB_CW_COLORMAP          : constant CW_Mask_Type := 8192;
      XCB_CW_CURSOR            : constant CW_Mask_Type := 16384;

   end Constants;

   --
   -- Type definitions
   --

   type Void_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Void_Cookie_Type);

   type Intern_Atom_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Intern_Atom_Cookie_Type);

   subtype Atom_Type is Interfaces.Unsigned_32;

   type Atom_Access_Type is access all Atom_Type;

   type Atom_Iterator_Type is record
      Data  : Atom_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Atom_Iterator_Type);

   type Intern_Atom_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Atom          : aliased Atom_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Intern_Atom_Reply_Type);

   type Intern_Atom_Reply_Access_Type is access all Intern_Atom_Reply_Type;
   for Intern_Atom_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Intern_Atom_Reply_Access_Type);

   -- Opaque structure containing all data that XCB needs in order
   -- to communicate with an X server.
   type Connection_Type is limited null record;

   type Connection_Access_Type is access all Connection_Type;
   for Connection_Access_Type'Storage_Size use 0;
   pragma Convention (C, Connection_Access_Type);

   type Key_Code_Type is new Interfaces.Unsigned_8;

   type Key_Code_Access_Type is access all Key_Code_Type;

   type Key_Code_Iterator_Type is record
      Data  : Key_Code_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Key_Code_Iterator_Type);

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
      Minimum_Keycode             : aliased Key_Code_Type;
      Maximum_Keycode             : aliased Key_Code_Type;
      Paddding_1                  : aliased Setup_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Type);

   type Setup_Constant_Access_Type is access constant Setup_Type;

   type Value_List_Array is array (Natural range <>) of Interfaces.Unsigned_32;
   pragma Convention (C, Value_List_Array);

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

   type Drawable_Access_Type is access all Drawable_Type;

   type Drawable_Iterator_Type is record
      Data  : Drawable_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Drawable_Iterator_Type);

   type Fontable_Type is new Interfaces.Unsigned_32;

   type Fontable_Access_Type is access all Fontable_Type;

   type Fontable_Iterator_Type is record
      Data  : Fontable_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Fontable_Iterator_Type);

   type Button_Id_Type is new Interfaces.Unsigned_8;

   type Button_Id_Access_Type is access all Button_Id_Type;

   type Button_Id_Iterator_Type is record
      Data  : Button_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Button_Id_Iterator_Type);

   type Point_Type is record
      X : aliased Interfaces.Integer_16;
      Y : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_Type);

   type Point_Access_Type is access all Point_Type;

   type Point_Iterator_Type is record
      Data  : access Point_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_Iterator_Type);

   subtype Window_Id_Type is Drawable_Type;

   type Window_Id_Access_Type is access all Window_Id_Type;

   subtype Colormap_Type is Interfaces.Unsigned_32;

   type Colormap_Access_Type is access all Colormap_Type;

   type Colormap_Iterator_Type is record
      Data  : Colormap_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Colormap_Iterator_Type);

   subtype Visual_Id_Type is Interfaces.Unsigned_32;

   type Visual_Id_Access_Type is access all Visual_Id_Type;

   type Visual_Id_Iterator_Type is record
      Data  : Visual_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Id_Iterator_Type);

   type Rectangle_Type is record
      X      : aliased Interfaces.C.short;
      Y      : aliased Interfaces.C.short;
      Width  : aliased Interfaces.Unsigned_16;
      Height : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Rectangle_Type);

   type Rectangle_Access_Type is access all Rectangle_Type;

   type Rectangle_Iterator_Type is record
      Data  : Rectangle_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Rectangle_Iterator_Type);

   type Arc_Type is record
      X      : aliased Interfaces.Integer_16;
      Y      : aliased Interfaces.Integer_16;
      Width  : aliased Interfaces.Unsigned_16;
      Height : aliased Interfaces.Unsigned_16;
      Angle_1 : aliased Interfaces.Integer_16;
      Angle_2 : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Arc_Type);

   type Arc_Access_Type is access all Arc_Type;

   type Arc_Iterator_Type is record
      Data  : Arc_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Arc_Iterator_Type);

   type Format_Padding_0_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_8;
   type Format_Type is record
      Depth           : aliased Interfaces.Unsigned_8;
      Bits_Per_Pixel  : aliased Interfaces.Unsigned_8;
      Scanline_Pad    : aliased Interfaces.Unsigned_8;
      Padding_0_Array : aliased Format_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Format_Type);

   type Format_Access_Type is access all Format_Type;

   type Format_Iterator_Type is record
      Data  : Format_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Format_Iterator_Type);

   type Visual_Class_Type is
     (XCB_VISUAL_CLASS_STATIC_GRAY,
      XCB_VISUAL_CLASS_GRAY_SCALE,
      XCB_VISUAL_CLASS_STATIC_COLOR,
      XCB_VISUAL_CLASS_PSEUDO_COLOR,
      XCB_VISUAL_CLASS_TRUE_COLOR,
      XCB_VISUAL_CLASS_DIRECT_COLOR);
   pragma Convention (C, Visual_Class_Type);

   type Visual_Kind_Padding_0_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;

   type Visual_Kind_Type is record
      Visual_Id          : aliased Visual_Id_Type;
      u_class            : aliased Interfaces.Unsigned_8;
      bits_per_rgb_value : aliased Interfaces.Unsigned_8;
      colormap_entries   : aliased Interfaces.Unsigned_16;
      red_mask           : aliased Interfaces.Unsigned_32;
      green_mask         : aliased Interfaces.Unsigned_32;
      blue_mask          : aliased Interfaces.Unsigned_32;
      pad0               : aliased Visual_Kind_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Kind_Type);

   type Visual_Kind_Access_Type is access all Visual_Kind_Type;

   type Visual_Kind_Iterator_Type is record
      Data  : Visual_Kind_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Kind_Iterator_Type);

   type Depth_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Depth_Type is record
      Depth           : aliased Interfaces.Unsigned_8;
      Padding_0       : aliased Interfaces.Unsigned_8;
      Visuals_Length  : aliased Interfaces.Unsigned_16;
      Padding_1_Array : aliased Depth_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Depth_Type);

   type Depth_Access_Type is access all Depth_Type;

   type Depth_Iterator_Type is record
      Data  : Depth_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Depth_Iterator_Type);

   type Backing_Store_Type is
     (XCB_BACKING_STORE_NOT_USEFUL,
      XCB_BACKING_STORE_WHEN_MAPPED,
      XCB_BACKING_STORE_ALWAYS);
   pragma Convention (C, Backing_Store_Type);

   type Screen_Type is record
      Root                  : aliased Window_Id_Type;
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
      Root_Visual_Id        : aliased Visual_Id_Type;
      Backing_Stores        : aliased Interfaces.Unsigned_8;
      Save_Unders           : aliased Interfaces.Unsigned_8;
      Root_Depth            : aliased Window_Depth_Type;
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

   type Screen_Iterator_Access_Type is access all Screen_Iterator_Type;

   type Setup_Request_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Setup_Request_Type is record
      Byte_Order                         : aliased Interfaces.Unsigned_8;
      Padding_0                          : aliased Interfaces.Unsigned_8;
      Protocol_Major_Version             : aliased Interfaces.Unsigned_16;
      Protocol_Minor_Version             : aliased Interfaces.Unsigned_16;
      Authorization_Protocol_Name_Length : aliased Interfaces.Unsigned_16;
      Authorization_Protocol_Data_Length : aliased Interfaces.Unsigned_16;
      Padding_1_Array                    : aliased Setup_Request_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Request_Type);

   type Setup_Request_Access_Type is access all Setup_Request_Type;

   type Setup_Request_Iterator_Type is record
      Data : Setup_Request_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Request_Iterator_Type);

   type Setup_Failed_Type is record
      Status                 : aliased Interfaces.Unsigned_8;
      Reason_Length          : aliased Interfaces.Unsigned_8;
      Protocol_Major_Version : aliased Interfaces.Unsigned_16;
      Protocol_Minor_Version : aliased Interfaces.Unsigned_16;
      Length                 : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Failed_Type);

   type Setup_Failed_Access_Type is access all Setup_Failed_Type;

   type Setup_Failed_Iterator_Type is record
      Data  : Setup_Failed_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Failed_Iterator_Type);

   type Setup_Authenticate_Padding_0_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_8;
   type Setup_Authenticate_Type is record
      Status    : aliased Interfaces.Unsigned_8;
      Padding_0 : aliased Setup_Authenticate_Padding_0_Array_Type;
      Length    : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Authenticate_Type);

   type Setup_Authenticate_Access_Type is access all Setup_Authenticate_Type;

   type Setup_Authenticate_Iterator_Type is record
      data  : Setup_Authenticate_Access_Type;
      c_rem : aliased Interfaces.C.int;
      index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Authenticate_Iterator_Type);

   type Image_Order_Type is
     (XCB_IMAGE_ORDER_LSB_FIRST,
      XCB_IMAGE_ORDER_MSB_FIRST);
   pragma Convention (C, Image_Order_Type);


   type Window_Class_Type is
     (XCB_WINDOW_CLASS_COPY_FROM_PARENT,
      XCB_WINDOW_CLASS_INPUT_OUTPUT,
      XCB_WINDOW_CLASS_INPUT_ONLY);
   pragma Convention (C, Window_Class_Type);

   type Prop_Mode_Type is
     (XCB_PROP_MODE_REPLACE,
      XCB_PROP_MODE_PREPEND,
      XCB_PROP_MODE_APPEND);
   pragma Convention (C, Prop_Mode_Type);

   type Expose_Event_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Expose_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Window_Id     : aliased Window_Id_Type;
      X             : aliased Interfaces.Unsigned_16;
      Y             : aliased Interfaces.Unsigned_16;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
      Count         : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Expose_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Expose_Event_Type);

   type Expose_Event_Access_Type is access all Expose_Event_Type;

   subtype Timestamp_Type is Interfaces.Unsigned_32;

   type Timestamp_Access_Type is access all Timestamp_Type;

   type Timestamp_Iterator_Type is record
      Data  : Timestamp_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Timestamp_Iterator_Type);

   type Keysym_Type is new Interfaces.Unsigned_32;

   type Keysym_Access_Type is access all Keysym_Type;

   type Keysym_Iterator_Type is record
      Data  : Keysym_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Keysym_Iterator_Type);

   type Button_Press_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Button_Id_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Time          : aliased Timestamp_Type;
      Root          : aliased Window_Id_Type;
      Event         : aliased Window_Id_Type;
      Child         : aliased Window_Id_Type;
      Root_X        : aliased Interfaces.Integer_16;
      Root_Y        : aliased Interfaces.Integer_16;
      Event_X       : aliased Interfaces.Integer_16;
      Event_Y       : aliased Interfaces.Integer_16;
      State         : aliased Interfaces.Unsigned_16;
      Same_Screen   : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Button_Press_Event_Type);

   type Button_Press_Event_Access_Type is access all Button_Press_Event_Type;

   type Button_Release_Event_Type is new Button_Press_Event_Type;

   type Button_Release_Event_Access_Type is access all Button_Release_Event_Type;

   type Motion_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Time          : aliased Timestamp_Type;
      Root          : aliased Window_Id_Type;
      Event         : aliased Window_Id_Type;
      Child         : aliased Window_Id_Type;
      Root_X        : aliased Interfaces.Integer_16;
      Root_Y        : aliased Interfaces.Integer_16;
      Event_X       : aliased Interfaces.Integer_16;
      Event_Y       : aliased Interfaces.Integer_16;
      State         : aliased Interfaces.Unsigned_16;
      Same_Screen   : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Motion_Notify_Event_Type);

   type Motion_Notify_Event_Access_Type is access all Motion_Notify_Event_Type;

   type Enter_Notify_Event_Type is record
      Response_Kind     : aliased Interfaces.Unsigned_8;
      Detail            : aliased Interfaces.Unsigned_8;
      Sequence          : aliased Interfaces.Unsigned_16;
      Time              : aliased Timestamp_Type;
      Root              : aliased Window_Id_Type;
      Event             : aliased Window_Id_Type;
      Child             : aliased Window_Id_Type;
      Root_X            : aliased Interfaces.Integer_16;
      Root_Y            : aliased Interfaces.Integer_16;
      Event_X           : aliased Interfaces.Integer_16;
      Event_Y           : aliased Interfaces.Integer_16;
      State             : aliased Interfaces.Unsigned_16;
      Mode              : aliased Interfaces.Unsigned_8;
      Same_Screen_Focus : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Enter_Notify_Event_Type);

   type Enter_Notify_Event_Access_Type is access all Enter_Notify_Event_Type;

   type Leave_Notify_Event_Type is new Enter_Notify_Event_Type;

   type Leave_Notify_Event_Access_Type is access all Leave_Notify_Event_Type;

   type Key_Press_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Key_Code_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Time          : aliased Timestamp_Type;
      Root          : aliased Window_Id_Type;
      Event         : aliased Window_Id_Type;
      Child         : aliased Window_Id_Type;
      Root_X        : aliased Interfaces.Integer_16;
      Root_Y        : aliased Interfaces.Integer_16;
      Event_X       : aliased Interfaces.Integer_16;
      Event_Y       : aliased Interfaces.Integer_16;
      State         : aliased Interfaces.Unsigned_16;
      Same_Screen   : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Key_Press_Event_Type);

   type Key_Press_Event_Access_Type is access all Key_Press_Event_Type;

   type Key_Release_Event_Type is new Key_Press_Event_Type;

   type Key_Release_Event_Access_Type is access all Key_Release_Event_Type;

   type Generic_Error_Padding_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_32;
   type Generic_Error_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Error_Code    : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Resource_Id   : aliased Interfaces.Unsigned_32;
      Minor_Code    : aliased Interfaces.Unsigned_16;
      Major_Code    : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Padding_Array : aliased Generic_Error_Padding_Array_Type;
      Full_Sequence : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Error_Type);

   type Generic_Error_Access_Type is access all Generic_Error_Type;

   type Generic_Iterator_Type is record
      Data  : System.Address;
      c_rem : aliased Interfaces.C.int;
      index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Iterator_Type);

   type Generic_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Reply_Type);

   type GE_Event_Padding_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_32;
   type GE_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Event_Kind    : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Interfaces.Unsigned_16;
      Padding_Array : aliased GE_Event_Padding_Array_Type;
      Full_Sequence : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, GE_Event_Type);

   type Authorization_Info_Type is record
      Name_Length : aliased Interfaces.C.int;
      Name        : Interfaces.C.Strings.chars_ptr;
      Data_Length : aliased Interfaces.C.int;
      Data        : Interfaces.C.Strings.chars_ptr;
   end record;
   pragma Convention (C_Pass_By_Copy, Authorization_Info_Type);

   type Authorization_Info_Access_Type is access all Authorization_Info_Type;

   type Special_Event_Type is null record;

   type Special_Event_Access is access all Special_Event_Type;

   type Query_Extension_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Present       : aliased Interfaces.Unsigned_8;
      Major_Op_Code : aliased Interfaces.Unsigned_8;
      First_Event   : aliased Interfaces.Unsigned_8;
      First_Error   : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Extension_Reply_Type);

   type Query_Extension_Reply_Constant_Access_Type is access constant Query_Extension_Reply_Type;

   type Extension_Type is null record;

   type Extension_Access_Type is access all Extension_Type;

   type Char2b_Type is record
      Byte_1 : aliased Interfaces.Unsigned_8;
      Byte_2 : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Char2b_Type);

   type Char2b_Access_Type is access all Char2b_Type;

   type Char2b_Iterator_Type is record
      data  : Char2b_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Char2b_Iterator_Type);

   type Window_Iterator_Type is record
      Data  : Window_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Window_Iterator_Type);

   type Pixmap_Id_Type is new Interfaces.Unsigned_32;

   type Pixmap_Id_Access_Type is access all Pixmap_Id_Type;

   type Pixmap_Iterator_Type is record
      Data  : Pixmap_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Pixmap_Iterator_Type);

   type Cursor_Id_Type is new Interfaces.Unsigned_32;

   type Cursor_Id_Access_Type is access all Cursor_Id_Type;

   type Cursor_Iterator_Type is record
      Data  : Cursor_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Cursor_Iterator_Type);



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

   function Poll_For_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;
   pragma Import (C, Poll_For_Event, "xcb_poll_for_event");

   function Poll_For_Queued_Event (C : Connection_Access_Type) return Generic_Event_Access_Type;
   pragma Import (C, Poll_For_Queued_Event, "xcb_poll_for_queued_event");

   function Poll_For_Special_Event (C  : Connection_Access_Type;
                                    SE : System.Address) return Generic_Event_Access_Type;
   pragma Import (C, Poll_For_Special_Event, "xcb_poll_for_special_event");

   function Wait_For_Special_Event (C : Connection_Access_Type;
                                    SE : System.Address) return Generic_Event_Access_Type;
   pragma Import (C, Wait_For_Special_Event, "xcb_wait_for_special_event");

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
                       Value_Mask : Value_Mask_Type;
                       Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_GC, "xcb_create_gc");

   function Generate_Id (C : Connection_Access_Type) return Graphical_Context_Type;

   function Generate_Id (C : Connection_Access_Type) return Font_Id_Type;

   function Create_Window (C            : Connection_Access_Type;
                           Depth        : Window_Depth_Type;
                           Window_Id    : Window_Id_Type;
                           Parent       : Window_Id_Type;
                           X            : X_Coordinate_Type;
                           Y            : Y_Coordinate_Type;
                           Width        : Width_Type;
                           Height       : Height_Type;
                           Border_Width : Border_Width_Type;
                           U_Class      : Window_Class_Type;
                           Visual_Id    : Visual_Id_Type;
                           Value_Mask   : CW_Mask_Type;
                           Value_List   : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Window, "xcb_create_window");

   function Map_Window (C      : Connection_Access_Type;
                        Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Window, "xcb_map_window");

   function Map_Window_Checked (C : Connection_Access_Type;
                                Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Window_Checked, "xcb_map_window_checked");

   function Generate_Id (C : Connection_Access_Type) return Window_Id_Type;

   function Setup_Roots_Iterator (R : Setup_Constant_Access_Type) return Screen_Iterator_Type;
   pragma Import (C, Setup_Roots_Iterator, "xcb_setup_roots_iterator");

   function Intern_Atom (C              : Connection_Access_Type;
                         Only_If_Exists : Interfaces.Unsigned_8;
                         Name_Length    : Interfaces.Unsigned_16;
                         Name           : Interfaces.C.Strings.chars_ptr) return Intern_Atom_Cookie_Type;
   pragma Import (C, Intern_Atom, "xcb_intern_atom");

   function Intern_Atom_Reply (C      : Connection_Access_Type;
                               Cookie : Intern_Atom_Cookie_Type;
                               Error  : System.Address) return Intern_Atom_Reply_Access_Type;
   pragma Import (C, Intern_Atom_Reply, "xcb_intern_atom_reply");

   function Change_Property (c           : Connection_Access_Type;
                             Mode        : Interfaces.Unsigned_8;
                             Window      : Window_Id_Type;
                             Property    : Atom_Type;
                             C_Kind      : Atom_Type;
                             Format      : Interfaces.Unsigned_8;
                             Data_Length : Interfaces.Unsigned_32;
                             Data        : System.Address) return Void_Cookie_Type;
   pragma Import (C, Change_Property, "xcb_change_property");

   function To_Expose_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                             Target => Expose_Event_Access_Type);

   function To_Button_Press_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                   Target => Button_Press_Event_Access_Type);

   function To_Button_Release_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                     Target => Button_Release_Event_Access_Type);

   function To_Motion_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                    Target => Motion_Notify_Event_Access_Type);

   function To_Enter_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                   Target => Enter_Notify_Event_Access_Type);

   function To_Leave_Notify_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                   Target => Leave_Notify_Event_Access_Type);

   function To_Key_Press_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                Target => Key_Press_Event_Access_Type);

   function To_Key_Release_Event is new Ada.Unchecked_Conversion (Source => Generic_Event_Access_Type,
                                                                  Target => Key_Release_Event_Access_Type);

   function Open_Font_Checked (C           : Connection_Access_Type;
                               Font_Id     : Font_Id_Type;
                               Name_Length : Interfaces.Unsigned_16;
                               name        : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Open_Font_Checked, "xcb_open_font_checked");

   function Request_Check (C      : Connection_Access_Type;
                           Cookie : Void_Cookie_Type) return Generic_Error_Access_Type;
   pragma Import (C, Request_Check, "xcb_request_check");

   function Create_GC_Checked (C          : Connection_Access_Type;
                               Context_Id : XCB.Graphical_Context_Type;
                               Drawable   : XCB.Drawable_Type;
                               Value_Mask : Value_Mask_Type;
                               Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_GC_Checked, "xcb_create_gc_checked");

   function Close_Font_Checked (C : Connection_Access_Type;
                                Font : XCB.Font_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Close_Font_Checked, "xcb_close_font_checked");

  --
  -- @brief Draws text
  --
  -- @param c The connection
  -- @param string_len The length of the \a string. Note that this parameter limited by 255 due to
  -- using 8 bits!
  -- @param drawable The drawable (Window or Pixmap) to draw text on.
  -- @param gc The graphics context to use.
  -- \n
  -- The following graphics context components are used: plane-mask, foreground,
  -- background, font, subwindow-mode, clip-x-origin, clip-y-origin, and clip-mask.
  -- @param x The x coordinate of the first character, relative to the origin of \a drawable.
  -- @param y The y coordinate of the first character, relative to the origin of \a drawable.
  -- @param string The string to draw. Only the first 255 characters are relevant due to the data
  -- type of \a string_len.
  -- @return A cookie
  --
  -- Fills the destination rectangle with the background pixel from \a gc, then
  -- paints the text with the foreground pixel from \a gc. The upper-left corner of
  -- the filled rectangle is at [x, y - font-ascent]. The width is overall-width,
  -- the height is font-ascent + font-descent. The overall-width, font-ascent and
  -- font-descent are as returned by `xcb_query_text_extents` (TODO).
  --
  -- Note that using X core fonts is deprecated (but still supported) in favor of
  -- client-side rendering using Xft.
  --
  -- This form can be used only if the request will not cause
  -- a reply to be generated. Any returned error will be
  -- saved for handling by xcb_request_check().
   function Image_Text_8_Checked (C             : Connection_Access_Type;
                                  String_Length : Interfaces.Unsigned_8;
                                  Drawable      : Drawable_Type;
                                  GC            : Graphical_Context_Type;
                                  X             : Interfaces.Integer_16;
                                  Y             : Interfaces.Integer_16;
                                  Text          : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Image_Text_8_Checked, "xcb_image_text_8_checked");

   function Free_GC (C  : Connection_Access_Type;
                     GC : Graphical_Context_Type) return Void_Cookie_Type;
   pragma Import (C, Free_GC, "xcb_free_gc");

   procedure Screen_Next (I : Screen_Iterator_Access_Type);
   pragma Import (C, Screen_Next, "xcb_screen_next");

   function Get_Maximum_Request_Length (C : Connection_Access_Type) return Interfaces.Unsigned_32;
   pragma Import (C, Get_Maximum_Request_Length, "xcb_get_maximum_request_length");

   procedure Prefetch_Maximum_Request_Length (C : Connection_Access_Type);
   pragma Import (C, Prefetch_Maximum_Request_Length, "xcb_prefetch_maximum_request_length");

   -- Listen for a special event.
   function Register_For_Special_XGE (C            : Connection_Access_Type;
                                      Extension    : System.Address;
                                      Extension_Id : Interfaces.Unsigned_32;
                                      Stamp        : access Interfaces.Unsigned_32) return Special_Event_Access;
   pragma Import (C, Register_For_Special_XGE, "xcb_register_for_special_xge");

   -- Stop listening for a special event.
   procedure Unregister_For_Special_Event (C : Connection_Access_Type;
                                           SE : Special_Event_Access);
   pragma Import (C, Unregister_For_Special_Event, "xcb_unregister_for_special_event");

   procedure Discard_Reply (C        : Connection_Access_Type;
                            Sequence : Interfaces.C.unsigned);
   pragma Import (C, Discard_Reply, "xcb_discard_reply");

   function Get_Extension_Data (C   : Connection_Access_Type;
                                Ext : Extension_Access_Type) return Query_Extension_Reply_Constant_Access_Type;
   pragma Import (C, Get_Extension_Data, "xcb_get_extension_data");

   procedure Prefetch_Extension_Data (C   : Connection_Access_Type;
                                      Ext : Extension_Access_Type);
   pragma Import (C, Prefetch_Extension_Data, "xcb_prefetch_extension_data");

   function Get_File_Descriptor (C : Connection_Access_Type) return Interfaces.C.int;
   pragma Import (C, Get_File_Descriptor, "xcb_get_file_descriptor");

   -- Connects to the X server.
   function Connect_To_File_Descriptor (FD        : Interfaces.C.int;
                                        Auth_Info : Authorization_Info_Access_Type) return Connection_Access_Type;
   pragma Import (C, Connect_To_File_Descriptor, "xcb_connect_to_fd");

   function Parse_Display (Name    : Interfaces.C.Strings.chars_ptr;
                           Host    : System.Address; -- Pointer to a malloced copy of the hostname
                           Display : access Interfaces.C.int;
                           Screen  : access Interfaces.C.int) return Interfaces.C.int;
   pragma Import (C, Parse_Display, "xcb_parse_display");

   function Connect_To_Display_With_Authorization_Info (Display : Interfaces.C.Strings.chars_ptr;
                                                        Auth    : Authorization_Info_Access_Type;
                                                        Screen  : access Interfaces.C.int) return Connection_Access_Type;
   pragma Import (C, Connect_To_Display_With_Authorization_Info, "xcb_connect_to_display_with_auth_info");

end XCB;
