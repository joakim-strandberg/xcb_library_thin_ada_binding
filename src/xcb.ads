with Interfaces.C.Strings;
with System;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

package XCB is

   pragma Linker_Options ("-lxcb");

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
   XCB_BUTTON_PRESS      : constant := 4;
   XCB_MOTION_NOTIFY     : constant := 6;
   XCB_ENTER_NOTIFY      : constant := 7;
   XCB_FOCUS_IN          : constant := 9;
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
   XCB_PROPERTY_NOTIFY   : constant := 28;
   XCB_SELECTION_CLEAR   : constant := 29;
   XCB_SELECTION_REQUEST : constant := 30;
   XCB_SELECTION_NOTIFY  : constant := 31;
   XCB_COLORMAP_NOTIFY   : constant := 32;
   XCB_CLIENT_MESSAGE    : constant := 33;
   XCB_MAPPING_NOTIFY    : constant := 34;
   XCB_GE_GENERIC        : constant := 35;

   XCB_KEY_RELEASE       : constant := 3;
   XCB_BUTTON_RELEASE    : constant := 5;
   XCB_LEAVE_NOTIFY      : constant := 8;
   XCB_FOCUS_OUT         : constant := 10;
   XCB_CIRCULATE_REQUEST : constant := 27;
   XCB_REQUEST           : constant := 1;
   XCB_VALUE             : constant := 2;
   XCB_WINDOW            : constant := 3;
   XCB_PIXMAP            : constant := 4;
   XCB_ATOM              : constant := 5;
   XCB_CURSOR            : constant := 6;
   XCB_FONT              : constant := 7;
   XCB_MATCH             : constant := 8;
   XCB_DRAWABLE          : constant := 9;
   XCB_ACCESS            : constant := 10;
   XCB_ALLOC             : constant := 11;
   XCB_COLORMAP          : constant := 12;
   XCB_GCONTEXT          : constant := 13;
   XCB_IDCHOICE          : constant := 14;
   XCB_NAME              : constant := 15;
   XCB_LENGTH            : constant := 16;
   XCB_IMPLEMENTATION    : constant := 17;

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

   type Visual_Class_Type is new Interfaces.Unsigned_8;
   XCB_VISUAL_CLASS_STATIC_GRAY  : constant Visual_Class_Type := 0;
   XCB_VISUAL_CLASS_GRAY_SCALE   : constant Visual_Class_Type := 1;
   XCB_VISUAL_CLASS_STATIC_COLOR : constant Visual_Class_Type := 2;
   XCB_VISUAL_CLASS_PSEUDO_COLOR : constant Visual_Class_Type := 3;
   XCB_VISUAL_CLASS_TRUE_COLOR   : constant Visual_Class_Type := 4;
   XCB_VISUAL_CLASS_DIRECT_COLOR : constant Visual_Class_Type := 5;

   type Event_Mask_Type is new Interfaces.Unsigned_32;
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

   type Backing_Store_Type is new Interfaces.Unsigned_8;
   XCB_BACKING_STORE_NOT_USEFUL  : constant Backing_Store_Type := 0;
   XCB_BACKING_STORE_WHEN_MAPPED : constant Backing_Store_Type := 1;
   XCB_BACKING_STORE_ALWAYS      : constant Backing_Store_Type := 2;

   type Image_Order_Type is new Interfaces.Unsigned_8;
   XCB_IMAGE_ORDER_LSBFIRST : constant Image_Order_Type := 0;
   XCB_IMAGE_ORDER_MSBFIRST : constant Image_Order_Type := 1;

   type Mod_Mask_Type is new Interfaces.Unsigned_32;
   XCB_MOD_MASK_SHIFT   : constant Mod_Mask_Type := 1;
   XCB_MOD_MASK_LOCK    : constant Mod_Mask_Type := 2;
   XCB_MOD_MASK_CONTROL : constant Mod_Mask_Type := 4;
   XCB_MOD_MASK_1       : constant Mod_Mask_Type := 8;
   XCB_MOD_MASK_2       : constant Mod_Mask_Type := 16;
   XCB_MOD_MASK_3       : constant Mod_Mask_Type := 32;
   XCB_MOD_MASK_4       : constant Mod_Mask_Type := 64;
   XCB_MOD_MASK_5       : constant Mod_Mask_Type := 128;
   XCB_MOD_MASK_ANY     : constant Mod_Mask_Type := 32768;

   type Key_But_Mask_Type is new Interfaces.Unsigned_32;
   XCB_KEY_BUT_MASK_SHIFT    : constant Key_But_Mask_Type := 1;
   XCB_KEY_BUT_MASK_LOCK     : constant Key_But_Mask_Type := 2;
   XCB_KEY_BUT_MASK_CONTROL  : constant Key_But_Mask_Type := 4;
   XCB_KEY_BUT_MASK_MOD_1    : constant Key_But_Mask_Type := 8;
   XCB_KEY_BUT_MASK_MOD_2    : constant Key_But_Mask_Type := 16;
   XCB_KEY_BUT_MASK_MOD_3    : constant Key_But_Mask_Type := 32;
   XCB_KEY_BUT_MASK_MOD_4    : constant Key_But_Mask_Type := 64;
   XCB_KEY_BUT_MASK_MOD_5    : constant Key_But_Mask_Type := 128;
   XCB_KEY_BUT_MASK_BUTTON_1 : constant Key_But_Mask_Type := 256;
   XCB_KEY_BUT_MASK_BUTTON_2 : constant Key_But_Mask_Type := 512;
   XCB_KEY_BUT_MASK_BUTTON_3 : constant Key_But_Mask_Type := 1024;
   XCB_KEY_BUT_MASK_BUTTON_4 : constant Key_But_Mask_Type := 2048;
   XCB_KEY_BUT_MASK_BUTTON_5 : constant Key_But_Mask_Type := 4096;

   type Window_Type is new Interfaces.Unsigned_8;
   XCB_WINDOW_NONE : constant Window_Type := 0;

   type Button_Mask_Type is new Interfaces.Unsigned_32;
   XCB_BUTTON_MASK_1   : constant Button_Mask_Type := 256;
   XCB_BUTTON_MASK_2   : constant Button_Mask_Type := 512;
   XCB_BUTTON_MASK_3   : constant Button_Mask_Type := 1024;
   XCB_BUTTON_MASK_4   : constant Button_Mask_Type := 2048;
   XCB_BUTTON_MASK_5   : constant Button_Mask_Type := 4096;
   XCB_BUTTON_MASK_ANY : constant Button_Mask_Type := 32768;

   type Motion_Type is new Interfaces.Unsigned_8;
   XCB_MOTION_NORMAL : constant Motion_Type := 0;
   XCB_MOTION_HINT   : constant Motion_Type := 1;

   type Notify_Detail_Type is new Interfaces.Unsigned_8;
   XCB_NOTIFY_DETAIL_ANCESTOR          : constant Notify_Detail_Type := 0;
   XCB_NOTIFY_DETAIL_VIRTUAL           : constant Notify_Detail_Type := 1;
   XCB_NOTIFY_DETAIL_INFERIOR          : constant Notify_Detail_Type := 2;
   XCB_NOTIFY_DETAIL_NONLINEAR         : constant Notify_Detail_Type := 3;
   XCB_NOTIFY_DETAIL_NONLINEAR_VIRTUAL : constant Notify_Detail_Type := 4;
   XCB_NOTIFY_DETAIL_POINTER           : constant Notify_Detail_Type := 5;
   XCB_NOTIFY_DETAIL_POINTER_ROOT      : constant Notify_Detail_Type := 6;
   XCB_NOTIFY_DETAIL_NONE              : constant Notify_Detail_Type := 7;

   type Notify_Mode_Type is new Interfaces.Unsigned_8;
   XCB_NOTIFY_MODE_NORMAL        : constant Notify_Mode_Type := 0;
   XCB_NOTIFY_MODE_GRAB          : constant Notify_Mode_Type := 1;
   XCB_NOTIFY_MODE_UNGRAB        : constant Notify_Mode_Type := 2;
   XCB_NOTIFY_MODE_WHILE_GRABBED : constant Notify_Mode_Type := 3;

   type Visibility_Type is new Interfaces.Unsigned_8;
   XCB_VISIBILITY_UNOBSCURED         : constant Visibility_Type := 0;
   XCB_VISIBILITY_PARTIALLY_OBSCURED : constant Visibility_Type := 1;
   XCB_VISIBILITY_FULLY_OBSCURED     : constant Visibility_Type := 2;

   type Place_Type is new Interfaces.Unsigned_8;
   XCB_PLACE_ON_TOP    : constant Place_Type := 0;
   XCB_PLACE_ON_BOTTOM : constant Place_Type := 1;

   type Property_Type is new Interfaces.Unsigned_8;
   XCB_PROPERTY_NEW_VALUE : constant Property_Type := 0;
   XCB_PROPERTY_DELETE    : constant Property_Type := 1;

   type Time_Type is new Interfaces.Unsigned_8;
   XCB_TIME_CURRENT_TIME : constant Time_Type := 0;

   type Atom_Type is new Interfaces.Unsigned_8;
   XCB_ATOM_NONE                : constant Atom_Type := 0;
   XCB_ATOM_ANY                 : constant Atom_Type := 0;
   XCB_ATOM_PRIMARY             : constant Atom_Type := 1;
   XCB_ATOM_SECONDARY           : constant Atom_Type := 2;
   XCB_ATOM_ARC                 : constant Atom_Type := 3;
   XCB_ATOM_ATOM                : constant Atom_Type := 4;
   XCB_ATOM_BITMAP              : constant Atom_Type := 5;
   XCB_ATOM_CARDINAL            : constant Atom_Type := 6;
   XCB_ATOM_COLORMAP            : constant Atom_Type := 7;
   XCB_ATOM_CURSOR              : constant Atom_Type := 8;
   XCB_ATOM_CUT_BUFFER_0        : constant Atom_Type := 9;
   XCB_ATOM_CUT_BUFFER_1        : constant Atom_Type := 10;
   XCB_ATOM_CUT_BUFFER_2        : constant Atom_Type := 11;
   XCB_ATOM_CUT_BUFFER_3        : constant Atom_Type := 12;
   XCB_ATOM_CUT_BUFFER_4        : constant Atom_Type := 13;
   XCB_ATOM_CUT_BUFFER_5        : constant Atom_Type := 14;
   XCB_ATOM_CUT_BUFFER_6        : constant Atom_Type := 15;
   XCB_ATOM_CUT_BUFFER_7        : constant Atom_Type := 16;
   XCB_ATOM_DRAWABLE            : constant Atom_Type := 17;
   XCB_ATOM_FONT                : constant Atom_Type := 18;
   XCB_ATOM_INTEGER             : constant Atom_Type := 19;
   XCB_ATOM_PIXMAP              : constant Atom_Type := 20;
   XCB_ATOM_POINT               : constant Atom_Type := 21;
   XCB_ATOM_RECTANGLE           : constant Atom_Type := 22;
   XCB_ATOM_RESOURCE_MANAGER    : constant Atom_Type := 23;
   XCB_ATOM_RGB_COLOR_MAP       : constant Atom_Type := 24;
   XCB_ATOM_RGB_BEST_MAP        : constant Atom_Type := 25;
   XCB_ATOM_RGB_BLUE_MAP        : constant Atom_Type := 26;
   XCB_ATOM_RGB_DEFAULT_MAP     : constant Atom_Type := 27;
   XCB_ATOM_RGB_GRAY_MAP        : constant Atom_Type := 28;
   XCB_ATOM_RGB_GREEN_MAP       : constant Atom_Type := 29;
   XCB_ATOM_RGB_RED_MAP         : constant Atom_Type := 30;
   XCB_ATOM_STRING              : constant Atom_Type := 31;
   XCB_ATOM_VISUAL_ID           : constant Atom_Type := 32;
   XCB_ATOM_WINDOW              : constant Atom_Type := 33;
   XCB_ATOM_WM_COMMAND          : constant Atom_Type := 34;
   XCB_ATOM_WM_HINTS            : constant Atom_Type := 35;
   XCB_ATOM_WM_CLIENT_MACHINE   : constant Atom_Type := 36;
   XCB_ATOM_WM_ICON_NAME        : constant Atom_Type := 37;
   XCB_ATOM_WM_ICON_SIZE        : constant Atom_Type := 38;
   XCB_ATOM_WM_NAME             : constant Atom_Type := 39;
   XCB_ATOM_WM_NORMAL_HINTS     : constant Atom_Type := 40;
   XCB_ATOM_WM_SIZE_HINTS       : constant Atom_Type := 41;
   XCB_ATOM_WM_ZOOM_HINTS       : constant Atom_Type := 42;
   XCB_ATOM_MIN_SPACE           : constant Atom_Type := 43;
   XCB_ATOM_NORM_SPACE          : constant Atom_Type := 44;
   XCB_ATOM_MAX_SPACE           : constant Atom_Type := 45;
   XCB_ATOM_END_SPACE           : constant Atom_Type := 46;
   XCB_ATOM_SUPERSCRIPT_X       : constant Atom_Type := 47;
   XCB_ATOM_SUPERSCRIPT_Y       : constant Atom_Type := 48;
   XCB_ATOM_SUBSCRIPT_X         : constant Atom_Type := 49;
   XCB_ATOM_SUBSCRIPT_Y         : constant Atom_Type := 50;
   XCB_ATOM_UNDERLINE_POSITION  : constant Atom_Type := 51;
   XCB_ATOM_UNDERLINE_THICKNESS : constant Atom_Type := 52;
   XCB_ATOM_STRIKEOUT_ASCENT    : constant Atom_Type := 53;
   XCB_ATOM_STRIKEOUT_DESCENT   : constant Atom_Type := 54;
   XCB_ATOM_ITALIC_ANGLE        : constant Atom_Type := 55;
   XCB_ATOM_X_HEIGHT            : constant Atom_Type := 56;
   XCB_ATOM_QUAD_WIDTH          : constant Atom_Type := 57;
   XCB_ATOM_WEIGHT              : constant Atom_Type := 58;
   XCB_ATOM_POINT_SIZE          : constant Atom_Type := 59;
   XCB_ATOM_RESOLUTION          : constant Atom_Type := 60;
   XCB_ATOM_COPYRIGHT           : constant Atom_Type := 61;
   XCB_ATOM_NOTICE              : constant Atom_Type := 62;
   XCB_ATOM_FONT_NAME           : constant Atom_Type := 63;
   XCB_ATOM_FAMILY_NAME         : constant Atom_Type := 64;
   XCB_ATOM_FULL_NAME           : constant Atom_Type := 65;
   XCB_ATOM_CAP_HEIGHT          : constant Atom_Type := 66;
   XCB_ATOM_WM_CLASS            : constant Atom_Type := 67;
   XCB_ATOM_WM_TRANSIENT_FOR    : constant Atom_Type := 68;

   type Colormap_State_Type is new Interfaces.Unsigned_8;
   XCB_COLORMAP_STATE_UNINSTALLED : constant Colormap_State_Type := 0;
   XCB_COLORMAP_STATE_INSTALLED   : constant Colormap_State_Type := 1;

   type Colormap_Type is new Interfaces.Unsigned_8;
   XCB_COLORMAP_NONE : constant Colormap_Type := 0;

   type Mapping_Type is new Interfaces.Unsigned_8;
   XCB_MAPPING_MODIFIER : constant Mapping_Type := 0;
   XCB_MAPPING_KEYBOARD : constant Mapping_Type := 1;
   XCB_MAPPING_POINTER  : constant Mapping_Type := 2;

   type Window_Class_Type is new Interfaces.Unsigned_8;
   XCB_WINDOW_CLASS_COPY_FROM_PARENT : constant Window_Class_Type := 0;
   XCB_WINDOW_CLASS_INPUT_OUTPUT     : constant Window_Class_Type := 1;
   XCB_WINDOW_CLASS_INPUT_ONLY       : constant Window_Class_Type := 2;

   type Cw_Type is new Interfaces.Unsigned_32;
   XCB_CW_BACK_PIXMAP       : constant Cw_Type := 1;
   XCB_CW_BACK_PIXEL        : constant Cw_Type := 2;
   XCB_CW_BORDER_PIXMAP     : constant Cw_Type := 4;
   XCB_CW_BORDER_PIXEL      : constant Cw_Type := 8;
   XCB_CW_BIT_GRAVITY       : constant Cw_Type := 16;
   XCB_CW_WIN_GRAVITY       : constant Cw_Type := 32;
   XCB_CW_BACKING_STORE     : constant Cw_Type := 64;
   XCB_CW_BACKING_PLANES    : constant Cw_Type := 128;
   XCB_CW_BACKING_PIXEL     : constant Cw_Type := 256;
   XCB_CW_OVERRIDE_REDIRECT : constant Cw_Type := 512;
   XCB_CW_SAVE_UNDER        : constant Cw_Type := 1024;
   XCB_CW_EVENT_MASK        : constant Cw_Type := 2048;
   XCB_CW_DONT_PROPAGATE    : constant Cw_Type := 4096;
   XCB_CW_COLORMAP          : constant Cw_Type := 8192;
   XCB_CW_CURSOR            : constant Cw_Type := 16384;

   type Back_Pixmap_Type is new Interfaces.Unsigned_8;
   XCB_BACK_PIXMAP_NONE            : constant Back_Pixmap_Type := 0;
   XCB_BACK_PIXMAP_PARENT_RELATIVE : constant Back_Pixmap_Type := 1;

   type Gravity_Type is new Interfaces.Unsigned_8;
   XCB_GRAVITY_BIT_FORGET : constant Gravity_Type := 0;
   XCB_GRAVITY_WIN_UNMAP  : constant Gravity_Type := 0;
   XCB_GRAVITY_NORTH_WEST : constant Gravity_Type := 1;
   XCB_GRAVITY_NORTH      : constant Gravity_Type := 2;
   XCB_GRAVITY_NORTH_EAST : constant Gravity_Type := 3;
   XCB_GRAVITY_WEST       : constant Gravity_Type := 4;
   XCB_GRAVITY_CENTER     : constant Gravity_Type := 5;
   XCB_GRAVITY_EAST       : constant Gravity_Type := 6;
   XCB_GRAVITY_SOUTH_WEST : constant Gravity_Type := 7;
   XCB_GRAVITY_SOUTH      : constant Gravity_Type := 8;
   XCB_GRAVITY_SOUTH_EAST : constant Gravity_Type := 9;
   XCB_GRAVITY_STATIC     : constant Gravity_Type := 10;

   type Map_State_Type is new Interfaces.Unsigned_8;
   XCB_MAP_STATE_UNMAPPED   : constant Map_State_Type := 0;
   XCB_MAP_STATE_UNVIEWABLE : constant Map_State_Type := 1;
   XCB_MAP_STATE_VIEWABLE   : constant Map_State_Type := 2;

   type Set_Mode_Type is new Interfaces.Unsigned_8;
   XCB_SET_MODE_INSERT : constant Set_Mode_Type := 0;
   XCB_SET_MODE_DELETE : constant Set_Mode_Type := 1;

   type Config_Window_Type is new Interfaces.Unsigned_8;
   XCB_CONFIG_WINDOW_X            : constant Config_Window_Type := 1;
   XCB_CONFIG_WINDOW_Y            : constant Config_Window_Type := 2;
   XCB_CONFIG_WINDOW_WIDTH        : constant Config_Window_Type := 4;
   XCB_CONFIG_WINDOW_HEIGHT       : constant Config_Window_Type := 8;
   XCB_CONFIG_WINDOW_BORDER_WIDTH : constant Config_Window_Type := 16;
   XCB_CONFIG_WINDOW_SIBLING      : constant Config_Window_Type := 32;
   XCB_CONFIG_WINDOW_STACK_MODE   : constant Config_Window_Type := 64;

   type Stack_Mode_Type is new Interfaces.Unsigned_8;
   XCB_STACK_MODE_ABOVE     : constant Stack_Mode_Type := 0;
   XCB_STACK_MODE_BELOW     : constant Stack_Mode_Type := 1;
   XCB_STACK_MODE_TOP_IF    : constant Stack_Mode_Type := 2;
   XCB_STACK_MODE_BOTTOM_IF : constant Stack_Mode_Type := 3;
   XCB_STACK_MODE_OPPOSITE  : constant Stack_Mode_Type := 4;

   type Circulate_Type is new Interfaces.Unsigned_8;
   XCB_CIRCULATE_RAISE_LOWEST  : constant Circulate_Type := 0;
   XCB_CIRCULATE_LOWER_HIGHEST : constant Circulate_Type := 1;

   type Prop_Mode_Type is new Interfaces.Unsigned_8;
   XCB_PROP_MODE_REPLACE : constant Prop_Mode_Type := 0;
   XCB_PROP_MODE_PREPEND : constant Prop_Mode_Type := 1;
   XCB_PROP_MODE_APPEND  : constant Prop_Mode_Type := 2;

   type Get_Property_Kind_Type is new Interfaces.Unsigned_8;
   XCB_GET_PROPERTY_KIND_ANY : constant Get_Property_Kind_Type := 0;

   type Send_Event_Dest_Type is new Interfaces.Unsigned_8;
   XCB_SEND_EVENT_DEST_POINTER_WINDOW : constant Send_Event_Dest_Type := 0;
   XCB_SEND_EVENT_DEST_ITEM_FOCUS     : constant Send_Event_Dest_Type := 1;

   type Grab_Mode_Type is new Interfaces.Unsigned_8;
   XCB_GRAB_MODE_SYNC  : constant Grab_Mode_Type := 0;
   XCB_GRAB_MODE_ASYNC : constant Grab_Mode_Type := 1;

   type Grab_Status_Type is new Interfaces.Unsigned_8;
   XCB_GRAB_STATUS_SUCCESS         : constant Grab_Status_Type := 0;
   XCB_GRAB_STATUS_ALREADY_GRABBED : constant Grab_Status_Type := 1;
   XCB_GRAB_STATUS_INVALID_TIME    : constant Grab_Status_Type := 2;
   XCB_GRAB_STATUS_NOT_VIEWABLE    : constant Grab_Status_Type := 3;
   XCB_GRAB_STATUS_FROZEN          : constant Grab_Status_Type := 4;

   type Cursor_Type is new Interfaces.Unsigned_8;
   XCB_CURSOR_NONE : constant Cursor_Type := 0;

   type Button_Index_Type is new Interfaces.Unsigned_8;
   XCB_BUTTON_INDEX_ANY : constant Button_Index_Type := 0;
   XCB_BUTTON_INDEX_1   : constant Button_Index_Type := 1;
   XCB_BUTTON_INDEX_2   : constant Button_Index_Type := 2;
   XCB_BUTTON_INDEX_3   : constant Button_Index_Type := 3;
   XCB_BUTTON_INDEX_4   : constant Button_Index_Type := 4;
   XCB_BUTTON_INDEX_5   : constant Button_Index_Type := 5;

   type Grab_Type is new Interfaces.Unsigned_8;
   XCB_GRAB_ANY : constant Grab_Type := 0;

   type Allow_Type is new Interfaces.Unsigned_8;
   XCB_ALLOW_ASYNC_POINTER   : constant Allow_Type := 0;
   XCB_ALLOW_SYNC_POINTER    : constant Allow_Type := 1;
   XCB_ALLOW_REPLAY_POINTER  : constant Allow_Type := 2;
   XCB_ALLOW_ASYNC_KEYBOARD  : constant Allow_Type := 3;
   XCB_ALLOW_SYNC_KEYBOARD   : constant Allow_Type := 4;
   XCB_ALLOW_REPLAY_KEYBOARD : constant Allow_Type := 5;
   XCB_ALLOW_ASYNC_BOTH      : constant Allow_Type := 6;
   XCB_ALLOW_SYNC_BOTH       : constant Allow_Type := 7;

   type Input_Focus_Type is new Interfaces.Unsigned_8;
   XCB_INPUT_FOCUS_NONE            : constant Input_Focus_Type := 0;
   XCB_INPUT_FOCUS_POINTER_ROOT    : constant Input_Focus_Type := 1;
   XCB_INPUT_FOCUS_PARENT          : constant Input_Focus_Type := 2;
   XCB_INPUT_FOCUS_FOLLOW_KEYBOARD : constant Input_Focus_Type := 3;

   type Font_Draw_Type is new Interfaces.Unsigned_8;
   XCB_FONT_DRAW_LEFT_TO_RIGHT : constant Font_Draw_Type := 0;
   XCB_FONT_DRAW_RIGHT_TO_LEFT : constant Font_Draw_Type := 1;

   type Gc_Type is new Interfaces.Unsigned_32;
   XCB_GC_FUNCTION              : constant Gc_Type := 1;
   XCB_GC_PLANE_MASK            : constant Gc_Type := 2;
   XCB_GC_FOREGROUND            : constant Gc_Type := 4;
   XCB_GC_BACKGROUND            : constant Gc_Type := 8;
   XCB_GC_LINE_WIDTH            : constant Gc_Type := 16;
   XCB_GC_LINE_STYLE            : constant Gc_Type := 32;
   XCB_GC_CAP_STYLE             : constant Gc_Type := 64;
   XCB_GC_JOIN_STYLE            : constant Gc_Type := 128;
   XCB_GC_FILL_STYLE            : constant Gc_Type := 256;
   XCB_GC_FILL_RULE             : constant Gc_Type := 512;
   XCB_GC_TILE                  : constant Gc_Type := 1024;
   XCB_GC_STIPPLE               : constant Gc_Type := 2048;
   XCB_GC_TILE_STIPPLE_ORIGIN_X : constant Gc_Type := 4096;
   XCB_GC_TILE_STIPPLE_ORIGIN_Y : constant Gc_Type := 8192;
   XCB_GC_FONT                  : constant Gc_Type := 16384;
   XCB_GC_SUBWINDOW_MODE        : constant Gc_Type := 32768;
   XCB_GC_GRAPHICS_EXPOSURES    : constant Gc_Type := 65536;
   XCB_GC_CLIP_ORIGIN_X         : constant Gc_Type := 131072;
   XCB_GC_CLIP_ORIGIN_Y         : constant Gc_Type := 262144;
   XCB_GC_CLIP_MASK             : constant Gc_Type := 524288;
   XCB_GC_DASH_OFFSET           : constant Gc_Type := 1048576;
   XCB_GC_DASH_LIST             : constant Gc_Type := 2097152;
   XCB_GC_ARC_MODE              : constant Gc_Type := 4194304;

   type Gx_Type is new Interfaces.Unsigned_8;
   XCB_GX_CLEAR         : constant Gx_Type := 0;
   XCB_GX_AND           : constant Gx_Type := 1;
   XCB_GX_AND_REVERSE   : constant Gx_Type := 2;
   XCB_GX_COPY          : constant Gx_Type := 3;
   XCB_GX_AND_INVERTED  : constant Gx_Type := 4;
   XCB_GX_NOOP          : constant Gx_Type := 5;
   XCB_GX_XOR           : constant Gx_Type := 6;
   XCB_GX_OR            : constant Gx_Type := 7;
   XCB_GX_NOR           : constant Gx_Type := 8;
   XCB_GX_EQUIV         : constant Gx_Type := 9;
   XCB_GX_INVERT        : constant Gx_Type := 10;
   XCB_GX_OR_REVERSE    : constant Gx_Type := 11;
   XCB_GX_COPY_INVERTED : constant Gx_Type := 12;
   XCB_GX_OR_INVERTED   : constant Gx_Type := 13;
   XCB_GX_NAND          : constant Gx_Type := 14;
   XCB_GX_SET           : constant Gx_Type := 15;

   type Line_Style_Type is new Interfaces.Unsigned_8;
   XCB_LINE_STYLE_SOLID       : constant Line_Style_Type := 0;
   XCB_LINE_STYLE_ON_OFF_DASH : constant Line_Style_Type := 1;
   XCB_LINE_STYLE_DOUBLE_DASH : constant Line_Style_Type := 2;

   type Cap_Style_Type is new Interfaces.Unsigned_8;
   XCB_CAP_STYLE_NOT_LAST   : constant Cap_Style_Type := 0;
   XCB_CAP_STYLE_BUTT       : constant Cap_Style_Type := 1;
   XCB_CAP_STYLE_ROUND      : constant Cap_Style_Type := 2;
   XCB_CAP_STYLE_PROJECTING : constant Cap_Style_Type := 3;

   type Join_Style_Type is new Interfaces.Unsigned_8;
   XCB_JOIN_STYLE_MITER : constant Join_Style_Type := 0;
   XCB_JOIN_STYLE_ROUND : constant Join_Style_Type := 1;
   XCB_JOIN_STYLE_BEVEL : constant Join_Style_Type := 2;

   type Fill_Style_Type is new Interfaces.Unsigned_8;
   XCB_FILL_STYLE_SOLID           : constant Fill_Style_Type := 0;
   XCB_FILL_STYLE_TILED           : constant Fill_Style_Type := 1;
   XCB_FILL_STYLE_STIPPLED        : constant Fill_Style_Type := 2;
   XCB_FILL_STYLE_OPAQUE_STIPPLED : constant Fill_Style_Type := 3;

   type Fill_Rule_Type is new Interfaces.Unsigned_8;
   XCB_FILL_RULE_EVEN_ODD : constant Fill_Rule_Type := 0;
   XCB_FILL_RULE_WINDING  : constant Fill_Rule_Type := 1;

   type Subwindow_Mode_Type is new Interfaces.Unsigned_8;
   XCB_SUBWINDOW_MODE_CLIP_BY_CHILDREN  : constant Subwindow_Mode_Type := 0;
   XCB_SUBWINDOW_MODE_INCLUDE_INFERIORS : constant Subwindow_Mode_Type := 1;

   type Arc_Mode_Type is new Interfaces.Unsigned_8;
   XCB_ARC_MODE_CHORD     : constant Arc_Mode_Type := 0;
   XCB_ARC_MODE_PIE_SLICE : constant Arc_Mode_Type := 1;

   type Clip_Ordering_Type is new Interfaces.Unsigned_8;
   XCB_CLIP_ORDERING_UNSORTED : constant Clip_Ordering_Type := 0;
   XCB_CLIP_ORDERING_YSORTED  : constant Clip_Ordering_Type := 1;
   XCB_CLIP_ORDERING_YXSORTED : constant Clip_Ordering_Type := 2;
   XCB_CLIP_ORDERING_YXBANDED : constant Clip_Ordering_Type := 3;

   type Coord_Mode_Type is new Interfaces.Unsigned_8;
   XCB_COORD_MODE_ORIGIN   : constant Coord_Mode_Type := 0;
   XCB_COORD_MODE_PREVIOUS : constant Coord_Mode_Type := 1;

   type Poly_Shape_Type is new Interfaces.Unsigned_8;
   XCB_POLY_SHAPE_COMPLEX   : constant Poly_Shape_Type := 0;
   XCB_POLY_SHAPE_NONCONVEX : constant Poly_Shape_Type := 1;
   XCB_POLY_SHAPE_CONVEX    : constant Poly_Shape_Type := 2;

   type Image_Format_Type is new Interfaces.Unsigned_8;
   XCB_IMAGE_FORMAT_XYBITMAP : constant Image_Format_Type := 0;
   XCB_IMAGE_FORMAT_XYPIXMAP : constant Image_Format_Type := 1;
   XCB_IMAGE_FORMAT_ZPIXMAP  : constant Image_Format_Type := 2;

   type Colormap_Alloc_Type is new Interfaces.Unsigned_8;
   XCB_COLORMAP_ALLOC_NONE : constant Colormap_Alloc_Type := 0;
   XCB_COLORMAP_ALLOC_ALL  : constant Colormap_Alloc_Type := 1;

   type Color_Flag_Type is new Interfaces.Unsigned_8;
   XCB_COLOR_FLAG_RED   : constant Color_Flag_Type := 1;
   XCB_COLOR_FLAG_GREEN : constant Color_Flag_Type := 2;
   XCB_COLOR_FLAG_BLUE  : constant Color_Flag_Type := 4;

   type Pixmap_Type is new Interfaces.Unsigned_8;
   XCB_PIXMAP_NONE : constant Pixmap_Type := 0;

   type Font_Type is new Interfaces.Unsigned_8;
   XCB_FONT_NONE : constant Font_Type := 0;

   type Query_Shape_Of_Type is new Interfaces.Unsigned_8;
   XCB_QUERY_SHAPE_OF_LARGEST_CURSOR  : constant Query_Shape_Of_Type := 0;
   XCB_QUERY_SHAPE_OF_FASTEST_TILE    : constant Query_Shape_Of_Type := 1;
   XCB_QUERY_SHAPE_OF_FASTEST_STIPPLE : constant Query_Shape_Of_Type := 2;

   type Kb_Type is new Interfaces.Unsigned_32;
   XCB_KB_KEY_CLICK_PERCENT : constant Kb_Type := 1;
   XCB_KB_BELL_PERCENT      : constant Kb_Type := 2;
   XCB_KB_BELL_PITCH        : constant Kb_Type := 4;
   XCB_KB_BELL_DURATION     : constant Kb_Type := 8;
   XCB_KB_LED               : constant Kb_Type := 16;
   XCB_KB_LED_MODE          : constant Kb_Type := 32;
   XCB_KB_KEY               : constant Kb_Type := 64;
   XCB_KB_AUTO_REPEAT_MODE  : constant Kb_Type := 128;

   type Led_Mode_Type is new Interfaces.Unsigned_8;
   XCB_LED_MODE_OFF : constant Led_Mode_Type := 0;
   XCB_LED_MODE_ON  : constant Led_Mode_Type := 1;

   type Auto_Repeat_Mode_Type is new Interfaces.Unsigned_8;
   XCB_AUTO_REPEAT_MODE_OFF     : constant Auto_Repeat_Mode_Type := 0;
   XCB_AUTO_REPEAT_MODE_ON      : constant Auto_Repeat_Mode_Type := 1;
   XCB_AUTO_REPEAT_MODE_DEFAULT : constant Auto_Repeat_Mode_Type := 2;

   type Blanking_Type is new Interfaces.Unsigned_8;
   XCB_BLANKING_NOT_PREFERRED : constant Blanking_Type := 0;
   XCB_BLANKING_PREFERRED     : constant Blanking_Type := 1;
   XCB_BLANKING_DEFAULT       : constant Blanking_Type := 2;

   type Exposures_Type is new Interfaces.Unsigned_8;
   XCB_EXPOSURES_NOT_ALLOWED : constant Exposures_Type := 0;
   XCB_EXPOSURES_ALLOWED     : constant Exposures_Type := 1;
   XCB_EXPOSURES_DEFAULT     : constant Exposures_Type := 2;

   type Host_Mode_Type is new Interfaces.Unsigned_8;
   XCB_HOST_MODE_INSERT : constant Host_Mode_Type := 0;
   XCB_HOST_MODE_DELETE : constant Host_Mode_Type := 1;

   type Family_Type is new Interfaces.Unsigned_8;
   XCB_FAMILY_INTERNET           : constant Family_Type := 0;
   XCB_FAMILY_DECNET             : constant Family_Type := 1;
   XCB_FAMILY_CHAOS              : constant Family_Type := 2;
   XCB_FAMILY_SERVER_INTERPRETED : constant Family_Type := 5;
   XCB_FAMILY_INTERNET_6         : constant Family_Type := 6;

   type Access_Control_Type is new Interfaces.Unsigned_8;
   XCB_ACCESS_CONTROL_DISABLE : constant Access_Control_Type := 0;
   XCB_ACCESS_CONTROL_ENABLE  : constant Access_Control_Type := 1;

   type Close_Down_Type is new Interfaces.Unsigned_8;
   XCB_CLOSE_DOWN_DESTROY_ALL      : constant Close_Down_Type := 0;
   XCB_CLOSE_DOWN_RETAIN_PERMANENT : constant Close_Down_Type := 1;
   XCB_CLOSE_DOWN_RETAIN_TEMPORARY : constant Close_Down_Type := 2;

   type Kill_Type is new Interfaces.Unsigned_8;
   XCB_KILL_ALL_TEMPORARY : constant Kill_Type := 0;

   type Screen_Saver_Type is new Interfaces.Unsigned_8;
   XCB_SCREEN_SAVER_RESET  : constant Screen_Saver_Type := 0;
   XCB_SCREEN_SAVER_ACTIVE : constant Screen_Saver_Type := 1;

   type Mapping_Status_Type is new Interfaces.Unsigned_8;
   XCB_MAPPING_STATUS_SUCCESS : constant Mapping_Status_Type := 0;
   XCB_MAPPING_STATUS_BUSY    : constant Mapping_Status_Type := 1;
   XCB_MAPPING_STATUS_FAILURE : constant Mapping_Status_Type := 2;

   type Map_Index_Type is new Interfaces.Unsigned_8;
   XCB_MAP_INDEX_SHIFT   : constant Map_Index_Type := 0;
   XCB_MAP_INDEX_LOCK    : constant Map_Index_Type := 1;
   XCB_MAP_INDEX_CONTROL : constant Map_Index_Type := 2;
   XCB_MAP_INDEX_1       : constant Map_Index_Type := 3;
   XCB_MAP_INDEX_2       : constant Map_Index_Type := 4;
   XCB_MAP_INDEX_3       : constant Map_Index_Type := 5;
   XCB_MAP_INDEX_4       : constant Map_Index_Type := 6;
   XCB_MAP_INDEX_5       : constant Map_Index_Type := 7;

   type Visual_Id_Type is new Interfaces.Unsigned_32;

   type Visual_Id_Access_Type is access all Visual_Id_Type;

   type Visual_Id_Iterator_Type is record
      Data  : Visual_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Id_Iterator_Type);

   type Visual_Id_Iterator_Access_Type is access all Visual_Id_Iterator_Type;

   type Timestamp_Type is new Interfaces.Unsigned_32;

   type Timestamp_Access_Type is access all Timestamp_Type;

   type Timestamp_Iterator_Type is record
      Data  : Timestamp_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Timestamp_Iterator_Type);

   type Timestamp_Iterator_Access_Type is access all Timestamp_Iterator_Type;

   type Keysym_Type is new Interfaces.Unsigned_32;

   type Keysym_Access_Type is access all Keysym_Type;

   type Keysym_Iterator_Type is record
      Data  : Keysym_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Keysym_Iterator_Type);

   type Keysym_Iterator_Access_Type is access all Keysym_Iterator_Type;

   type Keycode_Type is new Interfaces.Unsigned_8;

   type Keycode_Access_Type is access all Keycode_Type;

   type Keycode_Iterator_Type is record
      Data  : Keycode_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Keycode_Iterator_Type);

   type Keycode_Iterator_Access_Type is access all Keycode_Iterator_Type;

   type Button_Id_Type is new Interfaces.Unsigned_8;

   type Button_Id_Access_Type is access all Button_Id_Type;

   type Button_Id_Iterator_Type is record
      Data  : Button_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Button_Id_Iterator_Type);

   type Button_Id_Iterator_Access_Type is access all Button_Id_Iterator_Type;

   -- Identifier for objects in the XCB library. For example Windows,
   -- Graphical Contexts,...
   type X_Id_Type is new Interfaces.Unsigned_32;

   type Drawable_Id_Type is new X_Id_Type;

   type Drawable_Id_Access_Type is access all Drawable_Id_Type;

   type Drawable_Id_Iterator_Type is record
      Data  : Drawable_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Drawable_Id_Iterator_Type);

   type Drawable_Id_Iterator_Access_Type is access all Drawable_Id_Iterator_Type;

   subtype Window_Id_Type is Drawable_Id_Type;

   type Window_Id_Access_Type is access all Window_Id_Type;

   type Window_Id_Iterator_Type is record
      Data  : Window_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Window_Id_Iterator_Type);

   type Window_Id_Iterator_Access_Type is access all Window_Id_Iterator_Type;

   subtype Pixmap_Id_Type is Drawable_Id_Type;

   type Pixmap_Id_Access_Type is access all Pixmap_Id_Type;

   type Pixmap_Id_Iterator_Type is record
      Data  : Pixmap_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Pixmap_Id_Iterator_Type);

   type Pixmap_Id_Iterator_Access_Type is access all Pixmap_Id_Iterator_Type;

   type Fontable_Id_Type is new X_Id_Type;

   type Fontable_Id_Access_Type is access all Fontable_Id_Type;

   type Fontable_Id_Iterator_Type is record
      Data  : Fontable_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Fontable_Id_Iterator_Type);

   type Fontable_Id_Iterator_Access_Type is access all Fontable_Id_Iterator_Type;

   subtype Font_Id_Type is Fontable_Id_Type;

   type Font_Id_Access_Type is access all Font_Id_Type;

   type Font_Id_Iterator_Type is record
      Data  : Font_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Font_Id_Iterator_Type);

   type Font_Id_Iterator_Access_Type is access all Font_Id_Iterator_Type;

   subtype Gcontext_Id_Type is Fontable_Id_Type;

   type Gcontext_Id_Access_Type is access all Gcontext_Id_Type;

   type Gcontext_Id_Iterator_Type is record
      Data  : Gcontext_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Gcontext_Id_Iterator_Type);

   type Gcontext_Id_Iterator_Access_Type is access all Gcontext_Id_Iterator_Type;

   type Cursor_Id_Type is new X_Id_Type;

   type Cursor_Id_Access_Type is access all Cursor_Id_Type;

   type Cursor_Id_Iterator_Type is record
      Data  : Cursor_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Cursor_Id_Iterator_Type);

   type Cursor_Id_Iterator_Access_Type is access all Cursor_Id_Iterator_Type;

   type Colormap_Id_Type is new X_Id_Type;

   type Colormap_Id_Access_Type is access all Colormap_Id_Type;

   type Colormap_Id_Iterator_Type is record
      Data  : Colormap_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Colormap_Id_Iterator_Type);

   type Colormap_Id_Iterator_Access_Type is access all Colormap_Id_Iterator_Type;

   type Atom_Id_Type is new X_Id_Type;

   type Atom_Id_Access_Type is access all Atom_Id_Type;

   type Atom_Id_Iterator_Type is record
      Data  : Atom_Id_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Atom_Id_Iterator_Type);

   type Atom_Id_Iterator_Access_Type is access all Atom_Id_Iterator_Type;

   type Char_2B_Type is record
      Byte_1 : aliased Interfaces.Unsigned_8;
      Byte_2 : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Char_2B_Type);

   type Char_2B_Access_Type is access all Char_2B_Type;

   type Char_2B_Iterator_Type is record
      Data  : Char_2B_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Char_2B_Iterator_Type);

   type Char_2B_Iterator_Access_Type is access all Char_2B_Iterator_Type;

   type Point_Type is record
      X : aliased Interfaces.Integer_16;
      Y : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_Type);

   type Point_Access_Type is access all Point_Type;

   type Point_Iterator_Type is record
      Data  : Point_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Point_Iterator_Type);

   type Point_Iterator_Access_Type is access all Point_Iterator_Type;

   type Rectangle_Type is record
      X      : aliased Interfaces.Integer_16;
      Y      : aliased Interfaces.Integer_16;
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

   type Rectangle_Iterator_Access_Type is access all Rectangle_Iterator_Type;

   type Arc_Type is record
      X       : aliased Interfaces.Integer_16;
      Y       : aliased Interfaces.Integer_16;
      Width   : aliased Interfaces.Unsigned_16;
      Height  : aliased Interfaces.Unsigned_16;
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

   type Arc_Iterator_Access_Type is access all Arc_Iterator_Type;

   type Format_Padding_0_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_8;
   type Format_Type is record
      Depth          : aliased Interfaces.Unsigned_8;
      Bits_Per_Pixel : aliased Interfaces.Unsigned_8;
      Scanline_Pad   : aliased Interfaces.Unsigned_8;
      Padding_0      : aliased Format_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Format_Type);

   type Format_Access_Type is access all Format_Type;

   type Format_Iterator_Type is record
      Data  : Format_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Format_Iterator_Type);

   type Format_Iterator_Access_Type is access all Format_Iterator_Type;

   type Visual_Kind_Padding_0_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Visual_Kind_Type is record
      Visual_Id          : aliased Visual_Id_Type;
      Class              : aliased Interfaces.Unsigned_8;
      Bits_Per_Rgb_Value : aliased Interfaces.Unsigned_8;
      Colormap_Entries   : aliased Interfaces.Unsigned_16;
      Red_Mask           : aliased Interfaces.Unsigned_32;
      Green_Mask         : aliased Interfaces.Unsigned_32;
      Blue_Mask          : aliased Interfaces.Unsigned_32;
      Padding_0          : aliased Visual_Kind_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Kind_Type);

   type Visual_Kind_Access_Type is access all Visual_Kind_Type;

   type Visual_Kind_Iterator_Type is record
      Data  : Visual_Kind_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Visual_Kind_Iterator_Type);

   type Visual_Kind_Iterator_Access_Type is access all Visual_Kind_Iterator_Type;

   type Depth_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Depth_Type is record
      Depth       : aliased Interfaces.Unsigned_8;
      Padding_0   : aliased Interfaces.Unsigned_8;
      Visuals_Len : aliased Interfaces.Unsigned_16;
      Padding_1   : aliased Depth_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Depth_Type);

   type Depth_Access_Type is access all Depth_Type;

   type Depth_Iterator_Type is record
      Data  : Depth_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Depth_Iterator_Type);

   type Depth_Iterator_Access_Type is access all Depth_Iterator_Type;

   type Screen_Type is record
      Root                  : aliased Window_Id_Type;
      Default_Colormap      : aliased Colormap_Id_Type;
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
      Allowed_Depths_Len    : aliased Interfaces.Unsigned_8;
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
      Byte_Order                      : aliased Interfaces.Unsigned_8;
      Padding_0                       : aliased Interfaces.Unsigned_8;
      Protocol_Major_Version          : aliased Interfaces.Unsigned_16;
      Protocol_Minor_Version          : aliased Interfaces.Unsigned_16;
      Authorization_Protocol_Name_Len : aliased Interfaces.Unsigned_16;
      Authorization_Protocol_Data_Len : aliased Interfaces.Unsigned_16;
      Padding_1                       : aliased Setup_Request_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Request_Type);

   type Setup_Request_Access_Type is access all Setup_Request_Type;

   type Setup_Request_Iterator_Type is record
      Data  : Setup_Request_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Request_Iterator_Type);

   type Setup_Request_Iterator_Access_Type is access all Setup_Request_Iterator_Type;

   type Setup_Failed_Type is record
      Status                 : aliased Interfaces.Unsigned_8;
      Reason_Len             : aliased Interfaces.Unsigned_8;
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

   type Setup_Failed_Iterator_Access_Type is access all Setup_Failed_Iterator_Type;

   type Setup_Authenticate_Padding_0_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_8;
   type Setup_Authenticate_Type is record
      Status    : aliased Interfaces.Unsigned_8;
      Padding_0 : aliased Setup_Authenticate_Padding_0_Array_Type;
      Length    : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Authenticate_Type);

   type Setup_Authenticate_Access_Type is access all Setup_Authenticate_Type;

   type Setup_Authenticate_Iterator_Type is record
      Data  : Setup_Authenticate_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Authenticate_Iterator_Type);

   type Setup_Authenticate_Iterator_Access_Type is access all Setup_Authenticate_Iterator_Type;

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
      Vendor_Len                  : aliased Interfaces.Unsigned_16;
      Maximum_Request_Length      : aliased Interfaces.Unsigned_16;
      Roots_Len                   : aliased Interfaces.Unsigned_8;
      Pixmap_Formats_Len          : aliased Interfaces.Unsigned_8;
      Image_Byte_Order            : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Bit_Order     : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Scanline_Unit : aliased Interfaces.Unsigned_8;
      Bitmap_Format_Scanline_Pad  : aliased Interfaces.Unsigned_8;
      Min_Keycode                 : aliased Keycode_Type;
      Max_Keycode                 : aliased Keycode_Type;
      Padding_1                   : aliased Setup_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Type);

   type Setup_Access_Type is access all Setup_Type;

   type Setup_Iterator_Type is record
      Data  : Setup_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Setup_Iterator_Type);

   type Setup_Iterator_Access_Type is access all Setup_Iterator_Type;

   type Time_Coordinate_Type is record
      Time : aliased Timestamp_Type;
      X    : aliased Interfaces.Integer_16;
      Y    : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Time_Coordinate_Type);

   type Time_Coordinate_Access_Type is access all Time_Coordinate_Type;

   type Time_Coordinate_Iterator_Type is record
      Data  : Time_Coordinate_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Time_Coordinate_Iterator_Type);

   type Time_Coordinate_Iterator_Access_Type is access all Time_Coordinate_Iterator_Type;

   type Font_Properties_Type is record
      Name  : aliased Atom_Id_Type;
      Value : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Font_Properties_Type);

   type Font_Properties_Access_Type is access all Font_Properties_Type;

   type Font_Properties_Iterator_Type is record
      Data  : Font_Properties_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Font_Properties_Iterator_Type);

   type Font_Properties_Iterator_Access_Type is access all Font_Properties_Iterator_Type;

   type Character_Information_Type is record
      Left_Side_Bearing  : aliased Interfaces.Integer_16;
      Right_Side_Bearing : aliased Interfaces.Integer_16;
      Character_Width    : aliased Interfaces.Integer_16;
      Ascent             : aliased Interfaces.Integer_16;
      Descent            : aliased Interfaces.Integer_16;
      Attributes         : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Character_Information_Type);

   type Character_Information_Access_Type is access all Character_Information_Type;

   type Character_Information_Iterator_Type is record
      Data  : Character_Information_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Character_Information_Iterator_Type);

   type Character_Information_Iterator_Access_Type is access all Character_Information_Iterator_Type;

   type Str_Type is record
      Name_Len : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Str_Type);

   type Str_Access_Type is access all Str_Type;

   type Str_Iterator_Type is record
      Data  : Str_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Str_Iterator_Type);

   type Str_Iterator_Access_Type is access all Str_Iterator_Type;

   type Segment_Type is record
      X_1 : aliased Interfaces.Integer_16;
      Y_1 : aliased Interfaces.Integer_16;
      X_2 : aliased Interfaces.Integer_16;
      Y_2 : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Segment_Type);

   type Segment_Access_Type is access all Segment_Type;

   type Segment_Iterator_Type is record
      Data  : Segment_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Segment_Iterator_Type);

   type Segment_Iterator_Access_Type is access all Segment_Iterator_Type;

   type Color_Item_Type is record
      Pixel     : aliased Interfaces.Unsigned_32;
      Red       : aliased Interfaces.Unsigned_16;
      Green     : aliased Interfaces.Unsigned_16;
      Blue      : aliased Interfaces.Unsigned_16;
      Flags     : aliased Interfaces.Unsigned_8;
      Padding_0 : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Color_Item_Type);

   type Color_Item_Access_Type is access all Color_Item_Type;

   type Color_Item_Iterator_Type is record
      Data  : Color_Item_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Color_Item_Iterator_Type);

   type Color_Item_Iterator_Access_Type is access all Color_Item_Iterator_Type;

   type Red_Green_Blue_Padding_0_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Red_Green_Blue_Type is record
      Red       : aliased Interfaces.Unsigned_16;
      Green     : aliased Interfaces.Unsigned_16;
      Blue      : aliased Interfaces.Unsigned_16;
      Padding_0 : aliased Red_Green_Blue_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Red_Green_Blue_Type);

   type Red_Green_Blue_Access_Type is access all Red_Green_Blue_Type;

   type Red_Green_Blue_Iterator_Type is record
      Data  : Red_Green_Blue_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Red_Green_Blue_Iterator_Type);

   type Red_Green_Blue_Iterator_Access_Type is access all Red_Green_Blue_Iterator_Type;

   type Host_Type is record
      Family      : aliased Interfaces.Unsigned_8;
      Padding_0   : aliased Interfaces.Unsigned_8;
      Address_Len : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Host_Type);

   type Host_Access_Type is access all Host_Type;

   type Host_Iterator_Type is record
      Data  : Host_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Host_Iterator_Type);

   type Host_Iterator_Access_Type is access all Host_Iterator_Type;

   type Char_2B_Array_Type is array (Natural range <>) of Char_2B_Type;
   pragma Convention (C, Char_2B_Array_Type);

   type Rectangle_Array_Type is array (Natural range <>) of Rectangle_Type;
   pragma Convention (C, Rectangle_Array_Type);

   type Point_Array_Type is array (Natural range <>) of Point_Type;
   pragma Convention (C, Point_Array_Type);

   type Segment_Array_Type is array (Natural range <>) of Segment_Type;
   pragma Convention (C, Segment_Array_Type);

   type Arc_Array_Type is array (Natural range <>) of Arc_Type;
   pragma Convention (C, Arc_Array_Type);

   type Byte_Array_Type is array (Natural range <>) of Interfaces.Unsigned_8;
   pragma Convention (C, Byte_Array_Type);

   type Unsigned_32_Array_Type is array (Natural range <>) of Interfaces.Unsigned_32;
   pragma Convention (C, Unsigned_32_Array_Type);

   type Color_Item_Array_Type is array (Natural range <>) of Color_Item_Type;
   pragma Convention (C, Color_Item_Array_Type);

   type Client_Message_Data_Data_8_Array_Type is array (0 .. 19) of aliased Interfaces.Unsigned_8;
   type Client_Message_Data_Data_16_Array_Type is array (0 .. 9) of aliased Interfaces.Unsigned_16;
   type Client_Message_Data_Data_32_Array_Type is array (0 .. 4) of aliased Interfaces.Unsigned_32;
   type Client_Message_Data_Type (Discriminant : Natural := 0) is record
      case Discriminant is
         when 0 =>
            Data_8 : aliased Client_Message_Data_Data_8_Array_Type;
         when 1 =>
            Data_16 : aliased Client_Message_Data_Data_16_Array_Type;
         when others =>
            Data_32 : aliased Client_Message_Data_Data_32_Array_Type;
      end case;
   end record;
   pragma Convention (C_Pass_By_Copy, Client_Message_Data_Type);
   pragma Unchecked_Union (Client_Message_Data_Type);

   type Client_Message_Data_Access_Type is access all Client_Message_Data_Type;

   type Client_Message_Data_Iterator_Type is record
      Data  : Client_Message_Data_Access_Type;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Client_Message_Data_Iterator_Type);

   type Client_Message_Data_Iterator_Access_Type is access all Client_Message_Data_Iterator_Type;

   type Key_Press_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Keycode_Type;
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

   type Motion_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Motion_Type;
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
      Detail            : aliased Notify_Detail_Type;
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

   type Focus_In_Event_Padding_0_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Focus_In_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Detail        : aliased Notify_Detail_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Event         : aliased Window_Id_Type;
      Mode          : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Focus_In_Event_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Focus_In_Event_Type);

   type Focus_In_Event_Access_Type is access all Focus_In_Event_Type;

   type Keymap_Notify_Event_Keys_Array_Type is array (0 .. 30) of aliased Interfaces.Unsigned_8;
   type Keymap_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Keys          : aliased Keymap_Notify_Event_Keys_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Keymap_Notify_Event_Type);

   type Keymap_Notify_Event_Access_Type is access all Keymap_Notify_Event_Type;

   type Expose_Event_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Expose_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Window        : aliased Window_Id_Type;
      X             : aliased Interfaces.Unsigned_16;
      Y             : aliased Interfaces.Unsigned_16;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
      Count         : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Expose_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Expose_Event_Type);

   type Expose_Event_Access_Type is access all Expose_Event_Type;

   type Graphics_Exposure_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Graphics_Exposure_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Drawable      : aliased Drawable_Id_Type;
      X             : aliased Interfaces.Unsigned_16;
      Y             : aliased Interfaces.Unsigned_16;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
      Minor_Opcode  : aliased Interfaces.Unsigned_16;
      Count         : aliased Interfaces.Unsigned_16;
      Major_Opcode  : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Graphics_Exposure_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Graphics_Exposure_Event_Type);

   type Graphics_Exposure_Event_Access_Type is access all Graphics_Exposure_Event_Type;

   type No_Exposure_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Drawable      : aliased Drawable_Id_Type;
      Minor_Opcode  : aliased Interfaces.Unsigned_16;
      Major_Opcode  : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, No_Exposure_Event_Type);

   type No_Exposure_Event_Access_Type is access all No_Exposure_Event_Type;

   type Visibility_Notify_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Visibility_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Window        : aliased Window_Id_Type;
      State         : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Visibility_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Visibility_Notify_Event_Type);

   type Visibility_Notify_Event_Access_Type is access all Visibility_Notify_Event_Type;

   type Create_Notify_Event_Type is record
      Response_Kind     : aliased Interfaces.Unsigned_8;
      Padding_0         : aliased Interfaces.Unsigned_8;
      Parent            : aliased Window_Id_Type;
      Window            : aliased Window_Id_Type;
      X                 : aliased Interfaces.Integer_16;
      Y                 : aliased Interfaces.Integer_16;
      Width             : aliased Interfaces.Unsigned_16;
      Height            : aliased Interfaces.Unsigned_16;
      Border_Width      : aliased Interfaces.Unsigned_16;
      Override_Redirect : aliased Interfaces.Unsigned_8;
      Padding_1         : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Create_Notify_Event_Type);

   type Create_Notify_Event_Access_Type is access all Create_Notify_Event_Type;

   type Destroy_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Event         : aliased Window_Id_Type;
      Window        : aliased Window_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Destroy_Notify_Event_Type);

   type Destroy_Notify_Event_Access_Type is access all Destroy_Notify_Event_Type;

   type Unmap_Notify_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Unmap_Notify_Event_Type is record
      Response_Kind  : aliased Interfaces.Unsigned_8;
      Padding_0      : aliased Interfaces.Unsigned_8;
      Event          : aliased Window_Id_Type;
      Window         : aliased Window_Id_Type;
      From_Configure : aliased Interfaces.Unsigned_8;
      Padding_1      : aliased Unmap_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Unmap_Notify_Event_Type);

   type Unmap_Notify_Event_Access_Type is access all Unmap_Notify_Event_Type;

   type Map_Notify_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Map_Notify_Event_Type is record
      Response_Kind     : aliased Interfaces.Unsigned_8;
      Padding_0         : aliased Interfaces.Unsigned_8;
      Event             : aliased Window_Id_Type;
      Window            : aliased Window_Id_Type;
      Override_Redirect : aliased Interfaces.Unsigned_8;
      Padding_1         : aliased Map_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Map_Notify_Event_Type);

   type Map_Notify_Event_Access_Type is access all Map_Notify_Event_Type;

   type Map_Request_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Parent        : aliased Window_Id_Type;
      Window        : aliased Window_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Map_Request_Event_Type);

   type Map_Request_Event_Access_Type is access all Map_Request_Event_Type;

   type Reparent_Notify_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Reparent_Notify_Event_Type is record
      Response_Kind     : aliased Interfaces.Unsigned_8;
      Padding_0         : aliased Interfaces.Unsigned_8;
      Event             : aliased Window_Id_Type;
      Window            : aliased Window_Id_Type;
      Parent            : aliased Window_Id_Type;
      X                 : aliased Interfaces.Integer_16;
      Y                 : aliased Interfaces.Integer_16;
      Override_Redirect : aliased Interfaces.Unsigned_8;
      Padding_1         : aliased Reparent_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Reparent_Notify_Event_Type);

   type Reparent_Notify_Event_Access_Type is access all Reparent_Notify_Event_Type;

   type Configure_Notify_Event_Type is record
      Response_Kind     : aliased Interfaces.Unsigned_8;
      Padding_0         : aliased Interfaces.Unsigned_8;
      Event             : aliased Window_Id_Type;
      Window            : aliased Window_Id_Type;
      Above_Sibling     : aliased Window_Id_Type;
      X                 : aliased Interfaces.Integer_16;
      Y                 : aliased Interfaces.Integer_16;
      Width             : aliased Interfaces.Unsigned_16;
      Height            : aliased Interfaces.Unsigned_16;
      Border_Width      : aliased Interfaces.Unsigned_16;
      Override_Redirect : aliased Interfaces.Unsigned_8;
      Padding_1         : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Configure_Notify_Event_Type);

   type Configure_Notify_Event_Access_Type is access all Configure_Notify_Event_Type;

   type Configure_Request_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Stack_Mode    : aliased Stack_Mode_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Parent        : aliased Window_Id_Type;
      Window        : aliased Window_Id_Type;
      Sibling       : aliased Window_Id_Type;
      X             : aliased Interfaces.Integer_16;
      Y             : aliased Interfaces.Integer_16;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
      Border_Width  : aliased Interfaces.Unsigned_16;
      Value_Mask    : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Configure_Request_Event_Type);

   type Configure_Request_Event_Access_Type is access all Configure_Request_Event_Type;

   type Gravity_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Event         : aliased Window_Id_Type;
      Window        : aliased Window_Id_Type;
      X             : aliased Interfaces.Integer_16;
      Y             : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Gravity_Notify_Event_Type);

   type Gravity_Notify_Event_Access_Type is access all Gravity_Notify_Event_Type;

   type Resize_Request_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Window        : aliased Window_Id_Type;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Resize_Request_Event_Type);

   type Resize_Request_Event_Access_Type is access all Resize_Request_Event_Type;

   type Circulate_Notify_Event_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Circulate_Notify_Event_Padding_2_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Circulate_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Event         : aliased Window_Id_Type;
      Window        : aliased Window_Id_Type;
      Padding_1     : aliased Circulate_Notify_Event_Padding_1_Array_Type;
      Place         : aliased Interfaces.Unsigned_8;
      Padding_2     : aliased Circulate_Notify_Event_Padding_2_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Circulate_Notify_Event_Type);

   type Circulate_Notify_Event_Access_Type is access all Circulate_Notify_Event_Type;

   type Property_Notify_Event_Padding_1_Array_Type is array (0 .. 2) of aliased Interfaces.Unsigned_8;
   type Property_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Window        : aliased Window_Id_Type;
      Atom          : aliased Atom_Id_Type;
      Time          : aliased Timestamp_Type;
      State         : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Property_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Property_Notify_Event_Type);

   type Property_Notify_Event_Access_Type is access all Property_Notify_Event_Type;

   type Selection_Clear_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Time          : aliased Timestamp_Type;
      Owner         : aliased Window_Id_Type;
      Selection     : aliased Atom_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Selection_Clear_Event_Type);

   type Selection_Clear_Event_Access_Type is access all Selection_Clear_Event_Type;

   type Selection_Request_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Time          : aliased Timestamp_Type;
      Owner         : aliased Window_Id_Type;
      Requestor     : aliased Window_Id_Type;
      Selection     : aliased Atom_Id_Type;
      Target        : aliased Atom_Id_Type;
      Property      : aliased Atom_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Selection_Request_Event_Type);

   type Selection_Request_Event_Access_Type is access all Selection_Request_Event_Type;

   type Selection_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Time          : aliased Timestamp_Type;
      Requestor     : aliased Window_Id_Type;
      Selection     : aliased Atom_Id_Type;
      Target        : aliased Atom_Id_Type;
      Property      : aliased Atom_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Selection_Notify_Event_Type);

   type Selection_Notify_Event_Access_Type is access all Selection_Notify_Event_Type;

   type Colormap_Notify_Event_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Colormap_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Window        : aliased Window_Id_Type;
      Colormap      : aliased Colormap_Id_Type;
      U_New         : aliased Interfaces.Unsigned_8;
      State         : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Colormap_Notify_Event_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Colormap_Notify_Event_Type);

   type Colormap_Notify_Event_Access_Type is access all Colormap_Notify_Event_Type;

   type Client_Message_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Format        : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Window        : aliased Window_Id_Type;
      Kind          : aliased Atom_Id_Type;
      Data          : aliased Client_Message_Data_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Client_Message_Event_Type);

   type Client_Message_Event_Access_Type is access all Client_Message_Event_Type;

   type Mapping_Notify_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Request       : aliased Interfaces.Unsigned_8;
      First_Keycode : aliased Keycode_Type;
      Count         : aliased Interfaces.Unsigned_8;
      Padding_1     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Mapping_Notify_Event_Type);

   type Mapping_Notify_Event_Access_Type is access all Mapping_Notify_Event_Type;

   type Ge_Generic_Event_Padding_0_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type Ge_Generic_Event_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Ge_Generic_Event_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Ge_Generic_Event_Type);

   type Ge_Generic_Event_Access_Type is access all Ge_Generic_Event_Type;

   type Key_Release_Event_Type is new Key_Press_Event_Type;

   type Key_Release_Event_Access_Type is access all Key_Release_Event_Type;

   type Button_Release_Event_Type is new Button_Press_Event_Type;

   type Button_Release_Event_Access_Type is access all Button_Release_Event_Type;

   type Leave_Notify_Event_Type is new Enter_Notify_Event_Type;

   type Leave_Notify_Event_Access_Type is access all Leave_Notify_Event_Type;

   type Focus_Out_Event_Type is new Focus_In_Event_Type;

   type Focus_Out_Event_Access_Type is access all Focus_Out_Event_Type;

   type Circulate_Request_Event_Type is new Circulate_Notify_Event_Type;

   type Circulate_Request_Event_Access_Type is access all Circulate_Request_Event_Type;

   type Request_Error_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Error_Code    : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Bad_Value     : aliased Interfaces.Unsigned_32;
      Minor_Opcode  : aliased Interfaces.Unsigned_16;
      Major_Opcode  : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Request_Error_Type);

   type Value_Error_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Error_Code    : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Bad_Value     : aliased Interfaces.Unsigned_32;
      Minor_Opcode  : aliased Interfaces.Unsigned_16;
      Major_Opcode  : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Value_Error_Type);

   type Window_Error_Type is new Value_Error_Type;

   type Pixmap_Error_Type is new Value_Error_Type;

   type Atom_Error_Type is new Value_Error_Type;

   type Cursor_Error_Type is new Value_Error_Type;

   type Font_Error_Type is new Value_Error_Type;

   type Match_Error_Type is new Request_Error_Type;

   type Drawable_Error_Type is new Value_Error_Type;

   type Access_Error_Type is new Request_Error_Type;

   type Alloc_Error_Type is new Request_Error_Type;

   type Colormap_Error_Type is new Value_Error_Type;

   type Gcontext_Error_Type is new Value_Error_Type;

   type Idchoice_Error_Type is new Value_Error_Type;

   type Name_Error_Type is new Request_Error_Type;

   type Length_Error_Type is new Request_Error_Type;

   type Implementation_Error_Type is new Request_Error_Type;

   type Generic_Iterator_Type is record
      Data  : System.Address;
      C_Rem : aliased Interfaces.C.int;
      Index : aliased Interfaces.C.int;
   end record;
   pragma Convention (C_Pass_By_Copy, Generic_Iterator_Type);

   -- Opaque structure containing all data that XCB needs in order
   -- to communicate with an X server.
   type Connection_Type is limited null record;

   type Connection_Access_Type is access all Connection_Type;
   for Connection_Access_Type'Storage_Size use 0;
   pragma Convention (C, Connection_Access_Type);

   type Value_List_Array is array (Natural range <>) of Interfaces.Unsigned_32;
   pragma Convention (C, Value_List_Array);

   type Void_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Void_Cookie_Type);

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

   type Setup_Constant_Access_Type is access constant Setup_Type;

   function Request_Check
     (C      : Connection_Access_Type;
      Cookie : Void_Cookie_Type) return Generic_Error_Access_Type;
   pragma Import (C, Request_Check, "xcb_request_check");

   function To_Expose_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Expose_Event_Access_Type);

   function To_Button_Press_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Button_Press_Event_Access_Type);

   function To_Button_Release_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Button_Release_Event_Access_Type);

   function To_Motion_Notify_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Motion_Notify_Event_Access_Type);

   function To_Enter_Notify_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Enter_Notify_Event_Access_Type);

   function To_Leave_Notify_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Leave_Notify_Event_Access_Type);

   function To_Key_Press_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Key_Press_Event_Access_Type);

   function To_Key_Release_Event is new Ada.Unchecked_Conversion
     (Source => Generic_Event_Access_Type,
      Target => Key_Release_Event_Access_Type);

   -- Connects to the X server specified by displayname and
   -- returns a newly allocated xcb_connection_t structure.
   -- If displayname is NULL, uses the value of the DISPLAY environment
   -- variable. If a particular screen on that server is preferred, the
   -- int pointed to by @p screenp (if not @c NULL) will be set to that
   -- screen; otherwise the screen will be set to 0.
   function Connect
     (Display_Name   : Interfaces.C.Strings.chars_ptr;
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

   procedure Free is new Ada.Unchecked_Deallocation
     (Object => Generic_Event_Type,
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

   function Setup_Roots_Iterator (R : Setup_Constant_Access_Type) return Screen_Iterator_Type;
   pragma Import (C, Setup_Roots_Iterator, "xcb_setup_roots_iterator");

   function Generate_Id (C : Connection_Access_Type) return Fontable_Id_Type;

   function Generate_Id (C : Connection_Access_Type) return Drawable_Id_Type;

   procedure Char_2B_Next (I : Char_2B_Iterator_Access_Type);
   pragma Import (C, Char_2B_Next, "xcb_char2b_next");

   function Char_2B_End (I : Char_2B_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Char_2B_End, "xcb_char2b_end");

   procedure Point_Next (I : Point_Iterator_Access_Type);
   pragma Import (C, Point_Next, "xcb_point_next");

   function Point_End (I : Point_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Point_End, "xcb_point_end");

   procedure Rectangle_Next (I : Rectangle_Iterator_Access_Type);
   pragma Import (C, Rectangle_Next, "xcb_rectangle_next");

   function Rectangle_End (I : Rectangle_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Rectangle_End, "xcb_rectangle_end");

   procedure Arc_Next (I : Arc_Iterator_Access_Type);
   pragma Import (C, Arc_Next, "xcb_arc_next");

   function Arc_End (I : Arc_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Arc_End, "xcb_arc_end");

   procedure Format_Next (I : Format_Iterator_Access_Type);
   pragma Import (C, Format_Next, "xcb_format_next");

   function Format_End (I : Format_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Format_End, "xcb_format_end");

   procedure Visual_Kind_Next (I : Visual_Kind_Iterator_Access_Type);
   pragma Import (C, Visual_Kind_Next, "xcb_visualtype_next");

   function Visual_Kind_End (I : Visual_Kind_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Visual_Kind_End, "xcb_visualtype_end");

   procedure Depth_Next (I : Depth_Iterator_Access_Type);
   pragma Import (C, Depth_Next, "xcb_depth_next");

   function Depth_End (I : Depth_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Depth_End, "xcb_depth_end");

   procedure Screen_Next (I : Screen_Iterator_Access_Type);
   pragma Import (C, Screen_Next, "xcb_screen_next");

   function Screen_End (I : Screen_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Screen_End, "xcb_screen_end");

   procedure Setup_Request_Next (I : Setup_Request_Iterator_Access_Type);
   pragma Import (C, Setup_Request_Next, "xcb_setuprequest_next");

   function Setup_Request_End (I : Setup_Request_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Setup_Request_End, "xcb_setuprequest_end");

   procedure Setup_Failed_Next (I : Setup_Failed_Iterator_Access_Type);
   pragma Import (C, Setup_Failed_Next, "xcb_setupfailed_next");

   function Setup_Failed_End (I : Setup_Failed_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Setup_Failed_End, "xcb_setupfailed_end");

   procedure Setup_Authenticate_Next (I : Setup_Authenticate_Iterator_Access_Type);
   pragma Import (C, Setup_Authenticate_Next, "xcb_setupauthenticate_next");

   function Setup_Authenticate_End (I : Setup_Authenticate_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Setup_Authenticate_End, "xcb_setupauthenticate_end");

   procedure Setup_Next (I : Setup_Iterator_Access_Type);
   pragma Import (C, Setup_Next, "xcb_setup_next");

   function Setup_End (I : Setup_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Setup_End, "xcb_setup_end");

   procedure Time_Coordinate_Next (I : Time_Coordinate_Iterator_Access_Type);
   pragma Import (C, Time_Coordinate_Next, "xcb_timecoord_next");

   function Time_Coordinate_End (I : Time_Coordinate_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Time_Coordinate_End, "xcb_timecoord_end");

   procedure Font_Properties_Next (I : Font_Properties_Iterator_Access_Type);
   pragma Import (C, Font_Properties_Next, "xcb_fontprop_next");

   function Font_Properties_End (I : Font_Properties_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Font_Properties_End, "xcb_fontprop_end");

   procedure Character_Information_Next (I : Character_Information_Iterator_Access_Type);
   pragma Import (C, Character_Information_Next, "xcb_charinfo_next");

   function Character_Information_End (I : Character_Information_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Character_Information_End, "xcb_charinfo_end");

   procedure Str_Next (I : Str_Iterator_Access_Type);
   pragma Import (C, Str_Next, "xcb_str_next");

   function Str_End (I : Str_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Str_End, "xcb_str_end");

   procedure Segment_Next (I : Segment_Iterator_Access_Type);
   pragma Import (C, Segment_Next, "xcb_segment_next");

   function Segment_End (I : Segment_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Segment_End, "xcb_segment_end");

   procedure Color_Item_Next (I : Color_Item_Iterator_Access_Type);
   pragma Import (C, Color_Item_Next, "xcb_coloritem_next");

   function Color_Item_End (I : Color_Item_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Color_Item_End, "xcb_coloritem_end");

   procedure Red_Green_Blue_Next (I : Red_Green_Blue_Iterator_Access_Type);
   pragma Import (C, Red_Green_Blue_Next, "xcb_rgb_next");

   function Red_Green_Blue_End (I : Red_Green_Blue_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Red_Green_Blue_End, "xcb_rgb_end");

   procedure Host_Next (I : Host_Iterator_Access_Type);
   pragma Import (C, Host_Next, "xcb_host_next");

   function Host_End (I : Host_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Host_End, "xcb_host_end");

   procedure Window_Id_Next (I : Window_Id_Iterator_Access_Type);
   pragma Import (C, Window_Id_Next, "xcb_window_next");

   function Window_Id_End (I : Window_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Window_Id_End, "xcb_window_end");

   procedure Pixmap_Id_Next (I : Pixmap_Id_Iterator_Access_Type);
   pragma Import (C, Pixmap_Id_Next, "xcb_pixmap_next");

   function Pixmap_Id_End (I : Pixmap_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Pixmap_Id_End, "xcb_pixmap_end");

   procedure Cursor_Id_Next (I : Cursor_Id_Iterator_Access_Type);
   pragma Import (C, Cursor_Id_Next, "xcb_cursor_next");

   function Cursor_Id_End (I : Cursor_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Cursor_Id_End, "xcb_cursor_end");

   procedure Font_Id_Next (I : Font_Id_Iterator_Access_Type);
   pragma Import (C, Font_Id_Next, "xcb_font_next");

   function Font_Id_End (I : Font_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Font_Id_End, "xcb_font_end");

   procedure Gcontext_Id_Next (I : Gcontext_Id_Iterator_Access_Type);
   pragma Import (C, Gcontext_Id_Next, "xcb_gcontext_next");

   function Gcontext_Id_End (I : Gcontext_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Gcontext_Id_End, "xcb_gcontext_end");

   procedure Colormap_Id_Next (I : Colormap_Id_Iterator_Access_Type);
   pragma Import (C, Colormap_Id_Next, "xcb_colormap_next");

   function Colormap_Id_End (I : Colormap_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Colormap_Id_End, "xcb_colormap_end");

   procedure Atom_Id_Next (I : Atom_Id_Iterator_Access_Type);
   pragma Import (C, Atom_Id_Next, "xcb_atom_next");

   function Atom_Id_End (I : Atom_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Atom_Id_End, "xcb_atom_end");

   procedure Visual_Id_Next (I : Visual_Id_Iterator_Access_Type);
   pragma Import (C, Visual_Id_Next, "xcb_visualid_next");

   function Visual_Id_End (I : Visual_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Visual_Id_End, "xcb_visualid_end");

   procedure Timestamp_Next (I : Timestamp_Iterator_Access_Type);
   pragma Import (C, Timestamp_Next, "xcb_timestamp_next");

   function Timestamp_End (I : Timestamp_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Timestamp_End, "xcb_timestamp_end");

   procedure Keysym_Next (I : Keysym_Iterator_Access_Type);
   pragma Import (C, Keysym_Next, "xcb_keysym_next");

   function Keysym_End (I : Keysym_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Keysym_End, "xcb_keysym_end");

   procedure Keycode_Next (I : Keycode_Iterator_Access_Type);
   pragma Import (C, Keycode_Next, "xcb_keycode_next");

   function Keycode_End (I : Keycode_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Keycode_End, "xcb_keycode_end");

   procedure Button_Id_Next (I : Button_Id_Iterator_Access_Type);
   pragma Import (C, Button_Id_Next, "xcb_button_next");

   function Button_Id_End (I : Button_Id_Iterator_Type) return Generic_Iterator_Type;
   pragma Import (C, Button_Id_End, "xcb_button_end");

   function Create_Window_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Create_Window_Size_Of, "xcb_create_window_sizeof");

   function Create_Window_Checked
     (C            : Connection_Access_Type;
      Depth        : Interfaces.Unsigned_8;
      Wid          : Window_Id_Type;
      Parent       : Window_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16;
      Border_Width : Interfaces.Unsigned_16;
      Class        : Interfaces.Unsigned_16;
      Visual       : Visual_Id_Type;
      Value_Mask   : Interfaces.Unsigned_32;
      Value_List   : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Window_Checked, "xcb_create_window_checked");

   function Create_Window
     (C            : Connection_Access_Type;
      Depth        : Interfaces.Unsigned_8;
      Wid          : Window_Id_Type;
      Parent       : Window_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16;
      Border_Width : Interfaces.Unsigned_16;
      Class        : Interfaces.Unsigned_16;
      Visual       : Visual_Id_Type;
      Value_Mask   : Interfaces.Unsigned_32;
      Value_List   : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Window, "xcb_create_window");

   function Change_Window_Attributes_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Window_Attributes_Size_Of, "xcb_change_window_attributes_sizeof");

   function Change_Window_Attributes_Checked
     (C          : Connection_Access_Type;
      Window     : Window_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Window_Attributes_Checked, "xcb_change_window_attributes_checked");

   function Change_Window_Attributes
     (C          : Connection_Access_Type;
      Window     : Window_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Window_Attributes, "xcb_change_window_attributes");

   type Get_Window_Attributes_Reply_Padding_0_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Get_Window_Attributes_Reply_Type is record
      Response_Kind         : aliased Interfaces.Unsigned_8;
      Backing_Store         : aliased Backing_Store_Type;
      Sequence              : aliased Interfaces.Unsigned_16;
      Length                : aliased Interfaces.Unsigned_32;
      Visual                : aliased Visual_Id_Type;
      Class                 : aliased Interfaces.Unsigned_16;
      Bit_Gravity           : aliased Interfaces.Unsigned_8;
      Win_Gravity           : aliased Interfaces.Unsigned_8;
      Backing_Planes        : aliased Interfaces.Unsigned_32;
      Backing_Pixel         : aliased Interfaces.Unsigned_32;
      Save_Under            : aliased Interfaces.Unsigned_8;
      Map_Is_Installed      : aliased Interfaces.Unsigned_8;
      Map_State             : aliased Interfaces.Unsigned_8;
      Override_Redirect     : aliased Interfaces.Unsigned_8;
      Colormap              : aliased Colormap_Id_Type;
      All_Event_Masks       : aliased Interfaces.Unsigned_32;
      Your_Event_Mask       : aliased Interfaces.Unsigned_32;
      Do_Not_Propagate_Mask : aliased Interfaces.Unsigned_16;
      Padding_0             : aliased Get_Window_Attributes_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Window_Attributes_Reply_Type);

   type Get_Window_Attributes_Reply_Access_Type is access all Get_Window_Attributes_Reply_Type;
   for Get_Window_Attributes_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Window_Attributes_Reply_Access_Type);

   type Get_Window_Attributes_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Window_Attributes_Cookie_Type);

   function Get_Window_Attributes_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Window_Attributes_Cookie_Type;
      Error  : System.Address) return Get_Window_Attributes_Reply_Access_Type;
   pragma Import (C, Get_Window_Attributes_Reply, "xcb_get_window_attributes_reply");

   function Get_Window_Attributes_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Get_Window_Attributes_Cookie_Type;
   pragma Import (C, Get_Window_Attributes_Unchecked, "xcb_get_window_attributes_unchecked");

   function Get_Window_Attributes
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Get_Window_Attributes_Cookie_Type;
   pragma Import (C, Get_Window_Attributes, "xcb_get_window_attributes");

   function Destroy_Window_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Destroy_Window_Checked, "xcb_destroy_window_checked");

   function Destroy_Window (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Destroy_Window, "xcb_destroy_window");

   function Destroy_Subwindows_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Destroy_Subwindows_Checked, "xcb_destroy_subwindows_checked");

   function Destroy_Subwindows (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Destroy_Subwindows, "xcb_destroy_subwindows");

   function Change_Save_Set_Checked
     (C      : Connection_Access_Type;
      Mode   : Interfaces.Unsigned_8;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Save_Set_Checked, "xcb_change_save_set_checked");

   function Change_Save_Set
     (C      : Connection_Access_Type;
      Mode   : Interfaces.Unsigned_8;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Save_Set, "xcb_change_save_set");

   function Reparent_Window_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type;
      Parent : Window_Id_Type;
      X      : Interfaces.Integer_16;
      Y      : Interfaces.Integer_16) return Void_Cookie_Type;
   pragma Import (C, Reparent_Window_Checked, "xcb_reparent_window_checked");

   function Reparent_Window
     (C      : Connection_Access_Type;
      Window : Window_Id_Type;
      Parent : Window_Id_Type;
      X      : Interfaces.Integer_16;
      Y      : Interfaces.Integer_16) return Void_Cookie_Type;
   pragma Import (C, Reparent_Window, "xcb_reparent_window");

   function Map_Window_Checked (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Window_Checked, "xcb_map_window_checked");

   function Map_Window (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Window, "xcb_map_window");

   function Map_Subwindows_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Subwindows_Checked, "xcb_map_subwindows_checked");

   function Map_Subwindows (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Map_Subwindows, "xcb_map_subwindows");

   function Unmap_Window_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Unmap_Window_Checked, "xcb_unmap_window_checked");

   function Unmap_Window (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Unmap_Window, "xcb_unmap_window");

   function Unmap_Subwindows_Checked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Unmap_Subwindows_Checked, "xcb_unmap_subwindows_checked");

   function Unmap_Subwindows (C : Connection_Access_Type; Window : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Unmap_Subwindows, "xcb_unmap_subwindows");

   function Configure_Window_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Configure_Window_Size_Of, "xcb_configure_window_sizeof");

   function Configure_Window_Checked
     (C          : Connection_Access_Type;
      Window     : Window_Id_Type;
      Value_Mask : Interfaces.Unsigned_16;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Configure_Window_Checked, "xcb_configure_window_checked");

   function Configure_Window
     (C          : Connection_Access_Type;
      Window     : Window_Id_Type;
      Value_Mask : Interfaces.Unsigned_16;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Configure_Window, "xcb_configure_window");

   function Circulate_Window_Checked
     (C         : Connection_Access_Type;
      Direction : Interfaces.Unsigned_8;
      Window    : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Circulate_Window_Checked, "xcb_circulate_window_checked");

   function Circulate_Window
     (C         : Connection_Access_Type;
      Direction : Interfaces.Unsigned_8;
      Window    : Window_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Circulate_Window, "xcb_circulate_window");

   type Get_Geometry_Reply_Padding_0_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Get_Geometry_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Depth         : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Root          : aliased Window_Id_Type;
      X             : aliased Interfaces.Integer_16;
      Y             : aliased Interfaces.Integer_16;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
      Border_Width  : aliased Interfaces.Unsigned_16;
      Padding_0     : aliased Get_Geometry_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Geometry_Reply_Type);

   type Get_Geometry_Reply_Access_Type is access all Get_Geometry_Reply_Type;
   for Get_Geometry_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Geometry_Reply_Access_Type);

   type Get_Geometry_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Geometry_Cookie_Type);

   function Get_Geometry_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Geometry_Cookie_Type;
      Error  : System.Address) return Get_Geometry_Reply_Access_Type;
   pragma Import (C, Get_Geometry_Reply, "xcb_get_geometry_reply");

   function Get_Geometry_Unchecked
     (C        : Connection_Access_Type;
      Drawable : Drawable_Id_Type) return Get_Geometry_Cookie_Type;
   pragma Import (C, Get_Geometry_Unchecked, "xcb_get_geometry_unchecked");

   function Get_Geometry
     (C        : Connection_Access_Type;
      Drawable : Drawable_Id_Type) return Get_Geometry_Cookie_Type;
   pragma Import (C, Get_Geometry, "xcb_get_geometry");

   type Query_Tree_Reply_Padding_1_Array_Type is array (0 .. 13) of aliased Interfaces.Unsigned_8;
   type Query_Tree_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Root          : aliased Window_Id_Type;
      Parent        : aliased Window_Id_Type;
      Children_Len  : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Query_Tree_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Tree_Reply_Type);

   type Query_Tree_Reply_Access_Type is access all Query_Tree_Reply_Type;
   for Query_Tree_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Tree_Reply_Access_Type);

   function Query_Tree_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Query_Tree_Size_Of, "xcb_query_tree_sizeof");

   type Query_Tree_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Tree_Cookie_Type);

   function Query_Tree_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Tree_Cookie_Type;
      Error  : System.Address) return Query_Tree_Reply_Access_Type;
   pragma Import (C, Query_Tree_Reply, "xcb_query_tree_reply");

   function Query_Tree_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Query_Tree_Cookie_Type;
   pragma Import (C, Query_Tree_Unchecked, "xcb_query_tree_unchecked");

   function Query_Tree (C : Connection_Access_Type; Window : Window_Id_Type) return Query_Tree_Cookie_Type;
   pragma Import (C, Query_Tree, "xcb_query_tree");

   type Intern_Atom_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Atom          : aliased Atom_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Intern_Atom_Reply_Type);

   type Intern_Atom_Reply_Access_Type is access all Intern_Atom_Reply_Type;
   for Intern_Atom_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Intern_Atom_Reply_Access_Type);

   function Intern_Atom_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Intern_Atom_Size_Of, "xcb_intern_atom_sizeof");

   type Intern_Atom_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Intern_Atom_Cookie_Type);

   function Intern_Atom_Reply
     (C      : Connection_Access_Type;
      Cookie : Intern_Atom_Cookie_Type;
      Error  : System.Address) return Intern_Atom_Reply_Access_Type;
   pragma Import (C, Intern_Atom_Reply, "xcb_intern_atom_reply");

   function Intern_Atom_Unchecked
     (C              : Connection_Access_Type;
      Only_If_Exists : Interfaces.Unsigned_8;
      Name_Len       : Interfaces.Unsigned_16;
      Name           : Interfaces.C.Strings.chars_ptr) return Intern_Atom_Cookie_Type;
   pragma Import (C, Intern_Atom_Unchecked, "xcb_intern_atom_unchecked");

   function Intern_Atom
     (C              : Connection_Access_Type;
      Only_If_Exists : Interfaces.Unsigned_8;
      Name_Len       : Interfaces.Unsigned_16;
      Name           : Interfaces.C.Strings.chars_ptr) return Intern_Atom_Cookie_Type;
   pragma Import (C, Intern_Atom, "xcb_intern_atom");

   type Get_Atom_Name_Reply_Padding_1_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type Get_Atom_Name_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Name_Len      : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Get_Atom_Name_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Atom_Name_Reply_Type);

   type Get_Atom_Name_Reply_Access_Type is access all Get_Atom_Name_Reply_Type;
   for Get_Atom_Name_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Atom_Name_Reply_Access_Type);

   function Get_Atom_Name_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Atom_Name_Size_Of, "xcb_get_atom_name_sizeof");

   type Get_Atom_Name_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Atom_Name_Cookie_Type);

   function Get_Atom_Name_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Atom_Name_Cookie_Type;
      Error  : System.Address) return Get_Atom_Name_Reply_Access_Type;
   pragma Import (C, Get_Atom_Name_Reply, "xcb_get_atom_name_reply");

   function Get_Atom_Name_Unchecked
     (C    : Connection_Access_Type;
      Atom : Atom_Id_Type) return Get_Atom_Name_Cookie_Type;
   pragma Import (C, Get_Atom_Name_Unchecked, "xcb_get_atom_name_unchecked");

   function Get_Atom_Name (C : Connection_Access_Type; Atom : Atom_Id_Type) return Get_Atom_Name_Cookie_Type;
   pragma Import (C, Get_Atom_Name, "xcb_get_atom_name");

   function Change_Property_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Property_Size_Of, "xcb_change_property_sizeof");

   function Change_Property_Checked
     (C        : Connection_Access_Type;
      Mode     : Interfaces.Unsigned_8;
      Window   : Window_Id_Type;
      Property : Atom_Id_Type;
      Kind     : Atom_Id_Type;
      Format   : Interfaces.Unsigned_8;
      Data_Len : Interfaces.Unsigned_32;
      Data     : System.Address) return Void_Cookie_Type;
   pragma Import (C, Change_Property_Checked, "xcb_change_property_checked");

   function Change_Property
     (C        : Connection_Access_Type;
      Mode     : Interfaces.Unsigned_8;
      Window   : Window_Id_Type;
      Property : Atom_Id_Type;
      Kind     : Atom_Id_Type;
      Format   : Interfaces.Unsigned_8;
      Data_Len : Interfaces.Unsigned_32;
      Data     : System.Address) return Void_Cookie_Type;
   pragma Import (C, Change_Property, "xcb_change_property");

   function Delete_Property_Checked
     (C        : Connection_Access_Type;
      Window   : Window_Id_Type;
      Property : Atom_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Delete_Property_Checked, "xcb_delete_property_checked");

   function Delete_Property
     (C        : Connection_Access_Type;
      Window   : Window_Id_Type;
      Property : Atom_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Delete_Property, "xcb_delete_property");

   type Get_Property_Reply_Padding_0_Array_Type is array (0 .. 11) of aliased Interfaces.Unsigned_8;
   type Get_Property_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Format        : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Kind          : aliased Atom_Id_Type;
      Bytes_After   : aliased Interfaces.Unsigned_32;
      Value_Len     : aliased Interfaces.Unsigned_32;
      Padding_0     : aliased Get_Property_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Property_Reply_Type);

   type Get_Property_Reply_Access_Type is access all Get_Property_Reply_Type;
   for Get_Property_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Property_Reply_Access_Type);

   function Get_Property_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Property_Size_Of, "xcb_get_property_sizeof");

   type Get_Property_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Property_Cookie_Type);

   function Get_Property_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Property_Cookie_Type;
      Error  : System.Address) return Get_Property_Reply_Access_Type;
   pragma Import (C, Get_Property_Reply, "xcb_get_property_reply");

   function Get_Property_Unchecked
     (C           : Connection_Access_Type;
      Delete      : Interfaces.Unsigned_8;
      Window      : Window_Id_Type;
      Property    : Atom_Id_Type;
      Kind        : Atom_Id_Type;
      Long_Offset : Interfaces.Unsigned_32;
      Long_Length : Interfaces.Unsigned_32) return Get_Property_Cookie_Type;
   pragma Import (C, Get_Property_Unchecked, "xcb_get_property_unchecked");

   function Get_Property
     (C           : Connection_Access_Type;
      Delete      : Interfaces.Unsigned_8;
      Window      : Window_Id_Type;
      Property    : Atom_Id_Type;
      Kind        : Atom_Id_Type;
      Long_Offset : Interfaces.Unsigned_32;
      Long_Length : Interfaces.Unsigned_32) return Get_Property_Cookie_Type;
   pragma Import (C, Get_Property, "xcb_get_property");

   type List_Properties_Reply_Padding_1_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type List_Properties_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Atoms_Len     : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased List_Properties_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Properties_Reply_Type);

   type List_Properties_Reply_Access_Type is access all List_Properties_Reply_Type;
   for List_Properties_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Properties_Reply_Access_Type);

   function List_Properties_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Properties_Size_Of, "xcb_list_properties_sizeof");

   type List_Properties_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Properties_Cookie_Type);

   function List_Properties_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Properties_Cookie_Type;
      Error  : System.Address) return List_Properties_Reply_Access_Type;
   pragma Import (C, List_Properties_Reply, "xcb_list_properties_reply");

   function List_Properties_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return List_Properties_Cookie_Type;
   pragma Import (C, List_Properties_Unchecked, "xcb_list_properties_unchecked");

   function List_Properties
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return List_Properties_Cookie_Type;
   pragma Import (C, List_Properties, "xcb_list_properties");

   function Set_Selection_Owner_Checked
     (C         : Connection_Access_Type;
      Owner     : Window_Id_Type;
      Selection : Atom_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Selection_Owner_Checked, "xcb_set_selection_owner_checked");

   function Set_Selection_Owner
     (C         : Connection_Access_Type;
      Owner     : Window_Id_Type;
      Selection : Atom_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Selection_Owner, "xcb_set_selection_owner");

   type Get_Selection_Owner_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Owner         : aliased Window_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Selection_Owner_Reply_Type);

   type Get_Selection_Owner_Reply_Access_Type is access all Get_Selection_Owner_Reply_Type;
   for Get_Selection_Owner_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Selection_Owner_Reply_Access_Type);

   type Get_Selection_Owner_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Selection_Owner_Cookie_Type);

   function Get_Selection_Owner_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Selection_Owner_Cookie_Type;
      Error  : System.Address) return Get_Selection_Owner_Reply_Access_Type;
   pragma Import (C, Get_Selection_Owner_Reply, "xcb_get_selection_owner_reply");

   function Get_Selection_Owner_Unchecked
     (C         : Connection_Access_Type;
      Selection : Atom_Id_Type) return Get_Selection_Owner_Cookie_Type;
   pragma Import (C, Get_Selection_Owner_Unchecked, "xcb_get_selection_owner_unchecked");

   function Get_Selection_Owner
     (C         : Connection_Access_Type;
      Selection : Atom_Id_Type) return Get_Selection_Owner_Cookie_Type;
   pragma Import (C, Get_Selection_Owner, "xcb_get_selection_owner");

   function Convert_Selection_Checked
     (C         : Connection_Access_Type;
      Requestor : Window_Id_Type;
      Selection : Atom_Id_Type;
      Target    : Atom_Id_Type;
      Property  : Atom_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Convert_Selection_Checked, "xcb_convert_selection_checked");

   function Convert_Selection
     (C         : Connection_Access_Type;
      Requestor : Window_Id_Type;
      Selection : Atom_Id_Type;
      Target    : Atom_Id_Type;
      Property  : Atom_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Convert_Selection, "xcb_convert_selection");

   function Send_Event_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Send_Event_Size_Of, "xcb_send_event_sizeof");

   function Send_Event_Checked
     (C           : Connection_Access_Type;
      Propagate   : Interfaces.Unsigned_8;
      Destination : Window_Id_Type;
      Event_Mask  : Interfaces.Unsigned_32;
      Event       : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Send_Event_Checked, "xcb_send_event_checked");

   function Send_Event
     (C           : Connection_Access_Type;
      Propagate   : Interfaces.Unsigned_8;
      Destination : Window_Id_Type;
      Event_Mask  : Interfaces.Unsigned_32;
      Event       : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Send_Event, "xcb_send_event");

   type Grab_Pointer_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Status        : aliased Grab_Status_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Grab_Pointer_Reply_Type);

   type Grab_Pointer_Reply_Access_Type is access all Grab_Pointer_Reply_Type;
   for Grab_Pointer_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Grab_Pointer_Reply_Access_Type);

   type Grab_Pointer_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Grab_Pointer_Cookie_Type);

   function Grab_Pointer_Reply
     (C      : Connection_Access_Type;
      Cookie : Grab_Pointer_Cookie_Type;
      Error  : System.Address) return Grab_Pointer_Reply_Access_Type;
   pragma Import (C, Grab_Pointer_Reply, "xcb_grab_pointer_reply");

   function Grab_Pointer_Unchecked
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Event_Mask    : Interfaces.Unsigned_16;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8;
      Confine_To    : Window_Id_Type;
      Cursor        : Cursor_Id_Type;
      Time          : Timestamp_Type) return Grab_Pointer_Cookie_Type;
   pragma Import (C, Grab_Pointer_Unchecked, "xcb_grab_pointer_unchecked");

   function Grab_Pointer
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Event_Mask    : Interfaces.Unsigned_16;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8;
      Confine_To    : Window_Id_Type;
      Cursor        : Cursor_Id_Type;
      Time          : Timestamp_Type) return Grab_Pointer_Cookie_Type;
   pragma Import (C, Grab_Pointer, "xcb_grab_pointer");

   function Ungrab_Pointer_Checked
     (C    : Connection_Access_Type;
      Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Pointer_Checked, "xcb_ungrab_pointer_checked");

   function Ungrab_Pointer (C : Connection_Access_Type; Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Pointer, "xcb_ungrab_pointer");

   function Grab_Button_Checked
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Event_Mask    : Interfaces.Unsigned_16;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8;
      Confine_To    : Window_Id_Type;
      Cursor        : Cursor_Id_Type;
      Button        : Interfaces.Unsigned_8;
      Modifiers     : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Grab_Button_Checked, "xcb_grab_button_checked");

   function Grab_Button
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Event_Mask    : Interfaces.Unsigned_16;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8;
      Confine_To    : Window_Id_Type;
      Cursor        : Cursor_Id_Type;
      Button        : Interfaces.Unsigned_8;
      Modifiers     : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Grab_Button, "xcb_grab_button");

   function Ungrab_Button_Checked
     (C           : Connection_Access_Type;
      Button      : Interfaces.Unsigned_8;
      Grab_Window : Window_Id_Type;
      Modifiers   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Button_Checked, "xcb_ungrab_button_checked");

   function Ungrab_Button
     (C           : Connection_Access_Type;
      Button      : Interfaces.Unsigned_8;
      Grab_Window : Window_Id_Type;
      Modifiers   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Button, "xcb_ungrab_button");

   function Change_Active_Pointer_Grab_Checked
     (C          : Connection_Access_Type;
      Cursor     : Cursor_Id_Type;
      Time       : Timestamp_Type;
      Event_Mask : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Change_Active_Pointer_Grab_Checked, "xcb_change_active_pointer_grab_checked");

   function Change_Active_Pointer_Grab
     (C          : Connection_Access_Type;
      Cursor     : Cursor_Id_Type;
      Time       : Timestamp_Type;
      Event_Mask : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Change_Active_Pointer_Grab, "xcb_change_active_pointer_grab");

   type Grab_Keyboard_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Status        : aliased Grab_Status_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Grab_Keyboard_Reply_Type);

   type Grab_Keyboard_Reply_Access_Type is access all Grab_Keyboard_Reply_Type;
   for Grab_Keyboard_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Grab_Keyboard_Reply_Access_Type);

   type Grab_Keyboard_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Grab_Keyboard_Cookie_Type);

   function Grab_Keyboard_Reply
     (C      : Connection_Access_Type;
      Cookie : Grab_Keyboard_Cookie_Type;
      Error  : System.Address) return Grab_Keyboard_Reply_Access_Type;
   pragma Import (C, Grab_Keyboard_Reply, "xcb_grab_keyboard_reply");

   function Grab_Keyboard_Unchecked
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Time          : Timestamp_Type;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8) return Grab_Keyboard_Cookie_Type;
   pragma Import (C, Grab_Keyboard_Unchecked, "xcb_grab_keyboard_unchecked");

   function Grab_Keyboard
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Time          : Timestamp_Type;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8) return Grab_Keyboard_Cookie_Type;
   pragma Import (C, Grab_Keyboard, "xcb_grab_keyboard");

   function Ungrab_Keyboard_Checked
     (C    : Connection_Access_Type;
      Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Keyboard_Checked, "xcb_ungrab_keyboard_checked");

   function Ungrab_Keyboard (C : Connection_Access_Type; Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Keyboard, "xcb_ungrab_keyboard");

   function Grab_Key_Checked
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Modifiers     : Interfaces.Unsigned_16;
      Key           : Keycode_Type;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Grab_Key_Checked, "xcb_grab_key_checked");

   function Grab_Key
     (C             : Connection_Access_Type;
      Owner_Events  : Interfaces.Unsigned_8;
      Grab_Window   : Window_Id_Type;
      Modifiers     : Interfaces.Unsigned_16;
      Key           : Keycode_Type;
      Pointer_Mode  : Interfaces.Unsigned_8;
      Keyboard_Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Grab_Key, "xcb_grab_key");

   function Ungrab_Key_Checked
     (C           : Connection_Access_Type;
      Key         : Keycode_Type;
      Grab_Window : Window_Id_Type;
      Modifiers   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Key_Checked, "xcb_ungrab_key_checked");

   function Ungrab_Key
     (C           : Connection_Access_Type;
      Key         : Keycode_Type;
      Grab_Window : Window_Id_Type;
      Modifiers   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Key, "xcb_ungrab_key");

   function Allow_Events_Checked
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8;
      Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Allow_Events_Checked, "xcb_allow_events_checked");

   function Allow_Events
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8;
      Time : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Allow_Events, "xcb_allow_events");

   function Grab_Server_Checked (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, Grab_Server_Checked, "xcb_grab_server_checked");

   function Grab_Server (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, Grab_Server, "xcb_grab_server");

   function Ungrab_Server_Checked (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Server_Checked, "xcb_ungrab_server_checked");

   function Ungrab_Server (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, Ungrab_Server, "xcb_ungrab_server");

   type Query_Pointer_Reply_Padding_0_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Query_Pointer_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Same_Screen   : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Root          : aliased Window_Id_Type;
      Child         : aliased Window_Id_Type;
      Root_X        : aliased Interfaces.Integer_16;
      Root_Y        : aliased Interfaces.Integer_16;
      Win_X         : aliased Interfaces.Integer_16;
      Win_Y         : aliased Interfaces.Integer_16;
      Mask          : aliased Interfaces.Unsigned_16;
      Padding_0     : aliased Query_Pointer_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Pointer_Reply_Type);

   type Query_Pointer_Reply_Access_Type is access all Query_Pointer_Reply_Type;
   for Query_Pointer_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Pointer_Reply_Access_Type);

   type Query_Pointer_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Pointer_Cookie_Type);

   function Query_Pointer_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Pointer_Cookie_Type;
      Error  : System.Address) return Query_Pointer_Reply_Access_Type;
   pragma Import (C, Query_Pointer_Reply, "xcb_query_pointer_reply");

   function Query_Pointer_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Query_Pointer_Cookie_Type;
   pragma Import (C, Query_Pointer_Unchecked, "xcb_query_pointer_unchecked");

   function Query_Pointer
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return Query_Pointer_Cookie_Type;
   pragma Import (C, Query_Pointer, "xcb_query_pointer");

   type Get_Motion_Events_Reply_Padding_1_Array_Type is array (0 .. 19) of aliased Interfaces.Unsigned_8;
   type Get_Motion_Events_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Events_Len    : aliased Interfaces.Unsigned_32;
      Padding_1     : aliased Get_Motion_Events_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Motion_Events_Reply_Type);

   type Get_Motion_Events_Reply_Access_Type is access all Get_Motion_Events_Reply_Type;
   for Get_Motion_Events_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Motion_Events_Reply_Access_Type);

   function Get_Motion_Events_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Motion_Events_Size_Of, "xcb_get_motion_events_sizeof");

   type Get_Motion_Events_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Motion_Events_Cookie_Type);

   function Get_Motion_Events_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Motion_Events_Cookie_Type;
      Error  : System.Address) return Get_Motion_Events_Reply_Access_Type;
   pragma Import (C, Get_Motion_Events_Reply, "xcb_get_motion_events_reply");

   function Get_Motion_Events_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type;
      Start  : Timestamp_Type;
      Stop   : Timestamp_Type) return Get_Motion_Events_Cookie_Type;
   pragma Import (C, Get_Motion_Events_Unchecked, "xcb_get_motion_events_unchecked");

   function Get_Motion_Events
     (C      : Connection_Access_Type;
      Window : Window_Id_Type;
      Start  : Timestamp_Type;
      Stop   : Timestamp_Type) return Get_Motion_Events_Cookie_Type;
   pragma Import (C, Get_Motion_Events, "xcb_get_motion_events");

   type Translate_Coordinates_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Same_Screen   : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Child         : aliased Window_Id_Type;
      Dst_X         : aliased Interfaces.Integer_16;
      Dst_Y         : aliased Interfaces.Integer_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Translate_Coordinates_Reply_Type);

   type Translate_Coordinates_Reply_Access_Type is access all Translate_Coordinates_Reply_Type;
   for Translate_Coordinates_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Translate_Coordinates_Reply_Access_Type);

   type Translate_Coordinates_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Translate_Coordinates_Cookie_Type);

   function Translate_Coordinates_Reply
     (C      : Connection_Access_Type;
      Cookie : Translate_Coordinates_Cookie_Type;
      Error  : System.Address) return Translate_Coordinates_Reply_Access_Type;
   pragma Import (C, Translate_Coordinates_Reply, "xcb_translate_coordinates_reply");

   function Translate_Coordinates_Unchecked
     (C          : Connection_Access_Type;
      Src_Window : Window_Id_Type;
      Dst_Window : Window_Id_Type;
      Src_X      : Interfaces.Integer_16;
      Src_Y      : Interfaces.Integer_16) return Translate_Coordinates_Cookie_Type;
   pragma Import (C, Translate_Coordinates_Unchecked, "xcb_translate_coordinates_unchecked");

   function Translate_Coordinates
     (C          : Connection_Access_Type;
      Src_Window : Window_Id_Type;
      Dst_Window : Window_Id_Type;
      Src_X      : Interfaces.Integer_16;
      Src_Y      : Interfaces.Integer_16) return Translate_Coordinates_Cookie_Type;
   pragma Import (C, Translate_Coordinates, "xcb_translate_coordinates");

   function Warp_Pointer_Checked
     (C          : Connection_Access_Type;
      Src_Window : Window_Id_Type;
      Dst_Window : Window_Id_Type;
      Src_X      : Interfaces.Integer_16;
      Src_Y      : Interfaces.Integer_16;
      Src_Width  : Interfaces.Unsigned_16;
      Src_Height : Interfaces.Unsigned_16;
      Dst_X      : Interfaces.Integer_16;
      Dst_Y      : Interfaces.Integer_16) return Void_Cookie_Type;
   pragma Import (C, Warp_Pointer_Checked, "xcb_warp_pointer_checked");

   function Warp_Pointer
     (C          : Connection_Access_Type;
      Src_Window : Window_Id_Type;
      Dst_Window : Window_Id_Type;
      Src_X      : Interfaces.Integer_16;
      Src_Y      : Interfaces.Integer_16;
      Src_Width  : Interfaces.Unsigned_16;
      Src_Height : Interfaces.Unsigned_16;
      Dst_X      : Interfaces.Integer_16;
      Dst_Y      : Interfaces.Integer_16) return Void_Cookie_Type;
   pragma Import (C, Warp_Pointer, "xcb_warp_pointer");

   function Set_Input_Focus_Checked
     (C         : Connection_Access_Type;
      Revert_To : Interfaces.Unsigned_8;
      Focus     : Window_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Input_Focus_Checked, "xcb_set_input_focus_checked");

   function Set_Input_Focus
     (C         : Connection_Access_Type;
      Revert_To : Interfaces.Unsigned_8;
      Focus     : Window_Id_Type;
      Time      : Timestamp_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Input_Focus, "xcb_set_input_focus");

   type Get_Input_Focus_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Revert_To     : aliased Input_Focus_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Focus         : aliased Window_Id_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Input_Focus_Reply_Type);

   type Get_Input_Focus_Reply_Access_Type is access all Get_Input_Focus_Reply_Type;
   for Get_Input_Focus_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Input_Focus_Reply_Access_Type);

   type Get_Input_Focus_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Input_Focus_Cookie_Type);

   function Get_Input_Focus_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Input_Focus_Cookie_Type;
      Error  : System.Address) return Get_Input_Focus_Reply_Access_Type;
   pragma Import (C, Get_Input_Focus_Reply, "xcb_get_input_focus_reply");

   function Get_Input_Focus_Unchecked (C : Connection_Access_Type) return Get_Input_Focus_Cookie_Type;
   pragma Import (C, Get_Input_Focus_Unchecked, "xcb_get_input_focus_unchecked");

   function Get_Input_Focus (C : Connection_Access_Type) return Get_Input_Focus_Cookie_Type;
   pragma Import (C, Get_Input_Focus, "xcb_get_input_focus");

   type Query_Keymap_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Keymap_Reply_Type);

   type Query_Keymap_Reply_Access_Type is access all Query_Keymap_Reply_Type;
   for Query_Keymap_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Keymap_Reply_Access_Type);

   function Query_Keymap_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Query_Keymap_Size_Of, "xcb_query_keymap_sizeof");

   type Query_Keymap_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Keymap_Cookie_Type);

   function Query_Keymap_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Keymap_Cookie_Type;
      Error  : System.Address) return Query_Keymap_Reply_Access_Type;
   pragma Import (C, Query_Keymap_Reply, "xcb_query_keymap_reply");

   function Query_Keymap_Unchecked (C : Connection_Access_Type) return Query_Keymap_Cookie_Type;
   pragma Import (C, Query_Keymap_Unchecked, "xcb_query_keymap_unchecked");

   function Query_Keymap (C : Connection_Access_Type) return Query_Keymap_Cookie_Type;
   pragma Import (C, Query_Keymap, "xcb_query_keymap");

   function Open_Font_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Open_Font_Size_Of, "xcb_open_font_sizeof");

   function Open_Font_Checked
     (C        : Connection_Access_Type;
      Fid      : Font_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Open_Font_Checked, "xcb_open_font_checked");

   function Open_Font
     (C        : Connection_Access_Type;
      Fid      : Font_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Open_Font, "xcb_open_font");

   function Close_Font_Checked (C : Connection_Access_Type; Font : Font_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Close_Font_Checked, "xcb_close_font_checked");

   function Close_Font (C : Connection_Access_Type; Font : Font_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Close_Font, "xcb_close_font");

   type Query_Font_Reply_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Query_Font_Reply_Padding_2_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type Query_Font_Reply_Type is record
      Response_Kind      : aliased Interfaces.Unsigned_8;
      Padding_0          : aliased Interfaces.Unsigned_8;
      Sequence           : aliased Interfaces.Unsigned_16;
      Length             : aliased Interfaces.Unsigned_32;
      Min_Bounds         : aliased Character_Information_Type;
      Padding_1          : aliased Query_Font_Reply_Padding_1_Array_Type;
      Max_Bounds         : aliased Character_Information_Type;
      Padding_2          : aliased Query_Font_Reply_Padding_2_Array_Type;
      Min_Char_Or_Byte_2 : aliased Interfaces.Unsigned_16;
      Max_Char_Or_Byte_2 : aliased Interfaces.Unsigned_16;
      Default_Char       : aliased Interfaces.Unsigned_16;
      Properties_Len     : aliased Interfaces.Unsigned_16;
      Draw_Direction     : aliased Interfaces.Unsigned_8;
      Min_Byte_1         : aliased Interfaces.Unsigned_8;
      Max_Byte_1         : aliased Interfaces.Unsigned_8;
      All_Chars_Exist    : aliased Interfaces.Unsigned_8;
      Font_Ascent        : aliased Interfaces.Integer_16;
      Font_Descent       : aliased Interfaces.Integer_16;
      Char_Infos_Len     : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Font_Reply_Type);

   type Query_Font_Reply_Access_Type is access all Query_Font_Reply_Type;
   for Query_Font_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Font_Reply_Access_Type);

   function Query_Font_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Query_Font_Size_Of, "xcb_query_font_sizeof");

   type Query_Font_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Font_Cookie_Type);

   function Query_Font_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Font_Cookie_Type;
      Error  : System.Address) return Query_Font_Reply_Access_Type;
   pragma Import (C, Query_Font_Reply, "xcb_query_font_reply");

   function Query_Font_Unchecked
     (C    : Connection_Access_Type;
      Font : Fontable_Id_Type) return Query_Font_Cookie_Type;
   pragma Import (C, Query_Font_Unchecked, "xcb_query_font_unchecked");

   function Query_Font (C : Connection_Access_Type; Font : Fontable_Id_Type) return Query_Font_Cookie_Type;
   pragma Import (C, Query_Font, "xcb_query_font");

   type Query_Text_Extents_Reply_Type is record
      Response_Kind   : aliased Interfaces.Unsigned_8;
      Draw_Direction  : aliased Font_Draw_Type;
      Sequence        : aliased Interfaces.Unsigned_16;
      Length          : aliased Interfaces.Unsigned_32;
      Font_Ascent     : aliased Interfaces.Integer_16;
      Font_Descent    : aliased Interfaces.Integer_16;
      Overall_Ascent  : aliased Interfaces.Integer_16;
      Overall_Descent : aliased Interfaces.Integer_16;
      Overall_Width   : aliased Interfaces.Integer_32;
      Overall_Left    : aliased Interfaces.Integer_32;
      Overall_Right   : aliased Interfaces.Integer_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Text_Extents_Reply_Type);

   type Query_Text_Extents_Reply_Access_Type is access all Query_Text_Extents_Reply_Type;
   for Query_Text_Extents_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Text_Extents_Reply_Access_Type);

   function Query_Text_Extents_Size_Of
     (Buffer      : System.Address;
      Text_Length : Interfaces.Unsigned_32) return Interfaces.C.int;
   pragma Import (C, Query_Text_Extents_Size_Of, "xcb_query_text_extents_sizeof");

   type Query_Text_Extents_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Text_Extents_Cookie_Type);

   function Query_Text_Extents_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Text_Extents_Cookie_Type;
      Error  : System.Address) return Query_Text_Extents_Reply_Access_Type;
   pragma Import (C, Query_Text_Extents_Reply, "xcb_query_text_extents_reply");

   function Query_Text_Extents_Unchecked
     (C           : Connection_Access_Type;
      Font        : Fontable_Id_Type;
      Text_Length : Interfaces.Unsigned_32;
      Text        : Char_2B_Array_Type) return Query_Text_Extents_Cookie_Type;
   pragma Import (C, Query_Text_Extents_Unchecked, "xcb_query_text_extents_unchecked");

   function Query_Text_Extents
     (C           : Connection_Access_Type;
      Font        : Fontable_Id_Type;
      Text_Length : Interfaces.Unsigned_32;
      Text        : Char_2B_Array_Type) return Query_Text_Extents_Cookie_Type;
   pragma Import (C, Query_Text_Extents, "xcb_query_text_extents");

   type List_Fonts_Reply_Padding_1_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type List_Fonts_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Names_Len     : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased List_Fonts_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Fonts_Reply_Type);

   type List_Fonts_Reply_Access_Type is access all List_Fonts_Reply_Type;
   for List_Fonts_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Fonts_Reply_Access_Type);

   function List_Fonts_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Fonts_Size_Of, "xcb_list_fonts_sizeof");

   type List_Fonts_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Fonts_Cookie_Type);

   function List_Fonts_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Fonts_Cookie_Type;
      Error  : System.Address) return List_Fonts_Reply_Access_Type;
   pragma Import (C, List_Fonts_Reply, "xcb_list_fonts_reply");

   function List_Fonts_Unchecked
     (C           : Connection_Access_Type;
      Max_Names   : Interfaces.Unsigned_16;
      Pattern_Len : Interfaces.Unsigned_16;
      Pattern     : Interfaces.C.Strings.chars_ptr) return List_Fonts_Cookie_Type;
   pragma Import (C, List_Fonts_Unchecked, "xcb_list_fonts_unchecked");

   function List_Fonts
     (C           : Connection_Access_Type;
      Max_Names   : Interfaces.Unsigned_16;
      Pattern_Len : Interfaces.Unsigned_16;
      Pattern     : Interfaces.C.Strings.chars_ptr) return List_Fonts_Cookie_Type;
   pragma Import (C, List_Fonts, "xcb_list_fonts");

   type List_Fonts_With_Info_Reply_Padding_0_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type List_Fonts_With_Info_Reply_Padding_1_Array_Type is array (0 .. 3) of aliased Interfaces.Unsigned_8;
   type List_Fonts_With_Info_Reply_Type is record
      Response_Kind      : aliased Interfaces.Unsigned_8;
      Name_Len           : aliased Interfaces.Unsigned_8;
      Sequence           : aliased Interfaces.Unsigned_16;
      Length             : aliased Interfaces.Unsigned_32;
      Min_Bounds         : aliased Character_Information_Type;
      Padding_0          : aliased List_Fonts_With_Info_Reply_Padding_0_Array_Type;
      Max_Bounds         : aliased Character_Information_Type;
      Padding_1          : aliased List_Fonts_With_Info_Reply_Padding_1_Array_Type;
      Min_Char_Or_Byte_2 : aliased Interfaces.Unsigned_16;
      Max_Char_Or_Byte_2 : aliased Interfaces.Unsigned_16;
      Default_Char       : aliased Interfaces.Unsigned_16;
      Properties_Len     : aliased Interfaces.Unsigned_16;
      Draw_Direction     : aliased Interfaces.Unsigned_8;
      Min_Byte_1         : aliased Interfaces.Unsigned_8;
      Max_Byte_1         : aliased Interfaces.Unsigned_8;
      All_Chars_Exist    : aliased Interfaces.Unsigned_8;
      Font_Ascent        : aliased Interfaces.Integer_16;
      Font_Descent       : aliased Interfaces.Integer_16;
      Replies_Hint       : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Fonts_With_Info_Reply_Type);

   type List_Fonts_With_Info_Reply_Access_Type is access all List_Fonts_With_Info_Reply_Type;
   for List_Fonts_With_Info_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Fonts_With_Info_Reply_Access_Type);

   function List_Fonts_With_Info_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Fonts_With_Info_Size_Of, "xcb_list_fonts_with_info_sizeof");

   type List_Fonts_With_Info_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Fonts_With_Info_Cookie_Type);

   function List_Fonts_With_Info_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Fonts_With_Info_Cookie_Type;
      Error  : System.Address) return List_Fonts_With_Info_Reply_Access_Type;
   pragma Import (C, List_Fonts_With_Info_Reply, "xcb_list_fonts_with_info_reply");

   function List_Fonts_With_Info_Unchecked
     (C           : Connection_Access_Type;
      Max_Names   : Interfaces.Unsigned_16;
      Pattern_Len : Interfaces.Unsigned_16;
      Pattern     : Interfaces.C.Strings.chars_ptr) return List_Fonts_With_Info_Cookie_Type;
   pragma Import (C, List_Fonts_With_Info_Unchecked, "xcb_list_fonts_with_info_unchecked");

   function List_Fonts_With_Info
     (C           : Connection_Access_Type;
      Max_Names   : Interfaces.Unsigned_16;
      Pattern_Len : Interfaces.Unsigned_16;
      Pattern     : Interfaces.C.Strings.chars_ptr) return List_Fonts_With_Info_Cookie_Type;
   pragma Import (C, List_Fonts_With_Info, "xcb_list_fonts_with_info");

   function Set_Font_Path_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Set_Font_Path_Size_Of, "xcb_set_font_path_sizeof");

   function Set_Font_Path_Checked
     (C        : Connection_Access_Type;
      Font_Qty : Interfaces.Unsigned_16;
      Font     : System.Address) return Void_Cookie_Type;
   pragma Import (C, Set_Font_Path_Checked, "xcb_set_font_path_checked");

   function Set_Font_Path
     (C        : Connection_Access_Type;
      Font_Qty : Interfaces.Unsigned_16;
      Font     : System.Address) return Void_Cookie_Type;
   pragma Import (C, Set_Font_Path, "xcb_set_font_path");

   type Get_Font_Path_Reply_Padding_1_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type Get_Font_Path_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Path_Len      : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Get_Font_Path_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Font_Path_Reply_Type);

   type Get_Font_Path_Reply_Access_Type is access all Get_Font_Path_Reply_Type;
   for Get_Font_Path_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Font_Path_Reply_Access_Type);

   function Get_Font_Path_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Font_Path_Size_Of, "xcb_get_font_path_sizeof");

   type Get_Font_Path_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Font_Path_Cookie_Type);

   function Get_Font_Path_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Font_Path_Cookie_Type;
      Error  : System.Address) return Get_Font_Path_Reply_Access_Type;
   pragma Import (C, Get_Font_Path_Reply, "xcb_get_font_path_reply");

   function Get_Font_Path_Unchecked (C : Connection_Access_Type) return Get_Font_Path_Cookie_Type;
   pragma Import (C, Get_Font_Path_Unchecked, "xcb_get_font_path_unchecked");

   function Get_Font_Path (C : Connection_Access_Type) return Get_Font_Path_Cookie_Type;
   pragma Import (C, Get_Font_Path, "xcb_get_font_path");

   function Create_Pixmap_Checked
     (C        : Connection_Access_Type;
      Depth    : Interfaces.Unsigned_8;
      Pid      : Pixmap_Id_Type;
      Drawable : Drawable_Id_Type;
      Width    : Interfaces.Unsigned_16;
      Height   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Pixmap_Checked, "xcb_create_pixmap_checked");

   function Create_Pixmap
     (C        : Connection_Access_Type;
      Depth    : Interfaces.Unsigned_8;
      Pid      : Pixmap_Id_Type;
      Drawable : Drawable_Id_Type;
      Width    : Interfaces.Unsigned_16;
      Height   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Pixmap, "xcb_create_pixmap");

   function Free_Pixmap_Checked (C : Connection_Access_Type; Pixmap : Pixmap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Pixmap_Checked, "xcb_free_pixmap_checked");

   function Free_Pixmap (C : Connection_Access_Type; Pixmap : Pixmap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Pixmap, "xcb_free_pixmap");

   function Create_Gc_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Create_Gc_Size_Of, "xcb_create_gc_sizeof");

   function Create_Gc_Checked
     (C          : Connection_Access_Type;
      Cid        : Gcontext_Id_Type;
      Drawable   : Drawable_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Gc_Checked, "xcb_create_gc_checked");

   function Create_Gc
     (C          : Connection_Access_Type;
      Cid        : Gcontext_Id_Type;
      Drawable   : Drawable_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Create_Gc, "xcb_create_gc");

   function Change_Gc_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Gc_Size_Of, "xcb_change_gc_sizeof");

   function Change_Gc_Checked
     (C          : Connection_Access_Type;
      Gc         : Gcontext_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Gc_Checked, "xcb_change_gc_checked");

   function Change_Gc
     (C          : Connection_Access_Type;
      Gc         : Gcontext_Id_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Gc, "xcb_change_gc");

   function Copy_Gc_Checked
     (C          : Connection_Access_Type;
      Src_Gc     : Gcontext_Id_Type;
      Dst_Gc     : Gcontext_Id_Type;
      Value_Mask : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Copy_Gc_Checked, "xcb_copy_gc_checked");

   function Copy_Gc
     (C          : Connection_Access_Type;
      Src_Gc     : Gcontext_Id_Type;
      Dst_Gc     : Gcontext_Id_Type;
      Value_Mask : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Copy_Gc, "xcb_copy_gc");

   function Set_Dashes_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Set_Dashes_Size_Of, "xcb_set_dashes_sizeof");

   function Set_Dashes_Checked
     (C           : Connection_Access_Type;
      Gc          : Gcontext_Id_Type;
      Dash_Offset : Interfaces.Unsigned_16;
      Dashes_Len  : Interfaces.Unsigned_16;
      Dashes      : access Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Dashes_Checked, "xcb_set_dashes_checked");

   function Set_Dashes
     (C           : Connection_Access_Type;
      Gc          : Gcontext_Id_Type;
      Dash_Offset : Interfaces.Unsigned_16;
      Dashes_Len  : Interfaces.Unsigned_16;
      Dashes      : access Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Dashes, "xcb_set_dashes");

   function Set_Clip_Rectangles_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Set_Clip_Rectangles_Size_Of, "xcb_set_clip_rectangles_sizeof");

   function Set_Clip_Rectangles_Checked
     (C                 : Connection_Access_Type;
      Ordering          : Interfaces.Unsigned_8;
      Gc                : Gcontext_Id_Type;
      Clip_X_Origin     : Interfaces.Integer_16;
      Clip_Y_Origin     : Interfaces.Integer_16;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Clip_Rectangles_Checked, "xcb_set_clip_rectangles_checked");

   function Set_Clip_Rectangles
     (C                 : Connection_Access_Type;
      Ordering          : Interfaces.Unsigned_8;
      Gc                : Gcontext_Id_Type;
      Clip_X_Origin     : Interfaces.Integer_16;
      Clip_Y_Origin     : Interfaces.Integer_16;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Set_Clip_Rectangles, "xcb_set_clip_rectangles");

   function Free_Gc_Checked (C : Connection_Access_Type; Gc : Gcontext_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Gc_Checked, "xcb_free_gc_checked");

   function Free_Gc (C : Connection_Access_Type; Gc : Gcontext_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Gc, "xcb_free_gc");

   function Clear_Area_Checked
     (C         : Connection_Access_Type;
      Exposures : Interfaces.Unsigned_8;
      Window    : Window_Id_Type;
      X         : Interfaces.Integer_16;
      Y         : Interfaces.Integer_16;
      Width     : Interfaces.Unsigned_16;
      Height    : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Clear_Area_Checked, "xcb_clear_area_checked");

   function Clear_Area
     (C         : Connection_Access_Type;
      Exposures : Interfaces.Unsigned_8;
      Window    : Window_Id_Type;
      X         : Interfaces.Integer_16;
      Y         : Interfaces.Integer_16;
      Width     : Interfaces.Unsigned_16;
      Height    : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Clear_Area, "xcb_clear_area");

   function Copy_Area_Checked
     (C            : Connection_Access_Type;
      Src_Drawable : Drawable_Id_Type;
      Dst_Drawable : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      Src_X        : Interfaces.Integer_16;
      Src_Y        : Interfaces.Integer_16;
      Dst_X        : Interfaces.Integer_16;
      Dst_Y        : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Copy_Area_Checked, "xcb_copy_area_checked");

   function Copy_Area
     (C            : Connection_Access_Type;
      Src_Drawable : Drawable_Id_Type;
      Dst_Drawable : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      Src_X        : Interfaces.Integer_16;
      Src_Y        : Interfaces.Integer_16;
      Dst_X        : Interfaces.Integer_16;
      Dst_Y        : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Copy_Area, "xcb_copy_area");

   function Copy_Plane_Checked
     (C            : Connection_Access_Type;
      Src_Drawable : Drawable_Id_Type;
      Dst_Drawable : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      Src_X        : Interfaces.Integer_16;
      Src_Y        : Interfaces.Integer_16;
      Dst_X        : Interfaces.Integer_16;
      Dst_Y        : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16;
      Bit_Plane    : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Copy_Plane_Checked, "xcb_copy_plane_checked");

   function Copy_Plane
     (C            : Connection_Access_Type;
      Src_Drawable : Drawable_Id_Type;
      Dst_Drawable : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      Src_X        : Interfaces.Integer_16;
      Src_Y        : Interfaces.Integer_16;
      Dst_X        : Interfaces.Integer_16;
      Dst_Y        : Interfaces.Integer_16;
      Width        : Interfaces.Unsigned_16;
      Height       : Interfaces.Unsigned_16;
      Bit_Plane    : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Copy_Plane, "xcb_copy_plane");

   function Poly_Point_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Point_Size_Of, "xcb_poly_point_sizeof");

   function Poly_Point_Checked
     (C               : Connection_Access_Type;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Point_Checked, "xcb_poly_point_checked");

   function Poly_Point
     (C               : Connection_Access_Type;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Point, "xcb_poly_point");

   function Poly_Line_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Line_Size_Of, "xcb_poly_line_sizeof");

   function Poly_Line_Checked
     (C               : Connection_Access_Type;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Line_Checked, "xcb_poly_line_checked");

   function Poly_Line
     (C               : Connection_Access_Type;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Line, "xcb_poly_line");

   function Poly_Segment_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Segment_Size_Of, "xcb_poly_segment_sizeof");

   function Poly_Segment_Checked
     (C               : Connection_Access_Type;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Segments_Length : Interfaces.Unsigned_32;
      Segments        : Segment_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Segment_Checked, "xcb_poly_segment_checked");

   function Poly_Segment
     (C               : Connection_Access_Type;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Segments_Length : Interfaces.Unsigned_32;
      Segments        : Segment_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Segment, "xcb_poly_segment");

   function Poly_Rectangle_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Rectangle_Size_Of, "xcb_poly_rectangle_sizeof");

   function Poly_Rectangle_Checked
     (C                 : Connection_Access_Type;
      Drawable          : Drawable_Id_Type;
      Gc                : Gcontext_Id_Type;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Rectangle_Checked, "xcb_poly_rectangle_checked");

   function Poly_Rectangle
     (C                 : Connection_Access_Type;
      Drawable          : Drawable_Id_Type;
      Gc                : Gcontext_Id_Type;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Rectangle, "xcb_poly_rectangle");

   function Poly_Arc_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Arc_Size_Of, "xcb_poly_arc_sizeof");

   function Poly_Arc_Checked
     (C           : Connection_Access_Type;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Arcs_Length : Interfaces.Unsigned_32;
      Arcs        : Arc_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Arc_Checked, "xcb_poly_arc_checked");

   function Poly_Arc
     (C           : Connection_Access_Type;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Arcs_Length : Interfaces.Unsigned_32;
      Arcs        : Arc_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Arc, "xcb_poly_arc");

   function Fill_Poly_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Fill_Poly_Size_Of, "xcb_fill_poly_sizeof");

   function Fill_Poly_Checked
     (C               : Connection_Access_Type;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Shape           : Interfaces.Unsigned_8;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Fill_Poly_Checked, "xcb_fill_poly_checked");

   function Fill_Poly
     (C               : Connection_Access_Type;
      Drawable        : Drawable_Id_Type;
      Gc              : Gcontext_Id_Type;
      Shape           : Interfaces.Unsigned_8;
      Coordinate_Mode : Interfaces.Unsigned_8;
      Points_Length   : Interfaces.Unsigned_32;
      Points          : Point_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Fill_Poly, "xcb_fill_poly");

   function Poly_Fill_Rectangle_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Fill_Rectangle_Size_Of, "xcb_poly_fill_rectangle_sizeof");

   function Poly_Fill_Rectangle_Checked
     (C                 : Connection_Access_Type;
      Drawable          : Drawable_Id_Type;
      Gc                : Gcontext_Id_Type;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Fill_Rectangle_Checked, "xcb_poly_fill_rectangle_checked");

   function Poly_Fill_Rectangle
     (C                 : Connection_Access_Type;
      Drawable          : Drawable_Id_Type;
      Gc                : Gcontext_Id_Type;
      Rectangles_Length : Interfaces.Unsigned_32;
      Rectangles        : Rectangle_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Fill_Rectangle, "xcb_poly_fill_rectangle");

   function Poly_Fill_Arc_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Fill_Arc_Size_Of, "xcb_poly_fill_arc_sizeof");

   function Poly_Fill_Arc_Checked
     (C           : Connection_Access_Type;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Arcs_Length : Interfaces.Unsigned_32;
      Arcs        : Arc_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Fill_Arc_Checked, "xcb_poly_fill_arc_checked");

   function Poly_Fill_Arc
     (C           : Connection_Access_Type;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Arcs_Length : Interfaces.Unsigned_32;
      Arcs        : Arc_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Fill_Arc, "xcb_poly_fill_arc");

   function Put_Image_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Put_Image_Size_Of, "xcb_put_image_sizeof");

   function Put_Image_Checked
     (C           : Connection_Access_Type;
      Format      : Interfaces.Unsigned_8;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Width       : Interfaces.Unsigned_16;
      Height      : Interfaces.Unsigned_16;
      Dst_X       : Interfaces.Integer_16;
      Dst_Y       : Interfaces.Integer_16;
      Left_Pad    : Interfaces.Unsigned_8;
      Depth       : Interfaces.Unsigned_8;
      Data_Length : Interfaces.Unsigned_32;
      Data        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Put_Image_Checked, "xcb_put_image_checked");

   function Put_Image
     (C           : Connection_Access_Type;
      Format      : Interfaces.Unsigned_8;
      Drawable    : Drawable_Id_Type;
      Gc          : Gcontext_Id_Type;
      Width       : Interfaces.Unsigned_16;
      Height      : Interfaces.Unsigned_16;
      Dst_X       : Interfaces.Integer_16;
      Dst_Y       : Interfaces.Integer_16;
      Left_Pad    : Interfaces.Unsigned_8;
      Depth       : Interfaces.Unsigned_8;
      Data_Length : Interfaces.Unsigned_32;
      Data        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Put_Image, "xcb_put_image");

   type Get_Image_Reply_Padding_0_Array_Type is array (0 .. 19) of aliased Interfaces.Unsigned_8;
   type Get_Image_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Depth         : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Visual        : aliased Visual_Id_Type;
      Padding_0     : aliased Get_Image_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Image_Reply_Type);

   type Get_Image_Reply_Access_Type is access all Get_Image_Reply_Type;
   for Get_Image_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Image_Reply_Access_Type);

   function Get_Image_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Image_Size_Of, "xcb_get_image_sizeof");

   type Get_Image_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Image_Cookie_Type);

   function Get_Image_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Image_Cookie_Type;
      Error  : System.Address) return Get_Image_Reply_Access_Type;
   pragma Import (C, Get_Image_Reply, "xcb_get_image_reply");

   function Get_Image_Unchecked
     (C          : Connection_Access_Type;
      Format     : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Width      : Interfaces.Unsigned_16;
      Height     : Interfaces.Unsigned_16;
      Plane_Mask : Interfaces.Unsigned_32) return Get_Image_Cookie_Type;
   pragma Import (C, Get_Image_Unchecked, "xcb_get_image_unchecked");

   function Get_Image
     (C          : Connection_Access_Type;
      Format     : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Width      : Interfaces.Unsigned_16;
      Height     : Interfaces.Unsigned_16;
      Plane_Mask : Interfaces.Unsigned_32) return Get_Image_Cookie_Type;
   pragma Import (C, Get_Image, "xcb_get_image");

   function Poly_Text_8_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Text_8_Size_Of, "xcb_poly_text_8_sizeof");

   function Poly_Text_8_Checked
     (C            : Connection_Access_Type;
      Drawable     : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Text_8_Checked, "xcb_poly_text_8_checked");

   function Poly_Text_8
     (C            : Connection_Access_Type;
      Drawable     : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Text_8, "xcb_poly_text_8");

   function Poly_Text_16_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Poly_Text_16_Size_Of, "xcb_poly_text_16_sizeof");

   function Poly_Text_16_Checked
     (C            : Connection_Access_Type;
      Drawable     : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Text_16_Checked, "xcb_poly_text_16_checked");

   function Poly_Text_16
     (C            : Connection_Access_Type;
      Drawable     : Drawable_Id_Type;
      Gc           : Gcontext_Id_Type;
      X            : Interfaces.Integer_16;
      Y            : Interfaces.Integer_16;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Poly_Text_16, "xcb_poly_text_16");

   function Image_Text_8_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Image_Text_8_Size_Of, "xcb_image_text_8_sizeof");

   function Image_Text_8_Checked
     (C          : Connection_Access_Type;
      String_Len : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      Gc         : Gcontext_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Text       : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Image_Text_8_Checked, "xcb_image_text_8_checked");

   function Image_Text_8
     (C          : Connection_Access_Type;
      String_Len : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      Gc         : Gcontext_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Text       : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Image_Text_8, "xcb_image_text_8");

   function Image_Text_16_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Image_Text_16_Size_Of, "xcb_image_text_16_sizeof");

   function Image_Text_16_Checked
     (C          : Connection_Access_Type;
      String_Len : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      Gc         : Gcontext_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Text       : Char_2B_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Image_Text_16_Checked, "xcb_image_text_16_checked");

   function Image_Text_16
     (C          : Connection_Access_Type;
      String_Len : Interfaces.Unsigned_8;
      Drawable   : Drawable_Id_Type;
      Gc         : Gcontext_Id_Type;
      X          : Interfaces.Integer_16;
      Y          : Interfaces.Integer_16;
      Text       : Char_2B_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Image_Text_16, "xcb_image_text_16");

   function Create_Colormap_Checked
     (C      : Connection_Access_Type;
      Alloc  : Interfaces.Unsigned_8;
      Mid    : Colormap_Id_Type;
      Window : Window_Id_Type;
      Visual : Visual_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Create_Colormap_Checked, "xcb_create_colormap_checked");

   function Create_Colormap
     (C      : Connection_Access_Type;
      Alloc  : Interfaces.Unsigned_8;
      Mid    : Colormap_Id_Type;
      Window : Window_Id_Type;
      Visual : Visual_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Create_Colormap, "xcb_create_colormap");

   function Free_Colormap_Checked
     (C    : Connection_Access_Type;
      Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Colormap_Checked, "xcb_free_colormap_checked");

   function Free_Colormap (C : Connection_Access_Type; Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Colormap, "xcb_free_colormap");

   function Copy_Colormap_And_Free_Checked
     (C        : Connection_Access_Type;
      Mid      : Colormap_Id_Type;
      Src_Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Copy_Colormap_And_Free_Checked, "xcb_copy_colormap_and_free_checked");

   function Copy_Colormap_And_Free
     (C        : Connection_Access_Type;
      Mid      : Colormap_Id_Type;
      Src_Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Copy_Colormap_And_Free, "xcb_copy_colormap_and_free");

   function Install_Colormap_Checked
     (C    : Connection_Access_Type;
      Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Install_Colormap_Checked, "xcb_install_colormap_checked");

   function Install_Colormap (C : Connection_Access_Type; Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Install_Colormap, "xcb_install_colormap");

   function Uninstall_Colormap_Checked
     (C    : Connection_Access_Type;
      Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Uninstall_Colormap_Checked, "xcb_uninstall_colormap_checked");

   function Uninstall_Colormap (C : Connection_Access_Type; Cmap : Colormap_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Uninstall_Colormap, "xcb_uninstall_colormap");

   type List_Installed_Colormaps_Reply_Padding_1_Array_Type is
     array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type List_Installed_Colormaps_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Cmaps_Len     : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased List_Installed_Colormaps_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Installed_Colormaps_Reply_Type);

   type List_Installed_Colormaps_Reply_Access_Type is access all List_Installed_Colormaps_Reply_Type;
   for List_Installed_Colormaps_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Installed_Colormaps_Reply_Access_Type);

   function List_Installed_Colormaps_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Installed_Colormaps_Size_Of, "xcb_list_installed_colormaps_sizeof");

   type List_Installed_Colormaps_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Installed_Colormaps_Cookie_Type);

   function List_Installed_Colormaps_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Installed_Colormaps_Cookie_Type;
      Error  : System.Address) return List_Installed_Colormaps_Reply_Access_Type;
   pragma Import (C, List_Installed_Colormaps_Reply, "xcb_list_installed_colormaps_reply");

   function List_Installed_Colormaps_Unchecked
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return List_Installed_Colormaps_Cookie_Type;
   pragma Import (C, List_Installed_Colormaps_Unchecked, "xcb_list_installed_colormaps_unchecked");

   function List_Installed_Colormaps
     (C      : Connection_Access_Type;
      Window : Window_Id_Type) return List_Installed_Colormaps_Cookie_Type;
   pragma Import (C, List_Installed_Colormaps, "xcb_list_installed_colormaps");

   type Alloc_Color_Reply_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Alloc_Color_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Red           : aliased Interfaces.Unsigned_16;
      Green         : aliased Interfaces.Unsigned_16;
      Blue          : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Alloc_Color_Reply_Padding_1_Array_Type;
      Pixel         : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Reply_Type);

   type Alloc_Color_Reply_Access_Type is access all Alloc_Color_Reply_Type;
   for Alloc_Color_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Alloc_Color_Reply_Access_Type);

   type Alloc_Color_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Cookie_Type);

   function Alloc_Color_Reply
     (C      : Connection_Access_Type;
      Cookie : Alloc_Color_Cookie_Type;
      Error  : System.Address) return Alloc_Color_Reply_Access_Type;
   pragma Import (C, Alloc_Color_Reply, "xcb_alloc_color_reply");

   function Alloc_Color_Unchecked
     (C     : Connection_Access_Type;
      Cmap  : Colormap_Id_Type;
      Red   : Interfaces.Unsigned_16;
      Green : Interfaces.Unsigned_16;
      Blue  : Interfaces.Unsigned_16) return Alloc_Color_Cookie_Type;
   pragma Import (C, Alloc_Color_Unchecked, "xcb_alloc_color_unchecked");

   function Alloc_Color
     (C     : Connection_Access_Type;
      Cmap  : Colormap_Id_Type;
      Red   : Interfaces.Unsigned_16;
      Green : Interfaces.Unsigned_16;
      Blue  : Interfaces.Unsigned_16) return Alloc_Color_Cookie_Type;
   pragma Import (C, Alloc_Color, "xcb_alloc_color");

   type Alloc_Named_Color_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Pixel         : aliased Interfaces.Unsigned_32;
      Exact_Red     : aliased Interfaces.Unsigned_16;
      Exact_Green   : aliased Interfaces.Unsigned_16;
      Exact_Blue    : aliased Interfaces.Unsigned_16;
      Visual_Red    : aliased Interfaces.Unsigned_16;
      Visual_Green  : aliased Interfaces.Unsigned_16;
      Visual_Blue   : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Named_Color_Reply_Type);

   type Alloc_Named_Color_Reply_Access_Type is access all Alloc_Named_Color_Reply_Type;
   for Alloc_Named_Color_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Alloc_Named_Color_Reply_Access_Type);

   function Alloc_Named_Color_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Alloc_Named_Color_Size_Of, "xcb_alloc_named_color_sizeof");

   type Alloc_Named_Color_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Named_Color_Cookie_Type);

   function Alloc_Named_Color_Reply
     (C      : Connection_Access_Type;
      Cookie : Alloc_Named_Color_Cookie_Type;
      Error  : System.Address) return Alloc_Named_Color_Reply_Access_Type;
   pragma Import (C, Alloc_Named_Color_Reply, "xcb_alloc_named_color_reply");

   function Alloc_Named_Color_Unchecked
     (C        : Connection_Access_Type;
      Cmap     : Colormap_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Alloc_Named_Color_Cookie_Type;
   pragma Import (C, Alloc_Named_Color_Unchecked, "xcb_alloc_named_color_unchecked");

   function Alloc_Named_Color
     (C        : Connection_Access_Type;
      Cmap     : Colormap_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Alloc_Named_Color_Cookie_Type;
   pragma Import (C, Alloc_Named_Color, "xcb_alloc_named_color");

   type Alloc_Color_Cells_Reply_Padding_1_Array_Type is array (0 .. 19) of aliased Interfaces.Unsigned_8;
   type Alloc_Color_Cells_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Pixels_Len    : aliased Interfaces.Unsigned_16;
      Masks_Len     : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Alloc_Color_Cells_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Cells_Reply_Type);

   type Alloc_Color_Cells_Reply_Access_Type is access all Alloc_Color_Cells_Reply_Type;
   for Alloc_Color_Cells_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Alloc_Color_Cells_Reply_Access_Type);

   function Alloc_Color_Cells_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Alloc_Color_Cells_Size_Of, "xcb_alloc_color_cells_sizeof");

   type Alloc_Color_Cells_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Cells_Cookie_Type);

   function Alloc_Color_Cells_Reply
     (C      : Connection_Access_Type;
      Cookie : Alloc_Color_Cells_Cookie_Type;
      Error  : System.Address) return Alloc_Color_Cells_Reply_Access_Type;
   pragma Import (C, Alloc_Color_Cells_Reply, "xcb_alloc_color_cells_reply");

   function Alloc_Color_Cells_Unchecked
     (C          : Connection_Access_Type;
      Contiguous : Interfaces.Unsigned_8;
      Cmap       : Colormap_Id_Type;
      Colors     : Interfaces.Unsigned_16;
      Planes     : Interfaces.Unsigned_16) return Alloc_Color_Cells_Cookie_Type;
   pragma Import (C, Alloc_Color_Cells_Unchecked, "xcb_alloc_color_cells_unchecked");

   function Alloc_Color_Cells
     (C          : Connection_Access_Type;
      Contiguous : Interfaces.Unsigned_8;
      Cmap       : Colormap_Id_Type;
      Colors     : Interfaces.Unsigned_16;
      Planes     : Interfaces.Unsigned_16) return Alloc_Color_Cells_Cookie_Type;
   pragma Import (C, Alloc_Color_Cells, "xcb_alloc_color_cells");

   type Alloc_Color_Planes_Reply_Padding_1_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Alloc_Color_Planes_Reply_Padding_2_Array_Type is array (0 .. 7) of aliased Interfaces.Unsigned_8;
   type Alloc_Color_Planes_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Pixels_Len    : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Alloc_Color_Planes_Reply_Padding_1_Array_Type;
      Red_Mask      : aliased Interfaces.Unsigned_32;
      Green_Mask    : aliased Interfaces.Unsigned_32;
      Blue_Mask     : aliased Interfaces.Unsigned_32;
      Padding_2     : aliased Alloc_Color_Planes_Reply_Padding_2_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Planes_Reply_Type);

   type Alloc_Color_Planes_Reply_Access_Type is access all Alloc_Color_Planes_Reply_Type;
   for Alloc_Color_Planes_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Alloc_Color_Planes_Reply_Access_Type);

   function Alloc_Color_Planes_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Alloc_Color_Planes_Size_Of, "xcb_alloc_color_planes_sizeof");

   type Alloc_Color_Planes_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Alloc_Color_Planes_Cookie_Type);

   function Alloc_Color_Planes_Reply
     (C      : Connection_Access_Type;
      Cookie : Alloc_Color_Planes_Cookie_Type;
      Error  : System.Address) return Alloc_Color_Planes_Reply_Access_Type;
   pragma Import (C, Alloc_Color_Planes_Reply, "xcb_alloc_color_planes_reply");

   function Alloc_Color_Planes_Unchecked
     (C          : Connection_Access_Type;
      Contiguous : Interfaces.Unsigned_8;
      Cmap       : Colormap_Id_Type;
      Colors     : Interfaces.Unsigned_16;
      Reds       : Interfaces.Unsigned_16;
      Greens     : Interfaces.Unsigned_16;
      Blues      : Interfaces.Unsigned_16) return Alloc_Color_Planes_Cookie_Type;
   pragma Import (C, Alloc_Color_Planes_Unchecked, "xcb_alloc_color_planes_unchecked");

   function Alloc_Color_Planes
     (C          : Connection_Access_Type;
      Contiguous : Interfaces.Unsigned_8;
      Cmap       : Colormap_Id_Type;
      Colors     : Interfaces.Unsigned_16;
      Reds       : Interfaces.Unsigned_16;
      Greens     : Interfaces.Unsigned_16;
      Blues      : Interfaces.Unsigned_16) return Alloc_Color_Planes_Cookie_Type;
   pragma Import (C, Alloc_Color_Planes, "xcb_alloc_color_planes");

   function Free_Colors_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Free_Colors_Size_Of, "xcb_free_colors_sizeof");

   function Free_Colors_Checked
     (C             : Connection_Access_Type;
      Cmap          : Colormap_Id_Type;
      Plane_Mask    : Interfaces.Unsigned_32;
      Pixels_Length : Interfaces.Unsigned_32;
      Pixels        : Unsigned_32_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Colors_Checked, "xcb_free_colors_checked");

   function Free_Colors
     (C             : Connection_Access_Type;
      Cmap          : Colormap_Id_Type;
      Plane_Mask    : Interfaces.Unsigned_32;
      Pixels_Length : Interfaces.Unsigned_32;
      Pixels        : Unsigned_32_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Colors, "xcb_free_colors");

   function Store_Colors_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Store_Colors_Size_Of, "xcb_store_colors_sizeof");

   function Store_Colors_Checked
     (C            : Connection_Access_Type;
      Cmap         : Colormap_Id_Type;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Color_Item_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Store_Colors_Checked, "xcb_store_colors_checked");

   function Store_Colors
     (C            : Connection_Access_Type;
      Cmap         : Colormap_Id_Type;
      Items_Length : Interfaces.Unsigned_32;
      Items        : Color_Item_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Store_Colors, "xcb_store_colors");

   function Store_Named_Color_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Store_Named_Color_Size_Of, "xcb_store_named_color_sizeof");

   function Store_Named_Color_Checked
     (C        : Connection_Access_Type;
      Flags    : Interfaces.Unsigned_8;
      Cmap     : Colormap_Id_Type;
      Pixel    : Interfaces.Unsigned_32;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Store_Named_Color_Checked, "xcb_store_named_color_checked");

   function Store_Named_Color
     (C        : Connection_Access_Type;
      Flags    : Interfaces.Unsigned_8;
      Cmap     : Colormap_Id_Type;
      Pixel    : Interfaces.Unsigned_32;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Void_Cookie_Type;
   pragma Import (C, Store_Named_Color, "xcb_store_named_color");

   type Query_Colors_Reply_Padding_1_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type Query_Colors_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Colors_Len    : aliased Interfaces.Unsigned_16;
      Padding_1     : aliased Query_Colors_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Colors_Reply_Type);

   type Query_Colors_Reply_Access_Type is access all Query_Colors_Reply_Type;
   for Query_Colors_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Colors_Reply_Access_Type);

   function Query_Colors_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Query_Colors_Size_Of, "xcb_query_colors_sizeof");

   type Query_Colors_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Colors_Cookie_Type);

   function Query_Colors_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Colors_Cookie_Type;
      Error  : System.Address) return Query_Colors_Reply_Access_Type;
   pragma Import (C, Query_Colors_Reply, "xcb_query_colors_reply");

   function Query_Colors_Unchecked
     (C             : Connection_Access_Type;
      Cmap          : Colormap_Id_Type;
      Pixels_Length : Interfaces.Unsigned_32;
      Pixels        : Unsigned_32_Array_Type) return Query_Colors_Cookie_Type;
   pragma Import (C, Query_Colors_Unchecked, "xcb_query_colors_unchecked");

   function Query_Colors
     (C             : Connection_Access_Type;
      Cmap          : Colormap_Id_Type;
      Pixels_Length : Interfaces.Unsigned_32;
      Pixels        : Unsigned_32_Array_Type) return Query_Colors_Cookie_Type;
   pragma Import (C, Query_Colors, "xcb_query_colors");

   type Lookup_Color_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Exact_Red     : aliased Interfaces.Unsigned_16;
      Exact_Green   : aliased Interfaces.Unsigned_16;
      Exact_Blue    : aliased Interfaces.Unsigned_16;
      Visual_Red    : aliased Interfaces.Unsigned_16;
      Visual_Green  : aliased Interfaces.Unsigned_16;
      Visual_Blue   : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Lookup_Color_Reply_Type);

   type Lookup_Color_Reply_Access_Type is access all Lookup_Color_Reply_Type;
   for Lookup_Color_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Lookup_Color_Reply_Access_Type);

   function Lookup_Color_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Lookup_Color_Size_Of, "xcb_lookup_color_sizeof");

   type Lookup_Color_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Lookup_Color_Cookie_Type);

   function Lookup_Color_Reply
     (C      : Connection_Access_Type;
      Cookie : Lookup_Color_Cookie_Type;
      Error  : System.Address) return Lookup_Color_Reply_Access_Type;
   pragma Import (C, Lookup_Color_Reply, "xcb_lookup_color_reply");

   function Lookup_Color_Unchecked
     (C        : Connection_Access_Type;
      Cmap     : Colormap_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Lookup_Color_Cookie_Type;
   pragma Import (C, Lookup_Color_Unchecked, "xcb_lookup_color_unchecked");

   function Lookup_Color
     (C        : Connection_Access_Type;
      Cmap     : Colormap_Id_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Lookup_Color_Cookie_Type;
   pragma Import (C, Lookup_Color, "xcb_lookup_color");

   function Create_Cursor_Checked
     (C          : Connection_Access_Type;
      Cid        : Cursor_Id_Type;
      Source     : Pixmap_Id_Type;
      Mask       : Pixmap_Id_Type;
      Fore_Red   : Interfaces.Unsigned_16;
      Fore_Green : Interfaces.Unsigned_16;
      Fore_Blue  : Interfaces.Unsigned_16;
      Back_Red   : Interfaces.Unsigned_16;
      Back_Green : Interfaces.Unsigned_16;
      Back_Blue  : Interfaces.Unsigned_16;
      X          : Interfaces.Unsigned_16;
      Y          : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Cursor_Checked, "xcb_create_cursor_checked");

   function Create_Cursor
     (C          : Connection_Access_Type;
      Cid        : Cursor_Id_Type;
      Source     : Pixmap_Id_Type;
      Mask       : Pixmap_Id_Type;
      Fore_Red   : Interfaces.Unsigned_16;
      Fore_Green : Interfaces.Unsigned_16;
      Fore_Blue  : Interfaces.Unsigned_16;
      Back_Red   : Interfaces.Unsigned_16;
      Back_Green : Interfaces.Unsigned_16;
      Back_Blue  : Interfaces.Unsigned_16;
      X          : Interfaces.Unsigned_16;
      Y          : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Cursor, "xcb_create_cursor");

   function Create_Glyph_Cursor_Checked
     (C           : Connection_Access_Type;
      Cid         : Cursor_Id_Type;
      Source_Font : Font_Id_Type;
      Mask_Font   : Font_Id_Type;
      Source_Char : Interfaces.Unsigned_16;
      Mask_Char   : Interfaces.Unsigned_16;
      Fore_Red    : Interfaces.Unsigned_16;
      Fore_Green  : Interfaces.Unsigned_16;
      Fore_Blue   : Interfaces.Unsigned_16;
      Back_Red    : Interfaces.Unsigned_16;
      Back_Green  : Interfaces.Unsigned_16;
      Back_Blue   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Glyph_Cursor_Checked, "xcb_create_glyph_cursor_checked");

   function Create_Glyph_Cursor
     (C           : Connection_Access_Type;
      Cid         : Cursor_Id_Type;
      Source_Font : Font_Id_Type;
      Mask_Font   : Font_Id_Type;
      Source_Char : Interfaces.Unsigned_16;
      Mask_Char   : Interfaces.Unsigned_16;
      Fore_Red    : Interfaces.Unsigned_16;
      Fore_Green  : Interfaces.Unsigned_16;
      Fore_Blue   : Interfaces.Unsigned_16;
      Back_Red    : Interfaces.Unsigned_16;
      Back_Green  : Interfaces.Unsigned_16;
      Back_Blue   : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Create_Glyph_Cursor, "xcb_create_glyph_cursor");

   function Free_Cursor_Checked (C : Connection_Access_Type; Cursor : Cursor_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Cursor_Checked, "xcb_free_cursor_checked");

   function Free_Cursor (C : Connection_Access_Type; Cursor : Cursor_Id_Type) return Void_Cookie_Type;
   pragma Import (C, Free_Cursor, "xcb_free_cursor");

   function Recolor_Cursor_Checked
     (C          : Connection_Access_Type;
      Cursor     : Cursor_Id_Type;
      Fore_Red   : Interfaces.Unsigned_16;
      Fore_Green : Interfaces.Unsigned_16;
      Fore_Blue  : Interfaces.Unsigned_16;
      Back_Red   : Interfaces.Unsigned_16;
      Back_Green : Interfaces.Unsigned_16;
      Back_Blue  : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Recolor_Cursor_Checked, "xcb_recolor_cursor_checked");

   function Recolor_Cursor
     (C          : Connection_Access_Type;
      Cursor     : Cursor_Id_Type;
      Fore_Red   : Interfaces.Unsigned_16;
      Fore_Green : Interfaces.Unsigned_16;
      Fore_Blue  : Interfaces.Unsigned_16;
      Back_Red   : Interfaces.Unsigned_16;
      Back_Green : Interfaces.Unsigned_16;
      Back_Blue  : Interfaces.Unsigned_16) return Void_Cookie_Type;
   pragma Import (C, Recolor_Cursor, "xcb_recolor_cursor");

   type Query_Best_Size_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Width         : aliased Interfaces.Unsigned_16;
      Height        : aliased Interfaces.Unsigned_16;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Best_Size_Reply_Type);

   type Query_Best_Size_Reply_Access_Type is access all Query_Best_Size_Reply_Type;
   for Query_Best_Size_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Best_Size_Reply_Access_Type);

   type Query_Best_Size_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Best_Size_Cookie_Type);

   function Query_Best_Size_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Best_Size_Cookie_Type;
      Error  : System.Address) return Query_Best_Size_Reply_Access_Type;
   pragma Import (C, Query_Best_Size_Reply, "xcb_query_best_size_reply");

   function Query_Best_Size_Unchecked
     (C        : Connection_Access_Type;
      Class    : Interfaces.Unsigned_8;
      Drawable : Drawable_Id_Type;
      Width    : Interfaces.Unsigned_16;
      Height   : Interfaces.Unsigned_16) return Query_Best_Size_Cookie_Type;
   pragma Import (C, Query_Best_Size_Unchecked, "xcb_query_best_size_unchecked");

   function Query_Best_Size
     (C        : Connection_Access_Type;
      Class    : Interfaces.Unsigned_8;
      Drawable : Drawable_Id_Type;
      Width    : Interfaces.Unsigned_16;
      Height   : Interfaces.Unsigned_16) return Query_Best_Size_Cookie_Type;
   pragma Import (C, Query_Best_Size, "xcb_query_best_size");

   type Query_Extension_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Padding_0     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Present       : aliased Interfaces.Unsigned_8;
      Major_Opcode  : aliased Interfaces.Unsigned_8;
      First_Event   : aliased Interfaces.Unsigned_8;
      First_Error   : aliased Interfaces.Unsigned_8;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Extension_Reply_Type);

   type Query_Extension_Reply_Access_Type is access all Query_Extension_Reply_Type;
   for Query_Extension_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Query_Extension_Reply_Access_Type);

   function Query_Extension_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Query_Extension_Size_Of, "xcb_query_extension_sizeof");

   type Query_Extension_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Query_Extension_Cookie_Type);

   function Query_Extension_Reply
     (C      : Connection_Access_Type;
      Cookie : Query_Extension_Cookie_Type;
      Error  : System.Address) return Query_Extension_Reply_Access_Type;
   pragma Import (C, Query_Extension_Reply, "xcb_query_extension_reply");

   function Query_Extension_Unchecked
     (C        : Connection_Access_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Query_Extension_Cookie_Type;
   pragma Import (C, Query_Extension_Unchecked, "xcb_query_extension_unchecked");

   function Query_Extension
     (C        : Connection_Access_Type;
      Name_Len : Interfaces.Unsigned_16;
      Name     : Interfaces.C.Strings.chars_ptr) return Query_Extension_Cookie_Type;
   pragma Import (C, Query_Extension, "xcb_query_extension");

   type List_Extensions_Reply_Padding_0_Array_Type is array (0 .. 23) of aliased Interfaces.Unsigned_8;
   type List_Extensions_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Names_Len     : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Padding_0     : aliased List_Extensions_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Extensions_Reply_Type);

   type List_Extensions_Reply_Access_Type is access all List_Extensions_Reply_Type;
   for List_Extensions_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Extensions_Reply_Access_Type);

   function List_Extensions_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Extensions_Size_Of, "xcb_list_extensions_sizeof");

   type List_Extensions_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Extensions_Cookie_Type);

   function List_Extensions_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Extensions_Cookie_Type;
      Error  : System.Address) return List_Extensions_Reply_Access_Type;
   pragma Import (C, List_Extensions_Reply, "xcb_list_extensions_reply");

   function List_Extensions_Unchecked (C : Connection_Access_Type) return List_Extensions_Cookie_Type;
   pragma Import (C, List_Extensions_Unchecked, "xcb_list_extensions_unchecked");

   function List_Extensions (C : Connection_Access_Type) return List_Extensions_Cookie_Type;
   pragma Import (C, List_Extensions, "xcb_list_extensions");

   function Change_Keyboard_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Keyboard_Mapping_Size_Of, "xcb_change_keyboard_mapping_sizeof");

   function Change_Keyboard_Mapping_Checked
     (C                   : Connection_Access_Type;
      Keycode_Count       : Interfaces.Unsigned_8;
      First_Keycode       : Keycode_Type;
      Keysyms_Per_Keycode : Interfaces.Unsigned_8;
      Keysyms             : access Keysym_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Keyboard_Mapping_Checked, "xcb_change_keyboard_mapping_checked");

   function Change_Keyboard_Mapping
     (C                   : Connection_Access_Type;
      Keycode_Count       : Interfaces.Unsigned_8;
      First_Keycode       : Keycode_Type;
      Keysyms_Per_Keycode : Interfaces.Unsigned_8;
      Keysyms             : access Keysym_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Keyboard_Mapping, "xcb_change_keyboard_mapping");

   type Get_Keyboard_Mapping_Reply_Padding_0_Array_Type is array (0 .. 23) of aliased Interfaces.Unsigned_8;
   type Get_Keyboard_Mapping_Reply_Type is record
      Response_Kind       : aliased Interfaces.Unsigned_8;
      Keysyms_Per_Keycode : aliased Interfaces.Unsigned_8;
      Sequence            : aliased Interfaces.Unsigned_16;
      Length              : aliased Interfaces.Unsigned_32;
      Padding_0           : aliased Get_Keyboard_Mapping_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Keyboard_Mapping_Reply_Type);

   type Get_Keyboard_Mapping_Reply_Access_Type is access all Get_Keyboard_Mapping_Reply_Type;
   for Get_Keyboard_Mapping_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Keyboard_Mapping_Reply_Access_Type);

   function Get_Keyboard_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Keyboard_Mapping_Size_Of, "xcb_get_keyboard_mapping_sizeof");

   type Get_Keyboard_Mapping_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Keyboard_Mapping_Cookie_Type);

   function Get_Keyboard_Mapping_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Keyboard_Mapping_Cookie_Type;
      Error  : System.Address) return Get_Keyboard_Mapping_Reply_Access_Type;
   pragma Import (C, Get_Keyboard_Mapping_Reply, "xcb_get_keyboard_mapping_reply");

   function Get_Keyboard_Mapping_Unchecked
     (C             : Connection_Access_Type;
      First_Keycode : Keycode_Type;
      Count         : Interfaces.Unsigned_8) return Get_Keyboard_Mapping_Cookie_Type;
   pragma Import (C, Get_Keyboard_Mapping_Unchecked, "xcb_get_keyboard_mapping_unchecked");

   function Get_Keyboard_Mapping
     (C             : Connection_Access_Type;
      First_Keycode : Keycode_Type;
      Count         : Interfaces.Unsigned_8) return Get_Keyboard_Mapping_Cookie_Type;
   pragma Import (C, Get_Keyboard_Mapping, "xcb_get_keyboard_mapping");

   function Change_Keyboard_Control_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Keyboard_Control_Size_Of, "xcb_change_keyboard_control_sizeof");

   function Change_Keyboard_Control_Checked
     (C          : Connection_Access_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Keyboard_Control_Checked, "xcb_change_keyboard_control_checked");

   function Change_Keyboard_Control
     (C          : Connection_Access_Type;
      Value_Mask : Interfaces.Unsigned_32;
      Value_List : Value_List_Array) return Void_Cookie_Type;
   pragma Import (C, Change_Keyboard_Control, "xcb_change_keyboard_control");

   type Get_Keyboard_Control_Reply_Padding_0_Array_Type is array (0 .. 1) of aliased Interfaces.Unsigned_8;
   type Get_Keyboard_Control_Reply_Type is record
      Response_Kind      : aliased Interfaces.Unsigned_8;
      Global_Auto_Repeat : aliased Auto_Repeat_Mode_Type;
      Sequence           : aliased Interfaces.Unsigned_16;
      Length             : aliased Interfaces.Unsigned_32;
      Led_Mask           : aliased Interfaces.Unsigned_32;
      Key_Click_Percent  : aliased Interfaces.Unsigned_8;
      Bell_Percent       : aliased Interfaces.Unsigned_8;
      Bell_Pitch         : aliased Interfaces.Unsigned_16;
      Bell_Duration      : aliased Interfaces.Unsigned_16;
      Padding_0          : aliased Get_Keyboard_Control_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Keyboard_Control_Reply_Type);

   type Get_Keyboard_Control_Reply_Access_Type is access all Get_Keyboard_Control_Reply_Type;
   for Get_Keyboard_Control_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Keyboard_Control_Reply_Access_Type);

   function Get_Keyboard_Control_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Keyboard_Control_Size_Of, "xcb_get_keyboard_control_sizeof");

   type Get_Keyboard_Control_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Keyboard_Control_Cookie_Type);

   function Get_Keyboard_Control_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Keyboard_Control_Cookie_Type;
      Error  : System.Address) return Get_Keyboard_Control_Reply_Access_Type;
   pragma Import (C, Get_Keyboard_Control_Reply, "xcb_get_keyboard_control_reply");

   function Get_Keyboard_Control_Unchecked
     (C : Connection_Access_Type) return Get_Keyboard_Control_Cookie_Type;
   pragma Import (C, Get_Keyboard_Control_Unchecked, "xcb_get_keyboard_control_unchecked");

   function Get_Keyboard_Control (C : Connection_Access_Type) return Get_Keyboard_Control_Cookie_Type;
   pragma Import (C, Get_Keyboard_Control, "xcb_get_keyboard_control");

   function Bell_Checked (C : Connection_Access_Type; Percent : Interfaces.Integer_8) return Void_Cookie_Type;
   pragma Import (C, Bell_Checked, "xcb_bell_checked");

   function Bell (C : Connection_Access_Type; Percent : Interfaces.Integer_8) return Void_Cookie_Type;
   pragma Import (C, Bell, "xcb_bell");

   function Change_Pointer_Control_Checked
     (C                        : Connection_Access_Type;
      Acceleration_Numerator   : Interfaces.Integer_16;
      Acceleration_Denominator : Interfaces.Integer_16;
      Threshold                : Interfaces.Integer_16;
      Do_Acceleration          : Interfaces.Unsigned_8;
      Do_Threshold             : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Change_Pointer_Control_Checked, "xcb_change_pointer_control_checked");

   function Change_Pointer_Control
     (C                        : Connection_Access_Type;
      Acceleration_Numerator   : Interfaces.Integer_16;
      Acceleration_Denominator : Interfaces.Integer_16;
      Threshold                : Interfaces.Integer_16;
      Do_Acceleration          : Interfaces.Unsigned_8;
      Do_Threshold             : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Change_Pointer_Control, "xcb_change_pointer_control");

   type Get_Pointer_Control_Reply_Padding_1_Array_Type is array (0 .. 17) of aliased Interfaces.Unsigned_8;
   type Get_Pointer_Control_Reply_Type is record
      Response_Kind            : aliased Interfaces.Unsigned_8;
      Padding_0                : aliased Interfaces.Unsigned_8;
      Sequence                 : aliased Interfaces.Unsigned_16;
      Length                   : aliased Interfaces.Unsigned_32;
      Acceleration_Numerator   : aliased Interfaces.Unsigned_16;
      Acceleration_Denominator : aliased Interfaces.Unsigned_16;
      Threshold                : aliased Interfaces.Unsigned_16;
      Padding_1                : aliased Get_Pointer_Control_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Pointer_Control_Reply_Type);

   type Get_Pointer_Control_Reply_Access_Type is access all Get_Pointer_Control_Reply_Type;
   for Get_Pointer_Control_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Pointer_Control_Reply_Access_Type);

   type Get_Pointer_Control_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Pointer_Control_Cookie_Type);

   function Get_Pointer_Control_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Pointer_Control_Cookie_Type;
      Error  : System.Address) return Get_Pointer_Control_Reply_Access_Type;
   pragma Import (C, Get_Pointer_Control_Reply, "xcb_get_pointer_control_reply");

   function Get_Pointer_Control_Unchecked (C : Connection_Access_Type) return Get_Pointer_Control_Cookie_Type;
   pragma Import (C, Get_Pointer_Control_Unchecked, "xcb_get_pointer_control_unchecked");

   function Get_Pointer_Control (C : Connection_Access_Type) return Get_Pointer_Control_Cookie_Type;
   pragma Import (C, Get_Pointer_Control, "xcb_get_pointer_control");

   function Set_Screen_Saver_Checked
     (C               : Connection_Access_Type;
      Timeout         : Interfaces.Integer_16;
      Interval        : Interfaces.Integer_16;
      Prefer_Blanking : Interfaces.Unsigned_8;
      Allow_Exposures : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Screen_Saver_Checked, "xcb_set_screen_saver_checked");

   function Set_Screen_Saver
     (C               : Connection_Access_Type;
      Timeout         : Interfaces.Integer_16;
      Interval        : Interfaces.Integer_16;
      Prefer_Blanking : Interfaces.Unsigned_8;
      Allow_Exposures : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Screen_Saver, "xcb_set_screen_saver");

   type Get_Screen_Saver_Reply_Padding_1_Array_Type is array (0 .. 17) of aliased Interfaces.Unsigned_8;
   type Get_Screen_Saver_Reply_Type is record
      Response_Kind   : aliased Interfaces.Unsigned_8;
      Padding_0       : aliased Interfaces.Unsigned_8;
      Sequence        : aliased Interfaces.Unsigned_16;
      Length          : aliased Interfaces.Unsigned_32;
      Timeout         : aliased Interfaces.Unsigned_16;
      Interval        : aliased Interfaces.Unsigned_16;
      Prefer_Blanking : aliased Interfaces.Unsigned_8;
      Allow_Exposures : aliased Interfaces.Unsigned_8;
      Padding_1       : aliased Get_Screen_Saver_Reply_Padding_1_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Screen_Saver_Reply_Type);

   type Get_Screen_Saver_Reply_Access_Type is access all Get_Screen_Saver_Reply_Type;
   for Get_Screen_Saver_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Screen_Saver_Reply_Access_Type);

   type Get_Screen_Saver_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Screen_Saver_Cookie_Type);

   function Get_Screen_Saver_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Screen_Saver_Cookie_Type;
      Error  : System.Address) return Get_Screen_Saver_Reply_Access_Type;
   pragma Import (C, Get_Screen_Saver_Reply, "xcb_get_screen_saver_reply");

   function Get_Screen_Saver_Unchecked (C : Connection_Access_Type) return Get_Screen_Saver_Cookie_Type;
   pragma Import (C, Get_Screen_Saver_Unchecked, "xcb_get_screen_saver_unchecked");

   function Get_Screen_Saver (C : Connection_Access_Type) return Get_Screen_Saver_Cookie_Type;
   pragma Import (C, Get_Screen_Saver, "xcb_get_screen_saver");

   function Change_Hosts_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Change_Hosts_Size_Of, "xcb_change_hosts_sizeof");

   function Change_Hosts_Checked
     (C           : Connection_Access_Type;
      Mode        : Interfaces.Unsigned_8;
      Family      : Interfaces.Unsigned_8;
      Address_Len : Interfaces.Unsigned_16;
      Address     : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Hosts_Checked, "xcb_change_hosts_checked");

   function Change_Hosts
     (C           : Connection_Access_Type;
      Mode        : Interfaces.Unsigned_8;
      Family      : Interfaces.Unsigned_8;
      Address_Len : Interfaces.Unsigned_16;
      Address     : Byte_Array_Type) return Void_Cookie_Type;
   pragma Import (C, Change_Hosts, "xcb_change_hosts");

   type List_Hosts_Reply_Padding_0_Array_Type is array (0 .. 21) of aliased Interfaces.Unsigned_8;
   type List_Hosts_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Mode          : aliased Access_Control_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Hosts_Len     : aliased Interfaces.Unsigned_16;
      Padding_0     : aliased List_Hosts_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Hosts_Reply_Type);

   type List_Hosts_Reply_Access_Type is access all List_Hosts_Reply_Type;
   for List_Hosts_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, List_Hosts_Reply_Access_Type);

   function List_Hosts_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, List_Hosts_Size_Of, "xcb_list_hosts_sizeof");

   type List_Hosts_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, List_Hosts_Cookie_Type);

   function List_Hosts_Reply
     (C      : Connection_Access_Type;
      Cookie : List_Hosts_Cookie_Type;
      Error  : System.Address) return List_Hosts_Reply_Access_Type;
   pragma Import (C, List_Hosts_Reply, "xcb_list_hosts_reply");

   function List_Hosts_Unchecked (C : Connection_Access_Type) return List_Hosts_Cookie_Type;
   pragma Import (C, List_Hosts_Unchecked, "xcb_list_hosts_unchecked");

   function List_Hosts (C : Connection_Access_Type) return List_Hosts_Cookie_Type;
   pragma Import (C, List_Hosts, "xcb_list_hosts");

   function Set_Access_Control_Checked
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Access_Control_Checked, "xcb_set_access_control_checked");

   function Set_Access_Control
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Access_Control, "xcb_set_access_control");

   function Set_Close_Down_Mode_Checked
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Close_Down_Mode_Checked, "xcb_set_close_down_mode_checked");

   function Set_Close_Down_Mode
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Set_Close_Down_Mode, "xcb_set_close_down_mode");

   function Kill_Client_Checked
     (C        : Connection_Access_Type;
      Resource : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Kill_Client_Checked, "xcb_kill_client_checked");

   function Kill_Client
     (C        : Connection_Access_Type;
      Resource : Interfaces.Unsigned_32) return Void_Cookie_Type;
   pragma Import (C, Kill_Client, "xcb_kill_client");

   function Rotate_Properties_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Rotate_Properties_Size_Of, "xcb_rotate_properties_sizeof");

   function Rotate_Properties_Checked
     (C         : Connection_Access_Type;
      Window    : Window_Id_Type;
      Atoms_Len : Interfaces.Unsigned_16;
      U_Delta   : Interfaces.Integer_16;
      Atoms     : access Atom_Type) return Void_Cookie_Type;
   pragma Import (C, Rotate_Properties_Checked, "xcb_rotate_properties_checked");

   function Rotate_Properties
     (C         : Connection_Access_Type;
      Window    : Window_Id_Type;
      Atoms_Len : Interfaces.Unsigned_16;
      U_Delta   : Interfaces.Integer_16;
      Atoms     : access Atom_Type) return Void_Cookie_Type;
   pragma Import (C, Rotate_Properties, "xcb_rotate_properties");

   function Force_Screen_Saver_Checked
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Force_Screen_Saver_Checked, "xcb_force_screen_saver_checked");

   function Force_Screen_Saver
     (C    : Connection_Access_Type;
      Mode : Interfaces.Unsigned_8) return Void_Cookie_Type;
   pragma Import (C, Force_Screen_Saver, "xcb_force_screen_saver");

   type Set_Pointer_Mapping_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Status        : aliased Mapping_Status_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Set_Pointer_Mapping_Reply_Type);

   type Set_Pointer_Mapping_Reply_Access_Type is access all Set_Pointer_Mapping_Reply_Type;
   for Set_Pointer_Mapping_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Set_Pointer_Mapping_Reply_Access_Type);

   function Set_Pointer_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Set_Pointer_Mapping_Size_Of, "xcb_set_pointer_mapping_sizeof");

   type Set_Pointer_Mapping_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Set_Pointer_Mapping_Cookie_Type);

   function Set_Pointer_Mapping_Reply
     (C      : Connection_Access_Type;
      Cookie : Set_Pointer_Mapping_Cookie_Type;
      Error  : System.Address) return Set_Pointer_Mapping_Reply_Access_Type;
   pragma Import (C, Set_Pointer_Mapping_Reply, "xcb_set_pointer_mapping_reply");

   function Set_Pointer_Mapping_Unchecked
     (C       : Connection_Access_Type;
      Map_Len : Interfaces.Unsigned_8;
      Map     : access Interfaces.Unsigned_8) return Set_Pointer_Mapping_Cookie_Type;
   pragma Import (C, Set_Pointer_Mapping_Unchecked, "xcb_set_pointer_mapping_unchecked");

   function Set_Pointer_Mapping
     (C       : Connection_Access_Type;
      Map_Len : Interfaces.Unsigned_8;
      Map     : access Interfaces.Unsigned_8) return Set_Pointer_Mapping_Cookie_Type;
   pragma Import (C, Set_Pointer_Mapping, "xcb_set_pointer_mapping");

   type Get_Pointer_Mapping_Reply_Padding_0_Array_Type is array (0 .. 23) of aliased Interfaces.Unsigned_8;
   type Get_Pointer_Mapping_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Map_Len       : aliased Interfaces.Unsigned_8;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
      Padding_0     : aliased Get_Pointer_Mapping_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Pointer_Mapping_Reply_Type);

   type Get_Pointer_Mapping_Reply_Access_Type is access all Get_Pointer_Mapping_Reply_Type;
   for Get_Pointer_Mapping_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Pointer_Mapping_Reply_Access_Type);

   function Get_Pointer_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Pointer_Mapping_Size_Of, "xcb_get_pointer_mapping_sizeof");

   type Get_Pointer_Mapping_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Pointer_Mapping_Cookie_Type);

   function Get_Pointer_Mapping_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Pointer_Mapping_Cookie_Type;
      Error  : System.Address) return Get_Pointer_Mapping_Reply_Access_Type;
   pragma Import (C, Get_Pointer_Mapping_Reply, "xcb_get_pointer_mapping_reply");

   function Get_Pointer_Mapping_Unchecked (C : Connection_Access_Type) return Get_Pointer_Mapping_Cookie_Type;
   pragma Import (C, Get_Pointer_Mapping_Unchecked, "xcb_get_pointer_mapping_unchecked");

   function Get_Pointer_Mapping (C : Connection_Access_Type) return Get_Pointer_Mapping_Cookie_Type;
   pragma Import (C, Get_Pointer_Mapping, "xcb_get_pointer_mapping");

   type Set_Modifier_Mapping_Reply_Type is record
      Response_Kind : aliased Interfaces.Unsigned_8;
      Status        : aliased Mapping_Status_Type;
      Sequence      : aliased Interfaces.Unsigned_16;
      Length        : aliased Interfaces.Unsigned_32;
   end record;
   pragma Convention (C_Pass_By_Copy, Set_Modifier_Mapping_Reply_Type);

   type Set_Modifier_Mapping_Reply_Access_Type is access all Set_Modifier_Mapping_Reply_Type;
   for Set_Modifier_Mapping_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Set_Modifier_Mapping_Reply_Access_Type);

   function Set_Modifier_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Set_Modifier_Mapping_Size_Of, "xcb_set_modifier_mapping_sizeof");

   type Set_Modifier_Mapping_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Set_Modifier_Mapping_Cookie_Type);

   function Set_Modifier_Mapping_Reply
     (C      : Connection_Access_Type;
      Cookie : Set_Modifier_Mapping_Cookie_Type;
      Error  : System.Address) return Set_Modifier_Mapping_Reply_Access_Type;
   pragma Import (C, Set_Modifier_Mapping_Reply, "xcb_set_modifier_mapping_reply");

   function Set_Modifier_Mapping_Unchecked
     (C                     : Connection_Access_Type;
      Keycodes_Per_Modifier : Interfaces.Unsigned_8;
      Keycodes              : access Keycode_Type) return Set_Modifier_Mapping_Cookie_Type;
   pragma Import (C, Set_Modifier_Mapping_Unchecked, "xcb_set_modifier_mapping_unchecked");

   function Set_Modifier_Mapping
     (C                     : Connection_Access_Type;
      Keycodes_Per_Modifier : Interfaces.Unsigned_8;
      Keycodes              : access Keycode_Type) return Set_Modifier_Mapping_Cookie_Type;
   pragma Import (C, Set_Modifier_Mapping, "xcb_set_modifier_mapping");

   type Get_Modifier_Mapping_Reply_Padding_0_Array_Type is array (0 .. 23) of aliased Interfaces.Unsigned_8;
   type Get_Modifier_Mapping_Reply_Type is record
      Response_Kind         : aliased Interfaces.Unsigned_8;
      Keycodes_Per_Modifier : aliased Interfaces.Unsigned_8;
      Sequence              : aliased Interfaces.Unsigned_16;
      Length                : aliased Interfaces.Unsigned_32;
      Padding_0             : aliased Get_Modifier_Mapping_Reply_Padding_0_Array_Type;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Modifier_Mapping_Reply_Type);

   type Get_Modifier_Mapping_Reply_Access_Type is access all Get_Modifier_Mapping_Reply_Type;
   for Get_Modifier_Mapping_Reply_Access_Type'Storage_Size use 0;
   pragma Convention (C, Get_Modifier_Mapping_Reply_Access_Type);

   function Get_Modifier_Mapping_Size_Of (Buffer : System.Address) return Interfaces.C.int;
   pragma Import (C, Get_Modifier_Mapping_Size_Of, "xcb_get_modifier_mapping_sizeof");

   type Get_Modifier_Mapping_Cookie_Type is record
      Sequence : aliased Interfaces.C.unsigned;
   end record;
   pragma Convention (C_Pass_By_Copy, Get_Modifier_Mapping_Cookie_Type);

   function Get_Modifier_Mapping_Reply
     (C      : Connection_Access_Type;
      Cookie : Get_Modifier_Mapping_Cookie_Type;
      Error  : System.Address) return Get_Modifier_Mapping_Reply_Access_Type;
   pragma Import (C, Get_Modifier_Mapping_Reply, "xcb_get_modifier_mapping_reply");

   function Get_Modifier_Mapping_Unchecked
     (C : Connection_Access_Type) return Get_Modifier_Mapping_Cookie_Type;
   pragma Import (C, Get_Modifier_Mapping_Unchecked, "xcb_get_modifier_mapping_unchecked");

   function Get_Modifier_Mapping (C : Connection_Access_Type) return Get_Modifier_Mapping_Cookie_Type;
   pragma Import (C, Get_Modifier_Mapping, "xcb_get_modifier_mapping");

   function No_Operation_Checked (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, No_Operation_Checked, "xcb_no_operation_checked");

   function No_Operation (C : Connection_Access_Type) return Void_Cookie_Type;
   pragma Import (C, No_Operation, "xcb_no_operation");

end XCB;
