# Thin Ada binding to the XCB library
The purpose of the XCB library is to make it possible to make graphical user interfaces in Linux by implementing the client-side of the X11 display server protocol.

This is an Ada binding to the XCB library version 1.10. There are two later versions of the XCB library 1.11.0 and 1.11.1 but binding to those newer libraries has not been done, yet. It has been successfully tested on Mac OS X - El capitan and Ubuntu 14.04.

The Ada binding is contained in the XCB package in the files xcb.ads and xcb.adb. Just include them in your project and you have your Ada binding to the XCB library! (or prefably just with the xcb.gpr file in your project)

The XCB library is a library written in the C programming language. The header files for the XCB library that describe the interface to the library is contained in the files xcb.h and xproto.h. The contents of xproto.h is auto-generated from an xml file called xproto.xml by a Python script. The content of the XCB package is also auto-generated but has been edited manually with contents originating from xcb.h. Note that xproto.h is huge and xcb.h is very small in comparison. In later version of the XCB library the whole interface is auto-generated from xproto.xml.

# Xproto xml file parser
The application that auto-generates the xcb.ads file is included in the source tree. It is included to empower users to change the contents of the XCB package if they wish.

After generating the file, I have manually replaced
   function Query_Text_Extents_Size_Of (Buffer : System.Address) return Interfaces.C.int;
with:
   function Query_Text_Extents_Size_Of (Buffer      : System.Address;
                                        Text_Length : Interfaces.Unsigned_32) return Interfaces.C.int;

The file xproto.xml may be found in the xproto_parsing directory. The idea is to open the file xcb_parser.gpr in the GPS (GNAT Programming Studio), build the sources and then execute the application by clicking the "play" button from inside the GPS (this will make the application execute in the xproto_parsing directory and the application will be able to find the xproto.xml-file).

# Brad Moore's Deepend
The Deepend open source project provides three different implementations of Storage Pools and Subpools: Ada95, Ada2005 and Ada2012 implementations. The xproto.xml file parser is implemented in Ada95 and is an example of how to use the Ada95 implementation of Storage Pools (the package Basic_Bounded_Dynamic_Pools).

# Thanks to
- Dmitry Kazakov for writing his Simple Components. They are used in the xproto.xml-file parser application for reading UTF8-characters.

- David J.A. Koogler for providing feedback and improvement suggestions on the generated Ada code.

- Brad Moore for providing Storage Pool (and Subpool) implementations available in the Deepend Open Source project.
