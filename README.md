# Thin Ada binding to the XCB library
The purpose of the XCB library is to make it possible to make graphical user interfaces in Linux by implementing the client-side of the X11 display server protocol.

This is an Ada binding to the XCB library version 1.10. There are two later versions of the XCB library 1.11.0 and 1.11.1 but binding to those newer libraries has not been done, yet. It has been successfully tested on Mac OS X - El capitan and Ubuntu 14.04.

The Ada binding is contained in the XCB package in the files xcb.ads and xcb.adb. Just include them in your project and you have your Ada binding to the XCB library! (or prefably just with the xcb.gpr file in your project)

The XCB library is a library written in the C programming language. The header files for the XCB library that describe the interface to the library is contained in the files xcb.h and xproto.h. The contents of xproto.h is auto-generated from an xml file called xproto.xml by a Python script. The content of the XCB package is also auto-generated but has been edited manually with contents originating from xcb.h. Note that xproto.h is huge and xcb.h is very small in comparison. In later version of the XCB library the whole interface is auto-generated from xproto.xml.
