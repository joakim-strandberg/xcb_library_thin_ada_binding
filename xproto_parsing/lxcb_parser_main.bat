REM Link step of for xcb_parser_janus.exe
cd obj_janus
link  -subsystem:console -entry:mainCRTStartup -out:.exe %1.obj libcmt.lib kernel32.lib user32.lib -map:%1.map /NODEFAULTLIB:"libc.lib"
move %1.exe ..\xcb_parser_janus.exe
cd ..
