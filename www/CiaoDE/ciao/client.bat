@REM BAT for Running Windows Setup Script (all relative paths) [CLIENT]
@echo off
set CIAOLIB=.
"Win32\bin\ciaoengine.exe" client -C -b "Win32\wsetup.cpx"
regedit "c:/WINDOWS/TEMP/ciaocl.reg"
pause
@echo Done, you can now close this window.
