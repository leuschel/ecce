@REM BAT for Running Windows Setup Script (all relative paths) [MAIN]
@echo off
set CIAOLIB=.
"Win32\bin\ciaoengine.exe" main -C -b "Win32\wsetup.cpx"
regedit ciao.reg
pause
@echo Done, you can now close this window.
