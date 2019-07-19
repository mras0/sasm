@echo off
if "%1"=="" echo usage: %0 ASMFILE & exit /b 1
pushd "%~dp0"
del /q a.com 2>nul:
copy /y "%1" a.asm >nul: || ( popd & exit /b 1 )
dosbox.exe -exit sasm.com >nul: || ( popd & exit /b 1 )
rem ndisasm -b 16 -o 0x100 a.com
copy /y a.com "%~n1.com" >nul: || ( popd & exit /b 1 )
popd
