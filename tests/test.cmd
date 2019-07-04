@echo off
setlocal enabledelayedexpansion
if [%ASM%]==[] set ASM=%1
if [%ASM%]==[] set ASM=%~dp0..\build\debug\sasm.exe

set failed=0
for %%f in (%~dp0*.asm) do (
    %ASM% "%%f"
    comp /m "%%~nf.com" "%%~nf.ref" >nul:
    if errorlevel 1 echo %%~nf failed & set /a failed=!failed!+1
)

echo Result: %failed%

endlocal & exit /b %failed%
