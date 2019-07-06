@echo off
setlocal enabledelayedexpansion
if [%ASM%]==[] set ASM=%1
if [%ASM%]==[] set ASM=%~dp0..\build\debug\sasm.exe

set failed=0
for %%f in (%~dp0*.asm) do (
    %ASM% "%%f"
    comp /m "%%~nf.com" "%%~nf.ref" >nul:
    if errorlevel 1 (
        set /a failed=!failed!+1
        echo.
        echo %%~nf failed
        echo.
        echo Reference
        echo.
        ndisasm -b 16 -o 0x100 "%%~nf.ref"
        echo.
        echo Actual
        echo.
        ndisasm -b 16 -o 0x100 "%%~nf.com"
        echo.
        echo.
    )
)

echo Result: %failed%

endlocal & exit /b %failed%
