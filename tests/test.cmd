@echo off
setlocal enabledelayedexpansion
if [%ASM%]==[] set ASM=%1
if [%ASM%]==[] set ASM=%~dp0..\build\debug\sasm.exe

set failed=0
for %%f in (%~dp0*.asm) do (
    %ASM% "%%f"
    if errorlevel 1 (
        set /a failed=!failed!+1
    ) else (
        comp /m "%%~nf.com" "%%~nf.ref" >nul:
        if errorlevel 1 (
            set /a failed=!failed!+1
            ndisasm -b 16 -o 0x100 "%%~nf.ref" > "%TMP%\a.asm"
            ndisasm -b 16 -o 0x100 "%%~nf.com" > "%TMP%\b.asm"
            echo.
            echo %%~nf failed
            echo.
            echo Reference
            echo.
            type "%TMP%\a.asm"
            echo.
            echo Actual
            echo.
            type "%TMP%\b.asm"
            echo.
            echo.
            diff "%TMP%\a.asm" "%TMP%\b.asm"
        )
    )
)

echo Result: %failed%

endlocal & exit /b %failed%
