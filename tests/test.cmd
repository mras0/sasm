@echo off
setlocal enabledelayedexpansion
if [%ASM%]==[] set ASM=%~dp0..\build\debug\sasm.exe
if [%1]==[] (set TESTS=%~dp0t*.asm) else (SET TESTS=%1)

set failed=0
for %%f in (%TESTS%) do (
    echo %%f
    call %ASM% "%%f"
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

    rem exit early for now
    if !failed! GEQ 1 echo early exit due to failure & exit /b 1
)

endlocal & exit /b %failed%
