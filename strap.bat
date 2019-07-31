rem Assemble self
sasm.com sasm.asm sasm.com
rem And again (to ensure we're fully bootstrapped)
sasm.com sasm.asm sasm.com
sasm.com boot.asm boot.bin
sasm.com os.asm os.sys
sasm.com cmdp.asm cmdp.com
sasm.com insboot.asm insboot.com
sasm.com uvi.asm uvi.com
echo About to install bootloader. Don't try this on real hardware!
pause
insboot boot.bin
echo Done! Disk should be ready.
