del sasm.res
del os.res
del cmdp.res

echo Assembling sasm.asm
copy sasm.asm in.asm
sasm.com
ren a.com sasm.res

echo Assembling os.asm
copy os.asm in.asm
sasm.com
ren a.com os.res

echo Assembling cmdp.asm
copy cmdp.asm in.asm
sasm.com
ren a.com cmdp.res

del in.asm

dir
