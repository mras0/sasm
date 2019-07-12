#!/bin/bash -ex
ASM=${ASM-../build/sasm}
for f in t*.asm; do
	${ASM} $f
	N=$(basename $f .asm)
	diff $N.com $N.ref
done
