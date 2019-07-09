.PHONY: all test clean

all: sasm.com

clean:
	rm -rf sasm.com

test: all
	DOSBox.exe sasm.com && ndisasm -b 16 -o 0x100 a.com

sasm.com: sasm.asm
	nasm -f bin -o $@ $<
