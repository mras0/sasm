.PHONY: all test clean

all: sasm.com

clean:
	rm -rf sasm.com

test: all
	DOSBox.exe sasm.com

sasm.com: sasm.asm
	nasm -f bin -o $@ $<
