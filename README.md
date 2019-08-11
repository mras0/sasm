# SASM

## Introduction

[SASM](sasm.asm) is a self-hosting assembler for real mode x86 for use
in sufficiently DOS-like environments(\*). The supported syntax is
(meant to be) a subset of what [NASM](https://nasm.us/) accepts(\*).

To be self-hosting a very limited O/S is provided that can be used to
develop the assembler on a clean system (requiring only an 8086/8088
processor, approximately 256K of RAM, a floppy drive and limited IBM PC
compatible BIOS functionality(\*)).

\*: See/use the code to get a better idea of what the
requirements/limitations actually are.

## Warning

Don't run this software in environments where you care about the
consequences (this includes virtual environments where other files could
be corrupted, e.g. under DOSBox). While the code wasn't written with ill
intent in mind, it's only been lightly tested and most likely has severe
disk (and otherwise) corruption bugs, which can potentially render your
system unusable, so please test responsibly (and report any bugs you
find).

## Building

### Modern Systems

Ensure you have the following installed and in your `PATH`:

- CMake (3.7 or later, might work with earlier versions is you tweak
  `CMakeLists.txt`)
- A C compiler that CMake can recognize (might need to set `CC`)
- Optionally [QEMU](https://www.qemu.org/). In particular
  `qemu-system-i386` should be in `PATH`.
- Optionally [DOSBox](https://www.dosbox.com/)

Run the following commands in the source directory:

    mkdir build
    cd build
    cmake ..
    cmake --build .

If everything ran without errors there should be a `disk.img` file
containing the bootstrapping environment as well as `sasm.com` that can
be used directly (it's been assembled by the C version of the assembler
though, so while it should be equivalent to the result of self-hosting
it might differ).

Assuming QEMU support is available, the bootstrapping environment can be
tested:

    cmake --build . --target qemu_test


### DOS

First make sure you've read the warning section. The bootstrapping
process will use `int 13h/ah=03`, so make sure you're not going to
regret this :)

(Note: This has only been tested with MS-DOS 5.0 and FreeDOS 0.84)

Assemble sasm.asm using either the provided C version of SASM or NASM:

    nasm sasm.asm -o sasm.com

Format a disk for use (drive 0 = A: must be used):

    format /q a:

Copy required files to the disk:

    copy *.asm a:
    copy *.bat a:
    copy sasm.com a:

Perform bootstrap:

    a:
    strap

The disk should now be ready, reboot and enjoy the same experience as on
a modern, emulated system.
