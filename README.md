# SASM

## Introduction

[SASM](sasm.asm) is a self-hosting assembler for real mode x86 for use
in sufficiently DOS-like environments(\*). The supported syntax is
(meant to be) a subset of what [NASM](https://nasm.us/) accepts(\*).

To be self-hosting a very limited O/S is provided that can be used to
develop the assembler on a clean system (requiring only an 8086/8088
processor, approximately 256K of RAM, a CGA video card, a floppy drive
and limited IBM PC compatible BIOS functionality(\*)).

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

## Testing

Download a [release](https://github.com/mras0/sasm/releases) or build
the software yourself (see "Building"). `sasm.com` can be run on most
DOS compatible systems (this should include 32-bit Windows 10, but I
don't currently have such a system to test) like [DOSBox](https://www.dosbox.com/)
or [FreeDOS](http://www.freedos.org/).

A disk image (`disk.img`) containing the bootstrapping environment can
also be downloaded and used with your favorite virtual machine (e.g.
[Bochs](http://bochs.sourceforge.net/), [QEMU](https://www.qemu.org/),
[VirtualBox](https://www.virtualbox.org/) or
[PCem](https://pcem-emulator.co.uk/)).

The disk image needs to be mounted in floppy drive 0 (`A:`) and a CGA
compatible video adapter (e.g. VGA) should be present. 256K
RAM is necessary to be useful (though the bootstrapping process runs
with around 100K you won't be able to edit the larger files).

You can also try out the disk image online at [PCjs
Machines](https://www.pcjs.org/) (Tested 2019-08-23).

- Find a compatible configuration
  (e.g.  [IBM PC (Model 5150), 256Kb RAM, Color Display](https://www.pcjs.org/devices/pcx86/machine/5150/cga/256kb/))
- In the control panel (bottom) select "Browse..." and select the disk
  image.
- Press "Mount" and then the "Reset" or "Ctrl-Alt-Del" button.
- Hint: Press the "4.77 MHz" button a couple of times to get a less
  authentic but more enjoyable experience.


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

The CMake variable `QEMU_EXTRA_ARGS` can be used to provide extra
command line arguments to QEMU (e.g. `-curses`).

### DOS

First make sure you've read the warning section. The bootstrapping
process will use `int 13h/ah=03`, so make sure you're not going to
regret this :)

(Note: This has only been tested with MS-DOS 5.0 and FreeDOS 1.2)

Assemble sasm.asm using either the provided C version of SASM or NASM:

    nasm sasm.asm -o sasm.com

Format a disk for use (drive 0 = A: must be used):

    format /q a:

Copy required files to the disk:

    copy *.asm a:
    copy *.bat a:
    copy LICENSE.md a:
    copy sasm.com a:

Perform bootstrap:

    a:
    strap

The disk should now be ready, reboot and enjoy the same experience as on
a modern, emulated system.

## Supporting Utilities

### C Version of the Assembler

This repository also contains a C version of the assembler, which is
used for bootstrapping on modern systems and testing out
implementation ideas in a rapid(er) development environment. It gives
slightly better error messages than the ASM version and can warn about
unused labels (`-Wunused-label`) and give hints on where to manually
insert `short` for `jmp` (`-Wshort`).

### Disktool

`Disktool` can be used to create FAT12 floppy disk images, install
bootloaders to them and insert / extract files. Call it without
arguments to see a list of command line options.


### Disasm

Simple disassembler. Serves as test bed for the disassembler in
`debug.asm`.
