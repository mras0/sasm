;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INSBOOT.COM - Command line utility to install ;;
;;               a bootloader from a raw binary. ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        cpu 8086
        org 0x100

SECTOR_SIZE equ 512

Start:
        ;
        ; Get input filename from command line
        ;
        mov cl, [0x80]
        mov si, 0x81
.SkipSpace:
        and cl, cl
        jz .InvalidArgs
        dec cl
        lodsb
        cmp al, ' '
        je .SkipSpace
        and cl, cl
        jz .InvalidArgs
        mov di, InFileName
        mov ch, 11
        stosb
.CopyFname:
        lodsb
        cmp al, ' '
        jbe .Done
        stosb
        dec cl
        jz .Done
        dec ch
        jnz .CopyFname
.Done:
        xor al, al
        stosb
        jmp .ArgsOK
.InvalidArgs:
        mov dx, MsgErrArgs
        jmp Error
.ArgsOK:

        ;
        ; Open input file
        ;

        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        jnc .OpenOK
        mov dx, MsgErrInOpen
        jmp Error
.OpenOK:
        mov bx, ax

        ; Ensure buffer is filled with NULs
        mov di, Buffer
        mov cx, SECTOR_SIZE
        xor al, al
        rep stosb

        ;
        ; Read file
        ;

        mov cx, SECTOR_SIZE
        mov ah, 0x3f
        mov dx, Buffer
        int 0x21
        jnc .ReadOK
        mov ah, 0x3e ; Close file
        int 0x21
        mov dx, MsgErrInRead
        jmp Error
.ReadOK:
        push ax
        mov ah, 0x3e ; Close file
        int 0x21
        pop ax

        ;
        ; Figure out current drive
        ;
        mov ah, 0x19
        int 0x21
        ; Too dangerous to allow other drives than 0 for now...
        cmp al, 0
        mov dx, MsgErrDrive
        jne Error

        ;
        ; Write boot sector
        ;
        mov dl, al ; Won't work for hard drives (if C: = 0x02 as usual)
        mov ax, 0x0301
        mov cx, 1 ; Write to first sector (boot sector)
        xor dh, dh
        mov bx, Buffer
        int 0x13
        mov dx, MsgErrWrite
        jc Error
        ret

Error:
        mov ah, 9
        int 0x21
        mov ax, 0x4cff
        int 0x21

MsgErrArgs:     db 'Invalid arguments$'
MsgErrInOpen:   db 'Could not open input file$'
MsgErrInRead:   db 'Could not read from input file$'
MsgErrDrive:    db 'Sorry, will only write boot sector to drive 0 for now$'
MsgErrWrite:    db 'Error while writing boot sector$'

InFileName:     resb 13
Buffer:         resb SECTOR_SIZE
