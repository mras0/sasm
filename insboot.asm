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
BPB_SIZE    equ 0x13
BPB_OFFSET  equ 0x0B

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
        ; Figure out current drive
        ;
        mov ah, 0x19
        int 0x21
        ; Too dangerous to allow other drives than 0 for now...
        cmp al, 0
        je .DriveOK
        mov dx, MsgErrDrive
        jmp Error

.DriveOK:
        ; DL = Drive
        mov dl, al ; Won't work for hard drives (if C: = 0x02 as usual)
        xor dh, dh
        push dx

        ;
        ; Read current BPB
        ;
        mov ax, 0x0201
        mov cx, 1
        mov bx, Buffer
        int 0x13
        jnc .ReadOK
        mov dx, MsgErrRead
        jmp Error
.ReadOK:
        ; Copy it to temp area
        mov di, BPB
        mov si, Buffer+BPB_OFFSET
        mov cx, BPB_SIZE
        rep movsb

        ; Clear buffer
        mov di, Buffer
        mov cx, SECTOR_SIZE
        xor al, al
        rep movsb

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

        ;
        ; Read file
        ;

        mov cx, SECTOR_SIZE
        mov ah, 0x3f
        mov dx, Buffer
        int 0x21
        pushf
        push ax
        mov ah, 0x3e ; Close file
        int 0x21
        pop ax
        popf
        jnc .FileReadOK
        mov dx, MsgErrInRead
        jmp Error
.FileReadOK:

        ;
        ; Perform basic sanity checks of boot sector
        ;
        cmp word [Buffer], 0xEB | (BPB_OFFSET+BPB_SIZE-2)<<8
        jne .NotOK
        cmp byte [Buffer+2], 0x90
        jne .NotOK
        cmp word [Buffer+510], 0xAA55
        je .OK
.NotOK:
        mov dx, MsgErrBootSect
        jmp Error
.OK:

        ;
        ; Copy in old BPB
        ;
        mov si, BPB
        mov di, Buffer+BPB_OFFSET
        mov cx, BPB_SIZE
        rep movsb

        ;
        ; Write boot sector
        ;
        pop dx
        mov ax, 0x0301
        mov cx, 1 ; Write to first sector (boot sector)
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
MsgErrBootSect: db 'Boot sector does not look valid$'
MsgErrRead:     db 'Error reading from disk$'
MsgErrWrite:    db 'Error while writing boot sector$'

InFileName:     resb 13
BPB:            resb BPB_SIZE
Buffer:         resb SECTOR_SIZE
