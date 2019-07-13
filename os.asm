        org 0x0500

Main:
        mov [BootDrive], dl
        mov word [0x84], Int21Dispatch
        mov word [0x86], 0x00

        mov ax, 32 ; 512/16
        call Malloc
        mov [SectorBufSeg], ax

        mov ax, 288 ; 512*9/16
        call Malloc
        mov [FATSeg], ax

        ; Read FAT
        xor di, di
        mov ax, [FatSeg]
        mov es, ax
        mov ax, 1
        mov cx, 9
        call ReadSectors

        ; Read root directory
        mov ax, [SectorBufSeg]
        mov es, ax
        xor di, di
        mov ax, 19
        mov cx, 1
        call ReadSectors

        mov si, [SectorBufSeg]
        shl si, 4
        mov cx, 128
        call HexDump

        jmp Halt

; Halt with error message in BX
Fatal:
        push bx
        mov bx, MsgErrFatal
        call PutString
        pop bx
        call PutString
        call PutCrLf
Halt:
        hlt
        jmp Halt

PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
PutChar:
        pusha
        mov ah, 0x0e
        int 0x10
        popa
        ret

; Print string in BX
PutString:
        mov al, [bx]
        and al, al
        jnz .P
        ret
.P:
        call PutChar
        inc bx
        jmp PutString

; Print word in AX
PutHexWord:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
PutHexByte:
        push ax
        shr al, 4
        call PutHexDigit
        pop ax
PutHexDigit:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe PutChar
        add al, 7
        jmp PutChar

; Dump CX bytes from DS:SI
HexDump:
        mov bx, cx
        cmp bx, 16
        jbe .P
        mov bx, 16
.P:
        sub cx, bx
        mov dx, bx
        xor bx, bx
.H:
        mov al, [si+bx]
        call PutHexByte
        mov al, ' '
        call PutChar
        inc bl
        cmp bl, dl
        jne .H
.S:
        cmp bl, 16
        je .Cs
        mov al, ' '
        call PutChar
        call PutChar
        call PutChar
        inc bl
        jmp .S
.Cs:
        xor bx, bx
.C:
        mov al, [si+bx]
        cmp al, ' '
        jb .Rep
        cmp al, 0x7f
        ja .Rep
        jmp .Print
.Rep:
        mov al, '.'
.Print:
        call PutChar
        inc bl
        cmp bl, dl
        jne .C
        add si, dx
        call PutCrLf
        and cx, cx
        jnz HexDump
        ret



Int21Dispatch:
        push ax
        xor bx, bx
        mov ds, bx
        mov bx, MsgErrNotSupp
        call PutString
        pop ax
        mov al, ah
        call PutHexByte
        call PutCrLf
.Halt:   hlt
        jmp .Halt
        ;iret

; Read CX sectors starting from AX into ES:DI
ReadSectors:
        mov [DP_Start], ax
        mov [DP_Count], cx
        mov ax, es
        mov [DP_Seg], ax
        mov [DP_Off], di
        mov dl, [BootDrive]
        mov ah, 0x42
        mov si, DiskPacket
        int 0x13
        jc .DiskReadErr
        ret
.DiskReadErr:
        mov bx, MsgErrDisk
        jmp Fatal

; Alloc AX paragraphs, returns segment in AX
Malloc:
        mov bx, [FreeSeg]
        add ax, bx
        cmp bx, 0xA000
        jae .OOM
        mov [FreeSeg], ax
        mov ax, bx
        ret
.OOM:
        mov bx, MsgErrOOM
        jmp Fatal

DiskPacket:
        dw 0x0010   ; Size of disk packet
DP_Count:
        dw 0x0001   ; Number of blocks
DP_Off:
        dw 0        ; Offset
DP_Seg:
        dw 0x0000   ; Segment
DP_Start:
        dw 0x0000   ; Starting block number
        dw 0,0,0

MsgErrFatal:     db 'Fatal error: ', 0
MsgErrNotSupp:   db 'Not implemented: INT 21h/AH=', 0
MsgErrDisk:      db 'Error reading from disk', 0
MsgErrOOM:       db 'Out of memory', 0

FreeSeg:         dw 0x0800
SectorBufSeg:    dw 0
FATSeg:          dw 0
BootDrive:       db 0
