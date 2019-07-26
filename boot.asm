; The disktool assumes the code starts after the DOS 3.0 BPB
; It also handles putting in the boot signature at the end of the sector
; The file to load is assumed to be placed sequentially from cluster 2 onwards
; and cluster 2 is assumed to be at sector 33 (0x21). It's loaded at
; linear address 0x0500 (just after the IVT) and the stack top is at 0x7c00,
; DL contains the boot drive.

        org 0x7c1e

SEC_PER_TRACK equ 0x7c18
NUM_HEADS     equ 0x7c1a

        ; Ensure we're running from a known address
Start:
        push 0
        push RealStart
        retf
RealStart:
        cld
        cli
        mov ax, cs
        mov ds, ax
        mov es, ax
        mov ss, ax
        mov sp, 0x7c00
        sti

        ; Read sectors
        mov bx, 0x0050 ; Segment to put data
        mov ax, 0x0021 ; Starting LBA
        mov cx, 0x0010 ; Number of sectors, assume 8K is enough
.Read:
        push ax
        push bx
        push cx
        push dx
        mov es, bx
        ; LBA->CHS
        mov di, dx ; Preserve DL
        xor dx, dx
        mov bx, [SEC_PER_TRACK]
        div bx
        inc dl
        mov cx, dx
        xor dx, dx
        mov bx, [NUM_HEADS]
        div bx

        ;pusha
        ;call PutHexWord
        ;mov ax, 0x0e20
        ;int 0x10
        ;mov ax, dx
        ;call PutHexWord
        ;mov ax, 0x0e20
        ;int 0x10
        ;mov al, cl
        ;call PutHexByte
        ;mov ax, 0x0e0d
        ;int 0x10
        ;mov ax, 0x0e0a
        ;int 0x10
        ;popa

        ; CL = sector
        ; DX = head
        ; AX = cyl
        mov bx, di
        mov dh, dl ; DH=head
        mov dl, bl ; DL=drive number
        mov ch, al ; CH=cylinder
        ; ES:BX data buffer
        xor bx, bx
        mov ax, 0x0201 ; AL=number of sectors
        int 0x13
        jc Error
        pop dx
        pop cx
        pop bx
        pop ax
        add bx, 0x20   ; SECTOR_SIZE/0x10
        inc ax
        dec cx
        jnz .Read

        push 0x0500
        ; DL=boot drive
        ret
Error:
        call PutHexWord
        mov si, MsgDiskErr
        call PutString
        xor ax, ax
        int 0x16
        int 0x19

PutHexWord:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
PutHexByte:
        push ax
        shr al, 4
        call PutHexDig
        pop ax
PutHexDig:
        push ax
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe .P
        add al, 7
.P:
        mov ah, 0x0e
        int 0x10
        pop ax
        ret

PutString:
        mov ah, 0x0e
.Pr:
        lodsb
        and al, al
        jz .Done
        int 0x10
        jmp .Pr
.Done:
        ret

MsgDiskErr:   db ' Error reading from disk', 13, 10
              db 'Press any key to reboot', 13, 10, 0
