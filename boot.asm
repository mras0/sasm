; The disktool assumes the code starts after the DOS 3.0 BPB
; It also handles putting in the boot signature at the end of the sector
; The boot sector searches for OS.SYS in the root directory and loads it at
; linear address 0x0500 (just after the IVT) and the stack top is at 0x7c00,
; DL contains the boot drive.

; FAT is kept at 0x8000

        cpu 386 ; 8086 when SASM supports it
        org 0x7c1e

SEC_PER_FAT   equ 0x7c16
SEC_PER_TRACK equ 0x7c18
NUM_HEADS     equ 0x7c1a

FAT_ROOT_SEC     equ 19 ; FAT_RES_SECS + FAT_NUM_FATS * FAT_SEC_CNT
FAT_MAX_ROOTS    equ 14 ; ROOT_MAX_IDX / SECTOR_SIZE
FAT_DATA_SEC     equ 33 ; FAT_ROOT_SEC + FAT_MAX_ROOTS

DIR_ENTRY_LCLUST equ 0x1A ; WORD    Low word of start cluster (high word only used for FAT32)
DIR_ENTRY_SIZE   equ 0x20

        ; Ensure we're running from a known address
Start:
        xor ax, ax
        push ax
        mov ax, RealStart
        push ax
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

        ; Print loading message
        mov si, MsgLoading
        call PutString

        ; Read FAT
        mov bx, 0x0800
        mov ax, 1
        mov cx, [SEC_PER_FAT]
        call ReadSectors

        mov si, 0x0500
        mov ax, FAT_ROOT_SEC
        mov cx, FAT_MAX_ROOTS
.SearchFile:
        push cx
        mov bx, si
        mov cl, 4
        shr bx, cl
        mov cx, 1
        call ReadSectors
        pop cx
        mov bx, 16 ; BytesPerSector / DIR_ENTRY_SIZE
.DirLoop:
        push si
        mov di, FileName
        mov cx, 11
        repe cmpsb
        pop si
        jne .NextDE
        mov ax, [si+DIR_ENTRY_LCLUST]
        jmp .FileFound
.NextDE:
        add si, DIR_ENTRY_SIZE
        dec bx
        jnz .DirLoop
        inc ax
        dec cx
        jnz .SearchFile

        xor ax, ax
        mov si, MsgBootDskErr
        jmp Error

.FileFound:
        mov si, 0x8000
        mov cx, 0x0050 ; Destination segment
.LoadLoop:
        cmp ax, 0xff0
        jae .LoadDone

        push ax
        mov bx, cx
        mov cx, 1
        add ax, 31 ; FAT_DATA_SEC - 2
        call ReadSectors
        mov cx, bx
        pop ax
        add cx, 0x0020 ; BytesPerSector/0x10

        ; Move to next cluster
        mov bx, ax
        add bx, bx
        add bx, ax
        shr bx, 1
        mov ax, [si+bx]
        jnc .Even
        shr ax, 1
        shr ax, 1
        shr ax, 1
        shr ax, 1
.Even:
        and ah, 0x0f
        jmp .LoadLoop
.LoadDone:
        mov ax, 0x0500
        push ax
        ; DL=boot drive
        ret
Error:
        push ax
        call PutString
        mov al, ' '
        call PutChar
        pop ax
        call PutHexWord
        mov si, MsgReboot
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
        shr al, 1
        shr al, 1
        shr al, 1
        shr al, 1
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
.Pr:
        lodsb
        and al, al
        jz .Done
        call PutChar
        jmp .Pr
.Done:
        ret

PutChar:
        push ax
        mov ah, 0x0e
        int 0x10
        pop ax
        ret

; Read CX sectors from LBA AX segment BX
ReadSectors:
        push ax
        push bx
        push cx
        push dx
        push es
.Read:
        push ax
        push bx
        push cx
        push dx
        mov es, bx
        ; LBA->CHS
        mov bx, dx ; Preserve DL
        xor dx, dx
        div word [SEC_PER_TRACK]
        inc dl
        mov cx, dx
        xor dx, dx
        div word [NUM_HEADS]
        ; CL = sector
        ; DX = head
        ; AX = cyl
        mov dh, dl ; DH=head
        mov dl, bl ; DL=drive number
        mov ch, al ; CH=cylinder
        xor bx, bx ; ES:BX data buffer
        mov ax, 0x0201 ; AL=number of sectors
        int 0x13
        jc .Error
        pop dx
        pop cx
        pop bx
        pop ax
        add bx, 0x20   ; SECTOR_SIZE/0x10
        inc ax
        dec cx
        jnz .Read
        pop es
        pop dx
        pop cx
        pop bx
        pop ax
        ret
.Error:
        mov si, MsgDiskErr
        jmp Error

MsgLoading:    db 'Loading SDOS 1.0', 0
MsgDiskErr:    db 13, 10, 'Error reading from disk', 0
MsgBootDskErr: db 13, 10, 'Could not load OS.SYS', 0
MsgReboot:     db 13, 10, 'Press any key to reboot', 13, 10, 0

FileName:     db 'OS      SYS'
