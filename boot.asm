;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOOT.BIN - Boot sector that loads OS.SYS.     ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The boot sector searches for OS.SYS in the root directory
;; and loads it at linear address 0x0500 (just after the IVT
;; and BIOS data area) and the stack top is set to 0x7c00. This
;; limits the size to around 30K, which should suffice for now.
;; On entry to the loaded code DL contains the boot drive.
;;

        cpu 8086
        org 0x7c00

OS_SEG           equ 0x0050 ; Segment to load OS at
FAT_SEG          equ 0x0800 ; Segment to load FAT at (FAT must be located in first 64K)
SCRATCH_SEG      equ 0x0050 ; Segment in first 64K for 512 byte scratch buffer (before loading O/S)

SECTOR_SIZE      equ 512
PARAS_PER_SEC    equ SECTOR_SIZE/16 ; Sector size in paragraphs
DIR_ENTRY_LCLUST equ 0x1A ; WORD Low word of start cluster (high word only used for FAT32)
DIR_ENTRY_SIZE   equ 0x20

        ;
        ; Entry point
        ;

        jmp short Start     ; Use well known
        nop                 ; instruction sequence

        db 'SDOS 1.0'       ; OEM Name

        ;
        ; 1440 FD BPB (Overwritten by insboot/disktool)
        ;
        dw 512              ; BytesPerSector
SecsPerCluster:
        db 1                ; SectorsPerCluster
ReservedSectors:
        dw 1                ; ReservedSectors
NumFats:
        db 2                ; NumFats
MaxRootEntries:
        dw 224              ; MaxRootEntries
        dw 2880             ; TotalSectors
        db 0xF0             ; MediaDescriptor
SectorsPerFat:
        dw 9                ; SectorsPerFat
SectorsPerTrack:
        dw 18               ; SectorsPerTrack
NumHeads:
        dw 2                ; NumHeads
        dw 0                ; HiddenSectors

Start:
        ; Ensure we're running from a known address
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
        mov bx, FAT_SEG
        mov ax, 1
        mov cx, [SectorsPerFat]
        call ReadSectors

        push dx
        ; Calculate number of root dir entry sectors
        ; (Assumes completely filled sectors)
        mov ax, [MaxRootEntries]
        mov cl, 4 ; log2(SECTOR_SIZE) - log2(DIR_ENTRY_SIZE)
        shr ax, cl
        mov cx, ax

        ; Calculate root sector index
        xor ah, ah
        mov al, [NumFats]
        mul word [SectorsPerFat]
        add ax, [ReservedSectors]

        ; And store index of first data sector
        mov dx, ax
        add dx, cx
        mov [FirstDataSector], dx
        pop dx

.SearchFile:
        push cx
        mov bx, SCRATCH_SEG
        mov cx, 1
        call ReadSectors
        pop cx
        mov bx, SECTOR_SIZE/DIR_ENTRY_SIZE
        mov si, SCRATCH_SEG<<4
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

        ; Failed to find file
        xor ax, ax
        mov si, MsgBootDskErr
        jmp Error

.FileFound:
        mov si, FAT_SEG<<4
        mov cx, OS_SEG
.LoadLoop:
        cmp ax, 0xff0
        jae .LoadDone

        push ax
        mov bx, cx
        xor ch, ch
        mov cl, [SecsPerCluster]
        sub ax, 2 ; First data sector is for cluster 2
        push dx
        xor dx, dx
        mul cx
        pop dx
        add ax, [FirstDataSector]
        call ReadSectors
.A:
        add bx, PARAS_PER_SEC
        dec cl
        jnz .A
        mov cx, bx
        pop ax

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
        mov ax, OS_SEG<<4
        push ax
        ; DL=boot drive
        ret
Error:
        call PutHexWord
        call PutString
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
        mov cl, 4
        shr al, cl
        call PutHexDig
        pop ax
PutHexDig:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe .P
        add al, 7
.P:
        mov ah, 0x0e
        int 0x10
        ret

PutString:
        mov ah, 0x0e
.L:
        lodsb
        and al, al
        jnz .Pr
        ret
.Pr:
        int 0x10
        jmp .L

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
        div word [SectorsPerTrack]
        inc dl
        mov cx, dx
        xor dx, dx
        div word [NumHeads]
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
        add bx, PARAS_PER_SEC
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

FirstDataSector: resw 1

        ; Boot signature
        resb 510-($-$$)
        dw 0xaa55
