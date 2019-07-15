        org 0x0500

SECTOR_SIZE      equ 512

DIR_ENTRY_FNAME  equ 0x00 ; BYTE[8] File name
DIR_ENTRY_EXT    equ 0x08 ; BYTE[3] Extension
DIR_ENTRY_ATTR   equ 0x0B ; BYTE    File attributes
DIR_ENTRY_LCLUST equ 0x1A ; WORD    Low word of start cluster (high word only used for FAT32)
DIR_ENTRY_FSIZE  equ 0x1C ; DWORD   File size
DIR_ENTRY_SIZE   equ 0x20

FAT_RES_SECS     equ 1  ; Number of reserved sectors
FAT_NUM_FATS     equ 2  ; Number of FATS
FAT_SEC_CNT      equ 9  ; SectorsPerFat
FAT_ROOT_SEC     equ 19 ; FAT_RES_SECS + FAT_NUM_FATS * FAT_SEC_CNT
FAT_MAX_ROOTES   equ 14 ; MaxRootEntries * DIR_ENTRY_SIZE / SECTOR_SIZE
FAT_DATA_SEC     equ 33 ; FAT_ROOT_SEC + FAT_MAX_ROOTES

MAX_FILES        equ 8  ; Note: Check FileOpenBitmap

FILE_INFO_SIZE   equ 16 ; Size of file info
FILE_INFO_FSIZE  equ 0  ; DWORD File size
FILE_INFO_FOFF   equ 4  ; DWORD Current file offset
FILE_INFO_BUFSEG equ 8  ; WORD  Buffer segment
FILE_INFO_BUFOFF equ 10 ; WORD  Offset into buffer
FILE_INFO_BUFSZ  equ 12 ; WORD  Number of valid bytes in buffer
FILE_INFO_NCLUST equ 14 ; WORD  Next cluster to get

FILE_BUFFER_SIZE equ SECTOR_SIZE

; DOS error codes
ERR_NONE         equ 0x00 ; No error
ERR_FUNC_INV     equ 0x01 ; Function number invalid
ERR_FILE_NOT_FND equ 0x02 ; File not found
ERR_PATH_NOT_FND equ 0x03 ; Path not found
ERR_TOO_MANY_FIL equ 0x04 ; Too many open files
ERR_ACCESS_DENIE equ 0x05 ; Access denied
ERR_INVALID_HAND equ 0x06 ; Invalid handle
ERR_NO_MEM       equ 0x08 ; Insufficient memory

Main:
        ; Save boot drive (passed by boot loader)
        mov [BootDrive], dl

        mov bx, MsgLoading
        call PutString

        ; Set INT 20 vector
        mov word [0x80], Int20Dispatch
        mov word [0x82], 0x00

        ; Set INT 21 vector
        mov word [0x84], Int21Dispatch
        mov word [0x86], 0x00

        mov ax, SECTOR_SIZE
        mov bx, 1
        call MallocBytes
        mov [SectorBufSeg], ax

        mov ax, SECTOR_SIZE
        mov bx, FAT_SEC_CNT
        call Malloc
        mov [FATSeg], ax

        mov ax, MAX_FILES
        mov bx, FILE_INFO_SIZE
        call MallocBytes
        mov [FileInfoSeg], ax
        mov es, ax
        mov cl, MAX_FILES
        xor bx, bx
.InitFile:
        xor ax, ax
        mov [es:bx+FILE_INFO_FSIZE], ax
        mov [es:bx+FILE_INFO_FSIZE+2], ax
        mov [es:bx+FILE_INFO_FOFF], ax
        mov [es:bx+FILE_INFO_FOFF+2], ax
        mov [es:bx+FILE_INFO_BUFOFF], ax
        mov [es:bx+FILE_INFO_BUFSZ], ax
        mov [es:bx+FILE_INFO_NCLUST], ax
        push es
        push bx
        push cx
        mov ax, FILE_BUFFER_SIZE
        mov bx, 1
        call MallocBytes
        pop cx
        pop bx
        pop es
        mov [es:bx+FILE_INFO_BUFSEG], ax
        dec cl
        jnz .InitFile

        ; Read FAT
        xor di, di
        mov ax, [FatSeg]
        mov es, ax
        mov ax, FAT_RES_SECS
        mov cx, FAT_SEC_CNT
        call ReadSectors

        mov ax, FAT_MAX_ROOTES
        mov bx, SECTOR_SIZE
        call MallocBytes
        mov [RootSeg], ax

        ; Read root directory
        mov ax, [RootSeg]
        mov es, ax
        xor di, di
        mov ax, FAT_ROOT_SEC
        mov cx, FAT_MAX_ROOTES
        call ReadSectors

        ; Allocate room for command processor
        mov ax, 0x1000
        call Malloc
        mov [CmdpSeg], ax

        call PutCrLf ; Terminate Loading... message

        ; Find CmdpFName in RootSeg
        mov dx, CmdpFName
        call ExpandFName

        call FindFileInRoot
        cmp bx, 0xFFFF
        jne .FoundCmdP
        mov bx, MsgErrCmdNotF
        jmp Fatal
.FoundCmdP:
        ; Open file and read

        call OpenFileFromDE

        mov bx, ax
        push bx
        push ds
        mov ax, [CmdpSeg]
        mov ds, ax
        mov dx, 0x0100
        mov cx, 0xFF00
        call ReadFile
        pop ds
        pop bx
        ; AX = bytes read

        ; Close again
        call CloseFileHandle

        ; Start command interpreter.
        ; Build PSP (TODO: More...)
        mov ax, [CmdpSeg]
        mov es, ax
        mov word [es:0], 0x20CD ; Int 20h
        mov word [es:2], 0x9FFF ; Segment of first byte beyond memory allocated by program
        mov byte [es:0x80], 0   ; No command line arguments

        ; Match some of the register values (see http://www.fysnet.net/yourhelp.htm)
        cli
        mov ax, [CmdpSeg]
        mov ss, ax
        mov ds, ax
        mov es, ax
        mov dx, ax
        mov sp, 0xFFFE
        xor ax, ax
        xor bx, bx
        mov cx, 0x00FF
        mov si, 0x0100
        mov di, sp
        mov bp, 0x0900
        push bx ; So a local return will execute the INT 20 instruction at [cs:0]
        push dx
        push si
        sti
        retf

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
        ; Fall through

; Print character in AL
; Preserves all normal registers
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
        push si
.Main:
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
        jnz .Main
        pop si
        ret

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

; Alloc AX*BX bytes (Must be < 64K)
MallocBytes:
        mul bx
        and dx, dx
        jnz MallocOOM
        shr ax, 4
        ; Fall through

; Alloc AX paragraphs, returns segment in AX
Malloc:
        mov bx, [FreeSeg]
        add ax, bx
        cmp bx, 0xA000
        jae MallocOOM
        mov [FreeSeg], ax
        mov ax, bx
        ret
MallocOOM:
        mov bx, MsgErrOOM
        jmp Fatal

; Return carry clear if cluster index in AX is valid
ClusterValid:
        cmp ax, 0xFF0
        jae .Invalid
        cmp ax, 2
        jb .Invalid
        clc
        ret
.Invalid:
        stc
        ret


; Return next cluster after AX in AX
NextCluster:
        call ClusterValid
        jc InvalidCluster
        mov bx, [FATSeg]
        mov es, bx
        mov bx, ax
        add bx, bx
        add bx, ax
        shr bx, 1
        mov ax, [es:bx]
        jnc .Even
        shr ax, 4
.Even:
        and ah, 0x0f
        ret
InvalidCluster:
        mov bx, MsgErrCluster
        jmp Fatal

; Read cluster in AX to ES:DI
ReadCluster:
        call ClusterValid
        jc InvalidCluster
        add ax, 31 ; FAT_DATA_SEC - 2
        mov cx, 1
        jmp ReadSectors

; Expand filename in DS:DX to CurFileName
ExpandFName:
        push di
        mov ax, cs
        mov es, ax
        mov di, CurFileName
        mov bx, dx
        xor cx, cx
.FName:
        mov al, [bx]
        cmp al, '.'
        je .Dot
        and al, al
        jz .FillRest
        call .Store
        jmp .FName
.Dot:
        inc bx
.Fill1:
        cmp cl, 8
        je .Ext
        mov al, ' '
        stosb
        inc cl
        jmp .Fill1
.Ext:
        mov al, [bx]
        and al, al
        jz .FillRest
        call .Store
        jmp .Ext
.FillRest:
        cmp cl, 11
        jae .FillDone
        mov al, ' '
        stosb
        inc cl
        jmp .FillRest
.FillDone:
        pop di
        ret
.Store:
        cmp al, 'a'
        jb .R
        cmp al, 'z'
        ja .R
        and al, 0xDF ; to upper case
.R:
        stosb
        inc bx
        inc cl
        ret

; Try to find CurFileName in Root Directory
; Returns pointer to directory entry in BX
; or 0xFFFF
; DS must point to cs
FindFileInRoot:
        push si
        push di
        mov ax, [RootSeg]
        mov es, ax
        xor bx, bx
.DirLoop:
        ; TODO Check if out of range
        mov al, [es:bx]
        and al, al
        jz .NotFound

        mov di, bx
        mov si, CurFileName
        mov cl, 11
.Compare:
        mov al, [es:di]
        cmp [si], al
        jne .NextEntry
        inc si
        inc di
        dec cl
        jnz .Compare

        ; Match!
        jmp .Done
.NextEntry:
        add bx, DIR_ENTRY_SIZE
        jmp .DirLoop
.NotFound:
        mov bx, 0xFFFF
.Done:
        pop di
        pop si
        ret

; Print directory entry in ES:BX
PrintDirEntry:
        push si
        mov cl, 8
        mov si, bx
.Pr:
        mov al, [es:si]
        cmp al, ' '
        je .PrintDot
        call PutChar
        inc si
        dec cl
        jnz .Pr
.PrintDot:
        mov ch, cl
        mov al, '.'
        call PutChar
        mov si, bx
        add si, 8
        mov cl, 3
.Pr2:
        mov al, [es:si]
        cmp al, ' '
        je .PrintDone
        call PutChar
        inc si
        dec cl
        jnz .Pr2
.PrintDone:
        add cl, ch
        inc cl
        mov al, ' '
.Pad:
        call PutChar
        dec cl
        jnz .Pad
.Info:
        mov ax, [es:bx+DIR_ENTRY_FSIZE+2]
        call PutHexWord
        mov ax, [es:bx+DIR_ENTRY_FSIZE]
        call PutHexWord
        mov al, ' '
        call PutChar
        mov al, [es:bx+DIR_ENTRY_ATTR]
        call PutHexByte
        mov al, ' '
        call PutChar
        mov ax, [es:bx+DIR_ENTRY_LCLUST]
        call PutHexWord
        call PutCrLf
        pop si
        ret

; Open file from directory entry in ES:BX
; Returns file handle in AX
OpenFileFromDE:
        push si
        push di
        ; Grab needed info from DE
        mov di, [es:bx+DIR_ENTRY_FSIZE]
        mov cx, [es:bx+DIR_ENTRY_FSIZE+2]
        mov si, [es:bx+DIR_ENTRY_LCLUST]
        push cx
        call GetFileHandle
        pop cx
        push ax
        mov bx, FILE_INFO_SIZE
        mul bx
        mov bx, ax
        mov ax, [FileInfoSeg]
        mov es, ax
        pop ax
        ; AX    = FileHandle
        ; ES:BX = FileInfoPtr
        ; CX:DI = Filesize
        ; SI    = FirstCluster
        mov [es:bx+FILE_INFO_FSIZE], di
        mov [es:bx+FILE_INFO_FSIZE+2], cx
        mov [es:bx+FILE_INFO_NCLUST], si
        xor cx, cx
        mov [es:bx+FILE_INFO_FOFF], cx
        mov [es:bx+FILE_INFO_FOFF+2], cx
        mov [es:bx+FILE_INFO_BUFOFF], cx
        mov [es:bx+FILE_INFO_BUFSZ], cx
        pop di
        pop si
        ret

; Return new file handle in AX
GetFileHandle:
        mov bl, [FileOpenBitmap]
        mov bh, 1
        xor ax, ax
.L:
        mov cl, bl
        and cl, bh
        jz .Found
        inc al
        add bh, bh
        jnz .L
        mov bx, MsgErrFileMax
        jmp Fatal
.Found:
        or [FileOpenBitmap], bh
        ret

; Close file handle in BX
CloseFileHandle:
        mov cl, bl
        mov al, 1
        shl al, cl
        mov ah, [FileOpenBitmap]
        and ah, al
        jnz .OK
        mov bx, MsgErrFNotOpen
        jmp Fatal
.OK:
        sub [FileOpenBitmap], al
        ret


; Read from file handle in BX, CX bytes to DS:DX
; Returns number of bytes read in AX and carry clear on success
;         carry set on error with error code in AX
; Preserves BX, CX, DX and ES
; Tries to match the expected behavior of Int21_3F
ReadFile:
        push bx
        push cx
        push dx
        push es

        push dx
        mov ax, FILE_INFO_SIZE
        mul bx
        mov bx, ax
        mov ax, [cs:FileInfoSeg]
        mov es, ax
        pop dx

        ; ES:BX -> File info

.Again:
        ; First check if there's data in the buffer
        cmp word [es:bx+FILE_INFO_BUFSZ], 0
        jnz .HasData

        ; Fill buffer
        pusha
        push ds
        mov ax, cs
        mov ds, ax
        call FillFileBuffer
        pop ds
        popa
        cmp word [es:bx+FILE_INFO_BUFSZ], 0
        jz .EOF

.HasData:
        mov ax, word [es:bx+FILE_INFO_BUFSZ]
        cmp cx, ax
        jae .CopyFromBuffer
        mov ax, cx

.CopyFromBuffer:
        ; Copy AX bytes from file buffer
        push ax
        push cx
        push di
        push si
        push ds
        push es
        mov cx, ax
        mov di, [es:bx+FILE_INFO_BUFSEG]
        mov si, [es:bx+FILE_INFO_BUFOFF]
        mov ax, ds
        mov es, ax
        mov ds, di
        mov di, dx
        rep movsb
        pop es
        pop ds
        pop si
        pop di
        pop cx
        pop ax

        add [es:bx+FILE_INFO_BUFOFF], ax
        sub [es:bx+FILE_INFO_BUFSZ], ax
        add dx, ax
        sub cx, ax
        jnz .Again
.EOF:
        mov ax, cx ; Record final CX
        pop es
        pop dx
        pop cx
        pop bx
        ; How many bytes did we get out of the wanted ones?
        sub cx, ax
        xchg cx, ax
        clc
        ret

.Err:
        mov bx, cs
        mov ds, bx
        mov bx, MsgErrRead
        jmp Fatal

.MsgUpdateBuffer: db 'TODO: Data left in file buffer...', 0

; Fill buffer of file in ES:BX (buffer assumed to be empty before)
; File pointer is updated
FillFileBuffer:
        pusha
        ; How many bytes left in file?
        mov cx, [es:bx+FILE_INFO_FSIZE]
        mov dx, [es:bx+FILE_INFO_FSIZE+2]
        sub cx, [es:bx+FILE_INFO_FOFF]
        sbb dx, [es:bx+FILE_INFO_FOFF+2]

        and dx, dx
        jnz .Limit
        cmp cx, FILE_BUFFER_SIZE
        jbe .Update
.Limit:
        mov cx, FILE_BUFFER_SIZE
.Update:
        mov word [es:bx+FILE_INFO_BUFOFF], 0
        mov [es:bx+FILE_INFO_BUFSZ], cx
        add [es:bx+FILE_INFO_FOFF], cx
        adc word [es:bx+FILE_INFO_FOFF+2], 0
        and cx, cx ; EOF?
        jz .Done

        ; Assume FILE_BUFFER_SIZE == SECTOR_SIZE
        ; If increased, loop here
        mov ax, [es:bx+FILE_INFO_NCLUST]

        push es
        pusha
        mov di, [es:bx+FILE_INFO_BUFSEG]
        mov es, di
        xor di, di
        call ReadCluster
        popa
        push bx
        call NextCluster
        pop bx
        pop es

        mov [es:bx+FILE_INFO_NCLUST], ax

.Done:
        popa
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main INT 21h dispatch routine
;; Reference: Ralf Browns Interrupt List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Int21Dispatch:
        cld ; Make sure direction flag is always clear

        cmp ah, 0x02
        je Int21_02
        cmp ah, 0x09
        je Int21_09
        cmp ah, 0x3C
        je Int21_3C
        cmp ah, 0x3D
        je Int21_3D
        cmp ah, 0x3E
        je Int21_3E
        cmp ah, 0x3F
        je Int21_3F
        cmp ah, 0x4C
        je Int21_4C

Int21NotImpl:
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

; IRET with carry flag
; TODO: This is really ugly..
; Stack on entry:
; SP+4 Flags
; SP+2 CS
; SP+0 IP
IRETC:
        push bp
        mov bp, sp
        jc .c
        and byte [bp+6], 0xfe ; clear carry
        jmp .Ret
.C:
        or byte [bp+6], 1 ; set carry
.Ret:
        pop bp
        iret

; Int 21/AH=02h Write character to standard output
; DL character to write
; Returns character output in AL
Int21_02:
        mov al, dl
        call PutChar
        jmp IRETC

; Int 21/AH=09h Write string to standard output
; DS:DX points to '$' terminated string
; Returns 24h ('$' in AL)
Int21_09:
        push si
        mov si, dx
.Print:
        lodsb
        cmp al, '$'
        je .Done
        call PutChar
        jmp .Print
.Done:
        pop si
        jmp IRETC

; Int 21/AH=3Ch Create or truncate file
; CX    File attributes
; DS:DX ASCIZ filename
; Returns Carry clear on success, filehandle in AX
;         Carry set on error and error code in AX
Int21_3C:
        push bx
        push cx
        push dx
        push ds
        push es

        pusha
        mov bx, dx
        call PutString
        call PutCrLf
        popa

        call ExpandFName

        mov ax, cs
        mov ds, ax

        ; Find file
        call FindFileInRoot
        cmp bx, 0xFFFF
        jne .Found
        mov bx, .MsgN
        jmp Fatal
.Found:
        mov bx, .MsgF
        jmp Fatal

        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC
.MsgF: db 'TODO: File exists in INT21/AH=3Ch', 0
.MsgN: db 'TODO: File does not exist in INT21/AH=3Ch', 0

; Int 21/AH=3Dh Open existing file
; AL    access and sharing mode
; DS:DX points to ASCIZ filename
; CL    attribute mask of files to look for
; Returns Carry clear on success, filehandle in AX
;         Carry set on error and error code in AX
Int21_3D:
        push bx
        push cx
        push dx
        push ds
        push es

        call ExpandFName

        mov ax, cs
        mov ds, ax

        ; Find file
        call FindFileInRoot
        cmp bx, 0xFFFF
        jne .Found
        mov ax, ERR_FILE_NOT_FND
        stc
        jmp .Out

.Found:
        call OpenFileFromDE

        ; TODO: Register this file with the current process

        ; File handle in AX
        clc
.Out:
        ; Careful not to modify flags here
        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC

; Int 21/AH=3Eh Close file
; BX file handle
; Returns carry clear on success
;         carry set on error and error code in AX
Int21_3E:
        ; TODO: Flush buffers if output file
        pusha
        push ds
        mov ax, cs
        mov ds, ax
        call CloseFileHandle
        pop ds
        popa
        clc ; Assumes we always succeed
        jmp IRETC

; Int 21/AH=3Fh Read from file or device
; BX    file handle
; CX    number of bytes to read
; DS:DX buffer for data
; Returns Carry clear on success, number of bytes read in AX
;         Carry set on error and error code in AX
Int21_3F:
        call ReadFile
        jmp IRETC

Int21_4C:
        push ax
        mov ax, cs
        mov ds, ax
        mov bx, .MsgExited
        call PutString
        pop ax
        call PutHexByte
        call PutCrLf
        mov bx, .MsgTODO
        jmp Fatal
.MsgExited: db 'Program exited with AL=0x', 0
.MsgTODO:   db 'TODO: Handle this...', 0

Int20Dispatch:
        ; INT 20 is the same as INT 21/AX=4C00h
        mov ax, 0x4c00
        jmp Int21Dispatch

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants and data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgLoading:      db 'Loading SDOS 1.0', 0
MsgErrFatal:     db 'Fatal error: ', 0
MsgErrNotSupp:   db 'Not implemented: INT 21h/AH=', 0
MsgErrDisk:      db 'Error reading from disk', 0
MsgErrOOM:       db 'Out of memory', 0
MsgErrCluster:   db 'Cluster invalid', 0
MsgErrCmdNotF:   db 'Command processor not found', 0
MsgErrFileMax:   db 'Too many open files', 0
MsgErrRead:      db 'Error reading from file', 0
MsgErrFNotOpen:  db 'Invalid file handle', 0
;CmdpFName:       db 'CMDP.COM', 0
CmdpFName:       db 'SASM.COM', 0 ; XXX TODO TEMP

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

BootDrive:       db 0
FreeSeg:         dw 0x0800
SectorBufSeg:    dw 0
FATSeg:          dw 0
FileInfoSeg:     dw 0
RootSeg:         dw 0
CmdpSeg:         dw 0

FileOpenBitmap:  db 0 ; Must match MAX_FILES in size
CurFileName:     db '           ' ; 8+3 spaces
