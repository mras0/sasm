        org 0x0500

SECTOR_SIZE      equ 512

DIR_ENTRY_FNAME  equ 0x00 ; BYTE[8] File name
DIR_ENTRY_EXT    equ 0x08 ; BYTE[3] Extension
DIR_ENTRY_ATTR   equ 0x0B ; BYTE    File attributes
DIR_ENTRY_LCLUST equ 0x1A ; WORD    Low word of start cluster (high word only used for FAT32)
DIR_ENTRY_FSIZE  equ 0x1C ; DWORD   File size
DIR_ENTRY_SIZE   equ 0x20

ROOT_MAX_IDX     equ 0x1C00 ; MaxRootEntries * DIR_ENTRY_SIZE

FAT_RES_SECS     equ 1  ; Number of reserved sectors
FAT_NUM_FATS     equ 2  ; Number of FATS
FAT_SEC_CNT      equ 9  ; SectorsPerFat
FAT_ROOT_SEC     equ 19 ; FAT_RES_SECS + FAT_NUM_FATS * FAT_SEC_CNT
FAT_MAX_ROOTES   equ 14 ; ROOT_MAX_IDX / SECTOR_SIZE
FAT_DATA_SEC     equ 33 ; FAT_ROOT_SEC + FAT_MAX_ROOTES

EOC_CLUSTER      equ 0xFFF ; End of cluster chain marker (other values might be present on disk)

MAX_FILES        equ 8

FILE_INFO_FOFF   equ 0  ; DWORD Current file offset
FILE_INFO_BUFSEG equ 4  ; WORD  Buffer segment
FILE_INFO_BUFOFF equ 6  ; WORD  Offset into buffer
FILE_INFO_BUFSZ  equ 8  ; WORD  Number of valid bytes in buffer
FILE_INFO_CLUST  equ 10 ; WORD  Next cluster to get / Last cluster written
FILE_INFO_DIRENT equ 12 ; WORD  Offset into RootSeg of file entry
FILE_INFO_MODE   equ 14 ; BYTE  Mode (0 = Closed, 1 = Open for reading, 2 = Open for writing)
; Spare          equ 15
FILE_INFO_SIZE   equ 16 ; Size of file info

; Legal values for FILE_INFO_MODE
FMODE_CLOSED     equ 0
FMODE_READ_ONLY  equ 1
FMODE_WRITE_ONLY equ 2

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

NOT_FOUND        equ 0xFFFF ; Returned when entry/pointer not found

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
        mov [es:bx+FILE_INFO_FOFF], ax
        mov [es:bx+FILE_INFO_FOFF+2], ax
        mov [es:bx+FILE_INFO_BUFOFF], ax
        mov [es:bx+FILE_INFO_BUFSZ], ax
        mov [es:bx+FILE_INFO_CLUST], ax
        mov word [es:bx+FILE_INFO_DIRENT], NOT_FOUND
        mov [es:bx+FILE_INFO_MODE], al
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
        cmp bx, NOT_FOUND
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
        mov bx, cs
        mov ds, bx
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

; Read cluster in AX to ES:DI
ReadCluster:
        call ClusterValid
        jc InvalidCluster
        add ax, 31 ; FAT_DATA_SEC - 2
        mov cx, 1
        ; Fall through

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
        mov bx, MsgErrWrite
        jmp Fatal

; Write cluster in AX from ES:DI
; See also ReadCluster
WriteCluster:
        call ClusterValid
        jc InvalidCluster
        add ax, 31 ; FAT_DATA_SEC - 2
        mov cx, 1
        ; Fall through

; Write CX sectors starting from AX from ES:DI
WriteSectors:
        mov [cs:DP_Start], ax
        mov [cs:DP_Count], cx
        mov ax, es
        mov [cs:DP_Seg], ax
        mov [cs:DP_Off], di
        mov dl, [cs:BootDrive]
        mov ah, 0x43
        mov si, DiskPacket
        int 0x13
        jc .DiskWriteErr
        ret
.DiskWriteErr:
        mov bx, MsgErrWrite
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

; Add new cluster to chain in AX
; Returns new cluster in AX
AddCluster:
        push si
        mov si, ax ; Save previous cluster in SI
        mov bx, [cs:FatSeg]
        mov es, bx
        ; First find free cluster
        xor bx, bx ; FAT pointer
        xor cx, cx ; cluster
.Search:
        ; TODO: Handle disk full...
        mov dx, [es:bx]   ; DX=H0L2L1L0
        mov ax, dx        ; AX=H0L2L1L0
        and ax, 0xfff     ; AX=__L2L1L0
        jz .Found
        inc cx
        add bx, 2
        shr dx, 8         ; DX=____H0L2
        mov dh, [es:bx]   ; DX=H2H1H0L2
        shr dx, 4         ; DX=__H2H1H0
        jz .Found
        inc cx
        add bx, 1
        jmp .Search
.Found:
        push cx
        mov ax, si
        call ClusterValid
        jc .Done
        ; mov bx, cx
        ; mov ax, si
        ; call .UpdateCluster
        mov bx, .Msg
        jmp Fatal
.Done:
        pop ax
        push ax
        mov bx, ax
        mov ax, EOC_CLUSTER
        call .UpdateCluster
        pop ax
        pop si
        ret
.Msg: db 'TODO in AddCluster: Update last cluster', 0

; Update cluster in BX to point to AX
; Assumes ES=FatSeg
.UpdateCluster:
        and ax, 0x0FFF ; Ensure we're not messing with bits we shouldn't be
        mov cx, bx
        add bx, bx
        add bx, cx
        mov cx, [es:bx]
        shr bx, 1
        jnc .Even
        shl ax, 4
        and cl, 0xF0
        or al, cl
        jmp .UCDone
.Even:
        and ch, 0x0F
        or ah, ch
.UCDone:
        mov [es:bx], ax
        ret

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
; or 0xFFFF (NOT_FOUND)
; DS must point to cs
FindFileInRoot:
        push si
        push di
        mov ax, [RootSeg]
        mov es, ax
        xor bx, bx
.DirLoop:
        cmp bx, ROOT_MAX_IDX
        jae .NotFound

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
        mov bx, NOT_FOUND
.Done:
        pop di
        pop si
        ret

; Returns pointer to free entry in root directory in ES:BX
; The entry is initialized with CurFileName (but not written to disk)
; Assumes DS=CS
NewRootEntry:
        push si
        push di
        mov bx, [RootSeg]
        mov es, bx
        xor bx, bx
.Search:
        cmp byte [es:bx], 0 ; Maybe also allow '?' as first char (TODO)
        je .Found
        add bx, DIR_ENTRY_SIZE
        cmp bx, ROOT_MAX_IDX
        jae .RootFull
        jmp .Search
.Found:
        ; Copy filename
        mov si, CurFileName
        mov di, bx
        mov cx, 11
        rep movsb
        ; Clear the rest
        xor ax, ax
        mov cx, 21 ; DIR_ENTRY_SIZE - 11
        rep stosb
        mov byte [es:bx+DIR_ENTRY_ATTR], 0x20 ; Set archive bit
        mov word [es:bx+DIR_ENTRY_LCLUST], 0xFFF
        pop di
        pop si
        ret
.RootFull:
        mov bx, MsgErrRootFull
        jmp Fatal

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

; Returns size from file info at ES:BX in DX:AX
; Other registers preserved
GetFileSize:
        mov ax, [es:bx+FILE_INFO_DIRENT]
        push es
        push bx
        mov bx, [RootSeg]
        mov es, bx
        mov bx, ax
        mov ax, [es:bx+DIR_ENTRY_FSIZE]
        mov dx, [es:bx+DIR_ENTRY_FSIZE+2]
        pop bx
        pop es
        ret


; Print root directory
PrintRootDir:
        mov bx, [cs:RootSeg]
        mov es, bx
        xor bx, bx
.L:
        cmp byte [es:bx], 0
        jz .Skip
        push bx
        call PrintDirEntry
        pop bx
.Skip:
        add bx, DIR_ENTRY_SIZE
        cmp bx, ROOT_MAX_IDX
        jb .L
        ret

; Open file from directory entry in ES:BX
; Returns file handle in AX
OpenFileFromDE:
        push si
        push di
        ; Grab needed info from DE
        mov si, [es:bx+DIR_ENTRY_LCLUST]
        mov di, bx
        call GetFileHandle
        ; AX    = FileHandle
        ; ES:BX = FileInfoPtr
        ; SI    = FirstCluster
        ; DI    = Directory entry
        mov [es:bx+FILE_INFO_CLUST], si
        mov [es:bx+FILE_INFO_DIRENT], di
        xor ax, ax
        mov [es:bx+FILE_INFO_FOFF], ax
        mov [es:bx+FILE_INFO_FOFF+2], ax
        mov [es:bx+FILE_INFO_BUFOFF], ax
        mov [es:bx+FILE_INFO_BUFSZ], ax
        ; Make sure some other mode than FMODE_CLOSED is set
        mov byte [es:bx+FILE_INFO_MODE], FMODE_READ_ONLY
        pop di
        pop si
        ret

; Return new file handle in AX and file info in ES:BX
GetFileHandle:
        mov bx, [FileInfoSeg]
        mov es, bx
        xor bx, bx
        xor cx, cx
.Search:
        cmp byte [es:bx+FILE_INFO_MODE], FMODE_CLOSED
        je .Found
        add bx, FILE_INFO_SIZE
        inc cl
        cmp cl, MAX_FILES
        jne .Search
        mov bx, MsgErrFileMax
        jmp Fatal
.Found:
        mov ax, cx
        ret

; Close file handle in BX
CloseFileHandle:
        cmp bx, MAX_FILES
        jae .Error
        mov ax, [FileInfoSeg]
        mov es, ax
        mov ax, FILE_INFO_SIZE
        mul bx
        mov bx, ax
        xor ax, ax
        xchg al, [es:bx+FILE_INFO_MODE] ; Mark file closed
        and al, al
        jz .Error ;Not open
        cmp al, FMODE_READ_ONLY
        je .Done
        call FlushFileBuffer
        call WriteRootDir
        call WriteFAT
.Done:
        ret
.Error:
        mov bx, MsgErrFNotOpen
        jmp Fatal

WriteRootDir:
        mov ax, [cs:RootSeg]
        mov es, ax
        xor di, di
        mov ax, FAT_ROOT_SEC
        mov cx, FAT_MAX_ROOTES
        jmp WriteSectors

WriteFAT:
        mov ax, [cs:FATSeg]
        mov es, ax
        xor di, di
        mov ax, FAT_RES_SECS
        mov cx, FAT_SEC_CNT
        add cx, cx ; Write bot FATs
        jmp WriteSectors

; Flush file (write) buffer for file with info in ES:BX
FlushFileBuffer:
        mov cx, [es:bx+FILE_INFO_BUFOFF]
        and cx, cx
        jnz .Flush
        ret ; Nothing to flush
.Flush:
        ; TODO: Clear from BUFOFF to FILE_BUFFER_SIZE to avoid writing junk to file

        ; Alloc free cluster
        mov ax, [es:bx+FILE_INFO_CLUST]
        push ax
        push bx
        push es
        call AddCluster
        pop es
        pop bx
        pop dx ; Last cluster
        mov [es:bx+FILE_INFO_CLUST], ax

        ; Update directory entry
        push bx
        push si
        push es
        mov si, [es:bx+FILE_INFO_BUFOFF]
        mov bx, [es:bx+FILE_INFO_DIRENT]
        mov cx, [cs:RootSeg]
        mov es, cx
        add [es:bx+DIR_ENTRY_FSIZE], si
        adc word [es:bx+DIR_ENTRY_FSIZE+2], 0
        ; Need to update first cluster ?
        cmp dx, EOC_CLUSTER
        jne .DontUpdate
        mov [es:bx+DIR_ENTRY_LCLUST], ax
.DontUpdate:
        pop es
        pop si
        pop bx

        ; Write buffer to cluster in AX
        push es
        push di
        mov di, [es:bx+FILE_INFO_BUFSEG]
        mov es, di
        xor di, di
        call WriteCluster
        pop di
        pop es
        mov word [es:bx+FILE_INFO_BUFOFF], 0
        mov word [es:bx+FILE_INFO_BUFSZ], FILE_BUFFER_SIZE
        ret

; Write to from file handle in BX, CX bytes from DS:DX
; Returns number of bytes written in AX and carry clear on success
;         carry set on error with error code in AX
; Preserves BX, CX, DX and ES
; Tries to match the expected behavior of Int21_3F
WriteFile:
        push bx
        push cx
        push dx
        push si
        push es

        cmp bx, MAX_FILES
        jae .InvalidHandle

        mov si, cx ; Store original count in si

        push dx
        mov ax, FILE_INFO_SIZE
        mul bx
        mov bx, ax
        mov ax, [cs:FileInfoSeg]
        mov es, ax
        pop dx
        ; ES:BX -> File Info

        cmp byte [es:bx+FILE_INFO_MODE], FMODE_WRITE_ONLY
        jne .InvalidHandle

        ; CX: Number of bytes remaining
        ; DS:DX Source
.WriteLoop:
        and cx, cx
        jz .Done

        mov ax, [es:bx+FILE_INFO_BUFSZ]
        cmp ax, cx
        jbe .DoCopy ; Limited by buffer
        mov ax, cx
.DoCopy:
        ; Copy AX bytes to buffer
        pusha
        push es
        mov cx, ax
        mov di, [es:bx+FILE_INFO_BUFOFF]
        mov si, [es:bx+FILE_INFO_BUFSEG]
        mov es, si
        mov si, dx
        rep movsb
        pop es
        popa

        add dx, ax ; TODO: Handle if this overflows?
        sub cx, ax
        add [es:bx+FILE_INFO_BUFOFF], ax
        sub [es:bx+FILE_INFO_BUFSZ], ax
        jnz .WriteLoop

        mov bx, .MsgFlush
        jmp Fatal
.Done:
        mov ax, si ; All bytes written
        clc
        jmp .Ret
.InvalidHandle:
        mov ax, ERR_INVALID_HAND
        stc
.Ret:
        pop es
        pop si
        pop dx
        pop cx
        pop bx
        ret
.MsgFlush: db 'TODO: WriteFile: Flush!', 0

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

        cmp byte [es:bx+FILE_INFO_MODE], FMODE_READ_ONLY
        jne .InvalidHandle
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
        add dx, ax ; TODO: Handle if this overflows?
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
.InvalidHandle:
        pop es
        pop dx
        pop cx
        pop bx
        mov ax, ERR_INVALID_HAND
        stc
        ret
.Err:
        mov bx, MsgErrReadFile
        jmp Fatal

.MsgUpdateBuffer: db 'TODO: Data left in file buffer...', 0

; Fill buffer of file in ES:BX (buffer assumed to be empty before)
; File pointer is updated
FillFileBuffer:
        pusha
        ; How many bytes left in file?
        call GetFileSize
        mov cx, ax
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
        mov ax, [es:bx+FILE_INFO_CLUST]

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

        mov [es:bx+FILE_INFO_CLUST], ax

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
        cmp ah, 0x40
        je Int21_40
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

        push cx ; File attributes

        call ExpandFName

        mov ax, cs
        mov ds, ax

        ; Find file
        call FindFileInRoot
        cmp bx, NOT_FOUND
        je .NotFound

        ; TODO Truncate file then continue somewhere below
        mov bx, .MsgF
        jmp Fatal
.NotFound:
        call NewRootEntry
        pop cx ; File attributes
        mov [es:bx+DIR_ENTRY_ATTR], cl ; Set attributes

        call OpenFileFromDE
        mov byte [es:bx+FILE_INFO_MODE], FMODE_WRITE_ONLY
        mov word [es:bx+FILE_INFO_BUFSZ], FILE_BUFFER_SIZE
        mov word [es:bx+FILE_INFO_CLUST], EOC_CLUSTER
        ; AX = File handle
        clc

        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC
.MsgF: db 'TODO: File exists in INT21/AH=3Ch', 0

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
        cmp bx, NOT_FOUND
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

; Int 21/AH=40h Write to file or device
; BX    file handle
; CX    number of bytes to write
; DS:DX buffer for data
; Returns Carry clear on success, number of bytes written in AX
;         Carry set on error and error code in AX
Int21_40:
        call WriteFile
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
MsgErrRead:      db 'Error reading from disk', 0
MsgErrWrite:     db 'Error writing to disk', 0
MsgErrOOM:       db 'Out of memory', 0
MsgErrCluster:   db 'Cluster invalid', 0
MsgErrCmdNotF:   db 'Command processor not found', 0
MsgErrFileMax:   db 'Too many open files', 0
MsgErrReadFile:  db 'Error reading from file', 0
MsgErrFNotOpen:  db 'Invalid file handle', 0
MsgErrRootFull:  db 'Root directory full', 0
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

CurFileName:     db '           ' ; 8+3 spaces
