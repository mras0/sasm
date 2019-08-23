;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OS.SYS - Implements minimal support for the   ;;
;;          DOS syscalls that SASM needs to run. ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        cpu 8086
        org 0x0500

; TODO: Should probably switch to O/S stack when processing syscalls
; TODO: Always push all registers in the interrupt handler and modify
;       AX/flags there (or wherever the return value goes) rather than
;       doing for each syscall handler.

SECTOR_SIZE      equ 512

BPB_SECSPERCLUST equ 0x02 ; BYTE Logical sectors per cluster
BPB_RSRVEDSECS   equ 0x03 ; WORD Number of reserved logical sectors
BPB_NUMFATS      equ 0x05 ; BYTE Number of File Allocation Tables
BPB_MAXROOTENTS  equ 0x06 ; WORD Maximum number of root directory entries
BPB_TOTALSECTORS equ 0x08 ; WORD Total number of sectors on disk
BPB_SECSPERFAT   equ 0x0B ; WORD Logical sectors per FAT
BPB_SECPERTRACK  equ 0x0D ; WORD Physical sectors per track
BPB_NUMHEADS     equ 0x0F ; WORD Number of heads
BPB_SIZE         equ 0x13
BPB_OFFSET       equ 0x7C0B

DIR_ENTRY_FNAME  equ 0x00 ; BYTE[8] File name
DIR_ENTRY_EXT    equ 0x08 ; BYTE[3] Extension
DIR_ENTRY_ATTR   equ 0x0B ; BYTE    File attributes
DIR_ENTRY_FTIME  equ 0x16 ; WORD    File time
DIR_ENTRY_FDATE  equ 0x18 ; WORD    File date
DIR_ENTRY_LCLUST equ 0x1A ; WORD    Low word of start cluster (high word only used for FAT32)
DIR_ENTRY_FSIZE  equ 0x1C ; DWORD   File size
DIR_ENTRY_SIZE   equ 0x20

ENTRY_DELETED    equ 0xE5 ; Special value of DIR_ENTRY_FNAME[0]

EOC_CLUSTER      equ 0xFFF ; End of cluster chain marker (other values might be present on disk)

MAX_FILES        equ 8

FILE_INFO_FOFF   equ 0  ; DWORD Current file offset
FILE_INFO_BUFSEG equ 4  ; WORD  Buffer segment
FILE_INFO_BUFOFF equ 6  ; WORD  Offset into buffer
FILE_INFO_BUFSZ  equ 8  ; WORD  Number of valid bytes in buffer
FILE_INFO_CLUST  equ 10 ; WORD  Next cluster to get / Last cluster written
FILE_INFO_DIRENT equ 12 ; WORD  Offset into RootSeg of file entry
FILE_INFO_MODE   equ 14 ; BYTE  Mode (0 = Closed, 1 = Open for reading, 2 = Open for writing)
FILE_INFO_PSP    equ 15 ; WORD  Owning process PSP
FILE_INFO_SIZE   equ 17 ; Size of file info

; Legal values for FILE_INFO_MODE
FMODE_CLOSED     equ 0
FMODE_READ_ONLY  equ 1
FMODE_WRITE_ONLY equ 2

; Find First offsets into DTA
FF_STEMPLATE     equ 0x01 ; BYTE[11] Search template
FF_SATTR         equ 0x0C ; BYTE     Search attribute mask
FF_ROOT_IDX      equ 0x0D ; WORD     Current index (offset) into RootSeg
FF_FATTR         equ 0x15 ; BYTE     File attribute
FF_FTIME         equ 0x16 ; WORD     File time
FF_FDATE         equ 0x18 ; WORD     File date
FF_FSIZE         equ 0x1A ; DWORD    File size
FF_FNAME         equ 0x1E ; BYTE[13] File name and extension (ASCIIZ with dot)

; Program Segment Prefix offsets
PSP_INT20        equ 0x00 ; WORD  int 0x20 instruction
PSP_MAXSEG       equ 0x02 ; WORD  Segment of first byte beyond memory allocated to program
PSP_OLDINT22     equ 0x0A ; DWORD Old Int22 (Termination handler)
PSP_OLDINT23     equ 0x0E ; DWORD Old Int23 (Ctrl+C handler)
PSP_OLDINT24     equ 0x12 ; DWORD Old Int24 (Critical error handler)
PSP_PARENTPSP    equ 0x16 ; WORD  Parent PSP segment
PSP_OLDSP        equ 0x42 ; WORD  Old SP (0x42-0x4F are reserved)
PSP_OLDSS        equ 0x44 ; WORD  Old SS
PSP_CMDLEN       equ 0x80 ; BYTE  Number of byte in command line
PSP_CMDDAT       equ 0x81 ; BYTE[0x7F] Command Line
PSP_SIZE         equ 0x100

; DOS error codes
ERR_NONE         equ 0x00 ; No error
ERR_FUNC_INV     equ 0x01 ; Function number invalid
ERR_FILE_NOT_FND equ 0x02 ; File not found
ERR_PATH_NOT_FND equ 0x03 ; Path not found
ERR_TOO_MANY_FIL equ 0x04 ; Too many open files
ERR_ACCESS_DENIE equ 0x05 ; Access denied
ERR_INVALID_HAND equ 0x06 ; Invalid handle
ERR_NO_MEM       equ 0x08 ; Insufficient memory
ERR_NO_FILES     equ 0x12 ; No more files

NOT_FOUND        equ 0xFFFF ; Returned when entry/pointer not found

INT20_OFF        equ 0x20*4
INT20_SEG        equ 0x20*4+2
INT21_OFF        equ 0x21*4
INT21_SEG        equ 0x21*4+2
INT22_OFF        equ 0x22*4
INT22_SEG        equ 0x22*4+2

Main:
        ; Save boot drive (passed by boot loader)
        mov [BootDrive], dl

        ; Move stack to free up unused memory
        cli
        mov ax, ProgramEnd
        add ax, 512 ; O/S stack size
        mov sp, ax
        sti
        add ax, 15
        mov cl, 4
        shr ax, cl
        mov [FreeSeg], ax

        ; Get free memory
        int 0x12
        mov cl, 6 ; log2(1024/16)
        shl ax, cl
        mov [MaxSeg], ax

        ; Add dot to loading message
        mov al, '.'
        call PutChar

        ; Copy BPB from boot sector
        push ds
        pop es
        xor ax, ax
        mov ds, ax
        mov di, BPB
        mov si, BPB_OFFSET
        mov cx, BPB_SIZE
        rep movsb
        push cs
        pop ds

        ; Set intterupt vectors
        xor ax, ax
        mov word [INT20_OFF], Int20Dispatch
        mov word [INT20_SEG], ax
        mov word [INT21_OFF], Int21Dispatch
        mov word [INT21_SEG], ax
        mov word [INT22_OFF], DefaultTerminate
        mov word [INT22_SEG], ax

        mov ax, SECTOR_SIZE
        mov bx, [BPB+BPB_SECSPERFAT]
        call Malloc
        mov [FATSeg], ax

        xor dx, dx
        xor ah, ah
        mov al, [BPB+BPB_SECSPERCLUST]
        mov bx, SECTOR_SIZE
        mul bx
        mov [ClusterBytes], ax

        mov ax, [BPB+BPB_MAXROOTENTS]
        mov cl, 5
        shl ax, cl
        mov [RootMaxIdx], ax

        call GetFatRootSec
        mov dx, ax
        call GetFatNRootSecs
        add ax, dx
        mov [DataSector], ax

        mov ax, [BPB+BPB_TOTALSECTORS]
        sub ax, [DataSector]
        xor dx, dx
        xor bh, bh
        mov bl, [BPB+BPB_SECSPERCLUST]
        div bx
        add ax, 2 ; +2 since (valid) data clusters are offset by 2
        mov [MaxCluster], ax

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
        mov [es:bx+FILE_INFO_PSP], ax
        push es
        push bx
        push cx
        mov ax, [ClusterBytes]
        mov bx, 1
        call MallocBytes
        pop cx
        pop bx
        pop es
        mov [es:bx+FILE_INFO_BUFSEG], ax
        add bx, FILE_INFO_SIZE
        dec cl
        jnz .InitFile

        ; Read FAT
        xor di, di
        mov ax, [FATSeg]
        mov es, ax
        mov ax, [BPB+BPB_RSRVEDSECS]
        mov cx, [BPB+BPB_SECSPERFAT]
        call ReadSectors

        call GetFatNRootSecs
        mov bx, SECTOR_SIZE
        call MallocBytes
        mov [RootSeg], ax

        ; Add dot to loading message
        mov al, '.'
        call PutChar

        ; Read root directory
        mov ax, [RootSeg]
        mov es, ax
        xor di, di
        call GetFatNRootSecs
        mov cx, ax
        call GetFatRootSec
        call ReadSectors

        call PutCrLf ; Terminate Loading... message

        ; Load command interpreter
        mov dx, CmdpFName
        call ExpandFName
        call LoadProgram
        and ax, ax
        jnz .LoadOK
        mov bx, MsgErrCmdNotF
        jmp short Fatal
.LoadOK:
        mov [CmdpSeg], ax
        ; Start command interpreter
        call StartProgram ; Shouldn't return

        mov bx, MsgErrCmdpRet
        ; Fall through

; Halt with error message in BX
Fatal:
        push cs
        pop ds
        push bx
        mov bx, cs
        mov ds, bx
        mov bx, MsgErrFatal
        call PutString
        pop bx
        call PutString
        call PutCrLf
        mov bx, MsgReboot
        call PutString
        xor ax, ax
        int 0x16
        int 0x19

PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
        ; Fall through

; Print character in AL
; Preserves all normal registers
PutChar:
        push ax
        push bx
        push bp ; Some BIOSes destroy BP when the screen scrolls
        mov bx, 7
        mov ah, 0x0e
        int 0x10
        pop bp
        pop bx
        pop ax
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

%if 0
; Print word in AX
PutHexWord:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
%endif
PutHexByte:
        push ax
        shr al, 1
        shr al, 1
        shr al, 1
        shr al, 1
        call PutHexDigit
        pop ax
PutHexDigit:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe PutChar
        add al, 7
        jmp PutChar

; Convert LBA in AX to CHS in CX/DX
LBAtoCHS:
        push ax
        xor dx, dx
        div word [BPB+BPB_SECPERTRACK]
        inc dl
        mov cl, dl          ; CL=sector
        xor dx, dx
        div word [BPB+BPB_NUMHEADS]
        mov dh, dl          ; DH=head
        mov dl, [BootDrive] ; DL=drive number
        mov ch, al          ; CH=cylinder
        pop ax
        ret

; AX = FAT_RES_SECS + FAT_NUM_FATS * FAT_SEC_CNT
GetFatRootSec:
        push dx
        xor dx, dx
        xor ah, ah
        mov al, [cs:BPB+BPB_NUMFATS]
        mul word [cs:BPB+BPB_SECSPERFAT]
        add ax, [cs:BPB+BPB_RSRVEDSECS]
        pop dx
        ret

; AX = FAT_MAX_ROOTS
GetFatNRootSecs:
        mov ax, [cs:BPB+BPB_MAXROOTENTS]
        shr ax, 1
        shr ax, 1
        shr ax, 1
        shr ax, 1
        ret


; Adjust cluster in AX to match LBA and set CX to sector count
; AX = FAT_ROOT_SEC + FAT_MAX_ROOTS - 2
GetClusterParams:
        xor ch, ch
        mov cl, [cs:BPB+BPB_SECSPERCLUST]
        sub ax, 2
        push dx
        xor dx, dx
        mul cx
        add ax, [cs:DataSector]
        pop dx
        ; CX = SectorsPerCluster
        ; AX = DATA_SECTOR + (cluster - 2) * SectorsPerCluster
        ret

; Read cluster in AX to ES:DI
ReadCluster:
        call ClusterValid
        jnc .ClusterValid
        jmp InvalidCluster
.ClusterValid:
        call GetClusterParams
        ; Fall through

; Read CX sectors starting from AX into ES:DI
ReadSectors:
        push ax
        push bx
        push cx
        push dx
        push es
.Read:
        push cx
        call LBAtoCHS
        push ax
        mov bx, di
        mov ax, 0x0201
        int 0x13
        jc .DiskReadErr
        pop ax
        pop cx
        mov bx, es
        add bx, SECTOR_SIZE/0x10
        mov es, bx
        inc ax
        dec cx
        jnz .Read
        pop es
        pop dx
        pop cx
        pop bx
        pop ax
        ret
.DiskReadErr:
        mov al, ah
        call PutHexByte
        mov al, ' '
        call PutChar
        mov bx, MsgErrRead
        jmp Fatal

; Write cluster in AX from ES:DI
; See also ReadCluster
WriteCluster:
        call ClusterValid
        jnc .ClusterValid
        jmp InvalidCluster
.ClusterValid:
        call GetClusterParams
        ; Fall through

; Write CX sectors starting from AX from ES:DI
WriteSectors:
        push ds
        push es
        push ax
        push bx
        push cx
        push dx
        mov dx, cs
        mov ds, dx
.Write:
        push cx
        call LBAtoCHS
        push ax
        mov bx, di
        mov ax, 0x0301
        int 0x13
        jc .DiskWriteErr
        pop ax
        pop cx
        mov bx, es
        add bx, SECTOR_SIZE/0x10
        mov es, bx
        inc ax
        dec cx
        jnz .Write
        pop dx
        pop cx
        pop bx
        pop ax
        pop es
        pop ds
        ret
.DiskWriteErr:
        mov al, ah
        call PutHexByte
        mov al, ' '
        call PutChar
        mov bx, MsgErrWrite
        jmp Fatal

; Alloc AX*BX bytes (Must be < 64K)
MallocBytes:
        mul bx
        and dx, dx
        jnz MallocOOM
        mov cl, 4
        shr ax, cl
        ; Fall through

; Alloc AX paragraphs, returns segment in AX
Malloc:
        mov bx, [FreeSeg]
        add ax, bx
        cmp ax, [MaxSeg]
        ja MallocOOM
        mov [FreeSeg], ax
        mov ax, bx
        ret
MallocOOM:
        mov bx, MsgErrOOM
        jmp Fatal


; Load program from CurFileName
; Returns AX=Loaded segment (0 on error)
; Assumes CS=DS
LoadProgram:
        call FindFileInRoot
        cmp bx, NOT_FOUND
        jne .Found
        xor ax, ax
        ret
.Found:
        push bx
        push es

        mov ax, [MaxSeg]
        sub ax, [FreeSeg]
        cmp ax, 0x20
        jb MallocOOM
        mov bx, 0x1000
        cmp ax, bx
        jbe .SizeLimited
        mov ax, bx
.SizeLimited:
        mov bp, ax
        sub bp, PSP_SIZE/0x10 + 1 ; Need room for PSP and a little stack
        mov cl, 4
        shl bp, cl ; BP = Maximum bytes to read

        mov bx, ax
        shl bx, cl
        sub bx, 4 ; Make room for 2 words on stack (see below)
        mov [ChildSSSP], bx

        ; Allocate room
        call Malloc

        mov [ChildSSSP+2], ax
        mov word [ChildCSIP], 0x0100
        mov [ChildCSIP+2], ax

        ; Build PSP (TODO: More...)
        mov es, ax
        mov word [es:PSP_INT20], 0x20CD ; Int 20h
        mov bx, [MaxSeg]
        mov word [es:PSP_MAXSEG], bx     ; Segment of first byte beyond memory allocated by program
        mov byte [es:PSP_CMDLEN], 0      ; No command line arguments
        mov bx, [CurrentPSP]
        mov [es:PSP_PARENTPSP], bx       ; Store parent PSP
        ; Store old interrupt vectors
        mov si, INT22_OFF
        mov di, PSP_OLDINT22
        mov cx, 3*2
        rep movsw

        ; Ensure there's a 0 at the bottom of the stack
        ; So a local return will execute the INT 20 instruction at [cs:0]
        ; And the initial value of AX
        mov di, [ChildSSSP]
        xor ax, ax
        stosw ; initial AX=0 is more compatible
        stosw

        ; Point DTA at PSP:80h
        mov [DTA+2], es
        mov word [DTA], PSP_CMDLEN

        ; Update CurrentPSP
        mov [CurrentPSP], es
        pop es
        pop bx

        ; Open file and read
        call OpenFileFromDE

        mov bx, ax
        push bx
        push ds
        mov ds, [CurrentPSP]
        mov dx, PSP_SIZE
        mov cx, bp
        call ReadFile
        jnc .ReadOK
.ReadErr:
        mov bx, MsgErrRead
        jmp Fatal
.ReadOK:
        cmp ax, bp ; Read maximum bytes?
        jb .ReadDone
        mov cx, 1  ; Check that we're at EOF
        call ReadFile
        jc .ReadErr
        and ax, ax
        jz .ReadDone
        mov bx, MsgErrProgLoad ; File not completely read
        jmp Fatal
.ReadDone:
        pop ds
        pop bx
        ; AX = bytes read

        ; Close again
        call CloseFileHandle

        ; Return with segment in ax
        mov ax, [CurrentPSP]
        ret

; Start loaded program at segment AX
StartProgram:
        cli
        mov ds, ax
        ; Save stack pointer
        mov [PSP_OLDSP], sp
        mov [PSP_OLDSS], ss
        ; Match some of the register values (see http://www.fysnet.net/yourhelp.htm)
        mov ss, [cs:ChildSSSP+2]
        mov es, ax
        mov dx, [cs:ChildCSIP+2]
        mov sp, [cs:ChildSSSP]
        mov cx, 0x00FF
        mov si, [cs:ChildCSIP]
        mov di, sp
        mov bp, 0x0900
        pop bx ; Get initial value of AX
        mov ax, 0x0202 ; Interrupts enabled
        push ax ; Push flags
        xor ax, ax
        xchg ax, bx
        push dx
        push si
        iret
        ret

; Return carry clear if cluster index in AX is valid
ClusterValid:
        cmp ax, [cs:MaxCluster]
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
        mov cl, 4
        shr ax, cl
.Even:
        and ah, 0x0f
        ret
InvalidCluster:
        mov bx, MsgErrCluster
        jmp Fatal

; Add new cluster to chain in AX
; Returns new cluster in AX (or 0 if none could be found)
AddCluster:
        push si
        mov si, ax ; Save previous cluster in SI
        mov es, [cs:FATSeg]
        ; First find free cluster
        xor bx, bx ; FAT pointer
        xor cx, cx ; cluster
.Search:
        mov dx, [es:bx]   ; DX=H0L2L1L0
        mov ax, dx        ; AX=H0L2L1L0
        and ax, 0xfff     ; AX=__L2L1L0
        jz .Found
        inc cx
        add bx, 2
        mov dl, dh        ; DX=____H0L2
        mov dh, [es:bx]   ; DX=H2H1H0L2
        shr dx, 1         ; DX=__H2H1H0
        shr dx, 1
        shr dx, 1
        shr dx, 1
        jz .Found
        inc cx
        add bx, 1
        cmp cx, [cs:MaxCluster]
        jb .Search
        ; None found - disk is full
        xor ax, ax
        jmp short .Ret
.Found:
        push cx
        mov ax, si
        call ClusterValid
        jc .Done
        ; Update last cluster
        mov bx, ax
        mov ax, cx
        call UpdateCluster
.Done:
        pop ax
        push ax
        mov bx, ax
        mov ax, EOC_CLUSTER
        call UpdateCluster
        pop ax
.Ret:
        pop si
        ret

; Update cluster in BX to point to AX
; Assumes ES=FatSeg
UpdateCluster:
        and ax, 0x0FFF ; Ensure we're not messing with bits we shouldn't be
        mov cx, bx
        add bx, bx
        add bx, cx
        shr bx, 1
        mov cx, [es:bx]
        jnc .Even
        shl ax, 1
        shl ax, 1
        shl ax, 1
        shl ax, 1
        and cl, 0x0F
        or al, cl
        jmp short .UCDone
.Even:
        and ch, 0xF0
        or ah, ch
.UCDone:
        mov [es:bx], ax
        ret

; Free cluster chain in AX
FreeClusterChain:
        call ClusterValid
        jnc .Free
        ret
.Free:
        push ax
        call NextCluster
        pop bx
        push ax
        xor ax, ax ; Mark as free
        call UpdateCluster
        pop ax
        jmp FreeClusterChain

; Compress filename in DS:SI to ES:DI
CompressFName:
        push si
        push di
        push si
        mov cl, 8
.Name:
        lodsb
        cmp al, ' '
        je .NameDone
        stosb
        dec cl
        jnz .Name
.NameDone:
        mov al, '.'
        stosb
        pop si
        add si, 8
        mov cl, 3
.Ext:
        lodsb
        cmp al, ' '
        je .ExtDone
        stosb
        dec cl
        jnz .Ext
.ExtDone:
        xor al, al
        stosb
        pop di
        pop si
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
        ; If the first character is 0xE5 replace it with 0x05
        cmp byte [es:CurFileName], ENTRY_DELETED
        jne .ND
        mov byte [es:CurFileName], 0x05
.ND:
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

; Expand file specification in DS:DX to Find file template in DTA (ES:BX)
; Follows the algorithm from https://devblogs.microsoft.com/oldnewthing/20071217-00/?p=24143
; Preserves DS:DX and ES:BX
ExpandFSpec:
        push bx
        push si
        push di

        mov si, dx
        add bx, FF_STEMPLATE

        ; 1. Start with 11 spaces and the cursor at position 1
        mov di, bx
        mov cx, 11
        mov al, ' '
        rep stosb

        mov di, bx ; Cursor at position 1

.ReadChar:
        ; 2. Read a character from input and stop if at end
        lodsb
        and al, al
        jz .Stop

        ; 3. If the next character is a dot then set extension to '   ',
        ;    mov the cursor to position 9 and go to step 2
        cmp al, '.'
        jne .CheckAsterisk
        mov word [es:bx+8], '  '
        mov byte [es:bx+10], ' '
        mov di, bx
        add di, 8
        jmp .ReadChar

.CheckAsterisk:
        ; 4. If the next character is an asterisk, then fill the rest of
        ;    the pattern with question marks and move the cursor past the end
        ;    and go to step 2
        cmp al, '*'
        jne .NotAsterisk
        mov cx, bx
        sub cx, di
        add cx, 11
        mov al, '?'
        rep stosb
        mov di, bx
        add di, 11
        jmp .ReadChar

.NotAsterisk:
        ; 5. If not past the end, copy the character, and go to step 2
        mov cx, di
        sub cx, bx
        cmp cl, 11
        jae .ReadChar
        ; ToUpper
        cmp al, 'a'
        jb .Store
        cmp al, 'z'
        ja .Store
        and al, 0xDF
.Store:
        stosb
        jmp .ReadChar

.Stop:
        pop di
        pop si
        pop bx
        ret

; Search for next file matching DS:SI from Directory in ES:BX
; Return pointer to next matching file in ES:BX or
; BX = 0xFFFF (NOT_FOUND) if no match is found.
FindNextFile:
        push si
        push di
        mov dx, si ; Preserve original filename in DX
.DirLoop:
        cmp bx, [cs:RootMaxIdx]
        jae .NotFound

        mov al, [es:bx]
        and al, al
        jz .NextEntry
        cmp al, ENTRY_DELETED
        je .NextEntry

        mov di, bx
        mov si, dx
        mov cl, 11
.Compare:
        lodsb
        cmp al, '?'
        je .Wild
        cmp al, [es:di]
        jne .NextEntry
.Wild:
        inc di
        dec cl
        jnz .Compare

        ; Match!
        jmp short .Done
.NextEntry:
        add bx, DIR_ENTRY_SIZE
        jmp .DirLoop
.NotFound:
        mov bx, NOT_FOUND
.Done:
        pop di
        pop si
        ret

; Try to find CurFileName in Root Directory
; Returns pointer to directory entry in BX
; or 0xFFFF (NOT_FOUND)
; DS must point to CS
FindFileInRoot:
        push si
        mov ax, [RootSeg]
        mov es, ax
        xor bx, bx
        mov si, CurFileName
        call FindNextFile
        pop si
        ret

; Search for next file based on Find File structure in ES:BX
; Returns carry clear if file found
;         carry set and error code in AX if not found
DoFindFile:
        push si
        push di

.Redo:
        ; CX = Where to start search
        ; FF_ROOT_IDX points to last checked entry
        mov cx, [es:bx+FF_ROOT_IDX]
        add cx, DIR_ENTRY_SIZE

        push es
        push bx
        ; Point DS:SI at search template
        mov ax, es
        mov ds, ax
        mov si, bx
        add si, FF_STEMPLATE
        ; Point ES:BX at root dir
        mov ax, [cs:RootSeg]
        mov es, ax
        mov bx, cx
        call FindNextFile
        mov si, es
        mov ds, si
        mov si, bx
        ; DS:SI = DirEntry
        pop bx
        pop es
        ; ES:BX = Find File structure

        mov [es:bx+FF_ROOT_IDX], si ; Preserve updated root pointer

        cmp si, NOT_FOUND
        je .NotFound

        ; Check if the attribute filter matches
        mov al, [si+DIR_ENTRY_ATTR]
        and al, [es:bx+FF_SATTR]
        jnz .Redo

        ; Copy dir entry data to find file structure
        mov ax, [si+DIR_ENTRY_FTIME]
        mov [es:bx+FF_FTIME], ax
        mov ax, [si+DIR_ENTRY_FDATE]
        mov [es:bx+FF_FDATE], ax
        mov ax, [si+DIR_ENTRY_FSIZE]
        mov [es:bx+FF_FSIZE], ax
        mov ax, [si+DIR_ENTRY_FSIZE+2]
        mov [es:bx+FF_FSIZE+2], ax

        mov di, bx
        add di, FF_FNAME
        add si, DIR_ENTRY_FNAME
        call CompressFName

        clc
        jmp short .Done
.NotFound:
        stc
        mov ax, ERR_NO_FILES
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
        mov al, [es:bx]
        and al, al
        jz .Found
        cmp al, ENTRY_DELETED
        jz .Found
        add bx, DIR_ENTRY_SIZE
        cmp bx, [RootMaxIdx]
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
        mov word [es:bx+DIR_ENTRY_LCLUST], EOC_CLUSTER
        pop di
        pop si
        ret
.RootFull:
        mov bx, MsgErrRootFull
        jmp Fatal

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
        xor cx, cx
        mov [es:bx+FILE_INFO_FOFF], cx
        mov [es:bx+FILE_INFO_FOFF+2], cx
        mov [es:bx+FILE_INFO_BUFOFF], cx
        mov [es:bx+FILE_INFO_BUFSZ], cx
        ; Set owner
        mov cx, [cs:CurrentPSP]
        mov [es:bx+FILE_INFO_PSP], cx
        ; Make sure some other mode than FMODE_CLOSED is set
        mov byte [es:bx+FILE_INFO_MODE], FMODE_READ_ONLY
        ; Return file handle in AX
        pop di
        pop si
        ret

; Return new file handle in AX and file info in ES:BX
GetFileHandle:
        mov es, [FileInfoSeg]
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
        jae CloseFileError
        mov ax, [FileInfoSeg]
        mov es, ax
        mov ax, FILE_INFO_SIZE
        mul bx
        mov bx, ax
; Close file with info at ES:BX
CloseFileInfo:
        xor ax, ax
        xchg al, [es:bx+FILE_INFO_MODE] ; Mark file closed
        and al, al
        jz CloseFileError ;Not open
        cmp al, FMODE_READ_ONLY
        je .Done
        call FlushFileBuffer
        call WriteFSMeta
.Done:
        ret
CloseFileError:
        mov bx, MsgErrFNotOpen
        jmp Fatal

WriteFSMeta:
        call WriteFAT
        ; Fall through

WriteRootDir:
        mov ax, [cs:RootSeg]
        mov es, ax
        xor di, di
        call GetFatNRootSecs
        mov cx, ax
        call GetFatRootSec
        jmp WriteSectors

WriteFAT:
        mov ax, [cs:FATSeg]
        mov es, ax
        xor di, di

        xor dx, dx
        xor ah, ah
        mov al, [cs:BPB+BPB_NUMFATS]
        mul word [cs:BPB+BPB_SECSPERFAT]
        mov cx, ax
        mov ax, [cs:BPB+BPB_RSRVEDSECS]
        jmp WriteSectors

; Flush file (write) buffer for file with info in ES:BX
; Returns carry clear on success, carry set on error.
FlushFileBuffer:
        mov cx, [es:bx+FILE_INFO_BUFOFF]
        and cx, cx
        jnz .Flush
        clc
        ret ; Nothing to flush
.Flush:
        ; TODO: Clear from BUFOFF to ClusterBytes to avoid writing junk to file

        ; Alloc free cluster
        mov ax, [es:bx+FILE_INFO_CLUST]
        push ax
        push bx
        push es
        call AddCluster
        pop es
        pop bx
        pop dx ; Last cluster
        and ax, ax
        jnz .HasCluster
        stc
        ret
.HasCluster:
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
        push di
        push es
        mov di, [es:bx+FILE_INFO_BUFSEG]
        mov es, di
        xor di, di
        call WriteCluster
        pop es
        mov word [es:bx+FILE_INFO_BUFOFF], 0
        mov di, [cs:ClusterBytes]
        mov word [es:bx+FILE_INFO_BUFSZ], di
        pop di
        clc
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
        push di
        push ds
        push es

        cmp bx, MAX_FILES
        jae .InvalidHandle

        mov di, ds ; Save original DS in DI

        mov si, cs
        mov ds, si

        mov si, cx ; Store original count in SI

        push dx
        mov ax, FILE_INFO_SIZE
        mul bx
        mov bx, ax
        mov ax, [FileInfoSeg]
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
        push cx
        push si
        push di
        push es
        mov cx, ax
        mov ds, di
        mov di, [es:bx+FILE_INFO_BUFOFF]
        mov si, [es:bx+FILE_INFO_BUFSEG]
        mov es, si
        mov si, dx
        rep movsb
        pop es
        pop di
        pop si
        pop cx

        add dx, ax ; TODO: Handle if this overflows?
        sub cx, ax
        add [es:bx+FILE_INFO_BUFOFF], ax
        sub [es:bx+FILE_INFO_BUFSZ], ax
        jnz .WriteLoop

        push ax
        push bx
        push cx
        push dx
        call FlushFileBuffer
        pop dx
        pop cx
        pop bx
        pop ax
        jnc .WriteLoop
        mov ax, ERR_ACCESS_DENIE
        jmp short .Ret
.Done:
        mov ax, si ; All bytes written
        clc
        jmp short .Ret
.InvalidHandle:
        mov ax, ERR_INVALID_HAND
        stc
.Ret:
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
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

        cmp byte [es:bx+FILE_INFO_MODE], FMODE_READ_ONLY
        jne .InvalidHandle
.Again:
        ; First check if there's data in the buffer
        cmp word [es:bx+FILE_INFO_BUFSZ], 0
        jnz .HasData

        ; Fill buffer
        push ax
        push bx
        push cx
        push dx
        push ds
        push cs
        pop ds
        call FillFileBuffer
        pop ds
        pop dx
        pop cx
        pop bx
        pop ax
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

; Fill buffer of file in ES:BX (buffer assumed to be empty before)
; File pointer is updated
FillFileBuffer:
        ; How many bytes left in file?
        call GetFileSize
        mov cx, ax
        sub cx, [es:bx+FILE_INFO_FOFF]
        sbb dx, [es:bx+FILE_INFO_FOFF+2]

        and dx, dx
        jnz .Limit
        cmp cx, [ClusterBytes]
        jbe .Update
.Limit:
        mov cx, [ClusterBytes]
.Update:
        mov word [es:bx+FILE_INFO_BUFOFF], 0
        mov [es:bx+FILE_INFO_BUFSZ], cx
        add [es:bx+FILE_INFO_FOFF], cx
        adc word [es:bx+FILE_INFO_FOFF+2], 0
        and cx, cx ; EOF?
        jz .Done

        ; Assume FILE_BUFFER_SIZE == ClusterBytes
        ; If increased, loop here
        mov ax, [es:bx+FILE_INFO_CLUST]

        push es
        push ax
        push di
        mov di, [es:bx+FILE_INFO_BUFSEG]
        mov es, di
        xor di, di
        call ReadCluster
        pop di
        pop ax
        push bx
        call NextCluster
        pop bx
        pop es

        mov [es:bx+FILE_INFO_CLUST], ax

.Done:
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main INT 21h dispatch routine
;; Reference: Ralf Browns Interrupt List
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Int21Dispatch:
        cld ; Make sure direction flag is always clear

        push bx
        push bp
        mov bp, .DispatchList
        mov bx, .DispatchListEnd
.L:
        cmp ah, [cs:bp]
        jne .Next
        mov bx, [cs:bp+1]
        mov bp, sp
        xchg [bp+2], bx
        pop bp
        ret
.Next:
        add bp, 3
        cmp bp, bx
        jne .L

        ; Unimplemented syscall
        push ax
        xor bx, bx
        mov ds, bx
        mov bx, MsgErrNotSupp
        call PutString
        pop ax
        mov al, ah
        call PutHexByte
        call PutCrLf
.Halt:  hlt
        jmp .Halt
.DispatchList:
        db  0x02
        dw Int21_02
        db  0x08
        dw Int21_08
        db  0x09
        dw Int21_09
        db  0x0A
        dw Int21_0A
        db  0x19
        dw Int21_19
        db  0x1A
        dw Int21_1A
        db  0x2F
        dw Int21_2F
        db  0x36
        dw Int21_36
        db  0x3C
        dw Int21_3C
        db  0x3D
        dw Int21_3D
        db  0x3E
        dw Int21_3E
        db  0x3F
        dw Int21_3F
        db  0x40
        dw Int21_40
        db  0x41
        dw Int21_41
        db  0x4A
        dw Int21_4A
        db  0x4B
        dw Int21_4B
        db  0x4C
        dw Int21_4C
        db  0x4D
        dw Int21_4D
        db  0x4E
        dw Int21_4E
        db  0x4F
        dw Int21_4F
        db  0x56
        dw Int21_56
        db  0x62
        dw Int21_62
.DispatchListEnd:

; IRET with carry flag
; TODO: This is really ugly..
; Stack on entry:
; SP+4 Flags
; SP+2 CS
; SP+0 IP
IRETC:
        push bp
        mov bp, sp
        jc .C
        and byte [bp+6], 0xfe ; clear carry
        jmp short .Ret
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

; Int 21/AH=08h Character input without echo
; Returns character read in AL
Int21_08:
        xor ah, ah
        int 0x16
        iret

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

; Int 21/AH=0Ah Buffered input
; DS:DX points to buffer (first byte: maximum size,
; second byte: bytes read excluding CR)
Int21_0A:
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push ds
        push es

        push ds
        pop es ; ES=DS

        mov di, dx
        mov cx, [di] ; CH=Characters already used, CL=Maximum characters
        add di, 2
        and cl, cl
        jz .Ret
        dec cl ; CL = Maximum characters excuding CR
        cmp ch, cl
        jae .Ret
        xor bh, bh
        mov bl, ch
        add di, bx
.GetChars:
        xor ax, ax
        int 0x16
        cmp al, 0x0D ; CR
        je .Done
        cmp al, 0x08 ; Backspace
        jne .NotBS
        and ch, ch
        jz .GetChars
        ; Erase character
        call .EraseChar
        dec ch
        dec di
        jmp .GetChars
.NotBS:
        ; Don't go beyond limit
        cmp ch, cl
        jae .GetChars
        stosb
        call PutChar ; Echo
        inc ch
        jmp .GetChars
.Done:
        ; Ensure buffer is always CR terminated
        mov al, 0x0D
        stosb
        mov di, dx
        mov [di+1], ch
.Ret:
        pop es
        pop ds
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        iret
.EraseChar:
        mov al, 8
        call PutChar
        mov al, ' '
        call PutChar
        mov al, 8
        jmp PutChar

; Int 21/AH=19h Get current default drive
Int21_19:
        mov al, [cs:BootDrive]
        iret

; Int 21/AH=1Ah Set disk transfer area address
; DS:DX points to DTA
Int21_1A:
        push dx
        mov [cs:DTA], dx
        mov dx, ds
        mov [cs:DTA+2], dx
        pop dx
        iret

; Int 21/AH=2Fh Get disk transfer area address
; Returns DTA pointer in ES:BX
Int21_2F:
        mov bx, [cs:DTA+2]
        mov es, bx
        mov bx, [cs:DTA]
        iret

; Int 21/AH=36h Get free disk space
; Returns AX=sectors per cluster, BX=number of free clusters
;         CX=bytes per cluster, DX=total clusters on drive
Int21_36:
        cmp dl, [cs:BootDrive]
        je .DriveOK
        mov ax, 0xffff
        iret
.DriveOK:
        push ds
        push es
        push cs
        pop ds
        ; Count free clusters
        mov es, [FATSeg]
        xor ax, ax
        xor bx, bx
        xor cx, cx
        mov dx, [MaxCluster]
.Count:
        test word [es:bx], 0x0fff
        jnz .C1
        inc ax
.C1:
        inc bx
        inc cx
        test word [es:bx], 0xfff0
        jnz .C2
        inc ax
.C2:
        add bx, 2
        inc cx
        cmp cx, dx
        jb .Count
        mov bx, ax
        xor ah, ah
        mov al, [BPB+BPB_SECSPERCLUST]
        mov cx, SECTOR_SIZE
        sub dx, 2
        pop es
        pop ds
        iret

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
        jne .Found
        call NewRootEntry
        jmp short .OpenFromDE
.Found:
        ; File found, truncate
        xor ax, ax
        mov [es:bx+DIR_ENTRY_FSIZE], ax
        mov [es:bx+DIR_ENTRY_FSIZE+2], ax
        mov ax, [es:bx+DIR_ENTRY_LCLUST]
        push es
        push bx
        call FreeClusterChain
        pop bx
        pop es
.OpenFromDE:
        pop cx ; File attributes
        mov [es:bx+DIR_ENTRY_ATTR], cl ; Set attributes
        call OpenFileFromDE
        mov byte [es:bx+FILE_INFO_MODE], FMODE_WRITE_ONLY
        mov dx, [ClusterBytes]
        mov word [es:bx+FILE_INFO_BUFSZ], dx
        mov word [es:bx+FILE_INFO_CLUST], EOC_CLUSTER
        ; AX = File handle
        clc

        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC

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
        jmp short .Out

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
        push ax
        push bx
        push cx
        push dx
        push ds
        push es
        mov ax, cs
        mov ds, ax
        call CloseFileHandle
        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        pop ax
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

; Int 21/AH=41h Delete file
; DS:DX File to delete
Int21_41:
        push bx
        push cx
        push dx
        push ds
        push es

        call ExpandFName

        mov ax, cs
        mov ds, ax

        call FindFileInRoot

        cmp bx, NOT_FOUND
        jne .Found
        mov ax, ERR_FILE_NOT_FND
        stc
        jmp short .Ret
.Found:
        push es
        push bx
        mov ax, [es:bx+DIR_ENTRY_LCLUST]
        call FreeClusterChain
        pop bx
        pop es

        mov byte [es:bx+DIR_ENTRY_FNAME], ENTRY_DELETED
        call WriteFSMeta
        clc
.Ret:
        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC


; Int 21/AH=4Ah Resize memory block
; BX    New size in paragraphs
; ES    Segment to resize
Int21_4A:
        push bx
        ; HACK: Only allow resize of last loaded program
        mov ax, [cs:CurrentPSP]
        mov bx, es
        cmp bx, ax
        je .OK
        mov bx, .MsgNotImpl
        jmp Fatal
.OK:
        pop bx
        mov ax, es
        add ax, bx
        cmp ax, [cs:FreeSeg]
        jbe .OK2
        mov bx, .MsgNotImpl
        jmp Fatal
.OK2:
        mov [cs:FreeSeg], ax
        clc
        jmp IRETC
.MsgNotImpl: db 'FIXME in Int21_4A', 0

; Int 21/AH=4Bh Exec
; AL    Type of load (0 = load and execute, 1 = load but do not execute)
; DS:DX Program name
; ES:BX Parameter block (mostly ignored for now)
;       Far pointer at [es:bx+2] points to command line arguments
Int21_4B:
        cmp al, 1
        jbe .ArgOK
        mov al, ERR_FUNC_INV
        stc
        jmp IRETC

.ArgOK:
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        push ds
        push es

        push ax
        push bx ; Push parameter block
        push es

        ; Push command line
        mov ax, [es:bx+2]
        push ax
        mov ax, [es:bx+4]
        push ax

        call ExpandFName
        mov ax, cs
        mov ds, ax

        call LoadProgram

        pop ds ; Command line seg
        pop si ; Command line offset
        and ax, ax
        jnz .LoadOK
        add sp, 6 ; Discard ES/BX/AX from stack
        mov ax, ERR_FILE_NOT_FND ; Assume this is the cause...
        stc
        jmp short .Ret
.LoadOK:
        ; Copy command line (DS:SI from above)
        ; It's a one byte length byte and a CR terimanted
        ; string (NOTE: the CR doesn't count towards the length!)
        mov es, ax
        mov di, 0x80 ; Copy to PSP:80h (The command line)
        mov cl, [si]    ; Copy as many bytes as requested
        mov ch, 0x7E
        cmp cl, ch      ; .. But limit
        jbe .CopyCmdLine
        mov cl, ch
.CopyCmdLine:
        xor ch, ch
        inc cl
        rep movsb
        mov byte [es:di], 0x0D ; Make sure the string is always CR terminated
        pop es ; Pop parameter block
        pop di ; and offset
        pop bx ; argument (BL=0: load&execute, BL=1: load only)
        and bl, bl
        jz .Start
        push cs
        pop ds
        ; Only load requested, save CS/IP, SS/SP to parameter block
        add di, 0x0e ; Point at SS:SP
        mov ax, [ChildSSSP]
        stosw
        mov ax, [ChildSSSP+2]
        stosw
        mov ax, [ChildCSIP]
        stosw
        mov ax, [ChildCSIP+2]
        stosw
        jmp short .RetOK
.Start:
        call StartProgram
        ; Local return in DefaultTerminate goes here
        mov al, [cs:LastRetVal]
.RetOK:
        ; Leave return code in AL
        clc
.Ret:
        pop es
        pop ds
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        jmp IRETC

; Int 21/AH=4Ch Terminate with return code
; AL    Return code
Int21_4C:
        push cs
        pop ds

        ; Save return code
        xor ah, ah ; termination type: normal
        mov [LastRetVal], ax

        ;
        ; Close all files owned by process
        ;
        mov dx, [CurrentPSP]

        mov es, [FileInfoSeg]
        xor bx, bx
.Check:
        cmp byte [es:bx+FILE_INFO_MODE], FMODE_CLOSED
        je .Next
        cmp [es:bx+FILE_INFO_PSP], dx
        jne .Next
        push bx
        push dx
        push es
        call CloseFileInfo
        pop es
        pop dx
        pop bx
.Next:
        add bx, FILE_INFO_SIZE
        cmp bx, MAX_FILES*FILE_INFO_SIZE
        jb .Check

        ; Point ES at PSP of exiting process
        mov es, dx

        ; Restore stack
        mov sp, [es:PSP_OLDSP]
        mov ss, [es:PSP_OLDSS]

        ; NOTE: Don't use stack here as it may be invalid
        ; if the program was started manually after Int21/AX=4B01

        ; Restore old PSP
        mov bx, [es:PSP_PARENTPSP]
        mov [CurrentPSP], bx

        ; Get termination handler
        mov cx, [es:PSP_OLDINT22+2]
        mov bx, [es:PSP_OLDINT22]
        ; And restore it
        mov [INT22_OFF], bx
        mov [INT22_SEG], cx

        ; TODO: Restore Int23/Int24

        ; Restore free segment pointer
        ; This only works because TSR arent't supported
        mov [FreeSeg], es

        ; Call termination handler
        jmp far [INT22_OFF]

; Int 21/AH=4Dh Get return code (errorlevel)
; Returns AH = terminatin type (0=normal, 1=ctrl+c, 2=critical error,3=TSR)
;         AL = return code
;         carry clear
Int21_4D:
        mov ax, [cs:LastRetVal]
        clc
        jmp IRETC

; Int 21/AH=4Eh Find first matching file
; CX    File attribute mask (bits 0 and 5 ignored)
; DS:DX ASCIIZ file specification
; Returns carry clear and DTA filled on success
;         carray set and error code in AX on error
Int21_4E:
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        push ds
        push es

        ; Point ES:BX at find file structure (in DTA)
        mov bx, [cs:DTA+2]
        mov es, bx
        mov bx, [cs:DTA]

        ; Save search attribute
        or cl, 0x21 ; bit 0 and 5 (R/O and archive) are ignored
        not cl
        mov [es:bx+FF_SATTR], cl

        ; Exapnd file specification to DTA
        call ExpandFSpec

        ; Set dir index (DoFindFile increments at the start)
        xor ax, ax
        sub ax, DIR_ENTRY_SIZE
        mov word [es:bx+FF_ROOT_IDX], ax

        ; Jump to common code
        call DoFindFile

        pop es
        pop ds
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        jmp IRETC

; Int 21/AH=4Fh Find next matching file
; Returns carry clear and DTA filled on success
;         carray set and error code in AX on error
Int21_4F:
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        push ds
        push es

        ; Point ES:BX at find file structure (in DTA)
        mov bx, [cs:DTA+2]
        mov es, bx
        mov bx, [cs:DTA]

        ; Find next matching file
        call DoFindFile

        pop es
        pop ds
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        jmp IRETC

; Int 21/AH=56h Rename file
; DS:DX ASCIIZ filename of existing file
; ES:DI ASCIIZ new filename
; Returns carry clear
;         carray set and error code in AX on error
Int21_56:
        push bx
        push cx
        push dx
        push ds
        push es

        ; First check if the destination already exists
        push dx
        push ds
        push es
        mov dx, es
        mov ds, dx
        mov dx, di
        call ExpandFName
        mov ax, cs
        mov ds, ax
        call FindFileInRoot
        cmp bx, NOT_FOUND
        je .DestOK
        add sp, 6 ; pop 3 words
        mov ax, ERR_ACCESS_DENIE
        stc
        jmp short .Ret
.DestOK:
        ; Copy expanded filename to DTA
        mov cx, cs
        mov ds, cx
        mov cx, 11
        mov si, CurFileName
        mov di, [DTA+2]
        mov es, di
        mov di, [DTA]
        rep movsb
        pop es
        pop ds
        pop dx

        call ExpandFName
        mov ax, cs
        mov ds, ax
        call FindFileInRoot
        cmp bx, NOT_FOUND
        jne .SourceOK
        mov ax, ERR_FILE_NOT_FND
        stc
        jmp short .Ret
.SourceOK:
        mov di, bx
        mov si, [cs:DTA+2]
        mov ds, si
        mov si, [cs:DTA]
        mov cx, 11
        rep movsb
        call WriteRootDir
        clc
.Ret:
        pop es
        pop ds
        pop dx
        pop cx
        pop bx
        jmp IRETC


; Int 21/AH=56h Get current PSP address
; Returns BX=segment of current process (maybe loaded by Int21/AX=4B01h)
Int21_62:
        mov bx, [cs:CurrentPSP]
        iret

Int20Dispatch:
        ; INT 20 is the same as INT 21/AX=4C00h
        mov ax, 0x4c00
        jmp Int21Dispatch

DefaultTerminate:
        ret ; Local return (usually to Int21_4B)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants and data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgErrFatal:     db 'Fatal error: ', 0
MsgErrNotSupp:   db 'Not implemented: INT 21h/AH=', 0
MsgErrRead:      db 'Error reading from disk', 0
MsgErrWrite:     db 'Error writing to disk', 0
MsgErrOOM:       db 'Out of memory', 0
MsgErrCluster:   db 'Cluster invalid', 0
MsgErrCmdNotF:   db 'Command processor not found', 0
MsgErrFileMax:   db 'Too many open files', 0
MsgErrFNotOpen:  db 'Invalid file handle', 0
MsgErrRootFull:  db 'Root directory full', 0
MsgErrCmdpRet:   db 'Command processor returned', 0
MsgErrProgLoad:  db 'Error loading program', 0
MsgReboot:       db 'Press any key to reboot', 13, 10, 0
CmdpFName:       db 'CMDP.COM', 0

BootDrive:       resb 1
BPB:             resb BPB_SIZE
DataSector:      resw 1
ClusterBytes:    resw 1
MaxCluster:      resw 1  ; Note: +2 compared to max cluster index
RootMaxIdx:      resw 1
MaxSeg:          resw 1
FreeSeg:         resw 1
FATSeg:          resw 1
FileInfoSeg:     resw 1
RootSeg:         resw 1
CmdpSeg:         resw 1
CurrentPSP:      resw 1  ; Segment of last started process
DTA:             resw 2  ; Disk Transfer Area (defaults to PSP:80h)
LastRetVal:      resw 1  ; Last return value/termination type (errorlevel)
ChildSSSP:       resw 2  ; SS:SP of just loaded program
ChildCSIP:       resw 2  ; CS:IP of same
CurFileName:     resb 11

ProgramEnd:
