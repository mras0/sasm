;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CMDP.COM - Very limited command processor for ;;
;;            bootstrapping SASM.                ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        cpu 8086
        org 0x100

BUFFER_SIZE       EQU 512
STACK_SIZE        EQU 512
CMDLINE_MAX       EQU 0x7F ; Including terminating CR

; Offsets into FindFile structure
FF_FATTR         equ 0x15 ; BYTE     File attribute
FF_FTIME         equ 0x16 ; WORD     File time
FF_FDATE         equ 0x18 ; WORD     File date
FF_FSIZE         equ 0x1A ; DWORD    File size
FF_FNAME         equ 0x1E ; BYTE[13] File name and extension (ASCIIZ with dot)

Main:
        ; Clear BSS
        mov di, BssStart
        mov cx, ProgramEnd
        sub cx, di
        xor al, al
        rep movsb

        ; Free unused memory

        cli
        mov bx, ProgramEnd
        add bx, 15
        and bx, 0xFFF0
        add bx, STACK_SIZE
        mov sp, bx
        mov cl, 4
        shr bx, cl
        mov ax, ds
        add bx, ax
        mov [FirstFreeSeg], bx
        sti
        ; Free remaining memory
        mov ah, 0x4a
        int 0x21
        jc GenericError

        ;
        ; Run autoexec.bat if it exists
        ;

        mov dx, Autoexec
        mov ax, 0x3d00
        int 0x21
        jc .CmdLoop
        call RunCommandFile

.CmdLoop:
        call PutCrLf
        mov dx, MsgPrompt
        mov ah, 9
        int 0x21
        call GetCommandLine
        call CommandDispatch
        jmp .CmdLoop

; Run command file in AX
; Returns carry clear on success
;         carry set on error and error code in AX
RunCommandFile:
        mov bx, ax
.LineLoop:
        mov si, CL_Buffer
.CharLoop:
        mov cx, 1
        mov ah, 0x3f
        mov dx, si
        int 0x21
        jnc .ReadOK
        push ax
        mov ah, 0x3e
        int 0x21
        pop ax
        stc
        ret
.ReadOK:
        and ax, ax
        jz .Done
        lodsb
        cmp al, 0x0D
        je .Execute
        mov ax, si
        sub ax, CL_Buffer
        cmp ax, CMDLINE_MAX
        jb .CharLoop
        mov byte [si-1], 0x0D
.Execute:
        call CommandDispatch
        jmp .LineLoop
.Done:
        mov byte [si], 0x0D
        call CommandDispatch ; Execute what's in the buffer
        mov ah, 0x3e
        int 0x21
        ret

GenericError:
        mov dx, MsgErrGeneric
        ; Fall through

; Exit with error message in DX
Error:
        mov ah, 0x09
        int 0x21
        mov al, 13
        call PutChar
        mov al, 10
        call PutChar
        mov ax, 0x4cff
        int 0x21

; Put character in AL
PutChar:
        push ax
        push dx
        mov dl, al
        mov ah, 2
        int 0x21
        pop dx
        pop ax
        ret

PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
        call PutChar
        ret

; Print word in AX
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
        jmp short .Print
.Rep:
        mov al, '.'
.Print:
        call PutChar
        inc bl
        cmp bl, dl
        jne .C
        add si, dx
        and cx, cx
        jz .Done
        call PutCrLf
        jmp HexDump
.Done:
        ret

; Print 9 digit decimal number in DX:AX (space padded)
PutDec9:
        push ds
        push bp
        mov bx, 10
        mov bp, sp
        sub sp, 10
        dec bp
        mov byte [bp], '$'
.Cvt:
        push ax
        mov ax, dx
        xor dx, dx
        div bx
        mov cx, ax
        pop ax
        div bx
        xchg cx, dx
        push ax
        mov al, cl
        add al, '0'
        dec bp
        mov [bp], al
        pop ax
        mov cx, dx
        or cx, ax
        jnz .Cvt
        mov al, ' '
.Pad:
        cmp bp, sp
        je .Print
        dec bp
        mov [bp], al
        jmp .Pad
.Print:
        mov bx, ss
        mov ds, bx
        mov dx, bp
        mov ah, 0x09
        int 0x21
        add sp, 10
        pop bp
        pop ds
        ret

GetCommandLine:
        mov bx, CL_BufferInfo
        xor ah, ah
        mov al, CMDLINE_MAX
        mov [bx], ax
        mov dx, bx
        mov ah, 0x0a
        int 0x21
        call PutCrLf
        ret

OpenInput:
        ; Open input file for reading
        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        jc .Ret
        mov [InputFile], ax
.Ret:
        ret

OpenOutput:
        mov dx, OutFileName
        mov cx, 0x0020 ; Attributes
        mov ah, 0x3c
        int 0x21
        jc .Ret
        mov [OutputFile], ax
.Ret:
        ret

CloseInput:
        mov bx, [InputFile]
        jmp short CloseFile

CloseOutput:
        mov bx, [OutputFile]
        jmp short CloseFile

CloseFile:
        mov ah, 0x3e
        int 0x21
        jnc .OK
        jmp GenericError
.OK:
        ret

; Read from InputFile. Returns number of bytes read in AX
ReadToBuffer:
        mov cx, BUFFER_SIZE
        ; Fall through
ReadToBufferN: ; Read CX bytes
        mov ah, 0x3f
        mov bx, [InputFile]
        mov dx, Buffer
        int 0x21
        jc .ReadError
        ; Return number of bytes read in AX
        ret
.ReadError:
        mov dx, MsgErrRead
        jmp Error

; Write CX bytes from buffer to OutputFile
WriteFromBuffer:
        mov ah, 0x40
        mov bx, [OutputFile]
        mov dx, Buffer
        int 0x21
        jc .WriteError
        ret
.WriteError:
        mov dx, MsgErrWrite
        jmp Error

CommandDispatch:
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        push ds
        push es
        call .RealCD
        pop es
        pop ds
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        ret
.RealCD:
        mov [CmdOldSp], sp ; Save SP at entry (to allow easy exit)
        mov si, CL_Buffer
.SkipSpace:
        lodsb
        cmp al, 0x0D
        jne .NotEmpty
        jmp .Ret ; Empty line
.NotEmpty:
        cmp al, ' '
        jbe .SkipSpace
        dec si ; unget non-space character

        ; Copy command name to InFileName up till possible extension
        mov di, cs
        mov es, di
        mov di, InFileName
        mov cl, 8 ; Don't overflow comand
.CopyCommand:
        lodsb
        cmp al, '0' ; Characters less than 0x30 cannot be part of legal filename
        jb .CmdDone ; In particular this will stop on '.', '/' and CR
        call CToUpper
        stosb
        dec cl
        jnz .CopyCommand
        inc si ; undo 'unget' (we actually consumed the character)
.CmdDone:
        dec si ; unget
        mov byte [es:di], 0 ; NUL-terminate

        ; SI points at CommandLine after command (possibly at '.' before extension)
        ; DI points at InFileName at NUL terminator (at most 9 bytes in)

        mov bx, InFileName

        cmp word [bx], 'CO'
        jne .NotCopy
        cmp word [bx+2], 'PY'
        jne .NotInt
        cmp byte [bx+4], 0
        jne .NotInt
        jmp CmdCopy
.NotCopy:
        cmp word [bx], 'DE'
        jne .NotDel
        cmp word [bx+2], 'L'
        jne .NotInt
        jmp CmdDel
.NotInt:
        jmp .NotInternal
.NotDel:
        cmp word [bx], 'DI'
        jne .NotDi
        cmp word [bx+2], 'R'
        jne .NotDir
        jmp CmdDir
.NotDir:
        cmp word [bx+2], 'SK'
        jne .NotInt
        cmp word [bx+4], 'CO'
        jne .NotInt
        cmp word [bx+6], 'PY'
        jne .NotInt
        cmp byte [bx+8], 0
        jne .NotInt
        jmp CmdDiskCopy
.NotDi:
        cmp word [bx], 'EC'
        jne .NotEcho
        cmp word [bx+2], 'HO'
        jne .NotInternal
        cmp byte [bx+4], 0
        jne .NotInternal
        jmp CmdEcho
.NotEcho:
        cmp word [bx], 'EX'
        jne .NotExit
        cmp word [bx+2], 'IT'
        jne .NotInternal
        cmp byte [bx+4], 0
        jne .NotInternal
        jmp CmdExit
.NotExit:
        cmp word [bx], 'HD'
        jne .NotHd
        cmp byte [bx+2], 0
        jne .NotInternal
        jmp CmdHd
.NotHd:
        cmp word [bx], 'PA'
        jne .NotPause
        cmp word [bx+2], 'US'
        jne .NotInternal
        cmp word [bx+4], 'E'
        jne .NotInternal
        call CWaitKey
        jmp PutCrLf
.NotPause:
        cmp word [bx], 'RE'
        jne .NotRe
        cmp word [bx+2], 'N'
        jne .NotRen
        jmp CmdRen
.NotRen:
        cmp word [bx+2], 'M'
        jne .NotInternal
        jmp CmdRem
.NotRe:
        cmp word [bx], 'TY'
        jne .NotType
        cmp word [bx+2], 'PE'
        jne .NotInternal
        cmp byte [bx+4], 0
        jne .NotInternal
        jmp CmdType
.NotType:

.NotInternal:
        ; Copy extension (if present)
        cmp byte [si], '.'
        jne .AppendExt
        mov cl, 4
.CopyExt:
        lodsb
        cmp al, ' '
        jbe .ExtCopyDone
        call CToUpper
        stosb
        dec cl
        jnz .CopyExt
        inc si ; undo 'unget' (we actually consumed the character)
.ExtCopyDone:
        dec si
        jmp short .AppendNUL

.AppendExt:
        ; Try batchfile with the name
        push di
        mov ax, '.B'
        stosw
        mov ax, 'AT'
        stosw
        xor al, al
        pop di

        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        jc .NotBat
        jmp RunCommandFile

.NotBat:
        mov ax, '.C'
        stosw
        mov ax, 'OM'
        stosw

.AppendNUL:
        xor al, al
        stosb

        ; Figure out argument length
        xor bx, bx
        mov al, 0x0D
.FindLen:
        cmp [bx+si], al
        je .RTrim
        inc bx
        cmp bl, 0x7E
        jbe .FindLen ; Don't go too long
.RTrim:
        ; Remove spaces just before the CR
        and bx, bx
        jz .CmdLineDone
        cmp byte [si+bx-1], ' '
        ja .CmdLineDone
        dec bx
        jmp .RTrim
.CmdLineDone:
        mov byte [si+bx], 0x0D ; Ensure CR terminated even if trimmed
        ; The command line is length prefixed
        ; and the count doesn't include the 0x0D
        dec si
        mov [si], bl

        ; TODO: Check file extension....
        ; Run Program
        mov word [PB_ArgPtr], si
        mov ax, ds
        mov es, ax
        mov [PB_ArgPtr+2], ax
        mov ax, 0x4b00           ; Load and execute
        mov dx, InFileName       ; DS:DX -> program name
        mov bx, ParameterBlock   ; ES:BX -> parameter block
        int 0x21
        jnc .Ret

        mov si, InFileName
.BCP:
        lodsb
        and al, al
        jz .BCPD
        call PutChar
        jmp .BCP
.BCPD:
        call PutCrLf

        mov dx, MsgErrBadCommand
        mov ah, 0x09
        int 0x21
        call PutCrLf
.Ret:
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Commands
;;
;; Called with SI pointing at the command line after the command name
;; (possibly at a '.')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Upper case AL
CToUpper:
        cmp al, 'a'
        jb .Ret
        cmp al, 'z'
        ja .Ret
        and al, 0xDF ; to upper case
.Ret:
        ret

; Skip spaces, returns next character in AL
CSkipSpaces:
        cmp al, ' '
        jne .Done
        lodsb
        jmp CSkipSpaces
.Done:
        ret

; Copy filename and zero terminate string in ES:DI from SI
; Next character returned in AL
CCopyFName:
        mov cl, 12
.L:
        cmp al, ' '
        jbe .Done
        stosb
        lodsb
        dec cl
        jnz .L
.Done:
        mov ch, al
        xor al, al
        stosb
        mov al, ch
        ret

; Print message and wait for keypress. Returns carry set on CTRL+C
CWaitKey:
        mov dx, MsgPressAnyKey
        mov ah, 9
        int 0x21
        mov ah, 8
        int 0x21
        cmp al, 3
        jne .NE
        call PutCrLf
        stc
        ret
.NE:
        clc
        ret

COpenInError:
        mov dx, MsgErrOpenIn
        jmp short CError

COpenOutError:
        mov dx, MsgErrOpenOut
        jmp short CError

CArgError:
        mov dx, MsgErrInvArgs
        ; Fall through

; Display error in DX and longjump to main loop
CError:
        mov ah, 0x09
        int 0x21
        call PutCrLf
        mov sp, [CmdOldSp]
        ret

; Get filename from command line to ES:DI
CGetFilename:
        call CSkipSpaces
        cmp al, 0x0D
        je CArgError
        call CCopyFName
        ret

CCheckCR:
        call CSkipSpaces
        cmp al, 0x0D
        jne CArgError
        ret

CGetIn:
        mov di, cs
        mov es, di
        lodsb
        mov di, InFileName
        jmp CGetFilename

CGetInOut:
        call CGetIn
        mov di, OutFileName
        call CGetFilename
        jmp CCheckCR

CmdCopy:
        call CGetInOut

        call OpenInput
        jc COpenInError
        call OpenOutput
        jnc .Loop
        call CloseInput
        jmp COpenOutError
.Loop:
        call ReadToBuffer
        and ax, ax
        jz .Done
        mov cx, ax
        call WriteFromBuffer
        jmp .Loop
.Done:
        call CloseOutput
        call CloseInput
        ret

CmdDel:
        call CGetIn
        mov dx, InFileName
        mov ah, 0x41
        int 0x21
        mov dx, MsgErrDelete
        jc CError
        ret

CmdDir:
        mov di, cs
        mov es, di
        ; Fill pattern into InFileName
        mov di, InFileName

        lodsb
        call CSkipSpaces

        ; No pattern means '*.*'
        cmp al, 0x0D
        jne .NotEmpty

        mov ax, '*.'
        stosw
        mov ax, '*'
        stosw
        jmp short .Main

.NotEmpty:
        ; '.EXT' means '*.EXT'
        cmp al, '.'
        jne .CopyPattern
        mov ax, '*.'
        stosw
        lodsb ; Consumed '.'

.CopyPattern:
        cmp di, InFileNameEnd
        je .CopyDone
        cmp al, ' '
        jbe .CopyDone
        stosb
        lodsb
        jmp .CopyPattern

.CopyDone:
        xor al, al
        stosb
.Main:
        ; Int21/AH=1Ah Set disk transfer area address
        ; DS:DX points to DTA
        mov ah, 0x1a
        mov dx, 0x80
        int 0x21

        ; di:bp contains total number of bytes
        xor di, di
        xor bp, bp

        mov ah, 0x4e
        xor cx, cx
        mov dx, InFileName
        int 0x21
        jc .Done
.Find:
        mov si, 0x80 ; DTA defaults to PSP:80h, Offset of filename is 0x1E
        add si, FF_FNAME
        mov cl, 12
.Print:
        lodsb
        and al, al
        jz .Pad
        call PutChar
        dec cl
        jnz .Print
.Pad:
        and cl, cl
        jz .PadDone
        mov al, ' '
        call PutChar
        dec cl
        jmp .Pad
.PadDone:
        mov si, 0x80
        mov ax, [si+FF_FSIZE]
        mov dx, [si+FF_FSIZE+2]
        add bp, ax
        adc di, dx
        call PutDec9
        call PutCrLf
        ; Find next
        mov ah, 0x4F
        int 0x21
        jnc .Find
.Done:
        mov ax, bp
        mov dx, di
        call PutDec9
        mov dx, MsgBytesTotal
        mov ah, 9
        int 0x21
        ret

CmdEcho:
        lodsb
        cmp al, '.'
        je .Next
        cmp al, ' '
        je .Next
.Loop:
        cmp al, 0x0D
        je .Done
        call PutChar
.Next:
        lodsb
        jmp .Loop
.Done:
        call PutCrLf
        ret

; Offset from Buffer (containing boot sector)
NUM_SECTORS   equ 0x13
SEC_PER_FAT   equ 0x16
SEC_PER_TRACK equ 0x18
NUM_HEADS     equ 0x1a

CmdDiskCopy:
        ;
        ; Figure out current drive
        ;
        mov ah, 0x19
        int 0x21
        ; Too dangerous to allow other drives than 0 for now...
        cmp al, 0
        je .DriveOK
        mov dx, MsgErrDrive
        jmp CError
.DriveOK:

        ;
        ; Read boot sector to Buffer
        ;

        xor dh, dh
        mov dl, al
        push ds
        pop es
        mov bx, Buffer
        mov cx, 1
        mov ax, 0x0201
        int 0x13
        jnc .ReadBootOK
        mov dx, MsgErrDiskRead
        jmp CError
.ReadBootOK:

        mov di, [FirstFreeSeg]
        add di, 0x001f
        and di, 0xffe0
        mov si, [2]
        and si, 0xffe0
        sub si, di
        shr si, 1
        shr si, 1
        shr si, 1
        shr si, 1
        shr si, 1
        ; DI: Start segment
        ; SI: Number of sectors that can be stored
        ; BX: Remaining sectors
        ; CX/DX: Current CHS and drive
        mov bx, [Buffer+NUM_SECTORS]
.CopyLoop:
        mov ax, bx
        cmp ax, si
        jbe .Read
        mov ax, si
.Read:
        ; AX: Number of sectors to copy this time round
        mov bp, .DoRead
        call .DiskCmd
        stc
        call .DiskChange
        mov bp, .DoWrite
        call .DiskCmd
        clc
        call .DiskChange
        call .AddSectors
        sub bx, ax
        jnz .CopyLoop
        ret
.DiskCmd:
        ; Call BP for AX sectors
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push bp
.SectorLoop:
        ; How many sectors can we read w/o crossing 64K boundary?
        mov bx, 0x1000
        mov si, di
        and si, 0x0fff
        sub bx, si
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        cmp bx, ax
        jbe .CntOK
        mov bx, ax
.CntOK:
        ; Limit to a track
        cmp bx, [Buffer+SEC_PER_TRACK]
        jbe .CntOK2
        mov bx, [Buffer+SEC_PER_TRACK]
.CntOK2:
        push ax
        push bx
        push cx
        push dx
        push si
        push di
        push bp
        call bp
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        push ax
        mov ax, bx
        call .AddSectors
        pop ax
        sub ax, bx
        jz .Done
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        shr bx, 1
        add di, bx
        jmp .SectorLoop
.Done:
        pop bp
        pop di
        pop si
        pop dx
        pop cx
        pop bx
        pop ax
        ret
.AddSectors:
        ; Add AX sectors to CHS in CX/DX
        ; CH=cyl, DH=head, CL=sec
        push ax
        push bx
        push si
        push di
        mov si, dx
        xor dx, dx
        mov di, [Buffer+SEC_PER_TRACK]
        dec al
        add al, cl
        adc ah, 0
        div di
        ; DL = sector count
        ; AX = head count
        mov cl, dl
        inc cl
        mov dx, si
        add al, dh
        adc ah, 0
        add di, ax
        xor dx, dx
        mov di, [Buffer+NUM_HEADS]
        div di
        ; DL = head count
        ; AX = cylinder count
        add ch, al
        mov ax, si
        mov dh, dl
        mov dl, al
        pop di
        pop si
        pop bx
        pop ax
        ret
.DoRead:
        push ax
        push bx
        push cx
        push dx
        push dx
        push cx
        mov ax, bx
        call PutHexWord
        mov dx, .MsgRead
        mov ah, 0x09
        int 0x21
        mov ax, di
        call PutHexWord
        mov ah, 2
        mov dl, ' '
        int 0x21
        pop ax
        call PutHexWord
        mov ah, 2
        mov dl, ':'
        int 0x21
        pop ax
        call PutHexWord
        call PutCrLf
        pop dx
        pop cx
        pop bx
        pop ax
        mov si, MsgErrDiskRead
        mov ah, 0x02
.RWCommon:
        mov al, bl
        mov es, di
        xor bx, bx
        int 0x13
        jnc .RWOK
        mov dx, si
        jmp CError
.RWOK:
        ret
.MsgRead: db ' sectors to read to $'
.DoWrite:
        push ax
        push dx
        mov dx, .MsgWrite
        mov ah, 9
        int 0x21
        pop dx
        pop ax
        mov si, MsgErrDiskWrite
        mov ah, 0x03
        jmp .RWCommon
.MsgWrite: db 'Writing...',13,10,'$'
.DiskChange:
        push ax
        push bx
        push cx
        push dx
        mov dx, .MsgInsertSrc
        jnc .PDC
        mov dx, .MsgInsertDst
.PDC:
        mov ah, 9
        int 0x21
        call CWaitKey
        call PutCrLf
        pop dx
        pop cx
        pop bx
        pop ax
        ret
.MsgInsertSrc: db 'Insert source disk $'
.MsgInsertDst: db 'Insert destination disk $'

CmdExit:
        mov dx, MsgExiting
        mov ah, 9
        int 0x21
        call PutCrLf
        mov ax, 0x4c00
        int 0x21

HD_MAX_BYTES equ 24*16 ; 24 lines of hex and one for the message

CmdHd:
        call CGetIn

        call OpenInput
        jnc .L
        jmp COpenInError
.L:
        mov cx, HD_MAX_BYTES
        call ReadToBufferN
        and ax, ax
        jz .Done
        push ax
        mov cx, ax
        mov si, Buffer
        call HexDump
        pop ax
        cmp ax, HD_MAX_BYTES
        jne .Done
        call PutCrLf
        call CWaitKey
        jc .Done
        mov dl, 0x0D
        mov ah, 2
        int 0x21
        jmp .L
.Done:
        call CloseInput
        ret

CmdRen:
        call CGetInOut
        mov dx, InFileName
        mov di, OutFileName
        mov ah, 0x56
        int 0x21
        jnc .OK
        mov dx, MsgErrRename
        jmp CError
.OK:
        ret

CmdRem:
        ret

CmdType:
        call CGetIn

        call OpenInput
        jnc .HasIn
        jmp COpenInError
.HasIn:
        xor di, di ; line counter
        xor bx, bx ; char counter
.L:
        push bx
        call ReadToBuffer
        pop bx
        and ax, ax
        jz .Done
        mov cx, ax
        mov si, Buffer
.P:
        lodsb
        call PutChar
        cmp al, 10
        jne .NotLF
.NewLine:
        xor bx, bx
        inc di
        cmp di, 24
        jne .NextChar
        xor di, di
        push bx
        call CWaitKey
        pushf
        mov dl, 0x0D
        mov ah, 2
        int 0x21
        popf
        pop bx
        jc .Done
        jmp short .NextChar
.NotLF:
        inc bx
        cmp bx, 80
        je .NewLine
.NextChar:
        dec cx
        jnz .P
        jmp .L
.Done:
        call CloseInput
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgErrGeneric:    db 'Generic error message$'
MsgErrOpenIn:     db 'Could not open input file$'
MsgErrOpenOut:    db 'Could not open output file$'
MsgErrRead:       db 'Error reading from file$'
MsgErrWrite:      db 'Error writing to file$'
MsgErrBadCommand: db 'Unknown command$'
MsgErrInvArgs:    db 'Invalid argument(s)$'
MsgExiting:       db 'Command interpreter exiting$'
MsgErrDelete:     db 'Could not delete file$'
MsgErrRename:     db 'Could not rename file$'
MsgErrDrive:      db 'Sorry DISKCOPY will only try to work with drive 0 for now$'
MsgErrDiskRead:   db 'Error reading from disk$'
MsgErrDiskWrite:  db 'Error writing to disk$'
MsgPrompt:        db '# $'
MsgPressAnyKey:   db 'Press any key$'
MsgBytesTotal:    db ' bytes total', 13, 10, '$'
Autoexec:         db 'AUTOEXEC.BAT', 0

BssStart:

InputFile:        resw 1
OutputFile:       resw 1

ParameterBlock:   resw 1 ; Segment of environment to copy (0 = use caller's)
PB_ArgPtr:        resw 2 ; Pointer to arguments
                  resw 2 ; Pointer to first FCB
                  resw 2 ; Pointer second first FCB

InFileName:       resb 12
InFileNameEnd:    resb 1
OutFileName:      resb 13

FirstFreeSeg:     resw 1

CmdOldSp:         resw 1 ; SP on entry to CommandDispatch

; Command line
CL_BufferInfo:    resb 2 ; For use with Int 21/AH=0Ah (must precede CL_Buffer)
CL_Buffer:        resb CMDLINE_MAX

Buffer:           resb BUFFER_SIZE

; Keep at end
ProgramEnd:
