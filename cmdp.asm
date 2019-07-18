        org 0x100

BUFFER_SIZE       EQU 512

; Offsets into FindFile structure
FF_FATTR         equ 0x15 ; BYTE     File attribute
FF_FTIME         equ 0x16 ; WORD     File time
FF_FDATE         equ 0x18 ; WORD     File date
FF_FSIZE         equ 0x1A ; DWORD    File size
FF_FNAME         equ 0x1E ; BYTE[13] File name and extension (ASCIIZ with dot)

Main:
        mov dx, HelloMsg
        mov ah, 9
        int 0x21

        ; Free remaining memory if running under DOS
        mov ah, 0x4a
        ; Handle Stack...
        ;mov bx, Buffer
        ;add bx, 15
        ;and bx, 0xFFF0
        ;add bx, BUFFER_SIZE
        ;shr bx, 4
        mov bx, 0x1000 ; 64K
        int 0x21
        jc GenericError

        ; Run SASM

        mov word [.ArgPtr], .Args
        mov ax, ds
        mov [.ArgPtr+2], ax
        mov ax, 0x4b00           ; Load and execute
        mov dx, .ProgramName     ; DS:DX -> program name
        mov bx, .ParameterBlock  ; ES:BX -> parameter block
        int 0x21
        jnc .CallOk
        call PutHexWord
        call PutCrLf
        jmp GenericError
.CallOk:
        mov dx, .OKMsg
        mov ah, 9
        int 0x21

        call OpenInput

        call ReadToBuffer
        mov cx, ax
        mov si, Buffer
        cmp cx, 256
        jb .hd
        mov cx, 256
.hd:
        call HexDump

        call CloseInput

        ;call OpenOutput
        ;call CloseOutput
        ;; Delete file
        ;mov ah, 0x41
        ;mov dx, OutFileName
        ;int 0x21
        ;jc GenericError
        call CopyFile

        ; Int21/AH=2Fh
        ; Get disk transfer area address
        ; Returns DTA pointer in ES:BX
        mov ah, 0x2F
        int 0x21
        push bx
        mov ax, es
        call PutHexWord
        mov al, ':'
        call PutChar
        pop ax
        call PutHexWord
        call PutCrLf

        call HandleCommand
        ; Return.. (Shouldn't actually do that)
        ret

.OKMsg: db 'Back in CMDP!', 13, 10, '$'
.ProgramName: db 'SASM.COM', 0
.Args: db 'foo!', 0x0D
.ParameterBlock:
        dw 0 ; Segment of environment to copy (0 = use caller's)
.ArgPtr:
        dw 0, 0 ; Pointer to arguments
        dw 0, 0 ; Pointer to first FCB
        dw 0, 0 ; Pointer second first FCB

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
        pusha
        mov dl, al
        mov ah, 2
        int 0x21
        popa
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

OpenInput:
        ; Open input file for reading
        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        mov dx, MsgErrOpenIn
        jc Error
        mov [InputFile], ax
        ret

OpenOutput:
        mov dx, OutFileName
        mov cx, 0x0020 ; Attributes
        mov ah, 0x3c
        int 0x21
        mov dx, MsgErrOpenOut
        jc Error
        mov [OutputFile], ax
        ret

CloseInput:
        mov bx, [InputFile]
        jmp CloseFile

CloseOutput:
        mov bx, [OutputFile]
        jmp CloseFile

CloseFile:
        mov ah, 0x3e
        int 0x21
        jc GenericError
        ret

; Read from InputFile. Returns number of bytes read in AX
ReadToBuffer:
        mov ah, 0x3f
        mov bx, [InputFile]
        mov cx, BUFFER_SIZE
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


; Copy from InFileName to OutFileName
CopyFile:
        call OpenInput
        call OpenOutput
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

HandleCommand:
        mov si, CommandLine
.SkipSpace:
        lodsb
        cmp al, 0x0D
        je .Ret ; Empty line
        cmp al, ' '
        jbe .SkipSpace
        dec si ; unget non-space character

        ; Copy command name to InFileName up till possible extension
        mov di, cs
        mov es, di
        mov di, InFileName
        mov cl, 8 ; Don't overflow comand
.CopyCommand:
        je .CmdDone
        lodsb
        cmp al, '0' ; Characters less than 0x30 cannot be part of legal filename
        jb .CmdDone ; In particular this will stop on '.', '/' and CR
        cmp al, 'a'
        jb .StoreCmd
        cmp al, 'z'
        ja .StoreCmd
        and al, 0xDF ; to upper case
.StoreCmd:
        stosb
        dec cl
        jnz .CopyCommand
        inc si ; undo 'unget' (we actually consumed the character)
.CmdDone:
        dec si ; unget
        xor al, al
        stosb ; NUL-terminate

        ; SI points at CommandLine after command (possibly at '.' before extension)
        ; DI points at InFileName after NUL terminator (at most 9 bytes in)

        mov bx, InFileName

        cmp word [bx], 'CO'
        jne .NotCopy
        cmp word [bx+2], 'PY'
        jne .NotInternal
        cmp byte [bx+4], 0
        jne .NotInternal
        jmp .NotImpl
.NotCopy:
.NotDel:
        cmp word [bx], 'DI'
        jne .NotDir
        cmp word [bx+2], 'R'
        jne .NotInternal
        jmp CmdDir
.NotDir:
        cmp word [bx], 'EC'
        jne .NotEcho
        cmp word [bx+2], 'HO'
        jne .NotInternal
        cmp byte [bx+4], 0
        jne .NotInternal
        jmp CmdEcho
.NotEcho:
.NotExit:
.NotRen:

.NotInternal:
        mov dx, MsgErrBadCommand
        jmp .Error

.Ret:
        ret

.NotImpl:
        mov dx, MsgErrNotImpl
        ; Fall through
; Print command and exit with error message from dx
.Error:
        mov si, InFileName
.NIpr:
        lodsb
        and al, al
        jz .NId
        call PutChar
        jmp .NIpr
.NId:
        call PutCrLf
        jmp Error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Internal Commands
;;
;; Called with SI pointing at the command line after the command name
;; (possibly at a '.')
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

CmdDir:
        mov di, cs
        mov es, di
        ; Fill pattern into InFileName
        mov di, InFileName
.SkipSp:
        lodsb
        ; Skip spaces
        cmp al, ' '
        je .SkipSp

        ; No pattern means '*.*' = '*'
        cmp al, 0x0D
        jne .NotEmpty

        mov ax, '*'
        stosw
        jmp .Main

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

        mov ah, 0x4e
        xor cx, cx
        mov dx, InFileName
        int 0x21
        jc .Done
.Find:
        mov si, 0x80 ; DTA defaults to PSP:80h, Offset of filename is 0x1E
        add si, FF_FNAME
        mov cl, 11
.Print:
        lodsb
        and al, al
        jz .Pad
        call PutChar
        dec cl
        jmp .Print
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
        call PutDec9
        call PutCrLf
        ; Find next
        mov ah, 0x4F
        int 0x21
        jnc .Find
.Done:
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InFileName:       db 'SASM.COM', 0
resb 12 ; XX TEMP Just ensure enough space
InFileNameEnd: resb 1
OutFileName:      db 'Foo.TMP', 0
resb 13 ; XX TEMP Just ensure enough space
MsgErrGeneric:    db 'Generic error message$'
MsgErrOpenIn:     db 'Could not open input file$'
MsgErrOpenOut:    db 'Could not open output file$'
MsgErrRead:       db 'Error reading from file$'
MsgErrWrite:      db 'Error writing to file$'
MsgErrBadCommand: db 'Unknown command$'
MsgErrNotImpl:    db 'Not implemented$'

HelloMsg:         db 'Hello from command interpreter!', 13, 10, '$'

CommandLine:      db '    dir',0x0D
;CommandLine:      db '    echo.Hello world!',0x0D

InputFile:        resw 1
OutputFile:       resw 1
Buffer:           resb BUFFER_SIZE
