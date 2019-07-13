        org 0x100

BUFFER_SIZE       EQU 512

Main:
        ; Open file for reading
        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        jnc .OK
        mov dx, MsgErrOpenIn
        jmp Error
.OK:
        mov [InputFile], ax

        call ReadToBuffer
        mov si, Buffer
        mov cx, [BytesRead]
        cmp cx, 256
        jb .hd
        mov cx, 256
.hd:
        call HexDump

        ; Close input file
        mov ah, 0x3e
        mov bx, [InputFile]
        int 0x21
        jc GenericError
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

ReadToBuffer:
        mov ah, 0x3f
        mov bx, [InputFile]
        mov cx, BUFFER_SIZE
        mov dx, Buffer
        int 0x21
        jc .ReadError
        mov [BytesRead], ax
        ret
.ReadError:
        mov dx, MsgErrRead
        jmp Error


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InFileName:       db 'SASM.ASM', 0
MsgErrGeneric:    db 'Generic error message$'
MsgErrOpenIn:     db 'Could not open input file$'
MsgErrRead:       db 'Error reading from file$'

InputFile:        dw 0
BytesRead:        dw 0 ; Number of bytes read to Buffer
Buffer:  ; Must be at end!
