        org 0x100

Start:
        mov ax, [0x80]
        call PutHexByte
        call PutCrLf
        mov si, 0x81
.Pr1:
        lodsb
        cmp al, 13
        je .Done1
        call PutHexByte
        mov al, ' '
        call PutChar
        jmp .Pr1
.Done1:
        call PutCrLf
        mov si, 0x81
.Pr2:
        lodsb
        cmp al, 13
        je .Done2
        call PutChar
        jmp .Pr2
.Done2:
        call PutCrLf
        mov ax, 0x4c00
        int 0x21

PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
        jmp PutChar

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
PutChar:
        mov ah, 2
        mov dl, al
        int 0x21
        ret
