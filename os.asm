        org 0x0500

Main:
        mov bx, Msg
        call PutString
.Halt:
        hlt
        jmp .Halt

PutChar:
        mov ah, 0x0e
        int 0x10
        ret

PutString:
        mov al, [bx]
        and al, al
        jnz .P
        ret
.P:
        call PutChar
        inc bx
        jmp PutString

Msg: db 'Hello world!', 13, 10, 0
