        org 0x100

        mov word [Ptr], Msg
        mov ah, 9
        mov dx, [Ptr]
        int 0x21
        mov ax, 0x4c00
        int 0x21



Msg: db 'Hello world!', 13, 10, '$'
Ptr: dw 0
