        org 0x100

        mov ah, 0x09
        mov dx, msg
        int 0x21

        mov ax, 0x4c00
        int 0x21

msg: db 'Hello world!', 13, 10, '$'
