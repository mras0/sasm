        org 0x100

        push '12'
        push '3$'
        mov bp, sp
        mov ax, [bp]
        mov si, 2
        mov dx, [bp+si]
        mov [buf], cx
        mov [buf+2], dx
        add sp, 4

        mov ah, 9
        mov dx, buf
        int 0x21

        xor ax, ax
        inc ax
        inc bl
        dec ax
        dec bl

        mov ah, 0x4c
        int 0x21

buf:
        dw 0, 0
