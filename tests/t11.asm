        org 0x100

        push cs
        push ss
        push ds
        push es
        pop es
        pop ds
        pop ss
        pop ax
        mov bx, cs
        mov bx, ds
        mov bx, es
        mov bx, ss
        mov ss, bx
        mov ds, bx
        mov es, bx

        mov cl, [es:di]
        mov [es:di], cl

        mov cl, ' '

        mov ax, 0x4c00
        int 0x21
