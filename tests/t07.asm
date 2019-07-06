        org 0x100

        mov cl, [buf1+0]
        mov [buf2], cl
        mov si, buf1
        mov di, buf2
        mov cx, [si+1]
        mov [di+1], cx
        mov bx, 3
        mov dx, [si+bx]
        mov [di+bx], dx

        mov dx, buf2
        mov ah, 9
        int 0x21

        mov ax, 0x4c00
        int 0x21


buf1:
        db '01234'
buf2:
        db 0, 0, 0, 0, 0, '$'
