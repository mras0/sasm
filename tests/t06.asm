        org 0x100

        mov di, buf
        mov al, 'T'
        stosb
        mov byte [di], 'E'
        mov byte [di+1], 'S'
        mov bx, 2
        mov byte [bx+di], 'T'
        mov al, '!'
        mov [bx+di+1], al
        mov cx, '?@'
        mov [bx+di+2], cx
        mov byte [buf+7], '$'

        mov dx, buf
        mov ah, 0x09
        int 0x21

        mov ax, 0x4c00
        int 0x21

buf:
        db 0, 0, 0, 0, 0, 0, 0, 0
