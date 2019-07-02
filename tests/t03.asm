        org 0x100

main:
        mov si, msg
        call putstr

        mov ax, 0x4c00
        int 0x21

putch:
        pusha
        mov ah, 0x02
        mov dl, al
        int 0x21
        popa
        ret

putstr:
        push si
.print:
        lodsb
        and al, al
        jz .done
        call putch
        jmp .print
.done:
        pop si
        ret


msg: db 'Hello world!', 13, 10, 0
