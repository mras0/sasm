        org 0x100

main:
        mov ax, 54321
        call printdec
        mov ax, 65535
        call printdec
        mov ax, 0
        call printdec

        mov ax, 0x4c00
        int 0x21

putch:
        pusha
        mov ah, 0x02
        mov dl, al
        int 0x21
        popa
        ret

        ; Print decimal number in ax
printdec:
        push ax
        push bx
        push dx
        ; XXX: Ugly to use the stack as a character buffer in this way, but meh
        push 0
        mov bx, 10
.l:
        xor dx, dx
        div bx
        xchg dx, ax
        add al, '0'
        push ax
        xchg dx, ax
        and ax, ax
        jnz .l
.p:
        pop ax
        and ax, ax
        jz .done
        call putch
        jmp .p
.done:
        pop dx
        pop bx
        pop ax
        ret
