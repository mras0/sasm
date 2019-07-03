        org 0x100

main:
        mov ax, 0x0123
        call printhex4
        mov ax, 0x4567
        call printhex4
        mov ax, 0x89AB
        call printhex4
        mov ax, 0xCDEF
        call printhex4
        mov ax, 0x4c00
        int 0x21

putch:
        pusha
        mov ah, 0x02
        mov dl, al
        int 0x21
        popa
        ret

        ; Print hex digit in al
printhex1:
        push ax
        and al, 0xf
        add al, '0'
        cmp al, '9'
        jbe .pr
        add al, 7 ;'A'-'0'-10
.pr:
        call putch
        pop ax
        ret

        ; Print hex byte in al
printhex2:
        rol al, 4
        call printhex1
        rol al, 4
        call printhex1
        ret

        ; Print hex word in ax
printhex4:
        rol ax, 8
        call printhex2
        rol ax, 8
        call printhex2
        ret

msg: db 'Hello world!', 13, 10, 0
