        ; Case-sensitivity
ABC equ 2
ABc equ 4

X:      jmp short x
x:      jmp short X
Test:   jmp short TeSt
TeSt:   jmp short Test
        mov dx, ABC
        mov bx, ABc
