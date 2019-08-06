        A equ 1
        B equ 2+3
        C equ 1+2*3
        D equ (5-4)*(2- 10/3)
        E equ A|B
        F equ D & 0x1f
        G equ 2 <<  3
        H equ -2 +3
        I equ ~(3 | 7)

        org 0x80*2+10

Lab:
dw A, B, C, D, E, F, G, H, I, A-42, Lab+121
db A, B, C, D, E, F, G, H, I, F^A|G % 2

        mov al, 'A'-'0'-10
        mov dx, Lab+2+7 % 3
        mov ax, G*H+Lab
        jmp 100 + D - 2*I
