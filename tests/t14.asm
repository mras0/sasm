        org 0x100

        add dx, [foo]
        cmp [foo], bx

        mov ax, 0x4c00
        int 0x21

foo: dw 0
