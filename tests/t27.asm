        sub bx, 4
        xor cx, 2
        mov al, dl
        mov cl, al
        mov bx, ax
        mov dx, ax
        mov al, [0x1234]
        mov ax, [0x1234]
        mov [0xabcd], al
        mov [0xabcd], ax
        mov [0xabcd], al
        mov [0xabcd], ax
        mov word [0xaa55], 0xFFFF
        mov word [0x9876], 0x7f
        add word [0x1234], 0xFFFF
        sub word [bx+si+0x55aa], 0x7f
