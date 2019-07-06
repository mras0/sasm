        org 0x100
        jmp lab
        jnc lab
        call lab
        nop
lab:
        nop
        jmp lab
        jnc lab
        call lab
