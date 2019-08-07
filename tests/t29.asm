        org 0x7c00

Start:
        jmp short RealStart
        nop
        db 0x01, 0x02
RealStart:
        jmp $
        jmp $+2
        jne $-10

        resb 510 - ($-$$)
        dw 0xaa55
