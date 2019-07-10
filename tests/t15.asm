        org 256
        xchg bx, [dat]
        xchg [dat], dx
        mov cx, 0
        rep movsb
        rep movsw
        ret
dat:
        dw 0
        dw 'AB', 'XY'
