        org 256
        xchg bx, [dat]
        xchg [dat], dx
        ret
dat:
        dw 0
        dw 'AB', 'XY'
