        not al
        not dx
        not byte [es:bx+2]
        neg bl
        neg si
        neg word [2]
        mov ax, last
a: resw 2
dw 0x1234
b: resb 10
db '?'
x:resb 100
last:
