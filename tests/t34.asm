lab2:
        jle short lab1
        jge lab2
        xlat
        mov ax, [bx+si+0abcdh]
lab1:
