        cpu 8086
        org 0x100
        div word [2]
        ja X
        jmp X
        jae short X
        jmp short X
        loopne X
X:
        jb short X
        jb short X
        loope X
