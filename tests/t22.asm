        shr bl, cl
        shl dx, 1
        rol byte [X], cl
        ror word [X], 4
        ror word [X], 1
        pushf
        popf
        cbw
        cwd
        imul word [X]
        mov es, [X]
        mov [X], ds
X:
