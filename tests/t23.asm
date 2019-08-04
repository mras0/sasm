        cpu 8086
        cpu 286
        cpu 386
        cpu 186
a:
        int3
        into
        repne cmpsb
        repe cmpsw
        scasb
        scasw
        insb
        insw
        outsb
        outsw
        test dl, al
        test bx, cx
        test [si], dx
        test cl, [di+bx]
        test cl, 42
        test bx, 50
        test byte [si], 42
        test byte [bx], 60
        lea ax, [bx+42]
        les bx, [es:bx]
        lds si, [0]
        in al, 42
        in ax, 0xff
        in al, dx
        in ax, dx
        out 60, al
        out 72, ax
        out dx, al
        out dx, ax
        cmc
        aaa
        aad
        aam
        aas
        daa
        das
        jcxz a
        loop a
        loope a
        loopne a
        sahf
        lahf
        xlatb
