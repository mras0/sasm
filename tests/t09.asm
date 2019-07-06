        org 0
        add ax, [foo]
        movzx cx, [foo]
        movzx dx, byte [foo+2]
        movzx dx, bl
        add byte [foo], 4
        add word [foo], 4
        add word [foo+bx], 42
        add cx, [foo]
        inc byte [bar]
        inc word [bar]
        dec byte [bar]
        dec word [bar]
        add ax, foo
        rol dl, 2
        ror dl, 2
        rcl dl, 2
        rcr dl, 2
        shl dl, 2
        shr dl, 2
        sar dl, 2
        rol bx, 2
        ror bx, 2
        rcl bx, 2
        rcr bx, 2
        shl bx, 2
        shr bx, 2
        sar bx, 2
        div bx
        div bl
        mul cx
        mul cl
        idiv bx
        idiv bl
        imul cx
        imul cl
        call ax

foo: dw 0
bar: dw foo
