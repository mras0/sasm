        push Foo
        push 0x7f
        push 0x80
        push 0x100
        push 0xffff
        push 0xff80
        cli
        sti
        hlt
        retf
Foo:
