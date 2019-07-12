        org 0x1000
Start:
        stc
        clc
        rol dx, 12
        shr si, 4
        mov ax, .Foo
.Foo:
.Foo2:
Bar:
.Foo2:
.Foo:
Baz:
.Foo3:
.Foo:
.Foo2:
Foo:
.Fzz:
.Foo:
Fuz:
        mov ax, .Foo
.Foo:
