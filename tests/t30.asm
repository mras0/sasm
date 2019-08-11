        mov ax, ((2+3)/4)<<2
        mov ax, +2
        mov ax, -2
        mov bx, !0
        mov ax, -2

X equ 42
Y equ 60

%if X < Y
        mov al, 0
%endif


%if X > Y
        mov al, 0
%elif X == Y
        mov al, 1
%elif X ^^ Y
        mov al, 2
%else
        mov al, 3
%endif


%if X >= Y
        mov al, 0
%elif X == Y
        mov al, 1
%elif X <= Y
        mov al, 2
%else
        mov al, 3
%endif


%if X < Y
        %if X > Y && 1 < 2
                db 1
        %else
                db 2
        %endif
%else
        %if X != 22 || Y > 20
                db 3
        %else
                db 4
        %endif
%endif

