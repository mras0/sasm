BEGIN equ 0x100
DOS_EXIT equ 0x4c
SPACE equ ' '
HEADER equ 'MZ'


        org BEGIN

        cmp dl, SPACE

        mov ah, DOS_EXIT
        xor al, al
        int 0x21

db DOS_EXIT
dw HEADER
