        org 0x100

BUFFER_SIZE equ 512

; Heap node
HEAPN_PREV   equ 0  ; DWORD Previous node (far pointer)
HEAPN_NEXT   equ 4  ; DWORD Next node (far pointer)
HEAPN_LENGTH equ 8  ; WORD  Length (bytes)
HEAPN_SIZE   equ 10

; Line header
LINEH_PREV   equ 0  ; DWORD Previous line (far pointer)
LINEH_NEXT   equ 4  ; DWORD Next line (far pointer)
LINEH_LENGTH equ 8  ; WORD  Length
LINEH_SIZE   equ 10

; TODO: Handle (give error) when file doesn't fit in memory

Start:
        ; Init Heap
        mov ax, cs
        add ax, 0x1000
        mov [HeapStartSeg], ax
        mov word [FirstLine], 0
        mov [FirstLine+2], ax
        mov es, ax
        xor bp, bp
        xor di, di
        xor al, al
        mov cx, LINEH_SIZE
        rep stosb

        ; ES:BP -> CurrentLine
        ; ES:DI -> HeapPtr

        ; Open file
        mov ax, 0x3d00
        mov dx, FileName
        int 0x21
        mov dx, MsgErrOpen
        jc Error
        mov [File], ax

.Read:
        ; Read to buffer
        mov ah, 0x3f
        mov bx, [File]
        mov cx, BUFFER_SIZE
        mov dx, Buffer
        int 0x21
        mov dx, MsgErrRead
        jc Error
        and ax, ax
        jz .ReadDone
        mov cx, ax
        mov si, Buffer
.Char:
        movsb ; Copy from buffer to current line
        inc word [es:bp+LINEH_LENGTH] ; And increase line lnegth
        cmp byte [es:di+0xFFFF], 10 ; LF?
        jne .NextChar

        ; Remove CR+LF
        mov ax, 1
        cmp word [es:bp+LINEH_LENGTH], ax
        je .RemoveCrLf
        cmp byte [es:di+0xFFFE], 13 ; Char before CR?
        jne .RemoveCrLf
        inc ax
.RemoveCrLf:
        sub [es:bp+LINEH_LENGTH], ax
        sub di, ax

        ; Link in line
        mov [es:bp+LINEH_NEXT], di
        mov ax, es
        mov [es:bp+LINEH_NEXT+2], ax
        mov [es:di+LINEH_PREV], bp
        mov ax, es
        mov [es:di+LINEH_PREV+2], ax
        xor ax, ax
        mov [es:di+LINEH_NEXT], ax
        mov [es:di+LINEH_NEXT+2], ax
        mov [es:di+LINEH_LENGTH], ax

        ; Getting too close to 64K?
        cmp di, 0x8000
        jbe .NextLine
        mov ax, es
        add ax, 0x0800
        mov es, ax
        sub di, 0x8000
.NextLine:
        mov bp, di
        add di, LINEH_SIZE
.NextChar:
        dec cx
        jnz .Char
        jmp .Read
.ReadDone:
        ; Close file
        mov bx, [File]
        mov ah, 0x3e
        int 0x21

        mov ax, [es:bp+LINEH_PREV]
        mov bx, [es:bp+LINEH_PREV+2]
        mov cx, bx
        add cx, ax
        jz .FileRead ; Empty file
        cmp word [es:bp+LINEH_LENGTH], 0
        jne .FileRead ; Last line wasn't CR+LF terminated... TODO preserve this?
        ; Unlink final line (it doesn't contain anything)
        mov bp, ax
        mov es, bx
        xor ax, ax
        mov [es:bp+LINEH_NEXT], ax
        mov [es:bp+LINEH_NEXT+2], ax
.FileRead:
        mov [LastLine], bp
        mov ax, es
        mov [LastLine+2], ax

        ;
        ; Init Heap
        ;

        ; Account for bytes used in final line
        add bp, [es:bp+LINEH_LENGTH]
        add bp, LINEH_SIZE
        ; Round address to paragraph size
        add bp, 15
        shr bp, 4
        mov ax, es
        add ax, bp
        mov es, ax
        xor bp, bp

        mov [HeapFree+2], ax
        mov [HeapFree], bp

        ; Calculate number of free paragraphs
        mov bx, es
        mov cx, [2]
        sub cx, bx

        ; DX:DI = PREV
        xor dx, dx
        xor di, di
.InitHeap:
        and cx, cx
        jz .HeapDone

        mov ax, cx
        mov bx, 0x1000
        cmp ax, bx
        jbe .SetHeap
        mov ax, bx
.SetHeap:
        sub cx, ax
        shl ax, 4 ; Going to 0 is OK here
        sub ax, HEAPN_SIZE
        mov [es:bp+HEAPN_LENGTH], ax
        mov [es:bp+HEAPN_PREV], di
        mov [es:bp+HEAPN_PREV+2], dx
        mov ax, dx
        add ax, di
        jz .NextHeapBlock ; first block?
        push es
        mov ax, es
        mov es, dx
        mov [es:di+HEAPN_NEXT], bp
        mov [es:di+HEAPN_NEXT+2], ax
        pop es
.NextHeapBlock:
        xor ax, ax
        mov [es:bp+HEAPN_NEXT], ax
        mov [es:bp+HEAPN_NEXT+2], ax
        mov dx, es
        mov di, bp
        mov ax, es
        add ax, 0x1000 ; Not valid for final block, but doens't matter
        mov es, ax
        jmp .InitHeap
.HeapDone:

        call PrintLineBuf
        call PrintHeap

        ; Exit
        xor al, al
        jmp Exit

; Exit with error code in AL and message in DX
Error:
        push ax
        mov ah, 9
        int 0x21
        pop ax
        ; Fall through
Exit:
        mov ah, 0x4c
        int 0x21

; Put character in AL
PutChar:
        pusha
        mov dl, al
        mov ah, 2
        int 0x21
        popa
        ret

PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
        call PutChar
        ret

; Print dword in DX:AX
PutHexDword:
        push ax
        mov ax, dx
        call putHexWord
        mov al, ':'
        call PutChar
        pop ax
; Print word in AX
PutHexWord:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
PutHexByte:
        push ax
        shr al, 4
        call PutHexDigit
        pop ax
PutHexDigit:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe PutChar
        add al, 7
        jmp PutChar


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
PrintLineBuf:
        pusha
        push es
        mov bx, [FirstLine]
        mov ax, [FirstLine+2]
        mov es, ax
.Print:
        pusha
        mov dx, es
        mov ax, bx
        call PutHexDword
        mov al, ' '
        call PutChar
        popa
        pusha
        mov ax, [es:bx+LINEH_LENGTH]
        call PutHexWord
        mov al, ' '
        call PutChar
        popa

        mov cx, [es:bx+LINEH_LENGTH]
        and cx, cx
        jz .EmptyLine
        mov si, bx
        add si, LINEH_SIZE
.PrintChar:
        mov al, [es:si]
        call PutChar
        inc si
        dec cx
        jnz .PrintChar
.EmptyLine:
        call PutCrLf
        mov ax, [es:bx+LINEH_NEXT]
        mov cx, [es:bx+LINEH_NEXT+2]
        mov bx, ax
        mov es, cx
        add ax, cx ; NULL?
        jnz .Print
        pop es
        popa
        ret

PrintHeap:
        pusha
        push es
        mov di, [HeapFree+2]
        mov es, di
        mov di, [HeapFree]
        jmp .Check
.PrintHeap:
        pusha
        mov dx, es
        mov ax, di
        call PutHexDword
        mov al, ' '
        call PutChar
        popa

        pusha
        mov ax, [es:di+HEAPN_LENGTH]
        call PutHexWord
        call PutCrLf
        popa

        mov ax, [es:di+HEAPN_NEXT+2]
        mov di, [es:di+HEAPN_NEXT]
        mov es, ax
.Check:
        mov ax, es
        add ax, di
        jnz .PrintHeap
        pop es
        popa
        ret



MsgErrOpen:       db 'Could not open file', 13, 10, '$'
MsgErrRead:       db 'Error reading from file', 13, 10, '$'

FileName:         db 't01.asm',0
;FileName:         db 'sasm.asm', 0

HeapStartSeg:     resw 1
HeapFree:         resw 2
FirstLine:        resw 2
LastLine:         resw 2
File:             resw 1
Buffer:           resb BUFFER_SIZE
