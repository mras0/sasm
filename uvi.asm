;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; UVI - Useless VI clone for DOS-like OSes ;;
;;                                          ;;
;; Copyright 2019 Michael Rasmussen         ;;
;; See LICENSE.md for details               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;
;; Major limitations:
;;  * Probably so buggy you shouldn't trust it with your files
;;  * There's only a handful of supported commands
;;  * dd/yy/pp only works on complete line (lists)
;;  * Only lines of length BUFFER_SIZE can be edited
;;  * Unoptimized

;;
;; The file is stored in a doubly linked list of lines (without CR+LF)
;; starting for 'FirstLine'. A pointer to the first displayed line (top
;; of the screen) is stored in 'DispLine' and its 1-based index is stored
;; in DispLineIdx.
;;
;; Cursor position is relative to the top-left of the current screen.
;; Note that 'CursorX' may be beyond the current line length. The current
;; amount of horizontal scroll is stored in 'CurHScroll'.
;;
;; 'NeedUpdate' is set to indicate that the screen should be redrawn.
;; Commands that move the cursor need to call PlaceCursor, which also
;; handles horizontal scrolling.
;;
;; The heap is managed like line buffers (and their structures MUST match),
;; meaning each block is limited to 0x10000-HEAPN_SIZE bytes of extra storage
;; (for characters) since the line length is limited to 65535 bytes.
;;
;; The calling convention is a bit ad hoc, but generally the current node
;; or object will be stored in ES:BX and other pointer pairs are in DX:AX
;; and/or DI:SI.
;;
;; Assemble using: nasm -f bin -o uvi.com uvi.asm
;; or sasm uvi.asm
;;

        org 0x100

BUFFER_SIZE     equ 512
NDISP_LINES     equ 24   ; Number of displayed lines (of text from the file)
DISP_LINE_WORDS equ 80   ; Width of screen (each is one character + one attribute byte)
DISP_LINE_BYTES equ 160  ; DISP_LINE_WORDS * 2
SLINE_OFFSET    equ 3840 ; DISP_LINE_BYTES * NDISP_LINES
SLINE_CNT_OFF   equ 3988 ; SLINE_OFFSET + DISP_LINE_BYTES - 6*2
MAX_LINE_WIDTH  equ 74   ; DISP_LINE_WORDS - (5 digits + space)

; Heap node
HEAPN_PREV   equ 0  ; DWORD Previous node (far pointer)
HEAPN_NEXT   equ 4  ; DWORD Next node (far pointer)
HEAPN_LENGTH equ 8  ; WORD  Length (bytes)
HEAPN_SIZE   equ 10

; Line header (NOTE: Initial layout MUST match heap node)
LINEH_PREV   equ 0  ; DWORD Previous line (far pointer)
LINEH_NEXT   equ 4  ; DWORD Next line (far pointer)
LINEH_LENGTH equ 8  ; WORD  Length
LINEH_SIZE   equ 10

                         ; +8 yiels...
; COLOR_BLACK   equ 0x0 ; 0x8 dark gray
; COLOR_BLUE    equ 0x1 ; 0x9 bright blue
; COLOR_GREEN   equ 0x2 ; 0xA bright green
; COLOR_CYAN    equ 0x3 ; 0xB bright cyan
; COLOR_RED     equ 0x4 ; 0xC bright red
; COLOR_MAGENTA equ 0x5 ; 0xD bright magenta
; COLOR_BROWN   equ 0x6 ; 0xE yellow
; COLOR_GRAY    equ 0x7 ; 0xF white

COLOR_NORMAL equ 0x07
COLOR_WARN   equ 0x0e
COLOR_ERROR  equ 0x4f

K_BACKSPACE equ 0x08
K_RETURN    equ 0x0D
K_ESCAPE    equ 0x1B
K_HOME      equ 0x4700
K_UP        equ 0x4800
K_PGUP      equ 0x4900
K_LEFT      equ 0x4B00
K_RIGHT     equ 0x4D00
K_END       equ 0x4F00
K_DOWN      equ 0x5000
K_PGDOWN    equ 0x5100
K_DELETE    equ 0x5300

; TODO: Handle (give error) when file doesn't fit in memory

Start:
        ; Get previous video mode
        mov ah, 0x0f
        int 0x10
        mov [PrevVideoMode], al

        mov ax, 0x03 ; Set mode 3 to ensure we're in a known state
        int 0x10

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

        xor ax, ax
        mov [NumLines], ax
        mov [TotalBytes], ax
        mov [TotalBytes+2], ax

        ; ES:BP -> CurrentLine
        ; ES:DI -> HeapPtr

        ; Open file
        mov ax, 0x3d00
        mov dx, FileName
        int 0x21
        mov dx, MsgErrOpenInit
        jc Error
        mov [File], ax

.Read:
        ; Read to buffer
        mov ah, 0x3f
        mov bx, [File]
        mov cx, BUFFER_SIZE
        mov dx, Buffer
        int 0x21
        mov dx, MsgErrReadInit
        jc Error
        and ax, ax
        jz .ReadDone
        add [TotalBytes], ax
        adc word [TotalBytes+2], 0
        mov cx, ax
        mov si, Buffer
.Char:
        movsb ; Copy from buffer to current line
        inc word [es:bp+LINEH_LENGTH] ; And increase line lnegth
        cmp byte [es:di+0xFFFF], 10 ; LF?
        jne .NextChar

        inc word [NumLines]
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

        ;
        ; Prepare display variables
        ;
        mov word [DispLineIdx], 1
        mov ax, [FirstLine]
        mov dx, [FirstLine+2]
        mov [DispLine], ax
        mov [DispLine+2], dx

        xor ax, ax
        mov [CursorX], ax
        mov [CurHScroll], ax
        mov [CursorRelY], al
        mov [TempReg], ax
        mov [TempReg+2], ax
        mov [IsInsertMode], al


        push 0xb800
        pop es

        ;
        ; Set initial status
        ;
        mov ah, COLOR_NORMAL
        mov di, SLINE_OFFSET
        mov si, FileName
        call SCopyStr
        call SFormatFileInfo

        mov byte [NeedUpdate], 1
        call SetNormalCursor
        call PlaceCursor
.MainLoop:
        mov word [Count], 0
        call CheckRedraw
.ReadKey:
        call ReadKey
        cmp al, K_ESCAPE
        je .MainLoop

        ; 0 when Count is 0 means go to home column
        cmp word [Count], 0
        jne .CheckCount
        cmp al, '0'
        jbe .Command
.CheckCount:
        ; Read Count
        cmp al, '0'
        jb .Command
        cmp al, '9'
        ja .Command
        sub al, '0'
        xor ah, ah
        mov cx, ax
        xor dx, dx
        mov ax, [Count]
        mov bx, 10
        mul bx
        and dx, dx
        jnz .ReadKey ; Overflow (TODO: Notify user)
        add ax, cx
        jc .ReadKey  ; Overflow
        mov [Count], ax
        push ax
        mov ah, COLOR_NORMAL
        mov di, SLINE_CNT_OFF
        pop dx
        call SPutDecWord
        jmp .ReadKey

.Command:
        push ax
        call ClearStatusLine
        pop ax

        call CommandFromKey
        and bx, bx
        jz .Unknown

        ; Run the command once and let it handle Count
        push es
        push bx
        call bx
        pop bx
        ; If it didn't, the command will be repeated Count-1 times
        mov cx, [Count]
        and cx, cx
        jz .CommandDone
        dec cx
        jz .CommandDone
.Repeat:
        push cx
        push bx
        call bx
        pop bx
        pop cx
        dec cx
        jnz .Repeat
.CommandDone:
        pop es
        jmp .MainLoop

.Unknown:
        push ax
        call ClearStatusLine
        mov ah, COLOR_ERROR
        mov di, SLINE_OFFSET
        pop dx
        call SPutHexWord
        mov si, MsgErrUnknownKey
        call SCopyStr
        jmp .MainLoop

CommandFromKey:
        push ax
        mov dx, ax
        mov si, .CommandList
.L:
        lodsw
        and ah, ah
        jnz .CheckFull
        and al, al
        je .Found ; End of list
        cmp al, dl
        je .Found
        jmp .Next
.CheckFull:
        cmp ax, dx
        je .Found
.Next:
        add si, 2
        jmp .L
.Found:
        mov bx, [si]
        pop ax
        ret
.CommandList:
        dw ':'      , ExCommand
        dw 'a'      , Append
        dw 'd'      , DeleteCmd
        dw 'h'      , MoveLeft
        dw K_LEFT   , MoveLeft
        dw 'i'      , InsertMode
        dw 'j'      , MoveDown
        dw K_DOWN   , MoveDown
        dw 'k'      , MoveUp
        dw K_UP     , MoveUp
        dw 'l'      , MoveRight
        dw K_RIGHT  , MoveRight
        dw 'p'      , PasteAfter
        dw 'y'      , Yank
        dw 'A'      , AppendAfter
        dw 'G'      , GotoLine
        dw 'I'      , InsertBefore
        dw 'P'      , PasteBefore
        dw '0'      , MoveCurHome
        dw K_HOME   , MoveCurHome
        dw '$'      , MoveCurEnd
        dw K_END    , MoveCurEnd
        dw K_PGUP   , PageUp
        dw 2        , PageUp      ; CTRL-B
        dw K_PGDOWN , PageDown
        dw 6        , PageDown    ; CTRL-F
        dw 0        , 0

Quit:
        call RestoreVideoMode
        xor al, al
        jmp Exit

OutOfMemory:
        mov al, 0xff
        mov dx, MsgErrOOM
        ; Fall through
; Exit with error code in AL and message in DX
Error:
        push ax
        push dx
        call RestoreVideoMode
        pop dx
        mov ah, 9
        int 0x21
        pop ax
        ; Fall through
Exit:
        mov ah, 0x4c
        int 0x21

RestoreVideoMode:
        xor ah, ah
        mov al, [PrevVideoMode]
        int 0x10
        ret

CheckRedraw:
        cmp byte [NeedUpdate], 0
        jnz .Draw
        ret
.Draw:
        mov byte [NeedUpdate], 0
        jmp DrawLines

; Read (possibly extended key) to AX
ReadKey:
        xor ax, ax
        int 0x16
        ret

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
        call PutHexWord
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

; Convert word in AX to decimal representation (ASCIIZ) store in DI
; On return DI points to the first character to print
CvtWordDec:
        push ax
        push bx
        push dx
        mov bx, 10
        add di, 5
        mov byte [di], 0
.Cvt:
        xor dx, dx
        div bx
        add dl, '0'
        dec di
        mov [di], dl
        and ax, ax
        jnz .Cvt
        pop dx
        pop bx
        pop ax
        ret

CvtPadDecWord:
        mov word [di], '  '
        mov word [di+2], '  '
        mov word [di+4], '  '
        jmp CvtWordDec

; Produce four hex digits to buffer in DI
CvtWordHex:
        push ax
        mov al, ah
        call CvtByteHex
        pop ax
CvtByteHex:
        push ax
        shr al, 4
        call CvtNibHex
        pop ax
CvtNibHex:
        push ax
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe .Store
        add al, 7
.Store:
        mov [di], al
        inc di
        pop ax
        ret

; Convert dword in DX:AX to decimal representation (ASCIIZ) store in DI
; On return DI points to the first character to print
ConvertDwordDec:
        push ax
        push bx
        push cx
        push dx
        add di, 9
        mov byte [di], 0
        mov bx, 10
.Cvt:
        push ax
        mov ax, dx
        xor dx, dx
        div bx
        mov cx, ax
        pop ax
        div bx
        xchg cx, dx
        add cl, '0'
        dec di
        mov [di], cl
        mov cx, ax
        add cx, dx
        jnz .Cvt
        pop dx
        pop cx
        pop bx
        pop ax
        ret

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
        mov al, ' '
        call PutChar
        call PutChar
        popa

        pusha
        mov dx, [es:di+HEAPN_PREV+2]
        mov ax, [es:di+HEAPN_PREV]
        call PutHexDword
        mov al, ' '
        call PutChar
        popa

        mov ax, [es:di+HEAPN_NEXT+2]
        mov di, [es:di+HEAPN_NEXT]
        mov es, ax

        pusha
        mov dx, es
        mov ax, di
        call PutHexDword
        call PutCrLf
        popa

.Check:
        mov ax, es
        add ax, di
        jnz .PrintHeap
        pop es
        popa
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

LoadFirstLine:
        mov bx, [FirstLine+2]
        mov es, bx
        mov bx, [FirstLine]
        ret

LoadDispLine:
        mov bx, [DispLine+2]
        mov es, bx
        mov bx, [DispLine]
        ret

; Load next line from ES:BX to DX:AX
; Return zero flag set if non-null
LoadLineNext:
        mov ax, [es:bx+LINEH_NEXT]
        mov dx, [es:bx+LINEH_NEXT+2]
        and ax, ax
        jnz .Ret
        and dx, dx
.Ret:
        ret

; Load next line from ES:BX to DX:AX
; Return zero flag set if non-null
LoadLinePrev:
        mov ax, [es:bx+LINEH_PREV]
        mov dx, [es:bx+LINEH_PREV+2]
        and ax, ax
        jnz .Ret
        and dx, dx
.Ret:
        ret

LLFromDXAX:
        mov es, dx
        mov bx, ax
        ret

LoadCursorLine:
        call LoadDispLine
        push cx
        mov cl, [CursorRelY]
        and cl, cl
        jz .D
        push ax
        push dx
.L:
        call LoadLineNext
        jz .Err
        call LLFromDXAX
        dec cl
        jnz .L
        pop dx
        pop ax
.D:
        pop cx
        ret
.Err:
        mov al, 0xff
        mov dx, .InternalError
        jmp Error

.InternalError: db 'Internal error: Line list invalid', 13, 10, '$'


; Link previous in DI:SI to next in DX:AX
Link2:
        push es
        push bx
        mov bx, di
        or bx, si
        jz .NoPrev
        mov es, di
        mov [es:si+LINEH_NEXT], ax
        mov [es:si+LINEH_NEXT+2], dx
.NoPrev:
        mov bx, ax
        or bx, dx
        jz .NoNext
        mov es, dx
        mov bx, ax
        mov [es:bx+LINEH_PREV], si
        mov [es:bx+LINEH_PREV+2], di
.NoNext:
        pop bx
        pop es
        ret

; Add free node in ES:DI
AddFreeNode:
        ; TODO: Add sorted and coalesce adjcent blocks
        push es
        push ax
        push dx
        push di
        ; Minimize DI
        mov dx, di
        shr dx, 4
        mov ax, es
        add dx, ax
        mov es, dx
        and di, 0x0f
        xor ax, ax
        mov [es:di+LINEH_NEXT], ax
        mov [es:di+LINEH_NEXT+2], ax
        mov [es:di+LINEH_PREV], ax
        mov [es:di+LINEH_PREV+2], ax
        mov ax, di
        mov dx, es
        xchg ax, [HeapFree]
        xchg dx, [HeapFree+2]
        mov [es:di+HEAPN_NEXT], ax
        mov [es:di+HEAPN_NEXT+2], dx
        push ds
        push bp
        mov bp, dx
        mov ds, bp
        mov bp, ax
        mov [ds:bp+HEAPN_PREV], di
        mov ax, es
        mov [ds:bp+HEAPN_PREV+2], ax
        pop bp
        pop ds
        pop di
        pop dx
        pop ax
        pop es
        ret

; Free line list in ES:BX
FreeLineList:
        pusha
        push es
.FreeLoop:
        mov ax, es
        or ax, bx
        jz .Done
        call LoadLineNext
        mov di, bx
        call AddFreeNode
        call LLFromDXAX
        jmp .FreeLoop

.Done:
        pop es
        popa
        ret

; Copy line list in ES:BX, returns new list in ES:BX
CopyLineList:
        mov ax, es
        or ax, bx
        jnz .NotEmpty
        ret
.NotEmpty:
        ; Allocate first node
        call .CopyLine
        ; Push pointer to first line
        push dx
        push ax
.CopyLoop:
        ; Save previous line in DI:SI
        mov di, dx
        mov si, ax
        call LoadLineNext
        jz .CopyDone
        call LLFromDXAX
        call .CopyLine
        call Link2
        jmp .CopyLoop
.CopyDone:
        pop bx
        pop es
        ret
        ; Copy line in ES:BX to DX:AX
.CopyLine:
        mov ax, [es:bx+LINEH_LENGTH]
        mov cx, ax
        push es
        push bx
        call Malloc
        mov dx, es
        mov ax, bx
        pop bx
        pop es
        pusha
        push ds
        push es
        mov di, ax
        add di, LINEH_SIZE
        mov si, bx
        add si, LINEH_SIZE
        mov ax, es
        mov es, dx
        mov ds, ax
        rep movsb
        pop es
        pop ds
        popa
        ret

; Allocate HEAPN_SIZE + AX bytes
; Return new memory in ES:BX
Malloc:
        push ax
        push cx
        push dx
        push si
        push di

        push ax

        xor si, si
        xor di, di

        mov bx, [HeapFree+2]
        mov es, bx
        mov bx, [HeapFree]
.FindBlock:
        mov cx, es
        or cx, bx
        jz OutOfMemory
        ; Move prev to DI:SI
        cmp ax, [es:bx+HEAPN_LENGTH]
        jbe .Found
        mov di, es
        mov si, bx
        mov cx, [es:bx+HEAPN_NEXT+2]
        mov bx, [es:bx+HEAPN_NEXT]
        mov es, cx
        jmp .FindBlock
.Found:
        call LoadLineNext
        ; ES:BX Current node
        ; DI:SI Previous
        ; DX:AX Next
        call Link2 ; Unlink current node
        ; Was this the first node?
        mov cx, si
        or cx, di
        jnz .NotFirst
        ; Yes, update
        mov [HeapFree], ax
        mov [HeapFree+2], dx
.NotFirst:
        xor cx, cx
        mov [es:bx+HEAPN_PREV], cx
        mov [es:bx+HEAPN_PREV+2], cx
        mov [es:bx+HEAPN_NEXT], cx
        mov [es:bx+HEAPN_NEXT+2], cx

        pop ax
        mov cx, [es:bx+HEAPN_LENGTH]
        mov [es:bx+HEAPN_LENGTH], ax ; Store length before we lose it
        sub cx, ax
        sub cx, HEAPN_SIZE
        jb .BlockUsed

        ; Create new heap node at ES:BX+HEAPN_SIZE+AX
        mov di, bx
        add di, HEAPN_SIZE
        add di, ax
        mov [es:di+HEAPN_LENGTH], cx

        call AddFreeNode
.BlockUsed:
        pop di
        pop si
        pop dx
        pop cx
        pop ax
        ret


; Allocate new line of length AX to ES:BX
NewLine:
        push ax
        call Malloc
        xor ax, ax
        mov [es:bx+LINEH_PREV], ax
        mov [es:bx+LINEH_PREV+2], ax
        mov [es:bx+LINEH_NEXT], ax
        mov [es:bx+LINEH_NEXT+2], ax
        pop ax
        mov [es:bx+LINEH_LENGTH], ax
        ret

MoveCurHome:
        mov word [CursorX], 0
        jmp PlaceCursor

MoveCurEnd:
        call LoadCursorLine
        mov bx, [es:bx+LINEH_LENGTH]
        and bx, bx
        je .Done
        dec bx
.Done:
        mov [CursorX], bx
        jmp PlaceCursor

MoveLeft:
        mov ax, [CursorX]
        cmp ax, 0
        jne .OK
        ret
.OK:
        dec ax
        ; If we move left on a short line move left even
        ; if the "virtual cursor" is beyond the end
        call LoadCursorLine
        mov bx, [es:bx+LINEH_LENGTH]
        and bx, bx
        jz .RepLen
        dec bx
        cmp ax, bx
        jb .Done
.RepLen:
        dec bx
        mov ax, bx
.Done:
        mov [CursorX], ax
        jmp PlaceCursor

MoveRight:
        call LoadCursorLine
        mov bx, [es:bx+LINEH_LENGTH]
        and bx, bx
        jz .NotOK
        dec bx
        mov ax, [CursorX]
        cmp ax, bx
        jb .OK
.NotOK:
        ret
.OK:
        inc word [CursorX]
        jmp PlaceCursor

MoveUp:
        mov al, [CursorRelY]
        and al, al
        jz ScrollUp
        dec al
        mov [CursorRelY], al
        jmp PlaceCursor
ScrollUp:
        mov byte [NeedUpdate], 1
        call LoadDispLine
        call LoadLinePrev
        jz .Done ; At start of file (TODO: Give warning)
        mov [DispLine], ax
        mov [DispLine+2], dx
        dec word [DispLineIdx]
        mov byte [NeedUpdate], 1
        call PlaceCursor
.Done:
        ret

MoveDown:
        mov al, [CursorRelY]
        inc al
        push ax
        ; Check if moving down would put us beyond EOF
        xor ch, ch
        mov cl, al
        call LoadDispLine
.L:
        call LoadLineNext
        jnz .OK
        add sp, 2 ; Discard AX
        ret
.OK:
        call LLFromDXAX
        dec cl
        jnz .L
        pop ax
        ; At final line?
        cmp al, NDISP_LINES
        jae ScrollDown
        mov [CursorRelY], al
        jmp PlaceCursor
ScrollDown:
        call LoadDispLine
        call LoadLineNext
        jz .Done ; At EOF
        mov [DispLine], ax
        mov [DispLine+2], dx
        inc word [DispLineIdx]
        mov byte [NeedUpdate], 1
        call PlaceCursor
.Done:
        ret

DoForEachDL:
        mov cx, NDISP_LINES
.L:
        push bx
        push cx
        call bx
        pop cx
        pop bx
        dec cx
        jnz .L
        ret

PageUp:
        mov bx, MoveUp ; TODO: ScrollUp?
        jmp DoForEachDL

PageDown:
        mov bx, MoveDown ; TODO: ScrollDown?
        jmp DoForEachDL

GotoLine:
        xor cx, cx
        xchg cx, [Count]
        and cx, cx
        jz .NotImplemented
        push cx
        call LoadFirstLine
        dec cx
        jz .Done
.GL:
        call LoadLineNext
        jz .Err ; Premature EOF (TODO: Notify user)
        call LLFromDXAX
        dec cx
        jnz .GL
.Done:
        mov [DispLine], bx
        mov bx, es
        mov [DispLine+2], bx
        pop ax
        mov [DispLineIdx], ax
        mov byte [CursorRelY], 0
        mov byte [NeedUpdate], 1
        jmp PlaceCursor
.Err:
        add sp, 2
        ret

.NotImplemented:
        mov si, .Msg
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        jmp SCopyStr

.Msg: db 'GotoLine without count not implemented', 0

; Set TempReg to ES:BX freeing any contents
SetTempReg:
        push ax
        push cx
        push dx
        mov ax, bx
        mov dx, es
        xchg ax, [TempReg]
        xchg dx, [TempReg+2]
        mov cx, ax
        or cx, dx
        jz .Done
        push es
        push bx
        mov es, dx
        mov bx, ax
        call FreeLineList
        pop bx
        pop es
.Done:
        pop dx
        pop cx
        pop ax
        ret

NewDoc:
        xor ax, ax
        call NewLine
        mov ax, es
        mov [DispLine], bx
        mov [DispLine+2], ax
        mov [FirstLine], bx
        mov [FirstLine+2], ax
        mov word [DispLineIdx], 1
        xor ax, ax
        mov [CursorX], ax
        mov [CursorRelY], al
        mov byte [NeedUpdate], 1
        jmp PlaceCursor

; Returns result of stepping list [Count] times in DX:AX
StepList:
        push es
        push bx
        xor cx, cx
        xchg cx, [Count]
        and cx, cx
        jz .FoundLines
        dec cx
        jz .FoundLines
.L:
        call LoadLineNext
        jz .FoundLines
        call LLFromDXAX
        dec cx
        jnz .L
.FoundLines:
        call LoadLineNext
        pop bx
        pop es
        ret

DeleteCmd:
        call ReadKey
        cmp al, K_ESCAPE
        jne .NotEsc
        ret
.NotEsc:
        cmp al, 'd'
        je .DeleteLines
        mov si, .Msg
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        jmp SCopyStr
.DeleteLines:
        call LoadCursorLine
        call StepList
        mov si, [es:bx+LINEH_PREV]
        mov di, [es:bx+LINEH_PREV+2]

        ; ES:BX First line to delete
        ; DX:AX Line after
        ; DI:SI Line before

        ; Set prev of cut chain to NULL
        xor cx, cx
        mov [es:bx+LINEH_PREV], cx
        mov [es:bx+LINEH_PREV+2], cx

        ; And next of last in chain
        push es
        push bx
        mov es, dx
        mov bx, ax
        mov cx, [es:bx+LINEH_PREV+2]
        mov bx, [es:bx+LINEH_PREV]
        mov es, cx
        xor cx, cx
        mov [es:bx+LINEH_NEXT], cx
        mov [es:bx+LINEH_NEXT+2], cx
        pop bx
        pop es

        ; Unlink chain from main list
        call Link2

        ; Move line list to temp register
        call SetTempReg

        ; Line chain in ES:BX has been cut, now update state

        ; Line number     Variable              Pointer
        ; ----------------------------------------------
        ; 1               FirstLine
        ; .
        ; .
        ; DispLineIndex   DispLine
        ;                                     <- DI:SI
        ; DLI+CursorRelY  CursorLine          <- ES:BX
        ; .
        ; .
        ; Cursor+Count    Last deleted line
        ;                                     <- DX:AX
        ; .
        ; .


        mov cx, di
        or cx, si
        jnz .NotFirst
        ; First line was cut
        mov cx, dx
        or cx, ax
        jz NewDoc ; All lines cut
        ; First line cut, but there are lines afterwards
        mov [FirstLine], ax
        mov [FirstLine+2], dx
.NotFirst:
        ; FirstLine updated
        ; and we know the document isn't empty (handled above)

        mov cl, [CursorRelY]
        and cl, cl
        jnz .CheckCursor
        ; Lines were deleted from the top of the screen, we just
        ; need to move DispLine (since the document isn't empty,
        ; DX:AX is never NULL)
        mov [DispLine], ax
        mov [DispLine+2], dx
        jmp .Done
.CheckCursor:
        ; Lines were cut from somewhere other than the top
        ; Check if CursorRelY needs to be moved
        call LoadDispLine
        xor ch, ch
.StepDisp:
        call LoadLineNext
        jz .StepDone
        call LLFromDXAX
        inc ch
        dec cl
        jnz .StepDisp
.StepDone:
        mov [CursorRelY], ch
.Done:
        mov byte [NeedUpdate], 1
        jmp PlaceCursor

.Msg: db 'DeleteCmd only implemented for dd', 0

Yank:
        call ReadKey
        cmp al, K_ESCAPE
        jne .NotEsc
        ret
.NotEsc:
        cmp al, 'y'
        je .YankLines
        mov si, .Msg
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        jmp SCopyStr
.YankLines:
        call LoadCursorLine
        call StepList
        ; Grab pointer to final line
        push ds
        mov ds, dx
        mov si, ax
        mov di, [si+LINEH_PREV+2]
        mov si, [si+LINEH_PREV]
        mov ds, di
        xor cx, cx
        ; Set last next pointer in the range to NULL
        mov [si+LINEH_NEXT], cx
        mov [si+LINEH_NEXT+2], cx
        pop ds

        ; Copy list from ES:BX till DI:SI (inclusive)
        ; DX:AX points to line after the last one to copy

        pusha
        call CopyLineList
        call SetTempReg
        popa

        ; Restore modified next pointer
        push ds
        mov ds, di
        mov [si+LINEH_NEXT], ax
        mov [si+LINEH_NEXT+2], dx
        pop ds

        ret

.Msg: db 'Yank only implemented for yy', 0

; Load TempReg to ES:BX, returns zero flag set if empty
LoadTempReg:
        push ax
        mov bx, [TempReg]
        mov ax, [TempReg+2]
        mov es, ax
        or ax, bx
        pop ax
        ret

; Step ES:BX to final node in list
StepToLast:
        push ax
        push dx
.Search:
        call LoadLineNext
        jz .SearchDone
        call LLFromDXAX
        jmp .Search
.SearchDone:
        pop dx
        pop ax
        ret

; Paste after cursor
PasteAfter:
        call LoadTempReg
        jnz .NotEmpty
        ret
.NotEmpty:
        call CopyLineList
        ; DX:AX Points to the line list to insert
        mov dx, es
        mov ax, bx
        call LoadCursorLine
        ; ES:BX Points to the cursor line
        mov si, [es:bx+LINEH_NEXT]
        mov di, [es:bx+LINEH_NEXT+2]
        ; DI:SI points to the old next pointer
        push si
        push di

        ; Link in new lines
        mov di, es
        mov si, bx
        call Link2

        ; Search of end of lines
        mov es, dx
        mov bx, ax
        call StepToLast
        mov di, es
        mov si, bx
        ; Link end of new lines to old next pointer
        pop dx
        pop ax
        call Link2
        mov byte [NeedUpdate], 1
        jmp MoveDown

PasteBefore:
        call LoadTempReg
        jnz .NotEmpty
        ret
.NotEmpty:
        call CopyLineList
        mov dx, es
        mov ax, bx
        call LoadCursorLine
        mov di, [es:bx+LINEH_PREV+2]
        mov si, [es:bx+LINEH_PREV]

        ; Handle FirstLine/DispLine invariants
        mov cx, si
        or cx, di
        jnz .NotFirst
        mov [FirstLine], ax
        mov [FirstLine+2], dx
.NotFirst:
        cmp byte [CursorRelY], 0
        jnz .NotAtTop
        mov [DispLine], ax
        mov [DispLine+2], dx
.NotAtTop:
        ; DX:AX New lines
        ; ES:BX Line to insert before
        ; DI:SI Line to insert after

        call Link2
        push es
        push bx
        mov es, dx
        mov bx, ax
        call StepToLast
        mov di, es
        mov si, bx
        pop ax
        pop dx
        call Link2
        mov byte [NeedUpdate], 1
        ret

ExCommand:
        mov word [Count], 0 ; TODO: If Count > 0 start with ':.,.+Count-1'
        mov di, SLINE_OFFSET
        mov ah, COLOR_NORMAL
        mov al, ':'
        stosw
        mov bx, Buffer
        mov byte [bx], ':'
        inc bx
        mov dh, NDISP_LINES
        mov dl, 1
.ReadLoop:
        call SetCursor
        call ReadKey
        cmp al, K_ESCAPE
        je .Abort
        cmp al, K_RETURN
        je .Done
        cmp al, K_BACKSPACE
        je .Backspace
        cmp al, ' '
        jb .ReadLoop
        cmp al, '~'
        ja .ReadLoop
        cmp dl, 79
        je .ReadLoop ; Don't go past edge of screen yet (TODO)
        mov ah, COLOR_NORMAL
        stosw
        mov [bx], al
        inc bx
        inc dl
        jmp .ReadLoop
.Done:
        mov byte [bx], 0
        call ClearStatusLine
        call PerformExCmd
.Abort:
        call PlaceCursor
        ret
.Backspace:
        dec bx
        mov byte [bx], 0
        sub di, 2
        mov word [es:di], 0x0720
        dec dl
        jz .Abort ; Backspaced over ':'
        jmp .ReadLoop

; Perform EX command in Buffer
PerformExCmd:
        mov ax, [Buffer]
        cmp ax, ':q'
        je Quit
        cmp ax, ':w'
        je ExWrite

InvalidExCmd:
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        mov si, MsgErrNotImpl
        call SCopyStr
        mov al, ' '
        stosw
        mov si, Buffer
        call SCopyStr
        ret

ExWrite:
        mov si, Buffer
        add si, 2
        lodsb
        cmp al, 0
        je .NormalWrite
        cmp al, ' '
        je .SkipSpaces
        cmp al, 'q' ; OK
        jne InvalidExCmd
.SkipSpaces:
        mov al, [si]
        and al, al
        je .NormalWrite
        cmp al, ' '
        jne .WriteWithFname
        inc si
        jmp .SkipSpaces
.NormalWrite:
        mov si, FileName
.WriteWithFname:
        ; Write to file in SI
        mov dx, si
        mov cx, 0x20
        mov ah, 0x3c
        int 0x21
        jnc .OpenOK
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        mov si, MsgErrCreate
        call SCopyStr
        mov si, dx
        jmp SCopyStr
.OpenOK:
        push si
        mov bp, ax ; Save file handle in BP
        xor ax, ax
        mov [NumLines], ax
        mov [TotalBytes], ax
        mov [TotalBytes+2], ax
        push es
        call LoadFirstLine
.WriteLoop:
        push bx
        mov cx, [es:bx+LINEH_LENGTH]
        add [TotalBytes], cx
        adc word [TotalBytes+2], 0
        push ds
        mov ax, es
        mov ds, ax
        mov dx, bx
        add dx, LINEH_SIZE
        mov ah, 0x40
        mov bx, bp
        int 0x21
        pop ds
        jc .WriteError
        ; TODO: Could probably halve number of write calls
        ;       by borrowing two bytes beyond the line for CR+LF
        mov ah, 0x40
        mov dx, .CRLF
        mov cx, 2
        add [TotalBytes], cx
        adc word [TotalBytes+2], 0
        int 0x21
        jc .WriteError
        inc word [NumLines]
        pop bx
.WriteOK:
        call LoadLineNext
        jz .WriteDone
        call LLFromDXAX
        jmp .WriteLoop
.WriteDone:
        call .CloseFile
        pop es
        pop si
        cmp byte [Buffer+2], 'q'
        je Quit
        mov di, SLINE_OFFSET
        mov ah, COLOR_NORMAL
        call SCopyStr
        call SFormatFileInfo
        mov si, .Written
        jmp SCopyStr
.CloseFile:
        mov bx, bp
        mov ah, 0x3e
        int 0x21
        ret
.WriteError:
        pop bx
        pop es
        pop si
        push ax
        call .CloseFile
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        mov si, MsgErrWrite
        call SCopyStr
        pop dx
        jmp SPutHexWord
.CRLF: db 13, 10
.Written: db ' written', 0


; Link2: Link previous in DI:SI to next in DX:AX

; Replace line in ES:BX with new line in DX:AX
ReplaceLine:
        pusha
        ; Load prev node
        mov si, [es:bx+LINEH_PREV]
        mov di, [es:bx+LINEH_PREV+2]

        ; First line?
        mov cx, si
        or cx, di
        jnz .NotFirst
        mov [FirstLine], ax
        mov [FirstLine+2], dx
.NotFirst:

        ; Link prev to new node
        call Link2

        ; And next
        mov si, ax
        mov di, dx
        mov ax, [es:bx+LINEH_NEXT]
        mov dx, [es:bx+LINEH_NEXT+2]
        call Link2

        ; Replacing DispLine?
        cmp bx, [DispLine]
        jne .NotDisp
        mov cx, es
        cmp cx, [DispLine+2]
        jne .NotDisp
        mov [DispLine], si
        mov [DispLine+2], di
.NotDisp:
        popa
        ret

; Replace line in ES:BX with EditBuf
EnterEdit:
        push ds
        push es
        mov cx, [es:bx+LINEH_LENGTH]
        mov [EditBufHdr+LINEH_LENGTH], cx
        mov si, es
        mov ds, si
        mov si, bx
        add si, LINEH_SIZE
        mov di, cs
        mov es, di
        mov di, EditBuffer
        rep movsb
        pop es
        pop ds

        mov ax, EditBufHdr
        mov dx, ds
        call ReplaceLine

        ; Free old line
        call AddFreeNode

        ret

; Add new line in place of EditBuf (with its contents)
ExitEdit:
        ; Alloc new line
        mov ax, [EditBufHdr+LINEH_LENGTH]
        push ax
        call Malloc
        pop cx
        ; Copy contents
        mov di, bx
        add di, LINEH_SIZE
        mov si, EditBuffer
        rep movsb
        ; Insert the line inplace of EditBuffer
        mov ax, bx
        mov dx, es
        mov bx, ds
        mov es, bx
        mov bx, EditBufHdr
        call ReplaceLine
        ret

InsertMode:
        xor ax, ax
        xchg ax, [Count]
        and ax, ax
        jz .NoCount
        mov si, .MsgErrNoCount
        jmp .ErrRetIn
.NoCount:
        ; Idea: Copy current line to buffer and link in (free old line of course)
        ; Then edit current line and copy in afterwards

        push es
        call LoadCursorLine
        mov cx, [es:bx+LINEH_LENGTH]
        cmp cx, BUFFER_SIZE
        jb .LenOK
        pop es
        mov si, MsgErrLineLong
.ErrRetIn:
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        jmp SCopyStr
.LenOK:
        ; Ensure 0 <= CursorX <= LineLength (one beyond allowed for append)
        mov ax, [CursorX]
        cmp ax, cx
        jbe .CursorOK
        mov [CursorX], cx
.CursorOK:
        mov byte [IsInsertMode], 1
        call EnterEdit
        call SetInsertCursor
        call PlaceCursor ; Set cursor again, it might be one beyond the
                         ; line length (in append mode) which wasn't
                         ; legal before.
        pop es

.InsertLoop:
        ;;;;;;;;;;;;;; TEMP
        ;;;call ClearStatusLine
        ;;;mov ah, COLOR_ERROR
        ;;;mov di, SLINE_OFFSET
        ;;;mov dx, [EditBufHdr+LINEH_LENGTH]
        ;;;call SPutHexWord
        ;;;mov al, ' '
        ;;;stosw
        ;;;mov dx, [CursorX]
        ;;;call SPutHexWord
        ;;;;;;;;;;;;;;;;;;;;;

        call CheckRedraw
        call ReadKey
        push ax
        call ClearStatusLine
        pop ax
        cmp al, K_ESCAPE
        je .InsertDone
        cmp al, K_BACKSPACE
        je .Backspace
        cmp ax, K_DELETE
        je .Delete
        cmp ax, K_LEFT
        je .CursorLeft
        cmp ax, K_RIGHT
        je .CursorRight
        cmp ax, K_HOME
        je .CursorHome
        cmp ax, K_END
        je .CursorEnd
        cmp al, ' '
        jb .Unknown
        ; Normal character
        jmp .InsertChar
.Unknown:
        ; Unknown key in insert mode
        push ax
        call ClearStatusLine
        mov ah, COLOR_ERROR
        mov di, SLINE_OFFSET
        pop dx
        call SPutHexWord
        mov si, MsgErrUnknownKey
        call SCopyStr
        jmp .InsertLoop
.InsertDone:
        call ExitEdit
        call SetNormalCursor
        mov byte [IsInsertMode], 0
        jmp PlaceCursor ; Limit cursor in case it was beyond end of line
.ErrRet:
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        call SCopyStr
        jmp .InsertLoop
.InsertChar:
        mov bx, [EditBufHdr+LINEH_LENGTH]
        inc bx
        cmp bx, BUFFER_SIZE
        jb .ILenOK
        mov si, MsgErrLineLong
        jmp .ErrRet
.ILenOK:
        mov [EditBufHdr+LINEH_LENGTH], bx

        ; Insert character before CursorX
        ; Move CursorX..Length one char up
        mov dx, [CursorX]
        mov si, EditBuffer
        add si, bx
        mov cx, bx
        sub cx, dx
        jbe .ShiftUpDone
.ShiftUp:
        mov ah, [si+0xFFFF]
        mov [si], ah
        dec si
        dec cx
        jnz .ShiftUp
.ShiftUpDone:
        mov [si], al
        inc word [CursorX]
        jmp .UpdateRet
.Backspace:
        mov bx, [CursorX]
        and bx, bx
        jz .InsertLoop
        dec bx
        mov cx, [EditBufHdr+LINEH_LENGTH]
        dec cx
        mov ax, cx
        sub cx, bx
        jb .InsertLoop
        mov [EditBufHdr+LINEH_LENGTH], ax
        mov [CursorX], bx
        je .UpdateRet ; Nothing to copy (flags set above)
        mov si, EditBuffer
        add si, bx
.ShiftDown:
        mov al, [si+1]
        mov [si], al
        inc si
        dec cx
        jnz .ShiftDown
.UpdateRet:
        mov byte [NeedUpdate], 1
.PlaceCursorRet:
        call PlaceCursor
        jmp .InsertLoop
.Delete:
        mov cx, [EditBufHdr+LINEH_LENGTH]
        and cx, cx
        jz .InsertLoop
        mov bx, [CursorX]
        cmp bx, cx
        jae .InsertLoop
        mov ax, cx
        dec ax
        mov [EditBufHdr+LINEH_LENGTH], ax
        sub cx, bx
        mov si, EditBuffer
        add si, bx
        jmp .ShiftDown
.CursorLeft:
        mov ax, [CursorX]
        sub ax, 1
        jc .InsertLoop
.NewCursor:
        mov [CursorX], ax
        jmp .PlaceCursorRet
.CursorRight:
        mov ax, [CursorX]
        inc ax
        cmp ax, [EditBufHdr+LINEH_LENGTH]
        ja .InsertLoop
        jmp .NewCursor
.CursorHome:
        xor ax, ax
        jmp .NewCursor
.CursorEnd:
        mov ax, [EditBufHdr+LINEH_LENGTH]
        jmp .NewCursor
.MsgErrNoCount: db 'Count not implemented for insert', 0

InsertBefore:
        push es
        call LoadCursorLine
        mov cx, [es:bx+LINEH_LENGTH]
        add bx, LINEH_SIZE
        xor si, si
.SearchNonBlank:
        cmp si, cx
        je .SearchDone
        cmp byte [es:bx+si], ' '
        ja .SearchDone
        inc si
        jmp .SearchNonBlank
.SearchDone:
        pop es
        mov [CursorX], si
        jmp InsertMode

Append:
        inc word [CursorX]
        jmp InsertMode

AppendAfter:
        push es
        call MoveCurEnd
        pop es
        jmp Append

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing functions
;;
;; Assumes ES=0xb800
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlaceCursor:
        mov dx, [CursorX]
        ; Check if cursor is beyond
        push es
        push bx
        call LoadCursorLine
        mov bx, [es:bx+LINEH_LENGTH]
        and bx, bx
        jz .Empty
        cmp byte [IsInsertMode], 0
        jnz .Empty ; Allow cursor at line length in insert mode
        dec bx
.Empty:
        cmp dx, bx
        jbe .NotLarger
        mov dx, bx
.NotLarger:
        mov bx, dx
        inc bx
        sub bx, MAX_LINE_WIDTH
        jae .HasScrollAmm
        xor bx, bx
.HasScrollAmm:
        cmp [CurHScroll], bx
        je .ScrollChecked
        mov [CurHScroll], bx
        mov byte [NeedUpdate], 1
.ScrollChecked:
        pop bx
        pop es
        cmp dx, MAX_LINE_WIDTH
        jb .NotBeyondW
        mov dl, MAX_LINE_WIDTH
        dec dl
.NotBeyondW:
        add dl, 6
        mov dh, [CursorRelY]
        ; Fall through

; Set cursor at DL, DH (0 indexed)
SetCursor:
        push ax
        push bx
        mov ah, 0x02
        xor bh, bh
        int 0x10
        pop bx
        pop ax
        ret

SetInsertCursor:
        push ax
        push cx
        mov ah, 0x01
        mov cx, 0x0507
        int 0x10
        pop cx
        pop ax
        ret

SetNormalCursor:
        push ax
        push cx
        mov ah, 0x01
        mov cx, 0x0007
        int 0x10
        pop cx
        pop ax
        ret

DrawLines:
        xor di, di
        mov ax, [DispLineIdx]
        mov bp, [DispLine]
        mov dx, [DispLine+2]
        mov cx, NDISP_LINES

        ; ES:DI Points to start of current line in video memory
        ; DX:BP Points to the current line header
        ; AX    Current line number
        ; CX    Number of lines to display

.Main:
        push es
        pusha

        ;
        ; Line number
        ;
        pusha
        mov di, Buffer
        call CvtPadDecWord
        popa
        mov si, Buffer
        mov ah, 0x17
.Pr:
        lodsb
        and al, al
        jz .PrDone
        stosw
        jmp .Pr
.PrDone:
        mov al, ' '
        stosw

        ;
        ; Line
        ;

        push ds
        mov ds, dx
        mov si, bp
        mov bx, [si+LINEH_LENGTH]
        and bx, bx
        jz .LineDone

        ; Handle horizontal scrolling
        mov cx, [cs:CurHScroll]
        sub bx, cx
        ja .HScroll
        xor bx, bx
        jmp .LineDone
.HScroll:
        add si, cx
        add si, LINEH_SIZE
        mov ah, 0x27
        mov cx, MAX_LINE_WIDTH
        cmp cx, bx
        jbe .PrLine
        mov cx, bx
.PrLine:
        lodsb
        stosw
        dec cx
        jnz .PrLine
.LineDone:
        pop ds
        mov cx, MAX_LINE_WIDTH
        sub cx, bx
        jbe .NoRest
        mov ax, 0x3720 ; ' '
        rep stosw
.NoRest:
        popa
        ; Move to next line
        mov es, dx
        mov dx, [es:bp+LINEH_NEXT+2]
        mov bp, [es:bp+LINEH_NEXT]
        pop es

        inc ax                  ; ++LineNumber
        add di, DISP_LINE_BYTES ; Move to next line in video memory
        dec cx                  ; --LinesLeft
        jz .Done
        ; Reached EOF?
        mov bx, dx
        add bx, bp
        jnz .Main
        mov bx, cx
.EmptyLines:
        push di
        mov ax, 0x177e ; '~'
        stosw
        mov al, ' '
        mov cx, 5
        rep stosw
        mov ax, 0x3720 ; ' '
        mov cx, MAX_LINE_WIDTH
        rep stosw
        pop di
        add di, DISP_LINE_BYTES ; Move to next line
        dec bx
        jnz .EmptyLines
.Done:
        ret

ClearStatusLine:
        mov di, SLINE_OFFSET
        mov cx, DISP_LINE_WORDS
        mov ax, 0x0720
        rep stosw
        ret

ClearSLineCnt:
        mov di, SLINE_CNT_OFF
        mov cx, 6
        mov ax, 0x0720
        rep stosw
        ret

;
; Status line formatting helpers
;

SCopyStr:
        lodsb
        and al, al
        jz .Done
        stosw
        jmp SCopyStr
.Done:
        ret

; Put hex word in DX
SPutHexWord:
        push ax
        push di
        mov di, Buffer
        mov si, di
        mov ax, dx
        call CvtWordHex
        mov byte [di], 0
        pop di
        pop ax
        jmp SCopyStr

; Put decimal word in DX, trashes Buffer
SPutDecWord:
        push ax
        push di
        mov di, Buffer
        mov ax, dx
        call CvtWordDec
        mov si, di
        pop di
        pop ax
        jmp SCopyStr

; Put decimal dword in CX:DX, trashes Buffer
SPutDecDword:
        push ax
        push di
        mov di, Buffer
        mov ax, dx
        mov dx, cx
        call ConvertDwordDec
        mov si, di
        pop di
        pop ax
        jmp SCopyStr

SFormatFileInfo:
        mov al, ' '
        stosw
        mov dx, [NumLines]
        call SPutDecWord
        mov al, 'L'
        stosw
        mov al, ','
        stosw
        mov al, ' '
        stosw
        mov dx, [TotalBytes]
        mov cx, [TotalBytes+2]
        call SPutDecDword
        mov al, 'C'
        stosw
        ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants/Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgErrOpenInit:   db 'Could not open file', 13, 10, '$'
MsgErrReadInit:   db 'Error reading from file', 13, 10, '$'
MsgErrOOM:        db 'Out of memory', 13, 10, '$'
MsgErrUnknownKey: db ' unknown key/command', 0
MsgErrNotImpl:    db 'Unknown/unimplemented command', 0
MsgErrCreate:     db 'Error creating ', 0
MsgErrWrite:      db 'Error writing to file: ', 0
MsgErrLineLong:   db 'Error line too long to be edited (sorry)', 0

FileName:         db 'sasm.asm',0

PrevVideoMode:    resb 1
HeapStartSeg:     resw 1
HeapFree:         resw 2
FirstLine:        resw 2
File:             resw 1
Buffer:           resb BUFFER_SIZE

EditBufHdr:       resb LINEH_SIZE
EditBuffer:       resb BUFFER_SIZE ; Must follow EditBufHdr

IsInsertMode:     resb 1
NeedUpdate:       resb 1
DispLineIdx:      resw 1 ; 1-based index of the first displayed line
DispLine:         resw 2 ; Far pointer to first displayed line (header)

CursorX:          resw 1 ; Current column. May be >= Line Length and >= MAX_LINE_WIDTH
CursorRelY:       resb 1 ; Cursor Y relative to first displayed line (DispLine)
CurHScroll:       resw 1 ; Current horizontal scroll amount

Count:            resw 1
TempReg:          resw 2 ; "-register, Contains a line list of the cut lines (or NULL)

; Not always up to date
NumLines:         resw 1
TotalBytes:       resw 2
