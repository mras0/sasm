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

; Line header
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

        ;
        ; Prepare display variables
        ;
        mov word [DispLineIdx], 1
        mov ax, [FirstLine]
        mov dx, [FirstLine+2]
        mov [DispLine], ax
        mov [DispLine+2], dx

        xor al, al
        mov [CursorRelY], al

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
        call PlaceCursor
.MainLoop:
        xor ax, ax
        mov [Count], ax
        cmp [NeedUpdate], al
        je .ReadKey
        mov [NeedUpdate], al
        call DrawLines
.ReadKey:
        call ReadKey
        cmp al, 0x1B ; ESC?
        je .MainLoop

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
        mov bx, ExCommand
        cmp al, ':'
        je .Done
        mov bx, MoveDown
        cmp al, 'j'
        je .Done
        mov bx, MoveUp
        cmp al, 'k'
        je .Done
        mov bx, GotoLine
        cmp al, 'G'
        je .Done
        ; Not found
        xor bx, bx
.Done:
        ret

Quit:
        call RestoreVideoMode
        xor al, al
        jmp Exit

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
.Done:
        ret

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
        ret
.Err:
        add sp, 2
        ret

.NotImplemented:
        mov si, .Msg
        mov di, SLINE_OFFSET
        mov ah, COLOR_ERROR
        jmp SCopyStr

.Msg: db 'GotoLine without count not implemented', 0


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
        cmp al, 0x1B
        je .Abort
        cmp al, 0x0D
        je .Done
        cmp al, 0x08
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing functions
;;
;; Assumes ES=0xb800
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

PlaceCursor:
        mov dl, 6
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
MsgErrUnknownKey: db ' unknown key/command', 0
MsgErrNotImpl:    db 'Unknown/unimplemented command', 0
MsgErrCreate:     db 'Error creating ', 0
MsgErrWrite:      db 'Error writing to file: ', 0

;FileName:         db 't09.asm',0
FileName:         db 'sasm.asm', 0

PrevVideoMode:    resb 1
HeapStartSeg:     resw 1
HeapFree:         resw 2
FirstLine:        resw 2
LastLine:         resw 2
File:             resw 1
Buffer:           resb BUFFER_SIZE

NeedUpdate:       resb 1
DispLineIdx:      resw 1 ; 1-based index of the first displayed line
DispLine:         resw 2 ; Far pointer to first displayed line (header)

CursorRelY:       resb 1 ; Cursor Y relative to First display line

Count:            resw 1

; Not always up to date
NumLines:         resw 1
TotalBytes:       resw 2
