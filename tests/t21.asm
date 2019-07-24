        org 0x100

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

X:
        mov ax, .CommandList
        ret

.CommandList:
        dw ':'      , ExCommand
        dw 'd'      , DeleteCmd
        dw 'h'      , MoveLeft
        dw K_LEFT   , MoveLeft
        dw 'j'      , MoveDown
        dw K_DOWN   , MoveDown
        dw 'k'      , MoveUp
        dw K_UP     , MoveUp
        dw 'l'      , MoveRight
        dw K_RIGHT  , MoveRight
        dw 'p'      , PasteAfter
        dw 'y'      , Yank
        dw 'G'      , GotoLine
        dw 'P'      , PasteBefore
        dw '0'      , MoveCurHome
        dw K_HOME   , MoveCurHome
        dw '$'      , MoveCurEnd
        dw K_END    , MoveCurEnd
        dw 0        , 0

DeleteCmd:	ret
ExCommand:	ret
GotoLine:	ret
MoveCurEnd:	ret
MoveCurHome:	ret
MoveDown:	ret
MoveLeft:	ret
MoveRight:	ret
MoveUp:	        ret
PasteAfter:	ret
PasteBefore:	ret
Yank:	        ret
