;; Calling convention (unless otherwise mentioned):
;;
;;   Callee-saved: DS, BP, SI, DI
;;   Scratch: AX, BX, CX, DX, ES
;;   Return value: (DX:)AX or BX for pointer values,
;;                 boolean values via the carry flag
;;

; TODO: Investigate whether stack potentially overlaps with allocations
;;      SS:SP is intialized at the top of the 64K started at CS:0

TOKEN_MAX        equ 16         ; Maximum length of token (adjust token buffer if increasing)
INST_MAX         equ 5          ; Maximum length of directive/instruction
OUTPUT_MAX       equ 0x2000     ; Maximum output size
LABEL_MAX        equ 200        ; Maximum number of labels
FIXUP_MAX        equ 400        ; Maximum number of fixups
EQU_MAX          equ 100        ; Maximum number of equavates
DISPATCH_SIZE    equ 8          ; Size of DispatchListEntry
LABEL_SIZE       equ 22         ; Size of Label (TOKEN_MAX+2+2*sizeof(WORD))
LABEL_ADDR       equ 18         ; Offset of label address
LABEL_FIXUP      equ 20         ; Offset of label fixup
FIXUP_SIZE       equ 4          ; Size of Fixup
FIXUP_ADDR       equ 0          ; Offset of fixup address (into the output buffer)
FIXUP_LINK       equ 2          ; Offset of fixup link pointer (INVALID_ADDR terminates list)
EQU_SIZE         equ 20         ; Size of equate (TOKEN_MAX+2+sizeof(WORD))
EQU_VAL          equ 18         ; Offset of value in equate

HEX_ADJUST       equ 7          ; 'A'-'0'-10
QUOTE_CHAR       equ 39         ; '\''
INVALID_ADDR     equ 0xFFFF

; Value of Operand(L)Type:
; Less than 0xC0: Memory access with ModRM = OperandType
OP_REG          equ 0xC0
OP_LIT          equ 0xC1

; Register indices (OperandValue when OperandType = OP_REG)
; Lower 3 bits matches index, bits 4&5 give class (r8, r16, sreg)
R_AL             equ 0
R_CL             equ 1
R_DL             equ 2
R_BL             equ 3
R_AH             equ 4
R_CH             equ 5
R_DH             equ 6
R_BH             equ 7
R_AX             equ 8
R_CX             equ 9
R_DX             equ 10
R_BX             equ 11
R_SP             equ 12
R_BP             equ 13
R_SI             equ 14
R_DI             equ 15
R_ES             equ 16
R_CS             equ 17
R_SS             equ 18
R_DS             equ 19
R_INVALID        equ 21

; Condition Codes
CC_O             equ 0x0
CC_NO            equ 0x1
CC_C             equ 0x2
CC_NC            equ 0x3
CC_Z             equ 0x4
CC_NZ            equ 0x5
CC_NA            equ 0x6
CC_A             equ 0x7
CC_S             equ 0x8
CC_NS            equ 0x9
CC_PE            equ 0xa
CC_PO            equ 0xb
CC_L             equ 0xc
CC_NL            equ 0xd
CC_NG            equ 0xe
CC_G             equ 0xf

        org 0x100

ProgramEntry:
        call Init
        call MainLoop
        call Fini

        xor al, al
        jmp Exit

Init:
        ; First free paragraph is at the end of the program
        ; COM files get the largest available block
        mov ax, ProgramEnd
        add ax, 15
        shr ax, 4
        mov bx, cs
        add ax, bx
        mov [FirstFreeSeg], ax

        mov ax, OUTPUT_MAX
        mov bx, 1
        call Malloc
        mov [OutputSeg], ax

        mov ax, LABEL_MAX
        mov bx, LABEL_SIZE
        call Malloc
        mov [LabelSeg], ax

        mov ax, FIXUP_MAX
        mov bx, FIXUP_SIZE
        call Malloc
        mov [FixupSeg], ax
        ; Initial fixup list
        mov es, ax
        xor bx, bx
        mov cx, FIXUP_MAX
.FixupInit:
        mov ax, bx
        add ax, FIXUP_SIZE
        mov [es:bx+FIXUP_LINK], ax
        mov bx, ax
        dec cx
        jnz .FixupInit
        sub bx, FIXUP_SIZE
        mov word [es:bx+FIXUP_LINK], INVALID_ADDR ; Terminate free list

        mov ax, EQU_MAX
        mov bx, EQU_SIZE
        call Malloc
        mov [EquSeg], ax

        call ParserInit
        ret

Fini:
        call ParserFini

        call WriteOutput

        ret


NotImplemented:
        mov bx, MsgErrNotImpl
        ; Fall through

; Exit and print error message in BX
Error:
        ; Error in line ...
        push bx
        call PutCrLf
        mov bx, MsgErrInLine
        call PutString
        mov ax, [CurrentLine]
        call PutDec
        mov al, ':'
        call PutChar
        mov al, ' '
        call PutChar
        pop bx
        call PutString
        call PutCrLf
        ; Print curren token
        cmp byte [TokenLen], 0
        jz .NoTok
        mov bx, MsgCurToken
        call PutString
        mov bx, Token
        call PutString
        mov al, '"'
        call PutChar
        call PutCrLf
.NoTok:
        mov al, 0xff
        ; Fall through

; Exit with error code in AL
Exit:
        mov ah, 0x4c
        int 0x21

MainLoop:
        ; Update line counter
        mov ax, [NumNewLines]
        add [CurrentLine], ax
        mov word [NumNewLines], 0

        ; Grab next token
        call GetToken
        cmp byte [TokenLen], 0
        je .Done

        ; Dispatch
        call Dispatch

        jmp MainLoop

.Done:
        cmp byte [CurrentChar], 0
        je .Ret
        mov bx, MsgErrNotDone
        call Error
.Ret:
        ret

Dispatch:
        push si
        mov al, ':'
        call TryConsume
        jc .NotLabel
        call DefineLabel
        jmp .Done

.NotLabel:
        mov si, DispatchList

        ; Is the token too short/long to be a valid instruction/directive?
        mov al, [TokenLen]
        cmp al, 2
        jb .CheckEQU
        cmp al, INST_MAX
        ja .CheckEQU

        ; Initialize fixup pointers
        mov ax, INVALID_ADDR
        mov [CurrentFixup], ax
        mov [CurrentLFixup], ax
        ; And ExplicitSize
        mov byte [ExplicitSize], 0

.Compare:
        xor bx, bx
.CLoop:
        mov al, [Token+bx]
        mov ah, [si+bx]
        cmp al, ah
        jne .Next
        and al, al ; at NUL terminator?
        jz .Match
        inc bl
        cmp bl, INST_MAX
        jb .CLoop

.Match:
        ; Found match, dispatch
        movzx ax, byte [si+INST_MAX]
        mov bx, [si+INST_MAX+1]
        call bx
        cmp word [CurrentFixup], INVALID_ADDR
        jne .FixupUnhandled
        cmp word [CurrentLFixup], INVALID_ADDR
        jne .FixupUnhandled
        jmp .Done
.FixupUnhandled:
        mov bx, MsgErrFixupUnh
        jmp Error

.Next:
        add si, DISPATCH_SIZE
        cmp si, DispatchListEnd
        jb .Compare

.CheckEQU:
        mov al, 'E'
        call TryGetU
        jc .Invalid
        mov al, 'Q'
        call TryGetU
        jc .Invalid
        mov al, 'U'
        call TryGetU
        jc .Invalid
        call SkipWS
        call MakeEqu
.Done:
        pop si
        ret
.Invalid:
        mov bx, MsgErrUnknInst
        call Error

; Allocate AX*BX bytes, returns segment in AX
Malloc:
        ; Calculate how many paragraphs are needed
        mul bx
        and dx, dx
        jnz .Err ; Overflow
        add ax, 15
        shr ax, 4

        mov cx, [FirstFreeSeg]
        add ax, cx
        cmp ax, [2] ; (PSP) Segment of the first byte beyond the memory allocated to the program
        jae .Err ; Out of memory
        mov [FirstFreeSeg], ax
        mov ax, cx

        ret
.Err:
        mov bx, MsgErrMem
        jmp Error

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Screen Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write character in AL
PutChar:
        mov dl, al
        mov ah, 2
        int 0x21
        ret

; Write CR and LF
PutCrLf:
        mov al, 13
        call PutChar
        mov al, 10
        jmp PutChar

; Write ASCIIZ string in BX
PutString:
        mov al, [bx]
        and al, al
        jz .Done
        push bx
        call PutChar
        pop bx
        inc bx
        jmp PutString
.Done:
        ret

; Write decimal word in AX
PutDec:
        push di
        mov di, sp ; Assumes DS=SS
        sub sp, 6
        dec di
        mov byte [di], 0
        mov bx, 10
.DecLoop:
        xor dx, dx
        div bx
        add dl, '0'
        dec di
        mov [di], dl
        and ax, ax
        jnz .DecLoop
        mov bx, di
        call PutString
        add sp, 6
        pop di
        ret


; Write hexadecimal word in AX
PutHex:
        push ax
        mov al, ah
        call PutHexByte
        pop ax
        ; Fall through

; Write hexadecimal byte in AX
PutHexByte:
        push ax
        shr al, 4
        call PutHexDigit
        pop ax
        ; Fall through

; Write hexadecimal digit in AL
PutHexDigit:
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe PutChar
        add al, HEX_ADJUST
        jmp PutChar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File Output
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Write output buffer to file
WriteOutput:
        ; Create file
        mov dx, OutFileName
        mov cx, 0x0020 ; Attributes
        mov ah, 0x3c
        int 0x21
        jc .Error
        mov si, ax ; Save file handle in SI

        ; Write
        mov ah, 0x40
        mov bx, si
        mov cx, [NumOutBytes]
        ; ds:dx -> buffer
        mov dx, [OutputSeg]
        push ds
        mov ds, dx
        xor dx, dx
        int 0x21
        pop ds
        jc .Error
        cmp cx, [NumOutBytes]
        jne .Error

        ; Close file
        mov ax, 0x3e00
        mov bx, si
        int 0x21
        ret
.Error:
        mov bx, MsgErrOutput
        jmp Error


; Output byte in AL to output buffer
; Doesn't modify any registers
OutputByte:
        cmp word [NumOutBytes], OUTPUT_MAX
        jb .OK
        mov bx, MsgErrOutMax
        jmp Error
.OK:
        push di
        push es
        mov di, [OutputSeg]
        mov es, di
        mov di, [NumOutBytes]
        stosb
        inc word [NumOutBytes]
        inc word [CurrentAddress]
        pop es
        pop di
        ret

; Output word in AX
OutputWord:
        call OutputByte
        mov al, ah ; Only works because OutputByte is friendly
        jmp OutputByte

; Output 8-bit immediate if AL is 0, output 16-bit immediate otherwise
; the immediate is assumed to be in OperandValue
OutputImm:
        and al, al
        jz OutputImm8
        ; Fall through

; Output 16-bit immediate from OperandValue
OutputImm16:
        cmp word [CurrentFixup], INVALID_ADDR
        je .Output
        call RegisterFixup
.Output:
        mov ax, [OperandValue]
        jmp OutputWord

; Output 8-bit immediate from OperandValue
OutputImm8:
        mov al, [OperandValue]
        jmp OutputByte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

ParserInit:
        ; Open file for reading
        mov dx, FileName
        mov ax, 0x3d00
        int 0x21
        jnc .OK
        mov bx, MsgErrOpenIn
        jmp Error
.OK:
        mov [InputFile], ax
        call MoveNext
        ret

ParserFini:
        ; Close input file
        mov ax, 0x3e00
        mov bx, [InputFile]
        int 0x21
        ret

ReadNext:
        mov ah, 0x3f
        mov bx, [InputFile]
        mov cx, 1
        mov dx, CurrentChar
        int 0x21
        jc .NotOK  ; error code in AX
        cmp ax, 1
        jne .NotOK ; EOF
        cmp byte [CurrentChar], 10
        jne .Ret
        inc word [NumNewLines]
.Ret:
        ret
.NotOK:
        mov byte [CurrentChar], 0
        ret

; Try to get character in AL and ReadNext. Returns carry clear on success.
; I.e. it doesn't skip spaces after consuming the character.
TryGet:
        cmp [CurrentChar], al
        jne .NoMatch
        call ReadNext
        clc
        ret
.NoMatch:
        stc
        ret

; Like TryGet but case insensitive
TryGetU:
        call TryGet
        jc .NoMatch
        ret
.NoMatch:
        or al, ' ' ; ToUpper
        jmp TryGet

SkipWS:
        mov al, [CurrentChar]
        and al, al
        jz .Done
        cmp al, ' '
        ja .CheckComment
        call ReadNext
        jmp SkipWS
.CheckComment:
        cmp al, ';'
        jne .Done
.SkipComment:
        call ReadNext
        mov al, [CurrentChar]
        and al, al
        jz .Done
        cmp al, 10
        je SkipWS
        jmp .SkipComment
.Done:
        ret

MoveNext:
        call ReadNext
        call SkipWS
        ret

; Consume CurrentChar if it matches AL, move next and
; return carry clear. Carry is set ortherwise.
; AL is left unchanged (for Expect)
TryConsume:
        cmp [CurrentChar], al
        jne .NoMatch
        call MoveNext
        clc
        ret
.NoMatch:
        stc
        ret

; Abort if the next character isn't AL
Expect:
        call TryConsume
        jc .Error
        ret
.Error:
        mov bx, MsgErrExpected
        mov [bx], al
        jmp Error

GetToken:
        push di
        mov di, Token
.Get:
        mov al, [CurrentChar]
        cmp al, '.'
        je .Store
        cmp al, '_'
        je .Store
        ; IsDigit
        mov ah, al
        sub ah, '0'
        cmp ah, 9
        jbe .Store
        and al, 0xDF ; to lower case
        mov ah, al
        sub ah, 'A'
        cmp ah, 26
        ja .Done
.Store:
        cmp di, TokenEnd
        jae .Next ; don't store if beyond limit
        mov [di], al
        inc di
.Next:
        call ReadNext
        jmp .Get
.Done:
        mov byte [di], 0 ; zero-terminate
        sub di, Token
        mov ax, di
        mov [TokenLen], al
        pop di
        call SkipWS
        call FindEqu
        cmp bx, INVALID_ADDR
        je .NotEqu
        ; Found EQU convert it to a hex string (yes a bit lame)
        mov ax, [es:bx+EQU_VAL]
        mov byte [TokenLen], 6
        mov bx, Token
        mov word [bx], '0X'
        add bx, 2
        mov cl, 4
.Cvt:
        rol ax, 4
        mov ch, al
        and ch, 0x0f
        add ch, '0'
        cmp ch, '9'
        jbe .StoreDig
        add ch, HEX_ADJUST
.StoreDig:
        mov [bx], ch
        inc bx
        dec cl
        jnz .Cvt
        mov byte [bx], 0
.NotEqu:
        ret

; Get number to AX
GetNumber:
        call GetToken
        ; Fall through

; Get number from Token to AX
GetTokenNumber:
        cmp word [Token], '0X'
        je .Hex
        ; Decimal number
        xor ax, ax
        mov bx, Token
        xor ch, ch
.Dec:
        mov cl, [bx]
        and cl, cl
        jnz .NotDone
        ret
.NotDone:
        inc bx
        mov dx, 10
        mul dx
        and dx, dx
        jnz .Error
        sub cl, '0'
        cmp cl, 9
        ja .Error
        add ax, cx
        jmp .Dec
.Hex:
        mov cl, [TokenLen]
        sub cl, 2
        cmp cl, 4
        ja .Error
        xor ax, ax
        mov bx, Token
        add bx, 2
.HexCvt:
        shl ax, 4
        mov ch, [bx]
        inc bx
        sub ch, '0'
        cmp ch, 9
        jbe .Add
        sub ch, HEX_ADJUST
.Add:
        or al, ch
        dec cl
        jnz .HexCvt
        ret
.Error:
        mov bx, MsgErrInvalidNum
        jmp Error

; Returns carry clear if token is number
IsTokenNumber:
        mov al, [Token]
        sub al, '0'
        cmp al, 9
        ja .NotNumber
        clc
        ret
.NotNumber:
        stc
        ret

; Read one or two character literal to OperandValue
; Assumes initial quote character has been consumed
GetCharLit:
        movzx ax, [CurrentChar]
        mov [OperandValue], ax
        call ReadNext
        mov al, QUOTE_CHAR
        call TryConsume
        jnc .Done
        mov al, [CurrentChar]
        mov [OperandValue+1], al
        call ReadNext
        mov al, QUOTE_CHAR
        call Expect
.Done:
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Operand Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Get token and see if it's a register or number.
; Returns carry clear on success (token handled).
GetRegOrNumber:
        call GetToken
        ; Check if register
        cmp byte [TokenLen], 2
        jne .CheckNumber
        mov ax, [Token]
        mov bx, RegNames
        mov cl, R_INVALID
.ChecReg:
        cmp ax, [bx]
        jne .NextReg
        sub bx, RegNames
        shr bx, 1
        mov [OperandValue], bx
        mov byte [OperandType], OP_REG
        clc
        ret
.NextReg:
        add bx, 2
        dec cl
        jnz .ChecReg
        ; Fall through
.CheckNumber:
        call IsTokenNumber
        jnc .Number
        ret ; Keep carry set
.Number:
        call GetTokenNumber
        mov [OperandValue], ax
        mov byte [OperandType], OP_LIT
        clc
        ret

; Get operand to OperandType/OperandValue (and possibly CurrentFixup)
GetOperand:
        ; TODO: Character literal
        mov al, QUOTE_CHAR
        call TryGet
        jc .NotLit
        mov byte [OperandType], OP_LIT
        jmp GetCharLit
.NotLit:
        cmp byte [CurrentChar], '['
        je GetOperandMem

        call GetRegOrNumber
        jc .NotRegOrNumber
        ret

.NotRegOrNumber:
        ; Check for word/byte [mem]
        cmp byte [TokenLen], 4
        jne .CheckNamedLit
        mov ax, [Token]
        mov bx, [Token+2]
        cmp ax, 'BY'
        jne .CheckWord
        cmp bx, 'TE'
        jne .CheckNamedLit
        mov byte [ExplicitSize], 1
        jmp GetOperandMem
.CheckWord:
        cmp ax, 'WO'
        jne .CheckNamedLit
        cmp bx, 'RD'
        jne .CheckNamedLit
        mov byte [ExplicitSize], 2
        jmp GetOperandMem
.CheckNamedLit:
        ; Otherwise do named literal (fall through)
GetNamedLiteral:
        mov byte [OperandType], OP_LIT
        call FindOrMakeLabel
        mov ax, [LabelSeg]
        mov es, ax
        mov ax, [es:bx+LABEL_ADDR]
        cmp ax, INVALID_ADDR
        je .NeedFixup
        mov [OperandValue], ax
        ret
.NeedFixup:
        mov word [OperandValue], 0
        ;
        ; Add fixup for the label
        ;

        ; First grab a free fixup pointer
        mov ax, [NextFreeFixup]
        cmp ax, INVALID_ADDR
        jne .FixupValid
        mov bx, MsgErrFixupMax
        jmp Error
.FixupValid:
        ; Store the fixup along the other operand data
        mov [CurrentFixup], ax
        ; And update the linked list
        mov cx, [es:bx+LABEL_FIXUP] ; Remember old fixup pointer
        mov [es:bx+LABEL_FIXUP], ax ; Store fixup pointer

        ; Done with the label, switch to pointing to the fixups
        mov bx, [FixupSeg]
        mov es, bx
        mov bx, ax ; es:bx points to the new fixup node

        ; Update free list
        mov ax, [es:bx+FIXUP_LINK]
        mov [NextFreeFixup], ax

        ; And point the link at the old head
        mov [es:bx+FIXUP_LINK], cx

        ret

GetOperandMem:
        push si
        push di
        mov al, '['
        call Expect

        ; si = MODRM (0xff = displacement only)
        ; di = DISP
        mov si, 0xff
        mov di, 0

.Main:
        call GetRegOrNumber
        jc .NamedLit
        cmp byte [OperandType], OP_REG
        jne .Lit
        mov al, [OperandValue]
        cmp al, R_AX
        jb InvalidOperand
        cmp al, R_ES
        jae .SegOverride
        call .CombineModrm
        jmp .Next
.SegOverride:
        ; Output segment override here even though it's a bit dirty
        sub al, R_ES
        shl al, 3
        or al, 0x26
        call OutputByte
        mov al, ':'
        call Expect
        jmp .Main
.NamedLit:
        call GetNamedLiteral
        ; Fall through
.Lit:
        add di, [OperandValue]
        jmp .Next
.Next:
        mov al, '+'
        call TryConsume
        jnc .Main
        mov al, ']'
        call Expect
        mov [OperandValue], di
        ; Can't encode [bp] as MODRM=6
        mov ax, si
        cmp al, 6
        jne .NotBPRaw
        and di, di
        jnz .NotBPRaw
        or al, 0x40 ; Disp8
.NotBPRaw:
        cmp al, 0xff
        jne .NotDispOnly
        mov al, 6
        jmp .Done
.NotDispOnly:
        cmp word [CurrentFixup], INVALID_ADDR
        je .NoFixup
.Disp16:
        or al, 0x80 ; Disp16
        jmp .Done
.NoFixup:
        and di, di
        jz .Done
        mov bx, di
        movsx bx, bl
        cmp bx, di
        jne .Disp16
        or al, 0x40 ; Disp8
        ; Fall through
.Done:
        mov [OperandType], al
        pop di
        pop si
        ret

; Modify MODRM in si with 16-bit register in al
.CombineModrm:
        and al, 7
        mov cx, si
        mov bx, .TabFF
        cmp cl, 0xff
        je .Translate
        mov bx, .Tab04
        cmp cl, 0x04
        je .Translate
        mov bx, .Tab05
        cmp cl, 0x05
        je .Translate
        mov bx, .Tab06
        cmp cl, 0x06
        je .Translate
        mov bx, .Tab07
        cmp cl, 0x07
        jne InvalidOperand
        ; Fall through
.Translate:
        movzx cx, al
        add bx ,cx
        mov al, [bx]
        cmp al, 0xff
        je InvalidOperand
        movzx si, al
        ret
;
; Conversion tables from previous modrm value to new
;
;            AX    CX    DX    BX    SP    BP    SI    DI    Current ModR/M meaning
.Tab04: db  0xff, 0xff, 0xff, 0x00, 0xff, 0x02, 0xff, 0xff ; SI
.Tab05: db  0xff, 0xff, 0xff, 0x01, 0xff, 0x03, 0xff, 0xff ; DI
.Tab06: db  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x02, 0x03 ; BP
.Tab07: db  0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00, 0x01 ; BX
.TabFF: db  0xff, 0xff, 0xff, 0x07, 0xff, 0x06, 0x04, 0x05 ; Displacement only

SwapOperands:
        mov al, [OperandType]
        xchg al, [OperandLType]
        mov [OperandType], al
        mov ax, [OperandValue]
        xchg ax, [OperandLValue]
        mov [OperandValue], ax
        ; Fall through
SwapFixup:
        mov ax, [CurrentFixup]
        xchg ax, [CurrentLFixup]
        mov [CurrentFixup], ax
        ret

Get2Operands:
        call GetOperand
        mov al, ','
        call Expect
        call SwapOperands
        jmp GetOperand

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Define label (from Token) at current address
DefineLabel:
        cmp byte [Token], '.'
        je .Local
        ; Non-local label, retire all previously defined local labels
        call RetireLocLabs
.Local:
        call FindLabel
        cmp bx, INVALID_ADDR
        jne .KnownLabel
        call MakeLabel
        mov ax, [LabelSeg]
        mov es, ax
        mov ax, [CurrentAddress]
        mov [es:bx+LABEL_ADDR], ax
        ret
.KnownLabel:
        mov ax, [LabelSeg]
        mov es, ax
        cmp word [es:bx+LABEL_ADDR], INVALID_ADDR
        je .NotDup
        mov bx, MsgErrDupLabel
        jmp Error
.NotDup:
        mov ax, [CurrentAddress]
        mov [es:bx+LABEL_ADDR], ax ; Set label address
        ; Resolve fixups
        mov cx, [es:bx+LABEL_FIXUP] ; Store last valid fixup in CX
        mov word [es:bx+LABEL_FIXUP], INVALID_ADDR
        mov bx, cx
        push si
        push di
        mov dx, [FixupSeg]
        mov di, [OutputSeg]
.ResolveFixup:
        cmp bx, INVALID_ADDR
        je .Done

        mov cx, bx ; Update last valid fixup node
        mov es, dx
        mov si, [es:bx+FIXUP_ADDR]
        mov bx, [es:bx+FIXUP_LINK]
        mov es, di
        add [es:si], ax

        jmp .ResolveFixup
.Done:
        cmp cx, INVALID_ADDR
        je .NoFixups
        mov es, dx
        mov bx, cx
        mov ax, [NextFreeFixup]
        mov [es:bx+FIXUP_LINK], ax
        mov [NextFreeFixup], bx
.NoFixups:
        pop di
        pop si
        ret

; Print Label in BX (assumes ES=LabelSeg). Registers preserved.
PrintLabel:
        push ds
        pusha
        mov ax, es
        mov ds, ax
        call PutString
        mov al, ' '
        call PutChar
        popa
        pop ds
        mov ax, [es:bx+LABEL_ADDR]
        pusha
        call PutHex
        mov al, ' '
        call PutChar
        popa
        mov ax, [es:bx+LABEL_FIXUP]
        pusha
        call PutHex
        call PutCrLf
        popa
        ret

; Print all labels (registers preserved)
PrintLabels:
        push es
        pusha
        mov ax, [LabelSeg]
        mov es, ax
        xor bx, bx
        mov cx, [NumLabels]
        and cx, cx
        jz .Done
.L:
        call PrintLabel
        add bx, LABEL_SIZE
        dec cx
        jnz .L
.Done:
        popa
        pop es
        ret

RetireLocLabs:
        mov ax, [LabelSeg]
        mov es, ax
        xor bx, bx
        mov cx, [NumLabels]
        and cx, cx
        jnz .L
        ret
.L:
        cmp byte [es:bx], '.'
        jne .Next

        cmp word [es:bx+LABEL_ADDR], INVALID_ADDR
        jne .NotInv
.Inv:
        call PrintLabels
        call PrintLabel
        mov bx, MsgErrUndefLab
        jmp Error
.NotInv:
        cmp word [es:bx+LABEL_FIXUP], INVALID_ADDR
        jne .Inv

        mov ax, [NumLabels]
        dec ax
        mov [NumLabels], ax
        jz .Done

        mov dx, LABEL_SIZE
        mul dx

        push ds
        push si
        push di
        push cx
        mov si, es
        mov ds, si
        mov di, bx
        mov si, ax
        mov cx, LABEL_SIZE
        rep movsb
        pop cx
        pop di
        pop si
        pop ds

        sub bx, LABEL_SIZE ; Re-do current label

.Next:
        add bx, LABEL_SIZE
        dec cx
        jnz .L
.Done:
        ret

; Register fixup for CurrentFixup at this exact output location
RegisterFixup:
        mov bx, [CurrentFixup]
        mov ax, INVALID_ADDR
        cmp bx, ax
        jne .OK
        mov bx, MsgErrInternalErr
        jmp Error
.OK:
        mov [CurrentFixup], ax
        mov ax, [FixupSeg]
        mov es, ax
        mov ax, [NumOutBytes]
        mov [es:bx+FIXUP_ADDR], ax
        ret

; Find label matching Token, returns pointer in BX or
; INVALID_ADDR if not found
FindLabel:
        push si
        push di
        mov cx, [NumLabels]
        and cx, cx
        jz .NotFound
        mov ax, [LabelSeg]
        mov es, ax
        xor bx, bx
.Search:
        mov si, Token
        mov di, bx
.Compare:
        mov al, [si]
        cmp [es:di], al
        jne .Next
        and al, al
        jz .Done
        inc si
        inc di
        jmp .Compare
.Next:
        add bx, LABEL_SIZE
        dec cx
        jnz .Search
.NotFound:
        mov bx, INVALID_ADDR
.Done:
        pop di
        pop si
        ret

; Returns pointer to label in BX. Label must NOT exist.
MakeLabel:
        mov ax, [NumLabels]
        cmp ax, LABEL_MAX
        jb .OK
        mov bx, MsgErrLabelMax
        jmp Error
.OK:
        inc word [NumLabels]
        mov bx, LABEL_SIZE
        mul bx
        mov bx, ax
        mov ax, [LabelSeg]
        mov es, ax
        mov ax, INVALID_ADDR
        mov [es:bx+LABEL_ADDR], ax
        mov [es:bx+LABEL_FIXUP], ax
        push di
        push si
        movzx cx, [TokenLen]
        inc cx
        mov di, bx
        mov si, Token
        rep movsb
        pop si
        pop di
        ret

; Returns pointer to label (from Token) in BX
FindOrMakeLabel:
        call FindLabel
        cmp bx, INVALID_ADDR
        je MakeLabel
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQU Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Find EQU (from Token) in BX, INVALID_ADDR if not found
; Leaves ES pointer to EquSeg
FindEqu:
        push si
        push di
        mov ax, [EquSeg]
        mov es, ax
        mov cx, [NumEqus]
        and cx, cx
        jz .NotFound
        xor bx, bx
.Main:
        mov si, Token
        mov di, bx
.Compare:
        mov al, [si]
        cmp [es:di], al
        jne .Next
        and al, al
        jz .Done
        inc si
        inc di
        jmp .Compare
.Next:
        add bx, EQU_SIZE
        dec cx
        jnz .Main
        ; Fall through
.NotFound:
        mov bx, INVALID_ADDR
.Done:
        pop di
        pop si
        ret

; Create new EQU from Token and the next token(s) (EQU assumed to be parsed)
MakeEqu:
        call FindEqu
        cmp bx, INVALID_ADDR
        je .NewEqu
        mov bx, MsgErrDupEqu
        jmp Error
.NewEqu:
        mov ax, [NumEqus]
        cmp ax, EQU_MAX
        jb .HasRoom
        mov bx, MsgErrEquMax
        jmp Error
.HasRoom:
        inc word [NumEqus]
        mov bx, EQU_SIZE
        mul bx
        push si
        push di
        movzx cx, [TokenLen]
        inc cx
        mov di, ax
        mov si, Token
        rep movsb
        pop di
        pop si
        push ax
        mov al, QUOTE_CHAR
        call TryGet
        jc .Number
        call GetCharLit
        mov ax, [OperandValue]
        jmp .HasVal
.Number:
        call GetNumber
.HasVal:
        pop bx
        mov [es:bx+EQU_VAL], ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DirORG:
        call GetNumber
        mov [CurrentAddress], ax
        ret

; AL = 1 if DB 2 if DW
DirDX:
        push si
        mov si, ax
        cmp al, 1
.Main:
        mov al, QUOTE_CHAR
        call TryGet
        jc .NotLit
        ; Handle character literal
.CharLit:
        mov al, QUOTE_CHAR
        call TryConsume
        jnc .Next
        mov al, [CurrentChar]
        cmp al, ' '
        jae .OK
        mov bx, MsgErrChLitErr
        jmp Error
.OK:
        call OutputByte ; Always output bytes for character liters
        call ReadNext
        jmp .CharLit
.NotLit:
        call GetToken
        call IsTokenNumber
        jc .NamedLit
        call GetTokenNumber
        cmp si, 1
        jne .OutputWord
        call OutputByte
        jmp .Next
.OutputWord:
        call OutputWord
        jmp .Next
.NamedLit:
        cmp si, 2
        jne NotImplemented
        call GetNamedLiteral
        mov ax, [OperandValue]
        call OutputWord
        ; Fall thorugh
.Next:
        mov al, ','
        call TryConsume
        jnc .Main
        pop si
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

InvalidOperand:
        mov bx, MsgErrInvalidOpe
        jmp Error

HandleRel16:
        cmp byte [OperandType], OP_LIT
        jne InvalidOperand
        cmp word [CurrentFixup], INVALID_ADDR
        je .NoFixup
        call RegisterFixup
.NoFixup:
        mov ax, [OperandValue]
        sub ax, [CurrentAddress]
        sub ax, 2
        jmp OutputWord

; Output instruction byte in AL and Rel8 if OperandValue is a short
; (known) jump. Returns carry clear on success
HandleShortRel:
        cmp word [CurrentFixup], INVALID_ADDR
        jne .NotShort ; Too advanced for now
        mov bx, [OperandValue]
        sub bx, [CurrentAddress]
        sub bx, 2
        movsx cx, bl
        cmp cx, bx
        jne .NotShort
        push bx
        call OutputByte
        pop bx
        mov al, bl
        call OutputByte
        clc
        ret
.NotShort:
        stc
        ret

; Output instruction byte in AL, ORed with 1 if OperandValue
; is a 16-bit register (R_AX .. R_DI)
OutInst816:
        ; 16-bit?
        mov bl, [OperandValue]
        shr bl, 3
        cmp bl, 1
        jne .Not16
        or al, 1
.Not16:
        jmp OutputByte

; Outputs MODR/M byte(s) with register in AL
; and memory operand assumed in OperandL*
OutputModRM:
        shl al, 3
        or al, [OperandLType]
        call OutputByte
        mov al, [OperandLType]
        cmp al, 6
        je .Disp16
        and al, 0xc0
        cmp al, 0x80
        je .Disp16
        cmp al, 0x40
        je .Disp8
        ret
.Disp8:
        mov al, [OperandLValue]
        jmp OutputByte
.Disp16:
        cmp word [CurrentLFixup], INVALID_ADDR
        je .Out
        call SwapFixup
        call RegisterFixup
        call SwapFixup
.Out:
        mov ax, [OperandLValue]
        jmp OutputWord

; Output reg, reg instruction with instruction byte in AL
; The low bit of the instruction is set if OperandValue
; signifies a 16-bit (not sreg) register
OutputRR:
        call OutInst816
        mov al, 0xc0
        mov ah, [OperandLValue]
        and ah, 7
        or al, ah
        mov ah, [OperandValue]
        and ah, 7
        shl ah, 3
        or al, ah
        jmp OutputByte

; Output mem, reg otherwise see notes for OutputRR
OutputMR:
        ; TODO: Check if ExplicitSize (it's either redundant or wrong)
        call OutInst816
        mov al, [OperandValue]
        and al, 7
        jmp OutputModRM

; Output reg, mem otherwise see notes for OutputMR
OutputRM:
        push ax
        call SwapOperands
        pop ax
        jmp OutputMR

; Output mem, imm with base instruction in AH, /r in AL
OutputMImm:
        mov bl, [ExplicitSize]
        dec bl
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        push bx
        push ax
        or bl, ah
        mov al, bl
        call OutputByte ; Opcode
        pop ax
        call OutputModRM
        pop bx
        mov al, bl
        jmp OutputImm

InstMOV:
        call Get2Operands
        cmp byte [OperandLType], OP_REG
        je .MOVr
        jb .MOVm
        jmp InvalidOperand
.MOVr:
        cmp byte [OperandType], OP_REG
        je .MOVrr
        jb .MOVrm
        ; MOV reg, imm
        mov al, 0xb0
        add al, [OperandLValue]
        call OutputByte
        mov al, [OperandLValue]
        shr al, 3
        jmp OutputImm
.MOVrr:
        ; MOV reg, reg
        cmp byte [OperandLValue], R_ES
        jae .MOVs
        ; LHS isn't s-reg
        cmp byte [OperandValue], R_ES
        jae .MOVrs
        mov al, 0x88
        jmp OutputRR
.MOVrs:
        mov al, 0x8c
        jmp OutputRR
.MOVs:
        mov al, [OperandValue]
        sub al, R_AX
        cmp al, 7
        ja InvalidOperand
        call SwapOperands
        mov al, 0x8e
        jmp OutputRR
.MOVrm:
        mov al,0x8A
        jmp OutputRM
.MOVm:
        cmp byte [OperandType], OP_REG
        je .MOVmr
        jb InvalidOperand
        mov ax, 0xc600
        jmp OutputMImm
.MOVmr:
        mov al, 0x88
        jmp OutputMR

InstXCHG:
        call Get2Operands
        cmp byte [OperandLType], OP_REG
        je .XCHGr
        ja InvalidOperand
        cmp byte [OperandType], OP_REG
        jne InvalidOperand
.XCHGmr:
        mov al, 0x86
        call OutInst816
        mov al, [OperandValue]
        and al, 7
        jmp OutputModRM
.XCHGr:
        cmp byte [OperandType], OP_REG
        je .XCHGrr
        ja InvalidOperand
        call SwapOperands
        jmp .XCHGmr
.XCHGrr:
        mov al, 0x86
        jmp OutputRR

; AL=second opcode byte
InstMOVXX:
        push ax
        call Get2Operands
        pop ax
        cmp byte [OperandLType], OP_REG
        jne NotImplemented
        mov ah, al
        mov al, 0x0f
        call OutputWord
        cmp byte [OperandType], OP_REG
        je .RR
        call SwapOperands
        mov al, [OperandValue]
        and al, 7
        jmp OutputModRM
.RR:
        mov al, [OperandValue]
        and al, 7
        mov ah, [OperandLValue]
        and ah, 7
        shl ah, 3
        or al, ah
        or al, 0xc0
        jmp OutputByte


; AL=0 if INC, AL=1 if DEC
InstIncDec:
        push ax
        call GetOperand
        pop ax
        cmp byte [OperandType], OP_REG
        je .Reg
        ja InvalidOperand
        mov ah, [ExplicitSize]
        dec ah
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        or ah, 0xfe
        push ax
        mov al, ah
        call OutputByte
        call SwapOperands
        pop ax
        jmp OutputModRM
.Reg:
        shl al, 3
        mov ah, [OperandValue]
        mov bl, ah
        and ah, 7
        or al, ah
        or al, 0x40 ; AL = 0x40|dec<<3|(OperandValue&7)
        shr bl, 3
        jnz OutputByte
        or al, 0xc0 ; Could just be |0x80 since 0x40 is already or'ed in
        mov ah, al
        mov al, 0xfe
        jmp OutputWord

; Base instruction in AL (e.g. 0x38 for CMP)
InstALU:
        push ax
        call Get2Operands
        pop ax
        cmp byte [OperandLType], OP_REG
        je .ALUr
        ja InvalidOperand
        cmp byte [OperandType], OP_REG
        je OutputMR
        jb InvalidOperand
        shr al, 3
        mov ah, 0x80
        jmp OutputMImm
.ALUr:
        cmp byte [OperandType], OP_REG
        je OutputRR
        ja .ALUrl
        add al, 2
        jmp OutputRM
.ALUrl:
        cmp byte [OperandLValue], R_AL
        jne .ALUrl2
        add al, 4
        call OutputByte
        jmp OutputImm8
.ALUrl2:
        cmp byte [OperandLValue], R_AX
        jne .ALUrl3
        add al, 5
        call OutputByte
        jmp OutputImm16
.ALUrl3:
        mov ah, al
        or ah, 0xc0
        mov al, [OperandLValue]
        mov bl, al
        shr al, 3
        push ax
        or al, 0x80
        and bl, 7
        or ah, bl
        call OutputWord
        pop ax
        jmp OutputImm

; /r in AL (e.g. 6 for DIV)
InstMulDiv:
        push ax
        call GetOperand
        pop ax
        cmp byte [OperandType], OP_REG
        jne InvalidOperand
        ; Output 0xF6 | is16bit, 0xC0 | r<<3 | (OperandValue&7)
        mov ah, al
        shl ah, 3
        or ah, 0xC0
        mov al, [OperandValue]
        cmp al, R_ES
        jae InvalidOperand
        mov bl, al
        and bl, 7
        or ah, bl
        shr al, 3
        or al, 0xF6
        jmp OutputWord

; /r in AL (e.g. 4 for SHL)
InstROT:
        push ax
        call Get2Operands
        pop bx
        cmp byte [OperandLType], OP_REG
        jne NotImplemented
        cmp byte [OperandType], OP_LIT
        jne NotImplemented
        mov ah, [OperandLValue]
        ; Output 0xc0 | is16bit, 0xC0 | r<<3 | (OperandLValue&7)
        mov al, ah
        and ah, 7
        shl bl, 3
        or ah, bl
        or ah, 0xc0
        shr al, 3
        and al, 1
        or al, 0xc0
        call OutputWord
        jmp OutputImm8

InstINT:
        mov al, 0xcd
        call OutputByte
        call GetNumber
        jmp OutputByte

InstCALL:
        call GetOperand
        cmp byte [OperandType], OP_REG
        je .CallR
        mov al, 0xE8
        call OutputByte
        jmp HandleRel16
.CallR:
        mov al, 0xFF
        call OutputByte
        mov al, [OperandValue]
        and al, 7
        or al, 0xD0 ; 0xC0 | (2<<3)
        jmp OutputByte

InstPUSH:
        call GetOperand
        cmp byte [OperandType], OP_REG
        je .PushR
        jb InvalidOperand
        cmp word [CurrentFixup], INVALID_ADDR
        jne .PushImm16
        mov ax, [OperandValue]
        movsx bx, al
        cmp ax, bx
        jne .PushImm16
        mov al, 0x6A
        call OutputByte
        jmp OutputImm8
.PushImm16:
        mov al, 0x68
        call OutputByte
        jmp OutputImm16
.PushR:
        mov al, [OperandValue]
        sub al, R_AX
        cmp al, 8
        jae .PushS
        or al, 0x50
        jmp OutputByte
.PushS:
        sub al, 8
        shl al, 3
        or al, 0x06
        jmp OutputByte


InstPOP:
        call GetOperand
        cmp byte [OperandType], OP_REG
        jne InvalidOperand
        mov al, [OperandValue]
        sub al, R_AX
        js InvalidOperand
        cmp al, 8
        jae .PopS
        or al, 0x58
        jmp OutputByte
.PopS:
        and al, 7
        shl al, 3
        or al, 0x07
        jmp OutputByte

InstJMP:
        call GetOperand
        mov al, 0xEB
        call HandleShortRel
        jc .NotShort
        ret
.NotShort:
        mov al, 0xE9
        call OutputByte
        jmp HandleRel16

; AL contains the condition code
InstJCC:
        push si
        mov si, ax
        call GetOperand
        mov ax, 0x70
        or ax, si
        call HandleShortRel
        jnc .Done
        shl si, 8
        mov ax, 0x800F
        or ax, si
        call OutputWord
        call HandleRel16
.Done:
        pop si
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FileName:         db 'in.asm', 0
OutFileName:      db 'a.com', 0

MsgCurToken:      db 'Current token: "', 0
MsgErrInLine:     db 'Error in line ', 0
MsgErrNotImpl:    db 'Not implemented', 0
MsgErrNotDone:    db 'File not completely parsed', 0
MsgErrUnknInst:   db 'Unknown directive or instruction', 0
MsgErrOpenIn:     db 'Error opening input file', 0
MsgErrOutput:     db 'Error during output', 0
MsgErrMem:        db 'Alloc failed', 0
MsgErrInvalidNum: db 'Error invalid number', 0
MsgErrInvalidOpe: db 'Invalid operand', 0
MsgErrLabelMax:   db 'Too many labels', 0
MsgErrFixupMax:   db 'Too many fixups', 0
MsgErrDupLabel:   db 'Duplicate label', 0
MsgErrFixupUnh:   db 'Unhandled fixup', 0
MsgErrChLitErr:   db 'Invalid character literal', 0
MsgErrUndefLab:   db 'Undefined label', 0
MsgErrNoSize:     db 'Size missing for memory operand', 0
MsgErrDupEqu:     db 'Duplicate EQU', 0
MsgErrEquMax:     db 'Too many EQUs', 0
MsgErrOutMax:     db 'Output buffer full', 0
MsgErrInternalErr: db 'No fixup to register?', 0
MsgErrExpected:   db '? expected',0 ; NOTE! modified by Expect


RegNames:
    dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH'
    dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'
    dw 'ES', 'CS', 'SS', 'DS'

; Each entry has "DISPATCH_SIZE" and starts with a NUL-padded string
; of size INST_MAX followed by a byte that's passed in AL (AX) to the
; function in the final word.
DispatchList:
    ; Directives
    db 'DB',0,0,0, 0x01
    dw DirDX
    db 'DW',0,0,0, 0x02
    dw DirDX
    db 'ORG',0,0,  0x00
    dw DirORG

    ; MOV/XCHG
    db 'MOV',0,0,  0x00
    dw InstMOV
    db 'XCHG',0,   0x00
    dw InstXCHG

    ; MOVSX/MOVZX
    db 'MOVZX',    0xB6
    dw InstMOVXX
    db 'MOVSX',    0xBE
    dw InstMOVXX

    ; INC/DEC
    db 'INC',0,0,  0x00
    dw InstIncDec
    db 'DEC',0,0,  0x01
    dw InstIncDec

    ; ALU instructions (argument is base instruction)
    db 'ADD',0,0,  0x00
    dw InstALU
    db 'OR',0,0,0, 0x08
    dw InstALU
    db 'ADC',0,0,  0x10
    dw InstALU
    db 'SBB',0,0,  0x18
    dw InstALU
    db 'AND',0,0,  0x20
    dw InstALU
    db 'SUB',0,0,  0x28
    dw InstALU
    db 'XOR',0,0,  0x30
    dw InstALU
    db 'CMP',0,0,  0x38
    dw InstALU

    ; Misc
    db 'NOP',0,0,  0x90
    dw OutputByte
    db 'REP',0,0,  0xF3
    dw OutputByte
    db 'CLC',0,0,  0xF8
    dw OutputByte
    db 'STC',0,0,  0xF9
    dw OutputByte

    ; Mul/Div instructions (argument is /r)
    db 'MUL',0,0,  0x04
    dw InstMulDiv
    db 'IMUL',0,   0x05
    dw InstMulDiv
    db 'DIV',0,0,  0x06
    dw InstMulDiv
    db 'IDIV',0,   0x07
    dw InstMulDiv

    ; Rotate instructions (argument is /r)
    db 'ROL',0,0,  0x00
    dw InstROT
    db 'ROR',0,0,  0x01
    dw InstROT
    db 'RCL',0,0,  0x02
    dw InstROT
    db 'RCR',0,0,  0x03
    dw InstROT
    db 'SHL',0,0,  0x04
    dw InstROT
    db 'SHR',0,0,  0x05
    dw InstROT
    db 'SAR',0,0,  0x07
    dw InstROT

    ; Stack instructions
    db 'POP',0,0,  0x00
    dw InstPOP
    db 'POPA',0,   0x61
    dw OutputByte
    db 'PUSH',0,   0x00
    dw InstPUSH
    db 'PUSHA',    0x60
    dw OutputByte

    ; String instructions
    db 'MOVSB',    0xA4
    dw OutputByte
    db 'MOVSW',    0xA5
    dw OutputByte
    db 'STOSB',    0xAA
    dw OutputByte
    db 'LODSB',    0xAC
    dw OutputByte

    ; Flow control
    db 'RET',0,0,  0xC3
    dw OutputByte
    db 'INT',0,0,  0x00
    dw InstINT
    db 'CALL',0,   0x00
    dw InstCALL

    ; JMP
    db 'JMP',0,0,  0x00
    dw InstJMP

    ; Conditional jump instructions (argument is condition code)
    db 'JO',0,0,0, CC_O
    dw InstJCC
    db 'JNO',0,0,  CC_NO
    dw InstJCC
    db 'JC',0,0,0, CC_C
    dw InstJCC
    db 'JB',0,0,0, CC_C
    dw InstJCC
    db 'JNC',0,0,  CC_NC
    dw InstJCC
    db 'JNB',0,0,  CC_NC
    dw InstJCC
    db 'JAE',0,0,  CC_NC
    dw InstJCC
    db 'JZ',0,0,0, CC_Z
    dw InstJCC
    db 'JE',0,0,0, CC_Z
    dw InstJCC
    db 'JNZ',0,0,  CC_NZ
    dw InstJCC
    db 'JNE',0,0,  CC_NZ
    dw InstJCC
    db 'JNA',0,0,  CC_NA
    dw InstJCC
    db 'JBE',0,0,  CC_NA
    dw InstJCC
    db 'JA',0,0,0, CC_A
    dw InstJCC
    db 'JS',0,0,0, CC_S
    dw InstJCC
    db 'JNS',0,0,  CC_NS
    dw InstJCC
    db 'JPE',0,0,  CC_PE
    dw InstJCC
    db 'JPO',0,0,  CC_PO
    dw InstJCC
    db 'JL',0,0,0, CC_L
    dw InstJCC
    db 'JNL',0,0,  CC_NL
    dw InstJCC
    db 'JNG',0,0,  CC_NG
    dw InstJCC
    db 'JG',0,0,0, CC_G
    dw InstJCC

DispatchListEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; TODO: To save a few bytes the following should just be 'resb'/'resw'

FirstFreeSeg:     dw 0

;;; Parser
InputFile:        dw 0 ; Input file handle
CurrentLine:      dw 1 ; Current line being processed
NumNewLines:      dw 0 ; Number of newlines passed by ReadNext
CurrentChar:      db 0 ; Current input character (0 on EOF)
TokenLen:         db 0
Token:            db 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
TokenEnd:         db 0 ; NUL-terminator

; The last parsed operand is in OperandType/OperandValue
; Instructions with 2 operands have the left most operand saved in
; OperandLType/OperandLValue. CurrentFixup/CurrentLFixup holds
; any fixups needed for the operands (or INVALID_ADDR)
; See the EQUs at the top of the file for the meaning
OperandType:      db 0
OperandValue:     dw 0
CurrentFixup:     dw 0
OperandLType:     db 0
OperandLValue:    dw 0
CurrentLFixup:    dw 0

ExplicitSize:     db 0 ; Explicit size of memory operand (0=none, 1=byte, 2=word)

LabelSeg:         dw 0
NumLabels:        dw 0

FixupSeg:         dw 0
NextFreeFixup:    dw 0 ; Points to next free fixup node (or INVALID_ADDR)

EquSeg:           dw 0
NumEqus:          dw 0

;;; Output
OutputSeg:        dw 0
CurrentAddress:   dw 0 ; Current memory address of code (e.g. 0x100 first in a COM file)
NumOutBytes:      dw 0 ; Number of bytes output

;;; Keep last
ProgramEnd:
