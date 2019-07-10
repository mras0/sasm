;; Calling convention (unless otherwise mentioned):
;;
;;   Callee-saved: DS, BP, SI, DI
;;   Scratch: AX, BX, CX, DX, ES
;;   Return value: (DX:)AX or BX for pointer values,
;;                 boolean values via the carry flag
;;

TOKEN_MAX        equ 16         ; Maximum length of token (adjust token buffer if increasing)
INST_MAX         equ 5          ; Maximum length of directive/instruction
LABEL_MAX        equ 100        ; Maximum number of labels
DISPATCH_SIZE    equ 8          ; Size of DispatchListEntry
LABEL_SIZE       equ 22         ; Size of Label (TOKEN_MAX+2+2*sizeof(WORD))
LABEL_ADDR       equ 18         ; Offset of label address
LABEL_FIXUP      equ 20         ; Offset of label fixup

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

        mov ax, LABEL_MAX
        mov bx, LABEL_SIZE
        call Malloc
        mov [LabelSeg], ax

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

MsgCurToken: db 'Current token: "', 0
MsgErrInLine: db 'Error in line ', 0
MsgErrNotImpl: db 'Not implemented', 0
MsgErrNotDone: db 'File not completely parsed', 0
MsgErrUnknInst: db 'Unknown directive or instruction', 0

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
        ; TODO: Check if label

        mov si, DispatchList

        ; Is the token too short/long to be a valid instruction/directive?
        mov al, [TokenLen]
        cmp al, 2
        jb .CheckEQU
        cmp al, INST_MAX
        ja .CheckEQU

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
        jmp .Done

.Next:
        add si, DISPATCH_SIZE
        cmp si, DispatchListEnd
        jb .Compare

.CheckEQU:
        ; TODO: Check if EQU

        mov bx, MsgErrUnknInst
        call Error
.Done:
        pop si
        ret

; Allocate AX*BX bytes, returns segment in AX
Malloc:
        ; Calculate how many paragraphs are needed
        mul bx
        and dx, dx
        jnz .Err ; Overflow
        add ax, 15
        shr ax, 4

        add ax, [FirstFreeSeg]
        cmp ax, [2] ; (PSP) Segment of the first byte beyond the memory allocated to the program
        jae .Err ; Out of memory

        mov [FirstFreeSeg], ax

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
        push di
        push es
        mov di, [OutputSeg]
        mov es, di
        mov di, [NumOutBytes]
        stosb
        inc word [NumOutBytes]
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
        ; TODO: Handle Fixup
        ; if (CurrentFixup) FixupIsHere()
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
        mov bx, .Msg
        mov [bx], al
        jmp Error

.Msg: db '? expected',0

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
        ; TODO: Check if EQU
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
        mov bx, .Msg
        jmp Error
        .Msg: db 'Not implemented: Decimal number',0
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

; Get operand to OperandType/OperandValue (and possibly CurrentFixup)
GetOperand:
        ; TODO: Character literal
        cmp byte [CurrentChar], QUOTE_CHAR
        je NotImplemented
        ; TODO: Memory operand
        cmp byte [CurrentChar], '['
        je NotImplemented

;GetRegOrNumber:
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
        ret
.NextReg:
        add bx, 2
        dec cl
        jnz .ChecReg
        ; Fall through
.CheckNumber:
        mov al, [Token]
        sub al, '0'
        cmp al, 9
        ja .NotNumber
        call GetTokenNumber
        mov [OperandValue], ax
        mov byte [OperandType], OP_LIT
        ret
.NotNumber:
        ; Check for word/byte [mem]
        cmp byte [TokenLen], 4
        jne .CheckNamedLit
        mov ax, [Token]
        mov bx, [Token+2]
        cmp ax, 'BY'
        jne .CheckWord
        cmp bx, 'TE'
        jne .CheckNamedLit
        ; TODO: mov byte [ExplicitSize], 1
        ;       jmp GetOperandMem
        jmp NotImplemented
.CheckWord:
        cmp ax, 'WO'
        jne .CheckNamedLit
        cmp ax, 'RD'
        jne .CheckNamedLit
        ; TODO: mov byte [ExplicitSize], 2
        ;       jmp GetOperandMem
        jmp NotImplemented
.CheckNamedLit:
        ; Otherwise do named literal
;GetNamedLiteral:
        mov byte [OperandType], OP_LIT
        call FindOrMakeLabel
        jmp NotImplemented


SwapOperands:
        mov al, [OperandType]
        xchg al, [OperandLType]
        mov [OperandType], al
        mov ax, [OperandValue]
        xchg ax, [OperandLValue]
        mov [OperandValue], ax
        ret

Get2Operands:
        call GetOperand
        mov al, ','
        call Expect
        call SwapOperands
        jmp GetOperand

; Find label matching TokenText, returns pointer in BX or
; INVALID_ADDR if not found
FindLabel:
        push si
        push di
        mov ax, [LabelSeg]
        mov es, ax
        xor bx, bx
        mov cx, LABEL_MAX
.Search:
        mov si, Token
        mov di, bx
.Compare:
        mov al, [si]
        cmp [di], al
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
        mov bx, INVALID_ADDR
        jmp .Done
.Done:
        pop di
        pop si
        ret

FindOrMakeLabel:
        call FindLabel
        cmp bx, INVALID_ADDR
        je .NotFound
        mov bx, .F
        jmp Error
.NotFound:
        mov bx, .N
        jmp Error
.F: db 'TODO: Label found', 0
.N: db 'TODO: Label NOT found', 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DirORG:
        call GetNumber
        mov [CurrentAddress], ax
        ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
InvalidOperand:
        mov bx, MsgErrInvalidOpe
        jmp Error

InstMOV:
        call Get2Operands
        cmp byte [OperandLType], OP_REG
        je .MOVr
        jl .MOVm
        jmp InvalidOperand
.MOVr:
        cmp byte [OperandType], OP_REG
        je .MOVrr
        jl .MOVrm
        ; MOV reg, imm
        mov al, 0xb0
        add al, [OperandLValue]
        call OutputByte
        mov al, [OperandLValue]
        shr al, 3
        jmp OutputImm
.MOVrr:
        ; MOV reg, reg
        mov bx, .Msgrr
        jmp Error
.MOVrm:
        ; MOV reg, mem
        mov bx, .Msgrm
        jmp Error
.MOVm:
        cmp byte [OperandType], OP_REG
        je .MOVmr
        jl InvalidOperand
        mov bx, .Msgml
        jmp Error
.MOVmr:
        mov bx, .Msgmr
        jmp Error

.Msgrr: db 'Not implemented: MOVrr', 0
.Msgrm: db 'Not implemented: MOVrm', 0
.Msgmr: db 'Not implemented: MOVml', 0
.Msgml: db 'Not implemented: MOVmr', 0

InstINT:
        mov al, 0xcd
        call OutputByte
        call GetNumber
        jmp OutputByte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

FileName:         db 't01.asm', 0
OutFileName:      db 'a.com', 0

MsgErrOpenIn:     db 'Error opening input file', 0
MsgErrOutput:     db 'Error during output', 0
MsgErrMem:        db 'Alloc failed', 0
MsgErrInvalidNum: db 'Error invalid number', 0
MsgErrInvalidOpe: db 'Invalid operand', 0

RegNames:
    dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH'
    dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'
    dw 'ES', 'CS', 'SS', 'DS'

DispatchList:
    db 'ORG', 0, 0, 0x00
    dw DirORG
    db 'MOV', 0, 0, 0x00
    dw InstMOV
    db 'INT', 0, 0, 0x00
    dw InstINT
DispatchListEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
; OperandLType/OperandLValue
; See the EQUs at the top of the file for the meaning
OperandType:      db 0
OperandValue:     dw 0
OperandLType:     db 0
OperandLValue:    dw 0

LabelSeg:         dw 0
NumLabels:        dw 0

;;; Output
OutputSeg:        dw 0
CurrentAddress:   dw 0 ; Current memory address of code (e.g. 0x100 first in a COM file)
NumOutBytes:      dw 0 ; Number of bytes output

;;; Keep last
ProgramEnd:
