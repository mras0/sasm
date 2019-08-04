;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SASM - Simple/Stupid/Self-hosting Assembler   ;;
;;        for 16-bit x86 DOS-like Systems        ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The primary purpose of this assembler is to be able
;; to assemble itself running only under software assembled
;; with itself (excluding the BIOS). DOS is of course the
;; obvious choice as a bootstrapping environment and .COM
;; files are easy to work with, so that's the basis. Due to
;; programmer lazyness the minimum supported processor is
;; currently the 80386 (mostly due the use of PUSHA/POPA and
;; shift/rotates with an immediate other than 1 and using
;; rel16 for forward jumps).
;;
;; While the accepted syntax should be a strict subset of
;; what NASM allows and subsequently outputs, there are
;; some important differences (list not exhaustive):
;;   * SASM is a one-pass assembler and only performs very
;;     basic optimizations.
;;   * Literal expressions are only supported in memory
;;     operands and even then are limited to addition.
;;     (Yes, this means you have to write [SI+0xFFFF] to
;;     subtract one from SI).
;;   * The only supported instructions are those that
;;     were at some point needed. This can include
;;     some operand types not being supported.
;;   * NASM warns about resb/resw in code sections and
;;     ouputs zeros while SASM doesn't output anything
;;     for trailing resb/resw's.
;;   * SASM currently isn't case-sensitive for labels/
;;     equates.
;;   * Error checking is limited, and SASM might emit
;;     garbage opcode bytes when encountering an
;;     illegal instruction (like ADD ES, 4). Check the
;;     code with the C version or even better with NASM
;;     from time to time.
;;
;; Development was generally done by first implementing
;; support for the new instruction/directive/etc. in the
;; accompanying C-version assembler (while being careful
;; to write in a fashion that would be implementable in
;; assembly), and then getting this version up to date.
;;
;; Starting out, it was hard to know whether everything
;; would fit in 64K, so I opted to play it safe and
;; use far pointers in most places I anticipated it could
;; become necessary. This helped stress test the assembler
;; as well as my sanity. That's why some (most) things are
;; dimensioned for sizes that haven't be necessary yet.
;;
;; Assemble using: nasm -f bin -o sasm.com sasm.asm
;; or sasm sasm.asm
;;
;; Calling convention (unless otherwise mentioned):
;;
;;   Callee-saved: DS, BP, SI, DI
;;   Scratch: AX, BX, CX, DX, ES
;;   Return value: (DX:)AX or BX for pointer values,
;;                 boolean values via the carry flag
;;

STACK_SIZE       equ 512
TOKEN_MAX        equ 16         ; Maximum length of token (adjust token buffer if increasing)
INST_MAX         equ 6          ; Maximum length of directive/instruction (LOOPNE is longest)
BUFFER_SIZE      equ 512        ; Size of input buffer
OUTPUT_MAX       equ 0x2000     ; Maximum output size
LABEL_MAX        equ 200        ; Maximum number of labels
FIXUP_MAX        equ 400        ; Maximum number of fixups
EQU_MAX          equ 100        ; Maximum number of equates
DISPATCH_SIZE    equ 9          ; Size of DispatchListEntry (INST_MAX + 3)
LABEL_SIZE       equ 22         ; Size of Label (TOKEN_MAX+2+2*sizeof(WORD))
LABEL_ADDR       equ 18         ; Offset of label address
LABEL_FIXUP      equ 20         ; Offset of label fixup
FIXUP_ADDR       equ 0          ; Offset of fixup address (into the output buffer)
FIXUP_LINK       equ 2          ; Offset of fixup link pointer (INVALID_ADDR terminates list)
FIXUP_TYPE       equ 4          ; Offset of fixup type (FIXUP_DISP16 or FIXUP_REL8)
FIXUP_SIZE       equ 5          ; Size of Fixup
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

; Fixup types
FIXUP_DISP16     equ 0
FIXUP_REL8       equ 1

        cpu 386
        org 0x100

ProgramEntry:
        ; Clear BSS
        mov di, BSS
        mov cx, ProgramEnd
        sub cx, di
        xor al, al
        rep stosb

        ; First free paragraph is at the end of the program
        ; COM files get the largest available block
        mov ax, ProgramEnd
        add ax, 15
        and ax, 0xfff0
        add ax, STACK_SIZE
        cli
        mov sp, ax
        sti
        shr ax, 4
        mov bx, cs
        add ax, bx
        mov [FirstFreeSeg], ax

        call Init
        call MainLoop
        call Fini

        xor al, al
        jmp Exit

ParseCommandLine:
        mov cl, [0x80]
        inc cl ; Let count include CR
        mov si, 0x81
        mov di, InFileName
        call CopyFileName

        mov di, OutFileName
        call CopyFileName

        cmp byte [InFileName], 0
        jne .HasIn
        mov word [InFileName], 'A.'
        mov word [InFileName+2], 'AS'
        mov word [InFileName+4], 'M'
.HasIn:
        cmp byte [OutFileName], 0
        jne .Ret
        mov di, OutFileName
        mov si, InFileName
.Copy:
        lodsb
        cmp al, '.'
        jbe .AppendExt
        stosb
        jmp .Copy
.AppendExt:
        mov ax, '.C'
        stosw
        mov ax, 'OM'
        stosw
        xor al, al
        stosb
.Ret:
        ret

CopyFileName:
        and cl, cl
        jz .Done
        cmp byte [si], 0x0D
        je .Done
        ; Skip spaces
.SkipS:
        cmp byte [si], ' '
        jne .NotSpace
        inc si
        dec cl
        jnz .SkipS
        jmp .Done
.NotSpace:
        mov ch, 12
.Copy:
        cmp byte [si], ' '
        jbe .Done
        movsb
        dec cl
        jz .Done
        dec ch
        jnz .Copy
.Done:
        xor al, al
        stosb
        ret

Init:
        call ParseCommandLine

        mov bx, MsgHello
        call PutString
        mov bx, InFileName
        call PutString
        mov bx, MsgHello2
        call PutString
        mov bx, OutFileName
        call PutString
        call PutCrLf

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

        mov byte [CpuLevel], 3

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
        push cx
        push di
        push es
        mov di, [OutputSeg]
        mov es, di
        mov di, [NumOutBytes]
        xor cx, cx
        xchg cx, [PendingZeros]
        and cx, cx
        jnz .Zeros
        cmp word [NumOutBytes], OUTPUT_MAX
        jb .Output
.Overflow:
        mov bx, MsgErrOutMax
        jmp Error
.Output:
        stosb
        inc word [NumOutBytes]
        inc word [CurrentAddress]
        pop es
        pop di
        pop cx
        ret
.Zeros:
        add [NumOutBytes], cx
        cmp word [NumOutBytes], OUTPUT_MAX
        jae .Overflow
        push ax
        xor al, al
        rep stosb
        pop ax
        jmp .Output

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
        mov al, FIXUP_DISP16
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
        mov word [CurrentLine], 1

        ; Open file for reading
        mov dx, InFileName
        mov ax, 0x3d00
        int 0x21
        jnc .OK
        mov bx, MsgErrOpenIn
        jmp Error
.OK:
        mov [InputFile], ax
        call FillInBuffer
        call MoveNext
        ret

ParserFini:
        ; Close input file
        mov ax, 0x3e00
        mov bx, [InputFile]
        int 0x21
        ret

FillInBuffer:
        mov ah, 0x3f
        mov bx, [InputFile]
        mov cx, BUFFER_SIZE
        mov dx, InputBuffer
        int 0x21
        jc .ReadError  ; error code in AX
        mov [InputBufferBytes], ax
        mov word [InputBufferPos], 0
        ret

.ReadError:
        mov bx, MsgErrRead
        jmp Error

ReadNext:
        mov bx, [InputBufferPos]
        cmp bx, [InputBufferBytes]
        jb .HasData
        call FillInBuffer
        xor bx, bx
        and ax, ax
        jz .EOF
.HasData:
        mov al, [InputBuffer+bx]
        inc bx
        mov [InputBufferPos], bx
.EOF:
        mov [CurrentChar], al
        cmp al, 10
        jne .Ret
        inc word [NumNewLines]
.Ret:
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
        or al, ' ' ; ToLower
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
        and al, 0xDF ; to upper case
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
        ; Note instruction order
        cmp byte [es:bx+FIXUP_TYPE], FIXUP_DISP16
        mov bx, [es:bx+FIXUP_LINK]
        mov es, di
        jne .FixupRel8
        add [es:si], ax
        jmp .ResolveFixup
.FixupRel8:
        push ax
        mov ax, [NumOutBytes]
        sub ax, si
        dec ax
        cmp ax, 0x7F
        jbe .RangeOK
        mov bx, MsgErrNotRel8
        jmp Error
.RangeOK:
        mov [es:si], al
        pop ax
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

; Register fixup of type AL for CurrentFixup at this exact output location
RegisterFixup:
        mov bx, [CurrentFixup]
        mov cx, INVALID_ADDR
        cmp bx, cx
        jne .OK
        mov bx, MsgErrInternalE
        jmp Error
.OK:
        mov [CurrentFixup], cx
        mov cx, [FixupSeg]
        mov es, cx
        mov cx, [NumOutBytes]
        mov [es:bx+FIXUP_ADDR], cx
        mov [es:bx+FIXUP_TYPE], al
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

DirCPU:
        call GetNumber
        xor dx, dx
        mov bx, 100
        div bx
        cmp dx, 86
        jne .Invalid
        cmp ax, 0
        je .Invalid
        cmp ax, 3
        jbe .OK
        cmp ax, 80
        jne .Invalid
        mov ax, 0
.OK:
        mov [CpuLevel], al
        ret
.Invalid:
        mov bx, MsgErrInvalidCPU
        jmp Error


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
        xor cx, cx
.CharLit:
        push cx
        mov al, QUOTE_CHAR
        call TryConsume
        pop cx
        jnc .LitDone
        inc cx
        mov al, [CurrentChar]
        cmp al, ' '
        jae .OK
        mov bx, MsgErrChLitErr
        jmp Error
.OK:
        push cx
        call OutputByte ; Always output bytes for character literals
        call ReadNext
        pop cx
        jmp .CharLit
.LitDone:
        ; Output zero byte if necessary to ensure dw alignment
        cmp si, 2
        jne .Next
        and cl, 1
        jz .Next
        xor al, al
        call OutputByte
        jmp .Next
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
        call OutputImm16
        ; Fall thorugh
.Next:
        mov al, ','
        call TryConsume
        jnc .Main
        pop si
        ret

; AL = 1 if RESB 2 if RESW
DirResX:
        push ax
        call GetNumber
        pop bx
        xor dx, dx
        mul bx
        and dx, dx
        jnz .Overflow
        add [CurrentAddress], ax
        jc .Overflow
        add [PendingZeros], ax
        ret
.Overflow:
        mov bx, MsgErrMemOvrflow
        jmp Error

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
        mov al, FIXUP_DISP16
        call RegisterFixup
.NoFixup:
        mov ax, [OperandValue]
        sub ax, [CurrentAddress]
        sub ax, 2
        jmp OutputWord

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
        mov al, FIXUP_DISP16
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
        cmp byte [OperandLValue], R_ES
        jae .MOVsrm
        mov al,0x8A
        jmp OutputRM
.MOVsrm:
        ; MOV sreg, r/m
        mov al, 0x8E
        jmp OutputRM
.MOVm:
        cmp byte [OperandType], OP_REG
        je .MOVmr
        jb InvalidOperand
        mov ax, 0xc600
        jmp OutputMImm
.MOVmr:
        cmp byte [OperandValue], R_ES
        jae .MOVmsr
        mov al, 0x88
        jmp OutputMR
.MOVmsr:
        mov al, 0x8C
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

InstTEST:
        call Get2Operands
        cmp byte [OperandType], OP_REG
        je .TESTr
        jb .TESTm
        cmp byte [OperandLType], OP_REG
        ja InvalidOperand
        je .TESTrl
        ; TEST mem, lit
        mov al, [ExplicitSize]
        dec al
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        push ax
        or al, 0xF6
        call OutputByte
        xor al, al
        call OutputModRM
        pop ax
        jmp OutputImm
.TESTrl:
        ; TEST reg, lit 0x
        mov al, [OperandLValue]
        cmp al, R_ES
        jae InvalidOperand
        mov ah, al
        shr al, 3
        push ax
        or al, 0xF6
        and ah, 7
        or ah, 0xC0
        call OutputWord
        pop ax
        jmp OutputImm
.TESTr:
        ; TEST xxx, reg
        cmp byte [OperandValue], R_ES
        jae InvalidOperand
        mov al, 0x84
        cmp byte [OperandLType], OP_REG
        ja InvalidOperand
        jb OutputMR
        jmp OutputRR
.TESTm:
        cmp byte [OperandLType], OP_REG
        jne InvalidOperand
        call SwapOperands
        jmp .TESTr

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

HandleLXXArgs:
        call Get2Operands
        cmp byte [OperandLType], OP_REG
        jne InvalidOperand
        cmp byte [OperandType], OP_REG
        jae InvalidOperand
        mov bl, [OperandLValue]
        shr bl, 3
        cmp bl, 1
        jne InvalidOperand
        ret

InstLEA:
        call HandleLXXArgs
        mov al, 0x8D
        jmp OutputRM

; AL=opcode byte
InstLXS:
        push ax
        call HandleLXXArgs
        pop ax
        ; Suppress OR 1 in OutInst816
        mov byte [ExplicitSize], 0
        and byte [OperandLValue], 7
        jmp OutputRM

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

; AL=/r (e.g. 3 for NEG)
InstNotNeg:
        push ax
        call GetOperand
        cmp byte [OperandType], OP_REG
        je .NNr
        ja InvalidOperand
        mov al, [ExplicitSize]
        dec al
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        or al, 0xF6
        call OutputByte
        call SwapOperands
        pop ax
        jmp OutputModRM
.NNr:
        mov al, [OperandValue]
        mov ah, al
        shr al, 3
        cmp al, 1
        ja InvalidOperand
        or al, 0xF6
        call OutputByte
        and ah, 7
        pop bx
        shl bl, 3
        or ah, bl
        or ah, 0xc0
        mov al, ah
        jmp OutputByte

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
        jb .M
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
.M:
        push ax
        mov al, [ExplicitSize]
        dec al
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        or al, 0xf6
        call OutputByte
        call SwapOperands
        pop ax
        jmp OutputModRM

; /r in AL (e.g. 4 for SHL)
InstROT:
        push ax
        call Get2Operands
        pop bx
        cmp byte [OperandLType], OP_REG
        je .ROTr
        ja InvalidOperand

        mov al, [ExplicitSize]
        dec al
        jns .HasSize
        mov bx, MsgErrNoSize
        jmp Error
.HasSize:
        cmp byte [OperandType], OP_REG
        je .ROTmr
        jb InvalidOperand
        ; ROT mem, imm
        ; Special handling for ROT r/m, 1
        cmp byte [OperandValue], 1
        jne .ROTmimm
        or al, 0xd0
        jmp .ROTmrFinal
.ROTmimm:
        or al, 0xc0
        call .ROTmrFinal
        jmp OutputImm8
.ROTmr:
        cmp byte [OperandValue], R_CL
        jne InvalidOperand
        or al, 0xd2
.ROTmrFinal:
        push bx
        call OutputByte
        pop ax
        jmp OutputModRM
.ROTr:
        ; AL = is16bit
        ; AH = 0xC0 | r<<3 | (OperandLValue&7)
        mov ah, [OperandLValue]
        mov al, ah
        and ah, 7
        shl bl, 3
        or ah, bl
        or ah, 0xc0
        shr al, 3
        and al, 1

        cmp byte [OperandType], OP_REG
        ja .ROTrl
        jb InvalidOperand
        cmp byte [OperandValue], R_CL
        jne InvalidOperand
        or al, 0xd2
        jmp OutputWord
.ROTrl:
        ; Special handling for ROT r/m, 1
        cmp byte [OperandValue], 1
        jne .ROTrimm
        or al, 0xd0
        jmp OutputWord
.ROTrimm:
        or al, 0xc0
        call OutputWord
        jmp OutputImm8

InstIN:
        call Get2Operands
        mov al, 0xE4
        ; Fall through

HandleInOut:
        cmp byte [OperandLType], OP_REG
        jne InvalidOperand
        mov bl, [OperandLValue]
        test bl, 7
        jnz InvalidOperand
        shr bl, 3
        cmp bl, 1
        ja InvalidOperand
        or al, bl
        cmp byte [OperandType], OP_REG
        jb InvalidOperand
        ja .INl
        cmp byte [OperandValue], R_DX
        jne InvalidOperand
        or al, 0x08
        jmp OutputByte
.INl:
        call OutputByte
        jmp OutputImm8

InstOUT:
        call Get2Operands
        call SwapOperands
        mov al, 0xE6
        jmp HandleInOut

InstAAX:
        call OutputByte
        mov al, 0x0A
        jmp OutputByte

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

; Output instruction byte in AL and Rel8 if OperandValue is a short
; (known) jump or forced by AH=1 or SHORT in the program text.
; Returns carry clear on success
HandleShortRel:
        push ax
        call GetRegOrNumber
        pop ax
        jnc .HasOperand
        cmp word [Token], 'SH'
        jne .NamedLit
        cmp word [Token+2], 'OR'
        jne .NamedLit
        cmp word [Token+4], 'T'
        jne .NamedLit
        push ax
        call GetOperand
        pop ax
        mov ah, 1
        jmp .HasOperand
.NamedLit:
        push ax
        call GetNamedLiteral
        pop ax
.HasOperand:
        cmp byte [OperandType], OP_LIT
        jne InvalidOperand
        cmp word [CurrentFixup], INVALID_ADDR
        jne .Fixup
        mov bx, [OperandValue]
        sub bx, [CurrentAddress]
        sub bx, 2
        movsx cx, bl
        cmp cx, bx
        jne .NotShort
        push bx
        call OutputByte
        pop ax
        call OutputByte
.RetNC:
        clc
        ret
.Fixup:
        and ah, ah
        jz .NotShort
        call OutputByte
        mov al, FIXUP_REL8
        call RegisterFixup
        xor al, al
        call OutputByte
        jmp .RetNC
.NotShort:
        and ah, ah
        jz .RetC
        mov bx, MsgErrNotRel8
        jmp Error
.RetC:
        stc
        ret

InstJMP:
        xor ah, ah
        mov al, 0xEB
        call HandleShortRel
        jc .NotShort
        ret
.NotShort:
        mov al, 0xE9
        call OutputByte
        jmp HandleRel16

; AL contains opcode
InstJ8:
        mov ah, 1 ; Force short jump
        jmp HandleShortRel

; AL contains the condition code
InstJCC:
        push ax
        xor ah, ah
        cmp byte [CpuLevel], 3
        jae .Has386
        inc ah ; Force Rel8
.Has386:
        or al, 0x70
        call HandleShortRel
        pop ax
        jc .Rel16
        ret
.Rel16:
        mov ah, al
        or ah, 0x80
        mov al, 0x0F
        call OutputWord
        jmp HandleRel16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MsgHello:         db 'SASM 1.0 Processing ', 0
MsgHello2:        db ' to ', 0
MsgCurToken:      db 'Current token: "', 0
MsgErrInLine:     db 'Error in line ', 0
MsgErrNotImpl:    db 'Not implemented', 0
MsgErrNotDone:    db 'File not completely parsed', 0
MsgErrUnknInst:   db 'Unknown directive or instruction', 0
MsgErrOpenIn:     db 'Error opening input file', 0
MsgErrOutput:     db 'Error during output', 0
MsgErrRead:       db 'Error reading from input file', 0
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
MsgErrInternalE:  db 'No fixup to register?', 0
MsgErrMemOvrflow: db 'Address exceeds segment', 0
MsgErrInvalidCPU: db 'Invalid/unsupported CPU', 0
MsgErrNotRel8:    db 'Address out of 8-bit range', 0
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
    db 'DB',0,0,0,0, 0x01
    dw DirDX
    db 'DW',0,0,0,0, 0x02
    dw DirDX
    db 'RESB',0,0,   0x01
    dw DirResX
    db 'RESW',0,0,   0x02
    dw DirResX
    db 'ORG',0,0,0,  0x00
    dw DirORG
    db 'CPU',0,0,0,  0x00
    dw DirCPU

    ; MOV/XCHG/TEST
    db 'MOV',0,0,0,  0x00
    dw InstMOV
    db 'XCHG',0,0,   0x00
    dw InstXCHG
    db 'TEST',0,0,   0x00
    dw InstTEST

    ; MOVSX/MOVZX/LEA/LES/LDS
    db 'MOVZX',0,    0xB6
    dw InstMOVXX
    db 'MOVSX',0,    0xBE
    dw InstMOVXX
    db 'LEA',0,0,0,  0x00
    dw InstLEA
    db 'LES',0,0,0,  0xC4
    dw InstLXS
    db 'LDS',0,0,0,  0xC5
    dw InstLXS

    ; INC/DEC
    db 'INC',0,0,0,  0x00
    dw InstIncDec
    db 'DEC',0,0,0,  0x01
    dw InstIncDec

    ; NOT/NEG (argument is /r)
    db 'NOT',0,0,0,  0x02
    dw InstNotNeg
    db 'NEG',0,0,0,  0x03
    dw InstNotNeg

    ; ALU instructions (argument is base instruction)
    db 'ADD',0,0,0,  0x00
    dw InstALU
    db 'OR',0,0,0,0, 0x08
    dw InstALU
    db 'ADC',0,0,0,  0x10
    dw InstALU
    db 'SBB',0,0,0,  0x18
    dw InstALU
    db 'AND',0,0,0,  0x20
    dw InstALU
    db 'SUB',0,0,0,  0x28
    dw InstALU
    db 'XOR',0,0,0,  0x30
    dw InstALU
    db 'CMP',0,0,0,  0x38
    dw InstALU

    ; BCD instructions
    db 'DAA',0,0,0,  0x27
    dw OutputByte
    db 'DAS',0,0,0,  0x2F
    dw OutputByte
    db 'AAA',0,0,0,  0x37
    dw OutputByte
    db 'AAS',0,0,0,  0x3F
    dw OutputByte
    db 'AAM',0,0,0,  0xD4
    dw InstAAX
    db 'AAD',0,0,0,  0xD5
    dw InstAAX

    ; Misc
    db 'NOP',0,0,0,  0x90
    dw OutputByte
    db 'CBW',0,0,0,  0x98
    dw OutputByte
    db 'CWD',0,0,0,  0x99
    dw OutputByte
    db 'PUSHF',0,    0x9C
    dw OutputByte
    db 'POPF',0,0,   0x9D
    dw OutputByte
    db 'SAHF',0,0,   0x9E
    dw OutputByte
    db 'LAHF',0,0,   0x9F
    dw OutputByte
    db 'XLATB',0,    0xD7
    dw OutputByte

    ; I/O
    db 'IN',0,0,0,0, 0x00
    dw InstIN
    db 'OUT',0,0,0, 0x00
    dw InstOUT

    ; Prefixes
    db 'REPNE',0,    0xF2
    dw OutputByte
    db 'REPE',0,0,   0xF3
    dw OutputByte
    db 'REP',0,0,0,  0xF3
    dw OutputByte

    ; Flags/etc.
    db 'HLT',0,0,0,  0xF4
    dw OutputByte
    db 'CMC',0,0,0,  0xF5
    dw OutputByte
    db 'CLC',0,0,0,  0xF8
    dw OutputByte
    db 'STC',0,0,0,  0xF9
    dw OutputByte
    db 'CLI',0,0,0,  0xFA
    dw OutputByte
    db 'STI',0,0,0,  0xFB
    dw OutputByte
    db 'CLD',0,0,0,  0xFC
    dw OutputByte
    db 'STD',0,0,0,  0xFD
    dw OutputByte

    ; Mul/Div instructions (argument is /r)
    db 'MUL',0,0,0,  0x04
    dw InstMulDiv
    db 'IMUL',0,0,   0x05
    dw InstMulDiv
    db 'DIV',0,0,0,  0x06
    dw InstMulDiv
    db 'IDIV',0,0,   0x07
    dw InstMulDiv

    ; Rotate instructions (argument is /r)
    db 'ROL',0,0,0,  0x00
    dw InstROT
    db 'ROR',0,0,0,  0x01
    dw InstROT
    db 'RCL',0,0,0,  0x02
    dw InstROT
    db 'RCR',0,0,0,  0x03
    dw InstROT
    db 'SHL',0,0,0,  0x04
    dw InstROT
    db 'SHR',0,0,0,  0x05
    dw InstROT
    db 'SAR',0,0,0,  0x07
    dw InstROT

    ; Stack instructions
    db 'POP',0,0,0,  0x00
    dw InstPOP
    db 'POPA',0,0,   0x61
    dw OutputByte
    db 'PUSH',0,0,   0x00
    dw InstPUSH
    db 'PUSHA',0,    0x60
    dw OutputByte

    ; String instructions
    db 'INSB',0,0,   0x6C
    dw OutputByte
    db 'INSW',0,0,   0x6D
    dw OutputByte
    db 'OUTSB',0,    0x6E
    dw OutputByte
    db 'OUTSW',0,    0x6F
    dw OutputByte
    db 'MOVSB',0,    0xA4
    dw OutputByte
    db 'MOVSW',0,    0xA5
    dw OutputByte
    db 'CMPSB',0,    0xA6
    dw OutputByte
    db 'CMPSW',0,    0xA7
    dw OutputByte
    db 'STOSB',0,    0xAA
    dw OutputByte
    db 'STOSW',0,    0xAB
    dw OutputByte
    db 'LODSB',0,    0xAC
    dw OutputByte
    db 'LODSW',0,    0xAD
    dw OutputByte
    db 'SCASB',0,    0xAE
    dw OutputByte
    db 'SCASW',0,    0xAF
    dw OutputByte

    ; Flow control
    db 'RET',0,0,0,  0xC3
    dw OutputByte
    db 'RETF',0,0,   0xCB
    dw OutputByte
    db 'IRET',0,0,   0xCF
    dw OutputByte
    db 'HLT',0,0,0,  0xF4
    dw OutputByte
    db 'INT',0,0,0,  0x00
    dw InstINT
    db 'INT3',0,0,   0xCC
    dw OutputByte
    db 'INTO',0,0,   0xCE
    dw OutputByte
    db 'CALL',0,0,   0x00
    dw InstCALL

    ; JMP
    db 'JMP',0,0,0,  0x00
    dw InstJMP

    ; JCXZ and looping instructions (argument is opcode)
    db 'LOOPNE',     0xE0
    dw InstJ8
    db 'LOOPE',0,    0xE1
    dw InstJ8
    db 'LOOP',0,0,   0xE2
    dw InstJ8
    db 'JCXZ',0,0,   0xE3
    dw InstJ8

    ; Conditional jump instructions (argument is condition code)
    db 'JO',0,0,0,0, CC_O
    dw InstJCC
    db 'JNO',0,0,0,  CC_NO
    dw InstJCC
    db 'JC',0,0,0,0, CC_C
    dw InstJCC
    db 'JB',0,0,0,0, CC_C
    dw InstJCC
    db 'JNC',0,0,0,  CC_NC
    dw InstJCC
    db 'JNB',0,0,0,  CC_NC
    dw InstJCC
    db 'JAE',0,0,0,  CC_NC
    dw InstJCC
    db 'JZ',0,0,0,0, CC_Z
    dw InstJCC
    db 'JE',0,0,0,0, CC_Z
    dw InstJCC
    db 'JNZ',0,0,0,  CC_NZ
    dw InstJCC
    db 'JNE',0,0,0,  CC_NZ
    dw InstJCC
    db 'JNA',0,0,0,  CC_NA
    dw InstJCC
    db 'JBE',0,0,0,  CC_NA
    dw InstJCC
    db 'JA',0,0,0,0, CC_A
    dw InstJCC
    db 'JS',0,0,0,0, CC_S
    dw InstJCC
    db 'JNS',0,0,0,  CC_NS
    dw InstJCC
    db 'JPE',0,0,0,  CC_PE
    dw InstJCC
    db 'JPO',0,0,0,  CC_PO
    dw InstJCC
    db 'JL',0,0,0,0, CC_L
    dw InstJCC
    db 'JNL',0,0,0,  CC_NL
    dw InstJCC
    db 'JNG',0,0,0,  CC_NG
    dw InstJCC
    db 'JG',0,0,0,0, CC_G
    dw InstJCC

DispatchListEnd:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BSS:

FirstFreeSeg:     resw 1

CpuLevel:         resb 1

;;; Parser
InputFile:        resw 1 ; Input file handle
CurrentLine:      resw 1 ; Current line being processed
NumNewLines:      resw 1 ; Number of newlines passed by ReadNext
CurrentChar:      resb 1 ; Current input character (0 on EOF)
TokenLen:         resb 1
Token:            resb TOKEN_MAX
TokenEnd:         resb 1 ; NUL-terminator
InputBufferPos:   resw 1
InputBufferBytes: resw 1
InputBuffer:      resb BUFFER_SIZE

; The last parsed operand is in OperandType/OperandValue
; Instructions with 2 operands have the left most operand saved in
; OperandLType/OperandLValue. CurrentFixup/CurrentLFixup holds
; any fixups needed for the operands (or INVALID_ADDR)
; See the EQUs at the top of the file for the meaning
OperandType:      resb 1
OperandValue:     resw 1
CurrentFixup:     resw 1
OperandLType:     resb 1
OperandLValue:    resw 1
CurrentLFixup:    resw 1

ExplicitSize:     resb 1 ; Explicit size of memory operand (0=none, 1=byte, 2=word)

LabelSeg:         resw 1
NumLabels:        resw 1

FixupSeg:         resw 1
NextFreeFixup:    resw 1 ; Points to next free fixup node (or INVALID_ADDR)

EquSeg:           resw 1
NumEqus:          resw 1

;;; Output
OutputSeg:        resw 1
CurrentAddress:   resw 1 ; Current memory address of code (e.g. 0x100 first in a COM file)
NumOutBytes:      resw 1 ; Number of bytes output
PendingZeros:     resw 1 ; Number of zeros to output before next actual byte

InFileName:       resb 13
OutFileName:      resb 13

;;; Keep last
ProgramEnd:
