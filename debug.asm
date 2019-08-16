;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEBUG - Bare bones debugger for real mode x86 ;;
;;                                               ;;
;; Copyright 2019 Michael Rasmussen              ;;
;; See LICENSE.md for details                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; While disassembling ES holds the code segment
;;

        org 0x100
        cpu 8086

PREFIX_LOCK equ 0x01
PREFIX_F2   equ 0x02 ; REPNE/REPNZ
PREFIX_F3   equ 0x04 ; REPE/REPZ
PREFIX_ES   equ 0x08
PREFIX_CS   equ 0x10
PREFIX_SS   equ 0x20
PREFIX_DS   equ 0x40

OTYPE_AL    equ 0x00
OTYPE_CL    equ 0x01
OTYPE_DL    equ 0x02
OTYPE_BL    equ 0x03
OTYPE_AH    equ 0x04
OTYPE_CH    equ 0x05
OTYPE_DH    equ 0x06
OTYPE_BH    equ 0x07
OTYPE_AX    equ 0x08
OTYPE_CX    equ 0x09
OTYPE_DX    equ 0x0A
OTYPE_BX    equ 0x0B
OTYPE_SP    equ 0x0C
OTYPE_BP    equ 0x0D
OTYPE_SI    equ 0x0E
OTYPE_DI    equ 0x0F
OTYPE_ES    equ 0x10
OTYPE_CS    equ 0x11
OTYPE_SS    equ 0x12
OTYPE_DS    equ 0x13
OTYPE_1     equ 0x14 ; Constant 1
OTYPE_IMM8  equ 0x20
OTYPE_IMM16 equ 0x21
OTYPE_REL8  equ 0x22
OTYPE_REL16 equ 0x23
OTYPE_MOFF  equ 0x24
OTYPE_RM8   equ 0x40 ; r/m part of ModRM
OTYPE_RM16  equ 0x41
OTYPE_R8    equ 0x42 ; /r part of ModRM
OTYPE_R16   equ 0x43
OTYPE_SREG  equ 0x44
OTYPE_RTAB  equ 0x45 ; /r selects opcode from table
OTYPE_NONE  equ 0xFF

OTYPE_MASK_IMM   equ 0x20
OTYPE_MASK_MODRM equ 0x40

Start:
        mov di, BssStart
        mov cx, ProgramEnd
        sub cx, di
        xor ax, ax
        rep stosb

        mov [CodeSeg], cs
        mov word [CodeOff], 0x100

        mov es, [CodeSeg]
.D:
        mov ax, [CodeOff]
        cmp ax, ProgramEnd
        jae .Done

        mov dx, es
        call PutHexDword
        call PutSpace
        call Disasm
        jmp .D
.Done:
        ret

PutChar:
        push ax
        push dx
        mov dl, al
        mov ah, 2
        int 0x21
        pop dx
        pop ax
        ret

PutSpace:
        push ax
        mov al, ' '
        call PutChar
        pop ax
        ret

PutCrLf:
        push ax
        mov al, 13
        call PutChar
        mov al, 10
        call PutChar
        pop ax
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
        shr al, 1
        shr al, 1
        shr al, 1
        shr al, 1
        call PutHexDigit
        pop ax
PutHexDigit:
        push ax
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe .Pr
        add al, 7
.Pr:
        call PutChar
        pop ax
        ret

; Print '$'-terminated string in DS:DX
PutString:
        push ax
        mov ah, 9
        int 0x21
        pop ax
        ret

Fatal:
        push cs
        pop ds
        call PutString
        mov ax, 0x4CFF
        int 0x21

Disasm:
        mov ax, [CodeOff]
        push ax
        call GetInstruction
        pop si ; SI = Instruction start
        mov cx, [CodeOff]
        sub cx, si
        push cx
.PrintHex:
        mov al, [es:si]
        call PutHexByte
        inc si
        dec cx
        jnz .PrintHex
        pop ax
        mov cx, 9 ; InstMaxLen
        sub cx, ax
        jbe .PadDone
.Pad:
        call PutSpace
        call PutSpace
        dec cx
        jnz .Pad
.PadDone:
        mov dx, [InstInfo]
        ; TODO: Handle instruction name missing...
        push dx
        call PutRemPrefixes
        pop dx
        call PutString
        mov al, [InstInfo+2]
        cmp al, OTYPE_NONE
        je .Done
        call PutSpace
        call PutOp
        mov al, [InstInfo+3]
        cmp al, OTYPE_NONE
        je .Done
        push ax
        mov al, ','
        call PutChar
        call PutSpace
        pop ax
        call PutOp
.Done:
        call PutCrLf
        ret

GetPrefixes:
        mov byte [Prefixes], 0
        mov si, [CodeOff]
.GPs:
        mov al, [es:si]
        mov ah, PREFIX_LOCK
        cmp al, 0xF0
        je .Next
        mov ah, PREFIX_F2
        cmp al, 0xF2
        je .Next
        mov ah, PREFIX_F3
        cmp al, 0xF3
        je .Next
        mov ah, PREFIX_ES
        cmp al, 0x26
        je .Next
        mov ah, PREFIX_CS
        cmp al, 0x2E
        je .Next
        mov ah, PREFIX_ES
        cmp al, 0x36
        je .Next
        mov ah, PREFIX_CS
        cmp al, 0x3E
        je .Next
        mov [CodeOff], si
        ret
.Next:
        or byte [Prefixes], ah
        inc si
        jmp .GPs

; Get instruction byte to AL (and increment CodeOff)
GetIByte:
        push si
        mov si, [CodeOff]
        mov al, [es:si]
        inc word [CodeOff]
        pop si
        ret

; Get word to AX (using GetIByte)
GetIWord:
        call GetIByte
        mov ah, al
        call GetIByte
        xchg al, ah
        ret

GetInstruction:
        ;
        ; Handle prefixes
        ;
        call GetPrefixes

        ;
        ; Handle Instruction
        ;
        call GetIByte
        mov [InstBytes], al
        mov si, MainTab
        cmp al, 0x0F
        jne .OneByte
        jmp .NotImpl
.OneByte:
        xor bx, bx
        mov bl, al
        shl bx, 1
        shl bx, 1
        mov ax, [si+bx+2] ; AX = args
        mov bx, [si+bx]
        mov [InstInfo], bx
        mov [InstInfo+2], ax
        and bx, bx
        jnz .Known
        jmp .NotImpl
.Known:
        ;
        ; Handle ModRM
        ;
        mov byte [HasModRM], 0
        cmp al, OTYPE_NONE
        je .ModRMHandled
        test al, OTYPE_MASK_MODRM
        jnz .HasModRM
        cmp ah, OTYPE_NONE
        je .ModRMHandled
        test ah, OTYPE_MASK_MODRM
        jz .ModRMHandled
.HasModRM:
        inc byte [HasModRM]
        push ax
        call GetIByte
        mov [ModRM], al
        pop ax
.ModRMHandled:
        cmp al, OTYPE_RTAB
        jne .NotRTab
        mov dx, .XXX2
        jmp Fatal
.XXX2: db 'TODO: Handle OTYPE_RTAB$'
.NotRTab:

        push ax
        call GetRM
        pop ax

        ;
        ; Handle immediate
        ;
        cmp al, OTYPE_NONE
        je .ImmHandled
        mov cl, al
        test cl, OTYPE_MASK_IMM
        jnz .HandleImm
        cmp ah, OTYPE_NONE
        je .ImmHandled
        mov cl, ah
        test cl, OTYPE_MASK_IMM
        jnz .HandleImm
        jmp .ImmHandled
.HandleImm:
        push ax
        call GetIByte
        cbw
        mov [Immediate], ax
        test cl, 1
        jz .Not16
        call GetIByte
        mov [Immediate+1], al
.Not16:
        pop ax
.ImmHandled:
        ret

.NotImpl:
        mov al, [InstBytes]
        call PutHexByte
        mov dx, .MsgErrNI
        jmp Fatal
        ret
.MsgErrNI:   db ' Not implemented in GetInstruction$'

; AL/AH are operands
GetRM:
        cmp al, OTYPE_MOFF
        je .Moff
        cmp ah, OTYPE_MOFF
        je .Moff
        cmp byte [HasModRM], 0
        jne .ModRM
        ret
.Moff:
        mov dx, .MsgMoff
        jmp Fatal
.MsgMoff: db 'OTYPE_MOFF not implemented in GetRM$'
.ModRM:
        mov di, RMText
        mov bl, [ModRM]
        mov ch, bl
        mov cl, 6
        shr ch, cl
        cmp ch, 3
        jne .NotReg
        xor bh, bh
        and bl, 7   ; BL = ModRM&7
        ; 8-bit or 16-bit register? (Figure out from operand type)
        mov cl, 8
        cmp al, OTYPE_RM16
        je .GotReg
        cmp ah, OTYPE_RM16
        je .GotReg
        mov cl, 0
.GotReg:
        add bl, cl
        add bx, bx
        mov ax, [RegNames+bx]
        mov [di], ax
        mov byte [di+2], '$'
        ret
.NotReg:
        mov byte [es:di], '['
        inc di
        ; TODO: Segment override
        mov bh, bl
        and bh, 0xC7
        cmp bh, 6
        jne .NotRawDisp16
        call GetIWord
        call .CvtRMWord
        mov byte [es:di], ']'
        mov byte [es:di+1], '$'
        ret
.NotRawDisp16:
        mov al, [ModRM]
        call PutHexByte
        mov dx, .MsgX
        jmp Fatal
.MsgX: db ' <- handle this ModRM byte$'
.CvtRMWord:
        mov word [es:di], '0x'
        add di, 2
        mov cl, 4
        mov ch, 4
.Cvt:
        rol ax, cl
        push ax
        and al, 0x0f
        add al, '0'
        cmp al, '9'
        jbe .CvtStore
        add al, 7
.CvtStore:
        mov [es:di], al
        inc di
        pop ax
        dec ch
        jnz .Cvt
        ret

Put2Chars:
        call PutChar
        mov al, ah
        jmp PutChar

PutImm:
        push ax
        mov ax, '0x'
        call Put2Chars
        pop ax
        jmp PutHexWord

; Print operand in AL
PutOp:
        cmp al, OTYPE_1
        jb .Reg
        ja .NotReg
        mov al, '1'
        jmp PutChar
.Reg:
        xor bh, bh
        mov bl, al
        add bx, bx
        mov ax, [RegNames+bx]
        jmp Put2Chars
.NotReg:
        ; TODO: Check OTYPE_SREG and OTYPE_R* here and jump to .Reg
        mov bx, [Immediate]
        cmp al, OTYPE_IMM8
        je .Imm
        cmp al, OTYPE_IMM16
        je .Imm
        cmp al, OTYPE_REL8
        je .Rel
        cmp al, OTYPE_REL16
        je .Rel
        jmp .RM
.Rel:
        add bx, [CodeOff]
.Imm:
        mov ax, bx
        jmp PutImm
.RM:
        cmp al, OTYPE_RM8
        je .M
        cmp al, OTYPE_RM16
        je .M
        cmp al, OTYPE_MOFF
        je .M
        ; R8/R16/SREG
        xor bh, bh
        mov bl, [ModRM]
        shr bl, 1
        shr bl, 1
        shr bl, 1
        and bl, 7
        add bl, bl
        cmp al, OTYPE_R8
        je .PrintReg
        add bl, 16
        cmp al, OTYPE_R16
        je .PrintReg
        add bl, 16
        cmp al, OTYPE_SREG
        je .PrintReg
        call PutHexByte
        mov dx, .MsgErrNI
        jmp Fatal
.MsgErrNI:   db ' Not implemented in PutOp$'
.PrintReg:
        mov ax, [RegNames+bx]
        jmp Put2Chars
.M:
        mov dx, RMText
        jmp PutString

PutRemPrefixes:
        mov al, [Prefixes]
        and al, al
        jz .Done
.Handle:
        test al, PREFIX_LOCK
        jz .P1
        mov dx, N_LOCK
        call .PutWSpace
.P1:
        test al, PREFIX_F2
        jz .P2
        mov dx, N_REPNZ
        call .PutWSpace
.P2:
        test al, PREFIX_F3
        jz .P3
        mov dx, N_REPZ
        mov bl, [InstBytes]
        cmp bl, 0xA6 ; CMPSB
        je .PutRepz
        cmp bl, 0xA7 ; CMPSW
        je .PutRepz
        cmp bl, 0xAE ; SCASB
        je .PutRepz
        cmp bl, 0xAF ; SCASW
        je .PutRepz
        mov dx, N_REP
.PutRepz:
        call .PutWSpace
.P3:
        and al, ~(PREFIX_LOCK|PREFIX_F2|PREFIX_F3)
        jz .Done
        call PutHexByte
        mov dx, .MsgTODO
        jmp Fatal
.Done:
        ret
.MsgTODO: db ' <- Handle this Preifx in PutRemPrefixes$'
.PutWSpace:
        call PutString
        jmp PutSpace


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MainTab:
    ; 0x00
    dw N_ADD    , OTYPE_RM8    | OTYPE_R8    << 8 ; x0
    dw N_ADD    , OTYPE_RM16   | OTYPE_R16   << 8 ; x1
    dw N_ADD    , OTYPE_R8     | OTYPE_RM8   << 8 ; x2
    dw N_ADD    , OTYPE_R16    | OTYPE_RM16  << 8 ; x3
    dw N_ADD    , OTYPE_AL     | OTYPE_IMM8  << 8 ; x4
    dw N_ADD    , OTYPE_AX     | OTYPE_IMM16 << 8 ; x5
    dw N_PUSH   , OTYPE_ES     | OTYPE_NONE  << 8 ; x6
    dw N_POP    , OTYPE_ES     | OTYPE_NONE  << 8 ; x7
    dw N_OR     , OTYPE_RM8    | OTYPE_R8    << 8 ; x8
    dw N_OR     , OTYPE_RM16   | OTYPE_R16   << 8 ; x9
    dw N_OR     , OTYPE_R8     | OTYPE_RM8   << 8 ; xA
    dw N_OR     , OTYPE_R16    | OTYPE_RM16  << 8 ; xB
    dw N_OR     , OTYPE_AL     | OTYPE_IMM8  << 8 ; xC
    dw N_OR     , OTYPE_AX     | OTYPE_IMM16 << 8 ; xD
    dw N_PUSH   , OTYPE_CS     | OTYPE_NONE  << 8 ; xE
    dw 0        , 0 ; 0x0F -> Two byte instruction  xF
    ; 0x10
    dw N_ADC    , OTYPE_RM8    | OTYPE_R8    << 8 ; x0
    dw N_ADC    , OTYPE_RM16   | OTYPE_R16   << 8 ; x1
    dw N_ADC    , OTYPE_R8     | OTYPE_RM8   << 8 ; x2
    dw N_ADC    , OTYPE_R16    | OTYPE_RM16  << 8 ; x3
    dw N_ADC    , OTYPE_AL     | OTYPE_IMM8  << 8 ; x4
    dw N_ADC    , OTYPE_AX     | OTYPE_IMM16 << 8 ; x5
    dw N_PUSH   , OTYPE_SS     | OTYPE_NONE  << 8 ; x6
    dw N_POP    , OTYPE_SS     | OTYPE_NONE  << 8 ; x7
    dw N_SBB    , OTYPE_RM8    | OTYPE_R8    << 8 ; x8
    dw N_SBB    , OTYPE_RM16   | OTYPE_R16   << 8 ; x9
    dw N_SBB    , OTYPE_R8     | OTYPE_RM8   << 8 ; xA
    dw N_SBB    , OTYPE_R16    | OTYPE_RM16  << 8 ; xB
    dw N_SBB    , OTYPE_AL     | OTYPE_IMM8  << 8 ; xC
    dw N_SBB    , OTYPE_AX     | OTYPE_IMM16 << 8 ; xD
    dw N_PUSH   , OTYPE_DS     | OTYPE_NONE  << 8 ; xE
    dw N_POP    , OTYPE_DS     | OTYPE_NONE  << 8 ; xF
    ; 0x20
    dw N_AND    , OTYPE_RM8    | OTYPE_R8    << 8 ; x0
    dw N_AND    , OTYPE_RM16   | OTYPE_R16   << 8 ; x1
    dw N_AND    , OTYPE_R8     | OTYPE_RM8   << 8 ; x2
    dw N_AND    , OTYPE_R16    | OTYPE_RM16  << 8 ; x3
    dw N_AND    , OTYPE_AL     | OTYPE_IMM8  << 8 ; x4
    dw N_AND    , OTYPE_AX     | OTYPE_IMM16 << 8 ; x5
    dw 0        , 0 ; PREFIX_ES                     x6
    dw N_DAA    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x7
    dw N_SUB    , OTYPE_RM8    | OTYPE_R8    << 8 ; x8
    dw N_SUB    , OTYPE_RM16   | OTYPE_R16   << 8 ; x9
    dw N_SUB    , OTYPE_R8     | OTYPE_RM8   << 8 ; xA
    dw N_SUB    , OTYPE_R16    | OTYPE_RM16  << 8 ; xB
    dw N_SUB    , OTYPE_AL     | OTYPE_IMM8  << 8 ; xC
    dw N_SUB    , OTYPE_AX     | OTYPE_IMM16 << 8 ; xD
    dw 0        , 0 ; PREFIX_CS                   ; xE
    dw N_DAS    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0x30
    dw N_XOR    , OTYPE_RM8    | OTYPE_R8    << 8 ; x0
    dw N_XOR    , OTYPE_RM16   | OTYPE_R16   << 8 ; x1
    dw N_XOR    , OTYPE_R8     | OTYPE_RM8   << 8 ; x2
    dw N_XOR    , OTYPE_R16    | OTYPE_RM16  << 8 ; x3
    dw N_XOR    , OTYPE_AL     | OTYPE_IMM8  << 8 ; x4
    dw N_XOR    , OTYPE_AX     | OTYPE_IMM16 << 8 ; x5
    dw 0        , 0 ; PREFIX_SS                   ; x6
    dw N_AAA    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x7
    dw N_CMP    , OTYPE_RM8    | OTYPE_R8    << 8 ; x8
    dw N_CMP    , OTYPE_RM16   | OTYPE_R16   << 8 ; x9
    dw N_CMP    , OTYPE_R8     | OTYPE_RM8   << 8 ; xA
    dw N_CMP    , OTYPE_R16    | OTYPE_RM16  << 8 ; xB
    dw N_CMP    , OTYPE_AL     | OTYPE_IMM8  << 8 ; xC
    dw N_CMP    , OTYPE_AX     | OTYPE_IMM16 << 8 ; xD
    dw 0        , 0 ; PREFIX_DS                   ; xE
    dw N_AAS    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0x40
    dw N_INC    , OTYPE_AX     | OTYPE_NONE  << 8 ; x0
    dw N_INC    , OTYPE_CX     | OTYPE_NONE  << 8 ; x1
    dw N_INC    , OTYPE_DX     | OTYPE_NONE  << 8 ; x2
    dw N_INC    , OTYPE_BX     | OTYPE_NONE  << 8 ; x3
    dw N_INC    , OTYPE_SP     | OTYPE_NONE  << 8 ; x4
    dw N_INC    , OTYPE_BP     | OTYPE_NONE  << 8 ; x5
    dw N_INC    , OTYPE_SI     | OTYPE_NONE  << 8 ; x6
    dw N_INC    , OTYPE_DI     | OTYPE_NONE  << 8 ; x7
    dw N_DEC    , OTYPE_AX     | OTYPE_NONE  << 8 ; x8
    dw N_DEC    , OTYPE_CX     | OTYPE_NONE  << 8 ; x9
    dw N_DEC    , OTYPE_DX     | OTYPE_NONE  << 8 ; xA
    dw N_DEC    , OTYPE_BX     | OTYPE_NONE  << 8 ; xB
    dw N_DEC    , OTYPE_SP     | OTYPE_NONE  << 8 ; xC
    dw N_DEC    , OTYPE_BP     | OTYPE_NONE  << 8 ; xD
    dw N_DEC    , OTYPE_SI     | OTYPE_NONE  << 8 ; xE
    dw N_DEC    , OTYPE_DI     | OTYPE_NONE  << 8 ; xF
    ; 0x50
    dw N_PUSH   , OTYPE_AX     | OTYPE_NONE  << 8 ; x0
    dw N_PUSH   , OTYPE_CX     | OTYPE_NONE  << 8 ; x1
    dw N_PUSH   , OTYPE_DX     | OTYPE_NONE  << 8 ; x2
    dw N_PUSH   , OTYPE_BX     | OTYPE_NONE  << 8 ; x3
    dw N_PUSH   , OTYPE_SP     | OTYPE_NONE  << 8 ; x4
    dw N_PUSH   , OTYPE_BP     | OTYPE_NONE  << 8 ; x5
    dw N_PUSH   , OTYPE_SI     | OTYPE_NONE  << 8 ; x6
    dw N_PUSH   , OTYPE_DI     | OTYPE_NONE  << 8 ; x7
    dw N_POP    , OTYPE_AX     | OTYPE_NONE  << 8 ; x8
    dw N_POP    , OTYPE_CX     | OTYPE_NONE  << 8 ; x9
    dw N_POP    , OTYPE_DX     | OTYPE_NONE  << 8 ; xA
    dw N_POP    , OTYPE_BX     | OTYPE_NONE  << 8 ; xB
    dw N_POP    , OTYPE_SP     | OTYPE_NONE  << 8 ; xC
    dw N_POP    , OTYPE_BP     | OTYPE_NONE  << 8 ; xD
    dw N_POP    , OTYPE_SI     | OTYPE_NONE  << 8 ; xE
    dw N_POP    , OTYPE_DI     | OTYPE_NONE  << 8 ; xF
    ; 0x60
    dw N_PUSHA  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x0
    dw N_POPA   , OTYPE_NONE   | OTYPE_NONE  << 8 ; x1
    dw 0        , 0                               ; x2
    dw 0        , 0                               ; x3
    dw 0        , 0                               ; x4
    dw 0        , 0                               ; x5
    dw 0        , 0                               ; x6
    dw 0        , 0                               ; x7
    dw N_PUSH   , OTYPE_IMM16  | OTYPE_NONE  << 8 ; x8
    dw 0        , 0                               ; x9
    dw N_PUSH   , OTYPE_IMM8   | OTYPE_NONE  << 8 ; xA
    dw 0        , 0                               ; xB
    dw N_INSB   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xC
    dw N_INSW   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xD
    dw N_OUTSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xE
    dw N_OUTSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0x70
    dw N_JO     , OTYPE_REL8   | OTYPE_NONE  << 8 ; x0
    dw N_JNO    , OTYPE_REL8   | OTYPE_NONE  << 8 ; x1
    dw N_JC     , OTYPE_REL8   | OTYPE_NONE  << 8 ; x2
    dw N_JNC    , OTYPE_REL8   | OTYPE_NONE  << 8 ; x3
    dw N_JZ     , OTYPE_REL8   | OTYPE_NONE  << 8 ; x4
    dw N_JNZ    , OTYPE_REL8   | OTYPE_NONE  << 8 ; x5
    dw N_JNA    , OTYPE_REL8   | OTYPE_NONE  << 8 ; x6
    dw N_JA     , OTYPE_REL8   | OTYPE_NONE  << 8 ; x7
    dw N_JS     , OTYPE_REL8   | OTYPE_NONE  << 8 ; x8
    dw N_JNS    , OTYPE_REL8   | OTYPE_NONE  << 8 ; x9
    dw N_JPE    , OTYPE_REL8   | OTYPE_NONE  << 8 ; xA
    dw N_JPO    , OTYPE_REL8   | OTYPE_NONE  << 8 ; xB
    dw N_JL     , OTYPE_REL8   | OTYPE_NONE  << 8 ; xC
    dw N_JNL    , OTYPE_REL8   | OTYPE_NONE  << 8 ; xD
    dw N_JNG    , OTYPE_REL8   | OTYPE_NONE  << 8 ; xE
    dw N_JG     , OTYPE_REL8   | OTYPE_NONE  << 8 ; xF
    ; 0x80
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x0
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x1
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x2
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x3
    dw N_TEST   , OTYPE_RM8    | OTYPE_R8    << 8 ; x4
    dw N_TEST   , OTYPE_RM16   | OTYPE_R16   << 8 ; x5
    dw N_XCHG   , OTYPE_R8     | OTYPE_RM8   << 8 ; x6
    dw N_XCHG   , OTYPE_R16    | OTYPE_RM16  << 8 ; x7
    dw N_MOV    , OTYPE_RM8    | OTYPE_R8    << 8 ; x8
    dw N_MOV    , OTYPE_RM16   | OTYPE_R16   << 8 ; x9
    dw N_MOV    , OTYPE_R8     | OTYPE_RM8   << 8 ; xA
    dw N_MOV    , OTYPE_R16    | OTYPE_RM16  << 8 ; xB
    dw N_MOV    , OTYPE_RM16   | OTYPE_SREG  << 8 ; xC
    dw N_LEA    , OTYPE_R16    | OTYPE_RM16  << 8 ; xD
    dw N_MOV    , OTYPE_SREG   | OTYPE_RM16  << 8 ; xE
    dw 0        , 0                               ; xF
    ; 0x90
    dw N_NOP    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x0
    dw N_XCHG   , OTYPE_CX     | OTYPE_AX    << 8 ; x1
    dw N_XCHG   , OTYPE_DX     | OTYPE_AX    << 8 ; x2
    dw N_XCHG   , OTYPE_BX     | OTYPE_AX    << 8 ; x3
    dw N_XCHG   , OTYPE_SP     | OTYPE_AX    << 8 ; x4
    dw N_XCHG   , OTYPE_BP     | OTYPE_AX    << 8 ; x5
    dw N_XCHG   , OTYPE_SI     | OTYPE_AX    << 8 ; x6
    dw N_XCHG   , OTYPE_DI     | OTYPE_AX    << 8 ; x7
    dw N_CBW    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x8
    dw N_CWD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x9
    dw 0        , 0                               ; xA
    dw 0        , 0                               ; xB
    dw N_PUSHF  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xC
    dw N_POPF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xD
    dw N_SAHF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xE
    dw N_LAHF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0xA0
    dw N_MOV    , OTYPE_AL     | OTYPE_MOFF  << 8 ; x0
    dw N_MOV    , OTYPE_AX     | OTYPE_MOFF  << 8 ; x1
    dw N_MOV    , OTYPE_MOFF   | OTYPE_AL    << 8 ; x2
    dw N_MOV    , OTYPE_MOFF   | OTYPE_AX    << 8 ; x3
    dw N_MOVSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x4
    dw N_MOVSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x5
    dw N_CMPSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x6
    dw N_CMPSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x7
    dw 0        , 0                               ; x8
    dw 0        , 0                               ; x9
    dw N_STOSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xA
    dw N_STOSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xB
    dw N_LODSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xC
    dw N_LODSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xD
    dw N_SCASB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xE
    dw N_SCASW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0xB0
    dw N_MOV    , OTYPE_AL     | OTYPE_IMM8  << 8 ; x0
    dw N_MOV    , OTYPE_CL     | OTYPE_IMM8  << 8 ; x1
    dw N_MOV    , OTYPE_DL     | OTYPE_IMM8  << 8 ; x2
    dw N_MOV    , OTYPE_BL     | OTYPE_IMM8  << 8 ; x3
    dw N_MOV    , OTYPE_AH     | OTYPE_IMM8  << 8 ; x4
    dw N_MOV    , OTYPE_CH     | OTYPE_IMM8  << 8 ; x5
    dw N_MOV    , OTYPE_DH     | OTYPE_IMM8  << 8 ; x6
    dw N_MOV    , OTYPE_BH     | OTYPE_IMM8  << 8 ; x7
    dw N_MOV    , OTYPE_AX     | OTYPE_IMM16 << 8 ; x8
    dw N_MOV    , OTYPE_CX     | OTYPE_IMM16 << 8 ; x9
    dw N_MOV    , OTYPE_DX     | OTYPE_IMM16 << 8 ; xA
    dw N_MOV    , OTYPE_BX     | OTYPE_IMM16 << 8 ; xB
    dw N_MOV    , OTYPE_SP     | OTYPE_IMM16 << 8 ; xC
    dw N_MOV    , OTYPE_BP     | OTYPE_IMM16 << 8 ; xD
    dw N_MOV    , OTYPE_SI     | OTYPE_IMM16 << 8 ; xE
    dw N_MOV    , OTYPE_DI     | OTYPE_IMM16 << 8 ; xF
    ; 0xC0
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x0
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x1
    dw N_RET    , OTYPE_IMM16  | OTYPE_NONE  << 8 ; x2
    dw N_RET    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x3
    dw N_LES    , OTYPE_R16    | OTYPE_RM16  << 8 ; x4
    dw N_LDS    , OTYPE_R16    | OTYPE_RM16  << 8 ; x5
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x6
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x7
    dw N_RETF   , OTYPE_IMM16  | OTYPE_NONE  << 8 ; xA
    dw N_RETF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xB
    dw N_INT3   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xC
    dw N_INT    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; xD
    dw N_INTO   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xE
    dw N_IRET   , OTYPE_NONE   | OTYPE_NONE  << 8 ; xF
    ; 0xD0
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x0
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x1
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x2
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x3
    dw N_AAM    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; x4
    dw N_AAD    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; x5
    dw 0        , 0                               ; x6
    dw N_XLATB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; x7
    dw 0        , 0                               ; x8
    dw 0        , 0                               ; x9
    dw 0        , 0                               ; xA
    dw 0        , 0                               ; xB
    dw 0        , 0                               ; xC
    dw 0        , 0                               ; xD
    dw 0        , 0                               ; xE
    dw 0        , 0                               ; xF
    ; 0xE0
    dw N_LOOPNE , OTYPE_REL8   | OTYPE_NONE  << 8 ; x0
    dw N_LOOPE  , OTYPE_REL8   | OTYPE_NONE  << 8 ; x1
    dw N_LOOP   , OTYPE_REL8   | OTYPE_NONE  << 8 ; x2
    dw N_JCXZ   , OTYPE_REL8   | OTYPE_NONE  << 8 ; x3
    dw N_IN     , OTYPE_AL     | OTYPE_IMM8  << 8 ; x4
    dw N_IN     , OTYPE_AX     | OTYPE_IMM8  << 8 ; x5
    dw N_OUT    , OTYPE_IMM8   | OTYPE_AL    << 8 ; x6
    dw N_OUT    , OTYPE_IMM8   | OTYPE_AX    << 8 ; x7
    dw N_CALL   , OTYPE_REL16  | OTYPE_NONE  << 8 ; x8
    dw N_JMP    , OTYPE_REL16  | OTYPE_NONE  << 8 ; x9
    dw 0        , 0                               ; xA
    dw N_JMP    , OTYPE_REL8   | OTYPE_NONE  << 8 ; xB
    dw N_IN     , OTYPE_AL     | OTYPE_DX    << 8 ; xC
    dw N_IN     , OTYPE_AX     | OTYPE_DX    << 8 ; xD
    dw N_OUT    , OTYPE_DX     | OTYPE_AL    << 8 ; xE
    dw N_OUT    , OTYPE_DX     | OTYPE_AX    << 8 ; xF
    ; 0xF0
    dw 0        , 0 ; LOCK                        ; x0
    dw 0        , 0                               ; x1
    dw 0        , 0 ; REPNE/REP                   ; x2
    dw 0        , 0 ; REPE                        ; x3
    dw N_HLT    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x4
    dw N_CMC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x5
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x6
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; x7
    dw N_CLC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x8
    dw N_STC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; x9
    dw N_CLI    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xA
    dw N_STI    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xB
    dw N_CLD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xC
    dw N_STD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; xD
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; xE
    dw 0,0;dw ??, OTYPE_RTAB   | OTYPE_NONE  << 8 ; xF

N_LOCK:   db 'LOCK$'
N_REP:    db 'REP$'
N_REPZ:   db 'REPZ$'
N_REPNZ:  db 'REPNZ$'
R_ES:     db 'ES$'
R_CS:     db 'CS$'
R_SS:     db 'CS$'
R_DS:     db 'DS$'

N_AAA:    db 'AAA$'
N_AAM:    db 'AAM$'
N_AAS:    db 'AAS$'
N_AAD:    db 'AAD$'
N_DAA:    db 'DAA$'
N_DAS:    db 'DAS$'
N_JCXZ:   db 'JCXZ$'
N_LOOP:   db 'LOOP$'
N_LOOPE:  db 'LOOPE$'
N_LOOPNE: db 'LOOPNE$'
N_SAHF:   db 'SAHF$'
N_LAHF:   db 'LAHF$'
N_XLATB:  db 'XLATB$'
N_IN:     db 'IN$'
N_OUT:    db 'OUT$'
N_INSB:   db 'INSB$'
N_INSW:   db 'INSW$'
N_OUTSB:  db 'OUTSB$'
N_OUTSW:  db 'OUTSW$'
N_ADC:    db 'ADC$'
N_ADD:    db 'ADD$'
N_AND:    db 'AND$'
N_CALL:   db 'CALL$'
N_CALLF:  db 'CALLF$'
N_CBW:    db 'CBW$'
N_CLC:    db 'CLC$'
N_CLD:    db 'CLD$'
N_CLI:    db 'CLI$'
N_CMC:    db 'CMC$'
N_CMP:    db 'CMP$'
N_CMPSB:  db 'CMPSB$'
N_CMPSW:  db 'CMPSW$'
N_CWD:    db 'CWD$'
N_DEC:    db 'DEC$'
N_DIV:    db 'DIV$'
N_HLT:    db 'HLT$'
N_IDIV:   db 'IDIV$'
N_IMUL:   db 'IMUL$'
N_INC:    db 'INC$'
N_INT:    db 'INT$'
N_INT3:   db 'INT3$'
N_INTO:   db 'INTO$'
N_IRET:   db 'IRET$'
N_LDS:    db 'LDS$'
N_LEA:    db 'LEA$'
N_LES:    db 'LES$'
N_LODSB:  db 'LODSB$'
N_LODSW:  db 'LODSW$'
N_MOV:    db 'MOV$'
N_MOVSB:  db 'MOVSB$'
N_MOVSW:  db 'MOVSW$'
N_MOVSX:  db 'MOVSX$'
N_MOVZX:  db 'MOVZX$'
N_MUL:    db 'MUL$'
N_NEG:    db 'NEG$'
N_NOP:    db 'NOP$'
N_NOT:    db 'NOT$'
N_OR:     db 'OR$'
N_POP:    db 'POP$'
N_POPA:   db 'POPA$'
N_POPF:   db 'POPF$'
N_PUSH:   db 'PUSH$'
N_PUSHA:  db 'PUSHA$'
N_PUSHF:  db 'PUSHF$'
N_RCL:    db 'RCL$'
N_RCR:    db 'RCR$'
N_RET:    db 'RET$'
N_RETF:   db 'RETF$'
N_ROL:    db 'ROL$'
N_ROR:    db 'ROR$'
N_SAR:    db 'SAR$'
N_SBB:    db 'SBB$'
N_SCASB:  db 'SCASB$'
N_SCASW:  db 'SCASW$'
N_SHL:    db 'SHL$'
N_SHR:    db 'SHR$'
N_STC:    db 'STC$'
N_STD:    db 'STD$'
N_STI:    db 'STI$'
N_STOSB:  db 'STOSB$'
N_STOSW:  db 'STOSW$'
N_SUB:    db 'SUB$'
N_TEST:   db 'TEST$'
N_XCHG:   db 'XCHG$'
N_XOR:    db 'XOR$'

N_JMP:    db 'JMP$'
N_JMPF:   db 'JMPF$'
N_JO:     db 'JO$'
N_JNO:    db 'JNO$'
N_JC:     db 'JC$'
N_JNC:    db 'JNC$'
N_JZ:     db 'JZ$'
N_JNZ:    db 'JNZ$'
N_JNA:    db 'JNA$'
N_JA:     db 'JA$'
N_JS:     db 'JS$'
N_JNS:    db 'JNS$'
N_JPE:    db 'JPE$'
N_JPO:    db 'JPO$'
N_JL:     db 'JL$'
N_JNL:    db 'JNL$'
N_JNG:    db 'JNG$'
N_JG:     db 'JG$'

RegNames:
    dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH'
    dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'
    dw 'ES', 'CS', 'SS', 'DS'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BssStart:

CodeSeg:         resw 1
CodeOff:         resw 1
Prefixes:        resb 1
InstInfo:        resw 2
InstBytes:       resb 2
HasModRM:        resb 1
ModRM:           resb 1
Immediate:       resw 1
RMText:          resb 30

ProgramEnd: ; Keep last
