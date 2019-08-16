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
        mov si, [CodeOff]
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
        ret
.Next:
        or byte [Prefixes], ah
        inc word [CodeOff]
        jmp GetPrefixes

; Get instruction byte to al (and increment CodeOff)
GetIByte:
        push si
        mov si, [CodeOff]
        mov al, [es:si]
        inc word [CodeOff]
        pop si
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
        mov [RMText], ax
        mov byte [RMText+2], '$'
        ret
.NotReg:
        mov bh, bl
        and bh, 0xC7
        cmp bh, 6
        je .RMNotImpl ; raw disp16
        mov dx, .XXX
        jmp Fatal
        .XXX: db 'XXX$'
.RMNotImpl:
        mov al, [ModRM]
        call PutHexByte
        mov dx, .MsgX
        jmp Fatal
.MsgX: db ' <- handle this ModRM byte$'

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

RegNames:
    dw 'AL', 'CL', 'DL', 'BL', 'AH', 'CH', 'DH', 'BH'
    dw 'AX', 'CX', 'DX', 'BX', 'SP', 'BP', 'SI', 'DI'
    dw 'ES', 'CS', 'SS', 'DS'

MainTab:
        ; 00
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 10
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 20
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw N_SUB            , OTYPE_RM16  | OTYPE_R16   << 8
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 30
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 40
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 50
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 60
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 70
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 80
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; 90
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; A0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; B0
        dw N_MOV            , OTYPE_AL    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_CL    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_DL    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_BL    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_AH    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_CH    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_DH    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_BH    | OTYPE_IMM8  << 8
        dw N_MOV            , OTYPE_AX    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_CX    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_DX    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_BX    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_SP    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_BP    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_SI    | OTYPE_IMM16 << 8
        dw N_MOV            , OTYPE_DI    | OTYPE_IMM16 << 8
        ; C0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; D0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; E0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        ; F0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0
        dw                0 , 0

N_REP:    db 'REP$'
N_REPZ:   db 'REPZ$'
N_REPNZ:  db 'REPNZ$'

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
