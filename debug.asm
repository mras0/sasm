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

STACK_SIZE       equ 512
DEFAULT_DIS_SIZE equ 0x21 ; Slightly more than 32 bytes

PREFIX_LOCK      equ 0x01
PREFIX_F2        equ 0x02 ; REPNE/REPNZ
PREFIX_F3        equ 0x04 ; REPE/REPZ
PREFIX_ES        equ 0x08
PREFIX_CS        equ 0x10
PREFIX_SS        equ 0x20
PREFIX_DS        equ 0x40

OTYPE_AL         equ 0x00
OTYPE_CL         equ 0x01
OTYPE_DL         equ 0x02
OTYPE_BL         equ 0x03
OTYPE_AH         equ 0x04
OTYPE_CH         equ 0x05
OTYPE_DH         equ 0x06
OTYPE_BH         equ 0x07
OTYPE_AX         equ 0x08
OTYPE_CX         equ 0x09
OTYPE_DX         equ 0x0A
OTYPE_BX         equ 0x0B
OTYPE_SP         equ 0x0C
OTYPE_BP         equ 0x0D
OTYPE_SI         equ 0x0E
OTYPE_DI         equ 0x0F
OTYPE_ES         equ 0x10
OTYPE_CS         equ 0x11
OTYPE_SS         equ 0x12
OTYPE_DS         equ 0x13
OTYPE_1          equ 0x14 ; Constant 1
OTYPE_IMM8       equ 0x20
OTYPE_IMM16      equ 0x21
OTYPE_REL8       equ 0x22
OTYPE_REL16      equ 0x23
OTYPE_RM8        equ 0x40 ; r/m part of ModRM
OTYPE_RM16       equ 0x41
OTYPE_R8         equ 0x42 ; /r part of ModRM
OTYPE_R16        equ 0x43
OTYPE_SREG       equ 0x44
OTYPE_RTAB       equ 0x45 ; /r selects opcode from table
OTYPE_MOFF       equ 0x80
OTYPE_NONE       equ 0xFF

OTYPE_MASK_IMM   equ 0x20
OTYPE_MASK_MODRM equ 0x40

Start:
        ; Clear BSS
        mov di, BssStart
        mov cx, ProgramEnd
        sub cx, di
        xor ax, ax
        rep stosb

        cli
        mov bx, ProgramEnd
        add bx, 15
        and bx, 0xFFF0
        add bx, STACK_SIZE
        mov sp, bx
        mov [DbgSP], sp
        mov cl, 4
        shr bx, cl
        mov ax, ds
        add bx, ax
        sti
        ; Free remaining memory
        mov ah, 0x4a
        int 0x21

        ; Install single step handler
        mov ax, SingleStep
        mov dx, cs
        mov bl, 1
        call SetIntVec
        mov [OldInt1], ax
        mov [OldInt1+2], dx

        ; Install terminatation handler
        ; Called when the program exits
        ; Note: Must be installed before loading program
        mov ax, TerminateHandler
        mov dx, cs
        mov bl, 0x22
        call SetIntVec

        ; TODO: Pass command line to loaded program
        xor bx, bx
        mov bl, [0x80]
        and bl, bl
        jz .Usage
        mov si, 0x81
        mov byte [si+bx], 0 ; Change CR to NUL
.SkipSpace:
        lodsb
        and al, al
        jnz .CheckSpace
.Usage:
        mov dx, MsgErrUsage
        jmp Fatal
.CheckSpace:
        cmp al, ' '
        je .SkipSpace
        dec si
        mov dx, si
        mov bx, ParameterBlock
        mov ax, 0x4B01
        int 0x21
        jnc .LoadOK
        call PutHexWord
        mov dx, MsgErrLoad
        jmp Fatal
.LoadOK:
        ; Get current PSP address
        mov ah, 0x62
        int 0x21
        mov [CodeSeg], bx

        ; Initialize program registers
        mov ax, 0x100
        mov [DisOff], ax
        mov [Prog_SI], ax ; SI=0x0100
        mov [Prog_DX], bx ; DX=CS
        mov [Prog_IP], ax ; IP=0x0100
        mov [Prog_ES], bx ; ES=CS
        mov [Prog_CS], bx
        mov [Prog_SS], bx ; SS=CS
        mov [Prog_DS], bx ; DS=CS
        xor ax, ax
        mov [Prog_AX], ax ; AX=0x0000
        mov [Prog_BX], ax ; BX=0x0000 (TODO: DOS DEBUG.COM stores file size in BX:CX)
        mov word [Prog_CX], 0x00FF ; CX=0x00FF
        mov ax, 0xFFFE
        mov [Prog_DI], ax ; DI=SP
        mov [Prog_SP], ax ; SP=0xFFFE
        mov word [Prog_F], 0x0202 ; Interrupts enabled

CommandLoop:

.CmdLoop:
        ; Print prompt
        mov al, '-'
        call PutChar
        call ReadCommand
        jc .CmdLoop ; Blank line
        call CommandDispatch
        jmp .CmdLoop

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
        mov al, 0xFF
Exit:
        push ax
        ; Restore old Int1 handler
        mov dx, [OldInt1]
        mov ax, [OldInt1+2]
        mov bl, 1
        call SetIntVec
        pop ax
        mov ah, 0x4C
        int 0x21

; Set interrupt vector BL to DX:AX, return old vector in DX:AX
SetIntVec:
        xor bh, bh
        shl bx, 1
        shl bx, 1
        xor cx, cx
        mov es, cx
        xchg ax, [es:bx]
        xchg dx, [es:bx+2]
        ret

TerminateHandler:
        cli
        mov ax, cs
        mov ds, ax
        mov ss, ax
        mov sp, [DbgSP]
        sti
        mov dx, MsgProgramExit
        call PutString
        mov ah, 0x4d
        int 0x21
        push ax
        call PutHexWord
        call PutCrLf
        pop ax
        jmp Exit ; Exit for now..

SingleStep:
        cld
        mov [cs:Prog_DS], ds
        push cs
        pop ds
        mov [Prog_SS], ss
        mov [Prog_SP], sp
        mov [Prog_AX], ax
        mov [Prog_CX], cx
        mov [Prog_DX], dx
        mov [Prog_BX], bx
        mov [Prog_BP], bp
        mov [Prog_SI], si
        mov [Prog_DI], di
        mov [Prog_ES], es
        pop ax
        mov [Prog_IP], ax
        pop ax
        mov [Prog_CS], ax
        pop ax
        and ah, 0xFE ; Clear trap flag
        mov [Prog_F], ax
        mov ax, cs
        mov ss, ax
        mov sp, [DbgSP]
        sti
        mov si, EmptyArgs
        call Regs
        jmp CommandLoop

; Read line to CmdBuffer, returns SI pointing to first non-blank char
; Returns carry clear if non-empty
ReadCommand:
        ; Read command line
        mov bx, CmdBuffer
        mov word [bx], 0x7F
        mov dx, bx
        mov ah, 0x0a
        int 0x21
        call PutCrLf

        mov si, CmdBuffer+2
        ; Convert to upper case
        xor ch, ch
        mov cl, [si-1]
        and cl, cl
        jz .Blank
        xor bx, bx
.ToUpper:
        mov al, [si+bx]
        cmp al, 'a'
        jb .Next
        cmp al, 'z'
        ja .Next
        and al, 0xDF
        mov [si+bx], al
.Next:
        inc bx
        cmp bl, cl
        jne .ToUpper
        call CSkipSpaces
        cmp byte [si], ' '
        jbe .Blank
        clc
        ret
.Blank:
        stc
        ret

; SI=Command line
CommandDispatch:
        lodsb
        push ax
        call CSkipSpaces
        pop ax
        cmp al, 'G'
        je .CmdG
        cmp al, 'R'
        je .CmdR
        cmp al, 'T'
        je .CmdT
        cmp al, 'U'
        je .CmdU
        cmp al, 'Q'
        je .CmdQ
        jmp short InvalidCommmand
.CmdG:  jmp Go
.CmdR:  jmp Regs
.CmdT:  jmp Trace
.CmdU:  jmp Unassemble
.CmdQ: ; Q(uit)
        xor al, al
        jmp Exit

InvalidCommmand:
        mov dx, MsgErrInvCmd
        call PutString
        call PutCrLf
        ret

CSkipSpaces:
        lodsb
        cmp al, 0x0D
        je .Done
        cmp al, ' '
        jbe CSkipSpaces
.Done:
        dec si
        ret

; Get number from command line to DX
; returns carry clear on success
CGetNum:
        cmp byte [si], ' '
        ja .NotEmpty
        stc
        ret
.NotEmpty:
        xor dx, dx
        mov cl, 4
.L:
        lodsb
        cmp al, '0'
        jb .Done
        cmp al, '9'
        jbe .OK
        cmp al, 'A'
        jb .Done
        cmp al, 'F'
        ja .Done
.OK:
        shl dx, cl
        sub al, '0'
        cmp al, 9
        jbe .D
        sub al, 7
.D:
        or dl, al
        jmp .L
.Done:
        dec si
        mov ax, dx
        clc
        ret

Trace:
        cmp byte [si], ' '
        ja .ArgError
        or byte [Prog_F+1], 1 ; Set trap flag
        jmp StartProgram
.ArgError:
        mov dx, .Msg
        call PutString
        call PutCrLf
        ret
.Msg: db 'Arguments to T not implemented$'

Go:
        cmp byte [si], ' '
        jbe StartProgram
        mov dx, .Msg
        call PutString
        call PutCrLf
        ret
.Msg: db 'Arguments to G not implemented$'

StartProgram:
        cli
        ; Switch to program stack and build IRET frame
        mov ss, [Prog_SS]
        mov sp, [Prog_SP]
        mov ax, [Prog_F]
        push ax ; Flags
        mov ax, [Prog_CS]
        push ax ; CS
        mov ax, [Prog_IP]
        push ax ; IP
        mov es, [Prog_ES]
        mov ax, [Prog_AX]
        mov cx, [Prog_CX]
        mov dx, [Prog_DX]
        mov bx, [Prog_BX]
        mov bp, [Prog_BP]
        mov si, [Prog_SI]
        mov di, [Prog_DI]
        mov ds, [Prog_DS] ; DS last
        iret

; R(egister) [register]
Regs:
        cmp byte [si], ' '
        ja .Edit
        jmp .Show
.Edit:
        lodsw
        cmp ax, 'F'|0x0D<<8
        je .EditFlags
        cmp byte [si], ' '
        jbe .LenOK
.Bad:
        jmp InvalidCommmand
.EditFlags:
        call .PutFlags
        call PutSpace
        mov al, '-'
        call PutChar
        call ReadCommand
        jc .Done
.GetF:
        lodsw
        cmp byte [si], ' '
        ja .Bad
        mov dx, 1<<11
        mov bx, .Flags
.FindF:
        cmp [bx], ax
        jne .NotSet
        or [Prog_F], dx
        jmp short .NextF
.NotSet:
        cmp [bx+2], ax
        jne .NotClr
        not dx
        and [Prog_F], dx
        jmp short .NextF
.NotClr:
        shr dx, 1
        add bx, 4
        jnz .FindF
        jmp short .Bad
.NextF:
        call CSkipSpaces
        cmp byte [si], ' '
        ja .GetF
        jmp short .Done
.LenOK:
        mov si, .RegList
        mov cx, 8+5 ; Scan both lists
.MatchReg:
        cmp [si], ax
        je .FoundReg
        add si, 4
        dec cx
        jnz .MatchReg
        jmp InvalidCommmand
.FoundReg:
        mov bp, [si+2]
        call PutChar
        mov al, ah
        call PutChar
        call PutSpace
        mov ax, [bp]
        call PutHexWord
        call PutCrLf
        mov al, ':'
        call PutChar
        call ReadCommand
        jc .Done
        call CGetNum
        jc .Done
        mov [bp], ax
.Done:
        ret
.Show:
        mov si, .RegList
        mov cx, 8
        call .PutRegList
        call PutCrLf
        mov si, .RegList2
        mov cx, 5
        call .PutRegList
        call PutSpace
        call .PutFlags
        call PutCrLf
        mov dx, [Prog_CS]
        mov ax, [Prog_IP]
        mov es, dx
        mov [DisOff], ax
        call PutHexDword
        call PutSpace
        call Disasm
        call PutCrLf
        ret
.PutFlags:
        mov si, .Flags
        mov dx, [Prog_F]
        mov ax, 1<<11
.DoFlags:
        mov bx, [si]
        and bx, bx
        jz .SkipFlag
        test dx, ax
        jnz .PrintFlag
        mov bx, [si+2]
.PrintFlag:
        push ax
        mov al, bl
        call PutChar
        mov al, bh
        call PutChar
        call PutSpace
        pop ax
.SkipFlag:
        add si, 4
        shr ax, 1
        jnz .DoFlags
        ret
.PutRegList:
        lodsw
        call PutChar
        mov al, ah
        call PutChar
        mov al, '='
        call PutChar
        lodsw
        mov bx, ax
        mov ax, [bx]
        call PutHexWord
        call PutSpace
        call PutSpace
        dec cx
        jnz .PutRegList
        ret
.RegList:
        dw 'AX', Prog_AX
        dw 'BX', Prog_BX
        dw 'CX', Prog_CX
        dw 'DX', Prog_DX
        dw 'SP', Prog_SP
        dw 'BP', Prog_BP
        dw 'SI', Prog_SI
        dw 'DI', Prog_DI
.RegList2: ; Must follow .RegList!
        dw 'DS', Prog_DS
        dw 'ES', Prog_ES
        dw 'SS', Prog_SS
        dw 'CS', Prog_CS
        dw 'IP', Prog_IP
.Flags:
        dw 'OV', 'NV' ; bit 11 Overflow
        dw 'DN', 'UP' ; bit 10 Direction
        dw 'EI', 'DI' ; bit 9  Interrupt
        dw  0  , 0    ; bit 8  Trap
        dw 'NG', 'PL' ; bit 7  Sign
        dw 'ZR', 'NZ' ; bit 6  Zero
        dw  0  , 0    ; bit 5  Reserved
        dw 'AC', 'NA' ; bit 4  Adjust
        dw 0   , 0    ; bit 3  Reserved
        dw 'PE', 'PO' ; bit 2  Parity
        dw 0   , 0    ; bit 1  Reserved
        dw 'CY', 'NC' ; bit 0  Carry

; U(nassemble) [range]
Unassemble:
        mov es, [Prog_CS]
        mov bp, [DisOff]
        add bp, DEFAULT_DIS_SIZE
        call CGetNum
        jc .U
        cmp byte [si], ':'
        jne .WasOffset
        inc si
        mov es, ax
        call CGetNum
        jnc .WasOffset
        jmp InvalidCommmand
.WasOffset:
        mov [DisOff], ax
        mov bp, ax
        add bp, DEFAULT_DIS_SIZE
        call CSkipSpaces
        call CGetNum
        jc .U
        mov bp, ax
.U:
        ; Unassemble from DisOff..BP
        mov ax, [DisOff]
        cmp ax, bp
        jae .Done
        mov dx, es
        call PutHexDword
        call PutSpace
        call Disasm
        jmp .U
.Done:
        ret

Disasm:
        mov ax, [DisOff]
        push ax
        call GetInstruction
        pop si ; SI = Instruction start
        mov cx, [DisOff]
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
        and dx, dx
        jnz .Normal
        mov dx, N_UNKNOWN
        call PutString
        jmp short .Done
.Normal:
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
        mov si, [DisOff]
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
        mov [DisOff], si
        ret
.Next:
        or byte [Prefixes], ah
        inc si
        jmp .GPs

; Get instruction byte to AL (and increment DisOff)
GetIByte:
        push si
        mov si, [DisOff]
        mov al, [es:si]
        inc word [DisOff]
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
        cmp al, 0x0F
        jne .OneByte
        call GetIByte
        mov [InstBytes+1], al
        ; For now, don't bother with complete table for 0F XX
        cmp al, 0x80
        jb .NotJcc
        cmp al, 0x8F
        ja .NotJcc
        ; 0F 8x -> 7x with operand changed
        xor bh, bh
        mov bl, al
        sub bl, 0x10
        shl bx, 1
        shl bx, 1
        mov bx, [MainTab+bx]
        mov ax, OTYPE_REL16 | OTYPE_NONE<<8
        jmp short .CheckKnown
.NotJcc:
        ; MOVZX/MOVSX?
        mov bx, N_MOVZX
        cmp al, 0xB6
        je .MovXX
        mov bx, N_MOVSX
        cmp al, 0xBE
        je .MovXX
        xor bx, bx
        jmp short .CheckKnown
.MovXX:
        mov ax, OTYPE_R16 | OTYPE_RM8<<8
        jmp short .CheckKnown
.OneByte:
        xor bx, bx
        mov bl, al
        shl bx, 1
        shl bx, 1
        mov ax, [MainTab+bx+2] ; AX = args
        mov bx, [MainTab+bx]
.CheckKnown:
        mov [InstInfo], bx
        mov [InstInfo+2], ax
        and bx, bx
        jnz .Known
        ret
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
        xor dh, dh
        mov dl, [ModRM]
        mov cl, 3
        shr dl, cl
        and dl, 7
        ; CH +bx
        shl dx, 1
        shl dx, 1
        mov si, dx
        mov ax, [bx+si+2]
        mov bx, [bx+si]
        mov [InstInfo], bx
        mov [InstInfo+2], ax
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
        jmp short .ImmHandled
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

; AL/AH are operands
GetRM:
        mov di, RMText
        cmp al, OTYPE_MOFF
        je .Moff
        cmp ah, OTYPE_MOFF
        je .Moff
        cmp byte [HasModRM], 0
        jne .ModRM
        ret
.Moff:
        call .MemStart
        call GetIWord
        call .CvtRMWord
        jmp .MemDone
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
        mov [di], ax
        mov byte [di+2], '$'
        ret
.MemStart:
        push ax
        push bx
        mov byte [di], '['
        inc di
        ; Check segment override
        mov bl, [Prefixes]
        mov ax, 'ES'
        mov bh, PREFIX_ES
        test bl, bh
        jnz .DoSO
        mov ax, 'CS'
        mov bh, PREFIX_CS
        test bl, bh
        jnz .DoSO
        mov ax, 'SS'
        mov bh, PREFIX_SS
        test bl, bh
        jnz .DoSO
        mov ax, 'DS'
        mov bh, PREFIX_DS
        test bl, bh
        jnz .DoSO
        jmp short .SODone
.DoSO:
        not bh
        and [Prefixes], bh
        mov [di], ax
        mov byte [di+2], ':'
        add di, 3
.SODone:
        pop bx
        pop ax
        ret
.NotReg:
        call .MemStart
        mov bh, bl
        and bh, 0xC7
        cmp bh, 6
        jne .NotRawDisp16
        call GetIWord
        call .CvtRMWord
.MemDone:
        mov byte [di], ']'
        mov byte [di+1], '$'
        ret
.NotRawDisp16:
        xor bh, bh
        mov si, bx
        and si, 7
        add si, si
        mov si, [MemNames+si]
.Cpy:
        lodsb
        cmp al, '$'
        je .CpyDone
        mov [di], al
        inc di
        jmp .Cpy
.CpyDone:
        mov cl, 6
        shr bl, cl
        and bl, bl
        jz .MemDone
        call GetIByte
        cbw
        cmp bl, 1
        je .DoDisp
        mov bx, ax
        call GetIByte
        mov ah, al
        mov al, bl
.DoDisp:
        mov byte [di], '+'
        inc di
        call .CvtRMWord
        jmp .MemDone
.CvtRMWord:
        mov word [di], '0x'
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
        mov [di], al
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
        jmp short .RM
.Rel:
        add bx, [DisOff]
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
        test al, PREFIX_ES
        jz .NoES
        mov dx, N_ES
        call .PutWSpace
.NoES:
        test al, PREFIX_CS
        jz .NoCS
        mov dx, N_CS
        call .PutWSpace
.NoCS:
        test al, PREFIX_SS
        jz .NoSS
        mov dx, N_SS
        call .PutWSpace
.NoSS:
        test al, PREFIX_DS
        jz .Done
        mov dx, N_DS
        call .PutWSpace
.Done:
        ret
.PutWSpace:
        call PutString
        jmp PutSpace

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

MainTab:
    ; 0x00
    dw N_ADD    , OTYPE_RM8    | OTYPE_R8    << 8 ; 00
    dw N_ADD    , OTYPE_RM16   | OTYPE_R16   << 8 ; 01
    dw N_ADD    , OTYPE_R8     | OTYPE_RM8   << 8 ; 02
    dw N_ADD    , OTYPE_R16    | OTYPE_RM16  << 8 ; 03
    dw N_ADD    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 04
    dw N_ADD    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 05
    dw N_PUSH   , OTYPE_ES     | OTYPE_NONE  << 8 ; 06
    dw N_POP    , OTYPE_ES     | OTYPE_NONE  << 8 ; 07
    dw N_OR     , OTYPE_RM8    | OTYPE_R8    << 8 ; 08
    dw N_OR     , OTYPE_RM16   | OTYPE_R16   << 8 ; 09
    dw N_OR     , OTYPE_R8     | OTYPE_RM8   << 8 ; 0A
    dw N_OR     , OTYPE_R16    | OTYPE_RM16  << 8 ; 0B
    dw N_OR     , OTYPE_AL     | OTYPE_IMM8  << 8 ; 0C
    dw N_OR     , OTYPE_AX     | OTYPE_IMM16 << 8 ; 0D
    dw N_PUSH   , OTYPE_CS     | OTYPE_NONE  << 8 ; 0E
    dw 0        , 0 ; 0x0F -> Two byte instruction  0F
    ; 0x10
    dw N_ADC    , OTYPE_RM8    | OTYPE_R8    << 8 ; 10
    dw N_ADC    , OTYPE_RM16   | OTYPE_R16   << 8 ; 11
    dw N_ADC    , OTYPE_R8     | OTYPE_RM8   << 8 ; 12
    dw N_ADC    , OTYPE_R16    | OTYPE_RM16  << 8 ; 13
    dw N_ADC    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 14
    dw N_ADC    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 15
    dw N_PUSH   , OTYPE_SS     | OTYPE_NONE  << 8 ; 16
    dw N_POP    , OTYPE_SS     | OTYPE_NONE  << 8 ; 17
    dw N_SBB    , OTYPE_RM8    | OTYPE_R8    << 8 ; 18
    dw N_SBB    , OTYPE_RM16   | OTYPE_R16   << 8 ; 19
    dw N_SBB    , OTYPE_R8     | OTYPE_RM8   << 8 ; 1A
    dw N_SBB    , OTYPE_R16    | OTYPE_RM16  << 8 ; 1B
    dw N_SBB    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 1C
    dw N_SBB    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 1D
    dw N_PUSH   , OTYPE_DS     | OTYPE_NONE  << 8 ; 1E
    dw N_POP    , OTYPE_DS     | OTYPE_NONE  << 8 ; 1F
    ; 0x20
    dw N_AND    , OTYPE_RM8    | OTYPE_R8    << 8 ; 20
    dw N_AND    , OTYPE_RM16   | OTYPE_R16   << 8 ; 21
    dw N_AND    , OTYPE_R8     | OTYPE_RM8   << 8 ; 22
    dw N_AND    , OTYPE_R16    | OTYPE_RM16  << 8 ; 23
    dw N_AND    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 24
    dw N_AND    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 25
    dw 0        , 0 ; PREFIX_ES                     26
    dw N_DAA    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 27
    dw N_SUB    , OTYPE_RM8    | OTYPE_R8    << 8 ; 28
    dw N_SUB    , OTYPE_RM16   | OTYPE_R16   << 8 ; 29
    dw N_SUB    , OTYPE_R8     | OTYPE_RM8   << 8 ; 2A
    dw N_SUB    , OTYPE_R16    | OTYPE_RM16  << 8 ; 2B
    dw N_SUB    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 2C
    dw N_SUB    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 2D
    dw 0        , 0 ; PREFIX_CS                   ; 2E
    dw N_DAS    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 2F
    ; 0x30
    dw N_XOR    , OTYPE_RM8    | OTYPE_R8    << 8 ; 30
    dw N_XOR    , OTYPE_RM16   | OTYPE_R16   << 8 ; 31
    dw N_XOR    , OTYPE_R8     | OTYPE_RM8   << 8 ; 32
    dw N_XOR    , OTYPE_R16    | OTYPE_RM16  << 8 ; 33
    dw N_XOR    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 34
    dw N_XOR    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 35
    dw 0        , 0 ; PREFIX_SS                   ; 36
    dw N_AAA    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 37
    dw N_CMP    , OTYPE_RM8    | OTYPE_R8    << 8 ; 38
    dw N_CMP    , OTYPE_RM16   | OTYPE_R16   << 8 ; 39
    dw N_CMP    , OTYPE_R8     | OTYPE_RM8   << 8 ; 3A
    dw N_CMP    , OTYPE_R16    | OTYPE_RM16  << 8 ; 3B
    dw N_CMP    , OTYPE_AL     | OTYPE_IMM8  << 8 ; 3C
    dw N_CMP    , OTYPE_AX     | OTYPE_IMM16 << 8 ; 3D
    dw 0        , 0 ; PREFIX_DS                   ; 3E
    dw N_AAS    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 3F
    ; 0x40
    dw N_INC    , OTYPE_AX     | OTYPE_NONE  << 8 ; 40
    dw N_INC    , OTYPE_CX     | OTYPE_NONE  << 8 ; 41
    dw N_INC    , OTYPE_DX     | OTYPE_NONE  << 8 ; 42
    dw N_INC    , OTYPE_BX     | OTYPE_NONE  << 8 ; 43
    dw N_INC    , OTYPE_SP     | OTYPE_NONE  << 8 ; 44
    dw N_INC    , OTYPE_BP     | OTYPE_NONE  << 8 ; 45
    dw N_INC    , OTYPE_SI     | OTYPE_NONE  << 8 ; 46
    dw N_INC    , OTYPE_DI     | OTYPE_NONE  << 8 ; 47
    dw N_DEC    , OTYPE_AX     | OTYPE_NONE  << 8 ; 48
    dw N_DEC    , OTYPE_CX     | OTYPE_NONE  << 8 ; 49
    dw N_DEC    , OTYPE_DX     | OTYPE_NONE  << 8 ; 4A
    dw N_DEC    , OTYPE_BX     | OTYPE_NONE  << 8 ; 4B
    dw N_DEC    , OTYPE_SP     | OTYPE_NONE  << 8 ; 4C
    dw N_DEC    , OTYPE_BP     | OTYPE_NONE  << 8 ; 4D
    dw N_DEC    , OTYPE_SI     | OTYPE_NONE  << 8 ; 4E
    dw N_DEC    , OTYPE_DI     | OTYPE_NONE  << 8 ; 4F
    ; 0x50
    dw N_PUSH   , OTYPE_AX     | OTYPE_NONE  << 8 ; 50
    dw N_PUSH   , OTYPE_CX     | OTYPE_NONE  << 8 ; 51
    dw N_PUSH   , OTYPE_DX     | OTYPE_NONE  << 8 ; 52
    dw N_PUSH   , OTYPE_BX     | OTYPE_NONE  << 8 ; 53
    dw N_PUSH   , OTYPE_SP     | OTYPE_NONE  << 8 ; 54
    dw N_PUSH   , OTYPE_BP     | OTYPE_NONE  << 8 ; 55
    dw N_PUSH   , OTYPE_SI     | OTYPE_NONE  << 8 ; 56
    dw N_PUSH   , OTYPE_DI     | OTYPE_NONE  << 8 ; 57
    dw N_POP    , OTYPE_AX     | OTYPE_NONE  << 8 ; 58
    dw N_POP    , OTYPE_CX     | OTYPE_NONE  << 8 ; 59
    dw N_POP    , OTYPE_DX     | OTYPE_NONE  << 8 ; 5A
    dw N_POP    , OTYPE_BX     | OTYPE_NONE  << 8 ; 5B
    dw N_POP    , OTYPE_SP     | OTYPE_NONE  << 8 ; 5C
    dw N_POP    , OTYPE_BP     | OTYPE_NONE  << 8 ; 5D
    dw N_POP    , OTYPE_SI     | OTYPE_NONE  << 8 ; 5E
    dw N_POP    , OTYPE_DI     | OTYPE_NONE  << 8 ; 5F
    ; 0x60
    dw N_PUSHA  , OTYPE_NONE   | OTYPE_NONE  << 8 ; 60
    dw N_POPA   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 61
    dw 0        , 0                               ; 62
    dw 0        , 0                               ; 63
    dw 0        , 0                               ; 64
    dw 0        , 0                               ; 65
    dw 0        , 0                               ; 66
    dw 0        , 0                               ; 67
    dw N_PUSH   , OTYPE_IMM16  | OTYPE_NONE  << 8 ; 68
    dw 0        , 0                               ; 69
    dw N_PUSH   , OTYPE_IMM8   | OTYPE_NONE  << 8 ; 6A
    dw 0        , 0                               ; 6B
    dw N_INSB   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 6C
    dw N_INSW   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 6D
    dw N_OUTSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; 6E
    dw N_OUTSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; 6F
    ; 0x70
    dw N_JO     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 70
    dw N_JNO    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 71
    dw N_JC     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 72
    dw N_JNC    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 73
    dw N_JZ     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 74
    dw N_JNZ    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 75
    dw N_JNA    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 76
    dw N_JA     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 77
    dw N_JS     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 78
    dw N_JNS    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 79
    dw N_JPE    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7A
    dw N_JPO    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7B
    dw N_JL     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7C
    dw N_JNL    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7D
    dw N_JNG    , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7E
    dw N_JG     , OTYPE_REL8   | OTYPE_NONE  << 8 ; 7F
    ; 0x80
    dw Tab_80   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; 80
    dw Tab_81   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; 81
    dw Tab_80   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; 82
    dw Tab_83   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; 83
    dw N_TEST   , OTYPE_RM8    | OTYPE_R8    << 8 ; 84
    dw N_TEST   , OTYPE_RM16   | OTYPE_R16   << 8 ; 85
    dw N_XCHG   , OTYPE_R8     | OTYPE_RM8   << 8 ; 86
    dw N_XCHG   , OTYPE_R16    | OTYPE_RM16  << 8 ; 87
    dw N_MOV    , OTYPE_RM8    | OTYPE_R8    << 8 ; 88
    dw N_MOV    , OTYPE_RM16   | OTYPE_R16   << 8 ; 89
    dw N_MOV    , OTYPE_R8     | OTYPE_RM8   << 8 ; 8A
    dw N_MOV    , OTYPE_R16    | OTYPE_RM16  << 8 ; 8B
    dw N_MOV    , OTYPE_RM16   | OTYPE_SREG  << 8 ; 8C
    dw N_LEA    , OTYPE_R16    | OTYPE_RM16  << 8 ; 8D
    dw N_MOV    , OTYPE_SREG   | OTYPE_RM16  << 8 ; 8E
    dw 0        , 0                               ; 8F
    ; 0x90
    dw N_NOP    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 90
    dw N_XCHG   , OTYPE_CX     | OTYPE_AX    << 8 ; 91
    dw N_XCHG   , OTYPE_DX     | OTYPE_AX    << 8 ; 92
    dw N_XCHG   , OTYPE_BX     | OTYPE_AX    << 8 ; 93
    dw N_XCHG   , OTYPE_SP     | OTYPE_AX    << 8 ; 94
    dw N_XCHG   , OTYPE_BP     | OTYPE_AX    << 8 ; 95
    dw N_XCHG   , OTYPE_SI     | OTYPE_AX    << 8 ; 96
    dw N_XCHG   , OTYPE_DI     | OTYPE_AX    << 8 ; 97
    dw N_CBW    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 98
    dw N_CWD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; 99
    dw 0        , 0                               ; 9A
    dw 0        , 0                               ; 9B
    dw N_PUSHF  , OTYPE_NONE   | OTYPE_NONE  << 8 ; 9C
    dw N_POPF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 9D
    dw N_SAHF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 9E
    dw N_LAHF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; 9F
    ; 0xA0
    dw N_MOV    , OTYPE_AL     | OTYPE_MOFF  << 8 ; A0
    dw N_MOV    , OTYPE_AX     | OTYPE_MOFF  << 8 ; A1
    dw N_MOV    , OTYPE_MOFF   | OTYPE_AL    << 8 ; A2
    dw N_MOV    , OTYPE_MOFF   | OTYPE_AX    << 8 ; A3
    dw N_MOVSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; A4
    dw N_MOVSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; A5
    dw N_CMPSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; A6
    dw N_CMPSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; A7
    dw 0        , 0                               ; A8
    dw 0        , 0                               ; A9
    dw N_STOSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AA
    dw N_STOSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AB
    dw N_LODSB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AC
    dw N_LODSW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AD
    dw N_SCASB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AE
    dw N_SCASW  , OTYPE_NONE   | OTYPE_NONE  << 8 ; AF
    ; 0xB0
    dw N_MOV    , OTYPE_AL     | OTYPE_IMM8  << 8 ; B0
    dw N_MOV    , OTYPE_CL     | OTYPE_IMM8  << 8 ; B1
    dw N_MOV    , OTYPE_DL     | OTYPE_IMM8  << 8 ; B2
    dw N_MOV    , OTYPE_BL     | OTYPE_IMM8  << 8 ; B3
    dw N_MOV    , OTYPE_AH     | OTYPE_IMM8  << 8 ; B4
    dw N_MOV    , OTYPE_CH     | OTYPE_IMM8  << 8 ; B5
    dw N_MOV    , OTYPE_DH     | OTYPE_IMM8  << 8 ; B6
    dw N_MOV    , OTYPE_BH     | OTYPE_IMM8  << 8 ; B7
    dw N_MOV    , OTYPE_AX     | OTYPE_IMM16 << 8 ; B8
    dw N_MOV    , OTYPE_CX     | OTYPE_IMM16 << 8 ; B9
    dw N_MOV    , OTYPE_DX     | OTYPE_IMM16 << 8 ; BA
    dw N_MOV    , OTYPE_BX     | OTYPE_IMM16 << 8 ; BB
    dw N_MOV    , OTYPE_SP     | OTYPE_IMM16 << 8 ; BC
    dw N_MOV    , OTYPE_BP     | OTYPE_IMM16 << 8 ; BD
    dw N_MOV    , OTYPE_SI     | OTYPE_IMM16 << 8 ; BE
    dw N_MOV    , OTYPE_DI     | OTYPE_IMM16 << 8 ; BF
    ; 0xC0
    dw Tab_C0   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; C0
    dw Tab_C1   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; C1
    dw N_RET    , OTYPE_IMM16  | OTYPE_NONE  << 8 ; C2
    dw N_RET    , OTYPE_NONE   | OTYPE_NONE  << 8 ; C3
    dw N_LES    , OTYPE_R16    | OTYPE_RM16  << 8 ; C4
    dw N_LDS    , OTYPE_R16    | OTYPE_RM16  << 8 ; C5
    dw Tab_C6   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; C6
    dw Tab_C7   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; C7
    dw 0        , 0                               ; C8
    dw 0        , 0                               ; C9
    dw N_RETF   , OTYPE_IMM16  | OTYPE_NONE  << 8 ; CA
    dw N_RETF   , OTYPE_NONE   | OTYPE_NONE  << 8 ; CB
    dw N_INT3   , OTYPE_NONE   | OTYPE_NONE  << 8 ; CC
    dw N_INT    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; CD
    dw N_INTO   , OTYPE_NONE   | OTYPE_NONE  << 8 ; CE
    dw N_IRET   , OTYPE_NONE   | OTYPE_NONE  << 8 ; CF
    ; 0xD0
    dw Tab_D0   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; D0
    dw Tab_D1   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; D1
    dw Tab_D2   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; D2
    dw Tab_D3   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; D3
    dw N_AAM    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; D4
    dw N_AAD    , OTYPE_IMM8   | OTYPE_NONE  << 8 ; D5
    dw 0        , 0                               ; D6
    dw N_XLATB  , OTYPE_NONE   | OTYPE_NONE  << 8 ; D7
    dw 0        , 0                               ; D8
    dw 0        , 0                               ; D9
    dw 0        , 0                               ; DA
    dw 0        , 0                               ; DB
    dw 0        , 0                               ; DC
    dw 0        , 0                               ; DD
    dw 0        , 0                               ; DE
    dw 0        , 0                               ; DF
    ; 0xE0
    dw N_LOOPNE , OTYPE_REL8   | OTYPE_NONE  << 8 ; E0
    dw N_LOOPE  , OTYPE_REL8   | OTYPE_NONE  << 8 ; E1
    dw N_LOOP   , OTYPE_REL8   | OTYPE_NONE  << 8 ; E2
    dw N_JCXZ   , OTYPE_REL8   | OTYPE_NONE  << 8 ; E3
    dw N_IN     , OTYPE_AL     | OTYPE_IMM8  << 8 ; E4
    dw N_IN     , OTYPE_AX     | OTYPE_IMM8  << 8 ; E5
    dw N_OUT    , OTYPE_IMM8   | OTYPE_AL    << 8 ; E6
    dw N_OUT    , OTYPE_IMM8   | OTYPE_AX    << 8 ; E7
    dw N_CALL   , OTYPE_REL16  | OTYPE_NONE  << 8 ; E8
    dw N_JMP    , OTYPE_REL16  | OTYPE_NONE  << 8 ; E9
    dw 0        , 0                               ; EA
    dw N_JMP    , OTYPE_REL8   | OTYPE_NONE  << 8 ; EB
    dw N_IN     , OTYPE_AL     | OTYPE_DX    << 8 ; EC
    dw N_IN     , OTYPE_AX     | OTYPE_DX    << 8 ; ED
    dw N_OUT    , OTYPE_DX     | OTYPE_AL    << 8 ; EE
    dw N_OUT    , OTYPE_DX     | OTYPE_AX    << 8 ; EF
    ; 0xF0
    dw 0        , 0 ; LOCK                        ; F0
    dw 0        , 0                               ; F1
    dw 0        , 0 ; REPNE/REP                   ; F2
    dw 0        , 0 ; REPE                        ; F3
    dw N_HLT    , OTYPE_NONE   | OTYPE_NONE  << 8 ; F4
    dw N_CMC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; F5
    dw Tab_F6   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; F6
    dw Tab_F7   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; F7
    dw N_CLC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; F8
    dw N_STC    , OTYPE_NONE   | OTYPE_NONE  << 8 ; F9
    dw N_CLI    , OTYPE_NONE   | OTYPE_NONE  << 8 ; FA
    dw N_STI    , OTYPE_NONE   | OTYPE_NONE  << 8 ; FB
    dw N_CLD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; FC
    dw N_STD    , OTYPE_NONE   | OTYPE_NONE  << 8 ; FD
    dw Tab_FE   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; FE
    dw Tab_FF   , OTYPE_RTAB   | OTYPE_NONE  << 8 ; FF

Tab_80:
    dw N_ADD    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /0
    dw N_OR     , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /1
    dw N_ADC    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /2
    dw N_SBB    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /3
    dw N_AND    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /4
    dw N_SUB    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /5
    dw N_XOR    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /6
    dw N_CMP    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /7

Tab_81:
    dw N_ADD    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /0
    dw N_OR     , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /1
    dw N_ADC    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /2
    dw N_SBB    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /3
    dw N_AND    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /4
    dw N_SUB    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /5
    dw N_XOR    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /6
    dw N_CMP    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /7

Tab_83:
    dw N_ADD    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /0
    dw N_OR     , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /1
    dw N_ADC    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /2
    dw N_SBB    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /3
    dw N_AND    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /4
    dw N_SUB    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /5
    dw N_XOR    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /6
    dw N_CMP    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /7

Tab_C0:
    dw N_ROL    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /0
    dw N_ROR    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /1
    dw N_RCL    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /2
    dw N_RCR    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /3
    dw N_SHL    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /4
    dw N_SHR    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /5
    dw N_SHL    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /6
    dw N_SAR    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /7

Tab_C1:
    dw N_ROL    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /0
    dw N_ROR    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /1
    dw N_RCL    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /2
    dw N_RCR    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /3
    dw N_SHL    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /4
    dw N_SHR    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /5
    dw N_SHL    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /6
    dw N_SAR    , OTYPE_RM16   | OTYPE_IMM8  << 8 ; /7

Tab_C6:
    dw N_MOV    , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /0
    dw 0        , 0                               ; /1
    dw 0        , 0                               ; /2
    dw 0        , 0                               ; /3
    dw 0        , 0                               ; /4
    dw 0        , 0                               ; /5
    dw 0        , 0                               ; /6

Tab_C7:
    dw N_MOV    , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /0
    dw 0        , 0                               ; /1
    dw 0        , 0                               ; /2
    dw 0        , 0                               ; /3
    dw 0        , 0                               ; /4
    dw 0        , 0                               ; /5
    dw 0        , 0                               ; /6
    dw 0        , 0                               ; /7

Tab_D0:
    dw N_ROL    , OTYPE_RM8    | OTYPE_1     << 8 ; /0
    dw N_ROR    , OTYPE_RM8    | OTYPE_1     << 8 ; /1
    dw N_RCL    , OTYPE_RM8    | OTYPE_1     << 8 ; /2
    dw N_RCR    , OTYPE_RM8    | OTYPE_1     << 8 ; /3
    dw N_SHL    , OTYPE_RM8    | OTYPE_1     << 8 ; /4
    dw N_SHR    , OTYPE_RM8    | OTYPE_1     << 8 ; /5
    dw N_SHL    , OTYPE_RM8    | OTYPE_1     << 8 ; /6
    dw N_SAR    , OTYPE_RM8    | OTYPE_1     << 8 ; /7

Tab_D1:
    dw N_ROL    , OTYPE_RM16   | OTYPE_1     << 8 ; /0
    dw N_ROR    , OTYPE_RM16   | OTYPE_1     << 8 ; /1
    dw N_RCL    , OTYPE_RM16   | OTYPE_1     << 8 ; /2
    dw N_RCR    , OTYPE_RM16   | OTYPE_1     << 8 ; /3
    dw N_SHL    , OTYPE_RM16   | OTYPE_1     << 8 ; /4
    dw N_SHR    , OTYPE_RM16   | OTYPE_1     << 8 ; /5
    dw N_SHL    , OTYPE_RM16   | OTYPE_1     << 8 ; /6
    dw N_SAR    , OTYPE_RM16   | OTYPE_1     << 8 ; /7

Tab_D2:
    dw N_ROL    , OTYPE_RM8    | OTYPE_CL    << 8 ; /0
    dw N_ROR    , OTYPE_RM8    | OTYPE_CL    << 8 ; /1
    dw N_RCL    , OTYPE_RM8    | OTYPE_CL    << 8 ; /2
    dw N_RCR    , OTYPE_RM8    | OTYPE_CL    << 8 ; /3
    dw N_SHL    , OTYPE_RM8    | OTYPE_CL    << 8 ; /4
    dw N_SHR    , OTYPE_RM8    | OTYPE_CL    << 8 ; /5
    dw N_SHL    , OTYPE_RM8    | OTYPE_CL    << 8 ; /6
    dw N_SAR    , OTYPE_RM8    | OTYPE_CL    << 8 ; /7

Tab_D3:
    dw N_ROL    , OTYPE_RM16   | OTYPE_CL    << 8 ; /0
    dw N_ROR    , OTYPE_RM16   | OTYPE_CL    << 8 ; /1
    dw N_RCL    , OTYPE_RM16   | OTYPE_CL    << 8 ; /2
    dw N_RCR    , OTYPE_RM16   | OTYPE_CL    << 8 ; /3
    dw N_SHL    , OTYPE_RM16   | OTYPE_CL    << 8 ; /4
    dw N_SHR    , OTYPE_RM16   | OTYPE_CL    << 8 ; /5
    dw N_SHL    , OTYPE_RM16   | OTYPE_CL    << 8 ; /6
    dw N_SAR    , OTYPE_RM16   | OTYPE_CL    << 8 ; /7

Tab_F6:
    dw N_TEST   , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /0
    dw N_TEST   , OTYPE_RM8    | OTYPE_IMM8  << 8 ; /1
    dw N_NOT    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /2
    dw N_NEG    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /3
    dw N_MUL    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /4
    dw N_IMUL   , OTYPE_RM8    | OTYPE_NONE  << 8 ; /5
    dw N_DIV    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /6
    dw N_IDIV   , OTYPE_RM8    | OTYPE_NONE  << 8 ; /7

Tab_F7:
    dw N_TEST   , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /0
    dw N_TEST   , OTYPE_RM16   | OTYPE_IMM16 << 8 ; /1
    dw N_NOT    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /2
    dw N_NEG    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /3
    dw N_MUL    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /4
    dw N_IMUL   , OTYPE_RM16   | OTYPE_NONE  << 8 ; /5
    dw N_DIV    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /6
    dw N_IDIV   , OTYPE_RM16   | OTYPE_NONE  << 8 ; /7

Tab_FE:
    dw N_INC    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /0
    dw N_DEC    , OTYPE_RM8    | OTYPE_NONE  << 8 ; /1
    dw 0        , 0                               ; /2
    dw 0        , 0                               ; /3
    dw 0        , 0                               ; /4
    dw 0        , 0                               ; /5
    dw 0        , 0                               ; /6
    dw 0        , 0                               ; /7

Tab_FF:
    dw N_INC    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /0
    dw N_DEC    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /1
    dw N_CALL   , OTYPE_RM16   | OTYPE_NONE  << 8 ; /2
    dw N_CALLF  , OTYPE_RM16   | OTYPE_NONE  << 8 ; /3
    dw N_JMP    , OTYPE_RM16   | OTYPE_NONE  << 8 ; /4
    dw N_JMPF   , OTYPE_RM16   | OTYPE_NONE  << 8 ; /5
    dw N_PUSH   , OTYPE_RM16   | OTYPE_NONE  << 8 ; /6
    dw 0        , 0                               ; /7

N_UNKNOWN: db 'UNKNOWN$'

N_LOCK:   db 'LOCK$'
N_REP:    db 'REP$'
N_REPZ:   db 'REPZ$'
N_REPNZ:  db 'REPNZ$'
N_ES:     db 'ES$'
N_CS:     db 'CS$'
N_SS:     db 'SS$'
N_DS:     db 'DS$'

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

MN_0: db 'BX+SI$'
MN_1: db 'BX+DI$'
MN_2: db 'BP+SI$'
MN_3: db 'BP+DI$'
MN_4: db 'SI$'
MN_5: db 'DI$'
MN_6: db 'BP$'
MN_7: db 'BX$'
MemNames:
    dw MN_0, MN_1, MN_2, MN_3, MN_4, MN_5, MN_6, MN_7

EmptyArgs: db 0x0D

MsgProgramExit:  db 'Program exited with error code $'

MsgErrUsage:     db 'Usage: DEBUG program$'
MsgErrLoad:      db 'Could not load program$'
MsgErrInvCmd:    db 'Invalid Command$'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BssStart:

; Loaded program/debugger state

OldInt1:         resw 2
DbgSP:           resw 1 ; Debugger stack pointer when program running
CodeSeg:         resw 1 ; PSP segment of lodaded program
Prog_AX:         resw 1
Prog_CX:         resw 1
Prog_DX:         resw 1
Prog_BX:         resw 1
Prog_SP:         resw 1
Prog_BP:         resw 1
Prog_SI:         resw 1
Prog_DI:         resw 1
Prog_ES:         resw 1
Prog_CS:         resw 1
Prog_SS:         resw 1
Prog_DS:         resw 1
Prog_IP:         resw 1
Prog_F:          resw 1

; Disassembler State

DisOff:          resw 1
Prefixes:        resb 1
InstInfo:        resw 2
InstBytes:       resb 2
HasModRM:        resb 1
ModRM:           resb 1
Immediate:       resw 1
RMText:          resb 30

; Command handling state

ParameterBlock:  resw 1 ; 0x00 WORD  Segment of environment to copy (0 = use caller's)
PB_ArgPtr:       resw 2 ; 0x02 DWORD Pointer to arguments
                 resw 2 ; 0x06 DWORD Pointer to first FCB
                 resw 2 ; 0x0A DWORD Pointer second first FCB
                 resw 2 ; 0x0E DWORD Initial child SS:SP
                 resw 2 ; 0x12 DWORD Initial child CS:IP

CmdBuffer:       resb 2+0x7F

ProgramEnd: ; Keep last
