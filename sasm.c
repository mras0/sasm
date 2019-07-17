#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

typedef signed char S1;
typedef unsigned char U1;
typedef unsigned short U2;
typedef short S2;

#define TOKEN_MAX 32
#define LABEL_MAX 200
#define FIXUP_MAX 400
#define EQU_MAX   100
#define OUTPUT_MAX 0x2000
#define INVALID_ADDR 0xFFFF

struct Label {
    char Name[TOKEN_MAX+1];
    U2 Address;
    U2 Fixup;
};

struct Fixup {
    U2 Address;
    U2 Next;
};

struct Equ {
    char Name[TOKEN_MAX+1];
    U2 Value;
};

FILE* InputFile;
U2 CurrentLine;
U2 NumNewLines;
U1 CurrentChar;
char TokenText[TOKEN_MAX+1];
U1 TokenLen;
U2 CurrentAddress;
U1 OutputBuffer[OUTPUT_MAX];
U2 OutputOffset;
struct Label Labels[LABEL_MAX];
struct Fixup Fixups[FIXUP_MAX];
struct Equ   Equs[EQU_MAX];
U2 NumLabels;
U2 FreeFixup;
U2 NumEqus;

enum {
    OP_REG=0xc0,
    OP_LIT,
} OperandType, OperandLType;
U2 OperandValue, OperandLValue;
struct Fixup* CurrentFixup, * CurrentLFixup;
U1 ExplicitSize;

enum {
    R_AL, R_CL, R_DL, R_BL, R_AH, R_CH, R_DH, R_BH,
    R_AX, R_CX, R_DX, R_BX, R_SP, R_BP, R_SI, R_DI,
    R_ES, R_CS, R_SS, R_DS,
    R_INVALID
};

const char RegNames[][3] = {
    "AL", "CL", "DL", "BL", "AH", "CH", "DH", "BH",
    "AX", "CX", "DX", "BX", "SP", "BP", "SI", "DI",
    "ES", "CS", "SS", "DS",
};

#ifdef _MSC_VER
__declspec(noreturn)
#endif
void Error(const char* msg)
{
    fprintf(stderr, "Line %u: %s (Current token: \"%s\")\n", CurrentLine, msg, TokenText);
#if 0
    for (int i = 0; i < OutputOffset; ++i) printf("%02X ", OutputBuffer[i]);
#endif
    exit(1);
}

void OutputByte(U1 b)
{
    if (OutputOffset == OUTPUT_MAX) {
        Error("Output overflow");
    }
    OutputBuffer[OutputOffset++] = b;
    ++CurrentAddress;
}

void OutputWord(U2 w)
{
    OutputByte(w & 0xff);
    OutputByte(w >> 8);
}

struct Equ* FindEqu(void)
{
    for (int i = 0 ; i < NumEqus; ++i) {
        if (!strcmp(TokenText, Equs[i].Name)) {
            return &Equs[i];
        }
    }
    return NULL;
}

struct Equ* DefineEqu(void)
{
    if (FindEqu()) {
        printf("Equ: \"%s\"\n", TokenText);
        Error("Equ already defined");
    }
    if (NumEqus == EQU_MAX) {
        Error("Too many EQUs");
    }
    struct Equ* e = &Equs[NumEqus++];
    strcpy(e->Name, TokenText);
    e->Value = 0;
    return e;
}

void ReadNext(void)
{
    int c = fgetc(InputFile);
    if (c == EOF) {
        CurrentChar = 0;
        return;
    } else if (c == '\n') {
        ++NumNewLines;
    }
    CurrentChar = (U1)c;
}

U1 GetChar(void)
{
    if (CurrentChar == 0) {
        Error("Unexpected EOF");
    }
    const U1 c = CurrentChar;
    ReadNext();
    return c;
}

bool TryGet(U1 ch)
{
    if (CurrentChar == ch) {
        ReadNext();
        return true;
    }
    return false;
}

void SkipWS(void)
{
    for (;;) {
        while (CurrentChar && CurrentChar <= ' ') {
            ReadNext();
        }
        if (CurrentChar != ';' ) {
            return;
        }
        do {
            ReadNext();
        } while (CurrentChar && CurrentChar != '\n');
    }
}

void MoveNext(void)
{
    ReadNext();
    SkipWS();
}

bool IsDigit(U1 ch)
{
    return ch >= '0' && ch <= '9';
}

bool IsAlpha(U1 ch)
{
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

U1 ToUpper(U1 ch)
{
    return ch >= 'a' && ch <= 'z' ? ch + 'A' - 'a' : ch;
}

bool IsTokenNumber(void)
{
    return IsDigit(TokenText[0]);
}

void GetToken(void)
{
    TokenLen = 0;
    while (CurrentChar == '.' || CurrentChar == '_' || IsDigit(CurrentChar) || IsAlpha(CurrentChar)) {
        if (TokenLen < TOKEN_MAX) {
            TokenText[TokenLen++] = ToUpper(CurrentChar);
        }
        ReadNext();
    }
    TokenText[TokenLen] = '\0';
    SkipWS();
    const struct Equ* e = FindEqu();
    if (e) {
        TokenLen = (U1)sprintf(TokenText, "0X%X", e->Value);
    }
}

bool TryConsume(U1 ch)
{
    if (CurrentChar != ch) {
        return false;
    }
    MoveNext();
    return true;
}

void Expect(U1 ch) {
    if (!TryConsume(ch)) {
        char err[] = "? expected";
        err[0] = ch;
        Error(err);
    }
}

U2 GetNumberFromToken(void)
{
    U2 num = 0;
    if (TokenLen > 2 && TokenText[0] == '0' && TokenText[1] == 'X') {
        if (TokenLen > 6) {
            Error("Hexnumber too large");
        }
        for (unsigned i = 2; i < TokenLen; ++i) {
            num <<= 4;
            U1 ch = TokenText[i];
            if (ch >= '0' && ch <= '9') {
                num |= ch - '0';
            } else if (ch >= 'A' && ch <= 'F') {
                num |= (ch - 'A') + 10;
            } else {
                Error("Invalid hex digit in number");
            }
        }
    } else {
        for (int i = 0; i < TokenLen; ++i) {
            const U1 val = TokenText[i] - '0';
            const U2 nn = num * 10 + val;
            if (val > 9 || nn < num) Error("Invalid decimal number");
            num = nn;
        }
    }
    return num;
}

U2 GetNumber(void)
{
    GetToken();
    if (!TokenLen) {
        Error("Invalid number");
    }
    return GetNumberFromToken();
}

struct Label* FindLabel(const char* text)
{
    for (int i = 0; i < NumLabels; ++i) {
        if (!strcmp(Labels[i].Name, text)) {
            return &Labels[i];
        }
    }
    return NULL;
}

void RetireLabel(U2 index)
{
    assert(index < NumLabels);
    //printf("Retiring %s\n", Labels[index].Name);
    if (Labels[index].Address == INVALID_ADDR) {
        printf("Label: \"%s\"\n", Labels[index].Name);
        Error("Undefined label");
    }
    assert(Labels[index].Fixup == INVALID_ADDR);
    if (index < NumLabels-1) {
        Labels[index] = Labels[NumLabels-1];
    }
    --NumLabels;
}

struct Label* NewLabel(void)
{
    assert(!FindLabel(TokenText));
    if (NumLabels == LABEL_MAX) {
        Error("Too many labels");
    }
    struct Label* l = &Labels[NumLabels++];
    strcpy(l->Name, TokenText);
    l->Address = INVALID_ADDR;
    l->Fixup = INVALID_ADDR;
    return l;
}

struct Label* FindOrMakeLabel(void)
{
    struct Label* l = FindLabel(TokenText);
    return l ? l :  NewLabel();
}

void AddFixup(struct Label* l)
{
    assert(l);
    if (FreeFixup == INVALID_ADDR) {
        Error("Too many fixups");
    }
    if (CurrentFixup) {
        Error("Too many fixups for instruction");
    }
    //printf("Adding fixup for %s\n", l->Name);
    const U2 idx = FreeFixup;
    CurrentFixup = &Fixups[idx];
    FreeFixup = CurrentFixup->Next;
    CurrentFixup->Next = l->Fixup;
    l->Fixup = idx;
}

bool IsShort(U2 num)
{
    return (S2)num <= 127 && (S2)num >= -128;
}

void AddU2(U1* d, U2 a)
{
    const U2 x = (d[0]|d[1]<<8)+a;
    d[0] = x & 0xff;
    d[1] = x >> 8;
}

void ResolveFixups(struct Label* l)
{
    assert(l->Address != INVALID_ADDR);
    U2 lastidx = INVALID_ADDR;
    for (U2 idx = l->Fixup; idx != INVALID_ADDR;) {
        struct Fixup* f = &Fixups[idx];
        assert(f->Address < OutputOffset);
        AddU2(&OutputBuffer[f->Address], l->Address);
        lastidx = idx;
        idx = f->Next;
    }
    if (lastidx != INVALID_ADDR) {
        assert(Fixups[lastidx].Next == INVALID_ADDR && l->Fixup != INVALID_ADDR);
        Fixups[lastidx].Next = FreeFixup;
        FreeFixup = l->Fixup;
        l->Fixup = INVALID_ADDR;
    }
}

void SwapFixup(void)
{
    struct Fixup* temp = CurrentFixup;
    CurrentFixup = CurrentLFixup;
    CurrentLFixup = temp;
}

void FixupIsHere(void)
{
    if (!CurrentFixup) {
        Error("No fixup active?");
    }
    CurrentFixup->Address = OutputOffset;
    CurrentFixup = NULL;
}

void DefineLabel(void)
{
    if (TokenText[0] != '.') {
        // Retire local labels
        for (U2 index = 0; index < NumLabels;) {
            if (Labels[index].Name[0] == '.') {
                RetireLabel(index);
            } else {
                ++index;
            }
        }
    }

    struct Label* l = FindLabel(TokenText);
    if (l) {
        if (l->Address != INVALID_ADDR) {
            Error("Duplicate label");
        }
        l->Address = CurrentAddress;
        ResolveFixups(l);
    } else {
        l = NewLabel();
        l->Address = CurrentAddress;
    }
}

void PrintOperand(bool alt)
{
    const U1 t = (U1)(alt ? OperandLType : OperandType);
    const U2 v = alt ? OperandLValue : OperandValue;
    if (t < OP_REG) {
        printf("[MODRM=%02X 0x%04X]", t, v);
        return;
    }

    switch (t) {
    case OP_REG:
        assert(v < R_INVALID);
        printf("%s", RegNames[v]);
        break;
    case OP_LIT:
        printf("0x%04X", v);
        break;
    default:
        Error("Invalid opreand type");
    }
}

void PrintInstr(const char* name, bool has2Operands)
{
    printf("%s ", name);
    if (has2Operands) {
        PrintOperand(true);
        printf(", ");
    }
    PrintOperand(false);
    printf("\n");
}

void GetReg(void)
{
    OperandValue = R_INVALID;
    for (U1 r = 0; r < sizeof(RegNames)/sizeof(*RegNames); ++r) {
        if (!strcmp(TokenText, RegNames[r])) {
            OperandValue = r;
            break;
        }
    }
}

void GetNamedLiteral(void)
{
    OperandType = OP_LIT;
    struct Label* l = FindOrMakeLabel();
    if (l->Address != INVALID_ADDR) {
        OperandValue = l->Address;
    } else {
        OperandValue = 0;
        AddFixup(l);
    }
}

U1 ModrmFromReg(void)
{
    switch (OperandValue) {
    case R_SI:
        return 4;
    case R_DI:
        return 5;
    case R_BP:
        return 6; // NOTE: Not legal on its own
    case R_BX:
        return 7;
    }
    return 0xFF;
}

U1 CombineModrmWithReg(U1 ModRM)
{
    if (ModRM == 0xFF) {
        return ModrmFromReg();
    }

    switch (OperandValue) {
    case R_SI:
        if (ModRM == 6) { // BP
            return 2;
        } else if (ModRM == 7) { // BX
            return 0;
        }
        break;
    case R_DI:
        if (ModRM == 6) { // BP
            return 3;
        } else if (ModRM == 7) { // BX
            return 1;
        }
        break;
    case R_BP:
        if (ModRM == 4) { // SI
            return 2;
        } else if (ModRM == 5) { // DI
            return 3;
        }
        break;
    case R_BX:
        if (ModRM == 4) { // SI
            return 0;
        } else if (ModRM == 5) { // DI
            return 1;
        }
        break;
    }
    return 0xFF;
}

bool GetRegOrNumber(void)
{
    GetToken();
    if (IsTokenNumber()) {
        OperandType  = OP_LIT;
        OperandValue = GetNumberFromToken();
        return true;
    }
    GetReg();
    if (OperandValue != R_INVALID) {
        OperandType = OP_REG;
        return true;
    }
    return false;
}

void GetOperandMem(void)
{
    assert(!CurrentFixup);

    Expect('[');

    U1 ModRM = 0xFF;
    U2 Disp  = 0;

    do {
    redo:
        if (GetRegOrNumber()) {
            if (OperandType == OP_LIT) {
                Disp += OperandValue;
            } else {
                assert(OperandType == OP_REG);
                if (OperandValue >= R_ES) {
                    // Segmnet override
                    assert(OperandValue <= R_DS);
                    OutputByte(0x26 | (U1)(OperandValue-R_ES)<<3);
                    Expect(':');
                    goto redo;
                }

                ModRM = CombineModrmWithReg(ModRM);
                if (ModRM == 0xFF) {
                    Error("Invalid register combination for memory operand");
                }
            }
        } else {
            GetNamedLiteral();
            Disp += OperandValue;
        }
    } while (TryConsume('+'));
    Expect(']');

    if (ModRM == 6 && !Disp) {
        ModRM |= 0x40;
    }

    if (ModRM == 0xFF) {
        ModRM = 6;
    } else if (CurrentFixup) {
        ModRM |= 0x80; // Disp16
    } else if (Disp) {
        if (IsShort(Disp)) {
            ModRM |= 0x40; // Disp8
        } else {
            ModRM |= 0x80; // Disp16
        }
    }

    OperandType = ModRM;
    OperandValue = Disp;
}

void GetCharLit(void)
{
    OperandValue = GetChar();
    if (!TryConsume('\'')) {
        OperandValue |= GetChar() << 8;
        Expect('\'');
    }
}

void GetOperand(void)
{
    if (TryGet('\'')) {
        OperandType = OP_LIT;
        GetCharLit();
    } else if (CurrentChar == '[') {
        GetOperandMem();
    } else {
        if (GetRegOrNumber()) {
            return;
        }
        if (!strcmp(TokenText, "BYTE") || !strcmp(TokenText, "WORD")) {
            assert(ExplicitSize == 0xFF);
            ExplicitSize = TokenText[0] == 'W';
            GetOperandMem();
            return;
        }

        GetNamedLiteral();
    }
}

void MoveToOperandL(void)
{
    OperandLType  = OperandType;
    OperandLValue = OperandValue;
    CurrentLFixup = CurrentFixup;
    CurrentFixup = NULL;
}

void DirectiveOrg(U1 arg)
{
    (void)arg;
    const U2 org = GetNumber();
    if (org < CurrentAddress) {
        Error("Invalid ORG (moving backwards)");
    }
    CurrentAddress = org;
}

void OutputDx(U1 size, U2 val)
{
    OutputByte(val&0xff);
    if (size != 1) {
        assert(size == 2);
        OutputByte(val>>8);
    } else {
        assert((val>>8) == 0);
    }
}

void DirectiveDx(U1 size)
{
    do {
        if (TryGet('\'')) {
            while (!TryConsume('\'')) {
                const U1 c = GetChar();
                if (c < 0x20) {
                    Error("Unterminated literal");
                }
                OutputDx(1, c); // Always output one byte when in character literal mode
            }
        } else {
            GetToken();
            if (IsTokenNumber()) {
                OutputDx(size, GetNumberFromToken());
            } else {
                if (size != 2) Error("Byte val reference not implemented");
                GetNamedLiteral();
                OutputDx(size, OperandValue);
            }
        }
    } while (TryConsume(','));
}

void InstINT(U1 arg)
{
    (void)arg;
    const U2 i = GetNumber();
    if (i > 0xff) {
        Error("Interrupt no. out of range");
    }
    OutputByte(0xCD);
    OutputByte(i&0xff);
}

void Get2Operands(void)
{
    GetOperand();
    MoveToOperandL();
    Expect(',');
    GetOperand();
}

void OutputImm16(void)
{
    if (CurrentFixup) {
        FixupIsHere();
    }
    OutputWord(OperandValue);
}

void OutputImm8(void)
{
    if (!IsShort(OperandValue) && OperandValue > 0xFF) {
        Error("8-bit immediate out of range");
    }
    OutputByte(OperandValue & 0xff);
}

void OutputImm(bool is16bit)
{
    if (is16bit) {
        OutputImm16();
    } else {
        OutputImm8();
    }
}

// Assumes left side operand is mem
void OutputModRM(U1 r)
{
    assert(r < 8 && OperandLType < OP_REG);
    OutputByte((U1)(OperandLType | (r<<3))); // ModRM
    if (OperandLType == 6 || (OperandLType & 0xc0) == 0x80) {
        if (CurrentLFixup) {
            SwapFixup();
            FixupIsHere();
            SwapFixup();
        }
        // Disp16
        OutputWord(OperandLValue);
    } else if ((OperandLType & 0xc0) == 0x40) {
        // Disp8
        OutputByte(OperandLValue&0xff);
    }
}

void OutputRR(U1 inst)
{
    if (!!(OperandLValue/8) != !!(OperandValue/8)) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandValue/8 == 1 ? 1 : 0)); // 16 or 8-bit?
    OutputByte(0xc0 | (OperandLValue&7) | (OperandValue&7)<<3);
}

void OutputMR(U1 inst)
{
    if (ExplicitSize != 0xFF && ExplicitSize != OperandValue/8) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandValue/8 ? 1 : 0)); // 16 or 8-bit?
    OutputModRM(OperandValue&7);
}

void SwapOperands(void)
{
    U1 tempT = OperandLType;
    U2 tempV = OperandLValue;
    OperandLType = OperandType;
    OperandLValue = OperandValue;
    OperandType = tempT;
    OperandValue = tempV;
    SwapFixup();
}

void OutputRM(U1 inst)
{
    SwapOperands();
    OutputMR(inst);
}

void OutputMImm(U1 inst, U1 r)
{
    if (ExplicitSize == 0xFF) {
        Error("Unknown operand size");
    }
    OutputByte(inst | ExplicitSize);   // Opcode
    OutputModRM(r);
    OutputImm(ExplicitSize);           // Immediate
}

void InstMOV(U1 arg)
{
    (void)arg;
    Get2Operands();
    // TODO: Use optimized opcodes when moving to from AL/AX

    if (OperandLType == OP_REG) {
        // LHS is register
        if (OperandType == OP_REG) {
            // MOV reg, reg
            if (OperandLValue < R_ES) {
                // LHS isn't an Sreg
                if (OperandValue < R_ES) {
                    // mov reg, reg
                    OutputRR(0x88);
                } else {
                    // Mov reg, sreg
                    if (OperandLValue < R_AX || OperandLValue > R_DI) {
                        goto Invalid;
                    }
                    OutputRR(0x8c);
                }
            } else {
                if (OperandValue < R_AX || OperandValue >= R_ES) {
                    goto Invalid;
                }
                SwapOperands();
                OutputRR(0x8e);
            }
        } else  if (OperandType == OP_LIT) {
            // MOV reg, imm
            if (OperandLValue >= R_ES) {
                goto Invalid;
            }
            OutputByte((U1)(0xB0 + OperandLValue));
            OutputImm(OperandLValue >= R_AX);
        } else {
            assert(OperandValue < OP_REG);
            // MOV reg, mem
            if (OperandLValue >= R_ES) {
                Error("TODO: 0x8E in MOV Sreg, mem16");
            }
            OutputRM(0x8A);
        }
    } else if (OperandLType < OP_REG) {
        // LHS is memory
        if (OperandType == OP_REG) {
            // MOV mem, reg
            if (OperandValue >= R_ES) {
                Error("Not implemented: MOV mem, Sreg");
            }
            OutputMR(0x88);
        } else if (OperandType == OP_LIT) {
            // mov mem, lit
            OutputMImm(0xC6, 0);
        } else {
            goto Invalid;
        }
    } else {
Invalid:
        PrintInstr("MOV", 2);
        Error("Invalid instruction");
    }
}

void InstMOVXX(U1 op2)
{
    Get2Operands();
    if (OperandLType == OP_REG && OperandLValue/8 == 1) {
        OutputByte(0x0F);
        OutputByte(op2);
        if (OperandType < OP_REG) {
            SwapOperands();
            OutputModRM(OperandValue & 7);
            return;
        } else if (OperandType == OP_REG) {
            OutputByte(0xc0 | (OperandLValue&7)<<3 | (OperandValue&7));
            return;
        }
    }
    Error("Invalid/unsupported operands to MOVZX");
}

void InstXCHG(U1 arg)
{
    (void)arg;
    Get2Operands();
    // TODO: Could use 0x90+r if either operand is AX
    if (OperandLType == OP_REG) {
        if (OperandType == OP_REG) {
            OutputRR(0x86);
            return;
        } else if (OperandType < OP_REG) {
            SwapOperands();
            goto common;
        }
    } else if (OperandLType < OP_REG && OperandType == OP_REG) {
    common:
        OutputByte(0x86 | (OperandValue/8==1?1:0));
        OutputModRM(OperandValue&7);
        return;
    }
    Error("Invalid/unsupported operands to XCHG");
}

void InstIncDec(U1 dec)
{
    assert(dec == 0 || dec == 1);
    GetOperand();
    if (OperandType == OP_REG) {
        if (OperandValue / 8 == 0) {
            OutputByte(0xFE);
            OutputByte(0xC0 | (dec<<3) | (OperandValue&7));
            return;
        } else if (OperandValue / 8 == 1) {
            OutputByte(0x40 | (dec<<3) | (OperandValue&7));
            return;
        }
    } else if (OperandType < OP_REG) {
        if (ExplicitSize == 0xFF) {
            Error("Operand size not specified");
        }
        OutputByte(0xFE | ExplicitSize);
        SwapOperands();
        OutputModRM(dec);
        return;
    }

    PrintInstr(dec?"DEC":"INC", false);
    Error("TODO");
}

void InstALU(U1 base)
{
    assert(((base & 7) | (base >> 6)) == 0);
    Get2Operands();
    if (OperandLType == OP_REG) {
        if (OperandType == OP_REG) {
            OutputRR(base);
            return;
        } else if (OperandType < OP_REG) {
            OutputRM(base + 2);
            return;
        } else if (OperandType == OP_LIT) {
            if (OperandLValue == R_AL) {
                OutputByte(base + 4);
                OutputImm8();
                return;
            } else if (OperandLValue == R_AX) {
                OutputByte(base + 5);
                OutputImm16();
                return;
            } else {
                const bool is16bit = !!(OperandLValue/8);
                OutputByte(0x80 | is16bit);
                OutputByte(0xC0 | (OperandLValue&7) | base);
                OutputImm(is16bit);
                return;
            }
        }
    } else if (OperandLType < OP_REG) {
        if (OperandType == OP_REG) {
            OutputMR(base);
            return;
        } else if (OperandType == OP_LIT) {
            OutputMImm(0x80, base >> 3);
            return;
        }
    }
    PrintInstr("ALU", true);
    Error("Not implemented ALU with non-reg arguments");
}

void InstROT(U1 r)
{
    assert(r < 8);
    Get2Operands();
    if (OperandType == OP_REG) {
        if (OperandValue == R_CL) {
            if (OperandLType == OP_REG) {
                const bool is16bit = OperandLValue/8==1;
                OutputByte(0xd2 | (is16bit?1:0));
                OutputByte(0xc0 | (r<<3) | (OperandLValue&7));
            } else {
                if (ExplicitSize == 0xFF) {
                    Error("Operand size not specified");
                }
                OutputByte(0xd2 | ExplicitSize);
                OutputModRM(r);
            }
            return;
        }
    } else if (OperandLType == OP_REG && OperandType == OP_LIT) {
        const bool is16bit = OperandLValue/8==1;
        OutputByte(0xc0 | (is16bit?1:0));
        OutputByte(0xc0 | (r<<3) | (OperandLValue&7));
        OutputImm8();
        return;
    }
    PrintInstr("ROT", 2);
    Error("Not implemented");
}

void InstMulDiv(U1 r)
{
    assert(r >= 4 && r < 8);
    GetOperand();
    if (OperandType != OP_REG || OperandValue >= R_ES) {
        Error("Invalid operands for mul/div");
    }
    OutputByte(0xF6 | (OperandValue/8==1));
    OutputByte(0xC0 | (r<<3) | (OperandValue&7));
}

void InstPUSH(U1 arg)
{
    (void)arg;
    GetOperand();
    if (OperandType == OP_REG) {
        if (OperandValue < R_AX) {
            Error("Cannot push 8-bit register");
        }
        if (OperandValue >= R_ES) {
            assert(OperandValue <= R_DS);
            OutputByte((U1)(0x06 | (OperandValue-R_ES)<<3));
        } else {
            OutputByte(0x50 | (OperandValue & 7));
        }
    } else if (OperandType == OP_LIT) {
        if (!IsShort(OperandValue) || CurrentFixup) {
            OutputByte(0x68);
            OutputImm16();
        } else {
            OutputByte(0x6A);
            OutputImm8();
        }
    } else {
        Error("Invalid / unsupported argument to PUSH");
    }
}

void InstPOP(U1 arg)
{
    (void)arg;
    GetOperand();
    if (OperandType != OP_REG || OperandValue < R_AX) {
        Error("Invalid/unsupported POP");
    }
    if (OperandValue >= R_ES) {
        assert(OperandValue <= R_DS && OperandValue != R_CS);
        OutputByte((U1)(0x07 | (OperandValue-R_ES)<<3));
    } else {
        OutputByte((U1)(0x58 | (OperandValue & 7)));
    }
}

void HandleRel16(void)
{
    if (OperandType != OP_LIT) {
        Error("Expected literal");
    }
    if (CurrentFixup) {
        FixupIsHere();
    }
    OutputWord((U2)(OperandValue - (CurrentAddress + 2)));
}

bool HandleShortRel(U1 inst)
{
    if (CurrentFixup || OperandType != OP_LIT) {
        return false;
    }
    const U2 rel = OperandValue - (CurrentAddress + 2);
    if (!IsShort(rel)) {
        return false;
    }
    OutputByte(inst);
    OutputByte(rel&0xff);
    return true;
}

void InstCALL(U1 arg)
{
    (void)arg;
    GetOperand();
    if (OperandType == OP_REG) {
        if (OperandValue/8 != 1) {
            Error("Invalid register operand to CALL");
        }
        OutputByte(0xFF);
        OutputByte(0xC0 | (2<<3) | (OperandValue&7));
        return;
    }
    OutputByte(0xE8);
    HandleRel16();
}

void InstJMP(U1 arg)
{
    (void)arg;
    GetOperand();
    if (!HandleShortRel(0xEB)) {
        OutputByte(0xE9);
        HandleRel16();
    }
}

void HandleJcc(U1 cc) {
    assert(cc < 16);
    GetOperand();
    if (!HandleShortRel(0x70|cc)){
        OutputWord(0x800F | cc<<8);
        HandleRel16();
    }
}

#define JO   0x0
#define JNO  0x1
#define JC   0x2
#define JNC  0x3
#define JZ   0x4
#define JNZ  0x5
#define JNA  0x6
#define JA   0x7
#define JS   0x8
#define JNS  0x9
#define JPE  0xa
#define JPO  0xb
#define JL   0xc
#define JNL  0xd
#define JNG  0xe
#define JG   0xf

static const struct {
    const char text[6];
    void (*func)(U1);
    U1 arg;
} DispatchList[] = {
//  { "12345" , &Directive12345 , 0x12 }
    { "ORG"   , &DirectiveOrg   , 0x00 },
    { "DB"    , &DirectiveDx    , 0x01 },
    { "DW"    , &DirectiveDx    , 0x02 },
    { "REP"   , &OutputByte     , 0xF3 },
    { "MOV"   , &InstMOV        , 0x00 },
    { "MOVZX" , &InstMOVXX      , 0xB6 },
    { "MOVSX" , &InstMOVXX      , 0xBE },
    { "XCHG"  , &InstXCHG       , 0x00 },
    { "INC"   , &InstIncDec     , 0x00 },
    { "DEC"   , &InstIncDec     , 0x01 },
    { "ADD"   , &InstALU        , 0x00 },
    { "OR"    , &InstALU        , 0x08 },
    { "ADC"   , &InstALU        , 0x10 },
    { "SBB"   , &InstALU        , 0x18 },
    { "AND"   , &InstALU        , 0x20 },
    { "SUB"   , &InstALU        , 0x28 },
    { "XOR"   , &InstALU        , 0x30 },
    { "CMP"   , &InstALU        , 0x38 },
    { "ROL"   , &InstROT        , 0x00 },
    { "ROR"   , &InstROT        , 0x01 },
    { "RCL"   , &InstROT        , 0x02 },
    { "RCR"   , &InstROT        , 0x03 },
    { "SHL"   , &InstROT        , 0x04 },
    { "SHR"   , &InstROT        , 0x05 },
    { "SAR"   , &InstROT        , 0x07 },
    { "MUL"   , &InstMulDiv     , 0x04 },
    { "IMUL"  , &InstMulDiv     , 0x05 },
    { "DIV"   , &InstMulDiv     , 0x06 },
    { "IDIV"  , &InstMulDiv     , 0x07 },
    { "INT"   , &InstINT        , 0x00 },
    { "RET"   , &OutputByte     , 0xC3 },
    { "RETF"  , &OutputByte     , 0xCB },
    { "IRET"  , &OutputByte     , 0xCF },
    { "NOP"   , &OutputByte     , 0x90 },
    { "PUSHA" , &OutputByte     , 0x60 },
    { "POPA"  , &OutputByte     , 0x61 },
    { "PUSH"  , &InstPUSH       , 0x00 },
    { "POP"   , &InstPOP        , 0x00 },
    { "MOVSB" , &OutputByte     , 0xA4 },
    { "MOVSW" , &OutputByte     , 0xA5 },
    { "STOSB" , &OutputByte     , 0xAA },
    { "LODSB" , &OutputByte     , 0xAC },
    { "HLT"   , &OutputByte     , 0xF4 },
    { "CLC"   , &OutputByte     , 0xF8 },
    { "STC"   , &OutputByte     , 0xF9 },
    { "CLI"   , &OutputByte     , 0xFA },
    { "STI"   , &OutputByte     , 0xFB },
    { "CLD"   , &OutputByte     , 0xFC },
    { "STD"   , &OutputByte     , 0xFD },
    { "CALL"  , &InstCALL       , 0x00 },
    { "JMP"   , &InstJMP        , 0x00 },
    { "JO"    , &HandleJcc      , JO   },
    { "JNO"   , &HandleJcc      , JNO  },
    { "JC"    , &HandleJcc      , JC   },
    { "JB"    , &HandleJcc      , JC   },
    { "JNC"   , &HandleJcc      , JNC  },
    { "JNB"   , &HandleJcc      , JNC  },
    { "JAE"   , &HandleJcc      , JNC  },
    { "JZ"    , &HandleJcc      , JZ   },
    { "JE"    , &HandleJcc      , JZ   },
    { "JNZ"   , &HandleJcc      , JNZ  },
    { "JNE"   , &HandleJcc      , JNZ  },
    { "JNA"   , &HandleJcc      , JNA  },
    { "JBE"   , &HandleJcc      , JNA  },
    { "JA"    , &HandleJcc      , JA   },
    { "JS"    , &HandleJcc      , JS   },
    { "JNS"   , &HandleJcc      , JNS  },
    { "JPE"   , &HandleJcc      , JPE  },
    { "JPO"   , &HandleJcc      , JPO  },
    { "JL"    , &HandleJcc      , JL   },
    { "JNL"   , &HandleJcc      , JNL  },
    { "JNG"   , &HandleJcc      , JNG  },
    { "JG"    , &HandleJcc      , JG   },
    };

bool TryGetU(U1 ch)
{
    assert(ch >= 'A' && ch <= 'Z');
    return TryGet(ch) || TryGet(ch + 'a' - 'A');
}

void Dispatch(void)
{
    if (TryConsume(':')) {
        DefineLabel();
        return;
    }
    ExplicitSize = 0xFF;
    for (unsigned i = 0; i < sizeof(DispatchList)/sizeof(*DispatchList); ++i) {
        if (!strcmp(TokenText, DispatchList[i].text)) {
            DispatchList[i].func(DispatchList[i].arg);
            if (CurrentLFixup || CurrentFixup) {
                Error("Fixup not handled");
            }
            return;
        }
    }

    if (!TryGetU('E') || !TryGetU('Q') || !TryGetU('U')) {
        Error("Invalid directive");
    }
    SkipWS();

    struct Equ* e = DefineEqu();
    if (TryGet('\'')) {
        GetCharLit();
        e->Value = OperandValue;
    } else {
        e->Value  = GetNumber();
    }
}

void ParserInit(const char* filename)
{
    if ((InputFile = fopen(filename, "rb")) == NULL) {
        fprintf(stderr, "Error opening %s\n", filename);
        exit(1);
    }
    for (U2 i = 0; i < FIXUP_MAX - 1; ++i) {
        Fixups[i].Next = i + 1;
    }
    Fixups[FIXUP_MAX-1].Next = INVALID_ADDR;
    FreeFixup = 0;
    CurrentLine = 1;
    MoveNext();
}

void ParserFini(void)
{
    fclose(InputFile);
    InputFile = NULL;
}

char* GetOutputFileName(const char* InputFileName)
{
    int l = strlen(InputFileName);
    char* OutputFileName = malloc(l + 4);
    if (!OutputFileName) {
        Error("Out of memory");
    }
    strcpy(OutputFileName, InputFileName);
    for (int i = l-1;;--i) {
        if (i == 0 || OutputFileName[i] == '\\' || OutputFileName[i] == '/') {
            strcpy(&OutputFileName[l], ".com");
            break;
        } else if (OutputFileName[i] == '.') {
            strcpy(&OutputFileName[i], ".com");
            break;
        }
    }
    return OutputFileName;
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
    Usage:
        fprintf(stderr, "Usage: %s input-file\n", argv[0]);
        return 1;
    }
    const char* InputFileName = NULL;
    char* OutputFileName = NULL;

    for (int i = 1; i < argc; ++i) {
        const char* a = argv[i];
        const bool HasNext = i + 1 < argc;
        if (a[0] == '-') {
            if (!strcmp(a, "-o")) {
                if (!HasNext) goto Usage;
                const char* OName = argv[++i];
                OutputFileName = malloc(strlen(OName)+1);
                if (!OutputFileName) Error("Out of memory");
                strcpy(OutputFileName,OName);
            } else {
                fprintf(stderr, "Invalid argument \"%s\"\n", a);
                return 1;
            }
        } else {
            if (InputFileName) goto Usage;
            InputFileName = a;
        }
    }
    if (!InputFileName) {
        goto Usage;
    }

    if (!OutputFileName) {
        OutputFileName = GetOutputFileName(InputFileName);
    }
    ParserInit(InputFileName);
    for (;;) {
        CurrentLine += NumNewLines;
        NumNewLines = 0;
        GetToken();
        if (!TokenLen) {
            break;
        }
        Dispatch();
    }
    // Make sure all labels have been defined
    while (NumLabels) {
        RetireLabel(0);
    }
    ParserFini();
#ifndef NDEBUG
    // Check for leaks
    int FixupFreeCnt = 0;
    for (int i = 0; i < FIXUP_MAX; ++i) {
        assert(FreeFixup < FIXUP_MAX);
        struct Fixup* f = &Fixups[FreeFixup];
        FreeFixup = f->Next;
        f->Next = INVALID_ADDR;
        ++FixupFreeCnt;
    }
    assert(FixupFreeCnt == FIXUP_MAX);
#endif
    printf("Writing %s\n", OutputFileName);
    FILE* OutputFile = fopen(OutputFileName, "wb");
    if (!OutputFile) {
        Error("Could not open output file");
    }
    fwrite(OutputBuffer, 1, OutputOffset, OutputFile);
    fclose(OutputFile);
    free(OutputFileName);
}
