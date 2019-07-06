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
#define LABEL_MAX 100
#define FIXUP_MAX 100
#define OUTPUT_MAX 0x1000
#define INVALID_ADDR 0xFFFF

FILE* InputFile;
U2 CurrentLine;
U2 NumNewLines;
U1 CurrentChar;
char TokenText[TOKEN_MAX+1];
U1 TokenLen;
U2 CurrentAddress;
U1 OutputBuffer[OUTPUT_MAX];
U2 OutputOffset;

struct Label {
    char Name[TOKEN_MAX+1];
    U2 Address;
    U2 Fixup;
};

struct Fixup {
    U2 Address;
    U2 Next;
};

struct Label Labels[LABEL_MAX];
struct Fixup Fixups[FIXUP_MAX];
U2 NumLabels;
U2 FreeFixup;
struct Fixup* CurrentFixup;

enum {
    OP_REG=0xc0,
    OP_LIT,
} OperandType, OperandLType;
U2 OperandValue, OperandLValue;
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
    for (int i = 0; i < OutputOffset; ++i) printf("%02X ", OutputBuffer[i]);
    exit(1);
}

void OutputByte(U1 b)
{
    if (b == OUTPUT_MAX) {
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

void GetToken(void)
{
    TokenLen = 0;
    while (CurrentChar == '.' || CurrentChar == ':' || (CurrentChar >= '0' && CurrentChar <= '9') || (CurrentChar >= 'A' && CurrentChar <= 'Z') || (CurrentChar >= 'a' && CurrentChar <= 'z')) {
        if (TokenLen < TOKEN_MAX) {
            TokenText[TokenLen++] = CurrentChar >= 'a' && CurrentChar <= 'z' ? CurrentChar + 'A' - 'a' : CurrentChar;
        }
        ReadNext();
    }
    TokenText[TokenLen] = '\0';
    SkipWS();
}

void ExpectComma(void)
{
    if (CurrentChar != ',') {
        Error("Comma expected");
    }
    MoveNext();
}

U2 GetNumber(void)
{
    GetToken();

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
    --TokenLen;
    TokenText[TokenLen] = '\0';
    //printf("Defining %s\n", TokenText);

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
    const U1 t = alt ? OperandLType : OperandType;
    const U1 v = alt ? OperandLValue : OperandValue;
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

void GetOperandMem(void)
{
    assert(CurrentChar == '[');
    MoveNext();

    U1 ModRM = 0xFF;
    U2 Disp  = 0;

    for (;;) {
        if (CurrentChar >= '0' && CurrentChar <= '9') {
            Disp += GetNumber();
        } else {
            GetToken();
            GetReg();
            if (OperandValue != R_INVALID) {
                if (ModRM == 0xFF) {
                    switch (OperandValue) {
                    case R_SI:
                        ModRM = 4;
                        goto Next;
                    case R_DI:
                        ModRM = 5;
                        goto Next;
                    case R_BP:
                        ModRM = 6; // NOTE: Not legal on its own
                        goto Next;
                    case R_BX:
                        ModRM = 7;
                        goto Next;
                    }
                } else {
                    switch (OperandValue) {
                    case R_SI:
                        if (ModRM == 6) { // BP
                            ModRM = 2;
                            goto Next;
                        } else if (ModRM == 7) { // BX
                            ModRM = 0;
                            goto Next;
                        }
                        break;
                    case R_DI:
                        if (ModRM == 6) { // BP
                            ModRM = 3;
                            goto Next;
                        } else if (ModRM == 7) { // BX
                            ModRM = 1;
                            goto Next;
                        }
                        break;
                    case R_BP:
                        if (ModRM == 4) { // SI
                            ModRM = 2;
                            goto Next;
                        } else if (ModRM == 5) { // DI
                            ModRM = 3;
                            goto Next;
                        }
                        break;
                    case R_BX:
                        if (ModRM == 4) { // SI
                            ModRM = 0;
                            goto Next;
                        } else if (ModRM == 5) { // DI
                            ModRM = 1;
                            goto Next;
                        }
                        break;
                    }
                }
                Error("Invalid register combination for memory operand");
            } else {
                GetNamedLiteral();
                Disp += OperandValue;
            }
        }
    Next:
        if (CurrentChar != '+' && CurrentChar != '-') {
            break;
        }
        if (CurrentChar == '-') {
            Error("Minus in memory operand not supported");
        }
        MoveNext();
    }
    if (CurrentChar != ']') {
        Error("Expected ]");
    }
    MoveNext();

    if (ModRM == 6 && !Disp) {
        ModRM |= 0x40;
    }

    if (ModRM == 0xFF) {
        ModRM = 6;
    } else if (Disp != 0) {
        if ((S2)Disp <= 127 && (S2)Disp >= -128) {
            ModRM |= 0x40; // Disp8
        } else {
            ModRM |= 0x80; // Disp16
        }
    }
    OperandType = ModRM;
    OperandValue = Disp;
}

void GetOperand(void)
{
    if (CurrentChar == '\'') {
        OperandType = OP_LIT;
        ReadNext();
        OperandValue = CurrentChar;
        ReadNext();
        if (CurrentChar != '\'') {
            OperandValue |= CurrentChar << 8;
            ReadNext();
        }
        if (CurrentChar != '\'') {
            Error("Invalid character literal");
        }
        MoveNext();
    } else if (CurrentChar >= '0' && CurrentChar <= '9') {
        OperandType  = OP_LIT;
        OperandValue = GetNumber();
    } else if (CurrentChar == '[') {
        GetOperandMem();
    } else {
        GetToken();
        GetReg();
        if (OperandValue != R_INVALID) {
            OperandType = OP_REG;
            return;
        }

        if (!strcmp(TokenText, "BYTE") || !strcmp(TokenText, "WORD")) {
            if (ExplicitSize != 0xFF) {
                Error("Explicit size specified more than once");
            }
            ExplicitSize = TokenText[0] == 'W';
            if (CurrentChar != '[') {
                Error("Expected [");
            }
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
    // TODO: CurrentFixup
}

void HandleRel16(void)
{
    GetOperand();
    if (OperandType != OP_LIT) {
        Error("Expected literal");
    }
    if (CurrentFixup) {
        FixupIsHere();
    }
    OutputWord((U2)(OperandValue - (CurrentAddress + 2)));
}

void DirectiveOrg(void)
{
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
    for (;;){
        if (CurrentChar == '\'') {
            ReadNext();
            while (CurrentChar != '\'') {
                if (CurrentChar < 0x20) {
                    Error("Unterminated literal");
                }
                OutputDx(size, CurrentChar);
                ReadNext();
            }
            MoveNext();
        } else if (CurrentChar >= '0' && CurrentChar <= '9') {
            OutputDx(size, GetNumber());
        } else {
            GetToken();
            Error("Not implemented in DirectiveDx");
        }
        if (CurrentChar != ',') {
            break;
        }
        MoveNext();
    }
}

void DirectiveDb(void)
{
    DirectiveDx(1);
}

void DirectiveDw(void)
{
    DirectiveDx(2);
}

void InstINT(void)
{
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
    ExpectComma();
    GetOperand();
}

void OutputImm16(void)
{
    OutputWord(OperandValue);
}

void OutputImm8(void)
{
    if ((U1)OperandValue > 255) {
        Error("8-bit immediate out of range");
    }
    OutputByte(OperandValue & 0xff);
}

void OutputImm(bool is16bit)
{
    if (CurrentFixup) {
        if (!is16bit) {
            Error("Invalid immediate with fixup");
        }
        FixupIsHere();
    }
    if (is16bit) {
        OutputImm16();
    } else {
        OutputImm8();
    }
}

void OutputModRM(U1 r)
{
    assert(r < 8);
    OutputByte(OperandLType | (r<<3)); // ModRM
    if (OperandLType == 6 || (OperandLType & 0xc0) == 0x80) {
        if (CurrentFixup) {
            // TODO: Handle mov [symbol1], symbol2
            FixupIsHere();
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
    if (OperandLValue/8 != OperandValue/8) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandLValue/8 ? 1 : 0)); // 16 or 8-bit?
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

void OutputRM(U1 inst)
{
    U1 tempT = OperandLType;
    U2 tempV = OperandLValue;
    OperandLType = OperandType;
    OperandLValue = OperandValue;
    OperandType = tempT;
    OperandValue = tempV;
    OutputMR(inst);
}

void InstMOV(void)
{
    Get2Operands();
    // TODO: Use optimized opcodes when moving to from AL/AX
    if (OperandLType == OP_REG && OperandType == OP_REG) {
        OutputRR(0x88);
        return;
    } else if (OperandLType < OP_REG && OperandType == OP_REG) {
        OutputMR(0x88);
        return;
    } else if (OperandLType == OP_REG && OperandType < OP_REG) {
        OutputRM(0x8A);
        return;
    } else if (OperandLType < OP_REG && OperandType == OP_LIT) {
        if (ExplicitSize == 0xFF) {
            Error("Unknown operand size");
        }
        OutputByte(0xC6 | ExplicitSize);   // Opcode
        OutputModRM(0);
        OutputImm(ExplicitSize);           // Immediate
        return;
    }
    if (OperandLType != OP_REG || OperandType != OP_LIT) {
        PrintInstr("MOV", true);
        Error("Not implemented: MOV <non-reg>, <non-lit>");
    }
    if (OperandLValue >= R_ES) {
        Error("Cannot move literal to sreg");
    }
    OutputByte(0xB0 + OperandLValue);
    OutputImm(OperandLValue >= R_AX);
}

void InstXCHG(void)
{
    Get2Operands();
    if (OperandLType == OP_REG && OperandType == OP_REG) {
        // TODO: Could use 0x90+r if either operand is AX
        OutputRR(0x86);
        return;
    }
    Error("Invalid/unsupported operands to XCHG");
}

void InstIncDec(bool dec)
{
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
    }

    PrintInstr(dec?"DEC":"INC", false);
    Error("TODO");
}

void InstINC(void)
{
    InstIncDec(0);
}

void InstDEC(void)
{
    InstIncDec(1);
}

void InstALU(U1 base)
{
    assert(((base & 7) | (base >> 6)) == 0);
    Get2Operands();
    if (OperandLType == OP_REG && OperandType == OP_REG) {
        OutputRR(base);
        return;
    } else if (OperandLType == OP_REG && OperandType == OP_LIT) {
        if (OperandLValue == R_AL) {
            OutputByte(base + 4);
            OutputImm8();
            return;
        } else if (OperandLValue == R_AX) {
            OutputByte(base + 5);
            OutputImm16();
            return;
        } else {
            const bool is16bit = !!(OperandLType/8);
            OutputByte(0x80 | is16bit);
            OutputByte(0xC0 | (OperandLValue&7) | base);
            OutputImm(is16bit);
            return;
        }
    }
    PrintInstr("ALU", true);
    Error("Not implemented ALU with non-reg arguments");
}

void InstADD(void) { InstALU(0x00); }
void InstOR(void)  { InstALU(0x08); }
void InstADC(void) { InstALU(0x10); }
void InstSBB(void) { InstALU(0x18); }
void InstAND(void) { InstALU(0x20); }
void InstSUB(void) { InstALU(0x28); }
void InstXOR(void) { InstALU(0x30); }
void InstCMP(void) { InstALU(0x38); }

void InstROL(void)
{
    Get2Operands();
    if (OperandLType == OP_REG && OperandType == OP_LIT) {
        const bool is16bit = OperandLValue/8==1;
        OutputByte(0xc0 | (is16bit?1:0));
        OutputByte(0xc0 | (0<<8) | (OperandLValue&7));
        OutputImm8();
        return;
    }
    Error("Not implemented: ROL");
}

void InstDIV(void)
{
    GetOperand();
    if (OperandType != OP_REG || OperandValue >= R_ES) {
        Error("Not implemented: DIV <non-small-reg>");
    }
    OutputByte(0xF6 | (OperandValue/8==1));
    OutputByte(0xC0 | (6<<3) | (OperandValue&7));
}

void InstMUL(void)
{
    Error("Not implemented: MUL");
}

void InstCALL(void)
{
    OutputByte(0xE8);
    HandleRel16();
}

void InstPUSHA(void)
{
    OutputByte(0x60);
}

void InstPOPA(void)
{
    OutputByte(0x61);
}

void InstPUSH(void)
{
    GetOperand();
    if (OperandType == OP_REG) {
        if (OperandValue < R_AX) {
            Error("Cannot push 8-bit register");
        }
        if (OperandValue >= R_ES) {
            Error("Not implemented: push s-reg");
        }
        OutputByte(0x50 | (OperandValue & 7));
    } else if (OperandType == OP_LIT) {
        if (OperandValue > 0xff) {
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

void InstPOP(void)
{
    GetOperand();
    if (OperandType != OP_REG || OperandValue < R_AX || OperandValue >= R_ES) {
        Error("Invalid/unsupported POP");
    }
    OutputByte(0x58 | (OperandValue & 7));
}

void InstRET(void)
{
    OutputByte(0xC3);
}

void InstLODSB(void)
{
    OutputByte(0xAC);
}

void InstSTOSB(void)
{
    OutputByte(0xAA);
}

void InstJMP(void)
{
    // TODO: Use EB for known short jumps
    OutputByte(0xE9);
    HandleRel16();
}

void HandleJcc(U1 cc) {
    // TODO: Use 0x70 | cc if known short jump
    assert(cc < 16);
    OutputWord(0x800F | cc<<8);
    HandleRel16();
}

void InstJO(void)   { HandleJcc(0x0); }
void InstJNO(void)  { HandleJcc(0x1); }
void InstJC(void)   { HandleJcc(0x2); }
void InstJNC(void)  { HandleJcc(0x3); }
void InstJZ(void)   { HandleJcc(0x4); }
void InstJNZ(void)  { HandleJcc(0x5); }
void InstJNA(void)  { HandleJcc(0x6); }
void InstJA(void)   { HandleJcc(0x7); }
void InstJS(void)   { HandleJcc(0x8); }
void InstJNS(void)  { HandleJcc(0x9); }
void InstJPE(void)  { HandleJcc(0xa); }
void InstJPO(void)  { HandleJcc(0xb); }
void InstJL(void)   { HandleJcc(0xc); }
void InstJNL(void)  { HandleJcc(0xd); }
void InstJNG(void)  { HandleJcc(0xe); }
void InstJG(void)   { HandleJcc(0xf); }

void InstIgnore0(void)
{
    printf("Ignoring %s\n", TokenText);
}

void InstIgnore1(void)
{
    printf("Ignoring %s ", TokenText);
    GetOperand();
    PrintOperand(false);
    printf("\n");
}

void InstIgnore2(void)
{
    printf("Ignoring %s ", TokenText);
    GetOperand();
    PrintOperand(false);
    ExpectComma();
    printf(", ");
    GetOperand();
    PrintOperand(false);
    printf("\n");
}

static const struct {
    const char* text;
    void (*func)(void);
} DispatchList[] = {
    { "ORG", &DirectiveOrg },
    { "DB", &DirectiveDb },
    { "DW", &DirectiveDw },

    { "MOV", &InstMOV },
    { "XCHG", &InstXCHG },

    { "INC", &InstINC },
    { "DEC", &InstDEC },

    { "ADD", &InstADD },
    { "OR" , &InstOR  },
    { "ADC", &InstADC },
    { "SBB", &InstSBB },
    { "AND", &InstAND },
    { "SUB", &InstSUB },
    { "XOR", &InstXOR },
    { "CMP", &InstCMP },

    { "ROL", &InstROL },

    { "DIV", &InstDIV },
    { "MUL", &InstMUL },

    { "CALL", &InstCALL },
    { "INT", &InstINT },
    { "RET", &InstRET },

    { "PUSHA", &InstPUSHA },
    { "POPA", &InstPOPA },
    { "PUSH", &InstPUSH },
    { "POP", &InstPOP },

    { "LODSB", &InstLODSB},
    { "STOSB", &InstSTOSB},

    { "JMP", &InstJMP },
    { "JO" , &InstJO  },
    { "JNO", &InstJNO },
    { "JC" , &InstJC  },
    { "JNC", &InstJNC },
    { "JZ" , &InstJZ  },
    { "JNZ", &InstJNZ },
    { "JNA", &InstJNA },
    { "JBE", &InstJNA },
    { "JA" , &InstJA  },
    { "JS" , &InstJS  },
    { "JNS", &InstJNS },
    { "JPE", &InstJPE },
    { "JPO", &InstJPO },
    { "JL" , &InstJL  },
    { "JNL", &InstJNL },
    { "JNG", &InstJNG },
    { "JG" , &InstJG  },
    
};

void Dispatch(void)
{
    if (TokenLen > 1 && TokenText[TokenLen-1] == ':') {
        DefineLabel();
        return;
    }
    ExplicitSize = 0xFF;
    for (unsigned i = 0; i < sizeof(DispatchList)/sizeof(*DispatchList); ++i) {
        if (!strcmp(TokenText, DispatchList[i].text)) {
            DispatchList[i].func();
            if (CurrentFixup) {
                Error("Fixup not handled");
            }
            return;
        }
    }
    Error("Invalid directive");
}

void ParserInit(const char* filename)
{
    if ((InputFile = fopen(filename, "rb")) == NULL) {
        fprintf(stderr, "Error opening %s\n", filename);
        exit(1);
    }
    for (int i = 0; i < FIXUP_MAX - 1; ++i) {
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

int main(int argc, char* argv[])
{
    if (argc < 2) {
        fprintf(stderr, "Usage: %s input-file\n", argv[0]);
        return 1;
    }
    ParserInit(argv[1]);
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
    // Chek for leaks
    int FixupFreeCnt = 0;
    for (int i = 0; i < FIXUP_MAX; ++i) {
        assert(FreeFixup < FIXUP_MAX);
        struct Fixup* f = &Fixups[FreeFixup];
        FreeFixup = f->Next;
        f->Next = INVALID_ADDR;
    }
#endif
    int l = strlen(argv[1]);
    char* outfname = malloc(l + 4);
    if (!outfname) {
        Error("Out of memory");
    }
    strcpy(outfname, argv[1]);
    for (int i = l-1;;--i) {
        if (i == 0 || outfname[i] == '\\' || outfname[i] == '/') {
            strcpy(&outfname[l], ".com");
            break;
        } else if (outfname[i] == '.') {
            strcpy(&outfname[i], ".com");
            break;
        }
    }
    printf("Writing %s\n", outfname);
    FILE* OutputFile = fopen(outfname, "wb");
    if (!OutputFile) {
        Error("Could not open output file");
    }
    fwrite(OutputBuffer, 1, OutputOffset, OutputFile);
    fclose(OutputFile);
    free(outfname);
}
