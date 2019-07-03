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
    OP_REG,
    OP_LIT,
    OP_MEM,
} OperandType, OperandLType;
U2 OperandValue, OperandLValue;

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

void Error(const char* msg)
{
    fprintf(stderr, "Line %u: %s (Current token: \"%s\")\n", CurrentLine, msg, TokenText);
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

void PrintOperand(void)
{
    switch (OperandType) {
    case OP_REG:
        assert(OperandValue < R_INVALID);
        printf("%s", RegNames[OperandValue]);
        break;
    case OP_LIT:
        printf("0x%04X", OperandValue);
        break;
    case OP_MEM:
        printf("[0x%04X]", OperandValue);
        break;
    default:
        assert(0);
    }
}

void GetOperandMem(void)
{
    assert(CurrentChar == '[');
    OperandType = OP_MEM;
    MoveNext();
    GetToken();
    struct Label* l = FindOrMakeLabel();
    if (l->Address != INVALID_ADDR) {
        OperandValue = l->Address;
    } else {
        OperandValue = 0;
        AddFixup(l);
    }
    if (CurrentChar != ']') {
        Error("Expected ]");
    }
    MoveNext();
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

        for (U1 r = 0; r < sizeof(RegNames)/sizeof(*RegNames); ++r) {
            if (!strcmp(TokenText, RegNames[r])) {
                OperandType = OP_REG;
                OperandValue = r;
                return;
            }
        }

        if (!strcmp(TokenText, "BYTE") || !strcmp(TokenText, "WORD")) {
            if (CurrentChar != '[') {
                Error("Expected [");
            }
            GetOperandMem();
            return;
        }

        OperandType = OP_LIT;

        struct Label* l = FindOrMakeLabel();
        if (l->Address != INVALID_ADDR) {
            OperandValue = l->Address;
        } else {
            OperandValue = 0;
            AddFixup(l);
        }
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

#if 0
void InstIgnore0(void)
{
    printf("Ignoring %s\n", TokenText);
}

void InstIgnore1(void)
{
    printf("Ignoring %s ", TokenText);
    GetOperand();
    PrintOperand();
    printf("\n");
}

void InstIgnore2(void)
{
    printf("Ignoring %s ", TokenText);
    GetOperand();
    PrintOperand();
    ExpectComma();
    printf(", ");
    GetOperand();
    PrintOperand();
    printf("\n");
}
#endif

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

void OutputRR(U1 inst)
{
    if (OperandLValue/8 != OperandValue/8) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandLValue/8 ? 1 : 0)); // 16 or 8-bit?
    OutputByte(0xc0 | (OperandLValue&7) | (OperandValue&7)<<3);
}

void InstMOV(void)
{
    Get2Operands();
    if (OperandLType == OP_REG && OperandType == OP_REG) {
        OutputRR(0x88);
        return;
    }
    if (OperandLType != OP_REG || OperandType != OP_LIT) {
        Error("Not implemented: MOV <non-reg>, <non-lit>");
    }
    if (OperandLValue >= R_ES) {
        Error("Cannot move literal to sreg");
    }
    OutputByte(0xB0 + OperandLValue);
    OutputImm(OperandLValue >= R_AX);
}

void InstALU(U1 base)
{
    Get2Operands();
    if (OperandLType == OP_REG && OperandType == OP_REG) {
        OutputRR(base);
        return;
    } else if (OperandLType == OP_REG && OperandType == OP_LIT) {
        if (OperandLValue == R_AL) {
            OutputByte(base + 4);
            OutputImm8();
            return;
        }
    }
    Error("Not implemented AND with non-reg arguments");
}

void InstADD(void) { InstALU(0x00); }
void InstAND(void) { InstALU(0x20); }
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
    if (OperandType != OP_REG) {
        Error("Not implemented PUSH imm");
    }
    if (OperandValue < R_AX) {
        Error("Cannot push 8-bit register");
    }
    if (OperandValue >= R_ES) {
        Error("Not implemented: push s-reg");
    }
    OutputByte(0x50 | (OperandValue & 7));
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

static const struct {
    const char* text;
    void (*func)(void);
} DispatchList[] = {
    { "ORG", &DirectiveOrg },
    { "DB", &DirectiveDb },
    { "DW", &DirectiveDw },

    { "MOV", &InstMOV },
    { "ADD", &InstADD },
    { "AND", &InstAND },
    { "CMP", &InstCMP },

    { "ROL", &InstROL },

    { "CALL", &InstCALL },
    { "INT", &InstINT },
    { "RET", &InstRET },

    { "PUSHA", &InstPUSHA },
    { "POPA", &InstPOPA },
    { "PUSH", &InstPUSH },
    { "POP", &InstPOP },

    { "LODSB", &InstLODSB},

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
    FILE* OutputFile = fopen("out.com", "wb");
    if (!OutputFile) {
        Error("Could not open output file");
    }
    fwrite(OutputBuffer, 1, OutputOffset, OutputFile);
    fclose(OutputFile);
}
