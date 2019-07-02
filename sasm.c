#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef unsigned char U1;
typedef unsigned short U2;

#define TOKEN_MAX 32
#define LABEL_MAX 100
#define FIXUP_MAX 100
#define OUTPUT_MAX 0x1000
#define INVALID_ADDR 0xFFFF

FILE* InputFile;
U2 CurrentLine;
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
} OperandType;
U2 OperandValue;

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

void ReadNext(void)
{
    int c = fgetc(InputFile);
    if (c == EOF) {
        CurrentChar = 0;
        return;
    } else if (c == '\n') {
        ++CurrentLine;
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
    printf("Retiring %s\n", Labels[index].Name);
    if (Labels[index].Address == INVALID_ADDR) {
        Error("Undefined label");
    }
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
    printf("Adding fixup for %s\n", l->Name);
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
    U2 lastidx = INVALID_ADDR;
    for (U2 idx = l->Fixup; idx != INVALID_ADDR;) {
        struct Fixup* f = &Fixups[idx];
        assert(f->Address < OutputOffset);
        AddU2(&OutputBuffer[f->Address], l->Address);
        lastidx = idx;
        idx = f->Next;
    }
    if (lastidx != INVALID_ADDR) {
        assert(Fixups[lastidx].Next == INVALID_ADDR);
        Fixups[lastidx].Next = FreeFixup;
        FreeFixup = lastidx;
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

void InstINT(void)
{
    const U2 i = GetNumber();
    if (i > 0xff) {
        Error("Interrupt no. out of range");
    }
    OutputByte(0xCD);
    OutputByte(i&0xff);
}

void InstMOV(void)
{
    GetOperand();
    ExpectComma();
    const U1 op1Type = OperandType;
    const U2 op1Val  = OperandValue;
    GetOperand();
    if (op1Type != OP_REG || OperandType != OP_LIT) {
        Error("Not implemented: MOV <non-reg>, <non-lit>");
    }
    if (op1Val >= R_ES) {
        Error("Cannot move literal to sreg");
    }
    OutputByte(0xB0 + op1Val);
    if (CurrentFixup) {
        if (op1Val < R_AX) {
            Error("Invalid dest reg. when fixup needed");
        }
        FixupIsHere();
    }
    OutputByte(OperandValue&0xff);
    if (op1Val >= R_AX) {
        OutputByte(OperandValue>>8);
    }
}

static const struct {
    const char* text;
    void (*func)(void);
} DispatchList[] = {
    { "ORG", &DirectiveOrg },
    { "DB", &DirectiveDb },
    { "DW", &DirectiveDw },

    { "CLC", &InstIgnore0 },
    { "STC", &InstIgnore0 },
    { "CALL", &InstIgnore1 },
    { "INT", &InstINT },
    { "RET", &InstIgnore0 },
    { "MOV", &InstMOV },
    { "MOVZX", &InstIgnore2 },
    { "INC", &InstIgnore1 },
    { "DEC", &InstIgnore1 },
    { "ADD", &InstIgnore2 },
    { "AND", &InstIgnore2 },
    { "CMP", &InstIgnore2 },
    { "SUB", &InstIgnore2 },
    { "OR",  &InstIgnore2 },
    { "XOR", &InstIgnore2 },
    { "XCHG", &InstIgnore2 },
    { "ROL", &InstIgnore2 },
    { "SHL", &InstIgnore2 },
    { "MUL", &InstIgnore1 },
    { "DIV", &InstIgnore1 },
    { "PUSHA", &InstIgnore0 },
    { "POPA", &InstIgnore0 },
    { "PUSH", &InstIgnore1 },
    { "POP", &InstIgnore1 },
    { "LODSB", &InstIgnore0 },
    { "STOSB", &InstIgnore0 },
    { "LOOP", &InstIgnore1 },
    { "JMP", &InstIgnore1 },
    { "JO" , &InstIgnore1 },
    { "JNO", &InstIgnore1 },
    { "JC" , &InstIgnore1 },
    { "JNC", &InstIgnore1 },
    { "JZ" , &InstIgnore1 },
    { "JNZ", &InstIgnore1 },
    { "JNA", &InstIgnore1 },
    { "JA" , &InstIgnore1 },
    { "JS" , &InstIgnore1 },
    { "JNS", &InstIgnore1 },
    { "JPE", &InstIgnore1 },
    { "JPO", &InstIgnore1 },
    { "JL" , &InstIgnore1 },
    { "JNL", &InstIgnore1 },
    { "JNG", &InstIgnore1 },
    { "JG" , &InstIgnore1 },
    { "JB" , &InstIgnore1 },
    { "JBE", &InstIgnore1 },
    { "JA" , &InstIgnore1 },
    { "JAE", &InstIgnore1 },
    { "JE" , &InstIgnore1 },
    { "JNE", &InstIgnore1 },

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
    CurrentLine = 1;
    MoveNext();
}

void ParserFini(void)
{
    fclose(InputFile);
    InputFile = NULL;
}

int main()
{
    for (int i = 0; i < FIXUP_MAX - 1; ++i) {
        Fixups[i].Next = i + 1;
    }
    Fixups[FIXUP_MAX-1].Next = INVALID_ADDR;
    ParserInit("../tests/t02.asm");
    for (;;) {
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
