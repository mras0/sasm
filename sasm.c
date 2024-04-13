#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <assert.h>

typedef signed char S1;
typedef unsigned char U1;
typedef unsigned short U2;
typedef short S2;

#define TOKEN_MAX 16
#define LABEL_MAX 300
#define FIXUP_MAX 600
#define EQU_MAX   100
#define OUTPUT_MAX 0x2000
#define IF_MAX    8 // Maximum nesting of %if directives

#define INVALID_ADDR 0xFFFF
#define NO_SIZE 0xFF // ExplicitSize when no size specified

#define FIXUP_DISP16 0
#define FIXUP_REL8   1
#define FIXUP_REL16  2

#define IF_ACTIVE     1 // %if/%elif/%else block currently active
#define IF_WAS_ACTIVE 2 // %if/%elif block was previously active
#define IF_SEEN_ELSE  4 // %else block seen

struct Label {
    char Name[TOKEN_MAX+1];
    U2 Address;
    U2 Fixup;
    U2 Line;     // Line where label was defined
    U2 UsedLine; // Where it was first used
};

struct Fixup {
    U2 Address;
    U2 Next;
    U2 Line;
    U1 Type;
};

struct Equ {
    char Name[TOKEN_MAX+1];
    U2 Value;
};

struct If {
    U1 State;
    U2 Line;
};

FILE* InputFile;
U2 CurrentLine;
U2 NumNewLines;
U1 CurrentChar;
bool GotNL;
U2 CurrentOp;
char TokenText[TOKEN_MAX+1];
char UTokenText[TOKEN_MAX+1];
bool IsTokenNumber;
U1 TokenLen;
U2 SectionStart;
U2 CurrentAddress;
U1 OutputBuffer[OUTPUT_MAX];
U2 OutputOffset;
U2 PendingZeros;
struct Label Labels[LABEL_MAX];
struct Fixup Fixups[FIXUP_MAX];
struct Equ   Equs[EQU_MAX];
struct If    Ifs[IF_MAX];
U2 NumLabels;
U2 FreeFixup;
U2 NumEqus;
U2 NumIfs;
bool WarnUnusedLabel;
bool WarnShort;

enum {
    OP_REG=0xc0,
    OP_LIT,
} OperandType, OperandLType;
U2 OperandValue, OperandLValue;
struct Fixup* CurrentFixup, * CurrentLFixup;
U1 ExplicitSize;
U1 CpuLevel = 3; // Assume 386

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
#elif defined(__GNUC__)
__attribute__((__noreturn__))
#endif
static void Error(const char* msg)
{
    fprintf(stderr, "Line %u: %s (Current token: \"%s\")\n", CurrentLine, msg, TokenText);
    exit(1);
}

static void CheckCPU(U1 level)
{
    if (level > CpuLevel) {
        Error("Instruction not supported at this CPU level");
    }
}

static void OutputByte(U1 b)
{
    if (OutputOffset + PendingZeros >= OUTPUT_MAX) {
        Error("Output overflow");
    }
    if (PendingZeros) {
        memset(OutputBuffer + OutputOffset, 0, PendingZeros);
        OutputOffset += PendingZeros;
        PendingZeros = 0;
    }
    OutputBuffer[OutputOffset++] = b;
    ++CurrentAddress;
}

static void OutputWord(U2 w)
{
    OutputByte(w & 0xff);
    OutputByte(w >> 8);
}

static struct Equ* FindEqu(void)
{
    for (int i = 0 ; i < NumEqus; ++i) {
        if (!strcmp(TokenText, Equs[i].Name)) {
            return &Equs[i];
        }
    }
    return NULL;
}

static struct Equ* DefineEqu(void)
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

static void ReadNext(void)
{
    int c = fgetc(InputFile);
    if (c == EOF) {
        CurrentChar = 0;
        return;
    } else if (c == '\n') {
        GotNL = true;
        ++NumNewLines;
    }
    CurrentChar = (U1)c;
}

static U1 GetChar(void)
{
    if (CurrentChar == 0) {
        Error("Unexpected EOF");
    }
    const U1 c = CurrentChar;
    ReadNext();
    return c;
}

static bool TryGet(U1 ch)
{
    if (CurrentChar == ch) {
        ReadNext();
        return true;
    }
    return false;
}

static void SkipWS(void)
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

static void MoveNext(void)
{
    ReadNext();
    SkipWS();
}

static bool IsDigit(U1 ch)
{
    return ch >= '0' && ch <= '9';
}

static bool IsAlpha(U1 ch)
{
    return (ch >= 'A' && ch <= 'Z') || (ch >= 'a' && ch <= 'z');
}

static U1 ToUpper(U1 ch)
{
    return ch >= 'a' && ch <= 'z' ? ch + 'A' - 'a' : ch;
}


static U2 GetNumberFromToken(void)
{
    U2 num = 0;
    const char* text;
    unsigned len;

    if (TokenLen > 2 && UTokenText[0] == '0' && UTokenText[1] == 'X') {
        len = TokenLen - 2;
        text = UTokenText + 2;
    HexVal:
        if (len > 4) {
            Error("Hexnumber too large");
        }
        while (len--) {
            num <<= 4;
            U1 ch = *text++;
            if (ch >= '0' && ch <= '9') {
                num |= ch - '0';
            } else if (ch >= 'A' && ch <= 'F') {
                num |= (ch - 'A') + 10;
            } else {
                Error("Invalid hex digit in number");
            }
        }
    } else if (UTokenText[0] == '0' && UTokenText[1] == 'H') {
        return 0;
    } else if (UTokenText[TokenLen - 1] == 'H') {
        len = TokenLen - 1;
        text = UTokenText;
        if (UTokenText[0] == '0') {
            ++text;
            --len;
        }
        goto HexVal;
    } else {
        for (int i = 0; i < TokenLen; ++i) {
            const U1 val = UTokenText[i] - '0';
            const U2 nn = num * 10 + val;
            if (val > 9 || nn < num) Error("Invalid decimal number");
            num = nn;
        }
    }
    return num;
}

static void GetToken(void)
{
    TokenLen = 0;
    while (CurrentChar == '.' || CurrentChar == '_' || CurrentChar == '$' || CurrentChar == '%'
        || IsDigit(CurrentChar) || IsAlpha(CurrentChar)) {
        if (TokenLen < TOKEN_MAX) {
            UTokenText[TokenLen] = ToUpper(CurrentChar);
            TokenText[TokenLen++] = CurrentChar;
        }
        ReadNext();
    }
    UTokenText[TokenLen] = '\0';
    TokenText[TokenLen] = '\0';
    SkipWS();

    IsTokenNumber = IsDigit(TokenText[0]);
    if (IsTokenNumber) {
        OperandValue = GetNumberFromToken();
    } else {
        if (!strcmp(TokenText, "$")) {
            IsTokenNumber = true;
            OperandValue = CurrentAddress;
        } else if (!strcmp(TokenText, "$$")) {
            IsTokenNumber = true;
            OperandValue = SectionStart;
        } else {
            const struct Equ* e = FindEqu();
            if (e) {
                IsTokenNumber = true;
                OperandValue = e->Value;
            }
        }
    }
}

static bool TryConsume(U1 ch)
{
    if (CurrentChar != ch) {
        return false;
    }
    MoveNext();
    return true;
}

static void Expect(U1 ch) {
    if (!TryConsume(ch)) {
        char err[] = "? expected";
        err[0] = ch;
        Error(err);
    }
}

static U2 GetNumber(void)
{
    GetToken();
    if (!IsTokenNumber) {
        Error("Invalid number");
    }
    return OperandValue;
}

static struct Label* FindLabel(const char* text)
{
    for (int i = 0; i < NumLabels; ++i) {
        if (!strcmp(Labels[i].Name, text)) {
            return &Labels[i];
        }
    }
    return NULL;
}

static void RetireLabel(U2 index)
{
    assert(index < NumLabels);
    //printf("Retiring %s\n", Labels[index].Name);
    if (!Labels[index].UsedLine) {
        if (WarnUnusedLabel) {
            printf("Line %u: Warning: Label \"%s\" unused\n", Labels[index].Line, Labels[index].Name);
        }
        assert(Labels[index].Address != INVALID_ADDR);
    } else if (Labels[index].Address == INVALID_ADDR) {
        printf("Line %u: Label: \"%s\"\n", Labels[index].UsedLine, Labels[index].Name);
        Error("Undefined label");
    }
    assert(Labels[index].Fixup == INVALID_ADDR);
    if (index < NumLabels-1) {
        Labels[index] = Labels[NumLabels-1];
    }
    --NumLabels;
}

static struct Label* NewLabel(void)
{
    assert(!FindLabel(TokenText));
    if (NumLabels == LABEL_MAX) {
        Error("Too many labels");
    }
    struct Label* l = &Labels[NumLabels++];
    strcpy(l->Name, TokenText);
    l->Address = INVALID_ADDR;
    l->Fixup = INVALID_ADDR;
    l->UsedLine = NumLabels == 1 ? CurrentLine : 0; // Mark first label as used to avoid useless warning
    l->Line = 0;
    return l;
}

static struct Label* FindOrMakeLabel(void)
{
    struct Label* l = FindLabel(TokenText);
    return l ? l :  NewLabel();
}

static void AddFixup(struct Label* l)
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

static bool IsShort(U2 num)
{
    return (S2)num <= 127 && (S2)num >= -128;
}

static void AddU2(U1* d, U2 a)
{
    const U2 x = (d[0]|d[1]<<8)+a;
    d[0] = x & 0xff;
    d[1] = x >> 8;
}

static void ResolveFixups(struct Label* l)
{
    assert(l->Address != INVALID_ADDR);
    U2 lastidx = INVALID_ADDR;
    for (U2 idx = l->Fixup; idx != INVALID_ADDR;) {
        struct Fixup* f = &Fixups[idx];
        assert(f->Address < OutputOffset);
        if (f->Type == FIXUP_DISP16 || f->Type == FIXUP_REL16) {
            AddU2(&OutputBuffer[f->Address], l->Address);
            if (WarnShort && f->Type == FIXUP_REL16 && IsShort(OutputOffset - (f->Address + 2))) {
                printf("Line %u: Warning: Could be short jump\n", f->Line);
            }
        } else {
            assert(f->Type == FIXUP_REL8);
            // Note: this only works because multiple ORG commands aren't allowed
            const U2 rel = OutputOffset - (f->Address + 1);
            if (rel > 0x7F) {
                printf("Line %u: 0x%04X - %04X (= %04X) > 0x7F for %s\n", f->Line, OutputOffset , f->Address + 1, rel, l->Name);
                Error("Relative jump out of range");
            }
            OutputBuffer[f->Address] = (U1)rel;
        }
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

static void SwapFixup(void)
{
    struct Fixup* temp = CurrentFixup;
    CurrentFixup = CurrentLFixup;
    CurrentLFixup = temp;
}

static void RegisterFixup(U1 type)
{
    if (!CurrentFixup) {
        Error("No fixup active?");
    }
    CurrentFixup->Address = OutputOffset;
    CurrentFixup->Type    = type;
    CurrentFixup->Line    = CurrentLine;
    CurrentFixup = NULL;
}

static void DefineLabel(void)
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
    l->Line = CurrentLine;
}

static void PrintOperand(bool alt)
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

static void PrintInstr(const char* name, bool has2Operands)
{
    printf("%s ", name);
    if (has2Operands) {
        PrintOperand(true);
        printf(", ");
    }
    PrintOperand(false);
    printf("\n");
}

static U1 GetReg(void)
{
    for (U1 r = 0; r < sizeof(RegNames)/sizeof(*RegNames); ++r) {
        if (!strcmp(UTokenText, RegNames[r])) {
            return r;
        }
    }
    return R_INVALID;
}

static bool GetRegOrNumber(void)
{
    GetToken();
    if (IsTokenNumber) {
        OperandType  = OP_LIT;
        return true;
    }
    OperandValue = GetReg();
    if (OperandValue != R_INVALID) {
        OperandType = OP_REG;
        return true;
    }
    return false;
}

static void GetNamedLiteral(void)
{
    OperandType = OP_LIT;
    struct Label* l = FindOrMakeLabel();
    if (!l->UsedLine) {
        l->UsedLine = CurrentLine;
    }
    if (l->Address != INVALID_ADDR) {
        OperandValue = l->Address;
    } else {
        OperandValue = 0;
        AddFixup(l);
    }
}

static U2 GetCharLit(void)
{
    U2 num = GetChar();
    if (!TryConsume('\'')) {
        num |= GetChar() << 8;
        Expect('\'');
    }
    return num;
}

static U2 GetExpr0(void);

static U2 GetPrimary(void)
{
    SkipWS();
    if (TryConsume('(')) {
        const U2 num = GetExpr0();
        Expect(')');
        return num;
    } else if (TryGet('\'')) {
        return GetCharLit();
    } else if (IsDigit(CurrentChar)) {
        return GetNumber();
    } else {
        GetToken();
        if (!IsTokenNumber) {
            GetNamedLiteral();
        }
        return OperandValue;
    }
}

#define OPER_LSH ('<'|0x80)
#define OPER_RSH ('>'|0x80)
#define OPER_LEQ 'L'
#define OPER_GEQ 'G'
#define OPER_EQ  '='
#define OPER_NEQ '!'
#define OPER_LAND ('&'|0x80)
#define OPER_LXOR ('^'|0x80)
#define OPER_LOR  ('|'|0x80)
#define MAX_PREC 0xFF

static U2 GetOp(void)
{
    SkipWS();
    if (GotNL) {
        return 0;
    }
    U1 prec = 0;
    U1 op = CurrentChar;
    switch (CurrentChar) {
    case '*': case '/': case '%':
        prec = 5;
        break;
    case '+': case '-':
        prec = 6;
        break;
    case '<': case '>':
        ReadNext();
        if (CurrentChar == op) {
            op |= 0x80;
            prec = 7;
            break;
        }
        prec = 9;
        if (CurrentChar != '=') {
            goto NoNext;
        }
        op = op == '<' ? OPER_LEQ : OPER_GEQ;
        break;
    case '=':
        ReadNext();
        if (CurrentChar != '=') {
            Error("Invalid operator");
        }
        prec = 10;
        break;
    case '!':
        ReadNext();
        if (CurrentChar != '=') {
            Error("Invalid operator");
        }
        prec = 10;
        break;
    case '&':
        prec = 11;
        break;
    case '^':
        prec = 12;
        break;
    case '|':
        prec = 13;
        break;
    default:
        return 0;
    }
    ReadNext();
    if (op == '&' || op == '^' || op == '|') {
        if (CurrentChar == op) {
            ReadNext();
            op |= 0x80;
            prec += 3;
        }
    }
NoNext:
    assert(prec);
    return prec<<8|op;
}

static bool IsUnaryOp(void)
{
    return CurrentChar == '+' || CurrentChar == '-' || CurrentChar == '~' || CurrentChar == '!';
}

static U2 GetUnary(void)
{
    SkipWS();
    if (IsUnaryOp()) {
        const char op = CurrentChar;
        MoveNext();
        const U2 num = GetUnary();
        switch (op) {
        case '+': return num;
        case '-': return -num;
        case '~': return ~num;
        case '!': return !num;
        default:
            Error("Internaal error: Invalid unary op");
        }
    }
    return GetPrimary();
}

static U2 Compute(U1 op, U2 lhs, U2 rhs) {
    if (rhs == 0 && (op == '/' || op == '%')) {
        Error("Division by zero");
    }
    switch (op) {
    case '*': return lhs * rhs;
    case '/': return lhs / rhs;
    case '%': return lhs % rhs;
    case '+': return lhs + rhs;
    case '-': return lhs - rhs;
    case OPER_LSH: return lhs << rhs;
    case OPER_RSH: return lhs << rhs;
    case '<': return lhs < rhs;
    case OPER_LEQ: return lhs <= rhs;
    case OPER_GEQ: return lhs >= rhs;
    case '>': return lhs > rhs;
    case '=': return lhs == rhs;
    case '!': return lhs != rhs;
    case '&': return lhs & rhs;
    case '^': return lhs ^ rhs;
    case '|': return lhs | rhs;
    case OPER_LAND: return lhs && rhs;
    case OPER_LXOR: return !!lhs ^ !!rhs;
    case OPER_LOR: return lhs || rhs;
    default:
        printf("%u %c (0x%02X) %u\n", lhs, op, op, rhs);
        Error("Internal error. Operator not supported");
    }
}

static U2 GetExpr1(U2 LHS, U1 OuterPrecedence)
{
    for (;;) {
        if (!CurrentOp) {
            CurrentOp = GetOp();
        }
        const U1 InnerPrecedence = CurrentOp>>8;
        if (!CurrentOp || InnerPrecedence > OuterPrecedence) {
            return LHS;
        }
        const U1 Op = CurrentOp & 0xff;
        CurrentOp = 0;
        U2 RHS = GetUnary();
        for (;;) {
            CurrentOp = GetOp();
            const U1 LookAheadPrcedence = CurrentOp>>8;
            if (!CurrentOp || LookAheadPrcedence >= InnerPrecedence) { // TODO: Handle right associative ops here
                break;
            }
            RHS = GetExpr1(RHS, LookAheadPrcedence);
        }
        LHS = Compute(Op, LHS, RHS);
    }
}

static U2 GetExpr0(void)
{
    const U2 res = GetExpr1(GetUnary(), MAX_PREC);
    if (CurrentOp) {
        printf("CurrentOp: %c (prec: %d)\n", CurrentOp&0xff, CurrentOp>>8);
        Error("Unprocessed operand");
    }
    return res;
}

static void NewExpr(void)
{
    CurrentOp = 0;
    GotNL = false;
}

static U2 GetExpr(void)
{
    NewExpr();
    return GetExpr0();
}

static U1 ModrmFromReg(void)
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

static U1 CombineModrmWithReg(U1 ModRM)
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

static void GetOperandMem(void)
{
    assert(!CurrentFixup);

    Expect('[');

    U1 ModRM = 0xFF;
    U2 Disp  = 0;
    bool neg;
    do {
    redo:
        neg = TryConsume('-');
        if (GetRegOrNumber()) {
            if (OperandType == OP_LIT) {
                goto disp;
            }
            if (neg) {
                Error("Can't negate register");
            }
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
        } else {
            GetNamedLiteral();
        disp:
            if (neg) {
                Disp -= OperandValue;
            } else {
                Disp += OperandValue;
            }
        }
        if (CurrentChar == '-') {
            goto redo;
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

static void GetOperand(void)
{
    if (CurrentChar == '[') {
        GetOperandMem();
        return;
    } else if (CurrentChar == '(' || IsUnaryOp()) {
        OperandType = OP_LIT;
        OperandValue = GetExpr();
        return;
    }
    NewExpr();
    if (TryGet('\'')) {
        OperandValue = GetCharLit();
        OperandType = OP_LIT;
    } else {
        GetToken();
        if (IsTokenNumber) {
            OperandType = OP_LIT;
        } else {
            OperandValue = GetReg();
            if (OperandValue != R_INVALID) {
                OperandType = OP_REG;
                return;
            } else if (!strcmp(UTokenText, "BYTE") || !strcmp(UTokenText, "WORD") || !strcmp(UTokenText, "FAR")) {
                assert(ExplicitSize == NO_SIZE);
                ExplicitSize = UTokenText[0] != 'B';
                GetOperandMem();
                return;
            } else if (!strcmp(UTokenText, "SHORT")) {
                ExplicitSize = 1;
                OperandValue = GetExpr();
                OperandType = OP_LIT;
                return;
            }
            GetNamedLiteral();
        }
    }
    assert(OperandType == OP_LIT);
    OperandValue = GetExpr1(OperandValue, MAX_PREC);
}

static void MoveToOperandL(void)
{
    OperandLType  = OperandType;
    OperandLValue = OperandValue;
    CurrentLFixup = CurrentFixup;
    CurrentFixup = NULL;
}

static void DirectiveOrg(U1 arg)
{
    (void)arg;
    if (OutputOffset) {
        Error("ORG changed after emitting instruction");
    }
    const U2 org = GetExpr();
    if (org < CurrentAddress) {
        Error("Invalid ORG (moving backwards)");
    }
    CurrentAddress = org;
    SectionStart = org;
}

static void DirectiveCPU(U1 arg)
{
    (void)arg;
    const U2 level = GetExpr();
    switch (level) {
    case 8086:
        CpuLevel = 0;
        break;
    case 186:
    case 286:
    case 386:
        CpuLevel = (U1)(level / 100);
        break;
    default:
        Error("Unrecognized CPU");
    }
    assert(CpuLevel <= 3);
}

static void OutputImm16(void);

static void DirectiveDx(U1 size)
{
    assert(size == 1 || size == 2);
    do {
        if (TryGet('\'')) {
            U2 n = 0;
            while (!TryConsume('\'')) {
                const U1 c = GetChar();
                if (c < 0x20) {
                    Error("Unterminated literal");
                }
                OutputByte(c); // Always output one byte when in character literal mode
                ++n;
            }
            if (n % size) {
                // Make sure the output is correctly aligned
                OutputByte(0);
            }
        } else {
            const U2 val = GetExpr();
            if (CurrentFixup) {
                if (size == 1) {
                    Error("Fixup not possible for byte value");
                }
                RegisterFixup(FIXUP_DISP16);
            }
            OutputByte((U1)val);
            if (size > 1) {
                OutputByte(val>>8);
            }
        }
    } while (TryConsume(','));
}

static void DirectiveResx(U1 size)
{
    assert(size == 1 || size == 2);
    const U2 count = GetExpr();
    if (count * size + CurrentAddress > 0xFFFF) {
        Error("Memory overflow in RESX");
    }
    CurrentAddress += count * size;
    PendingZeros += count * size;
}

static void DirectiveIf(U1 arg)
{
    (void)arg;
    if (NumIfs == IF_MAX) {
        Error("%if's nested too deep");
    }
    const bool InDisabledBlock = NumIfs && !(Ifs[NumIfs-1].State & IF_ACTIVE);
    struct If* i = &Ifs[NumIfs++];
    i->Line = CurrentLine;
    i->State = GetExpr() ? IF_ACTIVE|IF_WAS_ACTIVE : 0;
    if (InDisabledBlock) {
        i->State = IF_WAS_ACTIVE;
    }
}

static void DirectiveElif(U1 arg)
{
    (void)arg;
    if (!NumIfs) {
        Error("%elif outside %if block");
    }
    struct If* i = &Ifs[NumIfs-1];
    const U2 val = GetExpr();
    if (i->State & IF_SEEN_ELSE) {
        Error("%elif invalid after %else");
    }
    if (val && !(i->State & IF_WAS_ACTIVE)) {
        i->State = IF_ACTIVE|IF_WAS_ACTIVE;
    } else {
        i->State &= ~IF_ACTIVE;
    }
}

static void DirectiveElse(U1 arg)
{
    (void)arg;
    if (!NumIfs) {
        Error("%else outside %if block");
    }
    struct If* i = &Ifs[NumIfs-1];
    if (i->State & IF_SEEN_ELSE) {
        Error("Multiple %else's not allowed");
    }
    if (!(i->State & IF_WAS_ACTIVE)) {
        i->State = IF_ACTIVE|IF_WAS_ACTIVE;
    } else {
        i->State &= ~IF_ACTIVE;
    }
    i->State |= IF_SEEN_ELSE;
}

static void DirectiveEndif(U1 arg)
{
    (void)arg;
    if (!NumIfs) {
        Error("%endif outside %if block");
    }
    --NumIfs;
}

static void InstINT(U1 arg)
{
    (void)arg;
    GetOperand();
    if (OperandType != OP_LIT || OperandValue > 0xff) {
        Error("Invalid operand to INT");
    }
    OutputByte(0xCD);
    OutputByte((U1)OperandValue);
}

static void Get2Operands(void)
{
    GetOperand();
    MoveToOperandL();
    Expect(',');
    GetOperand();
}

static void OutputImm16(void)
{
    if (CurrentFixup) {
        RegisterFixup(FIXUP_DISP16);
    }
    OutputWord(OperandValue);
}

static void OutputImm8(void)
{
    if (!IsShort(OperandValue) && OperandValue > 0xFF) {
        Error("8-bit immediate out of range");
    }
    OutputByte(OperandValue & 0xff);
}

static void OutputImm(bool is16bit)
{
    if (is16bit) {
        OutputImm16();
    } else {
        OutputImm8();
    }
}

// Assumes left side operand is mem
static void OutputModRM(U1 r)
{
    assert(r < 8 && OperandLType < OP_REG);
    OutputByte((U1)(OperandLType | (r<<3))); // ModRM
    if (OperandLType == 6 || (OperandLType & 0xc0) == 0x80) {
        if (CurrentLFixup) {
            SwapFixup();
            RegisterFixup(FIXUP_DISP16);
            SwapFixup();
        }
        // Disp16
        OutputWord(OperandLValue);
    } else if ((OperandLType & 0xc0) == 0x40) {
        // Disp8
        OutputByte(OperandLValue&0xff);
    }
}

static void OutputRR(U1 inst)
{
    if (!!(OperandLValue/8) != !!(OperandValue/8)) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandValue/8 == 1 ? 1 : 0)); // 16 or 8-bit?
    OutputByte(0xc0 | (OperandLValue&7) | (OperandValue&7)<<3);
}

static void OutputMR(U1 inst)
{
    if (ExplicitSize != NO_SIZE && ExplicitSize != OperandValue/8) {
        Error("Invalid register sizes");
    }
    OutputByte(inst | (OperandValue/8 == 1 ? 1 : 0)); // 16 or 8-bit?
    OutputModRM(OperandValue&7);
}

static void SwapOperands(void)
{
    U1 tempT = OperandLType;
    U2 tempV = OperandLValue;
    OperandLType = OperandType;
    OperandLValue = OperandValue;
    OperandType = tempT;
    OperandValue = tempV;
    SwapFixup();
}

static void OutputRM(U1 inst)
{
    SwapOperands();
    OutputMR(inst);
}

static void OutputMImm(U1 inst, U1 r)
{
    if (ExplicitSize == NO_SIZE) {
        Error("Unknown operand size");
    }
    if (ExplicitSize && inst == 0x80 && !CurrentFixup && IsShort(OperandValue)) {
        inst |= 3;
        ExplicitSize = 0;
    }

    OutputByte(inst | ExplicitSize);   // Opcode
    OutputModRM(r);
    OutputImm(ExplicitSize);           // Immediate
}

static void InstMOV(U1 arg)
{
    (void)arg;
    Get2Operands();

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
            assert(OperandType < OP_REG);
            // MOV reg, mem
            if (OperandType == 6 && (OperandLValue == R_AL || OperandLValue == R_AX)) {
                OutputByte(0xA0 | (OperandLValue/8==1));
                OutputImm16();
            } else {
                OutputRM(OperandLValue >= R_ES ? 0x8E : 0x8A);
            }
        }
    } else if (OperandLType < OP_REG) {
        // LHS is memory
        if (OperandType == OP_REG) {
            // MOV mem, reg
            if (OperandLType == 6 && (OperandValue == R_AL || OperandValue == R_AX)) {
                OutputByte(0xA2 | (OperandValue/8==1));
                SwapOperands();
                OutputImm16();
            } else {
                OutputMR(OperandValue >= R_ES ? 0x8C : 0x88);
            }
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

static void InstMOVXX(U1 op2)
{
    CheckCPU(3);
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

static void InstLEA(U1 arg)
{
    (void)arg;
    Get2Operands();
    if (OperandLType != OP_REG || OperandLValue/8 != 1 || OperandType >= OP_REG) {
        Error("Invalid operands to LEA");
    }
    OutputRM(0x8D);
}

static void InstLXS(U1 inst)
{
    Get2Operands();
    if (OperandLType != OP_REG || OperandLValue/8 != 1 || OperandType >= OP_REG) {
        Error("Invalid operands to LDS/LES");
    }
    // Suppress |1 in OutputRM (for 16-bit register value)
    OperandLValue %= 8;
    ExplicitSize = NO_SIZE;
    OutputRM(inst);
}

static void InstXCHG(U1 arg)
{
    (void)arg;
    Get2Operands();
    if (OperandLType == OP_REG) {
        if (OperandType == OP_REG) {
            if (OperandLValue == R_AX) {
                OutputByte(0x90 | (OperandValue&7));
            } else if (OperandValue == R_AX) {
                OutputByte(0x90 | (OperandLValue&7));
            } else {
                OutputRR(0x86);
            }
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

static void InstTEST(U1 arg)
{
    (void)arg;
    Get2Operands();

    if (OperandType == OP_LIT) {
        bool is16bit = false;
        if (OperandLType == OP_LIT) {
            goto Invalid;
        } else if (OperandLType == OP_REG) {
            if (OperandLValue >= R_ES) {
                goto Invalid;
            }
            if (OperandLValue == R_AL) {
                OutputByte(0xA8);
                OutputImm8();
                return;
            } else if (OperandLValue == R_AX) {
                OutputByte(0xA9);
                OutputImm16();
                return;
            }
            is16bit = (OperandLValue/8==1?1:0);
            OutputByte(0xF6 | is16bit);
            OutputByte(0xC0 | (OperandLValue&7));
        } else {
            if (ExplicitSize == NO_SIZE) {
                Error("Operand size not specified");
            }
            is16bit = ExplicitSize;
            OutputByte(0xF6 | ExplicitSize);
            OutputModRM(0);
        }
        OutputImm(is16bit);
    } else if (OperandType == OP_REG || (OperandType < OP_REG && OperandLType == OP_REG)) {
        if (OperandType != OP_REG) {
            SwapOperands();
        }
        if (OperandValue >= R_ES || OperandLType == OP_LIT) {
            goto Invalid;
        }
        if (OperandLType == OP_REG) {
            OutputRR(0x84);
        } else {
            OutputMR(0x84);
        }
    } else {
Invalid:
        PrintInstr("TEST Not implmeneted", true);
        Error("Invalid/unsupported operands to TEST");
    }
}

static void InstIncDec(U1 dec)
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
        if (ExplicitSize == NO_SIZE) {
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

static void InstNotNeg(U1 r)
{
    assert(r == 2 || r == 3);
    GetOperand();
    if (OperandType == OP_REG) {
        if (OperandValue/8>1) {
            // NOT/NEG S-Reg invalid
            Error("Invalid operand");
        }
        OutputByte(0xF6 | (U1)(OperandValue/8));
        OutputByte(0xC0 | r<<3 | (OperandValue&7));
    } else if (OperandType < OP_REG) {
        if (ExplicitSize == NO_SIZE) {
            Error("Unknown operand size");
        }
        OutputByte(0xF6 | ExplicitSize);
        SwapOperands();
        OutputModRM(r);
    } else {
        // Literal not allowed
        Error("Invalid operand");
    }
}

static void InstALU(U1 base)
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
                if (OperandLValue >= R_ES) {
                    Error("Invalid operand");
                }
                U1 inst = 0x80 | !!(OperandLValue/8);
                if ((inst&1) && !CurrentFixup && IsShort(OperandValue)) {
                    inst |= 2;
                }
                OutputByte(inst);
                OutputByte(0xC0 | (OperandLValue&7) | base);
                OutputImm(inst==0x81);
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

static void InstROT(U1 r)
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
                if (ExplicitSize == NO_SIZE) {
                    Error("Operand size not specified");
                }
                OutputByte(0xd2 | ExplicitSize);
                OutputModRM(r);
            }
            return;
        }
    } else if (OperandType == OP_LIT) {
        const U1 inst = OperandValue == 1 ? 0xD0 : 0xC0;
        if (OperandLType == OP_REG) {
            const bool is16bit = OperandLValue/8==1;
            OutputByte(inst | (is16bit?1:0));
            OutputByte(0xc0 | (r<<3) | (OperandLValue&7));
        } else {
            if (ExplicitSize == NO_SIZE) {
                Error("Operand size not specified");
            }
            OutputByte(inst | ExplicitSize);
            OutputModRM(r);
        }
        if (OperandValue != 1) {
            CheckCPU(1);
            OutputImm8();
        }
        return;
    }
    PrintInstr("ROT", 2);
    Error("Not implemented");
}

static void InstMulDiv(U1 r)
{
    assert(r >= 4 && r < 8);
    GetOperand();
    if (OperandType == OP_LIT) {
        Error("Invalid operands for mul/div");
    }
    if (OperandType == OP_REG) {
        if (OperandValue >= R_ES) {
            Error("Invalid register for mul/div");
        }
        OutputByte(0xF6 | (OperandValue/8==1));
        OutputByte(0xC0 | (r<<3) | (OperandValue&7));
    } else {
        if (ExplicitSize == NO_SIZE) {
            Error("Operand size not specified");
        }
        OutputByte(0xF6 | ExplicitSize);
        SwapOperands();
        OutputModRM(r);
    }
}

static void InstIN(U1 arg)
{
    (void)arg;
    Get2Operands();
    if (OperandLType == OP_REG && (OperandLValue == R_AL || OperandLValue == R_AX)) {
        const bool is16bit = OperandLValue == R_AX;
        if (OperandType == OP_REG && OperandValue == R_DX) {
            // IN AL/AX, DX
            OutputByte(0xEC | is16bit);
            return;
        } else if (OperandType == OP_LIT) {
            // IN AL/AX, imm8
            OutputByte(0xE4 | is16bit);
            OutputImm8();
            return;
        }
    }

    Error("Invalid operands to IN");
}

static void InstOUT(U1 arg)
{
    (void)arg;
    Get2Operands();
    if (OperandType == OP_REG && (OperandValue == R_AL || OperandValue == R_AX)) {
        const bool is16bit = OperandValue == R_AX;
        if (OperandLType == OP_REG && OperandLValue == R_DX) {
            OutputByte(0xEE | is16bit);
            return;
        } else if (OperandLType == OP_LIT) {
            OutputByte(0xE6 | is16bit);
            SwapOperands();
            OutputImm8();
            return;
        }

    }
    Error("Invalid operands to OUT");
}

static void InstAAX(U1 inst)
{
    OutputByte(inst);
    OutputByte(0x0A);
}

static void InstPUSH(U1 arg)
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
        CheckCPU(1);
        if (!IsShort(OperandValue) || CurrentFixup) {
            OutputByte(0x68);
            OutputImm16();
        } else {
            OutputByte(0x6A);
            OutputImm8();
        }
    } else {
        OutputByte(0xFF);
        SwapOperands();
        OutputModRM(6);
    }
}

static void InstPOP(U1 arg)
{
    (void)arg;
    GetOperand();
    if (OperandType < OP_REG) {
        OutputByte(0x8F);
        SwapOperands();
        OutputModRM(0);
        return;
    }
    if (OperandValue < R_AX) {
        Error("Invalid/unsupported POP");
    }
    if (OperandValue >= R_ES) {
        assert(OperandValue <= R_DS && OperandValue != R_CS);
        OutputByte((U1)(0x07 | (OperandValue-R_ES)<<3));
    } else {
        OutputByte((U1)(0x58 | (OperandValue & 7)));
    }
}

static void HandleRel16(bool CouldBeShort)
{
    if (OperandType != OP_LIT) {
        Error("Expected literal");
    }
    if (CurrentFixup) {
        RegisterFixup(CouldBeShort ? FIXUP_REL16 : FIXUP_DISP16);
    }
    OutputWord((U2)(OperandValue - (CurrentAddress + 2)));
}

static bool HandleFar(U1 inst)
{
    if (OperandType != OP_LIT || !TryConsume(':')) {
        return false;
    }
    OutputByte(inst);
    const U2 Saved = OperandValue;
    OutputWord(GetNumber());
    OutputWord(Saved);
    return true;
}

static void InstCALL(U1 arg)
{
    (void)arg;
    GetOperand();
    if (HandleFar(0x9A)) {
        return;
    }
    if (OperandType <= OP_REG) {
        OutputByte(0xFF);
        if (OperandType == OP_REG) {
            if (OperandValue/8 != 1) {
                Error("Invalid register operand to CALL");
            }
            OutputByte(0xC0 | (2<<3) | (OperandValue&7));
        } else {
            SwapOperands();
            OutputModRM(ExplicitSize == 1 ? 3 : 2); // far call?
        }
        return;
    }
    OutputByte(0xE8);
    HandleRel16(false);
}

static bool HandleShortRel(U1 inst, bool force)
{
    if (ExplicitSize == 1) {
        force = true;
    }

    if (OperandType != OP_LIT) {
        Error("Expected literal");
    }

    if (!CurrentFixup) {
        const U2 rel = OperandValue - (CurrentAddress + 2);
        if (IsShort(rel)) {
            OutputByte(inst);
            OutputByte(rel&0xff);
            return true;
        }
    } else if (force) {
        OutputByte(inst);
        RegisterFixup(FIXUP_REL8);
        OutputByte(0x00);
        return true;
    }

    if (force) {
        Error("Short jump out of range");
    }
    return false;
}


static void InstJMP(U1 arg)
{
    (void)arg;
    GetOperand();
    if (HandleFar(0xEA)) {
        return;
    }
    if (OperandType <= OP_REG) {
        OutputByte(0xFF);
        if (OperandType == OP_REG) {
            if (OperandValue>>3 != 1) {
                Error("Invalid register operand to JMP");
            }
            OutputByte(0xC0 | 4<<3 | (OperandValue&7));
        } else {
            SwapOperands();
            OutputModRM(ExplicitSize == 1 ? 5 : 4); // far jump?
        }
        return;
    }
    if (!HandleShortRel(0xEB, false)) {
        OutputByte(0xE9);
        HandleRel16(true);
    }
}

static void InstJcc(U1 cc) {
    assert(cc < 16);
    GetOperand();
    if (!HandleShortRel(0x70|cc, CpuLevel < 3)) {
        OutputWord(0x800F | cc<<8);
        HandleRel16(true);
    }
}

static void HandleJ8(U1 inst)
{
    GetOperand();
    HandleShortRel(inst, true);
}

static void OutputByte186(U1 inst)
{
    CheckCPU(1);
    OutputByte(inst);
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
    const char text[7];
    void (*func)(U1);
    U1 arg;
} DispatchList[] = {
//  { "123456" , &Directive123456 , 0x12 }
    { "ORG"    , &DirectiveOrg    , 0x00 },
    { "DB"     , &DirectiveDx     , 0x01 },
    { "DW"     , &DirectiveDx     , 0x02 },
    { "RESB"   , &DirectiveResx   , 0x01 },
    { "RESW"   , &DirectiveResx   , 0x02 },
    { "CPU"    , &DirectiveCPU    , 0x00 },
    { "%IF"    , &DirectiveIf     , 0x00 },
    { "%ELIF"  , &DirectiveElif   , 0x00 },
    { "%ELSE"  , &DirectiveElse   , 0x00 },
    { "%ENDIF" , &DirectiveEndif  , 0x00 },
    { "REPNE"  , &OutputByte      , 0xF2 },
    { "REPE"   , &OutputByte      , 0xF3 },
    { "REP"    , &OutputByte      , 0xF3 },
    { "MOV"    , &InstMOV         , 0x00 },
    { "MOVZX"  , &InstMOVXX       , 0xB6 },
    { "MOVSX"  , &InstMOVXX       , 0xBE },
    { "LEA"    , &InstLEA         , 0x00 },
    { "LES"    , &InstLXS         , 0xC4 },
    { "LDS"    , &InstLXS         , 0xC5 },
    { "XCHG"   , &InstXCHG        , 0x00 },
    { "TEST"   , &InstTEST        , 0x00 },
    { "INC"    , &InstIncDec      , 0x00 },
    { "DEC"    , &InstIncDec      , 0x01 },
    { "NOT"    , &InstNotNeg      , 0x02 },
    { "NEG"    , &InstNotNeg      , 0x03 },
    { "ADD"    , &InstALU         , 0x00 },
    { "OR"     , &InstALU         , 0x08 },
    { "ADC"    , &InstALU         , 0x10 },
    { "SBB"    , &InstALU         , 0x18 },
    { "AND"    , &InstALU         , 0x20 },
    { "SUB"    , &InstALU         , 0x28 },
    { "XOR"    , &InstALU         , 0x30 },
    { "CMP"    , &InstALU         , 0x38 },
    { "ROL"    , &InstROT         , 0x00 },
    { "ROR"    , &InstROT         , 0x01 },
    { "RCL"    , &InstROT         , 0x02 },
    { "RCR"    , &InstROT         , 0x03 },
    { "SHL"    , &InstROT         , 0x04 },
    { "SHR"    , &InstROT         , 0x05 },
    { "SAR"    , &InstROT         , 0x07 },
    { "MUL"    , &InstMulDiv      , 0x04 },
    { "IMUL"   , &InstMulDiv      , 0x05 },
    { "DIV"    , &InstMulDiv      , 0x06 },
    { "IDIV"   , &InstMulDiv      , 0x07 },
    { "INT"    , &InstINT         , 0x00 },
    { "INT3"   , &OutputByte      , 0xCC },
    { "INTO"   , &OutputByte      , 0xCE },
    { "RET"    , &OutputByte      , 0xC3 },
    { "RETF"   , &OutputByte      , 0xCB },
    { "IRET"   , &OutputByte      , 0xCF },
    { "NOP"    , &OutputByte      , 0x90 },
    { "CBW"    , &OutputByte      , 0x98 },
    { "CWD"    , &OutputByte      , 0x99 },
    { "PUSHA"  , &OutputByte186   , 0x60 },
    { "POPA"   , &OutputByte186   , 0x61 },
    { "PUSHF"  , &OutputByte      , 0x9C },
    { "POPF"   , &OutputByte      , 0x9D },
    { "PUSH"   , &InstPUSH        , 0x00 },
    { "POP"    , &InstPOP         , 0x00 },
    { "INSB"   , &OutputByte      , 0x6C },
    { "INSW"   , &OutputByte      , 0x6D },
    { "OUTSB"  , &OutputByte      , 0x6E },
    { "OUTSW"  , &OutputByte      , 0x6F },
    { "MOVSB"  , &OutputByte      , 0xA4 },
    { "MOVSW"  , &OutputByte      , 0xA5 },
    { "CMPSB"  , &OutputByte      , 0xA6 },
    { "CMPSW"  , &OutputByte      , 0xA7 },
    { "STOSB"  , &OutputByte      , 0xAA },
    { "STOSW"  , &OutputByte      , 0xAB },
    { "LODSB"  , &OutputByte      , 0xAC },
    { "LODSW"  , &OutputByte      , 0xAD },
    { "SCASB"  , &OutputByte      , 0xAE },
    { "SCASW"  , &OutputByte      , 0xAF },
    { "IN"     , &InstIN          , 0x00 },
    { "OUT"    , &InstOUT         , 0x00 },
    { "SAHF"   , &OutputByte      , 0x9E },
    { "LAHF"   , &OutputByte      , 0x9F },
    { "XLATB"  , &OutputByte      , 0xD7 },
    { "XLAT"   , &OutputByte      , 0xD7 },
    { "HLT"    , &OutputByte      , 0xF4 },
    { "CMC"    , &OutputByte      , 0xF5 },
    { "CLC"    , &OutputByte      , 0xF8 },
    { "STC"    , &OutputByte      , 0xF9 },
    { "CLI"    , &OutputByte      , 0xFA },
    { "STI"    , &OutputByte      , 0xFB },
    { "CLD"    , &OutputByte      , 0xFC },
    { "STD"    , &OutputByte      , 0xFD },
    { "DAA"    , &OutputByte      , 0x27 },
    { "DAS"    , &OutputByte      , 0x2F },
    { "AAA"    , &OutputByte      , 0x37 },
    { "AAS"    , &OutputByte      , 0x3F },
    { "AAM"    , &InstAAX         , 0xD4 },
    { "AAD"    , &InstAAX         , 0xD5 },
    { "CALL"   , &InstCALL        , 0x00 },
    { "JMP"    , &InstJMP         , 0x00 },
    { "LOOPNE" , &HandleJ8        , 0xE0 },
    { "LOOPE"  , &HandleJ8        , 0xE1 },
    { "LOOP"   , &HandleJ8        , 0xE2 },
    { "JCXZ"   , &HandleJ8        , 0xE3 },
    { "JO"     , &InstJcc         , JO   },
    { "JNO"    , &InstJcc         , JNO  },
    { "JC"     , &InstJcc         , JC   },
    { "JB"     , &InstJcc         , JC   },
    { "JNC"    , &InstJcc         , JNC  },
    { "JNB"    , &InstJcc         , JNC  },
    { "JAE"    , &InstJcc         , JNC  },
    { "JZ"     , &InstJcc         , JZ   },
    { "JE"     , &InstJcc         , JZ   },
    { "JNZ"    , &InstJcc         , JNZ  },
    { "JNE"    , &InstJcc         , JNZ  },
    { "JNA"    , &InstJcc         , JNA  },
    { "JBE"    , &InstJcc         , JNA  },
    { "JA"     , &InstJcc         , JA   },
    { "JS"     , &InstJcc         , JS   },
    { "JNS"    , &InstJcc         , JNS  },
    { "JPE"    , &InstJcc         , JPE  },
    { "JPO"    , &InstJcc         , JPO  },
    { "JL"     , &InstJcc         , JL   },
    { "JGE"    , &InstJcc         , JNL  },
    { "JNL"    , &InstJcc         , JNL  },
    { "JNG"    , &InstJcc         , JNG  },
    { "JLE"    , &InstJcc         , JNG  },
    { "JG"     , &InstJcc         , JG   },
    };

static bool TryGetU(U1 ch)
{
    assert(ch >= 'A' && ch <= 'Z');
    return TryGet(ch) || TryGet(ch + 'a' - 'A');
}

static void Dispatch(void)
{
    if (TryConsume(':')) {
        DefineLabel();
        return;
    }
    ExplicitSize = NO_SIZE;
    for (unsigned i = 0; i < sizeof(DispatchList)/sizeof(*DispatchList); ++i) {
        if (!strcmp(UTokenText, DispatchList[i].text)) {
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

    struct Equ* e = DefineEqu();
    e->Value = GetExpr();
}

static void ParserInit(const char* filename)
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

static void ParserFini(void)
{
    if (ferror(InputFile)) {
        Error("Read error");
    } else if (!feof(InputFile)) {
        Error("File not completely read");
    }
    fclose(InputFile);
    InputFile = NULL;
}

static char* GetOutputFileName(const char* InputFileName)
{
    int l = (int)strlen(InputFileName);
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

void NextMainToken(void)
{
    CurrentLine += NumNewLines;
    NumNewLines = 0;
    GetToken();
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
    Usage:
        fprintf(stderr, "Usage: %s input-file [-o output] [-Wshort] [-Wunused-label]\n", argv[0]);
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
            } else if (!strcmp(a, "-Wunused-label")) {
                WarnUnusedLabel = true;
            } else if (!strcmp(a, "-Wshort")) {
                WarnShort = true;
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
        if (NumIfs && !(Ifs[NumIfs-1].State & IF_ACTIVE)) {
            // Skip to next directive...
            do {
                while (CurrentChar && CurrentChar != '%') {
                    MoveNext();
                }
                NextMainToken();
            } while (TokenLen && strcmp(UTokenText, "%IF") && strcmp(UTokenText, "%ELSE") && strcmp(UTokenText, "%ELIF") && strcmp(UTokenText, "%ENDIF"));
        } else {
            NextMainToken();
        }
        if (!TokenLen) {
            break;
        }
        Dispatch();
    }
    // Make sure all labels have been defined
    while (NumLabels) {
        RetireLabel(0);
    }
    if (NumIfs) {
        while (NumIfs--) {
            printf("Line %u: %%if not terminated\n", Ifs[NumIfs].Line);
        }
        Error("Unterminated %if-block(s)");
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
