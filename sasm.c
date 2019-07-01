#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef unsigned char U1;
typedef unsigned short U2;

#define TOKEN_MAX 32

FILE* InputFile;
U2 CurrentLine;
U1 CurrentChar;
char TokenText[TOKEN_MAX+1];
U1 TokenLen;

void Error(const char* msg)
{
    fprintf(stderr, "Line %u: %s (Current token: \"%s\")\n", CurrentLine, msg, TokenText);
    exit(1);
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

void GetToken(void)
{
    TokenLen = 0;
    while (CurrentChar && CurrentChar != ',' && CurrentChar > ' ') {
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
    ReadNext();
    SkipWS();
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

void GetOperand(void)
{
    if (CurrentChar == '\'') {
        ReadNext();
        U2 lit = CurrentChar;
        U1 litSize = 1;
        ReadNext();
        if (CurrentChar != '\'') {
            lit |= CurrentChar << 8;
            ReadNext();
            ++litSize;
        }
        if (CurrentChar != '\'') {
            Error("Invalid character literal");
        }
        ReadNext();
        SkipWS();
        printf("Ignoring character literal 0x%0.*X\n", litSize*2, lit);
        return;
    }

    GetToken();
    if (!strcmp(TokenText, "BYTE") || !strcmp(TokenText, "WORD")) {
        GetToken();
    }
}

void DefineLabel(void)
{
    --TokenLen;
    TokenText[TokenLen] = '\0';
    printf("Ignoring label \"%s\"\n", TokenText);
}

void DirectiveOrg(void)
{
    const U2 org = GetNumber();
    printf("Ignoring ORG 0x%04X\n", org);
}

void DirectiveDx(U1 size)
{
    printf("Ignoring D%c\n", size==2?'W':'B');
    for (;;){
        if (CurrentChar == '\'') {
            printf(" Raw chars:");
            ReadNext();
            while (CurrentChar != '\'') {
                if (CurrentChar < 0x20) {
                    Error("Unterminated literal");
                }
                printf(" %02X", CurrentChar);
                ReadNext();
            }
            printf("\n");
            ReadNext();
            SkipWS();
        } else if (CurrentChar >= '0' && CurrentChar <= '9') {
            const U2 n = GetNumber();
            printf("  %X\n", n);
        } else {
            GetToken();
            printf("  %s\n", TokenText);
        }
        if (CurrentChar != ',') {
            break;
        }
        ReadNext();
        SkipWS();
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
    printf("%s\n", TokenText);
}

void InstIgnore2(void)
{
    printf("Ignoring %s ", TokenText);
    GetOperand();
    printf("%s, ", TokenText);
    ExpectComma();
    GetOperand();
    printf("%s\n", TokenText);
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
    { "INT", &InstIgnore1 },
    { "RET", &InstIgnore0 },
    { "MOV", &InstIgnore2 },
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
    CurrentChar = ' ';
    SkipWS();
}

void ParserFini(void)
{
    fclose(InputFile);
    InputFile = NULL;
}

int main()
{
    ParserInit("../old2/sasm.asm");
    for (;;) {
        GetToken();
        if (!TokenLen) {
            break;
        }
        printf("%3u: Token: \"%s\"\n", CurrentLine, TokenText);
        Dispatch();
    }
    ParserFini();
}
