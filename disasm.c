#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>

typedef signed char S1;
typedef unsigned char U1;
typedef unsigned short U2;
typedef short S2;
typedef unsigned int U4;
typedef int S4;

enum {
    R_AL, R_CL, R_DL, R_BL, R_AH, R_CH, R_DH, R_BH,
    R_AX, R_CX, R_DX, R_BX, R_SP, R_BP, R_SI, R_DI,
    R_ES, R_CS, R_SS, R_DS,
};

enum {
    OTYPE_IMM8  = R_DS+1,
    OTYPE_IMM16,
    OTYPE_REL16,

    OTYPE_NONE = 0xFF
};

static const char RegNames[][3] = {
    "AL", "CL", "DL", "BL", "AH", "CH", "DH", "BH",
    "AX", "CX", "DX", "BX", "SP", "BP", "SI", "DI",
    "ES", "CS", "SS", "DS",
};

#define PREFIX_F2 0x01 // REPNE/REPNZ
#define PREFIX_F3 0x02 // REP/REPZ
#define PREFIX_ES 0x04
#define PREFIX_CS 0x08
#define PREFIX_SS 0x10
#define PREFIX_DS 0x20

#define OTYPE_IS_REG(ot) ((U2)(ot) <= R_DS)

#define OP_TEXT_MAX 16

static U1* File;
static U2 FileSize;
static U2 CurrentAddress;
static U1 Prefixes;
static union {
    U1 I8;
    U2 I16;
} Immediate;

static char Op1Text[OP_TEXT_MAX];
static char Op2Text[OP_TEXT_MAX];

#define READ_U1(offset) ((U1)((U4)(offset) < (U4)FileSize ? File[offset] : 0))
#define READ_U2(offset) ((U2)(READ_U1((offset)+1)<<8|READ_U1(offset)))


#ifdef _MSC_VER
__declspec(noreturn)
#endif
static void Error(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    fputs("", stderr);
    va_end(va);
    exit(1);
}

static void ReadFile(const char* FileName)
{
    FILE* fp = fopen(FileName, "rb");
    if (!fp) {
        Error("Could not open %s", FileName);
    }
    fseek(fp, 0L, SEEK_END);
    FileSize = (U2)ftell(fp);
    File = malloc(FileSize);
    if (!File) {
        Error("Error allocating memory for file");
    }
    fseek(fp, 0L, SEEK_SET);
    if (!fread(File, FileSize, 1, fp) || ferror(fp)) {
        Error("Error reading from %s", FileName);
    }
    fclose(fp);
}

static void GetOp(char* Str, U1 op)
{
    if (op == OTYPE_NONE) {
    } else if (OTYPE_IS_REG(op)) {
        strcpy(Str, RegNames[op]);
    } else if (op == OTYPE_IMM8) {
        snprintf(Str, OP_TEXT_MAX, "0x%02X", Immediate.I8);
    } else if (op == OTYPE_IMM16) {
        snprintf(Str, OP_TEXT_MAX, "0x%04X", Immediate.I16);
    } else if (op == OTYPE_REL16) {
        snprintf(Str, OP_TEXT_MAX, "0x%04X", CurrentAddress+Immediate.I16);
    } else {
        Error("Unimplemented Optype: %02X\n", op);
    }
}

#define N_CALL   "CALL"
#define N_DEC    "DEC"
#define N_INC    "INC"
#define N_INT    "INT"
#define N_INT3   "INT3"
#define N_MOV    "MOV"
#define N_POPA   "POPA"
#define N_PUSHA  "PUSHA"

static const struct InstructionInfo {
    const char* name;
    U1 op1;
    U1 op2;
} Instructions[256] = {
    // 0x00
    // 0x10
    // 0x20
    // 0x30
    // 0x40
    [0x40] = { N_INC    , R_AX        , OTYPE_NONE  },
    [0x41] = { N_INC    , R_CX        , OTYPE_NONE  },
    [0x42] = { N_INC    , R_DX        , OTYPE_NONE  },
    [0x43] = { N_INC    , R_BX        , OTYPE_NONE  },
    [0x44] = { N_INC    , R_SP        , OTYPE_NONE  },
    [0x45] = { N_INC    , R_BP        , OTYPE_NONE  },
    [0x46] = { N_INC    , R_SI        , OTYPE_NONE  },
    [0x47] = { N_INC    , R_DI        , OTYPE_NONE  },
    [0x48] = { N_DEC    , R_AX        , OTYPE_NONE  },
    [0x49] = { N_DEC    , R_CX        , OTYPE_NONE  },
    [0x4A] = { N_DEC    , R_DX        , OTYPE_NONE  },
    [0x4B] = { N_DEC    , R_BX        , OTYPE_NONE  },
    [0x4C] = { N_DEC    , R_SP        , OTYPE_NONE  },
    [0x4D] = { N_DEC    , R_BP        , OTYPE_NONE  },
    [0x4E] = { N_DEC    , R_SI        , OTYPE_NONE  },
    [0x4F] = { N_DEC    , R_DI        , OTYPE_NONE  },
    // 0x50
    // 0x60
    [0x60] = { N_PUSHA  , OTYPE_NONE  , OTYPE_NONE  },
    [0x61] = { N_POPA   , OTYPE_NONE  , OTYPE_NONE  },
    // 0x70
    // 0x80
    // 0x90
    // 0xA0
    // 0xB0
    [0xB0] = { N_MOV    , R_AL        , OTYPE_IMM8  },
    [0xB1] = { N_MOV    , R_CL        , OTYPE_IMM8  },
    [0xB2] = { N_MOV    , R_DL        , OTYPE_IMM8  },
    [0xB3] = { N_MOV    , R_BL        , OTYPE_IMM8  },
    [0xB4] = { N_MOV    , R_AH        , OTYPE_IMM8  },
    [0xB5] = { N_MOV    , R_CH        , OTYPE_IMM8  },
    [0xB6] = { N_MOV    , R_DH        , OTYPE_IMM8  },
    [0xB7] = { N_MOV    , R_BH        , OTYPE_IMM8  },
    [0xB8] = { N_MOV    , R_AX        , OTYPE_IMM16 },
    [0xB9] = { N_MOV    , R_CX        , OTYPE_IMM16 },
    [0xBA] = { N_MOV    , R_DX        , OTYPE_IMM16 },
    [0xBB] = { N_MOV    , R_BX        , OTYPE_IMM16 },
    [0xBC] = { N_MOV    , R_SP        , OTYPE_IMM16 },
    [0xBD] = { N_MOV    , R_BP        , OTYPE_IMM16 },
    [0xBE] = { N_MOV    , R_SI        , OTYPE_IMM16 },
    [0xBF] = { N_MOV    , R_DI        , OTYPE_IMM16 },
    // 0xC0
    [0xCC] = { N_INT3   , OTYPE_NONE  , OTYPE_NONE  },
    [0xCD] = { N_INT    , OTYPE_IMM8  , OTYPE_NONE  },
    // 0xD0
    // 0xE0
    [0xE8] ={ N_CALL    , OTYPE_REL16 , OTYPE_NONE  },
    // 0xF0
};

U2 Decode(U2 offset)
{
    const U2 start = offset;
    Prefixes = 0;

    // Prefixes
    for (;; ++offset) {
        const U1 b = READ_U1(offset);
        if (b == 0xF2)      Prefixes |= PREFIX_F2;
        else if (b == 0xF3) Prefixes |= PREFIX_F3;
        else if (b == 0x26) Prefixes |= PREFIX_ES;
        else if (b == 0x2E) Prefixes |= PREFIX_CS;
        else if (b == 0x36) Prefixes |= PREFIX_SS;
        else if (b == 0x3E) Prefixes |= PREFIX_DS;
        else break;
    }

    U1 inst = READ_U1(offset);
    ++offset;
    if (inst == 0x0F) {
        Error("Two byte instructions not supported. Next byte: %02X", READ_U1(offset));
    }

    const struct InstructionInfo* II = &Instructions[inst];
    if (!II->name) {
        Error("Instruction not implemented: %02X\n", inst);
    }

    if (II->op1 == OTYPE_IMM8 || II->op2 == OTYPE_IMM8) {
        Immediate.I8 = READ_U1(offset);
        ++offset;
    } else if (II->op1 == OTYPE_IMM16 || II->op2 == OTYPE_IMM16
        || II->op1 == OTYPE_REL16 || II->op2 == OTYPE_REL16) {
        Immediate.I16 = READ_U2(offset);
        offset += 2;
    }

    if (Prefixes) {
        Error("Unconsumed prefixes: %02X\n", Prefixes);
    }

    for (U2 i = start; i < offset; ++i) {
        printf("%02X ", READ_U1(i));
    }

    const U1 InstMaxBytes = 6;
    assert(offset-start < InstMaxBytes);
    for (U2 i = InstMaxBytes-(offset-start); i--; ) {
        printf("   ");
    }

    if (II->op1 != OTYPE_NONE) {
        GetOp(Op1Text, II->op1);
        if (II->op2 != OTYPE_NONE) {
            GetOp(Op2Text, II->op2);
        }
    }

    printf("%s", II->name);
    if (II->op1 != OTYPE_NONE) {
        printf(" %s", Op1Text);
        if (II->op2 != OTYPE_NONE) {
            printf(", %s", Op2Text);
        }
    }
    printf("\n");

    return offset;
}

int main(void)
{
    ReadFile("../tests/t01.ref");
    CurrentAddress = 0x0100;
    for (U2 offset = 0; offset < FileSize;) {
        printf("%04X ", CurrentAddress+offset);
        offset = Decode(offset);
    }
    free(File);
}
