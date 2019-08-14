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
    OTYPE_REL8,
    OTYPE_REL16,
    OTYPE_RM8,    // r/m part of MODRM
    OTYPE_RM16,
    OTYPE_R8,     // /r of ModRM
    OTYPE_R16,

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
#define OTYPE_HAS_MODRM(ot) ((U2)(ot) >= OTYPE_RM8 && (U2)(ot) <= OTYPE_R16)

#define OP_TEXT_MAX 16

static U1* File;
static U2 FileSize;
static U2 CurrentAddress;
static U1 Prefixes;
static union {
    U1 I8;
    U2 I16;
} Immediate;
static U1 ModRM;

static char Op1Text[OP_TEXT_MAX];
static char Op2Text[OP_TEXT_MAX];
static char RMText[OP_TEXT_MAX];

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
        return;
    } else if (OTYPE_IS_REG(op)) {
        strcpy(Str, RegNames[op]);
        return;
    }
    switch (op) {
    case OTYPE_IMM8:
        snprintf(Str, OP_TEXT_MAX, "0x%02X", Immediate.I8);
        break;
    case OTYPE_IMM16:
        snprintf(Str, OP_TEXT_MAX, "0x%04X", Immediate.I16);
        break;
    case OTYPE_REL8:
        snprintf(Str, OP_TEXT_MAX, "0x%04X", (U2)(CurrentAddress+(S1)Immediate.I8));
        break;
    case OTYPE_REL16:
        snprintf(Str, OP_TEXT_MAX, "0x%04X", (U2)(CurrentAddress+Immediate.I16));
        break;
    case OTYPE_RM8:
    case OTYPE_RM16:
        strcpy(Str, RMText);
        break;
    case OTYPE_R8:
        strcpy(Str, RegNames[R_AL + (ModRM&7)]);
        break;
    case OTYPE_R16:
        strcpy(Str, RegNames[R_AX + (ModRM&7)]);
        break;
    default:
        Error("Unimplemented Optype: %02X\n", op);
    }
}

#define N_ADC    "ADC"
#define N_ADD    "ADD"
#define N_AND    "AND"
#define N_CALL   "CALL"
#define N_CMP    "CMP"
#define N_CMPSB  "CMPSB"
#define N_CMPSW  "CMPSW"
#define N_DEC    "DEC"
#define N_INC    "INC"
#define N_INT    "INT"
#define N_INT3   "INT3"
#define N_LODSB  "LODSB"
#define N_LODSW  "LODSW"
#define N_MOV    "MOV"
#define N_MOVSB  "MOVSB"
#define N_MOVSW  "MOVSW"
#define N_OR     "OR"
#define N_POP    "POP"
#define N_POPA   "POPA"
#define N_PUSH   "PUSH"
#define N_PUSHA  "PUSHA"
#define N_RET    "RET"
#define N_SBB    "SBB"
#define N_SCASB  "SCASB"
#define N_SCASW  "SCASW"
#define N_STOSB  "STOSB"
#define N_STOSW  "STOSW"
#define N_SUB    "SUB"
#define N_XOR    "XOR"
#define N_JMP    "JMP"
#define N_JO     "JO"
#define N_JNO    "JNO"
#define N_JC     "JC"
#define N_JNC    "JNC"
#define N_JZ     "JZ"
#define N_JNZ    "JNZ"
#define N_JNA    "JNA"
#define N_JA     "JA"
#define N_JS     "JS"
#define N_JNS    "JNS"
#define N_JPE    "JPE"
#define N_JPO    "JPO"
#define N_JL     "JL"
#define N_JNL    "JNL"
#define N_JNG    "JNG"
#define N_JG     "JG"

struct InstructionInfo {
    const char* name;
    U1 op1;
    U1 op2;
};

static const struct InstructionInfo Instructions[256] = {
    // 0x00
    [0x00] = { N_ADD    , OTYPE_RM8   , OTYPE_R8    },
    [0x01] = { N_ADD    , OTYPE_RM16  , OTYPE_R16   },
    [0x02] = { N_ADD    , OTYPE_R8    , OTYPE_RM8   },
    [0x03] = { N_ADD    , OTYPE_R16   , OTYPE_RM16  },
    [0x04] = { N_ADD    , R_AL        , OTYPE_IMM8  },
    [0x05] = { N_ADD    , R_AX        , OTYPE_IMM16 },
    [0x08] = { N_OR     , OTYPE_RM8   , OTYPE_R8    },
    [0x09] = { N_OR     , OTYPE_RM16  , OTYPE_R16   },
    [0x0A] = { N_OR     , OTYPE_R8    , OTYPE_RM8   },
    [0x0B] = { N_OR     , OTYPE_R16   , OTYPE_RM16  },
    [0x0C] = { N_OR     , R_AL        , OTYPE_IMM8  },
    [0x0D] = { N_OR     , R_AX        , OTYPE_IMM16 },
    // 0x10
    [0x10] = { N_ADC    , OTYPE_RM8   , OTYPE_R8    },
    [0x11] = { N_ADC    , OTYPE_RM16  , OTYPE_R16   },
    [0x12] = { N_ADC    , OTYPE_R8    , OTYPE_RM8   },
    [0x13] = { N_ADC    , OTYPE_R16   , OTYPE_RM16  },
    [0x14] = { N_ADC    , R_AL        , OTYPE_IMM8  },
    [0x15] = { N_ADC    , R_AX        , OTYPE_IMM16 },
    [0x18] = { N_SBB    , OTYPE_RM8   , OTYPE_R8    },
    [0x19] = { N_SBB    , OTYPE_RM16  , OTYPE_R16   },
    [0x1A] = { N_SBB    , OTYPE_R8    , OTYPE_RM8   },
    [0x1B] = { N_SBB    , OTYPE_R16   , OTYPE_RM16  },
    [0x1C] = { N_SBB    , R_AL        , OTYPE_IMM8  },
    [0x1D] = { N_SBB    , R_AX        , OTYPE_IMM16 },
    // 0x20
    [0x20] = { N_AND    , OTYPE_RM8   , OTYPE_R8    },
    [0x21] = { N_AND    , OTYPE_RM16  , OTYPE_R16   },
    [0x22] = { N_AND    , OTYPE_R8    , OTYPE_RM8   },
    [0x23] = { N_AND    , OTYPE_R16   , OTYPE_RM16  },
    [0x24] = { N_AND    , R_AL        , OTYPE_IMM8  },
    [0x25] = { N_AND    , R_AX        , OTYPE_IMM16 },
    [0x28] = { N_SUB    , OTYPE_RM8   , OTYPE_R8    },
    [0x29] = { N_SUB    , OTYPE_RM16  , OTYPE_R16   },
    [0x2A] = { N_SUB    , OTYPE_R8    , OTYPE_RM8   },
    [0x2B] = { N_SUB    , OTYPE_R16   , OTYPE_RM16  },
    [0x2C] = { N_SUB    , R_AL        , OTYPE_IMM8  },
    [0x2D] = { N_SUB    , R_AX        , OTYPE_IMM16 },
    // 0x30
    [0x30] = { N_XOR    , OTYPE_RM8   , OTYPE_R8    },
    [0x31] = { N_XOR    , OTYPE_RM16  , OTYPE_R16   },
    [0x32] = { N_XOR    , OTYPE_R8    , OTYPE_RM8   },
    [0x33] = { N_XOR    , OTYPE_R16   , OTYPE_RM16  },
    [0x34] = { N_XOR    , R_AL        , OTYPE_IMM8  },
    [0x35] = { N_XOR    , R_AX        , OTYPE_IMM16 },
    [0x38] = { N_CMP    , OTYPE_RM8   , OTYPE_R8    },
    [0x39] = { N_CMP    , OTYPE_RM16  , OTYPE_R16   },
    [0x3A] = { N_CMP    , OTYPE_R8    , OTYPE_RM8   },
    [0x3B] = { N_CMP    , OTYPE_R16   , OTYPE_RM16  },
    [0x3C] = { N_CMP    , R_AL        , OTYPE_IMM8  },
    [0x3D] = { N_CMP    , R_AX        , OTYPE_IMM16 },
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
    [0x50] = { N_PUSH   , R_AX        , OTYPE_NONE  },
    [0x51] = { N_PUSH   , R_CX        , OTYPE_NONE  },
    [0x52] = { N_PUSH   , R_DX        , OTYPE_NONE  },
    [0x53] = { N_PUSH   , R_BX        , OTYPE_NONE  },
    [0x54] = { N_PUSH   , R_SP        , OTYPE_NONE  },
    [0x55] = { N_PUSH   , R_BP        , OTYPE_NONE  },
    [0x56] = { N_PUSH   , R_SI        , OTYPE_NONE  },
    [0x57] = { N_PUSH   , R_DI        , OTYPE_NONE  },
    [0x58] = { N_POP    , R_AX        , OTYPE_NONE  },
    [0x59] = { N_POP    , R_CX        , OTYPE_NONE  },
    [0x5A] = { N_POP    , R_DX        , OTYPE_NONE  },
    [0x5B] = { N_POP    , R_BX        , OTYPE_NONE  },
    [0x5C] = { N_POP    , R_SP        , OTYPE_NONE  },
    [0x5D] = { N_POP    , R_BP        , OTYPE_NONE  },
    [0x5E] = { N_POP    , R_SI        , OTYPE_NONE  },
    [0x5F] = { N_POP    , R_DI        , OTYPE_NONE  },
    // 0x60
    [0x60] = { N_PUSHA  , OTYPE_NONE  , OTYPE_NONE  },
    [0x61] = { N_POPA   , OTYPE_NONE  , OTYPE_NONE  },
    // 0x70
    // 0x80
    [0x88] = { N_MOV    , OTYPE_RM8   , OTYPE_R8  },
    // 0x90
    // 0xA0
    [0xA4] = { N_MOVSB  , OTYPE_NONE  , OTYPE_NONE  },
    [0xA5] = { N_MOVSW  , OTYPE_NONE  , OTYPE_NONE  },
    [0xA6] = { N_CMPSB  , OTYPE_NONE  , OTYPE_NONE  },
    [0xA7] = { N_CMPSW  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAA] = { N_STOSB  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAB] = { N_STOSW  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAC] = { N_LODSB  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAD] = { N_LODSW  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAE] = { N_SCASB  , OTYPE_NONE  , OTYPE_NONE  },
    [0xAF] = { N_SCASW  , OTYPE_NONE  , OTYPE_NONE  },
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
    [0xC3] = { N_RET    , OTYPE_NONE  , OTYPE_NONE  },
    [0xCC] = { N_INT3   , OTYPE_NONE  , OTYPE_NONE  },
    [0xCD] = { N_INT    , OTYPE_IMM8  , OTYPE_NONE  },
    // 0xD0
    // 0xE0
    [0xE8] = { N_CALL    , OTYPE_REL16 , OTYPE_NONE  },
    [0xEB] = { N_JMP     , OTYPE_REL8  , OTYPE_NONE  },
    // 0xF0
};

static const struct InstructionInfo Instructions0F[256] = {
    [0x80] = { N_JO      , OTYPE_REL16 , OTYPE_NONE  },
    [0x81] = { N_JNO     , OTYPE_REL16 , OTYPE_NONE  },
    [0x82] = { N_JC      , OTYPE_REL16 , OTYPE_NONE  },
    [0x83] = { N_JNC     , OTYPE_REL16 , OTYPE_NONE  },
    [0x84] = { N_JZ      , OTYPE_REL16 , OTYPE_NONE  },
    [0x85] = { N_JNZ     , OTYPE_REL16 , OTYPE_NONE  },
    [0x86] = { N_JNA     , OTYPE_REL16 , OTYPE_NONE  },
    [0x87] = { N_JA      , OTYPE_REL16 , OTYPE_NONE  },
    [0x88] = { N_JS      , OTYPE_REL16 , OTYPE_NONE  },
    [0x89] = { N_JNS     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8a] = { N_JPE     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8b] = { N_JPO     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8c] = { N_JL      , OTYPE_REL16 , OTYPE_NONE  },
    [0x8d] = { N_JNL     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8e] = { N_JNG     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8f] = { N_JG      , OTYPE_REL16 , OTYPE_NONE  },
};

U1 Decode(U2 offset)
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
    struct InstructionInfo* II;
    if (inst == 0x0F) {
        inst = READ_U1(offset);
        ++offset;
        II = &Instructions0F[inst];
        if (!II->name) {
            Error("Instruction not implemented: 0F %02X\n", inst);
        }
    } else {
        II = &Instructions[inst];
        if (!II->name) {
            Error("Instruction not implemented: %02X\n", inst);
        }
    }

    if (OTYPE_HAS_MODRM(II->op1) || OTYPE_HAS_MODRM(II->op2)) {
        ModRM = READ_U1(offset);
        ++offset;

        if ((ModRM & 0xC0) == 0xC0) {
            const U1 rm = OTYPE_HAS_MODRM(II->op1) && (II->op1 == OTYPE_RM8 || II->op1 == OTYPE_RM16) ? II->op1 : II->op2;
            strcpy(RMText, RegNames[((ModRM>>3)&7) + (rm == OTYPE_RM8 ? R_AL : R_AX)]);
        } else if ((ModRM & 0xC7) == 6) {
            snprintf(RMText, OP_TEXT_MAX, "0x%04X", READ_U2(offset));
            offset += 2;
        } else {
            Error("Not implemented: ModRM %02X", ModRM);
        }
    }

    if (II->op1 == OTYPE_IMM8 || II->op2 == OTYPE_IMM8
        || II->op1 == OTYPE_REL8 || II->op2 == OTYPE_REL8) {
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
        printf("%02X", READ_U1(i));
    }

    const U1 InstMaxLen = 8;
    const U2 InstLen = offset-start;
    assert(InstLen < InstMaxLen);

    CurrentAddress += InstLen; // Increment before resolving Rel8/Rel16

    for (U2 i = InstLen; i < InstMaxLen; ++i) {
        printf("  ");
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

    return (U1)InstLen;
}

int main(void)
{
    ReadFile("../tests/t04.ref");
    CurrentAddress = 0x0100;
    for (U2 offset = 0; offset < FileSize;) {
        printf("%04X ", CurrentAddress);
        const U1 len = Decode(offset);
        offset += len;
    }
    free(File);
}
