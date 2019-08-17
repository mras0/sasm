#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <stdbool.h>
#include <assert.h>

typedef signed char S1;
typedef unsigned char U1;
typedef unsigned short U2;
typedef short S2;
typedef unsigned int U4;
typedef int S4;

struct InstructionInfo {
    const char* name;
    U1 op1;
    U1 op2;
};

enum {
    R_AL, R_CL, R_DL, R_BL, R_AH, R_CH, R_DH, R_BH,
    R_AX, R_CX, R_DX, R_BX, R_SP, R_BP, R_SI, R_DI,
    R_ES, R_CS, R_SS, R_DS,
};

enum {
    // R_AL .. R_DS
    OTYPE_1 = R_DS+1,        // Constant 1
    OTYPE_IMM8 = 0x20,
    OTYPE_IMM16,
    OTYPE_REL8,
    OTYPE_REL16,
    OTYPE_MOFF,
    OTYPE_RM8 = 0x40,        // r/m part of MODRM
    OTYPE_RM16,
    OTYPE_R8,                // /r of ModRM
    OTYPE_R16,
    OTYPE_SREG,
    OTYPE_RTAB,              // /r selects opcode from table

    OTYPE_NONE = 0xFF
};

#define OTYPE_IS_REG(ot) ((U2)(ot) <= R_DS)
#define OTYPE_HAS_MODRM(ot) ((ot) != OTYPE_NONE && ((ot) & 0x40))

#define PREFIX_LOCK 0x01
#define PREFIX_F2   0x02 // REPNE/REPNZ
#define PREFIX_F3   0x04 // REP/REPZ
#define PREFIX_ES   0x08
#define PREFIX_CS   0x10
#define PREFIX_SS   0x20
#define PREFIX_DS   0x40

#define OP_TEXT_MAX 20

static const char RegNames[][3] = {
    "AL", "CL", "DL", "BL", "AH", "CH", "DH", "BH",
    "AX", "CX", "DX", "BX", "SP", "BP", "SI", "DI",
    "ES", "CS", "SS", "DS",
};

static const char* MemNames[8] = {
    "BX+SI", "BX+DI", "BP+SI", "BP+DI",
    "SI",    "DI",    "BP",    "BX"
};

static const char* PrefixNames[7] = {
    "LOCK", "REPNZ", "REP", "ES", "CS", "SS", "DS"
};

static U1* File;
static U2 FileSize;
static U2 Offset;
static U2 CurrentAddress;
static const struct InstructionInfo* II;
static U1 Prefixes;
static bool HasModRM;
static U1 InstructionBytes[2];
static U1 ModRM;
static U1 ImmSize;
static union {
    U1 I8;
    U2 I16;
} Immediate;
static char RMText[OP_TEXT_MAX];
static const char* PrefixText;

U1 ReadU1(void)
{
    if (Offset >= FileSize) {
        return 0xFF;
    }
    return File[Offset++];
}

U2 ReadU2(void)
{
    const U1 low = ReadU1();
    return low | ReadU1() << 8;
}

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

static void GetOp(U1 op)
{
    if (OTYPE_IS_REG(op)) {
        printf("%s", RegNames[op]);
        return;
    }
    switch (op) {
    case OTYPE_1:
        printf("1");
        break;
    case OTYPE_IMM8:
        printf("0x%02X", Immediate.I8);
        break;
    case OTYPE_IMM16:
        printf("0x%04X", Immediate.I16);
        break;
    case OTYPE_REL8:
        printf("0x%04X", (U2)(CurrentAddress+(S1)Immediate.I8));
        break;
    case OTYPE_REL16:
        printf("0x%04X", (U2)(CurrentAddress+Immediate.I16));
        break;
    case OTYPE_RM8:
    case OTYPE_RM16:
    case OTYPE_MOFF:
        printf("%s", RMText);
        break;
    case OTYPE_R8:
        printf("%s", RegNames[R_AL + ((ModRM>>3)&7)]);
        break;
    case OTYPE_R16:
        printf("%s", RegNames[R_AX + ((ModRM>>3)&7)]);
        break;
    case OTYPE_SREG:
        printf("%s", RegNames[R_ES + ((ModRM>>3)&7)]);
        break;
    default:
        Error("Unimplemented Optype: %02X\n", op);
    }
}

#define N_REP    "REP"
#define N_REPZ   "REPZ"
#define N_REPNZ  "REPNZ"

#define N_AAA    "AAA"
#define N_AAM    "AAM"
#define N_AAS    "AAS"
#define N_AAD    "AAD"
#define N_DAA    "DAA"
#define N_DAS    "DAS"
#define N_JCXZ   "JCXZ"
#define N_LOOP   "LOOP"
#define N_LOOPE  "LOOPE"
#define N_LOOPNE "LOOPNE"
#define N_SAHF   "SAHF"
#define N_LAHF   "LAHF"
#define N_XLATB  "XLATB"
#define N_IN     "IN"
#define N_OUT    "OUT"
#define N_INSB   "INSB"
#define N_INSW   "INSW"
#define N_OUTSB  "OUTSB"
#define N_OUTSW  "OUTSW"
#define N_ADC    "ADC"
#define N_ADD    "ADD"
#define N_AND    "AND"
#define N_CALL   "CALL"
#define N_CALLF  "CALLF"
#define N_CBW    "CBW"
#define N_CLC    "CLC"
#define N_CLD    "CLD"
#define N_CLI    "CLI"
#define N_CMC    "CMC"
#define N_CMP    "CMP"
#define N_CMPSB  "CMPSB"
#define N_CMPSW  "CMPSW"
#define N_CWD    "CWD"
#define N_DEC    "DEC"
#define N_DIV    "DIV"
#define N_HLT    "HLT"
#define N_IDIV   "IDIV"
#define N_IMUL   "IMUL"
#define N_INC    "INC"
#define N_INT    "INT"
#define N_INT3   "INT3"
#define N_INTO   "INTO"
#define N_IRET   "IRET"
#define N_LDS    "LDS"
#define N_LEA    "LEA"
#define N_LES    "LES"
#define N_LODSB  "LODSB"
#define N_LODSW  "LODSW"
#define N_MOV    "MOV"
#define N_MOVSB  "MOVSB"
#define N_MOVSW  "MOVSW"
#define N_MOVSX  "MOVSX"
#define N_MOVZX  "MOVZX"
#define N_MUL    "MUL"
#define N_NEG    "NEG"
#define N_NOP    "NOP"
#define N_NOT    "NOT"
#define N_OR     "OR"
#define N_POP    "POP"
#define N_POPA   "POPA"
#define N_POPF   "POPF"
#define N_PUSH   "PUSH"
#define N_PUSHA  "PUSHA"
#define N_PUSHF  "PUSHF"
#define N_RCL    "RCL"
#define N_RCR    "RCR"
#define N_RET    "RET"
#define N_RETF   "RETF"
#define N_ROL    "ROL"
#define N_ROR    "ROR"
#define N_SAR    "SAR"
#define N_SBB    "SBB"
#define N_SCASB  "SCASB"
#define N_SCASW  "SCASW"
#define N_SHL    "SHL"
#define N_SHR    "SHR"
#define N_STC    "STC"
#define N_STD    "STD"
#define N_STI    "STI"
#define N_STOSB  "STOSB"
#define N_STOSW  "STOSW"
#define N_SUB    "SUB"
#define N_TEST   "TEST"
#define N_XCHG   "XCHG"
#define N_XOR    "XOR"

#define N_JMP    "JMP"
#define N_JMPF   "JMPF"
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

static const struct InstructionInfo Instructions_80[8] = {
    [0x00] = { N_ADD    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x01] = { N_OR     , OTYPE_RM8   , OTYPE_IMM8  },
    [0x02] = { N_ADC    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x03] = { N_SBB    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x04] = { N_AND    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x05] = { N_SUB    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x06] = { N_XOR    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x07] = { N_CMP    , OTYPE_RM8   , OTYPE_IMM8  },
};

static const struct InstructionInfo Instructions_81[8] = {
    [0x00] = { N_ADD    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x01] = { N_OR     , OTYPE_RM16  , OTYPE_IMM16 },
    [0x02] = { N_ADC    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x03] = { N_SBB    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x04] = { N_AND    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x05] = { N_SUB    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x06] = { N_XOR    , OTYPE_RM16  , OTYPE_IMM16 },
    [0x07] = { N_CMP    , OTYPE_RM16  , OTYPE_IMM16 },
};

static const struct InstructionInfo Instructions_83[8] = {
    [0x00] = { N_ADD    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x01] = { N_OR     , OTYPE_RM16  , OTYPE_IMM8  },
    [0x02] = { N_ADC    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x03] = { N_SBB    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x04] = { N_AND    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x05] = { N_SUB    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x06] = { N_XOR    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x07] = { N_CMP    , OTYPE_RM16  , OTYPE_IMM8  },
};

static const struct InstructionInfo Instructions_C0[8] = {
    [0x00] = { N_ROL    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x01] = { N_ROR    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x02] = { N_RCL    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x03] = { N_RCR    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x04] = { N_SHL    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x05] = { N_SHR    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x06] = { N_SHL    , OTYPE_RM8   , OTYPE_IMM8  },
    [0x07] = { N_SAR    , OTYPE_RM8   , OTYPE_IMM8  },
};

static const struct InstructionInfo Instructions_C1[8] = {
    [0x00] = { N_ROL    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x01] = { N_ROR    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x02] = { N_RCL    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x03] = { N_RCR    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x04] = { N_SHL    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x05] = { N_SHR    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x06] = { N_SHL    , OTYPE_RM16  , OTYPE_IMM8  },
    [0x07] = { N_SAR    , OTYPE_RM16  , OTYPE_IMM8  },
};

static const struct InstructionInfo Instructions_C6[8] = {
    [0x00] = { N_MOV    , OTYPE_RM8   , OTYPE_IMM8  },
};

static const struct InstructionInfo Instructions_C7[8] = {
    [0x00] = { N_MOV    , OTYPE_RM16  , OTYPE_IMM16 },
};

static const struct InstructionInfo Instructions_D0[8] = {
    [0x00] = { N_ROL    , OTYPE_RM8   , OTYPE_1     },
    [0x01] = { N_ROR    , OTYPE_RM8   , OTYPE_1     },
    [0x02] = { N_RCL    , OTYPE_RM8   , OTYPE_1     },
    [0x03] = { N_RCR    , OTYPE_RM8   , OTYPE_1     },
    [0x04] = { N_SHL    , OTYPE_RM8   , OTYPE_1     },
    [0x05] = { N_SHR    , OTYPE_RM8   , OTYPE_1     },
    [0x06] = { N_SHL    , OTYPE_RM8   , OTYPE_1     },
    [0x07] = { N_SAR    , OTYPE_RM8   , OTYPE_1     },
};

static const struct InstructionInfo Instructions_D1[8] = {
    [0x00] = { N_ROL    , OTYPE_RM16  , OTYPE_1     },
    [0x01] = { N_ROR    , OTYPE_RM16  , OTYPE_1     },
    [0x02] = { N_RCL    , OTYPE_RM16  , OTYPE_1     },
    [0x03] = { N_RCR    , OTYPE_RM16  , OTYPE_1     },
    [0x04] = { N_SHL    , OTYPE_RM16  , OTYPE_1     },
    [0x05] = { N_SHR    , OTYPE_RM16  , OTYPE_1     },
    [0x06] = { N_SHL    , OTYPE_RM16  , OTYPE_1     },
    [0x07] = { N_SAR    , OTYPE_RM16  , OTYPE_1     },
};

static const struct InstructionInfo Instructions_D2[8] = {
    [0x00] = { N_ROL    , OTYPE_RM8   , R_CL        },
    [0x01] = { N_ROR    , OTYPE_RM8   , R_CL        },
    [0x02] = { N_RCL    , OTYPE_RM8   , R_CL        },
    [0x03] = { N_RCR    , OTYPE_RM8   , R_CL        },
    [0x04] = { N_SHL    , OTYPE_RM8   , R_CL        },
    [0x05] = { N_SHR    , OTYPE_RM8   , R_CL        },
    [0x06] = { N_SHL    , OTYPE_RM8   , R_CL        },
    [0x07] = { N_SAR    , OTYPE_RM8   , R_CL        },
};

static const struct InstructionInfo Instructions_D3[8] = {
    [0x00] = { N_ROL    , OTYPE_RM16  , R_CL        },
    [0x01] = { N_ROR    , OTYPE_RM16  , R_CL        },
    [0x02] = { N_RCL    , OTYPE_RM16  , R_CL        },
    [0x03] = { N_RCR    , OTYPE_RM16  , R_CL        },
    [0x04] = { N_SHL    , OTYPE_RM16  , R_CL        },
    [0x05] = { N_SHR    , OTYPE_RM16  , R_CL        },
    [0x06] = { N_SHL    , OTYPE_RM16  , R_CL        },
    [0x07] = { N_SAR    , OTYPE_RM16  , R_CL        },
};

static const struct InstructionInfo Instructions_F6[8] = {
    [0x00] = { N_TEST   , OTYPE_RM8   , OTYPE_IMM8  },
    [0x01] = { N_TEST   , OTYPE_RM8   , OTYPE_IMM8  },
    [0x02] = { N_NOT    , OTYPE_RM8   , OTYPE_NONE  },
    [0x03] = { N_NEG    , OTYPE_RM8   , OTYPE_NONE  },
    [0x04] = { N_MUL    , OTYPE_RM8   , OTYPE_NONE  },
    [0x05] = { N_IMUL   , OTYPE_RM8   , OTYPE_NONE  },
    [0x06] = { N_DIV    , OTYPE_RM8   , OTYPE_NONE  },
    [0x07] = { N_IDIV   , OTYPE_RM8   , OTYPE_NONE  },
};

static const struct InstructionInfo Instructions_F7[8] = {
    [0x00] = { N_TEST   , OTYPE_RM16  , OTYPE_IMM16 },
    [0x01] = { N_TEST   , OTYPE_RM16  , OTYPE_IMM16 },
    [0x02] = { N_NOT    , OTYPE_RM16  , OTYPE_NONE  },
    [0x03] = { N_NEG    , OTYPE_RM16  , OTYPE_NONE  },
    [0x04] = { N_MUL    , OTYPE_RM16  , OTYPE_NONE  },
    [0x05] = { N_IMUL   , OTYPE_RM16  , OTYPE_NONE  },
    [0x06] = { N_DIV    , OTYPE_RM16  , OTYPE_NONE  },
    [0x07] = { N_IDIV   , OTYPE_RM16  , OTYPE_NONE  },
};

static const struct InstructionInfo Instructions_FE[8] = {
    [0x00] = { N_INC    , OTYPE_RM8   , OTYPE_NONE  },
    [0x01] = { N_DEC    , OTYPE_RM8   , OTYPE_NONE  },
};

static const struct InstructionInfo Instructions_FF[8] = {
    [0x00] = { N_INC    , OTYPE_RM16  , OTYPE_NONE  },
    [0x01] = { N_DEC    , OTYPE_RM16  , OTYPE_NONE  },
    [0x02] = { N_CALL   , OTYPE_RM16  , OTYPE_NONE  },
    [0x03] = { N_CALLF  , OTYPE_RM16  , OTYPE_NONE  },
    [0x04] = { N_JMP    , OTYPE_RM16  , OTYPE_NONE  },
    [0x05] = { N_JMPF   , OTYPE_RM16  , OTYPE_NONE  },
    [0x06] = { N_PUSH   , OTYPE_RM16  , OTYPE_NONE  },
};

static const struct InstructionInfo Instructions[256] = {
    // 0x00
    [0x00] = { N_ADD    , OTYPE_RM8   , OTYPE_R8    },
    [0x01] = { N_ADD    , OTYPE_RM16  , OTYPE_R16   },
    [0x02] = { N_ADD    , OTYPE_R8    , OTYPE_RM8   },
    [0x03] = { N_ADD    , OTYPE_R16   , OTYPE_RM16  },
    [0x04] = { N_ADD    , R_AL        , OTYPE_IMM8  },
    [0x05] = { N_ADD    , R_AX        , OTYPE_IMM16 },
    [0x06] = { N_PUSH   , R_ES        , OTYPE_NONE  },
    [0x07] = { N_POP    , R_ES        , OTYPE_NONE  },
    [0x08] = { N_OR     , OTYPE_RM8   , OTYPE_R8    },
    [0x09] = { N_OR     , OTYPE_RM16  , OTYPE_R16   },
    [0x0A] = { N_OR     , OTYPE_R8    , OTYPE_RM8   },
    [0x0B] = { N_OR     , OTYPE_R16   , OTYPE_RM16  },
    [0x0C] = { N_OR     , R_AL        , OTYPE_IMM8  },
    [0x0D] = { N_OR     , R_AX        , OTYPE_IMM16 },
    [0x0E] = { N_PUSH   , R_CS        , OTYPE_NONE  },
    // 0x0F -> Two byte instruction
    // 0x10
    [0x10] = { N_ADC    , OTYPE_RM8   , OTYPE_R8    },
    [0x11] = { N_ADC    , OTYPE_RM16  , OTYPE_R16   },
    [0x12] = { N_ADC    , OTYPE_R8    , OTYPE_RM8   },
    [0x13] = { N_ADC    , OTYPE_R16   , OTYPE_RM16  },
    [0x14] = { N_ADC    , R_AL        , OTYPE_IMM8  },
    [0x15] = { N_ADC    , R_AX        , OTYPE_IMM16 },
    [0x16] = { N_PUSH   , R_SS        , OTYPE_NONE  },
    [0x17] = { N_POP    , R_SS        , OTYPE_NONE  },
    [0x18] = { N_SBB    , OTYPE_RM8   , OTYPE_R8    },
    [0x19] = { N_SBB    , OTYPE_RM16  , OTYPE_R16   },
    [0x1A] = { N_SBB    , OTYPE_R8    , OTYPE_RM8   },
    [0x1B] = { N_SBB    , OTYPE_R16   , OTYPE_RM16  },
    [0x1C] = { N_SBB    , R_AL        , OTYPE_IMM8  },
    [0x1D] = { N_SBB    , R_AX        , OTYPE_IMM16 },
    [0x1E] = { N_PUSH   , R_DS        , OTYPE_NONE  },
    [0x1F] = { N_POP    , R_DS        , OTYPE_NONE  },
    // 0x20
    [0x20] = { N_AND    , OTYPE_RM8   , OTYPE_R8    },
    [0x21] = { N_AND    , OTYPE_RM16  , OTYPE_R16   },
    [0x22] = { N_AND    , OTYPE_R8    , OTYPE_RM8   },
    [0x23] = { N_AND    , OTYPE_R16   , OTYPE_RM16  },
    [0x24] = { N_AND    , R_AL        , OTYPE_IMM8  },
    [0x25] = { N_AND    , R_AX        , OTYPE_IMM16 },
    [0x27] = { N_DAA    , OTYPE_NONE  , OTYPE_NONE  },
    [0x28] = { N_SUB    , OTYPE_RM8   , OTYPE_R8    },
    [0x29] = { N_SUB    , OTYPE_RM16  , OTYPE_R16   },
    [0x2A] = { N_SUB    , OTYPE_R8    , OTYPE_RM8   },
    [0x2B] = { N_SUB    , OTYPE_R16   , OTYPE_RM16  },
    [0x2C] = { N_SUB    , R_AL        , OTYPE_IMM8  },
    [0x2D] = { N_SUB    , R_AX        , OTYPE_IMM16 },
    [0x2F] = { N_DAS    , OTYPE_NONE  , OTYPE_NONE  },
    // 0x30
    [0x30] = { N_XOR    , OTYPE_RM8   , OTYPE_R8    },
    [0x31] = { N_XOR    , OTYPE_RM16  , OTYPE_R16   },
    [0x32] = { N_XOR    , OTYPE_R8    , OTYPE_RM8   },
    [0x33] = { N_XOR    , OTYPE_R16   , OTYPE_RM16  },
    [0x34] = { N_XOR    , R_AL        , OTYPE_IMM8  },
    [0x35] = { N_XOR    , R_AX        , OTYPE_IMM16 },
    [0x37] = { N_AAA    , OTYPE_NONE  , OTYPE_NONE  },
    [0x38] = { N_CMP    , OTYPE_RM8   , OTYPE_R8    },
    [0x39] = { N_CMP    , OTYPE_RM16  , OTYPE_R16   },
    [0x3A] = { N_CMP    , OTYPE_R8    , OTYPE_RM8   },
    [0x3B] = { N_CMP    , OTYPE_R16   , OTYPE_RM16  },
    [0x3C] = { N_CMP    , R_AL        , OTYPE_IMM8  },

    [0x3D] = { N_CMP    , R_AX        , OTYPE_IMM16 },
    [0x3F] = { N_AAS    , OTYPE_NONE  , OTYPE_NONE  },
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
    [0x68] = { N_PUSH   , OTYPE_IMM16 , OTYPE_NONE  },
    [0x6A] = { N_PUSH   , OTYPE_IMM8  , OTYPE_NONE  },
    [0x6C] = { N_INSB   , OTYPE_NONE  , OTYPE_NONE  },
    [0x6D] = { N_INSW   , OTYPE_NONE  , OTYPE_NONE  },
    [0x6E] = { N_OUTSB  , OTYPE_NONE  , OTYPE_NONE  },
    [0x6F] = { N_OUTSW  , OTYPE_NONE  , OTYPE_NONE  },
    // 0x70
    [0x70] = { N_JO      , OTYPE_REL8  , OTYPE_NONE  },
    [0x71] = { N_JNO     , OTYPE_REL8  , OTYPE_NONE  },
    [0x72] = { N_JC      , OTYPE_REL8  , OTYPE_NONE  },
    [0x73] = { N_JNC     , OTYPE_REL8  , OTYPE_NONE  },
    [0x74] = { N_JZ      , OTYPE_REL8  , OTYPE_NONE  },
    [0x75] = { N_JNZ     , OTYPE_REL8  , OTYPE_NONE  },
    [0x76] = { N_JNA     , OTYPE_REL8  , OTYPE_NONE  },
    [0x77] = { N_JA      , OTYPE_REL8  , OTYPE_NONE  },
    [0x78] = { N_JS      , OTYPE_REL8  , OTYPE_NONE  },
    [0x79] = { N_JNS     , OTYPE_REL8  , OTYPE_NONE  },
    [0x7A] = { N_JPE     , OTYPE_REL8  , OTYPE_NONE  },
    [0x7B] = { N_JPO     , OTYPE_REL8  , OTYPE_NONE  },
    [0x7C] = { N_JL      , OTYPE_REL8  , OTYPE_NONE  },
    [0x7D] = { N_JNL     , OTYPE_REL8  , OTYPE_NONE  },
    [0x7E] = { N_JNG     , OTYPE_REL8  , OTYPE_NONE  },
    [0x7F] = { N_JG      , OTYPE_REL8  , OTYPE_NONE  },
    // 0x80
    [0x80] = { (char*)Instructions_80 , OTYPE_RTAB  , OTYPE_NONE  },
    [0x81] = { (char*)Instructions_81 , OTYPE_RTAB  , OTYPE_NONE  },
    [0x82] = { (char*)Instructions_80 , OTYPE_RTAB  , OTYPE_NONE  },
    [0x83] = { (char*)Instructions_83 , OTYPE_RTAB  , OTYPE_NONE  },
    [0x84] = { N_TEST   , OTYPE_RM8   , OTYPE_R8    },
    [0x85] = { N_TEST   , OTYPE_RM16  , OTYPE_R16   },
    [0x86] = { N_XCHG   , OTYPE_R8    , OTYPE_RM8   },
    [0x87] = { N_XCHG   , OTYPE_R16   , OTYPE_RM16  },
    [0x88] = { N_MOV    , OTYPE_RM8   , OTYPE_R8    },
    [0x89] = { N_MOV    , OTYPE_RM16  , OTYPE_R16   },
    [0x8A] = { N_MOV    , OTYPE_R8    , OTYPE_RM8   },
    [0x8B] = { N_MOV    , OTYPE_R16   , OTYPE_RM16  },
    [0x8C] = { N_MOV    , OTYPE_RM16  , OTYPE_SREG  },
    [0x8D] = { N_LEA    , OTYPE_R16   , OTYPE_RM16  },
    [0x8E] = { N_MOV    , OTYPE_SREG  , OTYPE_RM16  },
    // 0x90
    [0x90] = { N_NOP    , OTYPE_NONE  , OTYPE_NONE  },
    [0x98] = { N_CBW    , OTYPE_NONE  , OTYPE_NONE  },
    [0x99] = { N_CWD    , OTYPE_NONE  , OTYPE_NONE  },
    [0x9C] = { N_PUSHF  , OTYPE_NONE  , OTYPE_NONE  },
    [0x9D] = { N_POPF   , OTYPE_NONE  , OTYPE_NONE  },
    [0x9E] = { N_SAHF   , OTYPE_NONE  , OTYPE_NONE  },
    [0x9F] = { N_LAHF   , OTYPE_NONE  , OTYPE_NONE  },
    // 0xA0
    [0xA0] = { N_MOV    , R_AL        , OTYPE_MOFF  },
    [0xA1] = { N_MOV    , R_AX        , OTYPE_MOFF  },
    [0xA2] = { N_MOV    , OTYPE_MOFF  , R_AL        },
    [0xA3] = { N_MOV    , OTYPE_MOFF  , R_AX        },
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
    [0xC0] = { (char*)Instructions_C0 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xC1] = { (char*)Instructions_C1 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xC2] = { N_RET    , OTYPE_IMM16 , OTYPE_NONE  },
    [0xC3] = { N_RET    , OTYPE_NONE  , OTYPE_NONE  },
    [0xC4] = { N_LES    , OTYPE_R16   , OTYPE_RM16  },
    [0xC5] = { N_LDS    , OTYPE_R16   , OTYPE_RM16  },
    [0xC6] = { (char*)Instructions_C6 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xC7] = { (char*)Instructions_C7 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xCA] = { N_RETF   , OTYPE_IMM16 , OTYPE_NONE  },
    [0xCB] = { N_RETF   , OTYPE_NONE  , OTYPE_NONE  },
    [0xCC] = { N_INT3   , OTYPE_NONE  , OTYPE_NONE  },
    [0xCD] = { N_INT    , OTYPE_IMM8  , OTYPE_NONE  },
    [0xCE] = { N_INTO   , OTYPE_NONE  , OTYPE_NONE  },
    [0xCF] = { N_IRET   , OTYPE_NONE  , OTYPE_NONE  },
    // 0xD0
    [0xD0] = { (char*)Instructions_D0 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xD1] = { (char*)Instructions_D1 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xD2] = { (char*)Instructions_D2 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xD3] = { (char*)Instructions_D3 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xD4] = { N_AAM     , OTYPE_IMM8  , OTYPE_NONE  },
    [0xD5] = { N_AAD     , OTYPE_IMM8  , OTYPE_NONE  },
    [0xD7] = { N_XLATB   , OTYPE_NONE  , OTYPE_NONE  },
    // 0xE0
    [0xE0] = { N_LOOPNE  , OTYPE_REL8  , OTYPE_NONE  },
    [0xE1] = { N_LOOPE   , OTYPE_REL8  , OTYPE_NONE  },
    [0xE2] = { N_LOOP    , OTYPE_REL8  , OTYPE_NONE  },
    [0xE3] = { N_JCXZ    , OTYPE_REL8  , OTYPE_NONE  },
    [0xE4] = { N_IN      , R_AL        , OTYPE_IMM8  },
    [0xE5] = { N_IN      , R_AX        , OTYPE_IMM8  },
    [0xE6] = { N_OUT     , OTYPE_IMM8  , R_AL        },
    [0xE7] = { N_OUT     , OTYPE_IMM8  , R_AX        },
    [0xE8] = { N_CALL    , OTYPE_REL16 , OTYPE_NONE  },
    [0xE9] = { N_JMP     , OTYPE_REL16 , OTYPE_NONE  },
    [0xEB] = { N_JMP     , OTYPE_REL8  , OTYPE_NONE  },
    [0xEC] = { N_IN      , R_AL        , R_DX        },
    [0xED] = { N_IN      , R_AX        , R_DX        },
    [0xEE] = { N_OUT     , R_DX        , R_AL        },
    [0xEF] = { N_OUT     , R_DX        , R_AX        },
    // 0xF0
    [0xF4] = { N_HLT    , OTYPE_NONE  , OTYPE_NONE  },
    [0xF5] = { N_CMC    , OTYPE_NONE  , OTYPE_NONE  },
    [0xF6] = { (char*)Instructions_F6 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xF7] = { (char*)Instructions_F7 , OTYPE_RTAB  , OTYPE_NONE  },
    [0xF8] = { N_CLC    , OTYPE_NONE  , OTYPE_NONE  },
    [0xF9] = { N_STC    , OTYPE_NONE  , OTYPE_NONE  },
    [0xFA] = { N_CLI    , OTYPE_NONE  , OTYPE_NONE  },
    [0xFB] = { N_STI    , OTYPE_NONE  , OTYPE_NONE  },
    [0xFC] = { N_CLD    , OTYPE_NONE  , OTYPE_NONE  },
    [0xFD] = { N_STD    , OTYPE_NONE  , OTYPE_NONE  },
    [0xFE] = { (char*)Instructions_FE , OTYPE_RTAB  , OTYPE_NONE  },
    [0xFF] = { (char*)Instructions_FF , OTYPE_RTAB  , OTYPE_NONE  },
};

static const struct InstructionInfo Instructions0F[256] = {
    // 0x80
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
    [0x8A] = { N_JPE     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8B] = { N_JPO     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8C] = { N_JL      , OTYPE_REL16 , OTYPE_NONE  },
    [0x8D] = { N_JNL     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8E] = { N_JNG     , OTYPE_REL16 , OTYPE_NONE  },
    [0x8F] = { N_JG      , OTYPE_REL16 , OTYPE_NONE  },
    // 0xB0
    [0xB6] = { N_MOVZX   , OTYPE_R16   , OTYPE_RM8   },
    [0xBE] = { N_MOVSX   , OTYPE_R16   , OTYPE_RM8   },
};

static void ReadPrefixes(void)
{
    Prefixes = 0;
    for (;; ++Offset) {
        const U1 b = File[Offset];
        if (b == 0xF0)      Prefixes |= PREFIX_LOCK;
        else if (b == 0xF2) Prefixes |= PREFIX_F2;
        else if (b == 0xF3) Prefixes |= PREFIX_F3;
        else if (b == 0x26) Prefixes |= PREFIX_ES;
        else if (b == 0x2E) Prefixes |= PREFIX_CS;
        else if (b == 0x36) Prefixes |= PREFIX_SS;
        else if (b == 0x3E) Prefixes |= PREFIX_DS;
        else break;
    }
}

static void GetRMText(void)
{
    U1 SegOverride = 0;
    const char* SegOverrideStr = "";
    if (Prefixes & PREFIX_ES) {
        SegOverride = PREFIX_ES;
        SegOverrideStr = "ES:";
    } else if (Prefixes & PREFIX_CS) {
        SegOverride = PREFIX_CS;
        SegOverrideStr = "CS:";
    } else if (Prefixes & PREFIX_SS) {
        SegOverride = PREFIX_SS;
        SegOverrideStr = "SS:";
    } else if (Prefixes & PREFIX_DS) {
        SegOverride = PREFIX_DS;
        SegOverrideStr = "DS:";
    }

    if (HasModRM) {
        U1 rm, other;
        if (OTYPE_HAS_MODRM(II->op1) && (II->op1 == OTYPE_RM8 || II->op1 == OTYPE_RM16)) {
            rm = II->op1;
            other = II->op2;
        } else {
            rm = II->op2;
            other = II->op1;
        }

        const char* sz = !ImmSize && other != OTYPE_NONE ? "" : rm == OTYPE_RM8 ? "BYTE " : "WORD ";
        if ((ModRM & 0xC0) == 0xC0) {
            strcpy(RMText, RegNames[(ModRM&7) + (rm == OTYPE_RM8 ? R_AL : R_AX)]);
        } else if ((ModRM & 0xC7) == 6) {
            snprintf(RMText, OP_TEXT_MAX, "%s[%s0x%04X]", sz, SegOverrideStr, ReadU2());
            Prefixes &= ~SegOverride; // Prefix used
        } else {
            char temp[8]="";
            if (ModRM>>6 == 1) {
                snprintf(temp, sizeof(temp), "+0x%04X", (U2)(S1)ReadU1());
            } else if (ModRM>>6 == 2) {
                snprintf(temp, sizeof(temp), "+0x%04X", ReadU2());
            }
            snprintf(RMText, OP_TEXT_MAX, "%s[%s%s%s]", sz, SegOverrideStr, MemNames[ModRM&7], temp);
            Prefixes &= ~SegOverride; // Prefix used
        }
    }

    if (II->op1 == OTYPE_MOFF || II->op2 == OTYPE_MOFF) {
        snprintf(RMText, OP_TEXT_MAX, "[%s0x%04X]", SegOverrideStr, ReadU2());
        Prefixes &= ~SegOverride; // Prefix used
    }
}

static void GetPrefixText()
{
    const U1 inst = InstructionBytes[0];
    const bool IsCmpsOrScas = inst == 0xA6 || inst == 0xA7 || inst == 0xAE || inst == 0xAF;
    PrefixText = NULL;
    if (Prefixes & PREFIX_F2) {
        if (IsCmpsOrScas) {
            Prefixes &= ~PREFIX_F2;
            PrefixText = N_REPNZ;
        }
    } else if (Prefixes & PREFIX_F3) {
        if (inst == 0xA4 || inst == 0xA5 || inst == 0xAA || inst == 0xAB || inst == 0xAC || inst == 0xAd
            || inst == 0x6C || inst == 0x6D || inst == 0x6E || inst == 0x6F) {
            Prefixes &= ~PREFIX_F3;
            PrefixText = N_REP;
        } else if (IsCmpsOrScas) {
            Prefixes &= ~PREFIX_F3;
            PrefixText = N_REPZ;
        }
    }
}

static void GetInstruction(void)
{
    ReadPrefixes();

    HasModRM = false;

    InstructionBytes[0] = ReadU1();
    if (InstructionBytes[0] == 0x0F) {
        InstructionBytes[1] = ReadU1();
        II = &Instructions0F[InstructionBytes[1]];
        if (!II->name) {
            //fprintf(stderr, "%04X: Instruction not implemented: 0F %02X\n", CurrentAddress, InstructionBytes[1]);
            return;
        }
    } else {
        II = &Instructions[InstructionBytes[0]];
        if (!II->name) {
            //fprintf(stderr, "%04X: Instruction not implemented: %02X\n", CurrentAddress, InstructionBytes[0]);
            return;
        }
    }

    HasModRM = OTYPE_HAS_MODRM(II->op1) || OTYPE_HAS_MODRM(II->op2);
    if (HasModRM) {
        ModRM = ReadU1();
    }

    if (II->op1 == OTYPE_RTAB) {
        assert(II->name && HasModRM);
        II = &((const struct InstructionInfo*)Instructions[InstructionBytes[0]].name)[(ModRM>>3)&7];
        if (!II->name) {
            //fprintf(stderr, "%04X: Instruction not implemented: %02X /%d\n", CurrentAddress, InstructionBytes[0], (ModRM>>3)&7);
            return;
        }
    }

    // Determine immediate size before GetRMText()
    ImmSize = 0;
    if (II->op1 == OTYPE_IMM8 || II->op2 == OTYPE_IMM8
        || II->op1 == OTYPE_REL8 || II->op2 == OTYPE_REL8) {
        ImmSize = 1;
    } else if (II->op1 == OTYPE_IMM16 || II->op2 == OTYPE_IMM16
        || II->op1 == OTYPE_REL16 || II->op2 == OTYPE_REL16) {
        ImmSize = 2;
    }

    GetRMText();

    if (ImmSize == 1) {
        Immediate.I8 = ReadU1();
    } else if (ImmSize == 2) {
        Immediate.I16 = ReadU2();
    }
}

static void Disasm(void)
{
    const U2 InstStart = Offset;

    GetInstruction();

    const U1 InstMaxLen = 9;
    const U2 InstLen = Offset-InstStart;
    assert(InstLen < InstMaxLen);

    for (U2 i = InstStart; i < Offset; ++i) {
        printf("%02X", File[i]);
    }

    for (U2 i = InstLen; i < InstMaxLen; ++i) {
        printf("  ");
    }

    CurrentAddress += InstLen; // Increment before resolving Rel8/Rel16

    if (II->name) {
        GetPrefixText();
    }

    for (U1 idx = 0; Prefixes; ++idx, Prefixes>>=1) {
        if (Prefixes&1) {
            printf("%s ", PrefixNames[idx]);
        }
    }

    if (!II->name) {
        printf("DB 0x%02X", InstructionBytes[0]);
        if (InstructionBytes[0] == 0x0F) {
            printf(", 0x%02X", InstructionBytes[1]);
        }
        if (HasModRM) {
            printf(", 0x%02X", ModRM);
        }
        printf("\n");
        return;
    }

    if (PrefixText) {
        printf("%s ", PrefixText);
    }
    printf("%s", II->name);
    if (II->op1 != OTYPE_NONE) {
        printf(" ");
        GetOp(II->op1);
        if (II->op2 != OTYPE_NONE) {
            printf(", ");
            GetOp(II->op2);
        }
    }
    printf("\n");
}

int main(int argc, char* argv[])
{
    if (argc < 2) {
        Error("Usage: %s file", argv[0]);
    }
    ReadFile(argv[1]);
    CurrentAddress = 0x0100;
    while (Offset < FileSize) {
        printf("%04X ", CurrentAddress);
        Disasm();
    }
    free(File);
    return 0;
}
