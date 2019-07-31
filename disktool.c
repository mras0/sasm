#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>

typedef signed char S1;
typedef unsigned char U1;
typedef unsigned short U2;
typedef short S2;
typedef unsigned int U4;
typedef int S4;

#pragma pack(push, 1)
// Bios Parameter Block (DOS 3.0)
struct BPB {
    U2 BytesPerSector;     // Bytes per logical sector
    U1 SectorsPerCluster;  // Logical sectors per cluster
    U2 ReservedSectors;    // Number of reserved logical sectors
    U1 NumFats;            // Number of file allocation tables
    U2 MaxRootEntries;     // Maximum number of entries in the root directory
    U2 TotalSectors;       // Total logical sectors (if 0 use extended count)
    U1 MediaDescriptor;
    U2 SectorsPerFat;      // Logical sectors per FAT (if 0 use extended count)
    U2 SectorsPerTrack;    // Pysical sectors per track
    U2 NumHeads;           // Number of Heads
    U2 HiddenSectors;
};

struct DirEntry {
    U1 Name[8+3];
    U1 Attributes;
    U1 Reserved;
    U1 CTimeSecs;
    U2 CTime;
    U2 CDate;
    U2 ADate;
    U2 FirstClusterHi;
    U2 Wtime;
    U2 WDate;
    U2 FirstClusterLo;
    U4 FileSize;
};
#pragma pack(pop)

#define ATTR_R 0x01 // read only
#define ATTR_H 0x02 // hidden
#define ATTR_S 0x04 // system
#define ATTR_L 0x08 // volume label
#define ATTR_D 0x10 // directory
#define ATTR_A 0x20 // archive

#define ENTRY_DELETED 0xE5

#define OEM_NAME_OFFSET     0x03
#define BPB_OFFSET          0x0B

#define MAX_CLUSTER_SIZE    4096

#define MAX_ROOT_ENTRIES    1024

#define NUM_ROOT_SECTORS    ((DiskBPB.MaxRootEntries * sizeof(struct DirEntry) + DiskBPB.BytesPerSector - 1) / DiskBPB.BytesPerSector)
#define FAT_SECTOR          (DiskBPB.ReservedSectors)
#define ROOT_SECTOR         (FAT_SECTOR + DiskBPB.NumFats * DiskBPB.SectorsPerFat)
#define DATA_SECTOR         (ROOT_SECTOR + NUM_ROOT_SECTORS)
#define CLUSTER_MAX         ((DiskBPB.TotalSectors - DATA_SECTOR) / DiskBPB.SectorsPerCluster)
#define CLUSTER_SIZE        (DiskBPB.BytesPerSector * DiskBPB.SectorsPerCluster)

#define BOOT_CODE_OFFSET    (BPB_OFFSET + sizeof(struct BPB))
#define MAX_BOOT_CODE_SIZE  (510 - BOOT_CODE_OFFSET)

static FILE* DiskImg;
static struct BPB DiskBPB;
static U1* FAT;
static U1 ClusterBuffer[MAX_CLUSTER_SIZE];

#ifdef _MSC_VER
__declspec(noreturn)
#endif
void Error(const char* format, ...)
{
    va_list va;
    va_start(va, format);
    vfprintf(stderr, format, va);
    fputs("", stderr);
    va_end(va);
    exit(1);
}

void ReadSector(U4 lba, U1* buf)
{
    assert(DiskBPB.BytesPerSector && DiskBPB.BytesPerSector % 512 == 0 && DiskBPB.BytesPerSector < MAX_CLUSTER_SIZE);
    fseek(DiskImg, lba*DiskBPB.BytesPerSector, SEEK_SET);
    if (!fread(buf, DiskBPB.BytesPerSector, 1, DiskImg) || ferror(DiskImg)) {
        Error("Error reading sector %u", lba);
    }
}

void ReadSectors(U4 lba, U1* buf, U2 count) {
    while (count--) {
        ReadSector(lba, buf);
        buf += DiskBPB.BytesPerSector;
        ++lba;
    }
}

void ReadCluster(U2 cluster, U1* buf)
{
    assert(cluster >= 2 && cluster < CLUSTER_MAX);
    ReadSectors(DATA_SECTOR + (cluster - 2) * DiskBPB.SectorsPerCluster, buf, DiskBPB.SectorsPerCluster);
}

void WriteSector(U4 lba, const U1* buf)
{
    fseek(DiskImg, lba*DiskBPB.BytesPerSector, SEEK_SET);
    if (!fwrite(buf, DiskBPB.BytesPerSector, 1, DiskImg) || ferror(DiskImg)) {
        Error("Error writing sector %u", lba);
    }
}

void WriteSectors(U4 lba, const U1* buf, U2 count)
{
    while (count--) {
        WriteSector(lba, buf);
        buf += DiskBPB.BytesPerSector;
        ++lba;
    }
}

void WriteCluster(U2 cluster, const U1* buf)
{
    assert(cluster >= 2 && cluster < CLUSTER_MAX);
    WriteSectors(DATA_SECTOR + (cluster - 2) * DiskBPB.SectorsPerCluster, buf, DiskBPB.SectorsPerCluster);
}


void ReadBPB(void)
{
    U1 BootSect[512];
    DiskBPB.BytesPerSector = sizeof(BootSect);
    ReadSector(0, BootSect);
    if (BootSect[0] != 0xEB || BootSect[510] != 0x55 || BootSect[511] != 0xAA) {
        Error("Disk is not a valid bootable DOS disk");
    }

    // Little endian machine assumed...
    memcpy(&DiskBPB, BootSect + BPB_OFFSET, sizeof(DiskBPB));
    if (!DiskBPB.BytesPerSector || DiskBPB.BytesPerSector % 512 || DiskBPB.BytesPerSector > MAX_CLUSTER_SIZE) {
        Error("Invalid BPB: BytesPerSector = %u", DiskBPB.BytesPerSector);
    }
    if (!DiskBPB.SectorsPerCluster || DiskBPB.SectorsPerCluster * DiskBPB.BytesPerSector > MAX_CLUSTER_SIZE) {
        Error("Invalid BPB: SectorsPerCluster = %u", DiskBPB.SectorsPerCluster);
    }
    if (!DiskBPB.MaxRootEntries || DiskBPB.MaxRootEntries > MAX_ROOT_ENTRIES) {
        Error("Invalid BPB: MaxRootEntries = %u", DiskBPB.MaxRootEntries);
    }
}

void hexdump(const void* buffer, U2 size)
{
    const U1* d = buffer;
    for (U2 i = 0; i < size;) {
        U2 h = size - i;
        if (h>16) h = 16;
        printf("%04X ", i);
        for (U2 j = 0; j < h; ++j) printf("%02X ", d[i+j]);
        for (U2 j = h; j < 16; ++j) printf("   ");
        printf("  ");
        for (U2 j = 0; j < h; ++j) printf("%c ", d[i+j]>=' ' && d[i+j]<0x80 ? d[i+j] : '.');
        printf("\n");
        i += h;
    }
}

void AllocFAT(void)
{
    FAT = malloc(DiskBPB.SectorsPerFat * DiskBPB.BytesPerSector);
    if (!FAT) {
        Error("Could not alloc FAT");
    }
}

U2 GetFATEntry(U2 cluster)
{
    assert(cluster < CLUSTER_MAX);
    U2 val = *(const U2*)(FAT + cluster * 3 / 2);
    return cluster & 1 ? val >> 4 : val & 0xfff;
}

void SetFATEntry(U2 cluster, U2 value)
{
    assert(cluster < CLUSTER_MAX && value <= 0xfff);
    U1* E = FAT + cluster * 3 / 2;
    if (cluster & 1) {
        E[0] = (E[0] & 0x0F) | (value&0xf)<<4;
        E[1] = (U1)(value >> 4);
    } else {
        E[0] = value & 0xff;
        E[1] = (E[1] & 0xF0) | value>>8;
    }
}

// Returns 0 if none could be found
U2 GetFreeCluster(void)
{
    for (U2 c = 2; c < CLUSTER_MAX; ++c) {
        if (!GetFATEntry(c)) {
            return c;
        }
    }
    return 0;
}

void ReadFAT(void)
{
    AllocFAT();
    ReadSectors(FAT_SECTOR, FAT, DiskBPB.SectorsPerFat);
}

void MountDisk(void)
{
    ReadBPB();
    ReadFAT();
}

void ForEachRootEntry(U1 (*F)(void *, const struct DirEntry*), void* context)
{
    struct DirEntry* RootDir = malloc(sizeof(struct DirEntry) * MAX_ROOT_ENTRIES);
    if (!RootDir) {
        Error("Memory allocation failure");
    }
    ReadSectors(ROOT_SECTOR, (U1*)RootDir, NUM_ROOT_SECTORS);
    for (U2 i = 0; i < DiskBPB.MaxRootEntries; ++i) {
        if (!F(context, &RootDir[i])) {
            break;
        }
    }
    free(RootDir);
}

U1 PrintDirEntry(void* context, const struct DirEntry* de)
{
    (void)context;
    if (!de->Name[0]) {
        // Unused entry
    } else if (de->Attributes & ATTR_L) {
        printf("Volume label: %11.11s\n", de->Name);
    } else {
        U1 temp[13];
        U1* p = temp;
        for (int i = 0; i < 8 && de->Name[i] != ' '; ++i) {
            *p++ = de->Name[i];
        }
        *p++ = '.';
        for (int i = 8; i < 11 && de->Name[i] != ' '; ++i) {
            *p++ = de->Name[i];
        }
        *p++ = '\0';
        if (temp[0] == ENTRY_DELETED) {
            temp[0] = '?';
        } else if (temp[0] == 0x05) {
            temp[0] = ENTRY_DELETED;
        }

        printf("%-12s %02X %08X %04X\n", temp, de->Attributes, de->FileSize, de->FirstClusterLo);
    }
    return 1;
}

void ListDisk(void)
{
#define PR(f) printf("%-20s %u\n", #f, DiskBPB.f)
    PR(BytesPerSector);
    PR(SectorsPerCluster);
    PR(ReservedSectors);
    PR(NumFats);
    PR(MaxRootEntries);
    PR(TotalSectors);
    PR(MediaDescriptor);
    PR(SectorsPerFat);
    PR(SectorsPerTrack);
    PR(NumHeads);
#undef PR
    ForEachRootEntry(&PrintDirEntry, NULL);
}

void ShowFATInfo(void)
{
    printf("     ");
    for (U2 i = 0; i < 16; ++i) {
        printf("--%X%c", i, i==15?'\n':' ');
    }
    for (U2 i = 0; i < CLUSTER_MAX; ++i) {
        if (i && i % 16 == 0) printf("\n");
        if (i % 16 == 0) printf("%03X  ", i);
        const U2 val = GetFATEntry(i);
        printf("%03X ", val);
    }
    printf("\n");
}


const U1 DefaultBootCode[] = {
                        //        org 0x7c1e
  0x6a,0x00,            //        push 0
  0x1f,                 //        pop ds
  0xbe,0x37,0x7c,       //        mov si,Msg
  0xb4,0x0e,            //        mov ah,0x0e
  0xac,                 //.L:     lodsb
  0x20,0xc0,            //        and al,al
  0x0f,0x84,0x04,0x00,  //        jz .D
  0xcd,0x10,            //        int 0x10
  0xeb,0xf5,            //        jmp .L
  0x31,0xc0,            //.D:     xor ax,ax
  0xcd,0x16,            //        int 0x16
  0xcd,0x19,            //        int 0x19
//Msg: db 'Non-bootable SDOS disk. Press any key to reboot.',0
  0x4e,0x6f,0x6e,0x2d,0x62,0x6f,0x6f,0x74,0x61,0x62,0x6c,0x65,
  0x20,0x53,0x44,0x4f,0x53,0x20,0x64,0x69,0x73,0x6b,0x2e,0x20,
  0x50,0x72,0x65,0x73,0x73,0x20,0x61,0x6e,0x79,0x20,0x6b,0x65,
  0x79,0x20,0x74,0x6f,0x20,0x72,0x65,0x62,0x6f,0x6f,0x74,0x2e,
  0x00
};
const U1 OEMName[8] = { 'S', 'D', 'O', 'S', ' ', '1', '.', '0' };

void InstallBootLoader(const U1* Code, U2 CodeSize)
{
    if (CodeSize > MAX_BOOT_CODE_SIZE) {
        Error("CodeSize %u exceeds max size %u\n", CodeSize, MAX_BOOT_CODE_SIZE);
    }
    U1 BootSect[512];
    memset(BootSect, 0, sizeof(BootSect));

    // Jump past BPB
    BootSect[0] = 0xEB;
    BootSect[1] = BPB_OFFSET + sizeof(struct BPB) - 2;
    BootSect[2] = 0x90;
    memcpy(BootSect + 3, OEMName, 8);
    memcpy(BootSect + BPB_OFFSET, &DiskBPB, sizeof(DiskBPB));
    memcpy(BootSect + BOOT_CODE_OFFSET, Code, CodeSize);
    BootSect[510] = 0x55;
    BootSect[511] = 0xAA;
    WriteSector(0, BootSect);
}


void CreateDisk(void)
{
    // Create 1440 FD
    DiskBPB.BytesPerSector    = 512;
    DiskBPB.SectorsPerCluster = 1;
    DiskBPB.ReservedSectors   = 1;
    DiskBPB.NumFats           = 2;
    DiskBPB.MaxRootEntries    = 14 * 512 / sizeof(struct DirEntry);
    DiskBPB.TotalSectors      = 2880;
    DiskBPB.MediaDescriptor   = 0xF0;
    DiskBPB.SectorsPerFat     = 9;
    DiskBPB.SectorsPerTrack   = 18;
    DiskBPB.NumHeads          = 2;


    U1 Zeros[512];
    memset(Zeros, 0, sizeof(Zeros));

    // Make sure disk is fully created
    for (U2 i = 0; i < DiskBPB.TotalSectors; ++i) {
        WriteSector(i, Zeros);
    }

    // For the BPB to be recognized it seems like the disk has to
    // be bootable? (At least with FreeDOS 1.2)
    InstallBootLoader(DefaultBootCode, sizeof(DefaultBootCode));

    AllocFAT();
    memset(FAT, 0, DiskBPB.SectorsPerFat * DiskBPB.BytesPerSector);
    SetFATEntry(0, 0xFF0);
    SetFATEntry(1, 0xFFF);
}

void FlushFAT(void)
{
    WriteSectors(FAT_SECTOR, FAT, DiskBPB.SectorsPerFat);
    WriteSectors(FAT_SECTOR+DiskBPB.SectorsPerFat, FAT, DiskBPB.SectorsPerFat);
}

U1 ToUpper(U1 ch)
{
    return ch >= 'a' && ch < 'z' ? ch - ('a' - 'A') : ch;
}

void ExpandFileName(U1* Dst, const char* FileName)
{
    memset(Dst, ' ', 11);
    int pos = 0;
    for (; *FileName && *FileName != '.'; ++FileName) {
        if (pos < 8) {
            Dst[pos++] = ToUpper(*FileName);
        }
    }
    if (*FileName == '.') {
        ++FileName;
        pos = 8;
        for (; *FileName && pos < 11; ++FileName) {
            if (pos < 11) {
                Dst[pos++] = ToUpper(*FileName);
            }
        }
    }
}

void CreateFile(const char* FileName, const void* data, U4 size)
{
    struct DirEntry DE;
    memset(&DE, 0, sizeof(DE));
    DE.Attributes = ATTR_A;
    DE.FileSize = size;
    ExpandFileName(DE.Name, FileName);

    const U1* d = data;
    for (U2 LastCluster = 0; size;) {
        const U2 Cluster = GetFreeCluster();
        if (!Cluster) {
            Error("Disk full");
        }
        if (LastCluster) {
            SetFATEntry(LastCluster, Cluster);
        } else {
            DE.FirstClusterLo = Cluster;
        }
        SetFATEntry(Cluster, 0xFFF);

        const U4 here = size > (U4)CLUSTER_SIZE ? CLUSTER_SIZE : size;

        memcpy(ClusterBuffer, d, here);
        memset(ClusterBuffer + here, 0, CLUSTER_SIZE - here);
        WriteCluster(Cluster, ClusterBuffer);

        size -= here;
        d += here;
        LastCluster = Cluster;
    }

    U1 SectorBuffer[512];
    for (U2 entry = 0; entry < DiskBPB.MaxRootEntries; ++entry) {
        const U2 EntriesPerSector = DiskBPB.BytesPerSector/sizeof(struct DirEntry);
        const U2 Sector = ROOT_SECTOR + entry / EntriesPerSector;
        if (entry % EntriesPerSector == 0) {
            ReadSector(Sector, SectorBuffer);
        }
        struct DirEntry* E = &((struct DirEntry*)SectorBuffer)[entry % EntriesPerSector];
        if (!memcmp(E->Name, DE.Name, sizeof(E->Name))) {
            Error("%s already exists (not implemented)", E->Name);
        }
        if (!E->Name[0]) {
            memcpy(E, &DE, sizeof(*E));
            WriteSector(Sector, SectorBuffer);
            return;
        }
    }

    Error("Root directory full");
}

U1* ReadFile(const char* FileName, U4* size)
{
    FILE* fp = fopen(FileName, "rb");
    if (!fp) {
        Error("Could not open %s", FileName);
    }
    fseek(fp, 0L, SEEK_END);
    *size = (U4)ftell(fp);
    U1* data = malloc(*size);
    if (!data) {
        Error("Error allocating memory");
    }
    fseek(fp, 0L, SEEK_SET);
    if (!fread(data, *size, 1, fp) || ferror(fp)) {
        Error("Error reading from %s", FileName);
    }
    fclose(fp);
    return data;
}

void UpdateBootLoader(const char* BootFileName)
{
    U4 size;
    U1* data = ReadFile(BootFileName, &size);
    InstallBootLoader(data, size);
    free(data);
}

const char* GetBaseName(const char* FileName)
{
    const char* s = FileName + strlen(FileName);
    for (; s > FileName; --s) {
        if (*s == '/' || *s == '\\') {
            return s + 1;
        }
    }
    return FileName;
}

U1* GetFileFromDE(const struct DirEntry* DE)
{
    U1* data = malloc(DE->FileSize);
    if (!data) Error("Memory allocation failure");
    U2 cluster = DE->FirstClusterLo;
    U1* d = data;
    for (U4 size = DE->FileSize; size; size -= size > (U4)CLUSTER_SIZE ? CLUSTER_SIZE : size) {
        if (cluster < 2 || cluster >= 0xff0) {
            Error("Invalid cluster 0x%X for %11.11s", cluster, DE->Name);
        }
        if (size <= (U4)CLUSTER_SIZE) {
            ReadCluster(cluster, ClusterBuffer);
            memcpy(d, ClusterBuffer, size);
            d += size;
        } else {
            ReadCluster(cluster, d);
            d += CLUSTER_SIZE;
        }
        cluster = GetFATEntry(cluster);
    }
    return data;
}

struct FindFileContext {
    U1 OK;
    U1 FileName[11];
    const char* DiskFileName;
};

U1 FindFile(void* context, const struct DirEntry* DE)
{
    if (!DE->Name[0]) return 0;
    struct FindFileContext* ctx = context;
    if (memcmp(ctx->FileName, DE->Name, 11)) return 1;

    U1* data = GetFileFromDE(DE);
    FILE* out = fopen(ctx->DiskFileName, "wb");
    if (!out) {
        Error("Error creating %s", ctx->DiskFileName);
    }
    if (!fwrite(data, DE->FileSize, 1, out) || ferror(out)) {
        Error("Error writing to %s", ctx->DiskFileName);
    }
    fclose(out);
    free(data);
    printf("Writing %s 0x%X\n", ctx->DiskFileName, DE->FileSize);
    ctx->OK = 1;
    return 0;
}

void GetFile(const char* DiskFileName, const char* FileName)
{
    struct FindFileContext ctx;
    ctx.OK = 0;
    ctx.DiskFileName = DiskFileName;
    ExpandFileName(ctx.FileName, FileName);
    ForEachRootEntry(&FindFile, &ctx);
    if (!ctx.OK) {
        Error("%s not found in disk", FileName);
    }
}

void PutFile(const char* FileName, const char* DiskFileName)
{
    U4 size;
    U1* data = ReadFile(FileName, &size);
    CreateFile(DiskFileName, data, size);
    free(data);
}

int main(int argc, char* argv[])
{
    if (argc < 3) {
    Usage:
        Error("Usage: %s disk-image op [args...]\n"
            "  Operations:\n"
            "     list                 List information\n"
            "     fat                  Show FAT information\n"
            "     create               Create new disk\n"
            "     boot boot-file       Update bootloader (note: special format assumes org 0x%04X)\n"
            "     get file [sysname]   Get file fromt root directory (optionally to another filename)\n"
            "     put file [diskname]  Put file into root directory (optionally with another filename)\n"
            , argv[0], 0x7c00 + BOOT_CODE_OFFSET);
    }
    const char* DiskImgFileName = argv[1];
    enum {OP_LIST, OP_FAT, OP_CREATE, OP_BOOT, OP_GET, OP_PUT } op;
    if (!strcmp(argv[2], "list")) {
        op = OP_LIST;
    } else if (!strcmp(argv[2], "fat")) {
        op = OP_FAT;
    } else if (!strcmp(argv[2], "create")) {
        op = OP_CREATE;
    } else if (!strcmp(argv[2], "boot")) {
        if (argc < 4) {
            goto Usage;
        }
        op = OP_BOOT;
    } else if (!strcmp(argv[2], "get")) {
        if (argc < 4) {
            goto Usage;
        }
        op = OP_GET;
    } else if (!strcmp(argv[2], "put")) {
        if (argc < 4) {
            goto Usage;
        }
        op = OP_PUT;
    } else {
        goto Usage;
    }

    const U1 ReadOnly = op == OP_LIST || op == OP_FAT || op == OP_GET;
    DiskImg = fopen(DiskImgFileName, ReadOnly ? "rb" : "rb+");
    if (!DiskImg && op == OP_CREATE) {
        DiskImg = fopen(DiskImgFileName, "wb+");
    }
    if (!DiskImg) {
        Error("Error opening %s", DiskImgFileName);
    }

    if (op != OP_CREATE) {
      MountDisk();
    }

    if (op == OP_LIST) {
        ListDisk();
    } else if (op == OP_FAT) {
        ShowFATInfo();
    } else if (op == OP_CREATE) {
        CreateDisk();
    } else if (op == OP_BOOT) {
        UpdateBootLoader(argv[3]);
    } else if (op == OP_GET) {
        GetFile(argv[3], argc > 4 ? argv[4] : GetBaseName(argv[3]));
    } else if (op == OP_PUT) {
        PutFile(argv[3], argc > 4 ? argv[4] : GetBaseName(argv[3]));
    } else {
        assert(0);
    }

    if (!ReadOnly) {
        FlushFAT();
    }

    free(FAT);
    if (ferror(DiskImg)) {
        Error("Disk image error");
    }
    fclose(DiskImg);


    return 0;
}
