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

    struct DirEntry* RootDir = malloc(sizeof(struct DirEntry) * MAX_ROOT_ENTRIES);
    if (!RootDir) {
        Error("Memory allocation failure");
    }
    ReadSectors(ROOT_SECTOR, (U1*)RootDir, NUM_ROOT_SECTORS);
    for (U2 i = 0; i < DiskBPB.MaxRootEntries; ++i) {
        const struct DirEntry* de = &RootDir[i];
        if (!de->Name[0]) {
            break;
        }
        if (de->Attributes & ATTR_L) {
            printf("Volume label: %11.11s\n", de->Name);
            continue;
        }
        printf("%11.11s %02X %08X %04X\n", de->Name, de->Attributes, de->FileSize, de->FirstClusterLo);
    }
    free(RootDir);
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

    static const U1 OEMName[8] = { 'S', 'D', 'O', 'S', ' ', '1', '.', '0' };
    U1 BootSect[512];
    memset(BootSect, 0, sizeof(BootSect));

    // Make sure disk is fully created
    for (U2 i = 0; i < DiskBPB.TotalSectors; ++i) {
        WriteSector(i, BootSect);
    }

    // Jump past BPB
    BootSect[0] = 0xEB;
    BootSect[1] = BPB_OFFSET + sizeof(struct BPB) - 2; 
    BootSect[2] = 0x90;
    memcpy(BootSect + 3, OEMName, 8);
    memcpy(BootSect + BPB_OFFSET, &DiskBPB, sizeof(DiskBPB));
    BootSect[510] = 0x55;
    BootSect[511] = 0xAA;

    AllocFAT();
    memset(FAT, 0, DiskBPB.SectorsPerFat * DiskBPB.BytesPerSector);
    SetFATEntry(0, 0xFF0);
    SetFATEntry(1, 0xFFF);

    WriteSector(0, BootSect);
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

void CreateFile(const char* filename, const void* data, U4 size)
{
    struct DirEntry DE;
    memset(&DE, 0, sizeof(DE));
    memset(DE.Name, ' ', 11);

    int pos = 0;
    while (*filename && *filename != '.') {
        if (pos < 8) {
            DE.Name[pos++] = ToUpper(*filename++);
        }
    }
    if (*filename == '.') {
        ++filename;
        pos = 8;
        while (*filename && pos < 11) {
            DE.Name[pos++] = ToUpper(*filename++);
        }
    }
    DE.Attributes = ATTR_A;
    DE.FileSize = size;

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
    if (size > 510 - BOOT_CODE_OFFSET) {
        Error("Size of boot sector %u is too large", size);
    }
    fseek(DiskImg, BOOT_CODE_OFFSET, SEEK_SET);
    if (!fwrite(data, size, 1, DiskImg) || ferror(DiskImg)) {
        Error("Disk writing to disk image");
    }
    free(data);
}

void PutFile(const char* FileName)
{
    U4 size;
    U1* data = ReadFile(FileName, &size);
    CreateFile(FileName, data, size);
    free(data);
}

int main(int argc, char* argv[])
{
    if (argc < 3) {
    Usage:
        Error("Usage: %s disk-image op [args...]\n"
            "  Operations:\n"
            "     list            List information\n"
            "     fat             Show FAT information\n"
            "     create          Create new disk\n"
            "     boot boot-file  Update bootloader (note: special format assumes org 0x%04X)\n"
            "     put file        Put file into root directory\n"
            , argv[0], 0x7c00 + BOOT_CODE_OFFSET);
    }
    const char* DiskImgFileName = argv[1];
    enum {OP_LIST, OP_FAT, OP_CREATE, OP_BOOT, OP_PUT } op;
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
    } else if (!strcmp(argv[2], "put")) {
        if (argc < 4) {
            goto Usage;
        }
        op = OP_PUT;
    } else {
        goto Usage;
    }

    const U1 ReadOnly = op == OP_LIST || op == OP_FAT;
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
    } else if (op == OP_PUT) {
        PutFile(argv[3]);
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
