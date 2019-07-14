; The disktool assumes the code starts after the DOS 3.0 BPB
; It also handles putting in the boot signature at the end of the sector
; The file to load is assumed to be placed sequentially from cluster 2 onwards
; and cluster 2 is assumed to be at sector 33 (0x21). It's loaded at
; linear address 0x0500 (just after the IVT) and the stack top is at 0x7c00,
; DL contains the boot drive.

        org 0x7c1e

        ; Ensure we're running from a known address
Start:
        push 0
        push RealStart
        retf
RealStart:
        cld
        cli
        mov ax, cs
        mov ds, ax
        mov es, ax
        mov ss, ax
        mov sp, 0x7c00
        sti
        push dx
        mov ah, 0x42
        mov si, DiskPacket
        int 0x13
        jc Error
        pop dx

        push 0x0500
        ret
Error:
        mov si, MsgDiskErr
        call PutString
        xor ax, ax
        int 0x16
        int 0x19

PutString:
        mov ah, 0x0e
.Pr:
        lodsb
        and al, al
        jz .Done
        int 0x10
        jmp .Pr
.Done:
        ret

DiskPacket:
        dw 0x0010   ; Size of disk packet
        dw 0x0010   ; Number of blocks (Assume 8K is enough for now)
        dw 0        ; Offset
        dw 0x0050   ; Segment
        dw 0x21     ; Starting block number (First data cluster)
        dw 0,0,0

MsgDiskErr:
        db 'Error reading from disk', 13, 10
        db 'Press any key to reboot', 13, 10, 0
