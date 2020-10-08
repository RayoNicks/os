;FAT32文件系统DBR扇区
;检测是否支持int 13h扩展功能
;在FAT32文件系统中查找并加载启动文件
;2020.2.24~2020.2.27

    org 0x7C00

struc   DriveParametersPacket
    .size           resw    1
    .flags          resw    1
    .cylinders      resd    1
    .heads          resd    1
    .sectors        resd    1
    .TotalSectors   resd    2
    .SectorSize     resw    1
endstruc

BasePointer     equ     0x7C00
TotSecDisp      equ     -4

    ;BIOS Parameter Block
    jmp DBR_code_start
int13Ext    equ     $
    nop
    BS_OEMName          db  'FAT32DBR'
    BPB_BytesPerSec     dw  0x200
    BPB_SecPerClus      db  0x08
    BPB_RsvdSecCnt      dw  0x0100      ;DBR到FAT表的偏移
    BPB_NumFATs         db  0x02
    BPB_RootEntCnt      dw  0x00
    BPB_TotSec16        dw  0x00
    BPB_Media           db  0xF8
    BPB_FATSz16         dw  0x00
    BPB_SecPerTrk       dw  0x3F
    BPB_NumHeads        dw  0xFF
    BPB_HiddSec         dd  0x0100      ;MBR到DBR的偏移
    BPB_TotSec32        dd  0xEFFF00
    BPB_FATSz32         dd  0x3C00
    BPB_ExtFlags        dw  0x00
    BPB_FSVer           dw  0x00
    BPB_RootClus        dd  0x02        ;根目录簇号
    BPB_FSInfo          dw  0x01
    BPB_BkBootSec       dw  0x06
    BPB_Reserved        dd  0x00, 0x00, 0x00
    BS_DrvNum           db  0x80
    BS_Reserved1        db  0x00
    BS_BootSig          db  0x29
    BS_VolID            dd  0x00
    BS_VolLab           db  'Scratch FAT'
    BS_FileSysType      db  'FAT32   '

DBR_code_start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov bp, 0x7C00
    mov sp, bp
    sti

    mov [bp + BS_DrvNum - $$], dl       ;保存驱动器号
    mov byte [bp + int13Ext - $$], 0    ;nop指令清0

    mov di, DBRLoaded
    mov bx, 0x0007
    call dply_msg

    ;jmp not_supported

    ;检测是否支持int 13h扩展
    mov ah, 0x41
    mov bx, 0x55AA
    int 0x13
    jc not_supported
    cmp bx, 0xAA55
    jne not_supported
    test cl, 0x1
    jz not_supported

supported:
    inc byte [bp + int13Ext - $$]
    ;调用int 13h的48h功能获取驱动器信息
    mov ah, 48h
    sub sp, DriveParametersPacket_size
    mov si, sp          ;经过Bochs测试，应该是si
    mov word [si], DriveParametersPacket_size    ;空间是在栈上分配的，未初始化
    int 13h
    jc restart
    mov eax, 0xFFFFFFFF
    mov ebx, [si + DriveParametersPacket.TotalSectors + 4]
    cmp ebx, 0
    cmove eax, [si + DriveParametersPacket.TotalSectors]
    add sp, DriveParametersPacket_size
    push eax        ;[bp - 4] = 总扇区数目
    jmp loading_secondary

not_supported:
    ;调用int 13h的08h功能获取驱动器信息
    mov ah,0x8
    int 0x13
    jc restart
    movzx ax, dh
    inc ax
    mov [bp + BPB_NumHeads - $$], ax
    movzx dx, cl
    and dx, 0x3f
    mov [bp + BPB_SecPerTrk - $$], dx
    mul dx
    xchg cl, ch
    shr ch, 0x6
    inc cx
    mul cx
    push dx         ;[bp - 4] = 总扇区数目
    push ax

    ;加载DBR辅助扇区
loading_secondary:
    mov eax, [bp + BPB_HiddSec - $$]
    add eax, 12
    mov cx, 1
    mov bx, 0x7E00
    call load_sector
    cmp word [es:bx + 0x1FE], 0xAA55
    jne restart
    jmp 0x000:0x7E00

restart:
    mov di, BootFailed
    mov bx, 0x0007
    call dply_msg
    cbw
    int 16h
    int 19h

;显示C语言风格字符串
;es:di：字符串地址
;bx：页码和颜色属性
dply_msg:               ;和MBR中代码相同
    pusha
    mov ah, 0x03
    int 10h
    xor ax, ax
    mov cx, 0xFFFF
    mov bp, di
    repnz scasb
    not cx
    dec cx
    mov ax, 0x1301
    int 10h
    popa
    ret

struc   DiskAddressPacket
    .size       resb    1   ;结构体大小
    .reserved   resb    1   ;保留
    .count      resw    1   ;传输扇区数目
    .offset     resw    1   ;偏移
    .base       resw    1   ;段基址
    .start      resq    1   ;起始LBA
endstruc

;eax：LBA，cx：扇区个数，es:bx：加载地址
;carry = 0成功
load_sector:
RetryCnt    equ     3
    pusha
    movzx ecx, cx
    add eax, ecx
    cmp eax, [bp + TotSecDisp]
    ja load_sector_err
    sub eax, ecx
    cmp byte [bp + int13Ext - $$], 1
    jne CHS_loading
LBA_loading:
    ;在栈上构造DiskAddressPacket
    push dword 0x00000000
    push eax
    push es
    push bx
    push cx
    push DiskAddressPacket_size     ;8位立即数会自动扩展为16位
    mov si, sp
    mov di, RetryCnt
LBA_retry:
    mov ah, 0x42
    mov dl, [bp + BS_DrvNum - $$]
    int 13h
    jnc LBA_sector_loaded
    dec di
    je LBA_load_sector_err
    xor ah, ah
    int 13h
    jmp LBA_retry
LBA_load_sector_err:
    add sp, DiskAddressPacket_size
    popa
    add sp, 2
    jmp restart
LBA_sector_loaded:
    add sp, DiskAddressPacket_size
    popa
    ret
CHS_loading:
    mov di, RetryCnt
CHS_retry:
    push eax
    push ecx
    ;被除数最多24位，除数最多6位，商最多19位
    ;因此使用64位被除数，32位除数，结果在32位寄存器中
    xor edx, edx
    movzx ecx, word [bp + BPB_SecPerTrk - $$]
    div ecx
    mov cl, dl
    inc cl
    ;被除数最多19位，除数最多9位，商最多10位
    ;因此使用32位被除数，16位除数，结果在16位寄存器中
    mov edx, eax
    shr edx, 16
    div word [bp + BPB_NumHeads - $$]
    mov dh, [bp + BS_DrvNum - $$]
    xchg dl, dh
    mov ch, al
    shl ah, 6
    or cl, ah
    mov ax, 0x0201
    int 13h
    jnc CHS_sector_loaded
    dec di
    je CHS_load_sector_err
    xor ah, ah
    int 13h
    pop ecx
    pop eax
    jmp CHS_retry
CHS_sector_loaded:
    pop ecx
    pop eax
    add bx, [bp + BPB_BytesPerSec - $$]
    inc eax
    loop CHS_loading
    popa
    ret
CHS_load_sector_err:
    pop ecx
    pop eax
load_sector_err:
    popa
    add sp, 2
    jmp restart

DBRLoaded:      db      'DBR loaded...', 0x0D, 0x0A, 0x00
BootFailed:     db      'Boot failed.', 0x0D, 0x0A, 'Press any key to restart...', 0x0D, 0x0A, 0x00

    times 510 - ($ - $$) db 0x00
    db 0x55, 0xAA

struc ShortEntry
    .FileName           resb    11
    .attribute          resb    1
    .reserved           resb    1
    .CreateTimeTenth    resb    1
    .CreateTime         resw    1
    .CreateDate         resw    1
    .LastAccessDate     resw    1
    .ClusHighWord       resw    1
    .LastModTime        resw    1
    .LastModData        resw    1
    .ClusLowWord        resw    1
    .size               resd    1
endstruc

FATSecDisp      equ     -8
Clus2SecDisp    equ     -12
FATCurrSecDisp  equ     -16

    movzx ecx, word [bp + BPB_RsvdSecCnt - $$]
    add ecx, [bp + BPB_HiddSec - $$]
    push ecx                ;[bp - 8] = 第1个FAT表起始扇区
    movzx eax, byte [bp + BPB_NumFATs - $$]
    mul dword [bp + BPB_FATSz32 - $$]
    add eax, ecx
    push eax                ;[bp - 12] = 第2簇起始扇区
    push dword 0xFFFFFFFF   ;[bp - 16] = 内存中的FAT表的扇区号

FATOffset       equ     0x8000
RootDirOffset   equ     0x8200

    mov eax, [bp + BPB_RootClus - $$]
    and eax, 0x0FFFFFFF                 ;仅低28位有效
scanning_root_dir_cluster:
    ;0xFFFFFF0 ~ 0xFFFFFF6：保留簇
    ;0xFFFFFF7：坏簇
    ;0xFFFFFF8 ~ 0xFFFFFFF：结束簇
    cmp eax, 2
    jb restart
    cmp eax, 0x0FFFFFF7
    jae restart
    push eax        ;保存当前簇号
    call cluster_to_sector
loading_root_dir_sector:
    mov bx, RootDirOffset
    mov cx, 1
    call load_sector
    mov di, bx
    add bx, [bp + BPB_BytesPerSec - $$]
scanning_dir_entry:
    cmp byte [di], 0x00
    je file_not_found
    mov cx, ShortEntry.attribute - ShortEntry.FileName
    push di
    push si
    mov si, BootMgr
    repe cmpsb
    pop si
    pop di
    jz file_found
    add di, ShortEntry_size
    cmp di, bx
    jb scanning_dir_entry
    inc eax
    dec si
    jnz loading_root_dir_sector
    pop eax         ;恢复当前簇号
    call cal_next_cluster
    jmp scanning_root_dir_cluster

file_not_found:
    add sp, 4   ;平衡栈中eax
    mov bx, 0x0007
    mov di, BootMgr
    call dply_msg
    mov di, FileNotFound
    call dply_msg
    jmp restart

file_found:
    add sp, 4   ;平衡栈中eax
    mov ax, [di + ShortEntry.ClusHighWord]
    shl eax, 16
    mov ax, [di + ShortEntry.ClusLowWord]
    and eax, 0x0FFFFFFF
    cmp eax, 2
    jb restart
    cmp eax, 0x0FFFFFF7
    jae restart

loading_os_cluster:
    push eax                                    ;保存当前簇号
    call cluster_to_sector                      ;将eax中簇号转换为起始扇区
    movzx cx, byte [bp + BPB_SecPerClus - $$]
loading_os:
    push cx
    push es
    mov es, [BootMgrSeg]
    mov bx, 0x0000
    mov cx, 1
    call load_sector
    pop es
    pop cx
    add bx, [bp + BPB_BytesPerSec - $$]
    shr bx, 4
    add [BootMgrSeg], bx
    inc eax
    loop loading_os
    pop eax                                     ;恢复当前簇号
    call cal_next_cluster
    cmp eax, 2
    jb restart
    cmp eax, 0x0FFFFFF7
    je restart
    cmp eax, 0x0FFFFFF8
    jae file_loaded
    jmp loading_os_cluster

file_loaded:
    mov dl, [bp + BS_DrvNum - $$]

    ;测试autorun.inf
    ;push ds
    ;mov ax, 0x2000
    ;mov ds, ax
    ;mov si, 0x00
    ;mov di, 0x8400
    ;mov cx, 0x80
    ;rep movsd
    ;pop ds
    ;mov di, 0x8400
    ;mov bx, 0x0007
    ;call dply_msg
    ;jmp restart

    ;jmp $

    jmp 0x2000:0x0000

;eax：当前簇
;eax：扇区
cluster_to_sector:
    push edx
    push esi
    sub eax, 2
    movzx esi, byte [bp + BPB_SecPerClus - $$]
    mul esi
    add eax, [bp + Clus2SecDisp]
    pop esi
    pop edx
    ret

;eax：当前簇
;eax：下一簇
cal_next_cluster:
    push cx
    push edx
    push bx
    shl eax, 2
    mov edx, eax
    shr edx, 16
    div word [bp + BPB_BytesPerSec - $$]    ;eax = FAT项所在扇区， edx = 扇区中偏移
    cmp eax, [bp + FATCurrSecDisp]
    je get_next_cluster
    mov [bp + FATCurrSecDisp], eax
    add eax, dword [bp + FATSecDisp]
    ;定位到有效FAT表
    movzx ebx, word [bp + BPB_ExtFlags - $$]
    test bx, 0x80
    jz load_fat32_sector
    and ebx, 0xf
    cmp bl, [bp + BPB_NumFATs - $$]
    jae cal_next_cluster_err
    push edx
    mov ecx, eax
    mov eax, dword [bp + BPB_FATSz32 - $$]
    mul ebx
    add eax, ecx
    pop edx
load_fat32_sector:
    mov bx, FATOffset
    mov cx, 1
    call load_sector
    jc cal_next_cluster_err
get_next_cluster:
    mov eax, [FATOffset + edx]
    and eax, 0x0FFFFFFF
    pop bx
    pop edx
    pop cx
    ret
cal_next_cluster_err:
    add sp, 12
    jmp restart

BootMgrSeg:     dw      0x2000

BootMgr:        db      'BOOTMGR    ', 0x00
AutoRun:        db      'AUTORUN INF', 0x00
FileNotFound:   db      ' not found', 0x0D, 0x0A, 0x00
    times 1022 - ($ - $$) db 0
    db 0x55, 0xAA
