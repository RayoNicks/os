;FAT32文件系统DBR扇区
;在第一扇区中增加了打印寄存器的代码
;第一扇区加载第二和第三扇区
;检测是否支持int 13h扩展功能，但是UEFI主板上的CHS过小
;在FAT32文件系统中查找并加载操作系统
;2020.2.27~2020.2.29

    org 0x7C00

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
    mov bp, BasePointer
    mov sp, bp
    sti

    mov [bp + BS_DrvNum - $$], dl       ;保存驱动器号

    push 0x0007
    push DBRLoadedLen
    push DBRLoaded
    call dply_msg

    ;调用int 13h的08h功能获取驱动器信息，UEFI主板上获取的CHS太小
    mov ah, 0x8
    int 13h
    jc restart
    ;pushad
    ;call print_registers
    ;popad

    movzx ax, dh
    inc ax
    mov [bp + BPB_NumHeads - $$], ax
    movzx dx, cl
    and dx, 0x3f
    mov [bp + BPB_SecPerTrk - $$], dx
    mul dx          ;16位可以容纳扇区数 * 磁头数
    xchg cl, ch
    shr ch, 0x6
    inc cx
    mul cx
    push dx         ;[bp - 4] = 总扇区数目
    push ax

    ;加载DBR辅助扇区
DBRSecCnt   equ     2
loading_secondary:
    mov eax, [bp + BPB_SecPerTrk - $$]      ;扇区数和磁道数
    push eax
    mov eax, [bp + BPB_HiddSec - $$]
    add eax, 12
    cmp eax, [bp + TotSecDisp]
    jae restart
    push eax
    call LBA_to_CHS
    mov dl, byte [bp + BS_DrvNum - $$]
    mov bx, 0x7E00
    mov ax, 0x0200 | DBRSecCnt
    int 13h
    jc restart
check_boot_signature:
    cmp word [es:bx + 0x1FE], 0xAA55
    jne restart
    add bx, [bp + BPB_BytesPerSec - $$]
    dec al
    jnz check_boot_signature
    jmp detecting_int13_extensions

restart:
    push 0x0007
    push BootFailedLen
    push BootFailed
    call dply_msg
    cbw
    int 16h
    int 19h

;在栈上构造要打印的字符串
;eax=0xXXXXXXXX\r\n
;参数2：寄存器值
;参数1：寄存器名
ParaRegName     equ     4
ParaRegValue    equ     6
VarNumber       equ     -10
VarRegName      equ     -16
print_single_register:
    push bp
    mov bp, sp
    push 0x0A0D                         ;回车换行
    sub sp, 8                           ;为数字分配空间
    push '0x'                           ;直接编码为68 3078
    push ' ='                           ;直接编码为68 203D
    push 'e'                            ;扩展为16位的0x0065
    pusha                               ;为局部变量分配空间之后再保存通用寄存器
    lea si, [bp + ParaRegValue + 3]     ;si = &最高字节
    lea di, [bp + VarNumber]
    xor ah, ah
    mov cx, 4
next_byte:
    std
    lodsb
    push si             ;si指向下一个打印字节
    push ax             ;al保存当前打印字节
    mov si, HexNumber
    push si             ;si指向字符串表
    shr ax, 4           ;取高4位
    add si, ax
    cld
    movsb               ;[si] -> [di]
    pop si              ;si指向字符串表
    pop ax              ;al保存当前打印字节
    and ax, 0xf         ;取低4位
    add si, ax
    movsb
    pop si              ;si指向下一个打印字节
    loop next_byte
    mov si, [bp + ParaRegName]
    lea di, [bp + VarRegName + 1]
    movsw               ;寄存器名
    push 0x0007
    push 3 + 1 + 2 + 8 + 2
    lea ax, [bp + VarRegName]
    push ax
    call dply_msg
    popa
    leave
    ret 6

;需调用者恢复寄存器
;参数8：eax
;参数7：ecx
;参数6：edx
;参数5：ebx
;参数4：esp
;参数3：ebp
;参数2：esi
;参数1：edi
print_registers:
    push bp
    mov bp, sp
    lea si, [bp + 32]   ;si = &eax
    mov cx, 8
    mov bx, GPRName
next_register:
    std
    lodsd
    push eax
    push bx
    call print_single_register
    inc bx                      ;2个inc比add少1字节
    inc bx
    loop next_register
    leave
    ret

;打印字符串
;参数3：页码和颜色属性
;参数2：字符串长度
;参数1：字符串地址
ParaStr     equ     4
ParaSize    equ     6
ParaAttr    equ     8
dply_msg:
    push bp
    mov bp, sp
    pusha
    mov bx, [bp + ParaAttr]
    mov ah, 0x03
    int 10h
    mov cx, [bp + ParaSize]
    mov bp, [bp + ParaStr]
    mov ax, 0x1301
    int 10h
    popa
    leave
    ret 6

;将LBA转换为CHS
;参数3：磁头数
;参数2：扇区数
;参数1：LBA
;返回值在cx和dx中
ParaLBA     equ     4
ParaSecs    equ     8
ParaHeads   equ     10
VarCx       equ     -4
VarDx       equ     -2
LBA_to_CHS:
    push bp
    mov bp, sp
    sub sp, 4       ;暂存返回值
    pushad
    xor edx, edx
    movzx ecx, word [bp + ParaSecs]
    div ecx
    mov cl, dl
    inc cl
    ;被除数最多19位，除数最多9位，商最多10位
    ;因此使用32位被除数，16位除数，结果在16位寄存器中
    mov edx, eax
    shr edx, 16
    div word [bp + ParaHeads]
    xchg dl, dh
    mov ch, al
    shl ah, 6
    or cl, ah
    mov [bp + VarCx], cx
    mov [bp + VarDx], dx
    popad
    pop cx
    pop dx
    leave
    ret 8

GPRName:        db      'axcxdxbxspbpsidi'
HexNumber:      db      '0123456789ABCDEF'
DBRLoaded:      db      'DBR loaded...', 0x0D, 0x0A
DBRLoadedLen    equ     $ - DBRLoaded
BootFailed:     db      'Boot failed.', 0x0D, 0x0A, 'Press any key to restart...', 0x0D, 0x0A
BootFailedLen   equ     $ - BootFailed

    times 510 - ($ - $$) db 0x00
    db 0x55, 0xAA

;------------------------------第二扇区------------------------------

struc   DriveParametersPacket
    .size           resw    1
    .flags          resw    1
    .cylinders      resd    1
    .heads          resd    1
    .sectors        resd    1
    .TotalSectors   resd    2
    .SectorSize     resw    1
endstruc

detecting_int13_extensions:
    ;jmp loading_kernel
    mov byte [bp + int13Ext - $$], 0        ;nop指令清0
    mov dl, [bp + BS_DrvNum - $$]
    mov ah, 0x41
    mov bx, 0x55AA
    int 13h
    jc loading_kernel
    cmp bx, 0xAA55
    jne loading_kernel
    test cl, 0x1
    jz loading_kernel

supported:
    push 0x0007
    push ExtensionsLen
    push Extensions
    call dply_msg
    inc byte [bp + int13Ext - $$]
    ;调用int 13h的48h功能获取驱动器信息
    mov ah, 48h
    sub sp, DriveParametersPacket_size          ;在栈上分配数据结构
    mov si, sp                                  ;经过Bochs测试，应该是si
    mov word [si], DriveParametersPacket_size
    int 13h
    jc restart

    ;UEFI主板上48h功能获取的CHS依然不对，但是可以直接使用总扇区数
    ;push esi
    ;mov ebx, [si + DriveParametersPacket.cylinders]
    ;mov ecx, [si + DriveParametersPacket.heads]
    ;mov edi, [si + DriveParametersPacket.sectors]
    ;mov eax, [si + DriveParametersPacket.TotalSectors]
    ;mov edx, [si + DriveParametersPacket.TotalSectors + 4]
    ;mov si, [si + DriveParametersPacket.SectorSize]
    ;pushad
    ;call print_registers
    ;popad
    ;pop esi

    mov cx, [si + DriveParametersPacket.SectorSize]
    mov [bp + BPB_BytesPerSec], cx
    mov eax, 0xFFFFFFFF
    mov ebx, [si + DriveParametersPacket.TotalSectors + 4]
    cmp ebx, 0
    cmove eax, [si + DriveParametersPacket.TotalSectors]
    add sp, DriveParametersPacket_size
    mov [bp + TotSecDisp], eax                  ;[bp - 4] = 总扇区数目

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
FATSecDisp          equ     -8
Clus2StartDisp      equ     -12
FATCurrSecDisp      equ     -16

loading_kernel:
    movzx ecx, word [bp + BPB_RsvdSecCnt - $$]
    add ecx, [bp + BPB_HiddSec - $$]
    push ecx                ;[bp - 8] = 第1个FAT表起始扇区
    movzx eax, byte [bp + BPB_NumFATs - $$]
    mul dword [bp + BPB_FATSz32 - $$]
    add eax, ecx
    push eax                ;[bp - 12] = 第2簇起始扇区
    push dword 0xFFFFFFFF   ;[bp - 16] = 内存中的FAT表的扇区号

FATOffset       equ     0x7000
RootDirOffset   equ     0x7200

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
    push eax                        ;保存当前簇号
    movzx si, byte [bp + BPB_SecPerClus - $$]
    push eax
    call cluster_to_sector
loading_root_dir_sector:
    push eax
    push es
    mov bx, RootDirOffset
    push bx
    push 1
    call load_sector
    mov di, bx
    add bx, [bp + BPB_BytesPerSec - $$]
scanning_dir_entry:
    cmp byte [di], 0x00
    je file_not_found
    mov cx, ShortEntry.attribute - ShortEntry.FileName
    push di
    push si
    mov si, Kernel
    repe cmpsb
    pop si
    pop di
    jz file_found
    add di, ShortEntry_size
    cmp di, bx
    jb scanning_dir_entry
    inc eax                         ;下一扇区
    dec si                          ;剩余扇区
    jnz loading_root_dir_sector
    ;pop eax                        ;恢复当前簇号
    call cal_next_cluster           ;参数已经在栈中
    jmp scanning_root_dir_cluster

file_not_found:
    add sp, 4           ;平衡栈中eax
    push 0x0007
    push 11
    push Kernel
    call dply_msg
    push 0x0007
    push NotFoundLen
    push NotFound
    call dply_msg
    jmp restart

file_found:
    add sp, 4           ;平衡栈中eax
    mov ax, [di + ShortEntry.ClusHighWord]
    shl eax, 16
    mov ax, [di + ShortEntry.ClusLowWord]
    and eax, 0x0FFFFFFF
    cmp eax, 2
    jb restart
    cmp eax, 0x0FFFFFF7
    jae restart

loading_os_cluster:
    push eax                        ;保存当前簇号
    push eax
    call cluster_to_sector
    movzx cx, byte [bp + BPB_SecPerClus - $$]
loading_os_sector:
    push eax
    mov dx, [KernelSeg]
    push dx
    xor bx, bx
    push bx
    push 1
    call load_sector
    ;更新段基址
    add bx, [bp + BPB_BytesPerSec - $$]
    shr bx, 4
    add [KernelSeg], bx
    inc eax
    loop loading_os_sector
    ;pop eax                        ;恢复当前簇号
    call cal_next_cluster           ;参数已经在栈中
    cmp eax, 2
    jb restart
    cmp eax, 0x0FFFFFF7
    je restart
    cmp eax, 0x0FFFFFF8
    jae file_loaded
    jmp loading_os_cluster

file_loaded:
    push 0x0007
    push 11
    push Kernel
    call dply_msg
    push 0x0007
    push LoadedLen
    push Loaded
    call dply_msg

    ;测试autorun.inf
    ;call test_for_autorun

    mov dl, [bp + BS_DrvNum - $$]
    jmp 0x2000:0x0000

Extensions:     db      'Int 13h extensions installed...', 0x0D, 0x0A
ExtensionsLen   equ     $ - Extensions

Kernel          equ     BootMgr

KernelSeg:      dw      0x2000
BootMgr:        db      'BOOTMGR    ', 0x00
AutoRun:        db      'AUTORUN INF', 0x00
Loaded:         db      ' Loaded...', 0x0D, 0x0A
LoadedLen       equ     $ - Loaded
NotFound:       db      ' not found...', 0x0D, 0x0A
NotFoundLen     equ     $ - NotFound


    times 1022 - ($ - $$) db 0
    db 0x55, 0xAA

;------------------------------第三扇区------------------------------

test_for_autorun:
    mov di, 0x8400
    push di
    push ds
    mov ax, 0x2000
    mov ds, ax
    mov si, 0x00
    mov cx, 0x80
    rep movsd
    pop ds
    pop di
    mov bx, 0x0007
    push 0x0007
    mov cx, 0xFFFF
    repnz scasb
    not cx
    dec cx
    push cx
    push 0x8400
    call dply_msg
    jmp $

;将簇号转换为首扇区号
;参数1：簇
;返回值在eax中
ParaCluster     equ     4
VarSector       equ     -4
cluster_to_sector:
    push bp
    mov bp, sp
    sub sp, 4       ;返回值
    pushad
    mov eax, [bp + ParaCluster]
    sub eax, 2
    movzx ecx, byte [BPB_SecPerClus]
    mul ecx
    add eax, [BasePointer + Clus2StartDisp]
    mov [bp + VarSector], eax
    popad
    pop eax
    leave
    ret 4

;计算下一簇
;参数1：当前簇
;返回值在eax中
ParaCurrClus    equ     4
VarNextClus     equ     -4
cal_next_cluster:
    push bp
    mov bp, sp
    sub sp, 4
    pushad
    mov eax, [bp + ParaCluster]
    shl eax, 2
    mov edx, eax
    shr edx, 16
    div word [BPB_BytesPerSec]              ;eax = FAT项所在扇区， edx = 扇区中偏移
    cmp eax, [BasePointer + FATCurrSecDisp]
    je get_next_cluster
    mov [BasePointer + FATCurrSecDisp], eax
    add eax, dword [BasePointer + FATSecDisp]
    ;定位到有效FAT表
    movzx ebx, word [BPB_ExtFlags]
    test bx, 0x80
    jz load_fat32_sector
    and ebx, 0xf
    cmp bl, [BPB_NumFATs]
    jae cal_next_cluster_err
    push edx
    mov ecx, eax
    mov eax, dword [BPB_FATSz32]
    mul ebx
    add eax, ecx
    pop edx
load_fat32_sector:
    push eax
    push es
    push FATOffset
    push 1
    call load_sector
get_next_cluster:
    mov eax, [FATOffset + edx]
    and eax, 0x0FFFFFFF
    mov [bp + VarNextClus], eax
    popad
    pop eax
    leave
    ret 4
cal_next_cluster_err:
    mov word [bp + 2], restart
    popad
    leave
    ret 4

struc   DiskAddressPacket
    .size       resb    1   ;结构体大小
    .reserved   resb    1   ;保留
    .count      resw    1   ;传输扇区数目
    .offset     resw    1   ;偏移
    .base       resw    1   ;段基址
    .start      resq    1   ;起始LBA
endstruc
;参数4：起始LBA
;参数3：段基址
;参数2：段偏移
;参数1：数量
ParaSecCnt  equ     4
ParaOffset  equ     6
ParaBase    equ     8
ParaStart   equ     10
VarDAP      equ     -DiskAddressPacket_size
RetryCnt    equ     3
load_sector:
    push bp
    mov bp, sp
    push dword 0                ;在栈上构造DAP
    sub sp, 10
    push DiskAddressPacket_size
    pushad
    ;检测扇区编号
    movzx eax, word [bp + ParaSecCnt]
    add eax, [bp + ParaStart]
    cmp eax, [BasePointer + TotSecDisp]
    jae load_sector_err
    mov di, RetryCnt
    test byte [int13Ext], 0x01
    jnz LBA_loading
    mov bx, [bp + ParaOffset]
CHS_loading:
    mov ecx, [BPB_SecPerTrk]    ;扇区数和磁道数
    push ecx
    mov eax, [bp + ParaStart]
    push eax
    call LBA_to_CHS
    mov dl, [BS_DrvNum]         ;每次转换都会修改dl
CHS_retry:
    push es                     ;只在读取时临时修改es
    mov es, [bp + ParaBase]
    mov ax, 0x0201
    int 13h
    pop es
    jnc CHS_sector_loaded
    dec di
    je load_sector_err
    xor ah, ah
    int 13h
    jmp CHS_retry
CHS_sector_loaded:
    add bx, [BPB_BytesPerSec]
    inc dword [bp + ParaStart]
    dec word [bp + ParaSecCnt]
    jnz CHS_loading
    jmp sector_loaded
LBA_loading:
    mov dl, [BS_DrvNum]
    push di
    lea si, [bp + ParaSecCnt]
    lea di, [bp + VarDAP + 2]
    mov cx, 10
    repnz movsb
    lea si, [bp + VarDAP]
    pop di
LBA_retry:
    mov ah, 0x42
    int 13h
    jnc sector_loaded
    dec di
    je load_sector_err
    xor ah, ah
    int 13h
    jmp LBA_retry
load_sector_err:
    mov word [bp + 2], restart
sector_loaded:
    popad
    leave
    ret 10

    times 1534 - ($ - $$) db 0
    db 0x55, 0xAA
