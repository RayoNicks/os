;MBR��������
;ɨ�������
;�г����п������ķ���
;ѡ��һ������
;2020.2.22~2020.2.27

    org 0x7C00

StackPointer    equ     0x7C00

    ;��ʼ���μĴ���
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, StackPointer
    sti

    ;������������
    cmp dl, 0x8F
    jg MBR_loaded
    mov [DriveNo], dl

MBR_loaded:
    mov di, MBRLoaded
    mov bx, 0x0007
    call dply_msg

    ;��ѯ�Ƿ�֧��int 13h��չ
    mov ah, 41h
    mov bx, 0x55AA
    int 13h
    jc restart
    cmp bx, 0xAA55
    jne restart
    test cl, 0x1
    jz restart

supported:
    ;����MBR��������
    mov eax, 0x1
    mov cx, 1
    mov bx, 0x7E00
    call load_boot_sector
    jc restart
    jmp scanning_partition_table

restart:
    mov di, BootFailed
    mov bx, 0x07
    call dply_msg
    cbw
    int 16h
    call clear_screen
    int 19h

clear_screen:
    pusha
    ;��������������ʾģʽĬ��Ϊ80*25
    mov ax, 0x0600
    mov bx, 0x0700
    xor cx, cx
    mov dx, 0x1950
    int 10h
    ;�ƶ������ԭ��
    mov ax, 0x0200
    mov bh, 0x00
    mov dx, 0x0000
    int 10h
    popa
    ret

;��ʾC���Է���ַ���
;es:di���ַ�����ַ
;bx��ҳ�����ɫ����
dply_msg:
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

;��������
;eax��LBA��cx������������es:bx�����ص�ַ
;carry = 0�ɹ�
load_boot_sector:
RetryCnt    equ     5
    pusha
    ;��ջ�Ϲ���DiskAddressPacket
    push dword 0x00000000
    push eax
    push es
    push bx
    push cx
    push DiskAddressPacket_size    ;8λ���������Զ���չΪ16λ
    mov si, sp
    mov di, RetryCnt
retry:
    mov ah, 0x42
    mov dl, [DriveNo]
    int 13h
    jnc sector_loaded
    dec di
    je load_sector_err
    xor ah, ah
    int 13h
    jmp retry
sector_loaded:
    cmp word [es:bx + 0x1FE], 0xAA55
    jne load_sector_err
    add sp, DiskAddressPacket_size
    clc
    popa
    ret
load_sector_err:
    add sp, DiskAddressPacket_size
    stc
    popa
    ret

;int 13h����42h~44hʹ�õ�LBA�ṹ
struc   DiskAddressPacket
    .size       resb    1   ;�ṹ���С
    .reserved   resb    1   ;����
    .count      resw    1   ;����������Ŀ
    .offset     resw    1   ;ƫ��
    .base       resw    1   ;�λ�ַ
    .start      resq    1   ;��ʼLBA
endstruc

DriveNo     db  0x80

MBRLoaded:      db      'MBR loaded...', 0x0D, 0x0A, 0x00
BootFailed:     db      'Boot failed.', 0x0D, 0x0A, 'Press any key to restart...', 0x0D, 0x0A, 0x00

    times 0x1B8 - ($ - $$) db 0x00
    db 'Rayo', 0x00, 0x00

struc   PartitionTable
    .boot       resb    1
    .SHead      resb    1
    .SSector    resb    1
    .SCylinder  resb    1
    .SystemID   resb    1
    .EHead      resb    1
    .ESector    resb    1
    .ECylinder  resb    1
    .LBA        resd    1
    .size       resd    1
endstruc
PT1     equ     $
istruc PartitionTable
    at  .boot,          db  0x80
    at  .SHead,         db  0
    at  .SSector,       db  0x01
    at  .SCylinder,     db  0
    at  .SystemID,      db  0
    at  .EHead,         db  0x04
    at  .ESector,       db  0x04
    at  .ECylinder,     db  0
    at  .LBA,           dd  0x00
    at  .size,          dd  0x100
iend
PT2     equ     $
istruc PartitionTable
    at  .boot,          db  0x00
    at  .SHead,         db  0
    at  .SSector,       db  0
    at  .SCylinder,     db  0
    at  .SystemID,      db  0
    at  .EHead,         db  0
    at  .ESector,       db  0
    at  .ECylinder,     db  0
    at  .LBA,           dd  0
    at  .size,          dd  0
iend
PT3     equ     $
istruc PartitionTable
    at  .boot,          db  0x00
    at  .SHead,         db  0
    at  .SSector,       db  0
    at  .SCylinder,     db  0
    at  .SystemID,      db  0
    at  .EHead,         db  0
    at  .ESector,       db  0
    at  .ECylinder,     db  0
    at  .LBA,           dd  0
    at  .size,          dd  0
iend
PT4     equ     $
istruc PartitionTable
    at  .boot,          db  0x80
    at  .SHead,         db  0x04
    at  .SSector,       db  0x05
    at  .SCylinder,     db  0x00
    at  .SystemID,      db  0x0C
    at  .EHead,         db  0xFE
    at  .ESector,       db  0xFF
    at  .ECylinder,     db  0xD2
    at  .LBA,           dd  0x0100
    at  .size,          dd  0xEFFF00
iend

    times 510 - ($ - $$) db 0
    db 0x55, 0xAA

scanning_partition_table:
    mov si, PT1
    mov di, BootablePartition
    mov cx, 4
    xor ax, ax
begin_scanning:
    test byte [si], 0x80
    jz next_partition
    stosb
    inc byte [bootable]
next_partition:
    inc ax
    add si, PartitionTable_size
    loop begin_scanning

    ;û�п���������������
    cmp byte [bootable], 0
    je restart

printing_bootable_partition:
    xor cx, cx
    mov si, BootablePartition
    mov di, BootingFrom
begin_printing:
    lodsb
    or al, 0x30             ;ת��Ϊ����
    inc al
    mov [PartitionNo], al
    mov bx, 0x0007
    mov eax, [CRLF]
    mov [BootingEnd], eax
    
    cmp cl, [current]       ;��Ϊ��������ʾ��ʽ
    jne print
    mov bx, 0x0070
    mov eax, [Question]
    mov [BootingEnd], eax
print:
    call dply_msg
    inc cx
    cmp cl, byte [bootable]
    jb begin_printing

    ;������ƶ���ѡ����ĩβ
    mov ch, [current]
    sub ch, [bootable]
    mov cl, BootingEnd - BootingFrom
    call move_cursor

    ;����������ֻ��1����ֱ������
    cmp byte [bootable], 1
    je loading_boot_partition

get_input:
    mov ah, 0x00
    int 16h
    cmp ax, 0x1C0D          ;�س���
    je loading_boot_partition
    cmp ax, 0x3920          ;�ո��
    je loading_boot_partition

    ;�ָ���굽��1�ͷ
    mov ch, [current]
    neg ch
    mov cl, BootingFrom - BootingEnd

    cmp ax, 0x5000          ;�·����
    je down_arrow
    cmp ax, 0x4800          ;�Ϸ����
    jne get_input
up_arrow:
    cmp byte [current], 0
    jbe get_input
    call move_cursor
    dec byte [current]
    jmp printing_bootable_partition
down_arrow:
    mov al, [bootable]
    dec al
    cmp [current], al
    jae get_input
    call move_cursor
    inc byte [current]
    jmp printing_bootable_partition

loading_boot_partition:
    ;?����ʾ...
    mov ah, 0x09
    mov al, '.'
    mov bx, 0x0070
    mov cx, 3
    int 10h

    mov ch, [bootable]
    sub ch, [current]
    inc ch
    mov cl, BootingFrom - BootingEnd
    call move_cursor

MBRSegment      equ     0x0000
MBRBase         equ     0x0600
BRTmpBase       equ     0x8000

    movzx bx, byte [current]
    movzx ax, byte [BootablePartition + bx]
    push ax         ;����ѡ�е���������
    mov si, PT1
    shl ax, 4
    add si, ax

    ;���ظ÷���������
    mov eax, [si + PartitionTable.LBA]
    mov bx, BRTmpBase
    mov cx, 1
    call load_boot_sector
    jc restart

    ;�԰��Ƶ�0000:0600
    mov si, StackPointer
    mov di, 0x600
    mov cx, 0x100
    rep movsd
    jmp MBRSegment : MBRBase + MBR_transfered - $$

MBR_transfered:
    mov si, BRTmpBase
    mov di, StackPointer
    mov cx, 0x80
    rep movsd

    call clear_screen

    ;si����������ַ
    pop ax
    mov si, MBRBase + PT1 - $$
    shl ax, 4
    add si, ax
    movzx dx, [MBRBase + DriveNo - $$]      ;dl������������

    jmp 0x0000:0x7C00
    jmp $

;�ƶ����
;ch�����ƶ�����cl�����ƶ���
move_cursor:
    pusha
    push cx
    mov ah, 0x03
    int 10h
    pop cx
    add dh, ch
    add dl, cl
    mov ah, 0x02
    int 10h
    popa
    ret

bootable            db  0                           ;��������������
current             db  0                           ;������
BootablePartition   db  0x00, 0x00, 0x00, 0x00      ;ÿ���������Ӧ�ķ���

BootingFrom:    db      'Booting from partition '
PartitionNo:    db      0x31, 0x20
BootingEnd:     db      0x00, 0x00, 0x00, 0x00
Question:       db      '?', 0x0D, 0x0A, 0x00
CRLF:           db      0x20, 0x0D, 0x0A, 0x00

    times 1022 - ($ - $$) db 0x00
    db 0x55, 0xAA
