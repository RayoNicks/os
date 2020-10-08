;MBR�����������Գ���
;
;2020.2.18

    org 0x7C00

StackPointer equ 0x7C00

    ;��ʼ��Real Mode�μĴ���
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, StackPointer

    ;����
    mov ax, 0600h
    mov bh, 00000111b
    mov cx, 0000h
    mov dx, 0184fh

    ;���ù��λ��
    mov ax, 02h
    mov dx, 0000h
    int 10h

    ;��ʾ������Ϣ
    mov ax, 1301h
    mov bx, 0007h
    mov cx, MBRMessageLen
    mov bp, MBRMessage
    int 10h

    jmp $

MBRMessage:     db  'MBR loaded...'
MBRMessageLen   equ $ - MBRMessage

    times 510 - ($ - $$) db 0
    db 0x55, 0xAA