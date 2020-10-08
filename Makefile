DEV = /dev/sdb
IMG = hd16.img

all: mbr.bin dbr.bin
	dd if=mbr.bin of=$(IMG) bs=512 count=2 conv=notrunc
	dd if=dbr.bin of=$(IMG) bs=512 count=1 seek=256 conv=notrunc
	dd if=dbr.bin of=$(IMG) bs=512 count=2 skip=1 seek=268 conv=notrunc
	dd if=FAT1-1 of=$(IMG) bs=512 count=1 seek=512 conv=notrunc
	dd if=RootDir-1 of=$(IMG) bs=512 count=1 seek=31232 conv=notrunc
	dd if=autorun.inf of=$(IMG) bs=512 count=1 seek=31272 conv=notrunc

install: mbr.bin dbr.bin
	dd if=mbr.bin of=$(DEV) bs=512 count=2 conv=notrunc
	dd if=dbr.bin of=$(DEV) bs=512 count=1 seek=256 conv=notrunc
	dd if=dbr.bin of=$(DEV) bs=512 count=2 skip=1 seek=268 conv=notrunc

mbr.bin: MBR.asm
	nasm MBR.asm -o mbr.bin -l mbr.lst

dbr.bin:DBR.asm
	nasm DBR.asm -o dbr.bin -l dbr.lst
