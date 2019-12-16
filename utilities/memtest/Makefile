all:	memtest.asm memtest.oct memtest.hex

memtest.rom: memtest.asm
	zasm -uw --asm8080 memtest.asm

memtest.oct: memtest.rom
	hexdump -v -e '"%06.6_ao: " 8/1 "%03o " "\n"' memtest.rom > memtest.oct

memtest.hex: memtest.asm
	zasm -uw -x --asm8080 memtest.asm
