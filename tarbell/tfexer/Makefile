all:	tfexer.asm tfexer.oct tfexer.hex

tfexer.rom: tfexer.asm
	zasm -uw --asm8080 tfexer.asm

tfexer.oct: tfexer.rom
	hexdump -v -e '"%06.6_ao: " 8/1 "%03o " "\n"' tfexer.rom > tfexer.oct

tfexer.hex: tfexer.asm
	zasm -uw -x --asm8080 tfexer.asm
