all: test.o
	ld -arch x86_64 test.o -lSystem -o test

test.o: test.asm
	nasm -f macho64 -g -F dwarf test.asm -w+all
