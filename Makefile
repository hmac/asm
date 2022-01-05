all: test p13 aoc2021-1 aoc2021-1b

%.o: %.asm
	nasm -f macho64 -g -F dwarf -w+all $*.asm 

%: %.o
	ld -arch x86_64 $@.o -lSystem -o $@
