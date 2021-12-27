default: test

test: example
	@./example

example: example.o
	ld example.o -o example

example.o: example.asm
	as example.asm -o example.o

example.asm: bin/lithp example.lithp
	bin/lithp example.lithp > example.asm

bin/lithp: src/*
	odin build src -out:bin/lithp
