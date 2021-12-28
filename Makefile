default: test

clean:
	${RM} ./hello.asm ./hello.o ./hello bin/lithp

test: hello
	@./hello

hello: hello.o
	ld hello.o -o hello

hello.o: hello.asm
	as hello.asm -o hello.o

hello.asm: bin/lithp hello.lithp
	bin/lithp hello.lithp > hello.asm

bin/lithp: src/*
	odin build src -out:bin/lithp
