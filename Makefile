default: test

clean:
	${RM} ./hello.asm ./hello.o ./hello bin/lithp

test: hello
	./hello

hello: hello.asm
	fasm hello.asm

hello.asm: bin/lithp hello.lithp src/program_header.asm
	bin/lithp hello.lithp

bin/lithp: src/lithp.odin
	odin build src -out:bin/lithp -debug -opt:0 -define:bake_header=false$(EXTRA_FLAGS)
