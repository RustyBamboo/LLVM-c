IN_FILES=$(wildcard tests/*.in)
OUT_FILES=$(IN_FILES:.in=.out)
LLVM_FILES=$(IN_FILES:.in=.llvm)
EXE_FILES=$(IN_FILES:.in=.exe)

default: main.byte

clean:
	rm -f main.byte
	rm -fr _build/
	rm -rf tests/*.llvm
	rm -rf tests/*.exe
	rm -rf tests/*.out

main.byte:
	ocamlbuild -use-ocamlfind -pkgs llvm,llvm.analysis,llvm.bitwriter -use-menhir main.byte

tests/%.llvm: tests/%.in main.byte
	./main.byte $< $@
	llvm-dis $@ -o $@
	
tests/%.exe: tests/%.llvm
	llc $<
	clang -o $@ $<.s
	rm $<.s

tests/%.out: tests/%.exe
	$< > $@

tests: main.byte $(LLVM_FILES) $(EXE_FILES) $(OUT_FILES)
	@echo "Done testing. Results in tests dir."
