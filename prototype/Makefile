EXE=main.exe
BUILD=_build/default/$(EXE)

build:
	rm -f $(EXE) 
	dune build
	ln -s $(BUILD)

clean:
	dune clean
	rm -f $(EXE)
