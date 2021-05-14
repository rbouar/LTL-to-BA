build:
	make -C prototype build

clean:
	make -C prototype clean

topdf: buchi.dot
	dot -Tpdf buchi.dot -o buchi.pdf
