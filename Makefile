all: build rapport


build:
	make -C prototype build


rapport: rapport.pdf

rapport_aux:
	@mkdir rapport_aux

rapport.pdf: rapport/rapport.tex | rapport_aux
	pdflatex -output-directory rapport_aux rapport/rapport.tex
	mv rapport_aux/rapport.pdf .

clean:
	make -C prototype clean
	rm -rf rapport_aux/ rapport.pdf
