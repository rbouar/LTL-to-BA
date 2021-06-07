LATEX_AUX_DIR = latex_aux

all: build latex


build:
	make -C prototype build


latex: rapport.pdf slides.pdf

$(LATEX_AUX_DIR):
	@mkdir -p $@

rapport.pdf: latex/rapport.tex | $(LATEX_AUX_DIR)
	pdflatex -shell-escape -output-directory $(LATEX_AUX_DIR) latex/rapport.tex
	pdflatex -shell-escape -output-directory $(LATEX_AUX_DIR) latex/rapport.tex
	mv $(LATEX_AUX_DIR)/rapport.pdf .

slides.pdf : latex/slides.tex | $(LATEX_AUX_DIR)
	pdflatex -output-directory $(LATEX_AUX_DIR) latex/slides.tex
	mv $(LATEX_AUX_DIR)/slides.pdf .

clean:
	make -C prototype clean
	rm -rf $(LATEX_AUX_DIR) rapport.pdf slides.pdf

topdf: buchi.dot
	dot -Tpdf buchi.dot -o buchi.pdf
