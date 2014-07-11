TEX = $(wildcard *.tex)
BIB = references.bib
FIGS = $(wildcard im/*.pdf_tex im/*.pdf im/*.png)

.PHONY: all clean

all: paper.pdf

paper.pdf: $(TEX) $(BIB) $(FIGS) jpaper.cls
	echo $(FIGS)
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

clean:
	rm -f *.aux *.bbl *.blg *.log *.out

