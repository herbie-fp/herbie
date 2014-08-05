TEX = $(wildcard *.tex)
BIB = references.bib
FIGS = $(wildcard im/*.pdf_tex im/*.pdf im/*.png)

.PHONY: all clean rebib

all: paper.pdf

paper.pdf: $(TEX) $(BIB) $(FIGS) jpaper.cls
	pdflatex paper
	bibtex paper
	pdflatex paper
	pdflatex paper

clean:
	rm -f *.aux *.bbl *.blg *.log *.out paper.pdf

publish:
	cp paper.pdf ~/Dropbox/Public/casio.pdf

rebib:
	@ pdflatex paper > /dev/null
	@ bibtex paper > /dev/null
	@ grep "didn't find a database entry for" paper.blg | cut -d\  -f8 | tr -d \" | findcite >> references.bib
