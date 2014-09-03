
.PHONY: report publish link clean loc

all:
	$(MAKE) link
	$(MAKE) report

report:
	racket reports/make-report.rkt -p bench/hamming/

publish:
	bash reports/publish.sh

link:
	raco link casio
	raco link reports

cost:
	$(CC) -O0 cost.c -lm -o cost

clean:
	rm -f index.html
	rm -f cost
	rm -rf graphs/

loc:
	find reports/ casio/ -type f -exec cat {} \; | wc -l

doc/tr-14wi.pdf: doc/tr-14wi.tex
	cd doc/ && pdflatex -file-line-error -halt-on-error tr-14wi.tex
	rm doc/tr-14wi.aux doc/tr-14wi.log
