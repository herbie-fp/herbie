PREFIX=tc
BENCHDIR=bench/hamming/
CASIOFLAGS=-p

.PHONY: all report publish link clean loc

all:
	$(MAKE) link
	$(MAKE) report

report:
	racket reports/make-report.rkt -p $(CASIOFLAGS) $(BENCHDIR)

publish:
	bash reports/publish.sh

compile/results.casio.dat: graphs/results.casio.dat
	cp graphs/results.casio.dat compile/

compile: compile/results.casio.dat
	racket compile/compile.rkt -f $(PREFIX)~a.c compile/results.casio.dat

link:
	raco link casio
	raco link reports
	raco link compile

cost:
	$(CC) -O0 cost.c -lm -o cost

clean:
	rm -f cost
	rm -rf graphs/

loc:
	find reports/ casio/ -type f -exec cat {} \; | wc -l

doc/tr-14wi.pdf: doc/tr-14wi.tex
	cd doc/ && pdflatex -file-line-error -halt-on-error tr-14wi.tex
	rm doc/tr-14wi.aux doc/tr-14wi.log
