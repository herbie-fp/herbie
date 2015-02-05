FLAGS=-p
BENCHDIR=bench/hamming/

.PHONY: all report publish compile link clean loc all-fig all-convergence rebib paper drop

all:
	$(MAKE) link
	$(MAKE) report

paper: paper/paper.pdf

report:
	racket herbie/reports/make-report.rkt $(FLAGS) $(BENCHDIR)

publish:
	bash reports/publish.sh

publish-www:
	rsync --recursive www/ "uwplse.org:/var/www/herbie/"

drop: paper/paper.pdf
	cp paper/paper.pdf ~/Dropbox/Public/herbie.pdf

link:
	raco link herbie
	raco link reports
	raco link compile
	raco link herbie/simplify

herbie/compile/cost: herbie/compile/cost.c
	$(CC) -O0 $^ -lm -o $@

loc:
	find reports/ herbie/ -type f -exec cat {} \; | wc -l

clean:
	rm -f cost
	rm -rf graphs/
	rm -f herbie/compile/$(PREFIX)*.c
	rm -f herbie/compile/$(PREFIX)*.out
	rm -f herbie/compile/$(PREFIX).*.csv
	rm -f herbie/compile/$(PREFIX).*.json
	rm -f paper/*.bbl paper/*.blg paper/*.aux paper/*.log paper/*.out

doc/tr-14wi.pdf: doc/tr-14wi.tex
	cd doc/ && pdflatex -file-line-error -halt-on-error tr-14wi.tex
	rm doc/tr-14wi.aux doc/tr-14wi.log


# Evaluating Herbie's results
PREFIX=tc
RPREFIX=nr
DATAFILES=$(patsubst %,herbie/compile/$(PREFIX).%.csv,names if id of od)
RDATAFILES=$(patsubst %,herbie/compile/$(RPREFIX).%.csv,names if id of od)
CFILES=$(wildcard herbie/compile/$(PREFIX)*.c)
RCFILES=$(wildcard herbie/compile/$(RPREFIX)*.c)

.SECONDARY: $(DATAFILES)

compile: herbie/compile/single.herbie.dat herbie/compile/double.herbie.dat
	racket herbie/compile/compile.rkt -d compile -f $(PREFIX)~a.c $^

rcompile: herbie/compile/noregimes.herbie.dat
	racket herbie/compile/compile.rkt -d compile -f $(RPREFIX)~a.c $^ $^

# Flags for building and running C files
GCC_FLAGS=-std=c11
FAST_FLAGS=$(GCC_FLAGS) -march=native -mtune=native -mfpmath=both -O4 -flto
SLOW_FLAGS=$(GCC_FLAGS) -Wall -Wextra -Wpedantic -O0 -g

# How many samples to use for evaluation
EVALUATION_POINTS=100000

herbie/compile/%.o: herbie/compile/%.c
	gcc $(FAST_FLAGS) -c $< -o $@

herbie/compile/%.slow.o: herbie/compile/%.c
	gcc $(SLOW_FLAGS) -c $< -o $@

# Running evaluation binaries

herbie/compile/%.bin: herbie/compile/test.c herbie/compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if herbie/compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

herbie/compile/%.slow.bin: herbie/compile/test.c herbie/compile/%.slow.o
	gcc $(SLOW_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if herbie/compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

herbie/compile/%.out: herbie/compile/%.bin
	./$< $(EVALUATION_POINTS) > $@

# The max error experiment timeout, in seconds.
DMAX_TIMEOUT=60 \* 60

herbie/compile/%.dmax.out: herbie/compile/%.dh.bin
	./$< $(shell expr $(DMAX_TIMEOUT)) > $@

herbie/compile/dmax-all.csv: $(CFILES:.c=.dmax.out)
	cat $^ > $@

$(DATAFILES): $(CFILES:.c=.out)
	herbie/compile/all.sh herbie/compile/$(PREFIX) herbie/compile/

$(RDATAFILES): $(RCFILES:.c=.out)
	herbie/compile/all.sh herbie/compile/$(RPREFIX) herbie/compile/

herbie/compile/$(PREFIX).json: $(DATAFILES)
	python2 herbie/compile/makejson.py herbie/compile/$(PREFIX)

herbie/compile/$(RPREFIX).json: $(RDATAFILES)
	python2 herbie/compile/makejson.py herbie/compile/$(RPREFIX)

herbie/compile/%.bf.bin: herbie/compile/bruteforce.c herbie/compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr

herbie/compile/%.dh.bin: herbie/compile/max-error-hour.c herbie/compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if herbie/compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# Generating convergence binaries

herbie/compile/%.cv_if.bin: herbie/compile/convergence.c herbie/compile/%.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if herbie/compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# Broken for now...
#herbie/compile/sample-points.csv: herbie/compile/tc9.cv_if.bin
#	./$< > $@

all-convergence: $(CFILES:.c=.cv_if.png)

# Generating the PLDI'15 paper

PLDI15TEX=$(wildcard paper/*.tex)
PLDI15BIB=paper/references.bib
PLDI15TIKZFIGS=mpfr-bits runtime rect-f rect-d overhead-d err regimes-e2e
PLDI15FIGS=$(patsubst %,paper/fig/eval-%.tex,$(PLDI15TIKZFIGS)) paper/fig/overview-diagram.pdf

%.pdf: %.svg
	inkscape --export-pdf=$*.pdf $^

paper/paper.pdf: $(PLDI15TEX) $(PLDI15BIB) $(PLDI15FIGS)
	cd paper && pdflatex paper
	cd paper && bibtex paper
	cd paper && pdflatex paper
	cd paper && pdflatex paper

rebib:
	@ cd paper && pdflatex paper > /dev/null
	@ cd paper && bibtex paper > /dev/null
	@ grep "didn't find a database entry for" paper/paper.blg | cut -d\  -f8 | tr -d \" | findcite >> $(PLDI15BIB)

# Generating graphs

paper/fig/eval-mpfr-bits.tex: herbie/compile/mpfr-bits.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py bits -d compile > $@

paper/fig/eval-runtime.tex: herbie/compile/runtime.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py time -d compile > $@

paper/fig/eval-rect-f.tex: herbie/compile/tc.if.csv herbie/compile/tc.of.csv herbie/compile/tc.id.csv herbie/compile/tc.od.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py rect-f -d compile > $@

paper/fig/eval-rect-d.tex: herbie/compile/tc.id.csv herbie/compile/tc.od.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py rect-d -d compile > $@

paper/fig/eval-overhead-d.tex: herbie/compile/tc.id.csv herbie/compile/tc.od.csv herbie/compile/graph.py herbie/compile/nr.id.csv herbie/compile/nr.od.csv
	python2 herbie/compile/graph.py overhead-d -d compile > $@

paper/fig/eval-err.tex: herbie/compile/sample-points.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py err -d compile > $@

paper/fig/eval-regimes-e2e.tex: herbie/compile/tc.id.csv herbie/compile/tc.od.csv herbie/compile/nr.id.csv herbie/compile/nr.od.csv herbie/compile/graph.py
	python2 herbie/compile/graph.py regimes -d compile > $@
