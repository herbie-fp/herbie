# Running Casio
FLAGS=-p
BENCHDIR=bench/hamming/

.PHONY: all report publish compile link clean loc all-fig all-convergence rebib paper drop

all:
	$(MAKE) link
	$(MAKE) report

paper: pldi15/paper.pdf

report:
	racket reports/make-report.rkt $(FLAGS) $(BENCHDIR)

publish:
	bash reports/publish.sh
	cd dash/ && bash publish.sh

drop: pldi15/paper.pdf
	cp pldi15/paper.pdf ~/Dropbox/Public/casio.pdf

link:
	raco link casio
	raco link reports
	raco link compile
	raco link casio/simplify

cost:
	$(CC) -O0 cost.c -lm -o cost

loc:
	find reports/ casio/ -type f -exec cat {} \; | wc -l

clean:
	rm -f cost
	rm -rf graphs/
	rm -f compile/$(PREFIX)*.c
	rm -f compile/$(PREFIX)*.out
	rm -f compile/$(PREFIX).*.csv
	rm -f compile/$(PREFIX).*.json
	rm -f pldi15/*.bbl pldi15/*.blg pldi15/*.aux pldi15/*.log pldi15/*.out

doc/tr-14wi.pdf: doc/tr-14wi.tex
	cd doc/ && pdflatex -file-line-error -halt-on-error tr-14wi.tex
	rm doc/tr-14wi.aux doc/tr-14wi.log


# Evaluating Casio's results
PREFIX=tc
RPREFIX=nr
DATAFILES=$(patsubst %,compile/$(PREFIX).%.csv,names if id of od)
RDATAFILES=$(patsubst %,compile/$(RPREFIX).%.csv,names if id of od)
CFILES=$(wildcard compile/$(PREFIX)*.c)
RCFILES=$(wildcard compile/$(RPREFIX)*.c)

.SECONDARY: $(DATAFILES)

compile: compile/single.casio.dat compile/double.casio.dat
	racket compile/compile.rkt -d compile -f $(PREFIX)~a.c $^

rcompile: compile/noregimes.casio.dat
	racket compile/compile.rkt -d compile -f $(RPREFIX)~a.c $^ $^

# Flags for building and running C files
GCC_FLAGS=-std=c11
FAST_FLAGS=$(GCC_FLAGS) -march=native -mtune=native -mfpmath=both -O4 -flto
SLOW_FLAGS=$(GCC_FLAGS) -Wall -Wextra -Wpedantic -O0 -g

# How many samples to use for evaluation
EVALUATION_POINTS=100000

compile/%.o: compile/%.c
	gcc $(FAST_FLAGS) -c $< -o $@

compile/%.slow.o: compile/%.c
	gcc $(SLOW_FLAGS) -c $< -o $@

# Running evaluation binaries

compile/%.bin: compile/test.c compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

compile/%.slow.bin: compile/test.c compile/%.slow.o
	gcc $(SLOW_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

compile/%.out: compile/%.bin
	./$< $(EVALUATION_POINTS) > $@

# The max error experiment timeout, in seconds.
TIMEOUT=60*60

compile/%.dmax.out: compile/%.dh.bin
	./$< $(TIMEOUT) > $@

compile/dmax-all.out: $(CFILES:.c=.dmax.out)
	cat $^ > $@

$(DATAFILES): $(CFILES:.c=.out)
	compile/all.sh compile/$(PREFIX) compile/

$(RDATAFILES): $(RCFILES:.c=.out)
	compile/all.sh compile/$(RPREFIX) compile/

compile/$(PREFIX).json: $(DATAFILES)
	python2 compile/makejson.py compile/$(PREFIX)

compile/$(RPREFIX).json: $(RDATAFILES)
	python2 compile/makejson.py compile/$(RPREFIX)

compile/%.bf.bin: compile/bruteforce.c compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr

compile/%.dh.bin: compile/max-error-hour.c compile/%.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# Generating convergence binaries

compile/%.cv_if.bin: compile/convergence.c compile/%.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# Broken for now...
#compile/sample-points.csv: compile/tc9.cv_if.bin
#	./$< > $@

all-convergence: $(CFILES:.c=.cv_if.png)

# Generating the PLDI'15 paper

PLDI15TEX=$(wildcard pldi15/*.tex)
PLDI15BIB=pldi15/references.bib
PLDI15TIKZFIGS=mpfr-bits casio-runtime rect-f rect-d overhead-d err regimes-e2e
PLDI15FIGS=$(patsubst %,pldi15/fig/eval-%.tex,$(PLDI15TIKZFIGS)) pldi15/fig/overview-diagram.pdf

%.pdf: %.svg
	inkscape --export-pdf=$*.pdf $^

pldi15/paper.pdf: $(PLDI15TEX) $(PLDI15BIB) $(PLDI15FIGS)
	cd pldi15 && pdflatex paper
	cd pldi15 && bibtex paper
	cd pldi15 && pdflatex paper
	cd pldi15 && pdflatex paper

rebib:
	@ cd pldi15 && pdflatex paper > /dev/null
	@ cd pldi15 && bibtex paper > /dev/null
	@ grep "didn't find a database entry for" pldi15/paper.blg | cut -d\  -f8 | tr -d \" | findcite >> $(PLDI15BIB)

# Generating graphs

pldi15/fig/eval-mpfr-bits.tex: compile/mpfr-bits.csv compile/graph.py
	python2 compile/graph.py bits -d compile > $@

pldi15/fig/eval-casio-runtime.tex: compile/casio-runtime.csv compile/graph.py
	python2 compile/graph.py time -d compile > $@

pldi15/fig/eval-rect-f.tex: compile/tc.if.csv compile/tc.of.csv compile/tc.id.csv compile/tc.od.csv compile/graph.py
	python2 compile/graph.py rect-f -d compile > $@

pldi15/fig/eval-rect-d.tex: compile/tc.id.csv compile/tc.od.csv compile/graph.py
	python2 compile/graph.py rect-d -d compile > $@

pldi15/fig/eval-overhead-d.tex: compile/tc.id.csv compile/tc.od.csv compile/graph.py compile/nr.id.csv compile/nr.od.csv
	python2 compile/graph.py overhead-d -d compile > $@

pldi15/fig/eval-err.tex: compile/sample-points.csv compile/graph.py
	python2 compile/graph.py err -d compile > $@

pldi15/fig/eval-regimes-e2e.tex: compile/tc.id.csv compile/tc.od.csv compile/nr.id.csv compile/nr.od.csv compile/graph.py
	python2 compile/graph.py regimes -d compile > $@
