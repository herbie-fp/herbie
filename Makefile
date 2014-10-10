# Running Casio
FLAGS=-p
BENCHDIR=bench/hamming/

.PHONY: all report publish compile link clean loc all-fig all-convergence rebib paper

all:
	$(MAKE) link
	$(MAKE) report

paper: pldi15/paper.pdf

report:
	racket reports/make-report.rkt $(FLAGS) $(BENCHDIR)

publish:
	bash reports/publish.sh

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

doc/tr-14wi.pdf: doc/tr-14wi.tex
	cd doc/ && pdflatex -file-line-error -halt-on-error tr-14wi.tex
	rm doc/tr-14wi.aux doc/tr-14wi.log


# Evaluating Casio's results
PREFIX=tc
DATAFILES=$(patsubst %,compile/$(PREFIX).%.csv,names if id of od)
CFILES=$(wildcard compile/$(PREFIX)*.c)

.SECONDARY: $(DATAFILES)

compile: compile/single.casio.dat compile/double.casio.dat
	racket compile/compile.rkt -d compile -f $(PREFIX)~a.c $^

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

$(DATAFILES): $(CFILES:.c=.out)
	compile/all.sh compile/$(PREFIX)

compile/$(PREFIX).json: $(DATAFILES)
	python2 compile/makejson.py compile/$(PREFIX)

# Generating convergence binaries

compile/%.cv_if.bin: convergence.c %.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if compile/$*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

compile/%.cv_if.csv: %.cv_if.bin
	./$< > $@

compile/%.cv_if.png: %.cv_if.csv
	gnuplot -e 'set datafile separator ","; set logscale; set term png; set output "'$@'"; plot "'$<'" using 1:4 title "Standard Error v. Points Sampled" with linespoints'

all-convergence: $(CFILES:.c=.cv_if.png)

# Generating the PLDI'15 paper

PLDI15TEX=$(wildcard pldi15/*.tex)
PLDI15BIB=pldi15/references.bib
PLDI15TIKZFIGS=mpfr-bits casio-runtime rect-f rect-d overhead-d
PLDI15FIGS=$(patsubst %,pldi15/fig/eval-%.tex,$(PLDI15TIKZFIGS)) pldi15/fig/eval-err.pdf pldi15/fig/eval-regimes-e2e.pdf

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

pldi15/fig/eval-overhead-d.tex: compile/tc.id.csv compile/tc.od.csv compile/graph.py
	python2 compile/graph.py overhead-d -d compile > $@
