FLAGS=-p
BENCHDIR=bench/hamming/

.PHONY: report publish www compile clean loc deploy

all:
	$(MAKE) report

report:
	racket herbie/reports/run.rkt $(FLAGS) $(BENCHDIR)

publish:
	bash herbie/reports/publish.sh upload graphs/
	bash herbie/reports/publish.sh index

www:
	rsync --recursive www/ "uwplse.org:/var/www/herbie/"

loc:
	find herbie/ -type f -exec cat {} \; | wc -l

deploy:
# This won't restart the demo server, just pull the new static content
	cd $(shell ~/uwplse/getdir) && git pull

clean:
	rm -f cost
	rm -rf graphs/

cost: herbie/compile/cost.c
	$(CC) -O0 $^ -lm -o $@

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

# The rest of this Makefile is currently deprecated, awaiting the reboot of the compilation pipeline

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

