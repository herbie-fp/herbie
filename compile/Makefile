FLAGS=-std=c11
FAST_FLAGS=-march=native -mtune=native -mfpmath=both -O4 -flto
SLOW_FLAGS=-Wall -Wextra -Wpedantic -O0 -g
ITERS=1000000

%.bin: test.c %.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

%.slow.bin: test.c %.slow.o
	gcc $(FLAGS) $(SLOW_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

%.cv_if.bin: convergence.c %.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

%.cv_if.csv: %.cv_if.bin
	./$< > $@

%.cv_if.png: %.cv_if.csv
	gnuplot -e 'set datafile separator ","; set logscale; set term png; set output "'$@'"; plot "'$<'" using 1:4 title "Standard Error v. Points Sampled" with linespoints'

%.o: %.c Makefile
	gcc $(FLAGS) $(FAST_FLAGS) -c $< -o $@

%.slow.o: %.c Makefile
	gcc $(FLAGS) $(SLOW_FLAGS) -c $< -o $@

%.out: %.bin
	./$< $(ITERS) > $@

.PHONY: alltc allpng

alltc:
	for i in tc*.c; do make $${i%.c}.out; done

allpng:
	for i in tc*.c; do make $${i%.c}.cv_if.png; done
