FLAGS=-std=c11
FAST_FLAGS=-march=native -mtune=native -mfpmath=both -O4
SLOW_FLAGS=-Wall -Wextra -Wpedantic -O0 -g
ITERS=1000000

%.bin: test.c %.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

test_%.slow: test.c %.slow.o
	gcc $(FLAGS) $(SLOW_FLAGS) $^ -o $@ -lm -lgmp -lmpfr

%.o: %.c Makefile
	gcc -ffast-math $(FLAGS) $(FAST_FLAGS) -c $< -o $@

%.slow.o: %.c Makefile
	gcc $(FLAGS) $(SLOW_FLAGS) -c $< -o $@

%.out: %.bin
	./$< $(ITERS) > $@

alltc:
	for f in tc*.c; do make $${f%.c}.out; done
