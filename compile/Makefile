FLAGS=-std=c11
FAST_FLAGS=-ffast-math -march=native -mtune=intel -mfpmath=both -O4
SLOW_FLAGS=-Wall -Wextra -Wpedantic -O0 -g

test_%: test.c %.o
	gcc $(FLAGS) $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr

test_%.slow: test.c %.slow.o
	gcc $(FLAGS) $(SLOW_FLAGS) $^ -o $@ -lm -lgmp -lmpfr

%.o: %.c Makefile
	gcc $(FLAGS) $(FAST_FLAGS) -c $< -o $@

%.slow.o: %.c Makefile
	gcc $(FLAGS) $(SLOW_FLAGS) -c $< -o $@
