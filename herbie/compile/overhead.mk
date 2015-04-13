TESTCASES = $(sort $(dir $(wildcard */compiled.c)))

# Flags for building and running C files
GCC_FLAGS = -std=c11
FAST_FLAGS = $(GCC_FLAGS) -march=native -mtune=native -mfpmath=both -O4 -flto
SLOW_FLAGS = $(GCC_FLAGS) -Wall -Wextra -Wpedantic -O0 -g

%/fast.o: %/compiled.c
	gcc $(FAST_FLAGS) -c $< -o $@

%/overhead: test.c %/fast.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*/compiled.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# How many samples to use for evaluation
POINTS = 100000

%/overhead.csv: %/overhead
	./$< $(POINTS) > $@

names.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 1p > names.csv

pf.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 2p > pf.csv

pd.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 3p > pd.csv

if.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 5p > if.csv

id.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 6p > id.csv

of.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 7p > of.csv

od.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 8p > od.csv

df.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 9p > df.csv

dd.csv: $(patsubst %/,%/overhead.csv,$(TESTCASES))
	echo $^ | xargs -n1 sed -n 10p > dd.csv

DATAFILES = $(patsubst %,%.csv,names pf pd if id of od df dd)

.PHONY: all-overhead

all-overhead: $(DATAFILES)
