# Find all compiled test-cases
# If Herbie times out or crashes, no compiled.c is generated.
TESTCASES = $(sort $(dir $(wildcard */compiled.c)))

# Flags for building and running C files
FAST_FLAGS = -std=c11 -march=native -mtune=native -mfpmath=both -O3 -flto

%/fast.o: %/compiled.c
	gcc $(FAST_FLAGS) -c $< -o $@

%/overhead: overhead.c %/fast.o
	gcc $(FAST_FLAGS) $^ -o $@ -lm -lgmp -lmpfr -DNARGS=$(shell grep f_if $*/compiled.c | tr '()_ ,' '\n' | tail -n+2 | grep float -c)

# How many samples to use for evaluation
POINTS = 100000

%/overhead.csv: %/overhead
	./$< $(POINTS) > $@

# The output CSV files contain a bunch of fields.
# These rules aggregate each one into its own file.

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

# The top-level target is `overhead` to make the overhead numbers

.PHONY: overhead

overhead: $(DATAFILES)
