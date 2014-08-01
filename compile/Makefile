
test_%: test.c %.o
	gcc -Wall -g -O3 -mfpmath=387 -o test_$* -DNAME_id=$*_id -DNAME_od=$*_od -DNAME_im=$*_im $^ -lm -lgmp -lmpfr

%.o: %.c Makefile
	gcc -Wall -ffast-math -mfpmath=387 -O4 -g -c $<

%.slow.o: %.c Makefile
	gcc -Wall -Wextra -O0 -g -c $<
