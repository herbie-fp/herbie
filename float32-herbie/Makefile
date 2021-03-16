
all: float32-arith.so

float32-arith.so: build/float.o
	gcc -shared -o float32-arith.so build/float32.o

build/float.o:
	mkdir -p build/
	gcc -c -Wall -Werror -fpic -o build/float32.o src/float32.c

clean:
	rm -r build/
	rm float32-arith.so