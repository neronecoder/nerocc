CFLAGS=-std=c11 -g -fno-common

nerocc: main.o
	$(CC) -o nerocc main.o $(LDFLAGS)

test: nerocc
	./test.sh

clean:
	rm -f nerocc *.o *~ tmp*

.PHONY: test clean