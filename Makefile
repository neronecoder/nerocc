CFLAGS=-std=c11 -g -fno-common
SRCS=$(wildcard *.c)
OBJS=$(SRCS:.c=.o)

nerocc: $(OBJS)
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)

$(OBJS): nerocc.h

test: nerocc
	./test.sh
	./test-driver.sh

clean:
	rm -f nerocc *.o *~ tmp*

.PHONY: test clean