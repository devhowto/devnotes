# vim: set ft=make:

CC = gcc
LL = gcc

CFLAGS = -std=c99 \
				 -Og \
				 -g3 \
				 -Wall \
				 -Wextra \
				 -Wpedantic \
				 -fsanitize=address,undefined \
				 -c

LFLAGS = -fsanitize=address,undefined

out:
	mkdir -pv ./out

scuba_v1.o: scuba_v1.c
	$(CC) $(CFLAGS) scuba_v1.c -o ./scuba_v1.o

scuba_v1: scuba_v1.o
	$(LL) $(LFLAGS) scuba_v1.o -o ./out/scuba_v1

clean:
	rm -rfv ./out/*
	rm -rv ./*.o

