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

aquarium_v1.o: aquarium_v1.c
	$(CC) $(CFLAGS) aquarium_v1.c -o ./aquarium_v1.o

aquarium_v1: aquarium_v1.o
	$(LL) $(LFLAGS) aquarium_v1.o -o ./out/aquarium_v1

clean:
	rm -rfv ./out/*
	rm -v ./*.o

