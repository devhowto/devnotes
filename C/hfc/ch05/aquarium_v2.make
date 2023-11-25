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

aquarium_v2.o: aquarium_v2.c
	$(CC) $(CFLAGS) aquarium_v2.c -o ./aquarium_v2.o

aquarium_v2: aquarium_v2.o
	$(LL) $(LFLAGS) aquarium_v2.o -o ./out/aquarium_v2

clean:
	rm -rfv ./out/*
	rm -rv ./*.o

