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

aquarium.o: aquarium.c
	$(CC) $(CFLAGS) aquarium.c -o ./aquarium.o

aquarium: aquarium.o
	$(LL) $(LFLAGS) aquarium.o -o ./out/aquarium

clean:
	rm -rfv ./out/*

aquarium: aquarium.c

