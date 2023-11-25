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

aquarium_v3.o: aquarium_v3.c
	$(CC) $(CFLAGS) aquarium_v3.c -o ./aquarium_v3.o

aquarium_v3: aquarium_v3.o
	$(LL) $(LFLAGS) aquarium_v3.o -o ./out/aquarium_v3

clean:
	rm -rfv ./out/*
	rm -rv ./*.o

