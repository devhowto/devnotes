# vim: set ft=make:

CC = gcc
LL = gcc

CFLAGS = -std=c99 \
				 -Og \
				 -g3 \
				 -Wall \
				 -Wextra \
				 -Wpedantic \
				 -fsanitize=address,undefined

LFLAGS = -fsanitize=address,undefined

out:
	mkdir -pv ./out

out/%: %.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rfv ./out/*
