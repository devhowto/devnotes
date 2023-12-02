##
# Run like:
#
#   $ make out/example_v1
#
# Then, execute with:
#
#   $ ./out/example_v2
##

CLIBS=$(HOME)/local/clibs
CC=gcc
CFLAGS=-std=c99 \
	-g3 \
	-Wall \
	-Wextra \
	-Wpedantic \
	-fsanitize=address,undefined \
	-Iinclude

./out:
	mkdir -pv ./out

./out/%: ./%.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rfv ./out/*

