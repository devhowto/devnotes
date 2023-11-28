##
# Run like:
#
#   $ make out/example_v1
#   $ make out/example_v2
#
# Then, execute with:
#
#   $ ./out/example_v1
#   $ ./out/example_v2
##

CLIBS=$(HOME)/local/clibs
CC=gcc
CFLAGS=-std=c99 \
	-Og \
	-g3 \
	-Wall \
	-Wextra \
	-Wpedantic \
	-fsanitize=address,undefined \
	-Iinclude

./out:
	mkdir -pv ./out

./out/%: source/%.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rfv ./out/*
