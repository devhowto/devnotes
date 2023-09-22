##
# Run like:
#
#   $ make out/solution_v1
#   $ make out/solution_v2
#
# Then, execute with:
#
#   $ ./out/solution_v1
#   $ ./out/solution_v2
#

CC=gcc
CFLAGS=-std=c99 -Og -g3 -Wall -Wextra -Wpedantic -fsanitize=address,undefined

./out:
	mkdir -pv ./out

./out/%: %.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rv ./out/*
