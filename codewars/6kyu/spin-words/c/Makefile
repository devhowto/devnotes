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

CC=clang
# CFLAGS=-std=c99 -g3 -Wall -Wextra -Wpedantic
CFLAGS=-std=c99 -Wall -Wextra -Wpedantic -I ./criterion/include -L ./criterion/lib -lcriterion

./out:
	mkdir -pv ./out

./out/%: %.c
	$(CC) $(CFLAGS) $< -o $@

clean:
	rm -rfv ./out/*
