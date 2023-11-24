encrypt.o: encrypt.c
	gcc -std=c99 -Wall -Wextra -pedantic-errors encrypt.c -c -o ./out/encrypt.o
