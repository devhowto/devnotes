#include <stdio.h>

int main(void) {
  char word[10];
  int i = 0;

  while (scanf("%9s", word) == 1) {
    ++i;

    if (i % 2 == 1)
      fprintf(stdout, "%s\n", word);
    else
      fprintf(stderr, "%s\n", word);
  }

  return 0;
}
