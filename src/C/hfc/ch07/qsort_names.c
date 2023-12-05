#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int compare_names_asc(const void *a, const void *b) {
  char **sa = (char**)a;
  char **sb = (char**)b;

  return strcmp(*sa, *sb);
}

/**
 * Implement the desc version in terms of the asc one just by
 * flipping the params order.
 */
int compare_names_desc(const void *a, const void *b) {
  return compare_names_asc(b, a);
}

int main(void) {
  /* Uppercase letters sort first as their underlying code points
   * are lower than the lowercase letters. */
  char *names[5] = { "Vader", "Aayla", "Ahsoka", "Luke", "Yoda" };

  qsort(names, 5, sizeof(char*), compare_names_asc);
  for (short i = 0; i < 5; ++i)
    printf("%s\n", names[i]);

  printf("\n");

  qsort(names, 5, sizeof(char*), compare_names_desc);
  for (short i = 0; i < 5; ++i)
    printf("%s\n", names[i]);

  return 0;
}
