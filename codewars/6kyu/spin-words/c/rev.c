#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void rev(char* src, char* dst) {
  short i = 0;
  short j = strlen(src);

  *(dst + j--) = '\0';

  while ((j >= 0)) {
    *(dst + j--) = *(src + i++);
  }
}

int main(void) {
  char s[] = "hello";
  char* r = malloc(strlen(s) + 1);

  rev(s, r);

  printf("%s\n", r);

  free(r);

  return 0;
}
