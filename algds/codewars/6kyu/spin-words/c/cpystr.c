#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void cpystr(char* src, char* dst) {
  while ((*dst++ = *src++))
    ;
}

int main(void) {
  char* s = "hello";

  char* cpy = malloc(strlen(s) + 1);

  cpystr(s, cpy);
  printf("%s\n", cpy);

  free(cpy);

  return 0;
}

//
// https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html
//
