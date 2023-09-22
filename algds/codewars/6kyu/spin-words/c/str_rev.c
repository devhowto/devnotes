#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// void str_rev(char* src, char* dst, short len) {
//   dst = src;
  // short i = 0;
  // short j = len - 1;

  // *(dst + (len - 1)) = '\0';
  //
  // while (i < len)
  //   *(dst + j--) = *(src + i++);
// }

int main(void) {
  char* s = "hello world";
  char* rev = malloc(strlen(s) + 1);

  // str_rev(s, rev, strlen(s));

  printf("%s\n", s);
  // printf("%s\n", s);

  return 0;
}

//
// https://gcc.gnu.org/onlinedocs/gcc/Optimize-Options.html
//
